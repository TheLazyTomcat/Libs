{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Process-Global Variables

    This library provides a way of sharing global variables across different
    modules loaded within single process without a need for explicit
    cooperation.

    One might ask why is this needed - in the end, all code loaded in one
    virtual address space can see everything thereof, including not only global
    variables, but even the local ones (unless they are register-only) and more.
    This is true, but there are several problems with that, namely:

         - module cannot find any specific variable within the memory that was
           allocated in a different module without communicating/receiving its
           address

         - if some variable is to be shared, it must be created only once per
           entire process and then kept alive the entire time it is needed

         - the variable cannot be statically allocated (ie. defined), because
           the moment its parent module is unloaded, it dissapears or becomes
           inaccessible

         - whatewer the implementation, it cannot reference anything within any
           module (code, data, resources, nothing...), including its own (so no
           classes), because you never know who created what a who will be
           unloaded and when

         - allocation cannot be done using integrated memory manager, simply
           because every module can use a different one and what's more, each
           is running separately from managers in other modules

    Most of the mentioned problems can be solved by implementing the sharing
    and allocating mechanism in a separate dynamic library (DLL/SO) - but that
    is exactly what I wanted to avoid, because it requires that this dynamic
    library is deployed with every single program or library that uses such
    system. This unit implements standalone solution - no external DLL/SO is
    needed.
    Another possible solution is also to use memory-mapped files, but that is
    too heavy-weight for envisioned common use cases (sharing few numbers or
    maybe some limited resources).

    I will not delve into implementation details (read the code if you are
    interested and brave enough - note that the mechanisms used are the same
    both in Windows and Linux), but some things need to be clarified...

      To sum what this library does - it allows you to allocate a global
      variable that can then be found by and accessed in completely different
      module within the same process. It can be used eg. for sharing information
      that needs to be global within the entire program.

      Entire interface is procedural (so no objects), see above why. It can
      be wrapped in classes/objects, but those would be local-only to module
      running them.

      The variables are distinguished and searched for by their 32bit numeral
      identifiers (meaning there is a technical limit of about four billion
      distinct variables).
      There are functions accepting string identifiers, but those are only
      for the sake of convenience - the strings are always converted to
      integers using Alder32 checksum algorithm and these integer values are
      then used as identifiers. This means, among others, that two different
      strings can point to the same variable.

      The implementation also contains what is called "internal compatibility
      version" (ICV) - only libraries with the same version can share data.
      This mechanism is here to prevent problems when/if this library changes
      its internal workings, so libraries with incompatible code do not access
      the same internal state and inadvertently corrupt it.

    For more information about this library, refer to description of provided
    procedural interface and its types.

  Version 1.1 (2024-10-25)

  Internal compatibility version 1

  Last change 2025-02-20

  ©2024-2025 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.ProcessGlobalVars

  Dependencies:
    Adler32       - github.com/TheLazyTomcat/Lib.Adler32
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxMath       - github.com/TheLazyTomcat/Lib.AuxMath
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect       - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol ProcessGlobalVars_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit ProcessGlobalVars;
{
  ProcessGlobalVars_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  ProcessGlobalVars_UseAuxExceptions to achieve this.
}
{$IF Defined(ProcessGlobalVars_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
  {$IFNDEf CompTest}
    {$MESSAGE WARN 'Does not work in linux - required symbol is not exported.'}
  {$ENDIF}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}  

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
  {$INLINE ON}
  {$DEFINE CanInline}
{$ELSE}
  {$IF CompilerVersion >= 17} // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EPGVException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EPGVModuleEnumerationError = class(EPGVException);

  EPGVHeapAllocationError = class(EPGVException);

  EPGVInvalidValue      = class(EPGVException);
  EPGVInvalidState      = class(EPGVException);
  EPGVInvalidVariable   = class(EPGVException);
  EPGVUnknownVariable   = class(EPGVException);
  EPGVDuplicateVariable = class(EPGVException);

  EPGVMutexError  = class(EPGVException);
  EPGVSystemError = class(EPGVException);

{===============================================================================
--------------------------------------------------------------------------------
                        PGV public interface declaration
--------------------------------------------------------------------------------
===============================================================================}
type
{
  TPGVIdentifier

  This type is used within this library as a numerical identifier of variables
  managed here.

  It is always an unsigned 32bit-wide integer (with system endianness). 
}
  TPGVIdentifier = UInt32;

{
  TPGVIdentifierArray

  This is only used when enumerating existing variables.
}
  TPGVIdentifierArray = array of TPGVIdentifier; 

{
  TPGVVariable

  This type is used as a reference to existing variables. It is returned by
  functions searching for and/or (re)allocating managed variables.

  This type is a two-level pointer (pointer to pointer), meaning it points to
  a pointer and only that second-level pointer points to memory occupied by the
  referenced variable. Therefore, to directly access the variable, you have to
  dereference it twice (eg. SomeVar^^)!

  It is two-level to allow relocation of memory without affecting the reference.
  For example, you search for some variable you want to use - this reference is
  returned and you store it for further use. This stored reference will be valid
  for the entire lifetime of the variable, even if it is reallocated and moved
  to a different memory address (value of the second-level pointer changes, but
  the stored reference is still pointing to it).

    WARNING - whenever accessing the variable, use the provided two-level
              reference, never use the second-level pointer as other threads
              can change it without you knowing.

    NOTE - unless you are absolutely sure the variable will not be reallocated
           once it exists, you should lock the state (using GlobVarLock and
           GlobVarUnlock) when directly accessing the variable (dereferencing
           it instead of calling GlobVarStore and GlobVarLoad) - this avoids
           potential problems when the variable is accessed by multiple threads
           (eg. one thread might reallocate it while other thread is writing
           something to it - you do NOT want this to happen).

  Note on functions accepting parameter of type TPGVVariable as their input...

    All functions operating on variables that accept either numeral or string
    identifiers have one unfortunate thing in common - everytime they are
    called, they search the entire internal state for the requested variable,
    and this searching cannot be much optimized.

    To improve performance, you can call functions accepting direct variable
    reference (provided you already have it, eg. from allocation) - these
    functions are not searching the state, but insteady operate directly on the
    provided memory reference. That being said, make sure you provide a valid
    reference (if nil is passed, then these functions just raise an exception
    of class EPGVInvalidValue, if non-nil invalid pointer is used, then better
    be ready for nasty bugs).
}
  TPGVVariable = PPointer;
  PPGVVariable = ^TPGVVariable;

//------------------------------------------------------------------------------
{
  GlobVarInternalCompatibilityVersion

  Returns internal compatibility version (an integer number in effect for
  current implementation).

  This number is used only internally so this call is here only for informative
  purposes.

    WARNING - in 64bit systems, the highest bit of the result will be set,
              making the number a large negative integer. This is to separate
              64bit and 32bit implementations in case they meet in one process
              (should not be possible, but better be safe than sorry).
}
Function GlobVarInternalCompatibilityVersion: Int32;

{
  GlobVarTranslateIdentifier

  Converts string identifier to numeral identifier used to distinguish
  individual variables.

  The string is converted to a WideString (to minimize potential problems with
  encodings - but even then, you should avoid non-ASCII characters) and an
  Adler32 checksum is calculated for it. This sum is then casted directly to
  the resulting identifier. This means, among other things, that there is a
  posibility that two different strings produce the same identifier and
  therefore, when used, will reference the same variable - be aware of this.

  String identifiers are case sensitive ("abc" is NOT the same as "ABC").

    NOTE - further functions that are accepting string identifiers are calling
           this routine to convert the provided string. Although the Adler32
           checksum is fast, the effect of calculating it many times over might
           be noticeable. Therefore, in case you make a lot of calls with the
           same string identifier, you should consider converting it to numeral
           identifier at the start and then use only this numeral instead of
           the string.
}
Function GlobVarTranslateIdentifier(const Identifier: String): TPGVIdentifier;{$IF Defined(FPC) and Defined(CanInline)} inline;{$IFEND}

//------------------------------------------------------------------------------
{
  GlobVarLock

  Locks internal state of this library for the calling thread, making sure no
  other thread(s) can manipulate it while the lock is in effect. This lock is
  process-wide.

  It can be called multiple times, but each call to it must be paired by a call
  to GlobVarUnlock to unlock the state when you are done using it.

  If one thread holds the lock, all attempts to acquire the lock from other
  threads will block until the lock is released.

  All functions declared in this procedural interface, with notable exceptions
  being these:

      GlobVarInternalCompatibilityVersion
      GlobVarTranslateIdentifier
      GlobVarLock
      GlobVarUnlock

  ...are acquiring the lock during their execution. This means that all these
  functions are serialized (ie. only one call can be running at a time).

  It is here mainly to protect the state when making complex operations
  involving multiple calls to interface functions. Let's have an example:

      GlobVarLock;
      try
        If GlobVarFind('abc',Ptr) then
          begin
            If GlobVarSize('abc') <> 1 then
              Ptr := GlobVarRealloc('abc',1);
          end
        else Ptr := GlobVarAlloc('abc',1);
        Boolean(Ptr^^) := True;
      finally
        GlobVarUnlock;
      end;

  ...this all will be executed in a thread safe manner.

  It can also be used to protect individual variables when accesing them
  directly, without using provided load and store functions (which are
  serialized too).

    WARNING - do not hold the lock for prolonged time periods, since those
              locks are also acquired during module loading and unloading,
              it can cause serious problems at unpredictable times.
}
procedure GlobVarLock;

{
  GlobVarUnlock

  Unlocks the internal state. This function must be called as many times as
  GlobVarLock was called to achieve proper unlocking.
}
procedure GlobVarUnlock;

//------------------------------------------------------------------------------
{
  GlobVarCount

  Returns number of variables managed by this library.
}
Function GlobVarCount: Integer;

{
  GlobVarMemory

  Returs an amount of global memory (number of bytes) used by this library and
  allocated variables.

  When IncludeOverhead is False, then only memory used by the variables (all
  of them, including those stored in-situ - see GlobVarHeapStored for
  explanation) is returned.
  When set to True, then it returns size of all memory allocated for internal
  state plus memory allocated on the heap for individual variables (note that
  variables stored in the state are not counted, as their memory is already
  included as part of the state itself).
}
Function GlobVarMemory(IncludeOverhead: Boolean = False): TMemSize;

{
  GlobVarEnumerate

  Returns an array enumerating identifiers of all managed variables (an empty
  array is returned if no variable is present).
}
Function GlobVarEnumerate: TPGVIdentifierArray;

//------------------------------------------------------------------------------
{
  GlobVarFind

  Searches the internal state for a variable of given identifier.

  When it is found, then True is returned, output parameter Variable contains
  its reference and Size contains actual size of it.
  If not found then False is returned, Variable is set to nil and Size to zero.
}
Function GlobVarFind(Identifier: TPGVIdentifier; out Variable: TPGVVariable): Boolean; overload;
Function GlobVarFind(const Identifier: String; out Variable: TPGVVariable): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function GlobVarFind(Identifier: TPGVIdentifier; out Variable: TPGVVariable; out Size: TMemSize): Boolean; overload;
Function GlobVarFind(const Identifier: String; out Variable: TPGVVariable; out Size: TMemSize): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

type
{
  TPGVGetResult

  Type used by some overloads of function GlobVarGet to inform the caller
  about the result of operation.

    vgrCreated      - requested variable have not existed prior the call and
                      was allocated by it
    vgrOpened       - variable existed and its size matched the requested one
    vgrSizeMismatch - variable exists but its size do not match
    vgrError        - some unspecified error occured
}
  TPGVGetResult = (vgrCreated,vgrOpened,vgrSizeMismatch,vgrError);
  
{
  GlobVarGet

  First version (overloads returning type TPGVVariable)

    Returns a reference to variable of given identifier. If the variable cannot
    be found, then these functions raise an EPGVUnknownVariable exception.

  Second version (returning type TPGVGetResult)

    Tries to find variable of given identifier. If it is not found, then it
    allocates it.

    When the wanted variable exists, its size is compared to whatever is given
    in parameter Size. If they match, then parameter Size is left unchanged,
    output parameter Variable is set to a reference to the existing variable
    and result is set to vgrOpened. If the sizes do not match, then parameter
    Size is set to actual size of the variable, Variable contains reference to
    the variable and result is set to vgrSizeMismatch.

    If variable of given identifier does not exist, then it is allocated using
    Size parameter (this parameter is not changed). Variable then contains
    reference to the newly created entry and result is set to vgrCreated.

    If these functions return vgrError, then value of Size and Variable is
    undefined (but this result should not be ever returned, as exceptions are
    raised on errors).
}
Function GlobVarGet(Identifier: TPGVIdentifier): TPGVVariable; overload;
Function GlobVarGet(const Identifier: String): TPGVVariable; overload;

Function GlobVarGet(Identifier: TPGVIdentifier; var Size: TMemSize; out Variable: TPGVVariable): TPGVGetResult; overload;
Function GlobVarGet(const Identifier: String; var Size: TMemSize; out Variable: TPGVVariable): TPGVGetResult; overload;{$IFDEF CanInline} inline;{$ENDIF}

{
  GlobVarRename

  Changes identifier of selected variable to a new one. No existing references
  are affected by this operation.

  If variable with OldIdentifier does not exist, then ans EPGVUnknownVariable
  exception is raised. Overload accepting variable reference will raise an
  EPGVInvalidVariable exception if the reference is not valid.

  If old and new identifiers match (two different strings can produce the same
  identifier and therefore be considered equal - see GlobVarTranslateIdentifier
  for details), then nothing is done. But note that the variable is still
  searched for, and if it does not exist then an exception is raised.

  If a variable with NewIdentifier already exists, and it is not the one that
  is being renamed, then an EPGVDuplicateVariable exception is raised.
}
Function GlobVarRename(OldIdentifier,NewIdentifier: TPGVIdentifier): TPGVVariable; overload;
Function GlobVarRename(OldIdentifier: TPGVIdentifier; const NewIdentifier: String): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function GlobVarRename(const OldIdentifier: String; NewIdentifier: TPGVIdentifier): TPGVVariable; overload;
Function GlobVarRename(const OldIdentifier,NewIdentifier: String): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure GlobVarRename(Variable: TPGVVariable; NewIdentifier: TPGVIdentifier); overload;
procedure GlobVarRename(Variable: TPGVVariable; NewIdentifier: String); overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  GlobVarIsValid

  Returns true when the given variable reference is assigned and references
  a valid allocated variable, false otherwise.

  If CheckAddress is set to True, then a check whether the reference address
  actually points to a correct place in internal state is also performed.
  If set to False, then no such check is done and the reference is assumed to
  be pointing to a correct place (and you better make sure it does...).
}
Function GlobVarIsValid(Variable: TPGVVariable; CheckAddress: Boolean = True): Boolean;

{
  GlobVarIdentifier

  Returns identifier of variable whose reference is given.

  This is intended for situations where one needs an identifier of unknown
  variable or of a variable that might have been renamed.

  Raises an EPGVInvalidVariable exception if the reference is not valid.
}
Function GlobVarIdentifier(Variable: TPGVVariable): TPGVIdentifier;

{
  GlobVarExists

  Indicates whether the requested variable exists. When it exists, it will
  return true, when it does not exist, then it will return false.
}
Function GlobVarExists(Identifier: TPGVIdentifier): Boolean; overload;
Function GlobVarExists(const Identifier: String): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{
  GlobVarSize

  Returns size of the given variable.

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier).

    NOTE - existing variables cannot have size of zero.

  Overload accepting variable reference can also raise an EPGVInvalidVariable
  exception if the reference is not valid.
}
Function GlobVarSize(Identifier: TPGVIdentifier): TMemSize; overload;
Function GlobVarSize(const Identifier: String): TMemSize; overload;
Function GlobVarSize(Variable: TPGVVariable): TMemSize; overload;

{
  GlobVarHeapStored

  Indicates whether the given variable is stored on the heap (true is returned)
  or in-situ (false is returned).

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier).

  Variables stored on the heap reside in a memory that is obtained from
  (allocated by) a global memory manager (usually provided by operating system
  or system library).
  To limit memory fragmentation and use of resources, small variables (smaller
  or equal in size to a pointer) are stored directly in the internal state that
  manages those variables (this is called in-situ).

    WARNING  - Abovementioned means you should never assume anything about
               position of any variable in memory and certainly avoid writing
               outside its boundaries (you could corrupt the state).

  Overload accepting variable reference can also raise an EPGVInvalidVariable
  exception if the reference is not valid.
}
Function GlobVarHeapStored(Identifier: TPGVIdentifier): Boolean; overload;
Function GlobVarHeapStored(const Identifier: String): Boolean; overload;
Function GlobVarHeapStored(Variable: TPGVVariable): Boolean; overload;

//------------------------------------------------------------------------------
type
{
  TPGVVariableFlag
  TPGVVariableFlags

  Used when returning flags of selected variable. If the flag is set, then
  the corresponding enum value will be included in the TPGVVariableFlags set,
  otherwise it will be excluded from that set.

    vfRealocated - variable was reallocated at least once during its life (only
                   true reallocations set this flag, call to GlobVarRealloc(ate)
                   might not actually perform it if not needed)

    vfRenamed    - variable was renamed at least once (only true renamings
                   count)
}
  TPGVVariableFlag = (vfReallocated,vfRenamed);

  TPGVVariableFlags = set of TPGVVariableFlag;

{
  GlobVarGetFlags

  Returns a set type indicating which flags are set in the internal state of
  given variable. See types TPGVVariableFlag and TPGVVariableFlags for details.

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier).

  Overload accepting variable reference can also raise an EPGVInvalidVariable
  exception if the reference is not valid.
}
Function GlobVarGetFlags(Identifier: TPGVIdentifier): TPGVVariableFlags; overload;
Function GlobVarGetFlags(const Identifier: String): TPGVVariableFlags; overload;
Function GlobVarGetFlags(Variable: TPGVVariable): TPGVVariableFlags; overload;

{
  GlobVarSetFlags

  Sets flags according to the passed Flags set parameter and returns previous
  state of flags for the given variable.

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier). Overload
  accepting variable reference will raise an EPGVInvalidVariable exception
  if the reference is not valid.
}
Function GlobVarSetFlags(Identifier: TPGVIdentifier; Flags: TPGVVariableFlags): TPGVVariableFlags; overload;
Function GlobVarSetFlags(const Identifier: String; Flags: TPGVVariableFlags): TPGVVariableFlags; overload;
Function GlobVarSetFlags(Variable: TPGVVariable; Flags: TPGVVariableFlags): TPGVVariableFlags; overload;

{
  GlobVarGetFlag

  Returns state of selected flag for given variable. True means the flag is
  set (1), flase means it is clear (0).

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier). Overload
  accepting variable reference will raise an EPGVInvalidVariable exception
  if the reference is not valid.
}
Function GlobVarGetFlag(Identifier: TPGVIdentifier; Flag: TPGVVariableFlag): Boolean; overload;
Function GlobVarGetFlag(const Identifier: String; Flag: TPGVVariableFlag): Boolean; overload;
Function GlobVarGetFlag(Variable: TPGVVariable; Flag: TPGVVariableFlag): Boolean; overload;

{
  GlobVarSetFlag

  Sets state of selected flag for given variable to Value and returns its
  previous state.

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier). Overload
  accepting variable reference will raise an EPGVInvalidVariable exception
  if the reference is not valid.
}
Function GlobVarSetFlag(Identifier: TPGVIdentifier; Flag: TPGVVariableFlag; Value: Boolean): Boolean; overload;
Function GlobVarSetFlag(const Identifier: String; Flag: TPGVVariableFlag; Value: Boolean): Boolean; overload;
Function GlobVarSetFlag(Variable: TPGVVariable; Flag: TPGVVariableFlag; Value: Boolean): Boolean; overload;

//------------------------------------------------------------------------------
{
  GlobVarRefCount

  Returns current reference count of given variable.

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier).

  Overload accepting variable reference can also raise an EPGVInvalidVariable
  exception if the reference is not valid.

  Reference counting overview

    Reference counting of individual variables is NOT automatic - it is provided
    only to help with variable lifetime in multi-threaded and multi-module
    environment, where implementation cannot be sure whether it can free the
    variable or not (ie. whether it is still used by someone).

      WARNING - other functions operating on variables, other than those
                explicitly accessing reference count, completely ignore it,
                it is provided only for user code convenience.

    When a new variable is allocated, its reference count is set to zero.

    To increment or decrement it, you need to call funtions GlobVarAcquire or
    GlobVarRelease respectively.

    GlobVarRelease can be also used to automatically free the variable when
    its reference count drops to zero - see there for details.
}
Function GlobVarRefCount(Identifier: TPGVIdentifier): UInt32; overload;
Function GlobVarRefCount(const Identifier: String): UInt32; overload;
Function GlobVarRefCount(Variable: TPGVVariable): UInt32; overload;

{
  GlobVarAcquire

  Increments reference count of the given variable by one and returns the new
  count. Nothing else is changed about the variable.

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier).

  Overload accepting variable reference can also raise an EPGVInvalidVariable
  exception if the reference is not valid.

  If the reference count is already at High(UInt32), then an EPGVInvalidState
  exception is raised.    
}
Function GlobVarAcquire(Identifier: TPGVIdentifier): UInt32; overload;
Function GlobVarAcquire(const Identifier: String): UInt32; overload;
Function GlobVarAcquire(Variable: TPGVVariable): UInt32; overload;

{
  GlobVarRelease

  Decrements reference count of the given variable by one and returns its new
  value. If the count already is zero, then it is NOT decremented.

  If the reference count drops to zero or already is zero and argument CanFree
  is set to True, then the function also frees and removes the variable before
  returning (equivalent to calling GlobVarFree).

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised.

  Note that overload accepting variable reference is not provided because
  the reference cannot provide all information necessary for freeing.
}
Function GlobVarRelease(Identifier: TPGVIdentifier; CanFree: Boolean = False): UInt32; overload;
Function GlobVarRelease(const Identifier: String; CanFree: Boolean = False): UInt32; overload;

//------------------------------------------------------------------------------
{
  GlobVarAllocate
  GlobVarAlloc

  Allocates variable of given identifier and size.

  If variable with given identifier already exists, then an exception of class
  EPGVDuplicateVariable is raised.

  If size is set to zero, then no variable is allocated and the function will
  return nil (note that even in this case, the function will raise an exception
  if the variable already exists).

  Memory of the newly allocated variable is initialized to all zero.
}
Function GlobVarAllocate(Identifier: TPGVIdentifier; Size: TMemSize): TPGVVariable; overload;
Function GlobVarAllocate(const Identifier: String; Size: TMemSize): TPGVVariable; overload;

Function GlobVarAlloc(Identifier: TPGVIdentifier; Size: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function GlobVarAlloc(const Identifier: String; Size: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}

{
  GlobVarReallocate
  GlobVarRealloc

  Operation of these functions depends on whether the requested variable exists
  and also on the value of paramer NewSize.

  Variable exists

    If NewSize is above zero, then the variable is reallocated. If NewSize is
    the same as current size, then this reallocation does nothing. If NewSize
    is larger than current size, then the newly added memory space is zeroed.
    And finally, if NewSize is smaller than current size, then the data are
    truncated to fit the new size.

    When NewSize is zero then the variable is freed (equivalent to calling
    GlobVarFree - variable's reference count is ignored).

      NOTE - when the reallocation is performed, the variable's data might be
             relocated to a different memory address (whether this happens or
             not depends on many things, none of which is important here, you
             just remember that it CAN happen).

  Variable does not exist

    If NewSize is above zero, then new variable of given identifier and size is
    allocated (equivalent to calling GlobVarAllocate).

    When NewSize is zero, then nil is returned and nothing is allocated.
}
Function GlobVarReallocate(Identifier: TPGVIdentifier; NewSize: TMemSize): TPGVVariable; overload;
Function GlobVarReallocate(const Identifier: String; NewSize: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function GlobVarRealloc(Identifier: TPGVIdentifier; NewSize: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function GlobVarRealloc(const Identifier: String; NewSize: TMemSize): TPGVVariable; overload;{$IFDEF CanInline} inline;{$ENDIF}

{
  GlobVarFree

  Frees memory allocated for a given variable and removes it from the internal
  state - so it cannot be obtained or accessed again.

  If no variable of given name exists, then an EPGVUnknownVariable exception is
  raised.

    WARNING - if you still have references to removed variables, they will be
              corrupted, or, in the worst case, will point to a different
              variable. Therefore make sure you discard all existing references
              to variable that is being freed.

    WARNING - this function ignores variable's reference count and always
              frees it.
}
procedure GlobVarFree(Identifier: TPGVIdentifier); overload;
procedure GlobVarFree(const Identifier: String); overload;

//------------------------------------------------------------------------------
{
  GlobVarStore

  Writes up-to Count bytes from the provided buffer (Buffer) to a memory of
  given variable.

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier).

  If the variable is smaller than is Count, then only number of bytes
  corresponding to actual variable's size will be written.

  If the variable is larger than is the Count, then bytes beyond the Count are
  not affected.

  Overload accepting variable reference can also raise an EPGVInvalidVariable
  exception if the reference is not valid.

    NOTE - this function locks the internal state while writing, so you do not
           need to do it explicitly.
}
Function GlobVarStore(Identifier: TPGVIdentifier; const Buffer; Count: TMemSize): TMemSize; overload;
Function GlobVarStore(const Identifier: String; const Buffer; Count: TMemSize): TMemSize; overload;
Function GlobVarStore(Variable: TPGVVariable; const Buffer; Count: TMemSize): TMemSize; overload;

{
  GlobVarLoad

  Reads up to Count bytes from the requested variable into the provided Buffer.
  The buffer must be prepared by the caller and must be large enough to hold
  at least Count number of bytes.

  If the requested variable does not exist, then an EPGVUnknownVariable
  exception will be raised (overloads accepting indetifier).

  If the variable is smaller than is Count, then only number of bytes
  corresponding to actual variable's size will be read. Content of buffer
  beyond actually copied data is unaffected.

  If the variable is larger than is the Count, then only Count bytes will be
  read (filling the buffer), data beyond that are not copied and are not
  affected (but in rare circumstances might be accessed by the function).

  Overload accepting variable reference can also raise an EPGVInvalidVariable
  exception if the reference is not valid.

    NOTE - this function locks the internal state while reading, so you do not
           need to do it explicitly.
}
Function GlobVarLoad(Identifier: TPGVIdentifier; out Buffer; Count: TMemSize): TMemSize; overload;
Function GlobVarLoad(const Identifier: String; out Buffer; Count: TMemSize): TMemSize; overload;
Function GlobVarLoad(Variable: TPGVVariable; out Buffer; Count: TMemSize): TMemSize; overload;

implementation

uses
  {$IFDEF Windows}Windows,{$ELSE}UnixType, BaseUnix,{$ENDIF}
  Adler32, AuxMath, StrRect;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W4056:={$WARN 4056 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                           PGV internal implementation
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    PGV internal implementation - constants and types
===============================================================================}
type
  TPGVHead = packed record
    RefCount:     Integer;
    Flags:        UInt32;
  {$IFDEF Windows}
    Lock:         TRTLCriticalSection;
  {$ELSE}
    Lock:         pthread_mutex_t;
    Allocator:    record
      LibHandle:    Pointer;
      AllocFunc:    Function(size: size_t): Pointer; cdecl;
      ReallocFunc:  Function(ptr: Pointer; size: size_t): Pointer; cdecl;
      FreeFunc:     procedure(ptr: Pointer); cdecl;
    end;
  {$ENDIF}
    FirstSegment: Pointer;
    LastSegment:  Pointer;
  end;
  PPGVHead = ^TPGVHead;

var
  // main global variable
  VAR_HeadPtr:  PPGVHead = nil;

//------------------------------------------------------------------------------
type
  TPGVSegmentHead = packed record
    Flags:        UInt32;
    AllocCount:   Integer;
    NextSegment:  Pointer;
    PrevSegment:  Pointer;
  end;

  TPGVSegmentEntry = packed record
    Identifier: TPGVIdentifier;
    Flags:      UInt32;
    RefCount:   UInt32;
    Address:    Pointer;
    Size:       TMemSize;    
  end;
  PPGVSegmentEntry = ^TPGVSegmentEntry;

const
  // SEFLAG = segment entry flag
  PGV_SEFLAG_USED        = UInt32($00000100);
  PGV_SEFLAG_REALLOCATED = UInt32($00000200);
  PGV_SEFLAG_RENAMED     = UInt32($00000400);

  PGV_SEFLAG_SMLSIZE_MASK = UInt32($0000000F);

//------------------------------------------------------------------------------
const  
  PGV_SEGMENT_SIZE = 4096;  // one memory page, usually

  // helper constants (so that further declarations are shorter)
  PGV_SEGMENT_ENTRYCOUNT  = (PGV_SEGMENT_SIZE - SizeOf(TPGVSegmentHead)) div SizeOf(TPGVSegmentEntry);
  PGV_SEGMENT_ENTRIESSIZE = PGV_SEGMENT_ENTRYCOUNT * SizeOf(TPGVSegmentEntry);
  PGV_SEGMENT_PADDINGSIZE = PGV_SEGMENT_SIZE - PGV_SEGMENT_ENTRIESSIZE - SizeOf(TPGVSegmentHead);

type
  TPGVSegment = packed record
    Head:     TPGVSegmentHead;
    // add padding to ensure the type has exactly segment-size bytes
  {$IF PGV_SEGMENT_PADDINGSIZE > 0}
    Padding:  packed array[0..Pred(PGV_SEGMENT_PADDINGSIZE)] of Byte;
  {$IFEND}
    Entries:  packed array[0..Pred(PGV_SEGMENT_ENTRYCOUNT)] of TPGVSegmentEntry;
  end;
  PPGVSegment = ^TPGVSegment;

{$IF SizeOf(TPGVSegment) <> PGV_SEGMENT_SIZE}
  {$MESSAGE FATAL 'Invalid size of type TPGVSegment.'}
{$IFEND}

{===============================================================================
    PGV internal implementation - externals, system stuff
===============================================================================}
{$IFDEF Windows}
// Windows...
const
  HEAP_ZERO_MEMORY = $00000008;

Function EnumProcessModules(hProcess: THandle; lphModules: PHandle; cb: DWORD; lpcbNeeded: LPDWORD): BOOL; stdcall; external 'psapi.dll';

{$ELSE}//=======================================================================
// Linux...
type
  pthread_mutexattr_p = ^pthread_mutexattr_t;
  pthread_mutex_p = ^pthread_mutex_t;

const
  PTHREAD_MUTEX_RECURSIVE = 1;
  PTHREAD_MUTEX_ROBUST    = 1;

Function pthread_mutexattr_init(attr: pthread_mutexattr_p): cint; cdecl; external;
Function pthread_mutexattr_destroy(attr: pthread_mutexattr_p): cint; cdecl; external;
Function pthread_mutexattr_settype(attr: pthread_mutexattr_p; _type: cint): cint; cdecl; external;
Function pthread_mutexattr_setrobust(attr: pthread_mutexattr_p; robustness: cint): cint; cdecl; external;

Function pthread_mutex_init(mutex: pthread_mutex_p; attr: pthread_mutexattr_p): cint; cdecl; external;
Function pthread_mutex_destroy(mutex: pthread_mutex_p): cint; cdecl; external;

Function pthread_mutex_trylock(mutex: pthread_mutex_p): cint; cdecl; external;
Function pthread_mutex_lock(mutex: pthread_mutex_p): cint; cdecl; external;
Function pthread_mutex_unlock(mutex: pthread_mutex_p): cint; cdecl; external;
Function pthread_mutex_consistent(mutex: pthread_mutex_p): cint; cdecl; external;

//------------------------------------------------------------------------------
const
  libc = 'libc.so.6';

  RTLD_LAZY = $001;
  RTLD_NOW  = $002;

Function errno_ptr: pcint; cdecl; external name '__errno_location';

Function lin_malloc(size: size_t): Pointer; cdecl; external libc name 'malloc';
Function lin_realloc(ptr: Pointer; size: size_t): Pointer; cdecl; external libc name 'realloc';
procedure lin_free(ptr: Pointer); cdecl; external libc name 'free';

Function dlopen(filename: PChar; flags: cInt): Pointer; cdecl; external;
Function dlclose(handle: Pointer): cInt; cdecl; external;
Function dlsym(handle: Pointer; symbol: PChar): Pointer; cdecl; external;
Function dlerror: PChar; cdecl; external;

//------------------------------------------------------------------------------
type
  dl_phdr_info = record
    dlpi_addr:      PtrUInt;
    dlpi_name:      PChar;
    dlpi_phdr:      Pointer;  // structure, but we do not need it
    dlpi_phnum:     UInt16;
    dlpi_adds:      cuLongLong;
    dlpi_subs:      cuLongLong;
    dlpi_tls_modid: size_t;
    dlpi_tls_data:  Pointer;
  end;
  dl_phdr_info_p = ^dl_phdr_info;

  TDLIterCallback = Function(info: dl_phdr_info_p; size: size_t; data: Pointer): cInt; cdecl;

Function dl_iterate_phdr(callback: TDLIterCallback; data: Pointer): cInt; cdecl; external;

//------------------------------------------------------------------------------
threadvar
  ThrErrorCode: cInt;

Function PThrResChk(RetVal: cInt): Boolean;
begin
Result := RetVal = 0;
If Result then
  ThrErrorCode := 0
else
  ThrErrorCode := RetVal;
end;

{$ENDIF}

{===============================================================================
    PGV internal implementation - exports
===============================================================================}
type
{
  Type TPGVGetHeadFunc must EXACTLY match prototype of ProcessGlobalVarsGetHead
  (or vice-versa, your choice). And do not ever change it - rather introduce
  new function if different call is needed.
}
  TPGVGetHeadFunc = Function(Version: Int32): PPGVHead;{$IFDEF Windows} stdcall;{$ELSE} cdecl;{$ENDIF}

const
  PGV_EXPORTNAME_GETHEAD = 'ProcessGlobalVarsGetHead';

  PGV_VERSION_CURRENT = Int32(1){$IFDEF CPU64bit} or Int32(UInt32(1) shl 31){$ENDIF};

//------------------------------------------------------------------------------

Function ProcessGlobalVarsGetHead(Version: Int32): PPGVHead;{$IFDEF Windows} stdcall;{$ELSE} cdecl;{$ENDIF}{$IFDEF FPC} public;{$ENDIF}
begin
If Version = PGV_VERSION_CURRENT then
  Result := VAR_HeadPtr
else
  Result := nil;
end;

//------------------------------------------------------------------------------
exports
  ProcessGlobalVarsGetHead name PGV_EXPORTNAME_GETHEAD;

{===============================================================================
    PGV internal implementation - thread protection
===============================================================================}

procedure ThreadLockInit;
{$IFDEF Windows}
begin
InitializeCriticalSection(VAR_HeadPtr^.Lock);
end;
{$ELSE}
var
  MutexAttr:  pthread_mutexattr_t;
begin
If PThrResChk(pthread_mutexattr_init(@MutexAttr)) then
  try
    // make the mutex recursive and robust (it does not need to be process-shared)
    If not PThrResChk(pthread_mutexattr_settype(@MutexAttr,PTHREAD_MUTEX_RECURSIVE)) then
      raise EPGVMutexError.CreateFmt('ThreadLockInit: Failed to set mutex attribute type (%d).',[ThrErrorCode]);
    If not PThrResChk(pthread_mutexattr_setrobust(@MutexAttr,PTHREAD_MUTEX_ROBUST)) then
      raise EPGVMutexError.CreateFmt('ThreadLockInit: Failed to set mutex attribute robust (%d).',[ThrErrorCode]);
    If not PThrResChk(pthread_mutex_init(@VAR_HeadPtr^.Lock,@MutexAttr)) then
      raise EPGVMutexError.CreateFmt('ThreadLockInit: Failed to init mutex (%d).',[ThrErrorCode]);
  finally
    pthread_mutexattr_destroy(@MutexAttr);
  end
else raise EPGVMutexError.CreateFmt('ThreadLockInit: Failed to init mutex attributes (%d).',[ThrErrorCode]);
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure ThreadLockFinal;
begin
{$IFDEF Windows}
DeleteCriticalSection(VAR_HeadPtr^.Lock);
{$ELSE}
If not PThrResChk(pthread_mutex_destroy(@VAR_HeadPtr^.Lock)) then
  raise EPGVMutexError.CreateFmt('ThreadLockFinal: Failed to destroy mutex (%d).',[ThrErrorCode]);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TryThreadLock: Boolean;
{$IFDEF Windows}
begin
Result := TryEnterCriticalSection(VAR_HeadPtr^.Lock);
end;
{$ELSE}
var
  RetVal: cInt;
begin
RetVal := pthread_mutex_trylock(@VAR_HeadPtr^.Lock);
If RetVal = ESysEOWNERDEAD then
  begin
    If not PThrResChk(pthread_mutex_consistent(@VAR_HeadPtr^.Lock)) then
      raise EPGVMutexError.CreateFmt('TryThreadLock: Failed to make mutex consistent (%d).',[ThrErrorCode]);
    Result := True;
  end
else
  begin
    Result := PThrResChk(RetVal);
    If not Result and (RetVal <> ESysEBUSY) then
      raise EPGVMutexError.CreateFmt('TryThreadLock: Failed to try-lock mutex (%d).',[ThrErrorCode]);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure ThreadLock;
{$IFDEF Windows}
begin
EnterCriticalSection(VAR_HeadPtr^.Lock);
end;
{$ELSE}
var
  RetVal: cInt;
begin
RetVal := pthread_mutex_lock(@VAR_HeadPtr^.Lock);
If RetVal = ESysEOWNERDEAD then
  begin
    If not PThrResChk(pthread_mutex_consistent(@VAR_HeadPtr^.Lock)) then
      raise EPGVMutexError.CreateFmt('ThreadLock: Failed to make mutex consistent (%d).',[ThrErrorCode]);
  end
else If not PThrResChk(RetVal) then
  raise EPGVMutexError.CreateFmt('ThreadLock: Failed to lock mutex (%d).',[ThrErrorCode]);
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure ThreadUnlock;
begin
{$IFDEF Windows}
LeaveCriticalSection(VAR_HeadPtr^.Lock);
{$ELSE}
If not PThrResChk(pthread_mutex_unlock(@VAR_HeadPtr^.Lock)) then
  raise EPGVMutexError.CreateFmt('ThreadUnlock: Failed to unlock mutex (%d).',[ThrErrorCode]);
{$ENDIF}
end;

{===============================================================================
    PGV internal implementation - global memory management
===============================================================================}

procedure GlobalMemoryInit;
begin
{$IFDEF Windows}
// nothing to do here
{$ELSE}
{
  Unlike in Windows, where the memory manager is provided by the system, in
  Linux it is from glibc (a library). To ensure that everyone is using the
  same manager from the same library, we store the allocation interface (the
  pointers to functions) in global state and use it from there.

  Also, to make sure the library managing the memory is not unloaded too soon,
  we open it to increment its reference count. It is then closed when the last
  instance is finalized.
}
with VAR_HeadPtr^.Allocator do
  begin
    LibHandle := dlopen(libc,RTLD_LAZY);
    If not Assigned(LibHandle) then
      raise EPGVSystemError.CreateFmt('GlobalMemoryInit: Failed to open allocating library (%s).',[dlerror]);
    AllocFunc := @lin_malloc;
    ReallocFunc := @lin_realloc;
    FreeFunc := @lin_free;
  end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure GlobalMemoryFinal;
begin
{$IFDEF Windows}
// nothing to do here
{$ELSE}
If dlclose(VAR_HeadPtr^.Allocator.LibHandle) <> 0 then
  raise EPGVSystemError.CreateFmt('GlobalMemoryFinal: Failed to close allocating library (%s).',[dlerror]);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GlobalMemoryAllocate(Size: TMemSize): Pointer;
begin
{$IFDEF Windows}
Result := HeapAlloc(GetProcessHeap,HEAP_ZERO_MEMORY,Size);
If not Assigned(Result) then
  raise EPGVHeapAllocationError.Create('GlobalMemoryAllocate: Failed to allocate global memory.');
{$ELSE}
Result := VAR_HeadPtr^.Allocator.AllocFunc(size_t(Size));
If Assigned(Result) then
  FillChar(Result^,Size,0)
else
  raise EPGVHeapAllocationError.CreateFmt('GlobalMemoryAllocate: Failed to allocate global memory (%d).',[errno_ptr^]);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure GlobalMemoryReallocate(var Address: Pointer; NewSize: TMemSize);
begin
{$IFDEF Windows}
Address := HeapRealloc(GetProcessHeap,HEAP_ZERO_MEMORY,Address,NewSize);
If not Assigned(Address) then
  raise EPGVHeapAllocationError.Create('GlobalMemoryReallocate: Failed to reallocate global memory.');
{$ELSE}
Address := VAR_HeadPtr^.Allocator.ReallocFunc(Address,size_t(NewSize));
If not Assigned(Address) then
  raise EPGVHeapAllocationError.CreateFmt('GlobalMemoryReallocate: Failed to reallocate global memory (%d).',[errno_ptr^]);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure GlobalMemoryFree(var Address: Pointer);
begin
{$IFDEF Windows}
If not HeapFree(GetProcessHeap,0,Address) then
  raise EPGVHeapAllocationError.CreateFmt('GlobalMemoryFree: Could not free global memory (%d).',[GetLastError]);
Address := nil;
{$ELSE}
VAR_HeadPtr^.Allocator.FreeFunc(Address);
Address := nil;
{$ENDIF}
end;

{===============================================================================
    PGV internal implementation - module initialization
===============================================================================}
type
{$IFDEF Windows}
  TPGVModuleArray = array of THandle;
{$ELSE}
  TPGVModuleArray = record
    Arr:    array of String;
    Count:  Integer;
  end;
  PPGVModuleArray = ^TPGVModuleArray;
{$ENDIF}

//------------------------------------------------------------------------------
{$IFDEF Windows}
{$IFNDEF FPC}
var
  PrevDllProc:  Pointer;
  DLLParam:     PtrInt;

//------------------------------------------------------------------------------
{
  Reserved should be a pointer, but meh - this thing is one giant mess in
  existing compilers...

    In old Delphi, Reserved is Integer. In new ones, it is Pointer.

    FPC has global variable DLLParam, which holds value of Reserved. And it is
    declared as PtrInt.

    bla bla bla... >:/

  This should work everywhere, if the compiler (and RTL code) is sane. If not,
  please let me know.
}
procedure ProcessGlobalVarsDllProc(Reason: Integer; Reserved: PtrInt);
type
  TLocalDLLProc = procedure(Reason: Integer; Reserved: PtrInt);
begin
DLLParam := Reserved;
If Assigned(PrevDllProc) then
  TLocalDLLProc(PrevDllProc)(Reason,Reserved);
end;
{$ENDIF}

{$ELSE}//-linux-----------------------------------------------------------------

Function ModuleEnumCallback(info: dl_phdr_info_p; size: size_t; data: Pointer): cInt; cdecl;
begin
If Assigned(info) and (size >= (2 * SizeOf(Pointer))) then
  begin
    If PPGVModuleArray(Data)^.Count <= Length(PPGVModuleArray(Data)^.Arr) then
      SetLength(PPGVModuleArray(Data)^.Arr,Length(PPGVModuleArray(Data)^.Arr) + 16);
    PPGVModuleArray(Data)^.Arr[PPGVModuleArray(Data)^.Count] := String(info^.dlpi_name);
    Inc(PPGVModuleArray(Data)^.Count);
  end;
Result := 0;
end;

{$ENDIF}
//------------------------------------------------------------------------------

procedure EnumerateProcessModules(out Modules: TPGVModuleArray);
{$IFDEF Windows}
var
  BytesNeeded:  DWORD;
begin
Modules := nil;
BytesNeeded := 1024 * SizeOf(THandle);
repeat
  SetLength(Modules,BytesNeeded);
  If not EnumProcessModules(GetCurrentProcess,Addr(Modules[Low(Modules)]),Length(Modules) * SizeOf(THandle),@BytesNeeded) then
    raise EPGVModuleEnumerationError.CreateFmt('EnumerateProcessModules: Failed to enumerate process modules (%d).',[GetLastError]);
until DWORD(Length(Modules) * SizeOf(THandle)) >= BytesNeeded;
// limit length to what is really enumerated
SetLength(Modules,BytesNeeded div SizeOf(THandle));
end;
{$ELSE}
begin
Modules.Arr := nil;
Modules.Count := 0;
dl_iterate_phdr(@ModuleEnumCallback,@Modules);
SetLength(Modules.Arr,Modules.Count);
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure ModuleInitialization;
var
  Modules:      TPGVModuleArray;
  i:            Integer;
  GetHeadFunc:  Pointer;
  HeadTemp:     PPGVHead;
{$IFNDEF Windows}
  ProbedMod:    Pointer;
  LocalHead:    TPGVHead;
{$ENDIF}
begin
{
  NOTE - this function is called when a module is loaded - this loading is
         serialized by the OS, so there is no need for synchronization.
}
{$IFDEF Windows}{$IFNDEF FPC}
{
  Setup dllmain hook to intercept lpvReserved parameter when dll is detached
  (see ModuleFinalization for details).

  Also note that the hook is called BEFORE finalization of all units, including
  this one (at least that is the documented behaviour).
}
PrevDllProc := @SysInit.DllProcEx;
SysInit.DllProcEx := TDLLProcEx(@ProcessGlobalVarsDllProc);
{$ENDIF}{$ENDIF}
// enumerate modules loaded within this process...
EnumerateProcessModules(Modules);
{
  Traverse all loaded modules and look whether they export a properly named
  function.

  If this function is found, call it - when it returns nil, it means it is
  incompatible with this implementation and continue searching.
  When it returns non-nil pointer, use that for head pointer global variable,
  increment its reference counter and exit.

  If no function of that name is found or all exported functions return nil, do
  full initialization (allocation) here.

  Windows...

    Note that the modules contain the current module - this is not a problem
    since calling local *GetHead function will just return nil (as the global
    variable is not yet initilized) and therefore it will be ignored.
}
{$IFDEF Windows}
For i := Low(Modules) to High(Modules) do
  begin
    GetHeadFunc := GetProcAddress(Modules[i],PGV_EXPORTNAME_GETHEAD);
    If Assigned(GetHeadFunc) then
      begin
        HeadTemp := TPGVGetHeadFunc(GetHeadFunc)(PGV_VERSION_CURRENT);
        If Assigned(HeadTemp) then
          begin
            Inc(HeadTemp^.RefCount);
            VAR_HeadPtr := HeadTemp;
            // we have everything we need, do not continue this function
            Exit;
          end;
      end;
  end;
{$ELSE}
For i := Low(Modules.Arr) to Pred(Modules.Count) do
  begin
    ProbedMod := dlopen(PChar(Modules.Arr[i]),RTLD_NOW);
    If Assigned(ProbedMod) then
      try
        GetHeadFunc := dlsym(ProbedMod,PGV_EXPORTNAME_GETHEAD);
        If Assigned(GetHeadFunc) then
          begin
            HeadTemp := TPGVGetHeadFunc(GetHeadFunc)(PGV_VERSION_CURRENT);
            If Assigned(HeadTemp) then
              begin
                Inc(HeadTemp^.RefCount);
                VAR_HeadPtr := HeadTemp;
                Exit;
              end;
          end;
      finally
        If dlclose(ProbedMod) <> 0 then
          raise EPGVSystemError.CreateFmt('ModuleInitialization: Failed to close probed module (%s).',[dlerror]);
      end
    else raise EPGVSystemError.CreateFmt('ModuleInitialization: Failed to open probed module (%s).',[dlerror]);
  end;
{$ENDIF}
{
  If we are here it means no currently loaded module provides usable PGV, we
  need to allocete it ourselves.

  Linux...

    The head is allocated using global memory allocation routines, but these
    require that the head is already prepared - catch 22. We use local variable
    to provide what the allocation rutine require.
}
{$IFNDEF Windows}
LocalHead.Allocator.AllocFunc := @lin_malloc;
VAR_HeadPtr := @LocalHead;
{$ENDIF}
VAR_HeadPtr := GlobalMemoryAllocate(SizeOf(TPGVHead));
// initialize head fields (fields not explicitly touched here are zeroed)
VAR_HeadPtr^.RefCount := 1;
GlobalMemoryInit;
ThreadLockInit;
end;

{===============================================================================
    PGV internal implementation - module finalization
===============================================================================}

procedure FreeAllVariablesAndSegments;
var
  CurrentSegment: PPGVSegment;
  NextSegment:    PPGVSegment;
  i:              Integer;
begin
{
  We are last module using current VAR_HeadPtr, there should be no other threads
  able to access it by this point, but we still enter critical section to be
  sure. But, since this call is serialized by OS, we cannot block - that could
  create deadlock - instead non blocking try-enter is used here.

  If we cannot acquire the lock, just leave - its better to leak memory than
  raising an exception during library detach.
}
If TryThreadLock then
  try
    CurrentSegment := VAR_HeadPtr^.FirstSegment;
    VAR_HeadPtr^.FirstSegment := nil;
    while Assigned(CurrentSegment) do
      begin
      {
        Free individual entries (variables) - note there is no need to clear
        individual entries as the entire segment will be freed.
      }
        For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
          If ((CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0) and
             ((CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0) then
            GlobalMemoryFree(CurrentSegment^.Entries[i].Address);
        // free the segment
        NextSegment := CurrentSegment^.Head.NextSegment;
        GlobalMemoryFree(Pointer(CurrentSegment));
        CurrentSegment := NextSegment;
      end;
  finally
    ThreadUnlock;
  end;
end;

//------------------------------------------------------------------------------

procedure ModuleFinalization;
var
  LocalHead:  TPGVHead;
begin
{
  NOTE - this function is called when a module is unloaded, which is serialized
  on a process-wide basis by the OS, so there is no need for synchronization.

  Windows...

    Only call cleanup when a library is explicitly unloaded (parameter Reserved
    (here stored in DLLParam) in DLLMain is null/nil), do not call it when the
    process is terminating - freeing the heap then would cause an error
    (DbgBreakPoint is called by kernel).
}
If Assigned(VAR_HeadPtr){$IFDEF Windows} and (DLLParam = 0){$ENDIF} then
  begin
    Dec(VAR_HeadPtr^.RefCount);
    If VAR_HeadPtr^.RefCount <= 0 then
      begin
        FreeAllVariablesAndSegments;
        ThreadLockFinal;
        VAR_HeadPtr^.RefCount := 0;
        LocalHead := VAR_HeadPtr^;
        GlobalMemoryFree(Pointer(VAR_HeadPtr));
        VAR_HeadPtr := @LocalHead;
        GlobalMemoryFinal;
        VAR_HeadPtr := nil;
      end;
  end;
end;

{===============================================================================
    PGV internal implementation - segments management
===============================================================================}

Function SegmentAdd: PPGVSegment;
begin
{
  Why adding PGV_SEGMENT_PADDINGSIZE when it is zero? To stop FPC bitching
  about constant not being used... uuaaa!
}
Result := GlobalMemoryAllocate(SizeOf(TPGVSegment){$IF PGV_SEGMENT_PADDINGSIZE = 0} + PGV_SEGMENT_PADDINGSIZE{$IFEND});
If Assigned(VAR_HeadPtr^.FirstSegment) then
  begin
    // there are some segments already allocated
    Result^.Head.PrevSegment := VAR_HeadPtr^.LastSegment;
    PPGVSegment(VAR_HeadPtr^.LastSegment)^.Head.NextSegment := Result;
  end
// adding first segment
else VAR_HeadPtr^.FirstSegment := Result;
VAR_HeadPtr^.LastSegment := Result;
end;

//------------------------------------------------------------------------------

procedure SegmentRemove(Segment: PPGVSegment);
var
  Temp: Pointer;
begin
If Assigned(Segment^.Head.PrevSegment) then
  PPGVSegment(Segment^.Head.PrevSegment)^.Head.NextSegment := Segment^.Head.NextSegment;
If Assigned(Segment^.Head.NextSegment) then
  PPGVSegment(Segment^.Head.NextSegment)^.Head.PrevSegment := Segment^.Head.PrevSegment;
If VAR_HeadPtr^.FirstSegment = Segment then
  VAR_HeadPtr^.FirstSegment := Segment^.Head.NextSegment;
If VAR_HeadPtr^.LastSegment = Segment then
  VAR_HeadPtr^.LastSegment := Segment^.Head.PrevSegment;
Temp := Pointer(Segment);
GlobalMemoryFree(Temp);
end;

{===============================================================================
    PGV internal implementation - entries management and utility
===============================================================================}

Function EntryFromVar(Variable: TPGVVariable): PPGVSegmentEntry;
begin
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
Result := PPGVSegmentEntry(Pointer(PtrUInt(Variable) - PtrUInt(Addr(TPGVSegmentEntry(nil^).Address))));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

// I just don't want to put EntrySize before EntryIsValid, ok?!
Function EntrySize(Entry: PPGVSegmentEntry): TMemSize; forward;

Function EntryIsValid(Entry: PPGVSegmentEntry): Boolean;
begin
Result := ((Entry^.Flags and PGV_SEFLAG_USED) <> 0) and Assigned(Entry^.Address) and (EntrySize(Entry) <> 0);
end;

//------------------------------------------------------------------------------

Function EntryIsValidAddress(Entry: PPGVSegmentEntry): Boolean;
var
  CurrentSegment: PPGVSegment;
  Index:          Integer;
begin
Result := False;
CurrentSegment := VAR_HeadPtr^.FirstSegment;
while Assigned(CurrentSegment) do
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    with CurrentSegment^ do
      If (PtrUInt(Entry) >= PtrUInt(Addr(Entries[Low(Entries)]))) and
         (PtrUInt(Entry) <= PtrUInt(Addr(Entries[High(Entries)]))) then
        begin
          Index := (PtrUInt(Entry) - PtrUInt(Addr(Entries[Low(Entries)]))) div SizeOf(TPGVSegmentEntry);
          If (Index >= Low(Entries)) and (Index <= High(Entries)) then
            Result := Entry = Addr(Entries[Index]);
          // it the entry is here, it cannot be in any other segment
          Break{while};
        end;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    CurrentSegment := CurrentSegment^.Head.NextSegment;
  end;
end;

//------------------------------------------------------------------------------

Function EntryDecodeFlags(Entry: PPGVSegmentEntry): TPGVVariableFlags;
begin
Result := [];
If (Entry^.Flags and PGV_SEFLAG_REALLOCATED) <> 0 then
  Include(Result,vfReallocated);
If (Entry^.Flags and PGV_SEFLAG_RENAMED) <> 0 then
  Include(Result,vfRenamed);
end;

//------------------------------------------------------------------------------

procedure EntryEncodeFlags(Entry: PPGVSegmentEntry; Flags: TPGVVariableFlags);

  procedure SetFlagValue(FlagMask: UInt32; Value: Boolean);
  begin
    If Value then
      Entry^.Flags := Entry^.Flags or FlagMask
    else
      Entry^.Flags := Entry^.Flags and not FlagMask;
  end;

begin
SetFlagValue(PGV_SEFLAG_REALLOCATED,vfReallocated in Flags);
SetFlagValue(PGV_SEFLAG_RENAMED,vfRenamed in Flags);
end;

//------------------------------------------------------------------------------

Function EntrySetFlag(Entry: PPGVSegmentEntry; Flag: TPGVVariableFlag; Value: Boolean): Boolean;
var
  Flags:  TPGVVariableFlags;
begin
Flags := EntryDecodeFlags(Entry);
Result := Flag in Flags;
If Value then
  Include(Flags,Flag)
else
  Exclude(Flags,Flag);
EntryEncodeFlags(Entry,Flags);
end;

//------------------------------------------------------------------------------

Function EntrySize(Entry: PPGVSegmentEntry): TMemSize;
begin
If (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) <> 0 then
  Result := TMemSize(Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK)
else
  Result := Entry^.Size;
end;

//------------------------------------------------------------------------------

Function EntryFind(Identifier: TPGVIdentifier; out Segment: PPGVSegment; out Entry: PPGVSegmentEntry): Boolean;
var
  CurrentSegment: PPGVSegment;
  i:              Integer;
begin
// lock should be acquired by now
Segment := nil;
Entry := nil;
CurrentSegment := VAR_HeadPtr^.FirstSegment;
while Assigned(CurrentSegment) and not (Assigned(Segment) and Assigned(Entry)) do
  begin
    If CurrentSegment^.Head.AllocCount > 0 then
      For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
        If ((CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0) and
          (CurrentSegment^.Entries[i].Identifier = Identifier) then
          begin
            Segment := CurrentSegment;
            Entry := Addr(CurrentSegment^.Entries[i]);
            // while cycle will break too because both Segment and Entry are assigned
            Break{For i};
          end;
    CurrentSegment := CurrentSegment^.Head.NextSegment;
  end;
Result := Assigned(Segment) and Assigned(Entry);
end;

//------------------------------------------------------------------------------

procedure EntryRename(Entry: PPGVSegmentEntry; NewIdentifier: TPGVIdentifier);
var
  CheckSegment: PPGVSegment;
  CheckEntry:   PPGVSegmentEntry;
begin
If EntryFind(NewIdentifier,CheckSegment,CheckEntry) then
  begin
    If CheckEntry = Entry then
      Exit  // given variable already has the requested identifier
    else
      raise EPGVDuplicateVariable.CreateFmt('EntryRename: Variable 0x%.8x already exists.',[NewIdentifier]);
  end;
// entry of given new identifier does not exist, do the renaming
Entry^.Identifier := NewIdentifier;
Entry^.Flags := Entry^.Flags or PGV_SEFLAG_RENAMED;
end;

//------------------------------------------------------------------------------

procedure EntryAllocate(Identifier: TPGVIdentifier; Size: TMemSize; out Segment: PPGVSegment; out Entry: PPGVSegmentEntry);
var
  CurrentSegment: PPGVSegment;
  i:              Integer;
begin
{
  Check whether the entry already exists should have been performed prior
  calling this function.
}
If Size <= 0 then
  raise EPGVInvalidValue.CreateFmt('EntryAllocate: Invalid size (%u).',[Size]);
Segment := nil;
Entry := nil;
// find segment with unused entry
CurrentSegment := VAR_HeadPtr^.FirstSegment;
while Assigned(CurrentSegment) and not (Assigned(Segment) and Assigned(Entry)) do
  begin
    If CurrentSegment^.Head.AllocCount < Length(CurrentSegment^.Entries) then
      For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
        If ((CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) = 0) then
          begin
            Segment := CurrentSegment;
            Entry := Addr(Segment^.Entries[i]);
            Inc(Segment^.Head.AllocCount);
            Break{For i};
          end;
    CurrentSegment := CurrentSegment^.Head.NextSegment;
  end;
// if no unused entry was found, allocate new segment and get entry there
If not Assigned(Segment) or not Assigned(Entry) then
  begin
    Segment := SegmentAdd;
    Entry := Addr(Segment^.Entries[Low(Segment^.Entries)]);
    Segment^.Head.AllocCount := 1;
  end;
// allocate the entry
Entry^.Identifier := Identifier;
Entry^.Flags := PGV_SEFLAG_USED;
Entry^.RefCount := 0;
If Size <= SizeOf(TMemSize) then
  begin
    // do not allocate on heap, the variable can fit into entry's Size field
    Entry^.Flags := Entry^.Flags or UInt32(Size and PGV_SEFLAG_SMLSIZE_MASK);
    Entry^.Address := Addr(Entry^.Size);
    Entry^.Size := 0;
  end
else
  begin
    // variable cannot be stored in-situ, allocate on heap
    Entry^.Address := GlobalMemoryAllocate(Size);
    Entry^.Size := Size;
  end;
end;

//------------------------------------------------------------------------------

procedure EntryReallocate(Segment: PPGVSegment; Entry: PPGVSegmentEntry; NewSize: TMemSize);
var
  OldSize:  TMemSize;
begin
If NewSize <= 0 then
  raise EPGVInvalidValue.CreateFmt('EntryRealloc: Invalid new size (%u).',[NewSize]);
// check whether the entry actually belongs to that segment
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Entry) > PtrUInt(Segment)) and ((PtrUInt(Entry) < (PtrUInt(Segment) + PGV_SEGMENT_SIZE))) then
{$IFDEF FPCDWM}{$POP}{$ENDIF}
  begin
    OldSize := EntrySize(Entry);
    If OldSize <> NewSize then
      begin
        If (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) <> 0 then
          begin
            // data are currently stored in Size field
            If NewSize <= SizeOf(TMemSize) then
              begin
              {
                Data will still fit to Size field - do not move anything, just
                zero upper memory when growing.
              }
                If NewSize > OldSize then
                {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                  FillChar(Pointer(PtrUInt(Addr(Entry^.Size)) + PtrUInt(OldSize))^,NewSize - OldSize,0);
                {$IFDEF FPCDWM}{$POP}{$ENDIF}
                Entry^.Flags := (Entry^.Flags and not PGV_SEFLAG_SMLSIZE_MASK) or UInt32(NewSize and PGV_SEFLAG_SMLSIZE_MASK);
              end
            else
              begin
                // data will be move to heap
                Entry^.Flags := Entry^.Flags and not PGV_SEFLAG_SMLSIZE_MASK;
                Entry^.Address := GlobalMemoryAllocate(NewSize);
                Move(Entry^.Size,Entry^.Address^,OldSize);
                Entry^.Size := NewSize;
              end;
          end
        else
          begin
            // data are somewhere on the heap
            If NewSize <= SizeOf(TMemSize) then
              begin
                // data will go to size field
                Move(Entry^.Address^,Entry^.Size,NewSize);
                GlobalMemoryFree(Entry^.Address);
                Entry^.Address := Addr(Entry^.Size);
                Entry^.Flags := (Entry^.Flags and not PGV_SEFLAG_SMLSIZE_MASK) or UInt32(NewSize and PGV_SEFLAG_SMLSIZE_MASK);
              end
            else
              begin
                // data stay on heap, just reallocate
                GlobalMemoryReallocate(Entry^.Address,NewSize);
              {$IFNDEF Windows}
                // Linux manager does not clear the newly allocated memory
                If NewSize > OldSize then
                {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                  FillChar(Pointer(PtrUInt(Entry^.Address) + PtrUInt(OldSize))^,NewSize - OldSize,0);
                {$IFDEF FPCDWM}{$POP}{$ENDIF}
              {$ENDIF}
                Entry^.Size := NewSize;
              end;
          end;
        Entry^.Flags := Entry^.Flags or PGV_SEFLAG_REALLOCATED;
      end;
  end
else raise EPGVInvalidValue.Create('EntryRealloc: Given entry does not belong to given segment.');
end;

//------------------------------------------------------------------------------

procedure EntryFree(Segment: PPGVSegment; Entry: PPGVSegmentEntry);
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Entry) > PtrUInt(Segment)) and ((PtrUInt(Entry) < (PtrUInt(Segment) + PGV_SEGMENT_SIZE))) then
{$IFDEF FPCDWM}{$POP}{$ENDIF}
  begin
    // free the entry
    Entry^.Size := 0;
    // free memory only if the variable is allocated on the heap
    If (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0 then
      GlobalMemoryFree(Entry^.Address); // sets the address to nil
    Entry^.Flags := 0;
    Entry^.RefCount := 0;
    Entry^.Identifier := 0;
    // update segment
    Dec(Segment^.Head.AllocCount);
    If Segment^.Head.AllocCount <= 0 then
      SegmentRemove(Segment);
  end
else raise EPGVInvalidValue.Create('EntryFree: Given entry does not belong to given segment.');
end;


{===============================================================================
--------------------------------------------------------------------------------
                       PGV public interface implementation
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    PGV public interface implementation - main implementation
===============================================================================}

Function GlobVarInternalCompatibilityVersion: Int32;
begin
Result := PGV_VERSION_CURRENT;
end;

//------------------------------------------------------------------------------

Function GlobVarTranslateIdentifier(const Identifier: String): TPGVIdentifier;
begin
Result := TPGVIdentifier(WideStringAdler32(StrToWide(Identifier)));
end;

//==============================================================================

procedure GlobVarLock;
begin
ThreadLock;
end;

//------------------------------------------------------------------------------

procedure GlobVarUnlock;
begin
ThreadUnlock;
end;

//==============================================================================

Function GlobVarCount: Integer;
var
  CurrentSegment: PPGVSegment;
begin
GlobVarLock;
try
  Result := 0;
  CurrentSegment := VAR_HeadPtr^.FirstSegment;
  while Assigned(CurrentSegment) do
    begin
      Inc(Result,CurrentSegment^.Head.AllocCount);
      CurrentSegment := CurrentSegment^.Head.NextSegment;
    end;
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

Function GlobVarMemory(IncludeOverhead: Boolean = False): TMemSize;
var
  CurrentSegment: PPGVSegment;
  i:              Integer;
begin
GlobVarLock;
try
  If IncludeOverhead then
    Result := SizeOf(TPGVHead)
  else
    Result := 0;
  CurrentSegment := VAR_HeadPtr^.FirstSegment;
  while Assigned(CurrentSegment) do
    begin
      If IncludeOverhead then
        begin
          Inc(Result,SizeOf(TPGVSegment));
          If CurrentSegment^.Head.AllocCount > 0 then
            For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
              If (CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0 then
                // count only entries that are allocated on heap, ignore those stored in-situ
                If (CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0 then
                  Inc(Result,CurrentSegment^.Entries[i].Size);
        end
      else
        begin
          If CurrentSegment^.Head.AllocCount > 0 then
            For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
              If (CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0 then
                Inc(Result,EntrySize(Addr(CurrentSegment^.Entries[i])));
        end;
      CurrentSegment := CurrentSegment^.Head.NextSegment;
    end;
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

Function GlobVarEnumerate: TPGVIdentifierArray;
var
  ResultIndex:    Integer;
  CurrentSegment: PPGVSegment;
  i:              Integer;
begin
Result := nil;
GlobVarLock;
try
  SetLength(Result,GlobVarCount);
  If Length(Result) > 0 then
    begin
      ResultIndex := Low(Result);
      CurrentSegment := VAR_HeadPtr^.FirstSegment;
      while Assigned(CurrentSegment) do
        begin
          If CurrentSegment^.Head.AllocCount > 0 then
            For i := Low(CurrentSegment^.Entries) to High(CurrentSegment^.Entries) do
              If (CurrentSegment^.Entries[i].Flags and PGV_SEFLAG_USED) <> 0 then
                begin
                  Result[ResultIndex] := CurrentSegment^.Entries[i].Identifier;
                  Inc(ResultIndex);
                end;
          CurrentSegment := CurrentSegment^.Head.NextSegment;
        end;
    end;
finally
  GlobVarUnlock;
end;
end;

//==============================================================================

Function GlobVarFind(Identifier: TPGVIdentifier; out Variable: TPGVVariable): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Variable := Addr(Entry^.Address)
  else
    Variable := nil;
  Result := Assigned(Variable);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarFind(const Identifier: String; out Variable: TPGVVariable): Boolean;
begin
Result := GlobVarFind(GlobVarTranslateIdentifier(Identifier),Variable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarFind(Identifier: TPGVIdentifier; out Variable: TPGVVariable; out Size: TMemSize): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
Variable := nil;
Size := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      Variable := Addr(Entry^.Address);
      Size := EntrySize(Entry);
      Result := True;
    end;
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarFind(const Identifier: String; out Variable: TPGVVariable; out Size: TMemSize): Boolean;
begin
Result := GlobVarFind(GlobVarTranslateIdentifier(Identifier),Variable,Size);
end;

//------------------------------------------------------------------------------

Function GlobVarGet(Identifier: TPGVIdentifier): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Result := Addr(Entry^.Address)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarGet: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarGet(const Identifier: String): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    Result := Addr(Entry^.Address)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarGet: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarGet(Identifier: TPGVIdentifier; var Size: TMemSize; out Variable: TPGVVariable): TPGVGetResult;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
  CurrSize: TMemSize;
begin
Variable := nil;
Result := vgrError;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      Variable := Addr(Entry^.Address);
      // entry exists, check its size and act accordingly
      CurrSize := EntrySize(Entry);
      If Size <> CurrSize then
        begin
          Size := CurrSize;
          Result := vgrSizeMismatch;
        end
      else Result := vgrOpened;
    end
  else
    begin
      // entry does not exist, allocate it
      If Size > 0 then
        begin
          EntryAllocate(Identifier,Size,Segment,Entry);
          Variable := Addr(Entry^.Address);
          Result := vgrCreated;
          // size does not change
        end;
    end;
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarGet(const Identifier: String; var Size: TMemSize; out Variable: TPGVVariable): TPGVGetResult;
begin
Result := GlobVarGet(GlobVarTranslateIdentifier(Identifier),Size,Variable);
end;

//------------------------------------------------------------------------------

Function GlobVarRename(OldIdentifier,NewIdentifier: TPGVIdentifier): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If EntryFind(OldIdentifier,Segment,Entry) then
    begin
      // if new identifier is the same as old, do nothing
      If NewIdentifier <> OldIdentifier then
        EntryRename(Entry,NewIdentifier);
      Result := Addr(Entry^.Address);        
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarRename: Unknown variable 0x%.8x.',[OldIdentifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarRename(OldIdentifier: TPGVIdentifier; const NewIdentifier: String): TPGVVariable;
begin
Result := GlobVarRename(OldIdentifier,GlobVarTranslateIdentifier(NewIdentifier));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarRename(const OldIdentifier: String; NewIdentifier: TPGVIdentifier): TPGVVariable;
var
  OldIdentNum:  TPGVIDentifier;
  Segment:      PPGVSegment;
  Entry:        PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  OldIdentNum := GlobVarTranslateIdentifier(OldIdentifier);
  If EntryFind(OldIdentNum,Segment,Entry) then
    begin
      // if new identifier is the same as old, do nothing
      If NewIdentifier <> OldIdentNum then
        EntryRename(Entry,NewIdentifier);
      Result := Addr(Entry^.Address);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarRename: Unknown variable "%s".',[OldIdentifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarRename(const OldIdentifier,NewIdentifier: String): TPGVVariable;
begin
Result := GlobVarRename(OldIdentifier,GlobVarTranslateIdentifier(NewIdentifier));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure GlobVarRename(Variable: TPGVVariable; NewIdentifier: TPGVIdentifier);
var
  Entry:  PPGVSegmentEntry;
begin
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        EntryRename(Entry,NewIdentifier)
      else
        raise EPGVInvalidVariable.Create('GlobVarRename: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarRename: Nil variable reference.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure GlobVarRename(Variable: TPGVVariable; NewIdentifier: String);
begin
GlobVarRename(Variable,GlobVarTranslateIdentifier(NewIdentifier));
end;

//==============================================================================

Function GlobVarIsValid(Variable: TPGVVariable; CheckAddress: Boolean = True): Boolean;
var
  Entry:  PPGVSegmentEntry;
begin
Result := False;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If CheckAddress then
        begin
          If EntryIsValidAddress(Entry) then
            Result := EntryIsValid(Entry);
        end
      else Result := EntryIsValid(Entry);
    finally
      GlobVarUnlock;
    end;
  end;
end;

//------------------------------------------------------------------------------

Function GlobVarIdentifier(Variable: TPGVVariable): TPGVIdentifier;
var
  Entry:  PPGVSegmentEntry;
begin
Result := 0;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        Result := Entry^.Identifier
      else
        raise EPGVInvalidVariable.Create('GlobVarIdentifier: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarIdentifier: Nil variable reference.');
end;

//------------------------------------------------------------------------------

Function GlobVarExists(Identifier: TPGVIdentifier): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
GlobVarLock;
try
  Result := EntryFind(Identifier,Segment,Entry);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarExists(const Identifier: String): Boolean;
begin
Result := GlobVarExists(GlobVarTranslateIdentifier(Identifier));
end;

//------------------------------------------------------------------------------

Function GlobVarSize(Identifier: TPGVIdentifier): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Result := EntrySize(Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarSize: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarSize(const Identifier: String): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    Result := EntrySize(Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarSize: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarSize(Variable: TPGVVariable): TMemSize;
var
  Entry:  PPGVSegmentEntry;
begin
Result := 0;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        Result := EntrySize(Entry)
      else
        raise EPGVInvalidVariable.Create('GlobVarSize: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarSize: Nil variable reference.');
end;

//------------------------------------------------------------------------------

Function GlobVarHeapStored(Identifier: TPGVIdentifier): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Result := (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarHeapStored: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarHeapStored(const Identifier: String): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    Result := (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarHeapStored: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarHeapStored(Variable: TPGVVariable): Boolean;
var
  Entry:  PPGVSegmentEntry;
begin
Result := False;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        Result := (Entry^.Flags and PGV_SEFLAG_SMLSIZE_MASK) = 0
      else
        raise EPGVInvalidVariable.Create('GlobVarHeapStored: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarHeapStored: Nil variable reference.');
end;

//==============================================================================

Function GlobVarGetFlags(Identifier: TPGVIdentifier): TPGVVariableFlags;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := [];
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Result := EntryDecodeFlags(Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarGetFlags: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarGetFlags(const Identifier: String): TPGVVariableFlags;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := [];
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    Result := EntryDecodeFlags(Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarGetFlags: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarGetFlags(Variable: TPGVVariable): TPGVVariableFlags;
var
  Entry:  PPGVSegmentEntry;
begin
Result := [];
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        Result := EntryDecodeFlags(Entry)
      else
        raise EPGVInvalidVariable.Create('GlobVarGetFlags: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarGetFlags: Nil variable reference.');
end;

//------------------------------------------------------------------------------

Function GlobVarSetFlags(Identifier: TPGVIdentifier; Flags: TPGVVariableFlags): TPGVVariableFlags;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := [];
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      Result := EntryDecodeFlags(Entry);
      EntryEncodeFlags(Entry,Flags);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarSetFlags: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarSetFlags(const Identifier: String; Flags: TPGVVariableFlags): TPGVVariableFlags;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := [];
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    begin
      Result := EntryDecodeFlags(Entry);
      EntryEncodeFlags(Entry,Flags);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarSetFlags: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarSetFlags(Variable: TPGVVariable; Flags: TPGVVariableFlags): TPGVVariableFlags;
var
  Entry:  PPGVSegmentEntry;
begin
Result := [];
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        begin
          Result := EntryDecodeFlags(Entry);
          EntryEncodeFlags(Entry,Flags);
        end
      else raise EPGVInvalidVariable.Create('GlobVarSetFlags: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarSetFlags: Nil variable reference.');
end;

//------------------------------------------------------------------------------

Function GlobVarGetFlag(Identifier: TPGVIdentifier; Flag: TPGVVariableFlag): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Result := Flag in EntryDecodeFlags(Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarGetFlag: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarGetFlag(const Identifier: String; Flag: TPGVVariableFlag): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    Result := Flag in EntryDecodeFlags(Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarGetFlag: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarGetFlag(Variable: TPGVVariable; Flag: TPGVVariableFlag): Boolean;
var
  Entry:  PPGVSegmentEntry;
begin
Result := False;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        Result := Flag in EntryDecodeFlags(Entry)
      else
        raise EPGVInvalidVariable.Create('GlobVarGetFlag: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarGetFlag: Nil variable reference.');
end;

//------------------------------------------------------------------------------

Function GlobVarSetFlag(Identifier: TPGVIdentifier; Flag: TPGVVariableFlag; Value: Boolean): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    EntrySetFlag(Entry,Flag,Value)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarSetFlag: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarSetFlag(const Identifier: String; Flag: TPGVVariableFlag; Value: Boolean): Boolean;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := False;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    EntrySetFlag(Entry,Flag,Value)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarSetFlag: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarSetFlag(Variable: TPGVVariable; Flag: TPGVVariableFlag; Value: Boolean): Boolean;
var
  Entry:  PPGVSegmentEntry;
begin
Result := False;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        EntrySetFlag(Entry,Flag,Value)
      else
        raise EPGVInvalidVariable.Create('GlobVarSetFlag: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarGetFlag: Nil variable reference.');
end;

//==============================================================================

Function GlobVarRefCount(Identifier: TPGVIdentifier): UInt32;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    Result := Entry^.RefCount
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarRefCount: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarRefCount(const Identifier: String): UInt32;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    Result := Entry^.RefCount
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarRefCount: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarRefCount(Variable: TPGVVariable): UInt32;
var
  Entry:  PPGVSegmentEntry;
begin
Result := 0;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        Result := Entry^.RefCount
      else
        raise EPGVInvalidVariable.Create('GlobVarRefCount: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarRefCount: Nil variable reference.');
end;

//------------------------------------------------------------------------------

Function GlobVarAcquire(Identifier: TPGVIdentifier): UInt32;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      If Entry^.RefCount < High(UInt32) then
        begin
          Inc(Entry^.RefCount);
          Result := Entry^.RefCount;
        end
      else raise EPGVInvalidState.Create('GlobVarAcquire: Reference count already at its maximum.');
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarAcquire: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarAcquire(const Identifier: String): UInt32;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    begin
      If Entry^.RefCount < High(UInt32) then
        begin
          Inc(Entry^.RefCount);
          Result := Entry^.RefCount;
        end
      else raise EPGVInvalidState.Create('GlobVarAcquire: Reference count already at its maximum.');
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarAcquire: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarAcquire(Variable: TPGVVariable): UInt32;
var
  Entry:  PPGVSegmentEntry;
begin
Result := 0;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        begin
          If Entry^.RefCount < High(UInt32) then
            begin
              Inc(Entry^.RefCount);
              Result := Entry^.RefCount;
            end
          else raise EPGVInvalidState.Create('GlobVarAcquire: Reference count already at its maximum.');
        end
      else raise EPGVInvalidVariable.Create('GlobVarAcquire: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarAcquire: Nil variable reference.');
end;

//------------------------------------------------------------------------------

Function GlobVarRelease(Identifier: TPGVIdentifier; CanFree: Boolean = False): UInt32;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      If Entry^.RefCount > 0 then
        Dec(Entry^.RefCount);
      Result := Entry^.RefCount;
      If (Entry^.RefCount <= 0) and CanFree then
        EntryFree(Segment,Entry);  
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarRelease: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarRelease(const Identifier: String; CanFree: Boolean = False): UInt32;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    begin
      If Entry^.RefCount > 0 then
        Dec(Entry^.RefCount);
      Result := Entry^.RefCount;
      If (Entry^.RefCount <= 0) and CanFree then
        EntryFree(Segment,Entry);  
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarRelease: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//==============================================================================

Function GlobVarAllocate(Identifier: TPGVIdentifier; Size: TMemSize): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If not EntryFind(Identifier,Segment,Entry) then
    begin
      If Size > 0 then
        begin
          EntryAllocate(Identifier,Size,Segment,Entry);
          Result := Addr(Entry^.Address);
        end
      else Result := nil;
    end
  else raise EPGVDuplicateVariable.CreateFmt('GlobVarAllocate: Variable 0x%.8x already exists.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarAllocate(const Identifier: String; Size: TMemSize): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If not EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    begin
      If Size > 0 then
        begin
          EntryAllocate(GlobVarTranslateIdentifier(Identifier),Size,Segment,Entry);
          Result := Addr(Entry^.Address);
        end
      else Result := nil;
    end
  else raise EPGVDuplicateVariable.CreateFmt('GlobVarAllocate: Variable "%s" already exists.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

Function GlobVarAlloc(Identifier: TPGVIdentifier; Size: TMemSize): TPGVVariable;
begin
Result := GlobVarAllocate(Identifier,Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarAlloc(const Identifier: String; Size: TMemSize): TPGVVariable;
begin
Result := GlobVarAllocate(Identifier,Size);
end;

//------------------------------------------------------------------------------

Function GlobVarReallocate(Identifier: TPGVIdentifier; NewSize: TMemSize): TPGVVariable;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := nil;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      // entry already exists (its size cannot be zero)
      If NewSize > 0 then
        begin
          EntryReallocate(Segment,Entry,NewSize);
          Result := Addr(Entry^.Address);
        end
      else EntryFree(Segment,Entry);
    end
  // requested entry does not exist
  else If NewSize > 0 then
    begin
      EntryAllocate(Identifier,NewSize,Segment,Entry);
      Result := Addr(Entry^.Address);
    end;
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarReallocate(const Identifier: String; NewSize: TMemSize): TPGVVariable;
begin
Result := GlobVarReallocate(GlobVarTranslateIdentifier(Identifier),NewSize);
end;

//------------------------------------------------------------------------------

Function GlobVarRealloc(Identifier: TPGVIdentifier; NewSize: TMemSize): TPGVVariable;
begin
Result := GlobVarReallocate(Identifier,NewSize);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarRealloc(const Identifier: String; NewSize: TMemSize): TPGVVariable;
begin
Result := GlobVarReallocate(Identifier,NewSize);
end;

//------------------------------------------------------------------------------

procedure GlobVarFree(Identifier: TPGVIdentifier);
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    EntryFree(Segment,Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarFree: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure GlobVarFree(const Identifier: String);
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    EntryFree(Segment,Entry)
  else
    raise EPGVUnknownVariable.CreateFmt('GlobVarFree: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

//==============================================================================

Function GlobVarStore(Identifier: TPGVIdentifier; const Buffer; Count: TMemSize): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      Result := Min(Count,EntrySize(Entry));
      Move(Buffer,Entry^.Address^,Result);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarStore: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarStore(const Identifier: String; const Buffer; Count: TMemSize): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    begin
      Result := Min(Count,EntrySize(Entry));
      Move(Buffer,Entry^.Address^,Result);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarStore: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarStore(Variable: TPGVVariable; const Buffer; Count: TMemSize): TMemSize;
var
  Entry:  PPGVSegmentEntry;
begin
Result := 0;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        begin
          Result := Min(Count,EntrySize(Entry));
          Move(Buffer,Entry^.Address^,Result);
        end
      else raise EPGVInvalidVariable.Create('GlobVarStore: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarStore: Nil variable reference.');
end;

//------------------------------------------------------------------------------

Function GlobVarLoad(Identifier: TPGVIdentifier; out Buffer; Count: TMemSize): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(Identifier,Segment,Entry) then
    begin
      Result := Min(Count,EntrySize(Entry));
      Move(Entry^.Address^,Addr(Buffer)^,Result);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarLoad: Unknown variable 0x%.8x.',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarLoad(const Identifier: String; out Buffer; Count: TMemSize): TMemSize;
var
  Segment:  PPGVSegment;
  Entry:    PPGVSegmentEntry;
begin
Result := 0;
GlobVarLock;
try
  If EntryFind(GlobVarTranslateIdentifier(Identifier),Segment,Entry) then
    begin
      Result := Min(Count,EntrySize(Entry));
      Move(Entry^.Address^,Addr(Buffer)^,Result);
    end
  else raise EPGVUnknownVariable.CreateFmt('GlobVarLoad: Unknown variable "%s".',[Identifier]);
finally
  GlobVarUnlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GlobVarLoad(Variable: TPGVVariable; out Buffer; Count: TMemSize): TMemSize;
var
  Entry:  PPGVSegmentEntry;
begin
Result := 0;
If Assigned(Variable) then
  begin
    GlobVarLock;
    try
      Entry := EntryFromVar(Variable);
      If EntryIsValid(Entry) then
        begin
          Result := Min(Count,EntrySize(Entry));
          Move(Entry^.Address^,Addr(Buffer)^,Result);
        end
      else raise EPGVInvalidVariable.Create('GlobVarLoad: Invalid variable entry.');
    finally
      GlobVarUnlock;
    end;
  end
else raise EPGVInvalidValue.Create('GlobVarStore: Nil variable reference.');
end;


{===============================================================================
--------------------------------------------------------------------------------
                      Unit initialization and finalization
--------------------------------------------------------------------------------
===============================================================================}

initialization
  ModuleInitialization;

finalization
  ModuleFinalization;

end.

