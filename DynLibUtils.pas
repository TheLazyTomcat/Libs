{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Dynamic Library Utilities

    Main aim of this small library is to encapsulate dynamic library loading
    and symbol resolving (ie. obtaining addresses of functions and variables)
    on different systems.

    There are also contexts which offer more advanced options, but they are
    currently untested - use them with caution.

  Version 1.4 (2024-10-14)

  Last change 2024-10-14

  ©2020-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.DynLibUtils

  Dependencies:
  * AuxExceptions  - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps
  * SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
  * WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol DynLibUtils_UseAuxExceptions for details).

  Library SimpleCPUID is required only when compiling for x86(-64) CPU.

  Library WindowsVersion is required only when compiling for Windows OS.

  Libraries AuxExceptions and SimpleCPUID might also be required as an indirect
  dependencies.

  Indirect dependencies:
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit DynLibUtils;
{
  DynLibUtils_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.

  Note that, in fact, this unit cannot be compiled without asm when compiling
  for x86(-64) processor.
}
{$IFDEF DynLibUtils_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{
  DynLibUtils_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  DynLibUtils_UseAuxExceptions to achieve this.
}
{$IF Defined(DynLibUtils_UseAuxExceptions)}
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

{$IF Defined(CPU386) or Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE CPU_x86x}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH CLASSICPROCVARS+}
  {$IFDEF CPU_x86x}
    {$ASMMODE Intel}
  {$ENDIF}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17} // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

{$IF Defined(CPU_x86x) and Defined(PurePascal) and not Defined(CompTest)}
  {$MESSAGE WARN 'This unit cannot be compiled without ASM.'}
{$IFEND}

interface

uses
  {$IFDEF Windows}Windows,{$ENDIF} SysUtils, Classes,
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EDLUException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EDLULibraryOpenError  = class(EDLUException);
  EDLULibraryCloseError = class(EDLUException);
  EDLUInvalidParameter  = class(EDLUException);
  EDLUSymbolError       = class(EDLUException);

{$IFDEF Windows}
{===============================================================================
    Process error mode management - declaration
===============================================================================}

Function GetThreadErrorMode: DWORD;
Function SetThreadErrorMode(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL;

{$ENDIF}

{===============================================================================
    Symbol resolving auxiliary - declaration
===============================================================================}
type
  // TDLUSymbol is used in macro functions for symbol resolving
  TDLUSymbol = record
    Name:       String;     // name of the symbol
    AddressVar: PPointer;   // pointer to a variable to which the address shall be stored
  end;
  PDLUSymbol = ^TDLUSymbol;

// inline contructors for TDLUSymbol record
Function Symbol(const Name: String; AddressVar: PPointer): TDLUSymbol; overload; {$IFDEF CanInline}inline;{$ENDIF}
Function Symbol(AddressVar: PPointer; const Name: String): TDLUSymbol; overload; {$IFDEF CanInline}inline;{$ENDIF}

{===============================================================================
    Utility functions - declaration
===============================================================================}
{
  LibraryIsPresent

  Tries to load the requested library. If it succeeds, it returns true,
  otherwise it will return false.

  Critical error dialog is suppressed.
}
Function LibraryIsPresent(const LibFileName: String): Boolean;

{
  SymbolIsPresent

  Returns true when requested symbol can be obtained from given library, false
  otherwise.

  If will first load the library, then it will try to normally resolve the
  symbol and in the end will unload the library.

  If the library cannot be loaded, it will raise an EDLULibraryOpenError
  exception.

  Critical error dialog is suppressed.
}
Function SymbolIsPresent(const LibFileName, SymbolName: String): Boolean;

{
  LibrarySymbolIsPresent

  Returns true when the requested library can be loaded and requested symbol
  can be obtained from it, false otherwise. No exceptions are raised.

  Critical error dialog is suppressed.
}
Function LibrarySymbolIsPresent(const LibFileName, SymbolName: String): Boolean;


{===============================================================================
--------------------------------------------------------------------------------
                                Handle functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Functions operating on library handles are supposed to be used only locally
  (no global variables and such) or when the handles are strictly managed.
  If you want to operate on libraries globally (ie. using global variables)
  or from multiple threads, use contexts.

  Given the implementation, a variable of type TDLULibraryHandle must be
  assigned only once, because each call to CloseLibrary on it will invalidate
  that variable.
}
{===============================================================================
    Handle functions - declaration
===============================================================================}
type
  TDLULibraryHandle = {$IFDEF Windows}THandle{$ELSE}Pointer{$ENDIF};
  PDLULibraryHandle = ^TDLULibraryHandle;

const
  DefaultLibraryHandle = TDLULibraryHandle({$IFDEF Windows}0{$ELSE}nil{$ENDIF});

type
{
  TDLUOption

  Individual values of this enumeration are used to activate or deactivate
  specific options available in calls provided by this library.

  Most of implemented functions support only a subset of all options - refer
  to description of each function to see what options are observed.

  optDeepCheck

    Normally, when a library handle is checked for validity, it is only probed
    whether is is assigned - ie. is not null or nil.

    When this option is added, the checking continues by trying to obtain some
    (inplementation-specific) information about the library using the handle.
    If this succeeds, then the handle is assumed to be valid. In case of
    failure it is assumed to be invalid.

  optNoCriticalError

    In Windows OS, when a library cannot be loaded for whatever reason, the
    called WinAPI function will initiate critical system error, which will
    display an error dialog (a window).
    This might be very obtrusive (eg. when probing DLL existence) - so if this
    behavior is undesirable, include this option and this error dialog will be
    suppressed.

    This option has effect only in Windows OS, it is ignored elsewhere.

      WARNING - suppressing error dialog is inherently thread unsafe in systems
                older than Windows 7.

  optExceptionOnFailure

    This option is somewhat universal, because it can be observed by several
    groups of functions.

      Functions opening/loading libraries (eg. OpenLibrary)

        These usually indicate success or failure using boolean result. When
        this option is activated, then an EDLULibraryOpenError exception is
        raised when the loading fails for any reason (success is still
        indicated by returning True).

      Functions closing/unloading libraries (eg. CloseLibrary)

        These functions normally do not check for errors when calling the
        system functions doing the cleanup, but when this option is activated,
        the check is performed and an EDLULibraryCloseError exception is raised
        on failure.

      Functions resolving symbols (eg. GetSymbolAddr or ResolveSymbol)

        When resolving symbols, the failure is normally indicated by returning
        a nil pointer. With this option active, the EDLUSymbolError exception
        is raised whenever the symbol cannot be resolved.

          NOTE - there are situations where nil pointer is considered to be a
                 valid return, so some functions might return nil and not raise
                 an exception even with this option.

  optResolveNow

    When activated, loading of a library also resolves all undefined symbol
    before returning (RTLD_NOW is put into flags), as apposed to lazy binding
    when this option is not active (flags contain RTLD_LAZY).

    This has effect only in Linux OS, it is ignored in Windows. For more
    information please refer to Linux manual (dlopen(3)).

  optSafeLoad

    Activate this option if you want to preserve some selected system settings
    across the call when loading or unloading a library.

    In current implementation, it preserves process/thread error mode on
    Windows OS and X87 control word (FPU settings) and MXCSR register (SSE and
    AVX settings) on x86(-64) processors.

    It is here for situations where the loaded library is changing those
    settings but this behavior is undesirable.

  optBreakOnUnresolved

    This option is observed only by functions resolving multiple symbols. When
    active, it forces interruption of array/list traversal when currently
    processed symbol cannot be resolved.

      NOTE - option optExceptionOnFailure takes precedence over this one. If
             both are active and symbol that cannot be resolved is encountered,
             then an exception is raised as prescribed by optExceptionOnFailure.

  optNoSerialize

    This option affects only functions operating on contexts - when active,
    the call does not lock the context it is operating on (is not serialized).

    It can be used to improve performance in cases where serialization of
    every single call is not necessary.

      WARNING - this option is observed only when added to call parameter
                Options, its presence in context-stored options is ignored.
                This is due to a fact that context needs to be locked before
                the stored options can be accessed and probed for this option,
                and that locking directly contradicts this option being used.

      WARNING - take great care where and when you use this option, as using
                it in an inappropriate situation will inadvertently lead to
                corruption of the affected context.
                Generally, you should never use this option on sole calls,
                use it only if the context is currently thread locked, as
                the locking and unlocking calls do way more than just that.

  optAlwaysLoad

    Under normal circumstances, context function OpenLibrary loads the library
    (that is, calls system function to do so) only when first called on
    particular context variable - that variable is initialized in this call
    and the library is loaded. On subsequent calls it only increments internal
    reference count.
    But when this option is active, then this function loads the library even
    if the context is already intialized and library was already loaded.

    This option works similarly for CloseLibrary - this function decrements
    the internal reference count and only unloads the library if the count
    reaches zero. With this option active it always cloases the library
    (whether it is unloaded is decided by the OS).

    Each opening call with this option active must be paired by a closing
    class with it.

    Note that operating system keeps its own reference count for each loaded
    module/object, so this option might seem to be somewhat superfluous. But
    it is intended for situation where repeated loading or closing of one
    library pose some problem, whatever it migh be. 

  optSymbolListResolve

    If this option is active during symbol resolving using context, then the
    symbol is first searched for in context's list of resolved symbols.
    If found there, then address stored in this list is returned. When not
    found, then the symbol is resolved as usual (using system call and stored
    library handle).

  optSymbolListStore

    When resolving symbols using contexts with this option enabled, every
    successfully resolved symbol is stored along with its address in the list
    of resolved symbols within the context - as long as it was not resolved
    using the list (see optSymbolListResolve). If this option is not active,
    then nothing is written to this list.

    If the resolved symbol already is in the list, its address is updated
    on every new successful resolving.

  optContextOptions

    Affects only functions working with contexts.

    Each context stores its own options set, but under normal circumstances all
    functions working with contexts will use options given in the call argument
    Options, even if nothing is explicitly assigned there (a default empty set
    is used).

    When this option is included in Options parameter, then the called function
    will use context-stored options to alter its behaviour and will ignore
    other options given in the parameter. If this is active in contex options,
    then all calls working on that context will use contex options, irrespective
    of whether optContextOptions is given in Options argument or not.
}
  TDLUOption = (optDeepCheck,optNoCriticalError,optExceptionOnFailure,
                optResolveNow,optSafeLoad,optBreakOnUnresolved,optNoSerialize,
                optAlwaysLoad,optSymbolListResolve,optSymbolListStore,
                optContextOptions);

  TDLUOptions = set of TDLUOption;

{
  TDLUSymReslResult
  TDLUSymReslResults

  These types are used when resolving multiple symbols to indicate which
  symbols were passed to resolving (field Processed) and which were succesfully
  resolved (field Resolved).

  It can be used eg. in situation where option optBreakOnUnresolved is not
  activated and some symbols could not be resolved - to find which ones.
}
  TDLUSymReslResult = record
    Processed:  Boolean;
    Resolved:   Boolean;
  end;

  TDLUSymReslResults = array of TDLUSymReslResult;

{-------------------------------------------------------------------------------
    Handle functions - library functions
-------------------------------------------------------------------------------}
{
  CheckLibrary

  Returns true when passed library handle is valid, false otherwise.

  Observed options:

    optDeepCheck
}
Function CheckLibrary(LibraryHandle: TDLULibraryHandle; Options: TDLUOptions = []): Boolean; overload;

{
  OpenLibrary

  Loads the requested library and returns a handle to it. It will return null
  or nil (depending on OS) if it cannot be loaded.

  This handle must be closed using function CloseLibrary when you are done
  with it.

  Observed options:

    optDeepCheck, optNoCriticalError, optExceptionOnFailure, optResolveNow,
    optSafeLoad
}
Function OpenLibrary(const LibFileName: String; Options: TDLUOptions = []): TDLULibraryHandle; overload;

{
  OpenLibrary

  Loads the requested library and returns handle to it in output parameter
  LibraryHandle. This parameter is set to null or nil (depending on OS) if
  the library cannot be loaded. Returns true when the loading succeeds, false
  otherwise.

    NOTE - CheckLibrary is called internally to check whether the library was
           loaded successfully or not.

  The returned library handle must be closed using function CloseLibrary when
  you are done using it.

  If an exception is raised, then value of LibraryHandle is undefined.

  Observed options:

    optDeepCheck, optNoCriticalError, optExceptionOnFailure, optResolveNow,
    optSafeLoad
}
Function OpenLibrary(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; Options: TDLUOptions = []): Boolean; overload;

{
  CloseLibrary

  Closes and potentially unloads the library (unloading is managed by the OS).

  It checks the handle (calls CheckLibrary) before processing. If it is not
  deemed to be valid, it will exit without doing anything (no exception).

  Note that it will always invalide the library handle, irrespective of whether
  the OS unloads the library or not.

  Observed options:

    optDeepCheck, optSafeLoad, optExceptionOnFailure
}
procedure CloseLibrary(var LibraryHandle: TDLULibraryHandle; Options: TDLUOptions = []); overload;

{-------------------------------------------------------------------------------
    Handle functions - symbols addresses
-------------------------------------------------------------------------------}
{
  GetSymbolAddr

  Returns pointer to requested symbol. If it cannot be resolved, it returns nil.

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception (irrespective of whether optExceptionOnFailure is included in
  options or not).

  Observed options:

    optDeepCheck, optExceptionOnFailure
}
Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String; Options: TDLUOptions = []): Pointer; overload;

{
  GetSymbolAddr

  Stores address of requested symbol to Address output parameter.

  Returns true when the symbol was properly resolved, false otherwise (in which
  case the value of Address is undefined).

  Always raises an EDLUInvalidParameter exception if the library handle is not
  valid.

  This function is intended for situations where one wants to check validity
  of returned address even if it can be nil, without raising an exception.

  Observed options:

    optDeepCheck, optExceptionOnFailure
}
Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String; out Address: Pointer; Options: TDLUOptions = []): Boolean; overload;

{-------------------------------------------------------------------------------
    Handle functions - symbols resolving
-------------------------------------------------------------------------------}
{
  ResolveSymbol

  Resolves symbol whose name is given in Name field of parameter Symbol and
  stores its address to a variable pointed to by the field AddressVar in the
  same parameter. When it succeeds then True is returned, on failure False is
  returned.

  Note that variable for symbol address might be assigned even on failure,
  in which case its content is undefined.

  Always raises an EDLUInvalidParameter exception if the library handle is not
  valid.

  Observed options:

    optDeepCheck, optExceptionOnFailure
}
Function ResolveSymbol(LibraryHandle: TDLULibraryHandle; Symbol: TDLUSymbol; Options: TDLUOptions = []): Boolean; overload;

{
  ResolveSymbols

  Traverses given array of TDLUSymbol records (parameter Symbols) and tries to
  resolve all symbols within (see description of function ResolveSymbol for
  details as this function is internally called). Note that if one symbol is
  included multiple-times, it is resolved separately for each iteration (there
  is no duplicity protection).

  Result is set to number of successfully resolved symbols. If this number is
  equal to length of given array, then all symbols have been resolved without
  errors.

  Output parameter Results (in overload accepting this parameter) can be probed
  to see which symbols were successfully resolved and which not in case the
  returned count indicates not all were.

  Always raises an EDLUInvalidParameter exception if the library handle is not
  valid.

    NOTE - validity of provided library handle is checked even if no symbols
           (an empty array) is given.

  Observed options:

    optDeepCheck, optExceptionOnFailure, optBreakOnUnresolved
}
Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; const Symbols: array of TDLUSymbol; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; const Symbols: array of TDLUSymbol; Options: TDLUOptions = []): Integer; overload;

{
  ResolveSymbolList

  This function works the same as ResolveSymbols (see there for more info),
  but names of individual symbols are taken from items of passed string list
  and resulting addresses are stored at respective places in the list's Objects
  property (the pointers are casted to TObject).

  Parameter SymbolList is not declared as more general TStrings because that
  class does not fully implement Objects property, which is required here.

  Always raises an EDLUInvalidParameter exception if the library handle is not
  valid.

    NOTE - validity of provided library handle is checked even if no symbols
           (an empty list) is given.

  Observed options:

    optDeepCheck, optExceptionOnFailure, optBreakOnUnresolved
}
Function ResolveSymbolList(LibraryHandle: TDLULibraryHandle; SymbolList: TStringList; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function ResolveSymbolList(LibraryHandle: TDLULibraryHandle; SymbolList: TStringList; Options: TDLUOptions = []): Integer; overload;

{
  ResolveSymbolNames

  Works the same as ResolveSymbols (see there for more info), but names of
  individual symbols are taken from items in Names array and the resulting
  addresses are stored to variables pointed to by respective items in
  AddressPtrs array (eg. address for name in Names[3] is stored to variable
  pointed to by AddressVars[3]).

  Length of both Names and AddressVars arrays must be the same, otherwise an
  EDLUInvalidParameter exception is raised (irrespective of whether option
  optExceptionOnFailure is active or not).

  Always raises an EDLUInvalidParameter exception if the library handle is not
  valid.

    NOTE - validity of provided library handle is checked even if no symbols
           (an empty arrays) is given.

  Observed options:

    optDeepCheck, optExceptionOnFailure, optBreakOnUnresolved
}
Function ResolveSymbolNames(LibraryHandle: TDLULibraryHandle; const Names: array of String;
  const AddressVars: array of PPointer; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function ResolveSymbolNames(LibraryHandle: TDLULibraryHandle; const Names: array of String;
  const AddressVars: array of PPointer; Options: TDLUOptions = []): Integer; overload;

{-------------------------------------------------------------------------------
    Handle functions - macro functions
-------------------------------------------------------------------------------}
{
  OpenLibraryAndResolveSymbol(s/List/Names)

  All these functions are just simple macros calling OpenLibrary (overload
  directly returning handle) and then appropriate ResolveSymbol(s/List/Names).
  Refer to description of those called functions for their behavior (observed
  options, raised exceptions, meaning of parameters, return values, ...).

  Returns a return value of particular ResolveSymbol* used in the implementation
  of given function.

  If an exception is raised, then content of the output parameter LibraryHandle
  is undefined (if the exception is raised after the library was successfully
  opened, it is automatically closed).

    NOTE - given options are in effect for both OpenLibrary and ResolveSymbol*.
           This must be considered especially for option optExceptionOnFailure.

  Observed options:

    optDeepCheck, optNoCriticalError, optExceptionOnFailure, optResolveNow,
    optSafeLoad, optBreakOnUnresolved
}
Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: array of TDLUSymbol; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: array of TDLUSymbol; Options: TDLUOptions = []): Integer; overload;

Function OpenLibraryAndResolveSymbolList(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  SymbolList: TStringList; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function OpenLibraryAndResolveSymbolList(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  SymbolList: TStringList; Options: TDLUOptions = []): Integer; overload;

Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  const Names: array of String; const AddressVars: array of PPointer; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  const Names: array of String; const AddressVars: array of PPointer; Options: TDLUOptions = []): Integer; overload;


{===============================================================================
--------------------------------------------------------------------------------
                                Context functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Contexts provide more robust way of loading libraries and storing their
  handles for symbol resolving and unloading, and are a preffered way if you
  want to operate on libraries using global variables or in a multi-thread
  environment, as access to each context is serialized. But note that for
  simple cases it is still better to use library handles as they have much
  lower overhead.

  Each context stores and provides not only the handle obtained when loading
  a library, but also more information about that library along with some
  statistics, though this is currently implemented only in a limited form.

  The most important feature of contexts is, that the library to which a
  context is bound is loaded (ie. system function loading it is called) only
  once, in the first loading call. Also, the system freeing is done only once.
  This is achieved by maintaining an internal reference count. But note that
  this all holds true only per context, not globally (the same library can be
  loaded using a different context), and also can be disabled using options.

  Because the loading indicates whether the library was already loaded or not,
  this can be also used for optimizations in symbol resolving (ie. resolving is
  performed only on the first load and is omitted on subsequent load calls).

  For more information, refer to description of types and functions operating
  on contexts.
}
{===============================================================================
    Context functions - declaration
===============================================================================}
type
{
  TDLULibraryContextData

  This structure is used to store and return information and statistics about
  specific context.

  Some fields might need clarification, namely:

    OpenCount

      Stores how many times was this context used to open its library. It gets
      decremented on each closing.

    OpenFileName

      Stores exact string that was used to load the library - ie. the string
      used in a call that initialized this context.

    FullFileName

      This string is obtained from OS using the opened library handle. It is
      here to provide full library path when only file name or relative path
      was used to load the library.

    ResolvedSymbols

      This stores a list of already resolved symbols with their addresses.
}
  TDLULibraryContextData = record
    Handle:           TDLULibraryHandle;
    OpenCount:        Integer;
    OpenFileName:     String;
    FullFileName:     String;
    Options:          TDLUOptions;
    ResolvedSymbols:  array of record
                        Name:     String;
                        Address:  Pointer;
                      end;
  end;

const
{
  DLUContextSize
  DLUContextWords

  These two constants are intended only for internal use, please ignore them
  as they might be changed or even removed in the future.
}
  DLUContextSize = (3 * SizeOf(Integer)) + SizeOf(Boolean) + SizeOf(TObject) + SizeOf(TDLULibraryContextData);
  DLUContextWords = (DLUContextSize + {$IFDEF CPU64bit}7) shr 3{$ELSE}3) shr 2{$ENDIF};

type
{
  TDLULibraryContext

  Semi-opaque public type used to store the actual internal context.

  Always make sure to initialize the context variable, either by assigning
  DefaultLibraryContext constant to it or by zeroing the memory. But note that
  in (object) pascal, global and thread variables are automatically zeroed by
  the compiller, so this applies mainly to local or reused variables.
}
  TDLULibraryContext = array[0..Pred(DLUContextWords)] of NativeInt;
  PDLULibraryContext = ^TDLULibraryContext;

const
  DefaultLibraryContext: TDLULibraryContext =
    (0,0,0,0,0,0,0,0,0{$IFNDEF CPU64bit},0,0{$ENDIF});

{-------------------------------------------------------------------------------
    Context functions - utility functions
-------------------------------------------------------------------------------}
{
  ContextLock

  Locks the passed context so that only the current thread can access it.

  If this function is called while the lock is held by other thread, the call
  will block until the holder realeases its lock.

  Nested locks are allowed, but each lock operation must be paired with an
  unlock operation (a call to ContextUnlock).

  This direct access to internal lock is provided to allow for more complex
  operations on the context to be performed atomically - everything between
  ContextLock-ContextUnlock calls will be executed in a thread-safe manner.
}
procedure ContextLock(var Context: TDLULibraryContext); overload;

{
  ContextUnlock

  Unlocks the passed context.
}
procedure ContextUnlock(var Context: TDLULibraryContext); overload;

//------------------------------------------------------------------------------
{
  ContextGetData

  Returns information and statistics of the passed context.

  Raises an EDLUInvalidParameter exception if the context is not valid.

    WARNING - if the context is accessed from multiple threads, then returned
              data might not be accurate by the time the function returns,
              unless it is called inside a locked block.

  Observed options:

    optDeepCheck, optNoSerialize, optContextOptions
}
Function ContextGetData(var Context: TDLULibraryContext; Options: TDLUOptions = []): TDLULibraryContextData;

{
  ContextGetOptions

  Returns context-stored options.

    Each context stores its own set of options that can be used to alter
    behaviour of calls performed on that context - see description of type
    TDLUOption, enum value optContextOptions.

  Raises an EDLUInvalidParameter exception if the context is not valid.

    WARNING - if the context is accessed from multiple threads, then returned
              value might not be accurate by the time the function returns,
              unless it is called inside a locked block.

  Observed options:

    optDeepCheck, optNoSerialize, optContextOptions
}
Function ContextGetOptions(var Context: TDLULibraryContext; Options: TDLUOptions = []): TDLUOptions;

{
  ContextSetOptions

  Sets context-stored options to NewOptions and returns their previous value.

  Raises an EDLUInvalidParameter exception if the context is not valid.

    WARNING - if the context is accessed from multiple threads, then returned
              value might not be accurate by the time the function returns,
              unless it is called inside a locked block.

  Observed options:

    optDeepCheck, optNoSerialize, optContextOptions
}
Function ContextSetOptions(var Context: TDLULibraryContext; NewOptions: TDLUOptions; Options: TDLUOptions = []): TDLUOptions;

{
  ContextSetOptions

  Returns state of selected Option within context-stored options. If the option
  is active, then True is returned, false otherwise.

  Raises an EDLUInvalidParameter exception if the context is not valid.

    WARNING - if the context is accessed from multiple threads, then returned
              value might not be accurate by the time the function returns,
              unless it is called inside a locked block.

  Observed options:

    optDeepCheck, optNoSerialize, optContextOptions
}
Function ContextGetOption(var Context: TDLULibraryContext; Option: TDLUOption; Options: TDLUOptions = []): Boolean;

{
  ContextSetOptions

  Sets selected Option in context-stored options of given context to a value
  selected in NewState. When NewState is set to true, then the selected option
  is enabled, when set to false then it is disabled. Returns previous state of
  the selected option.

  Raises an EDLUInvalidParameter exception if the context is not valid.

    WARNING - if the context is accessed from multiple threads, then returned
              value might not be accurate by the time the function returns,
              unless it is called inside a locked block.

  Observed options:

    optDeepCheck, optNoSerialize, optContextOptions
}
Function ContextSetOption(var Context: TDLULibraryContext; Option: TDLUOption; NewState: Boolean; Options: TDLUOptions = []): Boolean;

{
  ContextSetOptions

  Enables selected Option in context-stored options of given context, returning
  previous value of those options.

  Raises an EDLUInvalidParameter exception if the context is not valid.

    WARNING - if the context is accessed from multiple threads, then returned
              value might not be accurate by the time the function returns,
              unless it is called inside a locked block.

  Observed options:

    optDeepCheck, optNoSerialize, optContextOptions
}
Function ContextIncludeOption(var Context: TDLULibraryContext; Option: TDLUOption; Options: TDLUOptions = []): TDLUOptions;

{
  ContextSetOptions

  Disables selected Option in context-stored options of given context, returning
  previous value of those options.

  Raises an EDLUInvalidParameter exception if the context is not valid.

    WARNING - if the context is accessed from multiple threads, then returned
              value might not be accurate by the time the function returns,
              unless it is called inside a locked block.

  Observed options:

    optDeepCheck, optNoSerialize, optContextOptions
}
Function ContextExcludeOption(var Context: TDLULibraryContext; Option: TDLUOption; Options: TDLUOptions = []): TDLUOptions;

{-------------------------------------------------------------------------------
    Context functions - library functions
-------------------------------------------------------------------------------}
{
  CheckLibrary

  Returns true when the passed context is valid, false otherwise.

  Context is considered valid when a library was previously successfully opened
  using it, ie. it contains a valid handle to a loaded library.

  Observed options:

    optDeepCheck, optNoSerialize, optContextOptions
}
Function CheckLibrary(var Context: TDLULibraryContext; Options: TDLUOptions = []): Boolean; overload;

{
  OpenLibrary

  If the context is not initialized, then it initializes the provided context
  variable (context-stored options are initialized to an empty set), loads
  the requested library and returns true.

  If the context is already initialized (that is, it was previously used to
  successfully load a library), then it will return false and further behavior
  of this functions depends on whether option optAlwaysLoad is active or not...

      optAlwaysLoad is active

        The function loads the library (calls system function) again and also
        increments internal reference count. Stored handle is not updated as
        the system function should return the same handle for the same opened
        object (both in Windows and Linux) - this is checked and if the new
        handle do not match the stored one, then an EDLULibraryOpenError
        exception s raised.
        The string given in LibFileName is ignored, instead a string stored in
        the context's field FullFileName is used.

      optAlwaysLoad is not active

        Function does NOT load the library again, only increments context's
        internal reference count (field OpenCount). LibFileName is ignored.

    WARNING - in both cases, no check is performed whether the currently
              requested file is the same as was opened in the original loading
              call, the given LibFileName string is completely ignored!

  If the library is loaded (first call), the returned handle is always checked
  for validity (whether with deep check depends on options). If it is not valid,
  then an EDLULibraryOpenError is raised, irrespective of whether the option
  optExceptionOnFailure is active or not.

    NOTE - if the context is accessed by multiple threads, then the function
           can return false and not actually load the library even if the
           context was uninitialized prior the call to this function. This is
           because other thread can initialize it before this call is able to
           do its own initialization.

  Observed options:

    optDeepCheck, optNoCriticalError, optExceptionOnFailure, optResolveNow,
    optSafeLoad, optNoSerialize, optAlwaysLoad, optContextOptions
}
Function OpenLibrary(const LibFileName: String; var Context: TDLULibraryContext; Options: TDLUOptions = []): Boolean; overload;

{
  CloseLibrary

  Decrements reference count of the context, when it reaches zero it then
  unloads the library and invalidates the context.

  Result indicates whether the context was invalidated (true) or not (false).

  If the provided context is not valid, then it raises an EDLUInvalidParameter
  exception, irrespective of whether optExceptionOnFailure option is active
  or not.

  Observed options:

    optDeepCheck, optExceptionOnFailure, optSafeLoad, optNoSerialize,
    optAlwaysLoad, optContextOptions
}
Function CloseLibrary(var Context: TDLULibraryContext; Options: TDLUOptions = []): Boolean; overload;

{-------------------------------------------------------------------------------
    Context functions - symbols addresses
-------------------------------------------------------------------------------}
{
  GetSymbolAddr

  Returns pointer to requested symbol. If it cannot be resolved, it returns nil.

  If the context is not valid, it will raise an EDLUInvalidParameter exception
  (irrespective of whether optExceptionOnFailure is included in options or not).

  Observed options:

    optDeepCheck, optExceptionOnFailure, optNoSerialize, optSymbolListStore,
    optSymbolListResolve, optContextOptions
}
Function GetSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String; Options: TDLUOptions = []): Pointer; overload;

{
  GetSymbolAddr

  Stores address of requested symbol to Address output parameter. Returns true
  when the symbol was properly resolved, false otherwise (in which case the
  value of Address is undefined).

  Always raises an EDLUInvalidParameter exception if the contex is not valid.

  Observed options:

    optDeepCheck, optExceptionOnFailure, optNoSerialize, optSymbolListStore,
    optSymbolListResolve, optContextOptions
}
Function GetSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String; out Address: Pointer; Options: TDLUOptions = []): Boolean; overload;

{-------------------------------------------------------------------------------
    Context functions - symbols resolving
-------------------------------------------------------------------------------}
{
  ResolveSymbol

  Resolves symbol whose name is given in Name field of parameter Symbol and
  stores its address to a variable pointed to by the field AddressVar in the
  same parameter. When it succeeds then True is returned, on failure False is
  returned.

  Note that variable for symbol address might be assigned even on failure,
  in which case its content is undefined.

  Always raises an EDLUInvalidParameter exception if the contex is not valid.

  Observed options:

    optDeepCheck, optExceptionOnFailure, optNoSerialize, optSymbolListStore,
    optSymbolListResolve, optContextOptions
}
Function ResolveSymbol(var Context: TDLULibraryContext; Symbol: TDLUSymbol; Options: TDLUOptions = []): Boolean; overload;

{
  ResolveSymbols

  Traverses given array of TDLUSymbol records (parameter Symbols) and tries to
  resolve all symbols within (see description of function ResolveSymbol for
  details as this function is internally called). Note that if one symbol is
  included multiple-times, it is resolved separately for each iteration (there
  is no duplicity protection).

  Result is set to number of successfully resolved symbols. If this number is
  equal to length of given array, then all symbols have been resolved without
  errors.

  Output parameter Results (in overload accepting this parameter) can be probed
  to see which symbols were successfully resolved and which not in case the
  returned count indicates not all were.

  Always raises an EDLUInvalidParameter exception if the context is not valid.

    NOTE - validity of provided context is checked even if no symbols (an empty
           array) is given.

  Observed options:

    optDeepCheck, optExceptionOnFailure, optBreakOnUnresolved, optNoSerialize,
    optSymbolListStore, optSymbolListResolve, optContextOptions
}
Function ResolveSymbols(var Context: TDLULibraryContext; const Symbols: array of TDLUSymbol; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function ResolveSymbols(var Context: TDLULibraryContext; const Symbols: array of TDLUSymbol; Options: TDLUOptions = []): Integer; overload;

{
  ResolveSymbolList

  This function works the same as ResolveSymbols (see there for more info),
  but names of individual symbols are taken from items of passed string list
  and resulting addresses are stored at respective places in the list's Objects
  property (the pointers are casted to TObject).

  Parameter SymbolList is not declared as more general TStrings because that
  class does not fully implement Objects property, which is required here.

  Always raises an EDLUInvalidParameter exception if the context is not valid.

    NOTE - validity of provided context is checked even if no symbols (an empty
           list) is given.

  Observed options:

    optDeepCheck, optExceptionOnFailure, optBreakOnUnresolved, optNoSerialize,
    optSymbolListStore, optSymbolListResolve, optContextOptions
}
Function ResolveSymbolList(var Context: TDLULibraryContext; SymbolList: TStringList; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function ResolveSymbolList(var Context: TDLULibraryContext; SymbolList: TStringList; Options: TDLUOptions = []): Integer; overload;

{
  ResolveSymbolNames

  Works the same as ResolveSymbols (see there for more info), but names of
  individual symbols are taken from items in Names array and the resulting
  addresses are stored to variables pointed to by respective items in
  AddressPtrs array (eg. address for name in Names[3] is stored to variable
  pointed to by AddressVars[3]).

  Length of both Names and AddressVars arrays must be the same, otherwise an
  EDLUInvalidParameter exception is raised (irrespective of whether option
  optExceptionOnFailure is active or not).

  Always raises an EDLUInvalidParameter exception if the context is not valid.

    NOTE - validity of provided context is checked even if no symbols (an empty
           arrays) is given.

  Observed options:

    optDeepCheck, optExceptionOnFailure, optBreakOnUnresolved, optNoSerialize,
    optSymbolListStore, optSymbolListResolve, optContextOptions
}
Function ResolveSymbolNames(var Context: TDLULibraryContext; const Names: array of String;
  const AddressVars: array of PPointer; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function ResolveSymbolNames(var Context: TDLULibraryContext; const Names: array of String;
  const AddressVars: array of PPointer; Options: TDLUOptions = []): Integer; overload;

{-------------------------------------------------------------------------------
    Context functions - macro functions
-------------------------------------------------------------------------------}
{
  OpenLibraryAndResolveSymbol(s/List/Names)

  All these functions are just simple macros calling OpenLibrary (overload
  directly returning handle) and then appropriate ResolveSymbol(s/List/Names).
  Refer to description of those called functions for their behavior (observed
  options, raised exceptions, meaning of parameters, return values, ...).

  Returns a return value of particular ResolveSymbol* used in the implementation
  of given function.

  If an exception is raised, then content of the output parameter Context is
  undefined.

    NOTE - given options are in effect for both OpenLibrary and ResolveSymbol*,
           be aware of that!

  Observed options:

    optDeepCheck, optNoCriticalError, optExceptionOnFailure, optResolveNow,
    optSafeLoad, optBreakOnUnresolved, optNoSerialize, optAlwaysLoad,
    optSymbolListStore, optSymbolListResolve, optContextOptions
}
Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: array of TDLUSymbol; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: array of TDLUSymbol; Options: TDLUOptions = []): Integer; overload;

Function OpenLibraryAndResolveSymbolList(const LibFileName: String; var Context: TDLULibraryContext;
  SymbolList: TStringList; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function OpenLibraryAndResolveSymbolList(const LibFileName: String; var Context: TDLULibraryContext;
  SymbolList: TStringList; Options: TDLUOptions = []): Integer; overload;

Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; var Context: TDLULibraryContext;
  const Names: array of String; const AddressVars: array of PPointer; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer; overload;
Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; var Context: TDLULibraryContext;
  const Names: array of String; const AddressVars: array of PPointer; Options: TDLUOptions = []): Integer; overload;

implementation

uses
  {$IFNDEF Windows}baseunix, dl,{$ENDIF} SyncObjs,
  StrRect, InterlockedOps{$IFDEF CPU_x86x}, SimpleCPUID{$ENDIF}
  {$IFDEF Windows}, WindowsVersion{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

{===============================================================================
    Safe loading - implementation
===============================================================================}
{$IFDEF CPU_x86x}

Function GetX87CW: Word; register; assembler;
var
  Temp: Word;
asm
    FSTCW   word ptr [Temp]
    MOV     AX, word ptr [Temp]
end;

//------------------------------------------------------------------------------

procedure SetX87CW(NewValue: Word); register; assembler;
var
  Temp: Word;
asm
    MOV     word ptr [Temp], NewValue
    FLDCW   word ptr [Temp]
end;

//------------------------------------------------------------------------------

Function GetMXCSR: LongWord; register; assembler;
var
  Temp: LongWord;
asm
    STMXCSR   dword ptr [Temp]
    MOV       EAX,  dword ptr [Temp]
end;

//------------------------------------------------------------------------------

procedure SetMXCSR(NewValue: LongWord); register; assembler;
var
  Temp: LongWord;
asm
    MOV       dword ptr [Temp], NewValue
    LDMXCSR   dword ptr [Temp]
end;

{$ENDIF}

//------------------------------------------------------------------------------
{$IFDEF CPU_x86x}
const
  DLU_SL_CPUFLAG_X87 = 1;
  DLU_SL_CPUFLAG_SSE = 2;

var
  VAR_SafeLoadCPUFlags: Integer = 0;  // written only during unit initialization
{$ENDIF}

type
  TDLUSafeLoadState = record
  {$IFDEF Windows}
    ErrorMode:  DWORD;
  {$ENDIF}
  {$IFDEF CPU_x86x}
    X87CW:      Word;
    MXCSR:      LongWord;
  {$ENDIF}
  end;

//------------------------------------------------------------------------------

procedure SL_SaveState(out State: TDLUSafeLoadState);
begin
{$IFDEF Windows}
State.ErrorMode := GetThreadErrorMode;
{$ENDIF}
{$IFDEF CPU_x86x}
If (VAR_SafeLoadCPUFlags and DLU_SL_CPUFLAG_X87) <> 0 then
  State.X87CW := GetX87CW;
If (VAR_SafeLoadCPUFlags and DLU_SL_CPUFLAG_SSE) <> 0 then
  State.MXCSR := GetMXCSR;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure SL_RestoreState(const State: TDLUSafeLoadState);
begin
{$IFDEF Windows}
SetThreadErrorMode(State.ErrorMode,nil);
{$ENDIF}
{$IFDEF CPU_x86x}
If (VAR_SafeLoadCPUFlags and DLU_SL_CPUFLAG_X87) <> 0 then
  SetX87CW(State.X87CW);
If (VAR_SafeLoadCPUFlags and DLU_SL_CPUFLAG_SSE) <> 0 then
  SetMXCSR(State.MXCSR);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure SL_Initialize;
begin
{$IFDEF CPU_x86x}
with TSimpleCPUID.Create do
try
  If Info.SupportedExtensions.X87 then
    VAR_SafeLoadCPUFlags := VAR_SafeLoadCPUFlags or DLU_SL_CPUFLAG_X87;
  If Info.SupportedExtensions.SSE then
    VAR_SafeLoadCPUFlags := VAR_SafeLoadCPUFlags or DLU_SL_CPUFLAG_SSE;
finally
  Free;
end;
{$ENDIF}
end;


{$IFDEF Windows}
{===============================================================================
    Process error mode management - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Process error mode management - internals
-------------------------------------------------------------------------------}
{
  "Solution" for thread safe error mode management (function GetThreadErrorMode
  and SetThreadErrorMode) in old systems (Windows Vista and older).
  Note that this solution is NOT in itself thread safe, it is here just to
  provide compatibility with older systems while allowing the use of newer
  system features.

  In Windows 7 and newer, the system functions GetThreadErrorMode and
  SetThreadErrorMode are used as usual.

  In older systems, functions that tries to emulate the functionality through
  use of SetErrorMode WinAPI function are called instead.
}

Function PEM_EMUL_GetThreadErrorMode: DWORD; stdcall;
begin
// note that GetErrorMode is available only from Windows Vista
Result := SetErrorMode(0);
SetErrorMode(Result);
end;

//------------------------------------------------------------------------------

Function PEM_EMUL_SetThreadErrorMode(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL; stdcall;
begin
If Assigned(lpOldMode) then
  lpOldMode^ := SetErrorMode(dwNewMode)
else
  SetErrorMode(dwNewMode);
Result := True;
end;

//------------------------------------------------------------------------------
var
  PEM_VAR_GetThreadErrorMode: Function: DWORD; stdcall = PEM_EMUL_GetThreadErrorMode;
  PEM_VAR_SetThreadErrorMode: Function(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL; stdcall = PEM_EMUL_SetThreadErrorMode;

//------------------------------------------------------------------------------

procedure PEM_Initialize;
var
  Module: TDLULibraryHandle;
begin
// for win7 and up, load "real" functions into procedural variables
If IsWindows7OrGreater then
  begin
    {
      kernel32.dll really should be loaded by this point, so there should be no
      need to call LoadLibrary (which might cause trouble because of a need to
      call FreeLibrary and so on)
    }
    Module := GetModuleHandle('kernel32.dll');
    If Module <> 0 then
      begin
        @PEM_VAR_GetThreadErrorMode := GetSymbolAddr(Module,'GetThreadErrorMode',[optExceptionOnFailure]);
        @PEM_VAR_SetThreadErrorMode := GetSymbolAddr(Module,'SetThreadErrorMode',[optExceptionOnFailure]);
      end
    else raise EDLULibraryOpenError.Create('Kernel32.dll not loaded.');
  end;
end;

{-------------------------------------------------------------------------------
    Process error mode management - public functions
-------------------------------------------------------------------------------}

Function GetThreadErrorMode: DWORD;
begin
Result := PEM_VAR_GetThreadErrorMode;
end;

//------------------------------------------------------------------------------

Function SetThreadErrorMode(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL;
begin
Result := PEM_VAR_SetThreadErrorMode(dwNewMode,lpOldMode);
end;

{$ENDIF}

{===============================================================================
    Symbol resolving auxiliary - implementation
===============================================================================}

Function Symbol(const Name: String; AddressVar: PPointer): TDLUSymbol;
begin
Result.Name := Name;
Result.AddressVar := AddressVar;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Symbol(AddressVar: PPointer; const Name: String): TDLUSymbol;
begin
Result.Name := Name;
Result.AddressVar := AddressVar;
end;


{===============================================================================
    Utility functions - implementation
===============================================================================}

Function LibraryIsPresent(const LibFileName: String): Boolean;
var
  LibraryHandle:  TDLULibraryHandle;
begin
LibraryHandle := OpenLibrary(LibFileName,[optNoCriticalError,optSafeLoad]);
try
  Result := CheckLibrary(LibraryHandle,[optDeepCheck]);
finally
  CloseLibrary(LibraryHandle,[optSafeLoad]);
end;
end;

//------------------------------------------------------------------------------

Function SymbolIsPresent(const LibFileName, SymbolName: String): Boolean;
var
  LibraryHandle:  TDLULibraryHandle;
  SymbolAddress:  Pointer;
begin
LibraryHandle := OpenLibrary(LibFileName,[optDeepCheck,optNoCriticalError,optExceptionOnFailure,optSafeLoad]);
try
  Result := GetSymbolAddr(LibraryHandle,SymbolName,SymbolAddress);
finally
  CloseLibrary(LibraryHandle,[optSafeLoad]);
end;
end;

//------------------------------------------------------------------------------

Function LibrarySymbolIsPresent(const LibFileName, SymbolName: String): Boolean;
var
  LibraryHandle:  TDLULibraryHandle;
  SymbolAddress:  Pointer;
begin
LibraryHandle := OpenLibrary(LibFileName,[optNoCriticalError,optSafeLoad]);
try
  If CheckLibrary(LibraryHandle,[optDeepCheck]) then
    Result := GetSymbolAddr(LibraryHandle,SymbolName,SymbolAddress)
  else
    Result := False;
finally
  CloseLibrary(LibraryHandle,[optSafeLoad]);
end;
end;

{===============================================================================
    External and system funtions - declaration
===============================================================================}
{$IFDEF Windows}
const
  UNICODE_STRING_MAX_CHARS = 32767;

Function SwitchToThread: BOOL; stdcall; external kernel32;

{$ELSE}//-----------------------------------------------------------------------
const
  RTLD_DI_LINKMAP = 2;

type
  plink_map = ^link_map;
  link_map = record
    l_addr: Pointer;
    l_name: PChar;
    l_ld:   Pointer;
    l_next: plink_map;
    l_prev: plink_map;
  end;

Function dlinfo(handle: Pointer; request: cInt; info: Pointer): cInt; cdecl; external;

Function sched_yield: cint; cdecl; external;
{$ENDIF}

{===============================================================================
    Internal auxiliary funtions - implementation
===============================================================================}

Function HandleToPtr(LibraryHandle: TDLULibraryHandle): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(LibraryHandle);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{===============================================================================
--------------------------------------------------------------------------------
                                Handle functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Handle functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Handle functions - library functions
-------------------------------------------------------------------------------}

Function CheckLibrary(LibraryHandle: TDLULibraryHandle; Options: TDLUOptions = []): Boolean;
{$IFDEF Windows}
var
  TempStr:  WideString;
begin
Result := LibraryHandle <> 0;
TempStr := '';
If Result and (optDeepCheck in Options) then
  begin
  {
    Note that I am NOT trying to get full module name here, this code will
    fail at doing that. So do not use it for that purpose!
  }
    SetLength(TempStr,UNICODE_STRING_MAX_CHARS);
    Result := GetModuleFileNameW(LibraryHandle,PWideChar(TempStr),Length(TempStr)) > 0;
  end;
end;
{$ELSE}
var
  TempMap:  link_map;
begin
Result := Assigned(LibraryHandle);
If Result and (optDeepCheck in Options) then
  Result := dlinfo(LibraryHandle,RTLD_DI_LINKMAP,@TempMap) = 0;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function OpenLibrary(const LibFileName: String; Options: TDLUOptions = []): TDLULibraryHandle; overload;
var
  SL_State:     TDLUSafeLoadState;
{$IFDEF Windows}
  OldErrorMode: DWORD;
{$ENDIF}
begin
If optSafeLoad in Options then
  SL_SaveState(SL_State);
try
{$IFDEF Windows}
  OldErrorMode := 0;
  // windows-only code
  If optNoCriticalError in Options then
    begin
      OldErrorMode := GetThreadErrorMode;
      SetThreadErrorMode(OldErrorMode or SEM_FAILCRITICALERRORS,nil);
    end;
  try
    Result := LoadLibraryEx(PSysChar(StrToSys(LibFileName)),0,0);
  finally
    If optNoCriticalError in Options then
      SetThreadErrorMode(OldErrorMode,nil);
  end;
{$ELSE}
  // linux-only code
  If optResolveNow in Options then
    Result := dlopen(PSysChar(StrToSys(LibFileName)),RTLD_NOW)
  else
    Result := dlopen(PSysChar(StrToSys(LibFileName)),RTLD_LAZY);
{$ENDIF}
  // common code, check for failures
  If optExceptionOnFailure in Options then
    If not CheckLibrary(Result,Options) then
      raise EDLULibraryOpenError.CreateFmt('OpenLibrary: Failed to open library "%s".',[LibFileName]);
finally
  If optSafeLoad in Options then
    SL_RestoreState(SL_State);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function OpenLibrary(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; Options: TDLUOptions = []): Boolean;
begin
{
  Following OpenLibrary calls CheckLibrary if option optExceptionOnFailure is
  active.
  We can assume, when this option is active and no exception was raised, that
  the check was already performed and is was succesfull, so we can optimize-out
  local check and return true straight away.
}
LibraryHandle := OpenLibrary(LibFileName,Options);
If not (optExceptionOnFailure in Options) then
  Result := CheckLibrary(LibraryHandle,Options)
else
  Result := True
end;

//------------------------------------------------------------------------------

procedure CloseLibrary(var LibraryHandle: TDLULibraryHandle; Options: TDLUOptions = []);
var
  SL_State: TDLUSafeLoadState;
begin
If optSafeLoad in Options then
  SL_SaveState(SL_State);
try
  If CheckLibrary(LibraryHandle,Options) then
    begin
    {$IFDEF Windows}
      If not FreeLibrary(LibraryHandle) then
        If optExceptionOnFailure in Options then
          raise EDLULibraryCloseError.CreateFmt('CloseLibrary: Failed to free library (%d).',[GetLastError]);
    {$ELSE}
      If dlclose(LibraryHandle) <> 0 then
        If optExceptionOnFailure in Options then
          raise EDLULibraryCloseError.CreateFmt('CloseLibrary: Failed to free library (%s).',[dlerror]);
    {$ENDIF}
      LibraryHandle := DefaultLibraryHandle;
    end;
finally
  If optSafeLoad in Options then
    SL_RestoreState(SL_State);
end;
end;

{-------------------------------------------------------------------------------
    Handle functions - symbols addresses
-------------------------------------------------------------------------------}

Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String; Options: TDLUOptions = []): Pointer;
{$IFNDEF Windows}
var
  ErrStr: PSysChar;
{$ENDIF}
begin
If CheckLibrary(LibraryHandle,Options) then
  begin
  {$IFDEF Windows}
    Result := GetProcAddress(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    If not Assigned(Result) and (optExceptionOnFailure in Options) then
      raise EDLUSymbolError.CreateFmt('GetSymbolAddr: Unable to resolve symbol "%s" (%d).',[SymbolName,GetLastError]);
  {$ELSE}
  {
    dlsym can return a VALID nil value, to check for errors, we have to look
    into what dlerror function returns after a call to dlsym. If it does not
    return anything (null/nil), we can assume no error has occured.
  }
    dlerror;  // clear last error
    Result := dlsym(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    If not Assigned(Result) and (optExceptionOnFailure in Options) then
      begin
        ErrStr := dlerror;
        If Assigned(ErrStr) then
          raise EDLUSymbolError.CreateFmt('GetSymbolAddr: Unable to resolve symbol "%s" (%s).',[SymbolName,SysToStr(ErrStr)]);
      end;
  {$ENDIF}
  end
else raise EDLUInvalidParameter.CreateFmt('GetSymbolAddr: Invalid library handle (0x%p).',[HandleToPtr(LibraryHandle)]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String; out Address: Pointer; Options: TDLUOptions = []): Boolean;
{$IFNDEF Windows}
var
  ErrStr: PSysChar;
{$ENDIF}
begin
If CheckLibrary(LibraryHandle,Options) then
  begin
  {$IFDEF Windows}
    Address := GetProcAddress(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    Result := Assigned(Address);
    If not Result and (optExceptionOnFailure in Options) then
      raise EDLUSymbolError.CreateFmt('GetSymbolAddr: Unable to resolve symbol "%s" (%d).',[SymbolName,GetLastError]);
  {$ELSE}
    dlerror;
    Address := dlsym(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    If not Assigned(Address) then
      begin
        ErrStr := dlerror;
        Result := not Assigned(ErrStr);
        If not Result and (optExceptionOnFailure in Options) then
          raise EDLUSymbolError.CreateFmt('GetSymbolAddr: Unable to resolve symbol "%s" (%s).',[SymbolName,SysToStr(ErrStr)]);
      end
    else Result := True;
  {$ENDIF}
  end
else raise EDLUInvalidParameter.CreateFmt('GetSymbolAddr: Invalid library handle (0x%p).',[HandleToPtr(LibraryHandle)]);
end;

{-------------------------------------------------------------------------------
    Handle functions - symbols resolving
-------------------------------------------------------------------------------}

Function ResolveSymbol(LibraryHandle: TDLULibraryHandle; Symbol: TDLUSymbol; Options: TDLUOptions = []): Boolean;
begin
Result := GetSymbolAddr(LibraryHandle,Symbol.Name,Symbol.AddressVar^,Options);
end;

//------------------------------------------------------------------------------

Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; const Symbols: array of TDLUSymbol; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
var
  i:  Integer;
begin
Result := 0;
Results := nil;
SetLength(Results,Length(Symbols));
{
  Check library here and then remove optDeepCheck from options to prevent
  unnecessary repeated deep checks in ResolveSymbol when this option is active.
}
If CheckLibrary(LibraryHandle,Options) then
  begin
    Exclude(Options,optDeepCheck);
    // initialize results
    For i := Low(Results) to High(Results) do
      begin
        Results[i].Processed := False;
        Results[i].Resolved := False;
      end;
    // traverse symbols
    For i := Low(Symbols) to High(Symbols) do
      begin
        Results[i].Processed := True;
        Results[i].Resolved := ResolveSymbol(LibraryHandle,Symbols[i],Options);
      {
        If ResolveSymbol raises an exception (when optExceptionOnFailure is
        included in options) then following is not executed.
      }
        If Results[i].Resolved then
          Inc(Result)
        else If optBreakOnUnresolved in Options then
          Break{For i};
      end;
  end
else raise EDLUInvalidParameter.CreateFmt('ResolveSymbols: Invalid library handle (0x%p).',[HandleToPtr(LibraryHandle)]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; const Symbols: array of TDLUSymbol; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := ResolveSymbols(LibraryHandle,Symbols,Results,Options);
end;

//------------------------------------------------------------------------------

Function ResolveSymbolList(LibraryHandle: TDLULibraryHandle; SymbolList: TStringList; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
var
  i:        Integer;
  TempPtr:  Pointer;
begin
Result := 0;
Results := nil;
SetLength(Results,SymbolList.Count);
If CheckLibrary(LibraryHandle,Options) then
  begin
    Exclude(Options,optDeepCheck);
    For i := Low(Results) to High(Results) do
      begin
        Results[i].Processed := False;
        Results[i].Resolved := False;
      end;
    For i := 0 to Pred(SymbolList.Count) do
      begin
        Results[i].Processed := True;
        Results[i].Resolved := ResolveSymbol(LibraryHandle,Symbol(SymbolList[i],@TempPtr),Options);
        If Results[i].Resolved then
          begin
            SymbolList.Objects[i] := TObject(TempPtr);
            Inc(Result);
          end
        else If optBreakOnUnresolved in Options then
          Break{For i};
      end;
  end
else raise EDLUInvalidParameter.CreateFmt('ResolveSymbolList: Invalid library handle (0x%p).',[HandleToPtr(LibraryHandle)]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResolveSymbolList(LibraryHandle: TDLULibraryHandle; SymbolList: TStringList; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := ResolveSymbolList(LibraryHandle,SymbolList,Results,Options);
end;

//------------------------------------------------------------------------------

Function ResolveSymbolNames(LibraryHandle: TDLULibraryHandle; const Names: array of String; const AddressVars: array of PPointer; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
var
  i:  Integer;
begin
Result := 0;
Results := nil;
SetLength(Results,Length(Names));
If Length(Names) <> Length(AddressVars) then
  raise EDLUInvalidParameter.CreateFmt('ResolveSymbolNames: Length of arrays do not match (%d,%d).',[Length(Names),Length(AddressVars)]);
If not CheckLibrary(LibraryHandle,Options) then
  raise EDLUInvalidParameter.CreateFmt('ResolveSymbolNames: Invalid library handle (0x%p).',[HandleToPtr(LibraryHandle)]);
Exclude(Options,optDeepCheck);
For i := Low(Results) to High(Results) do
  begin
    Results[i].Processed := False;
    Results[i].Resolved := False;
  end;
For i := Low(Names) to High(Names) do
  begin
    Results[i].Processed := True;
    Results[i].Resolved := ResolveSymbol(LibraryHandle,Symbol(Names[i],AddressVars[i]),Options);
    If Results[i].Resolved then
      Inc(Result)
    else If optBreakOnUnresolved in Options then
      Break{For i};
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResolveSymbolNames(LibraryHandle: TDLULibraryHandle; const Names: array of String; const AddressVars: array of PPointer; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := ResolveSymbolNames(LibraryHandle,Names,AddressVars,Results,Options);
end;

{-------------------------------------------------------------------------------
    Handle functions - macro functions
-------------------------------------------------------------------------------}

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; Symbols: array of TDLUSymbol; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
begin
LibraryHandle := OpenLibrary(LibFileName,Options);
try
{
  When optExceptionOnFailure is included in options, then handle validity check
  is performed in OpenLibrary. And if here, it means it was successful
  (otherwise and exception would be raised and following code would not be
  executed).
  This also means that in such a case, there is no need to perform more calls
  to CheckLibrary from ResolveSymbols - we cannot directly disable them, so we
  at least disable deep checks.
}
  If optExceptionOnFailure in Options then
    Exclude(Options,optDeepCheck);
  Result := ResolveSymbols(LibraryHandle,Symbols,Results,Options);
except
  CloseLibrary(LibraryHandle,Options);
  raise;  // re-raise exception
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; Symbols: array of TDLUSymbol; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := OpenLibraryAndResolveSymbols(LibFileName,LibraryHandle,Symbols,Results,Options);
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbolList(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; SymbolList: TStringList; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
begin
LibraryHandle := OpenLibrary(LibFileName,Options);
try
  If optExceptionOnFailure in Options then
    Exclude(Options,optDeepCheck);
  Result := ResolveSymbolList(LibraryHandle,SymbolList,Results,Options);
except
  CloseLibrary(LibraryHandle,Options);
  raise;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function OpenLibraryAndResolveSymbolList(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; SymbolList: TStringList; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := OpenLibraryAndResolveSymbolList(LibFileName,LibraryHandle,SymbolList,Results,Options);
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; const Names: array of String; const AddressVars: array of PPointer; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
begin
LibraryHandle := OpenLibrary(LibFileName,Options);
try
  If optExceptionOnFailure in Options then
    Exclude(Options,optDeepCheck);
  Result := ResolveSymbolNames(LibraryHandle,Names,AddressVars,Results,Options);
except
  CloseLibrary(LibraryHandle,Options);
  raise;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; const Names: array of String; const AddressVars: array of PPointer; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := OpenLibraryAndResolveSymbolNames(LibFileName,LibraryHandle,Names,AddressVars,Results,Options);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                Context functions
--------------------------------------------------------------------------------
===============================================================================}
type
  TDLULibraryContextInternal = packed record
    Contenders:   Integer;
    InitLock:     Integer;
    DataLock:     TCriticalSection;
    Internals:    packed record
      ResolvedSymbolCount:  Integer;  // used to hold true count of resolved symbols in data
    end;
    Data:         TDLULibraryContextData;
  end;
  PDLULibraryContextInternal = ^TDLULibraryContextInternal;

{$If SizeOf(TDLULibraryContext) < SizeOf(TDLULibraryContextInternal)}
  {$MESSAGE FATAL 'Public context too small.'}
{$IFEND}

const
  DLU_CTX_INITLOCK_UNINITIALIZED = 0;
  DLU_CTX_INITLOCK_INITIALIZED   = -1;
  DLU_CTX_INITLOCK_BEINGUPDATED  = 1; // any number except for 0 and -1 corresponds to this state

  DLU_CTX_SYMBOLLIST_GROWDELTA = 32;

{===============================================================================
    Context functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Context functions - internals
-------------------------------------------------------------------------------}

procedure ContextLock(var Context: TDLULibraryContextInternal); overload;
begin
InterlockedIncrement(Context.Contenders);
while True do
  case InterlockedCompareExchange(Context.InitLock,DLU_CTX_INITLOCK_BEINGUPDATED,DLU_CTX_INITLOCK_UNINITIALIZED) of
    DLU_CTX_INITLOCK_UNINITIALIZED: begin
    {
      InitLock now contains DLU_CTX_INITLOCK_BEINGUPDATED.
      
      Context is not initialized, do the initialization.
    }
      Context.DataLock := TCriticalSection.Create;
      // nobody should be in datalock, so it is save to assign OpenCount directly
      Context.Data.OpenCount := 0;
      // other fields are initialized by caller
      InterlockedStore(Context.InitLock,DLU_CTX_INITLOCK_INITIALIZED);
      Break{while...};
    end;
    DLU_CTX_INITLOCK_INITIALIZED:   begin
      // context is ready to be used, just continue
      Break{while...};
    end;
  else
  {
    Someone is either initializing or finalizing the context, wait a moment and
    try again.
  }
  {$IFDEF Windows}
    If not SwitchToThread then
  {$ELSE}
    If sched_yield <> 0 then
  {$ENDIF}
      Sleep(1);
    // cycle repeats...
  end;
Context.DataLock.Enter;
end;

//------------------------------------------------------------------------------

procedure ContextUnlock(var Context: TDLULibraryContextInternal); overload;
var
  DestroyLock:  Boolean;
begin
DestroyLock := Context.Data.OpenCount <= 0;
Context.DataLock.Leave;
If DestroyLock then
  while True do
    case InterlockedCompareExchange(Context.InitLock,DLU_CTX_INITLOCK_BEINGUPDATED,DLU_CTX_INITLOCK_INITIALIZED) of
      DLU_CTX_INITLOCK_UNINITIALIZED: begin
        // someone managed to do finalizing before us, just leave
        Break{while...};
      end;
      DLU_CTX_INITLOCK_INITIALIZED:   begin
      {
        InitLock now contains DLU_CTX_INITLOCK_BEINGUPDATED.

        Finalize the context, but only if we are the sole contender - this is
        because between our DataLock.Leave and CMPXCH, some other thread could
        have acquired the context lock and might be still holding it. If that
        would be the case, we would destroy the lock while it is held by that
        thread - no bueno. If contender count is one (or less) here, we can be
        fairly sure nobody else holds the lock or can acquire it before we
        release InitLock.
      }
        If InterlockedLoad(Context.Contenders) <= 1 then
          FreeAndNil(Context.DataLock);
        InterlockedStore(Context.InitLock,DLU_CTX_INITLOCK_UNINITIALIZED);
        Break{while...};      
      end;
    else
    {$IFDEF Windows}
      If not SwitchToThread then
    {$ELSE}
      If sched_yield <> 0 then
    {$ENDIF}
        Sleep(1);
    end;
InterlockedDecrement(Context.Contenders);     
end;

//------------------------------------------------------------------------------

Function ContextGetEffectiveOptions(const Context: TDLULibraryContextInternal; CallOptions: TDLUOptions): TDLUOptions;
begin
If (optContextOptions in Context.Data.Options) or (optContextOptions in CallOptions) then
  Result := (Context.Data.Options - [optNoSerialize]) + (CallOptions * [optNoSerialize])
else
  Result := CallOptions;
end;

//------------------------------------------------------------------------------

Function ContextCheckLibrary(const Context: TDLULibraryContextInternal; Options: TDLUOptions): Boolean;
begin
// this function only observes optDeepCheck, also the context must be locked
If Context.Data.OpenCount > 0 then
  Result := CheckLibrary(Context.Data.Handle,Options)
else
  Result := False;
end;

//------------------------------------------------------------------------------

procedure ContextGetFullFileName(var Context: TDLULibraryContextInternal);
{$IFDEF Windows}
var
  ModuleFileName: WideString;
  ReturnValue:    DWORD;
begin
ModuleFileName := '';
repeat
  SetLength(ModuleFileName,Length(ModuleFileName) + UNICODE_STRING_MAX_CHARS);
  ReturnValue := GetModuleFileNameW(Context.Data.Handle,PWideChar(ModuleFileName),Length(ModuleFileName))
until Integer(ReturnValue) < Length(ModuleFileName);
SetLength(ModuleFileName,ReturnValue);
Context.Data.FullFileName := WideToStr(ModuleFileName);
end;
{$ELSE}
var
  Info: plink_map;
begin
If dlinfo(Context.Data.Handle,RTLD_DI_LINKMAP,@Info) = 0 then
  Context.Data.FullFileName := ExpandFileName(SysToStr(Info^.l_name))
else
  Context.Data.FullFileName := '';  // do not raise exception, just use an empty string
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function ContextSymbolFind(var Context: TDLULibraryContextInternal; const SymbolName: String; out Index: Integer): Boolean;
var
  i:  Integer;
begin
Result := False;
Index := -1;
For i := Low(Context.Data.ResolvedSymbols) to Pred(Context.Internals.ResolvedSymbolCount) do
  // comparison must be case-sensitive
  If AnsiSameStr(SymbolName,Context.Data.ResolvedSymbols[i].Name) then
    begin
      Index := i;
      Result := True;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function ContextSymbolAdd(var Context: TDLULibraryContextInternal; const SymbolName: String; Address: Pointer): Integer;
begin
If not ContextSymbolFind(Context,SymbolName,Result) then
  begin
    If Length(Context.Data.ResolvedSymbols) <= Context.Internals.ResolvedSymbolCount then
      SetLength(Context.Data.ResolvedSymbols,Length(Context.Data.ResolvedSymbols) + DLU_CTX_SYMBOLLIST_GROWDELTA);
    Result := Context.Internals.ResolvedSymbolCount;
    Inc(Context.Internals.ResolvedSymbolCount);
  end;
Context.Data.ResolvedSymbols[Result].Name := SymbolName;
Context.Data.ResolvedSymbols[Result].Address := Address;
end;

{-------------------------------------------------------------------------------
    Context functions - utility functions
-------------------------------------------------------------------------------}

procedure ContextLock(var Context: TDLULibraryContext);
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
end;

//------------------------------------------------------------------------------

procedure ContextUnlock(var Context: TDLULibraryContext);
begin
ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;

//==============================================================================

Function ContextGetData(var Context: TDLULibraryContext; Options: TDLUOptions = []): TDLULibraryContextData;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
  i:            Integer;
begin
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Result := InternalCtx.Data;
      UniqueString(Result.OpenFileName);
      UniqueString(Result.FullFileName);
      // following always creates unique copy
      SetLength(Result.ResolvedSymbols,InternalCtx.Internals.ResolvedSymbolCount);
      For i := Low(Result.ResolvedSymbols) to High(Result.ResolvedSymbols) do
        UniqueString(Result.ResolvedSymbols[i].Name);
    end
  else raise EDLUInvalidParameter.Create('ContextGetData: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

//------------------------------------------------------------------------------

Function ContextGetOptions(var Context: TDLULibraryContext; Options: TDLUOptions = []): TDLUOptions;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    Result := InternalCtx.Data.Options
  else
    raise EDLUInvalidParameter.Create('ContextGetOptions: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

//------------------------------------------------------------------------------

Function ContextSetOptions(var Context: TDLULibraryContext; NewOptions: TDLUOptions; Options: TDLUOptions = []): TDLUOptions;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Result := InternalCtx.Data.Options;
      InternalCtx.Data.Options := NewOptions;
    end
  else raise EDLUInvalidParameter.Create('ContextSetOptions: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

//------------------------------------------------------------------------------

Function ContextGetOption(var Context: TDLULibraryContext; Option: TDLUOption; Options: TDLUOptions = []): Boolean;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
Result := False;  // dunno, delphi is complaining about undefined result
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    Result := Option in InternalCtx.Data.Options
  else
    raise EDLUInvalidParameter.Create('ContextGetOption: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

//------------------------------------------------------------------------------

Function ContextSetOption(var Context: TDLULibraryContext; Option: TDLUOption; NewState: Boolean; Options: TDLUOptions = []): Boolean;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
Result := False;
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Result := Option in InternalCtx.Data.Options;
      If NewState then
        Include(InternalCtx.Data.Options,Option)
      else
        Exclude(InternalCtx.Data.Options,Option);
    end
  else raise EDLUInvalidParameter.Create('ContextSetOption: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

//------------------------------------------------------------------------------

Function ContextIncludeOption(var Context: TDLULibraryContext; Option: TDLUOption; Options: TDLUOptions = []): TDLUOptions;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Result := InternalCtx.Data.Options;
      Include(InternalCtx.Data.Options,Option);
    end
  else raise EDLUInvalidParameter.Create('ContextIncludeOption: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

//------------------------------------------------------------------------------

Function ContextExcludeOption(var Context: TDLULibraryContext; Option: TDLUOption; Options: TDLUOptions = []): TDLUOptions;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Result := InternalCtx.Data.Options;
      Exclude(InternalCtx.Data.Options,Option);
    end
  else raise EDLUInvalidParameter.Create('ContextExcludeOption: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

{-------------------------------------------------------------------------------
    Context functions - library functions
-------------------------------------------------------------------------------}

Function CheckLibrary(var Context: TDLULibraryContext; Options: TDLUOptions = []): Boolean;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  Result := ContextCheckLibrary(InternalCtx,Options);
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

//------------------------------------------------------------------------------

Function OpenLibrary(const LibFileName: String; var Context: TDLULibraryContext; Options: TDLUOptions = []): Boolean;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
  NewHandle:    TDLULibraryHandle;
begin
Result := False;
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If InternalCtx.Data.OpenCount <= 0 then
    begin
      // initializing call
      InternalCtx.Data.Handle := OpenLibrary(LibFileName,Options);
    {
      If optExceptionOnFailure is active, it means the library handle was
      already checked in previous OpenLibrary call.
    }
      If not (optExceptionOnFailure in Options) then
        If not CheckLibrary(InternalCtx.Data.Handle,Options) then
          raise EDLULibraryOpenError.CreateFmt('OpenLibrary: Failed to open library "%s".',[LibFileName]);
      InternalCtx.Data.OpenCount := 1;
      InternalCtx.Data.OpenFileName := LibFileName;
      UniqueString(InternalCtx.Data.OpenFileName);
      ContextGetFullFileName(InternalCtx);
      UniqueString(InternalCtx.Data.FullFileName);
      InternalCtx.Data.Options := [];
      InternalCtx.Data.ResolvedSymbols := nil;
      InternalCtx.Internals.ResolvedSymbolCount := 0;      
      Result := True;
    end
  // the context was already used to load a library, ...
  else If optAlwaysLoad in Options then
    begin
      // ...load it again
      NewHandle := OpenLibrary(InternalCtx.Data.FullFileName,Options);
      If NewHandle <> InternalCtx.Data.Handle then
        raise EDLULibraryOpenError.CreateFmt('OpenLibrary: Failed to re-open library "%s".',[LibFileName]);
      Inc(InternalCtx.Data.OpenCount);
    end
  // ...do not load it again, only increment refcount
  else Inc(InternalCtx.Data.OpenCount);
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

//------------------------------------------------------------------------------

Function CloseLibrary(var Context: TDLULibraryContext; Options: TDLUOptions = []): Boolean;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
  TempHandle:   TDLULibraryHandle;
begin
Result := False;
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Dec(InternalCtx.Data.OpenCount);
      If InternalCtx.Data.OpenCount <= 0 then
        begin
          // last closing call, unload the library and finalize the context
          CloseLibrary(InternalCtx.Data.Handle,Options);  // sets handle to default
          InternalCtx.Data.OpenCount := 0;
          InternalCtx.Data.OpenFileName := '';
          InternalCtx.Data.FullFileName := '';
          InternalCtx.Data.Options := [];
          SetLength(InternalCtx.Data.ResolvedSymbols,0);
          InternalCtx.Internals.ResolvedSymbolCount := 0;
          Result := True;
        end
      else If optAlwaysLoad in Options then
        begin
          // unload library - preserve the handle (CloseLibrary would invalidate it)
          TempHandle := InternalCtx.Data.Handle;
          CloseLibrary(TempHandle,Options);
        end;
    end
  else raise EDLUInvalidParameter.Create('CloseLibrary: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

{-------------------------------------------------------------------------------
    Context functions - symbols addresses
-------------------------------------------------------------------------------}

Function GetSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String; Options: TDLUOptions = []): Pointer;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
  Index:        Integer;
begin
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Exclude(Options,optDeepCheck);
      // first try to find the symbol in a list of already resolved (if allowed)
      If optSymbolListResolve in Options then
        If ContextSymbolFind(InternalCtx,SymbolName,Index) then
          begin
            Result := InternalCtx.Data.ResolvedSymbols[Index].Address;
            Exit; // do not continue this function
          end;
      // do normal resolving
      If GetSymbolAddr(InternalCtx.Data.Handle,SymbolName,Result,Options) then
        If optSymbolListStore in Options then
          ContextSymbolAdd(InternalCtx,SymbolName,Result);
    end
  else raise EDLUInvalidParameter.Create('GetSymbolAddr: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String; out Address: Pointer; Options: TDLUOptions = []): Boolean;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
  Index:        Integer;
begin
Result := False;
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Exclude(Options,optDeepCheck);
      If optSymbolListResolve in Options then
        If ContextSymbolFind(InternalCtx,SymbolName,Index) then
          begin
            Address := InternalCtx.Data.ResolvedSymbols[Index].Address;
            Result := True;
            Exit;
          end;
      Result := GetSymbolAddr(InternalCtx.Data.Handle,SymbolName,Address,Options);
      If Result and (optSymbolListStore in Options) then
        ContextSymbolAdd(InternalCtx,SymbolName,Address);
    end
  else raise EDLUInvalidParameter.Create('GetSymbolAddr: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

{-------------------------------------------------------------------------------
    Context functions - symbols resolving
-------------------------------------------------------------------------------}

Function ResolveSymbol(var Context: TDLULibraryContext; Symbol: TDLUSymbol; Options: TDLUOptions = []): Boolean;
begin
Result := GetSymbolAddr(Context,Symbol.Name,Symbol.AddressVar^,Options);
end;

//------------------------------------------------------------------------------

Function ResolveSymbols(var Context: TDLULibraryContext; const Symbols: array of TDLUSymbol; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
  i:            Integer;
begin
Result := 0;
Results := nil;
SetLength(Results,Length(Symbols));
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  if optNoSerialize in options then
    writeln('boo');
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Exclude(Options,optDeepCheck);
      For i := Low(Results) to High(Results) do
        begin
          Results[i].Processed := False;
          Results[i].Resolved := False;
        end;
      For i := Low(Symbols) to High(Symbols) do
        begin
          Results[i].Processed := True;
        {
          Note that ResolveSymbol processes both optSymbolListResolve and
          optSymbolListStore options.

          Also no need to do locking in every call to ResolveSymbol later.
        }
          Results[i].Resolved := ResolveSymbol(Context,Symbols[i],Options + [optNoSerialize]);
          If Results[i].Resolved then
            Inc(Result)
          else If optBreakOnUnresolved in Options then
            Break{For i};
        end;
    end
  else raise EDLUInvalidParameter.Create('ResolveSymbols: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;    
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResolveSymbols(var Context: TDLULibraryContext; const Symbols: array of TDLUSymbol; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := ResolveSymbols(Context,Symbols,Results,Options);
end;

//------------------------------------------------------------------------------

Function ResolveSymbolList(var Context: TDLULibraryContext; SymbolList: TStringList; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
  i:            Integer;
  TempPtr:      Pointer;
begin
Result := 0;
Results := nil;
SetLength(Results,SymbolList.Count);
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Exclude(Options,optDeepCheck);
      For i := Low(Results) to High(Results) do
        begin
          Results[i].Processed := False;
          Results[i].Resolved := False;
        end;
      For i := 0 to Pred(SymbolList.Count) do
        begin
          Results[i].Processed := True;
          Results[i].Resolved := ResolveSymbol(Context,Symbol(SymbolList[i],@TempPtr),Options + [optNoSerialize]);
          If Results[i].Resolved then
            begin
              SymbolList.Objects[i] := TObject(TempPtr);
              Inc(Result);
            end
          else If optBreakOnUnresolved in Options then
            Break{For i};
        end;
    end
  else raise EDLUInvalidParameter.Create('ResolveSymbolList: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResolveSymbolList(var Context: TDLULibraryContext; SymbolList: TStringList; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := ResolveSymbolList(Context,SymbolList,Results,Options);
end;

//------------------------------------------------------------------------------

Function ResolveSymbolNames(var Context: TDLULibraryContext; const Names: array of String; const AddressVars: array of PPointer; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
  i:            Integer;
begin
Result := 0;
Results := nil;
SetLength(Results,Length(Names));
If Length(Names) <> Length(AddressVars) then
  raise EDLUInvalidParameter.CreateFmt('ResolveSymbolNames: Length of arrays do not match (%d,%d).',[Length(Names),Length(AddressVars)]);
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  If ContextCheckLibrary(InternalCtx,Options) then
    begin
      Exclude(Options,optDeepCheck);
      For i := Low(Results) to High(Results) do
        begin
          Results[i].Processed := False;
          Results[i].Resolved := False;
        end;
      For i := Low(Names) to High(Names) do
        begin
          Results[i].Processed := True;
          Results[i].Resolved := ResolveSymbol(Context,Symbol(Names[i],AddressVars[i]),Options + [optNoSerialize]);
          If Results[i].Resolved then
            Inc(Result)
          else If optBreakOnUnresolved in Options then
            Break{For i};
        end;
    end
  else raise EDLUInvalidParameter.Create('ResolveSymbolNames: Invalid context.');
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResolveSymbolNames(var Context: TDLULibraryContext; const Names: array of String; const AddressVars: array of PPointer; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := ResolveSymbolNames(Context,Names,AddressVars,Results,Options);
end;


{-------------------------------------------------------------------------------
    Context functions - macro functions
-------------------------------------------------------------------------------}

Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext; Symbols: array of TDLUSymbol; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
Result := 0;
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  OpenLibrary(LibFileName,Context,Options);
  try
    If optExceptionOnFailure in Options then
      Exclude(Options,optDeepCheck);
    Result := ResolveSymbols(Context,Symbols,Results,Options);
  except
    CloseLibrary(Context,Options);
    raise;  // re-raise exception
  end;
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext; Symbols: array of TDLUSymbol; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := OpenLibraryAndResolveSymbols(LibFileName,Context,Symbols,Results,Options);
end;
 
//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbolList(const LibFileName: String; var Context: TDLULibraryContext; SymbolList: TStringList; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
Result := 0;
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  OpenLibrary(LibFileName,Context,Options);
  try
    If optExceptionOnFailure in Options then
      Exclude(Options,optDeepCheck);
    Result := ResolveSymbolList(Context,SymbolList,Results,Options);
  except
    CloseLibrary(Context,Options);
    raise;
  end;
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function OpenLibraryAndResolveSymbolList(const LibFileName: String; var Context: TDLULibraryContext; SymbolList: TStringList; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := OpenLibraryAndResolveSymbolList(LibFileName,Context,SymbolList,Results,Options);
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; var Context: TDLULibraryContext; const Names: array of String; const AddressVars: array of PPointer; out Results: TDLUSymReslResults; Options: TDLUOptions = []): Integer;
var
  InternalCtx:  TDLULibraryContextInternal absolute Context;
begin
Result := 0;
If not (optNoSerialize in Options) then
  ContextLock(InternalCtx);
try
  Options := ContextGetEffectiveOptions(InternalCtx,Options);
  OpenLibrary(LibFileName,Context,Options);
  try
    If optExceptionOnFailure in Options then
      Exclude(Options,optDeepCheck);
    Result := ResolveSymbolNames(Context,Names,AddressVars,Results,Options);
  except
    CloseLibrary(Context,Options);
    raise;
  end;
finally
  If not (optNoSerialize in Options) then
    ContextUnlock(InternalCtx);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; var Context: TDLULibraryContext; const Names: array of String; const AddressVars: array of PPointer; Options: TDLUOptions = []): Integer;
var
  Results:  TDLUSymReslResults;
begin
Result := OpenLibraryAndResolveSymbolNames(LibFileName,Context,Names,AddressVars,Results,Options);
end;


{===============================================================================
    Unit initialization, finalization
===============================================================================}

{$IFDEF Windows}
initialization
  SL_Initialize;
  PEM_Initialize;
{$ENDIF}

end.
