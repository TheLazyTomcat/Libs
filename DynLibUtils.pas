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

  Version 1.3.1 (2023-10-18)

  Last change 2023-10-18

  ©2020-2023 František Milt

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
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
  * SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
  * WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion

  Library SimpleCPUID is required only when compiling for x86(-64) CPU.

  Library WindowsVersion is required only when compiling for Windows OS.

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
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
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
  AuxTypes;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EDLUException = class(Exception);

  EDLULibraryOpenError = class(EDLUException);
  EDLUInvalidParameter = class(EDLUException);
  EDLUSymbolError      = class(EDLUException);

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

  Tries to load the requested library. If it succeeded, it returns true,
  otherwise it will return false.

    NOTE - critical error dialog is suppressed.
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
}
Function SymbolIsPresent(const LibFileName, SymbolName: String): Boolean;


{===============================================================================
--------------------------------------------------------------------------------
                                Handle functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Handle functions - declaration
===============================================================================}
type
  TDLULibraryHandle = {$IFDEF Windows}THandle{$ELSE}Pointer{$ENDIF};
  PDLULibraryHandle = ^TDLULibraryHandle;

const
  DefaultLibraryHandle = TDLULibraryHandle({$IFDEF Windows}0{$ELSE}nil{$ENDIF});

{-------------------------------------------------------------------------------
    Handle functions - library functions
-------------------------------------------------------------------------------}
{
  CheckLibrary

  Returns true when passed parameter is initialized (is not null/nil), ie. it
  contains handle to a library, false otherwise.
}
Function CheckLibrary(LibraryHandle: TDLULibraryHandle): Boolean; overload;

{
  OpenLibrary

  Loads the requested library. It will return null/nil if it cannot be loaded.

  In Windows OS, when the library cannot be loaded for whatever reason, the
  called WinAPI function will initiate critical system error, which will
  display an error dialog (a window).
  This might be very obtrusive (eg. when probing DLL existence) - so if this
  behavior is undesirable, set SilentCriticalErrors parameter to true and this
  error dialog will be suppressed.

    NOTE - parameter SilentCriticalErrors has no effect in operating systems
           other than windows.

    WARNING - suppressing error dialog is inherently thread unsafe in systems
              older than Windows 7.
}
Function OpenLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle; overload;

{
  OpenLibraryAndCheck

  Works the same as OpenLibrary (in fact it is calling it), but when the
  library cannot be loaded (null/nil handle returned) it will raise an
  EDLULibraryOpenError exception.
}
Function OpenAndCheckLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle;

{
  SafeOpenLibrary

  Works the same as OpenLibrary, but it preserves some of the system settings
  across the call, namely process/thread error mode on Windows OS and X87
  control word and MXCSR register on x86(-64) processors.
  It is here for situations where the loaded library is changing those settings
  but this behavior is undesirable.
}
Function SafeOpenLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle; overload;

{
  SafeOpenAndCheckLibrary

  Works the same as OpenAndCheckLibrary, but preserves selected system settings
  (see SafeOpenLibrary for details).
}
Function SafeOpenAndCheckLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle;

{
  CloseLibrary

  Closes and potentially unloads the library (unloading is managed by OS).

  It checks the handle (function CheckLibrary) before processing, if it is not
  deemed to be valid, it will exit without doing anything.

  Note that it will invalide the library handle, irrespective of whether the OS
  unloads the library or not.
}
procedure CloseLibrary(var LibraryHandle: TDLULibraryHandle); overload;

{-------------------------------------------------------------------------------
    Handle functions - symbols addresses
-------------------------------------------------------------------------------}
{
  GetSymbolAddr

  Returns pointer to requested symbol. If it canot be resolved, it returns nil.

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception.
}
Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String): Pointer; overload;

{
  GetSymbolAddr

  Stores address of requested symbol to Address output parameter.

  Returns true when the symbol was properly resolved, false otherwise (in which
  case the value of Address is undefined).

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception.
}
Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String; out Address: Pointer): Boolean; overload;

{
  GetAndCheckSymbolAddr

  Tries to resolve requested symbol and return its address.

  If the requested symbol is not successfully resolved, then this function will
  raise an EDLUSymbolError exception. Otherwise it works the same as function
  GetSymbolAddr.

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception.
}
Function GetAndCheckSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String): Pointer; overload;

{-------------------------------------------------------------------------------
    Handle functions - symbols resolving
-------------------------------------------------------------------------------}
{
  ResolveSymbolNames

  Resolves given symbols and stores obtained pointers to variables pointed by
  the items in Addresses array. For each symbol name, the resolved address is
  stored at the same position in Addresses (eg. address for second name is
  stored at second position).

  If FailOnUnresolved is set to false, the function will try to resolve
  everything. That some names were not resoved is evidenced by returned value
  being lower than length of Names array.

    WARNING - it is not possible to discern which symbols were not resolved,
              as any symbol can be CORRECTLY resolved to nil. You have to test
              each symbol separately for example in calls to overload of
              GetSymbolAddr that indicates failure/success.

  If FailOnUnresolved is set to true, the function will raise an EDLUSymbolError
  exception on first unresolved symbol.

  Length of both Names and Addresses arrays must be the same, otherwise an
  EDLUInvalidParameter exception is raised.

  Returns number of succesfully resolved symbols.  
}
Function ResolveSymbolNames(LibraryHandle: TDLULibraryHandle; const Names: array of String;
  Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer; overload;

{
  ResolveSymbol

  Resolves symbol whose name is given in Name field of parameter Symbol and
  stores the address to a variable pointed to by the field AddressVar in the
  same parameter.

  When FailOnUnresolved is se to true, then the function will raise an
  EDLUSymbolError exception when the symbol cannot be resolved, otherwise the
  failure (false) or success (true) is indicated in the result.
}
Function ResolveSymbol(LibraryHandle: TDLULibraryHandle; Symbol: TDLUSymbol; FailOnUnresolved: Boolean = False): Boolean; overload;

{
  ResolveSymbols

  This function works the same as the ResolveSymbolNames, but names of
  individual symbols are taken from passed string list and resulting addresses
  are stored at respective places in Objects property (typecasted to TObject).

  The parameter Symbols is not declared as TStrings because that class does not
  fully implement Objects property.
}
Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer; overload;

{
  ResolveSymbols

  This overload is only repeatedly calling function ResolveSymbol to resolve
  individual TDLUSymbol structures in the passed array, see description of
  ResolveSymbol for details on its behavior.

  Other parameters and result value behaves the same as in the first overload
  of ResolveSymbols.
}
Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False): Integer; overload;

{-------------------------------------------------------------------------------
    Handle functions - macro functions
-------------------------------------------------------------------------------}
{
  OpenLibraryAndResolveSymbol(s/Names)

  Following functions are just an macro functions that are calling
  OpenAndCheckLibrary and then ResolveSymbol(s/Names). Refer to description of
  those functions for their behavior (raised exceptions, meaning of parameters,
  ...). 

  Returns a return value of particular ResolveSymbol(s/Names) used in the
  implementation of given function.

  If an exception is raised, then content of the output parameter LibraryHandle
  is undefined (when the exception is raised after the library was successfully
  opened, it gets closed)
}
Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; const Names: array of String;
  Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

{
  SafeOpenLibraryAndResolveSymbol(s/Names)

  These functions are working the same as OpenLibraryAndResolveSymbol(s/Names),
  but they preserve selected system settings (see SafeOpenLibrary for details).
}
Function SafeOpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle; const Names: array of String;
  Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

Function SafeOpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;
  
Function SafeOpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;


{===============================================================================
--------------------------------------------------------------------------------
                                Context functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Contexts are meant for situations where it is desirable to ensure that the
  library is loaded and unloaded only once - contexts ensure this using internal
  reference count. But note that this holds true only per context, not globally
  (the same library can be loaded using a different context).

  Because the loading indicates whether the library was already loaded or not,
  this can be also used for optimizations in symbol resolving (ie. resolving is
  performed only on the first load and is omitted on subsequent loads).

  Contexts can also provide more information about the library and some
  statistics, though this is currently implemented only in a limited form.
}
{===============================================================================
    Context functions - declaration
===============================================================================}
type
{
  Always make sure to initialize the context variable, either by assigning
  DefaultLibraryContext to it or by zeroing the memory.
}
  TDLULibraryContext = array[0..{$IFDEF CPU64bit}17{$ELSE}9{$ENDIF}] of UInt32;

  // used to store name:address pair in list of resolved symbols
  TDLUSymbolAddress = record
    Name:     String;
    Address:  Pointer;
  end;

  TDLULibraryContextData = record
    Handle:             TDLULibraryHandle;
    ReferenceCount:     Integer;
    OriginalFileName:   String;                     // string used to load the library
    TrueFileName:       String;                     // full path obtained back from the handle
    SymbolList:         array of TDLUSymbolAddress; // list of resolved symbols
    Options: record                                 
      SymbolListActive:   Boolean;
      SymbolListResolve:  Boolean;
    end;
  end;

const
  DefaultLibraryContext: TDLULibraryContext =
    (0,0,0,0,0,0,0,0,0,0{$IFDEF CPU64bit},0,0,0,0,0,0,0,0{$ENDIF});

{-------------------------------------------------------------------------------
    Context functions - utility functions
-------------------------------------------------------------------------------}
{
  ContextLock

  Locks the passed context so that only the current thread can access it.
  
  Nested locks are allowed, but remember to pair each lock operation with an
  unlock operation.
}
procedure ContextLock(var Context: TDLULibraryContext); overload;

{
  ContextUnlock

  Unlocks the passed context.
}
procedure ContextUnlock(var Context: TDLULibraryContext); overload;

{
  GetContextData

  Returns internal data of the passed context.

  Raises an EDLUInvalidParameter exception if the context is not valid.

    WARNING - if the context is accessed from multiple threads, the data might
              not be accurate by the time the function returns.
}
Function ContextGetData(var Context: TDLULibraryContext): TDLULibraryContextData;

{
  ContextSymbolListActive

  Activates or deactivates storing of resolved symbols into the symbol list.
  Existing list is not deleted when deactivating.

  When this option is active, every successful resolving of a symbol leads to
  that symbol being added into the symbol list along with its address.
  If option SymbolListResolve is inactive and the symbol is already present,
  its stored address is rewritten.

  To get current state of this option without changing it, use ContextGetData.

  Raises an EDLUInvalidParameter exception if the context is not valid.

  Initialy disabled.
}
Function ContextOptSymbolListActive(var Context: TDLULibraryContext; Activate: Boolean): Boolean;

{
  ContextSymbolListResolve

  Activates or deactivates resolving of symbols from symbol list when already
  present.

  When a symbol is being resolved (its address obtained) and this option is
  active, the the symbol is first looked for in the list of already resolved
  symbols. If found, then address stored in this list is returned. When not
  found or when this option is inactive, it is resolved as usual (using system
  calls).

  To get current state of this option without changing it, use ContextGetData.

  Raises an EDLUInvalidParameter exception if the context is not valid.

  Initialy disabled.

    NOTE - this option is independent of SymbolListActive.      
}
Function ContextOptSymbolListResolve(var Context: TDLULibraryContext; Activate: Boolean): Boolean;

{-------------------------------------------------------------------------------
    Context functions - library functions
-------------------------------------------------------------------------------}
{
  CheckLibrary

  Returns true when the passed context is valid, false otherwise.

  Context is considered valid when a library was previously successfully opened
  using it, ie. it contains a valid handle to a loaded library.
}
Function CheckLibrary(var Context: TDLULibraryContext): Boolean; overload;

{
  OpenLibrary

  If the context is not initialized, it opens the requested library using
  function OpenAndCheckLibrary, initializes the context and returns true.

  If the context is already initialized (that is, it was previously used to
  open a library), then the LibFileName is ignored (no check is performed
  whether the requested file is the same as was opened previously!) and only
  the context's internal reference count is incremented. In this case false is
  returned.

    WARNING - if the context is accessed by multiple threads, then the function
              can return false and not actually open the library even if the
              context was uninitialized prior the call to this function. This
              is because other thread can initialize it before the call is able
              to do its own initialization.
}
Function OpenLibrary(const LibFileName: String; var Context: TDLULibraryContext; SilentCriticalErrors: Boolean = False): Boolean; overload;

{
  SafeOpenLibrary

  Works the same as OpenLibrary, but preserves selected system settings (see
  SafeOpenLibrary function working with handle for details).
}
Function SafeOpenLibrary(const LibFileName: String; var Context: TDLULibraryContext; SilentCriticalErrors: Boolean = False): Boolean; overload;

{
  CloseLibrary

  Decrements reference count of the context, when it reaches zero it then
  unloads the library and invalidates the context.

  Result indicates whether the library was unloaded and context invalidated
  (true) or not (false).
}
Function CloseLibrary(var Context: TDLULibraryContext): Boolean; overload;

{-------------------------------------------------------------------------------
    Context functions - symbols addresses
-------------------------------------------------------------------------------}
{
  Following threee functions are behaviorally equivalent to functions of the
  same name and arguments list that are operating on handles instead of
  contexts, refer there for details.
}
Function GetSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String): Pointer; overload;
Function GetSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String; out Address: Pointer): Boolean; overload;

Function GetAndCheckSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String): Pointer; overload;

{-------------------------------------------------------------------------------
    Context functions - symbols resolving
-------------------------------------------------------------------------------}
{
  Following four funtions behave the same as their handle counterparts, see
  them for details.
}
Function ResolveSymbolNames(var Context: TDLULibraryContext; const Names: array of String;
  Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer; overload;

Function ResolveSymbol(var Context: TDLULibraryContext; Symbol: TDLUSymbol; FailOnUnresolved: Boolean = False): Boolean; overload;

Function ResolveSymbols(var Context: TDLULibraryContext; Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer; overload;
Function ResolveSymbols(var Context: TDLULibraryContext; Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False): Integer; overload;

{-------------------------------------------------------------------------------
    Context functions - macro functions
-------------------------------------------------------------------------------}
{
  See corresponding functions working on handles to get details about following
  six functions.
}
Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; var Context: TDLULibraryContext; const Names: array of String;
  Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;
Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

Function SafeOpenLibraryAndResolveSymbolNames(const LibFileName: String; var Context: TDLULibraryContext; const Names: array of String;
  Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

Function SafeOpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;    
Function SafeOpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

implementation

uses
  {$IFNDEF Windows}baseunix, dl,{$ENDIF} SyncObjs,
  StrRect{$IFDEF CPU_x86x}, SimpleCPUID{$ENDIF}
  {$IFDEF Windows}, WindowsVersion{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
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

procedure SafeLoadSaveState(out State: TDLUSafeLoadState);
begin
{$IFDEF Windows}
State.ErrorMode := GetThreadErrorMode;
{$ENDIF}
{$IFDEF CPU_x86x}
If (VAR_SafeLoadCPUFlags or DLU_SL_CPUFLAG_X87) <> 0 then
  State.X87CW := GetX87CW;
If (VAR_SafeLoadCPUFlags or DLU_SL_CPUFLAG_SSE) <> 0 then
  State.MXCSR := GetMXCSR;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure SafeLoadRestoreState(const State: TDLUSafeLoadState);
begin
{$IFDEF Windows}
SetThreadErrorMode(State.ErrorMode,nil);
{$ENDIF}
{$IFDEF CPU_x86x}
If (VAR_SafeLoadCPUFlags or DLU_SL_CPUFLAG_X87) <> 0 then
  SetX87CW(State.X87CW);
If (VAR_SafeLoadCPUFlags or DLU_SL_CPUFLAG_SSE) <> 0 then
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

Function EMUL_GetThreadErrorMode: DWORD; stdcall;
begin
// note that GetErrorMode is available only from Windows Vista
Result := SetErrorMode(0);
SetErrorMode(Result);
end;

//------------------------------------------------------------------------------

Function EMUL_SetThreadErrorMode(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL; stdcall;
begin
If Assigned(lpOldMode) then
  lpOldMode^ := SetErrorMode(dwNewMode)
else
  SetErrorMode(dwNewMode);
Result := True;
end;

//------------------------------------------------------------------------------
var
  VAR_GetThreadErrorMode: Function: DWORD; stdcall = EMUL_GetThreadErrorMode;
  VAR_SetThreadErrorMode: Function(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL; stdcall = EMUL_SetThreadErrorMode;

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
        @VAR_GetThreadErrorMode := GetAndCheckSymbolAddr(Module,'GetThreadErrorMode');
        @VAR_SetThreadErrorMode := GetAndCheckSymbolAddr(Module,'SetThreadErrorMode');
      end
    else raise EDLULibraryOpenError.Create('Kernel32.dll not loaded.');
  end;
end;

{-------------------------------------------------------------------------------
    Process error mode management - public functions
-------------------------------------------------------------------------------}

Function GetThreadErrorMode: DWORD;
begin
Result := VAR_GetThreadErrorMode;
end;

//------------------------------------------------------------------------------

Function SetThreadErrorMode(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL;
begin
Result := VAR_SetThreadErrorMode(dwNewMode,lpOldMode);
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
LibraryHandle := SafeOpenLibrary(LibFileName,True);
try
  Result := CheckLibrary(LibraryHandle);
finally
  CloseLibrary(LibraryHandle);  // if the handle is not valid, this won't do anything
end;
end;

//------------------------------------------------------------------------------

Function SymbolIsPresent(const LibFileName, SymbolName: String): Boolean;
var
  LibraryHandle:  TDLULibraryHandle;
  SymbolAddress:  Pointer;
begin
LibraryHandle := SafeOpenAndCheckLibrary(LibFileName,True);
try
  Result := GetSymbolAddr(LibraryHandle,SymbolName,SymbolAddress);
finally
  CloseLibrary(LibraryHandle);
end;
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

Function CheckLibrary(LibraryHandle: TDLULibraryHandle): Boolean;
begin
{$IFDEF Windows}
Result := LibraryHandle <> 0;
{$ELSE}
Result := Assigned(LibraryHandle);
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function OpenLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle;
{$IFDEF Windows}
var
  OldErrorMode: DWORD;
begin
OldErrorMode := 0;
If SilentCriticalErrors then
  begin
    OldErrorMode := GetThreadErrorMode;
    SetThreadErrorMode(OldErrorMode or SEM_FAILCRITICALERRORS,nil);
  end;
try
  Result := LoadLibraryEx(PSysChar(StrToSys(LibFileName)),0,0);
finally
  If SilentCriticalErrors then
    SetThreadErrorMode(OldErrorMode,nil);
end;
end;
{$ELSE}
begin
Result := dlopen(PSysChar(StrToSys(LibFileName)),RTLD_NOW);
end;
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function OpenAndCheckLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle;
begin
Result := OpenLibrary(LibFileName,SilentCriticalErrors);
If not CheckLibrary(Result) then
  raise EDLULibraryOpenError.CreateFmt('OpenLibrary: Failed to open library "%s".',[LibFileName]);
end;

//------------------------------------------------------------------------------

Function SafeOpenLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle;
var
  State:  TDLUSafeLoadState;
begin
SafeLoadSaveState(State);
try
  Result := OpenLibrary(LibFileName,SilentCriticalErrors);
finally
  SafeLoadRestoreState(State);
end;
end;

//------------------------------------------------------------------------------

Function SafeOpenAndCheckLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle;
var
  State:  TDLUSafeLoadState;
begin
SafeLoadSaveState(State);
try
  Result := OpenAndCheckLibrary(LibFileName,SilentCriticalErrors);
finally
  SafeLoadRestoreState(State);
end;
end;

//------------------------------------------------------------------------------

procedure CloseLibrary(var LibraryHandle: TDLULibraryHandle);
begin
If CheckLibrary(LibraryHandle) then
  begin
  {$IFDEF Windows}
    FreeLibrary(LibraryHandle);
    LibraryHandle := 0;
  {$ELSE}
    dlclose(LibraryHandle); // it can fail, but let's ignore it here
    LibraryHandle := nil;
  {$ENDIF}
  end;
end;


{-------------------------------------------------------------------------------
    Handle functions - symbols addresses
-------------------------------------------------------------------------------}

Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String): Pointer;
begin
If CheckLibrary(LibraryHandle) then
{$IFDEF Windows}
  Result := GetProcAddress(LibraryHandle,PSysChar(StrToSys(SymbolName)))
{$ELSE}
  Result := dlsym(LibraryHandle,PSysChar(StrToSys(SymbolName)))
{$ENDIF}
else
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  raise EDLUInvalidParameter.CreateFmt('GetSymbolAddr: Invalid library handle (0x%p).',[Pointer(LibraryHandle)]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String; out Address: Pointer): Boolean;
begin
If CheckLibrary(LibraryHandle) then
  begin
  {$IFDEF Windows}
    Address := GetProcAddress(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    Result := Assigned(Address);
  {$ELSE}
  {
    dlsym can return a VALID nil value, to check for errors, we have to look
    into what dlerror function returns after a call to dlsym, if it does not
    return anything (null/nil), we can assume no error has occured
  }
    dlerror;  // clear last error
    Address := dlsym(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    Result := not Assigned(dlerror);
  {$ENDIF}
  end
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
else raise EDLUInvalidParameter.CreateFmt('GetSymbolAddr: Invalid library handle (0x%p).',[Pointer(LibraryHandle)]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetAndCheckSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String): Pointer;
{$IFNDEF Windows}
var
  ErrorMsg: PSysChar;
{$ENDIF}
begin
If CheckLibrary(LibraryHandle) then
  begin
  {$IFDEF Windows}
    Result := GetProcAddress(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    If not Assigned(Result) then
      raise EDLUSymbolError.CreateFmt('GetAndCheckSymbolAddr: Unable to resolve symbol "%s" (0x%.8x).',[SymbolName,GetLastError]);
  {$ELSE}
    dlerror;  // clear last error
    Result := dlsym(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    ErrorMsg := dlerror;
    If Assigned(ErrorMsg) then
      raise EDLUSymbolError.CreateFmt('GetAndCheckSymbolAddr: Unable to resolve symbol "%s" (%s).',[SymbolName,SysToStr(ErrorMsg)]);
  {$ENDIF}
  end
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
else raise EDLUInvalidParameter.CreateFmt('GetAndCheckSymbolAddr: Invalid library handle (0x%p).',[Pointer(LibraryHandle)]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;


{-------------------------------------------------------------------------------
    Handle functions - symbols resolving
-------------------------------------------------------------------------------}

Function ResolveSymbolNames(LibraryHandle: TDLULibraryHandle; const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer;
var
  i:  Integer;
begin
Result := 0;
If Length(Names) = Length(Addresses) then
  begin
    If FailOnUnresolved then
      begin
        For i := Low(Names) to High(Names) do
          Addresses[i]^ := GetAndCheckSymbolAddr(LibraryHandle,Names[i]);
        Result := Length(Names);
      end
    else
      begin
        For i := Low(Names) to High(Names) do
          If GetSymbolAddr(LibraryHandle,Names[i],Addresses[i]^) then
            Inc(Result);
      end;
  end
else raise EDLUInvalidParameter.CreateFmt('ResolveSymbolNames: Length of arrays do not match (%d,%d).',[Length(Names),Length(Addresses)]);
end;

//------------------------------------------------------------------------------

Function ResolveSymbol(LibraryHandle: TDLULibraryHandle; Symbol: TDLUSymbol; FailOnUnresolved: Boolean = False): Boolean;
begin
If FailOnUnresolved then
  begin
    Symbol.AddressVar^ := GetAndCheckSymbolAddr(LibraryHandle,Symbol.Name);
    Result := True; // if something would be wrong, the GetAndCheckSymbolAddr would raise an exception
  end
else Result := GetSymbolAddr(LibraryHandle,Symbol.Name,Symbol.AddressVar^);
end;

//------------------------------------------------------------------------------

Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer;
var
  i:        Integer;
  TempPtr:  Pointer;
begin
Result := 0;
If FailOnUnresolved then
  begin
    For i := 0 to Pred(Symbols.Count) do
      Symbols.Objects[i] := TObject(GetAndCheckSymbolAddr(LibraryHandle,Symbols[i]));
    Result := Symbols.Count;
  end
else
  begin
    For i := 0 to Pred(Symbols.Count) do
      If GetSymbolAddr(LibraryHandle,Symbols[i],TempPtr) then
        begin
          Symbols.Objects[i] := TObject(TempPtr);
          Inc(Result);
        end
      else Symbols.Objects[i] := nil;
  end;
end;

//------------------------------------------------------------------------------

Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False): Integer; overload;
var
  i:  Integer;
begin
Result := 0;
For i := Low(Symbols) to High(Symbols) do
  If ResolveSymbol(LibraryHandle,Symbols[i],FailOnUnresolved) then
    Inc(Result);
end;


{-------------------------------------------------------------------------------
    Handle functions - macro functions
-------------------------------------------------------------------------------}

Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
begin
LibraryHandle := OpenAndCheckLibrary(LibFileName,SilentCriticalErrors);
try
  Result := ResolveSymbolNames(LibraryHandle,Names,Addresses,FailOnUnresolved);
except
  CloseLibrary(LibraryHandle);
  raise;  // re-raise exception
end;
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
begin
LibraryHandle := OpenAndCheckLibrary(LibFileName,SilentCriticalErrors);
try
  Result := ResolveSymbols(LibraryHandle,Symbols,FailOnUnresolved);
except
  CloseLibrary(LibraryHandle);
  raise;
end;
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
begin
LibraryHandle := OpenAndCheckLibrary(LibFileName,SilentCriticalErrors);
try
  Result := ResolveSymbols(LibraryHandle,Symbols,FailOnUnresolved);
except
  CloseLibrary(LibraryHandle);
  raise;
end;
end;

//------------------------------------------------------------------------------

Function SafeOpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
var
  State:  TDLUSafeLoadState;
begin
SafeLoadSaveState(State);
try
  Result := OpenLibraryAndResolveSymbolNames(LibFileName,LibraryHandle,Names,Addresses,FailOnUnresolved,SilentCriticalErrors);
finally
  SafeLoadRestoreState(State);
end;
end;

//------------------------------------------------------------------------------

Function SafeOpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
var
  State:  TDLUSafeLoadState;
begin
SafeLoadSaveState(State);
try
  Result := OpenLibraryAndResolveSymbols(LibFileName,LibraryHandle,Symbols,FailOnUnresolved,SilentCriticalErrors);
finally
  SafeLoadRestoreState(State);
end;
end;

//------------------------------------------------------------------------------
  
Function SafeOpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
var
  State:  TDLUSafeLoadState;
begin
SafeLoadSaveState(State);
try
  Result := OpenLibraryAndResolveSymbols(LibFileName,LibraryHandle,Symbols,FailOnUnresolved,SilentCriticalErrors);
finally
  SafeLoadRestoreState(State);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                Context functions
--------------------------------------------------------------------------------
===============================================================================}
type
  TDLULibraryContextInternal = record
    Contenders: Integer;
    InitLock:   Integer;
    DataLock:   TCriticalSection;
    Data:       TDLULibraryContextData;
    Internals:  record
      SymbolListCount: Integer;
    end;
  end;
  PDLULibraryContextInternal = ^TDLULibraryContextInternal;

{$If SizeOf(TDLULibraryContext) < SizeOf(TDLULibraryContextInternal)}
  {$MESSAGE FATAL 'Public context too small.'}
{$IFEND}

const
  DLU_CTX_INITLOCK_UNLOCKED = 0;
  DLU_CTX_INITLOCK_LOCKED   = -1;

  DLU_CTX_SYMBOLLIST_GROWDELTA = 32;

  DLU_CTX_RESOLVEMETHOD_ADDRESS = 0;
  DLU_CTX_RESOLVEMETHOD_VALID   = 1;
  DLU_CTX_RESOLVEMETHOD_RAISE   = 2;

{$IFDEF Windows}
const
  UNICODE_STRING_MAX_CHARS = 32767;

Function GetModuleFileNameExW(hProcess: THandle; hModule: THandle;
  lpFilename: PWideChar; nSize: DWORD): DWORD; stdcall; external 'psapi.dll';

{$ELSE}
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

{$ENDIF}

{===============================================================================
    Context functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Context functions - internals
-------------------------------------------------------------------------------}

procedure ContextLock(var Context: TDLULibraryContextInternal); overload;
var
  IsDone: Boolean;
begin
InterlockedIncrement(Context.Contenders);
repeat
  If InterlockedExchange(Context.InitLock,DLU_CTX_INITLOCK_LOCKED) = DLU_CTX_INITLOCK_UNLOCKED then
    begin
      // context was unlocked and is now locked, create data section is necessary
      If not Assigned(Context.DataLock) then
        Context.DataLock := TCriticalSection.Create;
      InterlockedExchange(Context.InitLock,DLU_CTX_INITLOCK_UNLOCKED);
      Context.DataLock.Enter;
      IsDone := True;
    end
  else
    begin
      // context was locked, wait a moment and try again
      Sleep(1);
      IsDone := False;
    end;
until IsDone;
end;

//------------------------------------------------------------------------------

procedure ContextUnlock(var Context: TDLULibraryContextInternal); overload;
var
  DestroyLock:  Boolean;
  IsDone:       Boolean;
begin
DestroyLock := Context.Data.ReferenceCount <= 0;
Context.DataLock.Leave;
If DestroyLock then
  repeat
    If InterlockedExchange(Context.InitLock,DLU_CTX_INITLOCK_LOCKED) = DLU_CTX_INITLOCK_UNLOCKED then
      begin
        If InterlockedExchangeAdd(Context.Contenders,0) <= 1 then
          FreeAndNil(Context.DataLock);
        InterlockedExchange(Context.InitLock,DLU_CTX_INITLOCK_UNLOCKED);
        IsDone := True;
      end
    else
      begin
        Sleep(1);
        IsDone := False;
      end;
  until IsDone;
InterlockedDecrement(Context.Contenders);
end;

//------------------------------------------------------------------------------

Function ContextSymbolResolve(var Context: TDLULibraryContextInternal; const SymbolName: String; out Address: Pointer; Method: Integer): Boolean;

  Function SymbolIndexOf: Integer;
  var
    i:  Integer;
  begin
    Result := -1;
    For i := Low(Context.Data.SymbolList) to High(Context.Data.SymbolList) do
      // symbol names are case sensitive...
      If AnsiSameStr(Context.Data.SymbolList[i].Name,SymbolName) then
        begin
          Result := i;
          Break{For i};
        end;
  end;

  procedure ContextSymbolAdd(Address: Pointer);
  var
    Index:  Integer;
  begin
    If Length(Context.Data.SymbolList) >= Context.Internals.SymbolListCount then
      SetLength(Context.Data.SymbolList,Length(Context.Data.SymbolList) + DLU_CTX_SYMBOLLIST_GROWDELTA);
    Index := Context.Internals.SymbolListCount;
    Context.Data.SymbolList[Index].Name := SymbolName;
    Context.Data.SymbolList[Index].Address := Address;
    Inc(Context.Internals.SymbolListCount);
  end;

var
  Index:  Integer;
begin
// context is assumed to be locked here
If Context.Data.Options.SymbolListActive or Context.Data.Options.SymbolListResolve then
  Index := SymbolIndexOf
else
  Index := -1;
Result := True;
If not Context.Data.Options.SymbolListResolve or (Index < 0) then
  begin
    case Method of
      DLU_CTX_RESOLVEMETHOD_VALID:
        Result := GetSymbolAddr(Context.Data.Handle,SymbolName,Address);
      DLU_CTX_RESOLVEMETHOD_RAISE:
        begin
          // following raises an exception in case of failure
          Address := GetAndCheckSymbolAddr(Context.Data.Handle,SymbolName);
          Result := True;
        end;
    else
     {DLU_CTX_RESOLVEMETHOD_ADDRESS}
      Address := GetSymbolAddr(Context.Data.Handle,SymbolName);
      Result := True;
    end;
    If Result and Context.Data.Options.SymbolListActive then
      begin
        If Index >= 0 then
          begin
            Context.Data.SymbolList[Index].Name := SymbolName;
            Context.Data.SymbolList[Index].Address := Address;
          end
        else ContextSymbolAdd(Address);
      end;
  end
else Address := Context.Data.SymbolList[Index].Address;
end;

//------------------------------------------------------------------------------

Function GetLibTrueFilePath(Handle: TDLULibraryHandle): String;
{$IFDEF Windows}
var
  ModuleFileName: WideString;
begin
SetLength(ModuleFileName,UNICODE_STRING_MAX_CHARS);
SetLength(ModuleFileName,GetModuleFileNameExW(GetCurrentProcess,Handle,
  PWideChar(ModuleFileName),Length(ModuleFileName)));
Result := WideToStr(ModuleFileName);
end;
{$ELSE}
var
  Info: plink_map;
begin
If dlinfo(Handle,RTLD_DI_LINKMAP,@Info) = 0 then
  Result := ExpandFileName(SysToStr(Info^.l_name))
else
  Result := ''; // do not raise exception, just return an empty string
end;
{$ENDIF}

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

//------------------------------------------------------------------------------

Function ContextGetData(var Context: TDLULibraryContext): TDLULibraryContextData;
var
  i:  Integer;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  If CheckLibrary(PDLULibraryContextInternal(@Context)^.Data.Handle) then
    begin
      Result := PDLULibraryContextInternal(@Context)^.Data;
      UniqueString(Result.OriginalFileName);
      UniqueString(Result.TrueFileName);
      // following always creates unique copy
      SetLength(Result.SymbolList,PDLULibraryContextInternal(@Context)^.Internals.SymbolListCount);
      For i := Low(Result.SymbolList) to High(Result.SymbolList) do
        UniqueString(Result.SymbolList[i].Name);
    end
  else raise EDLUInvalidParameter.Create('ContextGetData: Invalid context.');
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

//------------------------------------------------------------------------------

Function ContextOptSymbolListActive(var Context: TDLULibraryContext; Activate: Boolean): Boolean;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  with PDLULibraryContextInternal(@Context)^ do
    begin
      Result := Data.Options.SymbolListActive;
      If CheckLibrary(Data.Handle) then
        Data.Options.SymbolListActive := Activate
      else
        raise EDLUInvalidParameter.Create('ContextOptSymbolListActive: Invalid context.');
    end;
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

//------------------------------------------------------------------------------

Function ContextOptSymbolListResolve(var Context: TDLULibraryContext; Activate: Boolean): Boolean;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  with PDLULibraryContextInternal(@Context)^ do
    begin
      Result := Data.Options.SymbolListResolve;
      If CheckLibrary(Data.Handle) then
        Data.Options.SymbolListResolve := Activate
      else
        raise EDLUInvalidParameter.Create('ContextOptSymbolListResolve: Invalid context.');
    end;
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

{-------------------------------------------------------------------------------
    Context functions - library functions
-------------------------------------------------------------------------------}

Function CheckLibrary(var Context: TDLULibraryContext): Boolean;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  Result := CheckLibrary(PDLULibraryContextInternal(@Context)^.Data.Handle);
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

//------------------------------------------------------------------------------

Function OpenLibrary(const LibFileName: String; var Context: TDLULibraryContext; SilentCriticalErrors: Boolean = False): Boolean;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  with PDLULibraryContextInternal(@Context)^ do
    begin
      If Data.ReferenceCount <= 0 then
        begin
          Data.Handle := OpenAndCheckLibrary(LibFileName,SilentCriticalErrors);
          // init data
          Data.ReferenceCount := 1;
          Data.OriginalFileName := LibFileName;
          UniqueString(Data.OriginalFileName);
          Data.TrueFileName := GetLibTrueFilePath(Data.Handle);
          UniqueString(Data.TrueFileName);
          Data.Options.SymbolListActive := False;
          Data.Options.SymbolListResolve := False;
          SetLength(Data.SymbolList,0);
          Internals.SymbolListCount := 0;
          Result := True;
        end
      else
        begin
          Inc(Data.ReferenceCount);
          Result := False;
        end;
    end;
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

//------------------------------------------------------------------------------

Function SafeOpenLibrary(const LibFileName: String; var Context: TDLULibraryContext; SilentCriticalErrors: Boolean = False): Boolean;
var
  State:  TDLUSafeLoadState;
begin
SafeLoadSaveState(State);
try
  Result := OpenLibrary(LibFileName,Context,SilentCriticalErrors);
finally
  SafeLoadRestoreState(State);
end;
end;

//------------------------------------------------------------------------------

Function CloseLibrary(var Context: TDLULibraryContext): Boolean;
begin
Result := False;
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  with PDLULibraryContextInternal(@Context)^ do
    begin
      Dec(Data.ReferenceCount);
      If Data.ReferenceCount <= 0 then
        begin
          CloseLibrary(Data.Handle);
          // clear data
          Data.Handle := DefaultLibraryHandle;
          Data.ReferenceCount := 0;
          Data.OriginalFileName := '';
          Data.TrueFileName := '';
          Data.Options.SymbolListActive := False;
          Data.Options.SymbolListResolve := False;
          SetLength(Data.SymbolList,0);
          Internals.SymbolListCount := 0;
          Result := True;
        end;
    end;
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

{-------------------------------------------------------------------------------
    Context functions - symbols addresses
-------------------------------------------------------------------------------}

Function GetSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String): Pointer;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,SymbolName,Result,DLU_CTX_RESOLVEMETHOD_ADDRESS);
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String; out Address: Pointer): Boolean;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  Result := ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,SymbolName,Address,DLU_CTX_RESOLVEMETHOD_VALID);
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

//------------------------------------------------------------------------------

Function GetAndCheckSymbolAddr(var Context: TDLULibraryContext; const SymbolName: String): Pointer;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,SymbolName,Result,DLU_CTX_RESOLVEMETHOD_RAISE);
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

{-------------------------------------------------------------------------------
    Context functions - symbols resolving
-------------------------------------------------------------------------------}

Function ResolveSymbolNames(var Context: TDLULibraryContext; const Names: array of String;
  Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer;
var
  i:  Integer;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  Result := 0;
  If Length(Names) = Length(Addresses) then
    begin
      If FailOnUnresolved then
        begin
          For i := Low(Names) to High(Names) do
            ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,Names[i],Addresses[i]^,DLU_CTX_RESOLVEMETHOD_RAISE);
          Result := Length(Names);
        end
      else
        begin
          For i := Low(Names) to High(Names) do
            If ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,Names[i],Addresses[i]^,DLU_CTX_RESOLVEMETHOD_VALID) then
              Inc(Result);
        end;
    end
  else raise EDLUInvalidParameter.CreateFmt('ResolveSymbolNames: Length of arrays do not match (%d,%d).',[Length(Names),Length(Addresses)]);
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

//------------------------------------------------------------------------------

Function ResolveSymbol(var Context: TDLULibraryContext; Symbol: TDLUSymbol; FailOnUnresolved: Boolean = False): Boolean;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  If FailOnUnresolved then
    begin
      ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,Symbol.Name,Symbol.AddressVar^,DLU_CTX_RESOLVEMETHOD_RAISE);
      Result := True;
    end
  else Result := ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,Symbol.Name,Symbol.AddressVar^,DLU_CTX_RESOLVEMETHOD_VALID);
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

//------------------------------------------------------------------------------

Function ResolveSymbols(var Context: TDLULibraryContext; Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer;
var
  i:        Integer;
  TempPtr:  Pointer;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  Result := 0;
  If FailOnUnresolved then
    begin
      For i := 0 to Pred(Symbols.Count) do
        begin
          ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,Symbols[i],TempPtr,DLU_CTX_RESOLVEMETHOD_RAISE);
          Symbols.Objects[i] := TObject(TempPtr);
        end;
      Result := Symbols.Count;
    end
  else
    begin
      For i := 0 to Pred(Symbols.Count) do
        If ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,Symbols[i],TempPtr,DLU_CTX_RESOLVEMETHOD_VALID) then
          begin
            Symbols.Objects[i] := TObject(TempPtr);
            Inc(Result);
          end
        else Symbols.Objects[i] := nil;
    end;
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResolveSymbols(var Context: TDLULibraryContext; Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False): Integer;
var
  i:  Integer;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  Result := 0;
  // do not use ResolveSymbol to avoid context relock
  If FailOnUnresolved then
    begin
      For i := Low(Symbols) to High(Symbols) do
        ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,Symbols[i].Name,Symbols[i].AddressVar^,DLU_CTX_RESOLVEMETHOD_RAISE);
      Result := Length(Symbols);
    end
  else
    begin
      For i := Low(Symbols) to High(Symbols) do
        If ContextSymbolResolve(PDLULibraryContextInternal(@Context)^,Symbols[i].Name,Symbols[i].AddressVar^,DLU_CTX_RESOLVEMETHOD_VALID) then
          Inc(Result);
    end;
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

{-------------------------------------------------------------------------------
    Context functions - macro functions
-------------------------------------------------------------------------------}

Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; var Context: TDLULibraryContext; const Names: array of String;
  Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
begin
// following code will recursively relock the context, but the critical section should handle this
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  Result := 0;
  OpenLibrary(LibFileName,Context,SilentCriticalErrors);
  try
    Result := ResolveSymbolNames(Context,Names,Addresses,FailOnUnresolved);
  except
    CloseLibrary(Context);
    raise; 
  end;
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  Result := 0;
  OpenLibrary(LibFileName,Context,SilentCriticalErrors);
  try
    Result := ResolveSymbols(Context,Symbols,FailOnUnresolved);
  except
    CloseLibrary(Context);
    raise;
  end;
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
begin
ContextLock(PDLULibraryContextInternal(@Context)^);
try
  Result := 0;
  OpenLibrary(LibFileName,Context,SilentCriticalErrors);
  try
    Result := ResolveSymbols(Context,Symbols,FailOnUnresolved);
  except
    CloseLibrary(Context);
    raise;
  end;
finally
  ContextUnlock(PDLULibraryContextInternal(@Context)^);
end;
end;

//------------------------------------------------------------------------------

Function SafeOpenLibraryAndResolveSymbolNames(const LibFileName: String; var Context: TDLULibraryContext; const Names: array of String;
  Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
var
  State:  TDLUSafeLoadState;
begin
SafeLoadSaveState(State);
try
  Result := OpenLibraryAndResolveSymbolNames(LibFileName,Context,Names,Addresses,FailOnUnresolved,SilentCriticalErrors);
finally
  SafeLoadRestoreState(State);
end;
end;

//------------------------------------------------------------------------------

Function SafeOpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
var
  State:  TDLUSafeLoadState;
begin
SafeLoadSaveState(State);
try
  Result := OpenLibraryAndResolveSymbols(LibFileName,Context,Symbols,FailOnUnresolved,SilentCriticalErrors);
finally
  SafeLoadRestoreState(State);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
Function SafeOpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
var
  State:  TDLUSafeLoadState;
begin
SafeLoadSaveState(State);
try
  Result := OpenLibraryAndResolveSymbols(LibFileName,Context,Symbols,FailOnUnresolved,SilentCriticalErrors);
finally
  SafeLoadRestoreState(State);
end;
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
