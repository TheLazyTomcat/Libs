{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  UtilitySignal

    Small library designed to ease setup of real-time signal handler in Linux.
    It was designed primarily for use with posix timers (and possibly message
    queues), but can be, of course, used for other purposes too.

    I have decided to write it for two main reasons - one is to provide some
    siplified interface allowing for multiple handlers of single signal,
    the second is to limit number of used signals, of which count is very
    limited (30 or 32 per process in Linux), by allowing multiple users
    to use one signal allocated here.

    At unit initialization, this library selects and allocates one unused
    real-time signal and then installs an action routine that receives all
    incoming invocations of that signal.

      Note that this signal can be different every time the process is run. It
      can also differ between processes even if they are started from the same
      executable. Which means, among others, that this library cannot be used
      for interprocess communication, be aware of that!

      If this library is used multiple times within the same process (eg. when
      loaded with a dynamic library), this signal will be different for each
      instance. Because the number of available signals is limited, you should
      refrain from using this unit in a library or make sure one instance is
      shared across the entire process.

    The installed action routine processes the incoming signal and, using
    internal dispatcher object, passes processing to registered handlers. Which
    handlers will be called is selected according to a code received with the
    signal - only handlers (all of them) registered for the particular code
    will be called.

      Every handler has parameter BreakProcessing that is set to false upon
      entry. If handler sets this parameter to true before exitting, then no
      other handler registered for the code will be called for currently
      processed signal.

      WARNING - which thread will be processing any received signal is complety
                undefined and is selected arbitrarily (usually, it will the
                main thread, but do not count on that). You have to account for
                this fact when writing the handlers!

    Make sure you understand how signals work before using this library, so
    reading the linux manual (signal(7)) is strongly recommended.

  Version 1.0.1 (2024-08-19)

  Last change 2024-08-19

  ©2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.UtilitySignal

  Dependencies:
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions  - github.com/TheLazyTomcat/Lib.AuxExceptions
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps
    MulticastEvent - github.com/TheLazyTomcat/Lib.MulticastEvent

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol UtilitySignal_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit UtilitySignal;
{
  UtilitySignal_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  UtilitySignal_UseAuxExceptions to achieve this.
}
{$IF Defined(UtilitySignal_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------
{
  SilentDispatcherFailure

  When defined, any failure of acquiring internal signal dispatcher is silently
  ignored (the call will exit without performing its intended function). When
  not defined, then an exception of class EUSDispacherNotReeady is raised in
  such situation.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol UtilitySignal_SilentDispatcherFailure_OFF.
}
{$DEFINE SilentDispatcherFailure}
{$IFDEF UtilitySignal_SilentDispatcherFailure_OFF}
  {$UNDEF SilentDispatcherFailure}
{$ENDIF}

interface

uses
  SysUtils, BaseUnix
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EUSException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EUSIndexOutOfBounds  = class(EUSException);
  EUSInvalidValue      = class(EUSException);
  EUSSetupError        = class(EUSException);
  EUSDispacherNotReady = class(EUSException);

{===============================================================================
    Public types
===============================================================================}
type
  TUSSignalValue = record
    case Integer of
      0: (IntValue: Integer);
      1: (PtrValue: Pointer);
  end;

  TUSSignalInfo = record
    Signal: Integer;  // this will always be the same (SignalNumber)
    Code:   Integer;
    Value:  TUSSignalValue;
  end;

  TUSHandlerCallback = procedure(const Info: TUSSignalInfo; var BreakProcessing: Boolean);
  TUSHandlerEvent = procedure(const Info: TUSSignalInfo; var BreakProcessing: Boolean) of object;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - declaration
===============================================================================}
{
  SignalNumber

  Returns number of signal that was allocated for use by this library.
}
Function SignalNumber: Integer;

{
  CurrentProcessID

  Returns ID of the calling process. This can be used when sending a signal
  (see functions SendSignal further down).
}
Function CurrentProcessID: pid_t;

//------------------------------------------------------------------------------
{
  RegisterHandler

  Registers callback or event that will be called when allocated signal with
  given code is received. Overloads without Code argument are registering the
  handler for code SI_QUEUE (-1).

  A handler can be registered only once for each code, an attempt to register
  it again will silently fail (no exception will be raised).
}
procedure RegisterHandler(Code: Integer; Handler: TUSHandlerCallback);
procedure RegisterHandler(Code: Integer; Handler: TUSHandlerEvent);
procedure RegisterHandler(Handler: TUSHandlerCallback);
procedure RegisterHandler(Handler: TUSHandlerEvent);

{
  UnregisterHandler

  Unregisters callback or event from the given code, so it no longer is called
  when allocated signal with that code arrives. Overloads without Code argument
  are unregistering the handler from code SI_QUEUE (-1).

  Since handler can be registered only once for each code, unregistering it
  will remove it completely from processing of that code. Unregistering a
  handler that is not registered will silently fail (no exception is raised).
}
procedure UnregisterHandler(Code: Integer; Handler: TUSHandlerCallback);
procedure UnregisterHandler(Code: Integer; Handler: TUSHandlerEvent);
procedure UnregisterHandler(Handler: TUSHandlerCallback);
procedure UnregisterHandler(Handler: TUSHandlerEvent);

//------------------------------------------------------------------------------
{
  SendSignal

  Sends selected signal to a given process with given value.

  When the sending succeeds, true is returned and output parameter Error is set
  to 0. When it fails, false is returned and Error contains Linux error code
  that describes reason of failure.

  Note that sending signals is subject to privilege checks, so it might not be
  possible, depending on whan privileges the sending process have.

  The signal will arrive with code set to SI_QUEUE.

    WARNING - signals are quite deep subject, so do not use provided functions
              without considering what are you about to do. Always read the
              manual.
}
Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: TUSSignalValue; out Error: Integer): Boolean; overload;
Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: Integer; out Error: Integer): Boolean; overload;
Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: Pointer; out Error: Integer): Boolean; overload;

Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: TUSSignalValue): Boolean; overload;
Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: Integer): Boolean; overload;
Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: Pointer): Boolean; overload;

{
  Followng overloads are sending signal back to the calling process (but not
  necessarily the calling thread!) using the signal allocated for this library.
}
Function SendSignal(Value: TUSSignalValue; out Error: Integer): Boolean; overload;
Function SendSignal(Value: Integer; out Error: Integer): Boolean; overload;
Function SendSignal(Value: Pointer; out Error: Integer): Boolean; overload;

Function SendSignal(Value: TUSSignalValue): Boolean; overload;
Function SendSignal(Value: Integer): Boolean; overload;
Function SendSignal(Value: Pointer): Boolean; overload;

implementation

uses
  AuxClasses, MulticastEvent, InterlockedOps;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    System types, constants and externals
===============================================================================}
const
  SI_QUEUE = -1;

type
  sighandlerfce_t = procedure(signo: cint); cdecl;
  sigactionfce_t =  procedure(signo: cint; siginfo: psiginfo; context: Pointer); cdecl;

  sigset_t = array[0..Pred(1024 div (8 * SizeOf(culong)))] of culong;
  psigset_t = ^sigset_t;

  sigaction_t = record
    handler: record
      case Integer of
        0: (sa_handler:   sighandlerfce_t);
        1: (sa_sigaction: sigactionfce_t);
    end;
    sa_mask:      sigset_t;
    sa_flags:     cint;
    sa_restorer:  Pointer;
  end;
  psigaction_t = ^sigaction_t;

  sigval_t = record
    case Integer of
      0:  (sigval_int: cint);   // Integer value
      1:  (sigval_ptr: Pointer) // Pointer value
  end;

//------------------------------------------------------------------------------

Function getpid: pid_t; cdecl; external;

Function errno_ptr: pcint; cdecl; external name '__errno_location';

Function sigaction(signum: cint; act: psigaction_t; oact: psigaction_t): cint; cdecl; external;

Function sigemptyset(_set: psigset_t): cint; cdecl; external;

Function allocate_rtsig(high: cint): cint; cdecl; external name '__libc_allocate_rtsig';

Function sigqueue(pid: pid_t; sig: cint; value: sigval_t): cint; cdecl; external;


{===============================================================================
--------------------------------------------------------------------------------
                                 TUSCodeHandlers
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUSCodeHandlers - class declaration
===============================================================================}
type
  TUSCodeHandlers = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TUSHandlerCallback): Integer; reintroduce; overload;
    Function IndexOf(const Handler: TUSHandlerEvent): Integer; reintroduce; overload;
    Function Add(const Handler: TUSHandlerCallback): Integer; reintroduce; overload;
    Function Add(const Handler: TUSHandlerEvent): Integer; reintroduce; overload;
    Function Remove(const Handler: TUSHandlerCallback): Integer; reintroduce; overload;
    Function Remove(const Handler: TUSHandlerEvent): Integer; reintroduce; overload;
    procedure Call(const Info: TUSSignalInfo); reintroduce;
  end;

{===============================================================================
    TUSCodeHandlers - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUSCodeHandlers - public methods
-------------------------------------------------------------------------------}

Function TUSCodeHandlers.IndexOf(const Handler: TUSHandlerCallback): Integer;
begin
Result := inherited IndexOf(TCallback(Handler));
end;

//------------------------------------------------------------------------------

Function TUSCodeHandlers.IndexOf(const Handler: TUSHandlerEvent): Integer;
begin
Result := inherited IndexOf(TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TUSCodeHandlers.Add(const Handler: TUSHandlerCallback): Integer;
begin
Result := inherited Add(TCallback(Handler),False);
end;

//------------------------------------------------------------------------------

Function TUSCodeHandlers.Add(const Handler: TUSHandlerEvent): Integer;
begin
Result := inherited Add(TEvent(Handler),False);
end;
 
//------------------------------------------------------------------------------

Function TUSCodeHandlers.Remove(const Handler: TUSHandlerCallback): Integer;
begin
Result := inherited Remove(TCallback(Handler),True);
end;
 
//------------------------------------------------------------------------------

Function TUSCodeHandlers.Remove(const Handler: TUSHandlerEvent): Integer;
begin
Result := inherited Remove(TEvent(Handler),True);
end;
 
//------------------------------------------------------------------------------

procedure TUSCodeHandlers.Call(const Info: TUSSignalInfo);
var
  i:          Integer;
  BreakProc:  Boolean;
begin
BreakProc := False;
For i := LowIndex to HighIndex do
  begin
    If fEntries[i].IsMethod then
      TUSHandlerEvent(fEntries[i].HandlerMethod)(Info,BreakProc)
    else
      TUSHandlerCallback(fEntries[i].HandlerProcedure)(Info,BreakProc);
    If BreakProc then
      Break{for i};
  end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TUSDispatcher
--------------------------------------------------------------------------------
===============================================================================}
type
  TUSDispatcherItem = record
    Code:     Integer;
    Handlers: TUSCodeHandlers;
  end;

{===============================================================================
    TUSDispatcher - class declaration
===============================================================================}
type
  TUSDispatcher = class(TCustomListObject)
  protected
    fThreadLock:  TMultiReadExclusiveWriteSynchronizer;
    fItems:       array of TUSDispatcherItem;
    fItemCount:   Integer;
    fDirectMap:   array[-8..7] of Integer;
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    // list methods
    Function IndexOf(Code: Integer): Integer; virtual;
    Function Find(Code: Integer; out Index: Integer): Boolean; virtual;
    Function Add(Code: Integer): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
  public
    constructor Create;
    destructor Destroy; override;
  {
    Note that methods LowIndex, HighIndex, inherited CheckIndex and getters and
    setters of properties Count and Capacity are not thread protected. But this
    class is used only internally and is not exposed to outer world, therefore
    nobody should be able to access them.
  }
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    procedure RegisterHandler(Code: Integer; Handler: TUSHandlerCallback); overload; virtual;
    procedure RegisterHandler(Code: Integer; Handler: TUSHandlerEvent); overload; virtual;
    procedure UnregisterHandler(Code: Integer; Handler: TUSHandlerCallback); overload; virtual;
    procedure UnregisterHandler(Code: Integer; Handler: TUSHandlerEvent); overload; virtual;
    procedure Dispatch(const Info: TUSSignalInfo); overload; virtual; // called by signal handler
  end;

{===============================================================================
    TUSDispatcher - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUSDispatcher - protected methods
-------------------------------------------------------------------------------}

Function TUSDispatcher.GetCapacity: Integer;
begin
Result := Length(fItems);
end;

//------------------------------------------------------------------------------

procedure TUSDispatcher.SetCapacity(Value: Integer);
var
  i:  Integer;
begin
If Value >= 0 then
  begin
    If Value < fItemCount then
      begin
        For i := Value to HighIndex do
          FreeAndNil(fItems[i].Handlers);
        fItemCount := Value;
      end;
    SetLength(fItems,Value);
  end
else raise EUSInvalidValue.CreateFmt('TUSDispatcher.SetCapacity: Invalid capacity value (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TUSDispatcher.GetCount: Integer;
begin
Result := fItemCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TUSDispatcher.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TUSDispatcher.Initialize;
var
  i:  Integer;
begin
fThreadLock := TMultiReadExclusiveWriteSynchronizer.Create;
fItems := nil;
fItemCount := 0;
For i := Low(fDirectMap) to High(fDirectMap) do
  fDirectMap[i] := -1;
end;

//------------------------------------------------------------------------------

procedure TUSDispatcher.Finalize;
begin
Clear;
FreeAndNil(fThreadLock);
end;

//------------------------------------------------------------------------------

Function TUSDispatcher.IndexOf(Code: Integer): Integer;
var
  i:      Integer;
  L,H,C:  Integer;  // no, this is not Large Hadron Collider :P
begin
Result := -1;
If (Code >= Low(fDirectMap)) and (Code <= High(fDirectMap)) then
  Result := fDirectMap[Code]
else If fItemCount > 8 then
  begin
    L := LowIndex;
    H := HighIndex;
    while L <= H do
      begin
        C := (L + H) shr 1;  // div 2
        If Code < fItems[C].Code then
          H := Pred(C)
        else If Code > fItems[C].Code then
          L := Succ(C)
        else
          begin
            Result := C;
            Break{while};
          end;
      end;
  end
else
  begin
    For i := LowIndex to HighIndex do
      If fItems[i].Code = Code then
        begin
          Result := i;
          Break{For i};
        end;
  end;
end;

//------------------------------------------------------------------------------

Function TUSDispatcher.Find(Code: Integer; out Index: Integer): Boolean;
begin
Index := IndexOf(Code);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TUSDispatcher.Add(Code: Integer): Integer;
var
  i:  Integer;
begin
If not Find(Code,Result) then
  begin
    // find index for sorted addition
    Result := LowIndex;
    For i := LowIndex to HighIndex do
      If fItems[i].Code > Code then
        begin
          Result := i;
          Break{For i}
        end;
    Grow;
    For i := HighIndex downto Result do
      // yes, i + 1 is above HighIndex, but that item must exist because of Grow
      fItems[i + 1] := fItems[i];
    fItems[Result].Code := Code;
    fItems[Result].Handlers := TUSCodeHandlers.Create;
    Inc(fItemCount);
    If (Code >= Low(fDirectMap)) and (Code <= High(fDirectMap)) then
      fDirectMap[Code] := Result;
  end;
end;

//------------------------------------------------------------------------------

procedure TUSDispatcher.Delete(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    For i := Low(fDirectMap) to High(fDirectMap) do
      If fDirectMap[i] = Index then
        begin
          fDirectMap[i] := -1;
          Break{For i};
        end;
    FreeAndNil(fItems[Index].Handlers);
    For i := Index to Pred(HighIndex) do
      fItems[i] := fItems[i + 1];
    Dec(fItemCount);
    Shrink;
  end
else raise EUSIndexOutOfBounds.CreateFmt('TUSDispatcher.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TUSDispatcher.Clear;
var
  i:  Integer;
begin
For i := Low(fDirectMap) to High(fDirectMap) do
  fDirectMap[i] := -1;
For i := LowIndex to HighIndex do
  FreeAndNil(fItems[i].Handlers);
SetLength(fItems,0);
fItemCount := 0;
end;

{-------------------------------------------------------------------------------
    TUSDispatcher - public methods
-------------------------------------------------------------------------------}

constructor TUSDispatcher.Create;
begin
inherited Create;
initialize;
end;

//------------------------------------------------------------------------------

destructor TUSDispatcher.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TUSDispatcher.LowIndex: Integer;
begin
Result := Low(fItems);
end;

//------------------------------------------------------------------------------

Function TUSDispatcher.HighIndex: Integer;
begin
Result := Pred(fItemCount);
end;

//------------------------------------------------------------------------------

procedure TUSDispatcher.RegisterHandler(Code: Integer; Handler: TUSHandlerCallback);
var
  Index:  Integer;
begin
fThreadLock.BeginWrite;
try
  If not Find(Code,Index) then
    Index := Add(Code);
  fItems[Index].Handlers.Add(Handler);
finally
  fThreadLock.EndWrite;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TUSDispatcher.RegisterHandler(Code: Integer; Handler: TUSHandlerEvent);
var
  Index:  Integer;
begin
fThreadLock.BeginWrite;
try
  If not Find(Code,Index) then
    Index := Add(Code);
  fItems[Index].Handlers.Add(Handler);
finally
  fThreadLock.EndWrite;
end;
end;

//------------------------------------------------------------------------------

procedure TUSDispatcher.UnregisterHandler(Code: Integer; Handler: TUSHandlerCallback);
var
  Index:  Integer;
begin
fThreadLock.BeginWrite;
try
  If Find(Code,Index) then
    begin
      fItems[Index].Handlers.Remove(Handler);
      If fItems[Index].Handlers.Count <= 0 then
        Delete(Index);
    end;
finally
  fThreadLock.EndWrite;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TUSDispatcher.UnregisterHandler(Code: Integer; Handler: TUSHandlerEvent);
var
  Index:  Integer;
begin
fThreadLock.BeginWrite;
try
  If Find(Code,Index) then
    begin
      fItems[Index].Handlers.Remove(Handler);
      If fItems[Index].Handlers.Count <= 0 then
        Delete(Index);
    end;
finally
  fThreadLock.EndWrite;
end;
end;

//------------------------------------------------------------------------------

procedure TUSDispatcher.Dispatch(const Info: TUSSignalInfo);
var
  Index:  Integer;
begin
fThreadLock.BeginRead;
try
  If Find(Info.Code,Index) then
    fItems[Index].Handlers.Call(Info);
finally
  fThreadLock.EndRead;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - internal global variables
===============================================================================}
var
{
  GVAR_DispThrProt counter is used for thread protection of GVAR_Dispatcher
  variable, specifically to protect the object destruction (so it cannot be
  destroyed while some other thread is using it).

  On unit initialization, its value is set to High(Integer) - a maximum value
  it can hold (about two billions).

  On unit finalization, its current value, whatever it is, is negated. If,
  after this negation, its value becomes -High(Integer) (negative of maximum),
  then it is assumed nobody is currently using the dispatcher and it can be
  destroyed.

  When any thread wants to use the dispatcher, it has to first acquire it. This
  operation conditionally decrements the counter when it holds value above 1.

    If the condition is met and the decrement took place, then the user
    thread can safely access the dispatcher. Also, the dispatcher must be
    released after use (see further).

    If the counter was below or equel to 1, then the thread cannot acquire the
    dispatcher and must not use it. Release must not be called. Depending on
    defined symbols, this can lead to an exception being raised.

  When thread successfully acquired the dispatcher and now is done using it, it
  must release it.

    If the counter is zero, then nothing is done as this should not happen.
    If the counter is above 0, then it is incremented. If it is below zero,
    then it gets decremented.

    In any case, if value of the counter is below or equal to -High(Integer)
    after the release (meaning this unit was finalized and no other thread has
    the dispatcher acquired), then the calling thread must free the dispatcher.
    Note that, if the counter is negative but above -High(Integer), it means
    this unit was finalized but some other thread still uses the dispatcher.
    Last thread releasing it will destroy it.

  NOTE - In case of problems where some thread exits before incrementing the
         counter, the dispatcher will not be destroyed when the program exits.
         Some leaks might be reported when debugging, but in reality no memory
         is leaked because the process ended anyway (just not entirely cleanly).
         Therefore this eventuality is ignored.
}
  GVAR_DispThrProt:   Integer = 0;
  GVAR_Dispatcher:    TUSDispatcher = nil;
  GVAR_SignalNumber:  cint = 0;

{===============================================================================
    Procedural interface - internal routines
===============================================================================}

procedure DispatcherThreatProtectionInit;
begin
InterlockedStore(GVAR_DispThrProt,High(Integer));
end;

//------------------------------------------------------------------------------

Function DispatcherThreatProtectionFinal: Boolean;
begin
// true = can free the dispatcher
Result := InterlockedNeg(GVAR_DispThrProt) <= -High(Integer);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function AcquireInterlockedOp(A: Integer; var IntrRes: Integer): Integer; register;
begin
// IntrRes contains value of A on entry and is not changed
If A > 1 then
  Result := A - 1
else
  Result := A;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

Function DispatcherThreatProtectionAcquire: Boolean;
begin
// true = dispatcher is assigned and can be used (also do release)
Result := InterlockedOperation(GVAR_DispThrProt,AcquireInterlockedOp) > 1;
end;

//------------------------------------------------------------------------------

Function ReleaseInterlockedOp(A: Integer; var IntrRes: Integer): Integer; register;
begin
If A > 0 then
  Result := A + 1
else If A < 0 then
  Result := A - 1
else
  Result := 0;
IntrRes := Result;
end;

Function DispatcherThreatProtectionRelease: Boolean;
begin
// false = dispatcher must be freed now
Result := InterlockedOperation(GVAR_DispThrProt,ReleaseInterlockedOp) > -High(Integer);
end;

{===============================================================================
    Procedural interface - implementation
===============================================================================}

Function SignalNumber: Integer;
begin
Result := GVAR_SignalNumber;
end;

//------------------------------------------------------------------------------

Function CurrentProcessID: pid_t;
begin
Result := getpid;
end;

//==============================================================================

procedure RegisterHandler(Code: Integer; Handler: TUSHandlerCallback);
begin
If DispatcherThreatProtectionAcquire then
  try
    GVAR_Dispatcher.RegisterHandler(Code,Handler);
  finally
    If not DispatcherThreatProtectionRelease then
      FreeAndNil(GVAR_Dispatcher);
  end
{$IFNDEF SilentDispatcherFailure}
else raise EUSDispacherNotReady.Create('RegisterHandler: Dispatcher not ready.');
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RegisterHandler(Code: Integer; Handler: TUSHandlerEvent);
begin
If DispatcherThreatProtectionAcquire then
  try
    GVAR_Dispatcher.RegisterHandler(Code,Handler);
  finally
    If not DispatcherThreatProtectionRelease then
      FreeAndNil(GVAR_Dispatcher);
  end
{$IFNDEF SilentDispatcherFailure}
else raise EUSDispacherNotReady.Create('RegisterHandler: Dispatcher not ready.');
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RegisterHandler(Handler: TUSHandlerCallback);
begin
If DispatcherThreatProtectionAcquire then
  try
    GVAR_Dispatcher.RegisterHandler(SI_QUEUE,Handler);
  finally
    If not DispatcherThreatProtectionRelease then
      FreeAndNil(GVAR_Dispatcher);
  end
{$IFNDEF SilentDispatcherFailure}
else raise EUSDispacherNotReady.Create('RegisterHandler: Dispatcher not ready.');
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RegisterHandler(Handler: TUSHandlerEvent);
begin
If DispatcherThreatProtectionAcquire then
  try
    GVAR_Dispatcher.RegisterHandler(SI_QUEUE,Handler);
  finally
    If not DispatcherThreatProtectionRelease then
      FreeAndNil(GVAR_Dispatcher);
  end
{$IFNDEF SilentDispatcherFailure}
else raise EUSDispacherNotReady.Create('RegisterHandler: Dispatcher not ready.');
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure UnregisterHandler(Code: Integer; Handler: TUSHandlerCallback);
begin
If DispatcherThreatProtectionAcquire then
  try
    GVAR_Dispatcher.UnregisterHandler(Code,Handler);
  finally
    If not DispatcherThreatProtectionRelease then
      FreeAndNil(GVAR_Dispatcher);
  end
{$IFNDEF SilentDispatcherFailure}
else raise EUSDispacherNotReady.Create('UnregisterHandler: Dispatcher not ready.');
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure UnregisterHandler(Code: Integer; Handler: TUSHandlerEvent);
begin
If DispatcherThreatProtectionAcquire then
  try
    GVAR_Dispatcher.UnregisterHandler(Code,Handler);
  finally
    If not DispatcherThreatProtectionRelease then
      FreeAndNil(GVAR_Dispatcher);
  end
{$IFNDEF SilentDispatcherFailure}
else raise EUSDispacherNotReady.Create('UnregisterHandler: Dispatcher not ready.');
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure UnregisterHandler(Handler: TUSHandlerCallback);
begin
If DispatcherThreatProtectionAcquire then
  try
    GVAR_Dispatcher.UnregisterHandler(SI_QUEUE,Handler);
  finally
    If not DispatcherThreatProtectionRelease then
      FreeAndNil(GVAR_Dispatcher);
  end
{$IFNDEF SilentDispatcherFailure}
else raise EUSDispacherNotReady.Create('UnregisterHandler: Dispatcher not ready.');
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure UnregisterHandler(Handler: TUSHandlerEvent);
begin
If DispatcherThreatProtectionAcquire then
  try
    GVAR_Dispatcher.UnregisterHandler(SI_QUEUE,Handler);
  finally
    If not DispatcherThreatProtectionRelease then
      FreeAndNil(GVAR_Dispatcher);
  end
{$IFNDEF SilentDispatcherFailure}
else raise EUSDispacherNotReady.Create('UnregisterHandler: Dispatcher not ready.');
{$ENDIF}
end;

//==============================================================================

Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: TUSSignalValue; out Error: Integer): Boolean;
begin
Result := SendSignal(ProcessID,Signal,Value.PtrValue,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: Integer; out Error: Integer): Boolean;
var
  Temp: TUSSignalValue;
begin
FillChar(Addr(Temp)^,SizeOf(Temp),0);
Temp.IntValue := Value;
Result := SendSignal(ProcessID,Signal,Temp.PtrValue,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: Pointer; out Error: Integer): Boolean;
var
  SigValue: sigval_t;
begin
SigValue.sigval_ptr := Value;
If sigqueue(ProcessID,cint(Signal),SigValue) = 0 then
  begin
    Error := 0;
    Result := True;
  end
else
  begin
    Error := Integer(errno_ptr^);
    Result := False;
  end;
end;

//------------------------------------------------------------------------------

Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: TUSSignalValue): Boolean;
var
  Error: Integer;
begin
Result := SendSignal(ProcessID,Signal,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: Integer): Boolean;
var
  Error: Integer;
begin
Result := SendSignal(ProcessID,Signal,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(ProcessID: pid_t; Signal: Integer; Value: Pointer): Boolean;
var
  Error: Integer;
begin
Result := SendSignal(ProcessID,Signal,Value,Error);
end;

//------------------------------------------------------------------------------

Function SendSignal(Value: TUSSignalValue; out Error: Integer): Boolean;
begin
Result := SendSignal(getpid,GVAR_SignalNumber,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(Value: Integer; out Error: Integer): Boolean;
begin
Result := SendSignal(getpid,GVAR_SignalNumber,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(Value: Pointer; out Error: Integer): Boolean;
begin
Result := SendSignal(getpid,GVAR_SignalNumber,Value,Error);
end;

//------------------------------------------------------------------------------

Function SendSignal(Value: TUSSignalValue): Boolean;
var
  Error: Integer;
begin
Result := SendSignal(getpid,GVAR_SignalNumber,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(Value: Integer): Boolean;
var
  Error: Integer;
begin
Result := SendSignal(getpid,GVAR_SignalNumber,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(Value: Pointer): Boolean;
var
  Error: Integer;
begin
Result := SendSignal(getpid,GVAR_SignalNumber,Value,Error);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Signal handling
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Signal handling - internals
===============================================================================}

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure SignalHandler(signo: cint; siginfo: psiginfo; context: Pointer); cdecl;
var
  SignalInfo: TUSSignalInfo;
begin
// this function can by executed in the context of pretty much any random existing thread
If DispatcherThreatProtectionAcquire then
  try
    If signo = GVAR_SignalNumber then
      begin
        SignalInfo.Signal := Integer(signo);
        SignalInfo.Code := siginfo^.si_code;
        SignalInfo.Value.PtrValue := siginfo^._sifields._rt._sigval;
        GVAR_Dispatcher.Dispatch(SignalInfo);
      end;
  finally
    If not DispatcherThreatProtectionRelease then
      FreeAndNil(GVAR_Dispatcher);
  end;
// do not raise exceptions here even if SilentDispatcherFailure is not defined
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure InstallSignalAction;
var
  SignalAction: sigaction_t;
begin
// get unused signal number
GVAR_SignalNumber := allocate_rtsig(1);
If GVAR_SignalNumber < 0 then
  raise EUSSetupError.CreateFmt('SetupSignalHandler: Failed to allocate unused signal number (%d).',[errno_ptr^]);
// look if the selected signal is really unused (does not have handler assigned)
FillChar(Addr(SignalAction)^,SizeOf(sigaction_t),0);
If sigaction(GVAR_SignalNumber,nil,@SignalAction) <> 0 then
  raise EUSSetupError.CreateFmt('SetupSignalHandler: Failed to probe signal action (%d).',[errno_ptr^]);
If Assigned(SignalAction.handler.sa_sigaction) then
  raise EUSSetupError.CreateFmt('SetupSignalHandler: Allocated signal (%d) is already used.',[GVAR_SignalNumber]);
// setup signal handler
FillChar(Addr(SignalAction)^,SizeOf(sigaction_t),0);
SignalAction.handler.sa_sigaction := SignalHandler;
SignalAction.sa_flags := SA_SIGINFO;
// do not block anything
If sigemptyset(Addr(SignalAction.sa_mask)) <> 0 then
  raise EUSSetupError.CreateFmt('SetupSignalHandler: Emptying signal set failed (%d).',[errno_ptr^]);
If sigaction(GVAR_SignalNumber,@SignalAction,nil) <> 0 then
  raise EUSSetupError.CreateFmt('SetupSignalHandler: Failed to setup action for signal #%d (%d).',[GVAR_SignalNumber,errno_ptr^]);
end;

//------------------------------------------------------------------------------

procedure UninstallSignalAction;
var
  SignalAction: sigaction_t;
begin
// clear signal handler
FillChar(Addr(SignalAction)^,SizeOf(sigaction_t),0);
SignalAction.handler.sa_sigaction := nil;
If sigemptyset(Addr(SignalAction.sa_mask)) <> 0 then
  raise EUSSetupError.CreateFmt('UninstallSignalAction: Emptying signal set failed (%d).',[errno_ptr^]);
If sigaction(GVAR_SignalNumber,@SignalAction,nil) <> 0 then
  raise EUSSetupError.CreateFmt('UninstallSignalAction: Failed to setup action for signal #%d (%d).',[GVAR_SignalNumber,errno_ptr^]);
end;

//------------------------------------------------------------------------------

procedure DispatcherInit;
begin
{
  This is called at the module initialization, so no other thread that could
  use this unit can be running.
}
GVAR_Dispatcher := TUSDispatcher.Create;
DispatcherThreatProtectionInit;
end;

//------------------------------------------------------------------------------

procedure DispatcherFinal;
begin
If DispatcherThreatProtectionFinal then
  // nobody was using the dispatcher and it is now marked as unusable, free it
  FreeAndNil(GVAR_Dispatcher);
end;


{===============================================================================
--------------------------------------------------------------------------------
                      Unit initialization and finalization
--------------------------------------------------------------------------------
===============================================================================}
initialization
  DispatcherInit;
  InstallSignalAction;

finalization
  UninstallSignalAction;
  DispatcherFinal;

end.

