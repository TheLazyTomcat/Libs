{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple timer

    Simple non-visual interval timer.

    In Windows, it uses standard timer (SetTimer, KillTimer) that signals
    timeout using windows message (WM_TIMER). The on-timer event is called
    directly from handler of this message.
    In main thread, the messages are dispatched automatically, but if the
    timer is created in non-main thread, the method ProcessMessages must be
    called to process timer message and fire the on-timer event. 

    In Linux, it is based around POSIX interval timer (timer_create). The
    timeout is handled using signals, but the on-timer event is NOT called
    from signal handler. Instead, signal handler only sets a flag indicating
    timeout, and event is called from ProcessMessages method.
    In GUI application, if the timer is created in the main thread, method
    ProcessMessages is called automatically from application's on-idle event.
    In non-main thread, you are responsible to call this method - so the
    behavior is technically the same as in Windows OS.

  Version 1.2.4 (2024-08-19)

  Last change 2024-08-23

  ©2015-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleTimer

  Dependencies:
    AuxClasses    - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
  * UtilitySignal - github.com/TheLazyTomcat/Lib.UtilitySignal
  * UtilityWindow - github.com/TheLazyTomcat/Lib.UtilityWindow

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol SimpleTimer_UseAuxExceptions for details).

  Library UtilitySignal is required only when compiling for Linux OS.

  Library UtilityWindow is required only when compiling for Windows OS.

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps
    MulticastEvent - github.com/TheLazyTomcat/Lib.MulticastEvent
    SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils    - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo    - github.com/TheLazyTomcat/Lib.WinFileInfo
    WndAlloc       - github.com/TheLazyTomcat/Lib.WndAlloc

===============================================================================}
unit SimpleTimer;
{
  SimpleTimer_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  SimpleTimer_UseAuxExceptions to achieve this.
}
{$IF Defined(SimpleTimer_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
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

interface

uses
  {$IFDEF Windows}Windows, Messages,{$ENDIF} SysUtils,
  AuxTypes, AuxClasses{$IFDEF Windows}, UtilityWindow{$ENDIF}
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESTException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ESTTimerSetupError    = class(ESTException);
{$IFNDEF Windows}
  ESTTimerCreationError = class(ESTException);
  ESTTimerDeletionError = class(ESTException);
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                  TSimpleTimer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleTimer - class declaration
===============================================================================}
type
  TSimpleTimer = class(TCustomObject)
  protected
  {$IFDEF Windows}
    fOwnsWindow:      Boolean;
    fWindow:          TUtilityWindow;
  {$ELSE}
    fTimerExpired:    Integer;  // only for internal use
    fInMainThread:    Boolean;  // -//-
    fTimerCreated:    Boolean;
  {$ENDIF}
    fTimerID:         PtrUInt;
    fInterval:        UInt32;
    fEnabled:         Boolean;
    fTag:             Integer;
    fOnTimerEvent:    TNotifyEvent;
    fOnTimerCallback: TNotifyCallback;
    procedure SetInterval(Value: UInt32); virtual;
    procedure SetEnabled(Value: Boolean); virtual;
  {$IFDEF Windows}
    procedure Initialize(Window: TUtilityWindow; TimerID: PtrUInt); virtual;
  {$ELSE}
    procedure Initialize; virtual;
  {$ENDIF}
    procedure Finalize; virtual;
    procedure SetupTimer; virtual;
  {$IFDEF Windows}
    procedure MessagesHandler(var Msg: TMessage; var Handled: Boolean; Sent: Boolean); virtual;
  {$ELSE}
    procedure TimerExpired; virtual;
    procedure OnAppIdleHandler(Sender: TObject; var Done: Boolean); virtual;
  {$ENDIF}
    procedure DoOnTimer; virtual;
  public
  {$IFDEF Windows}
    constructor Create(Window: TUtilityWindow = nil; TimerID: PtrUInt = 1);
  {$ELSE}
    constructor Create;
  {$ENDIF}
    destructor Destroy; override;
    procedure ProcessMessages; virtual;
  {$IFDEF Windows}
    property OwnsWindow: Boolean read fOwnsWindow;
    property Window: TUtilityWindow read fWindow;
  {$ENDIF}
    property TimerID: PtrUInt read fTimerID;
    property Interval: UInt32 read fInterval write SetInterval;
    property Enabled: Boolean read fEnabled write SetEnabled;
    property Tag: Integer read fTag write fTag;
    property OnTimerCallback: TNotifyCallback read fOnTimerCallback write fOnTimerCallback;
    property OnTimerEvent: TNotifyEvent read fOnTimerEvent write fOnTimerEvent;
    property OnTimer: TNotifyEvent read fOnTimerEvent write fOnTimerEvent;
  end;

implementation

{$IFNDEF Windows}
uses
  Classes, BaseUnix, Linux{$IFDEF LCL}, Forms{$ENDIF},
  UtilitySignal;

{$LINKLIB RT}
{$ENDIF}

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                  TSimpleTimer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleTimer - internals
===============================================================================}
{$IFDEF Windows}

const
  USER_TIMER_MAXIMUM = $7FFFFFFF;
  USER_TIMER_MINIMUM = $0000000A;

{$ELSE}
const
  SIGEV_SIGNAL = 0;

  SI_TIMER = -2;

type
  timer_t  = PtrUInt; // in glic it is defined as void * (so pointer)
  ptimer_t = ^timer_t;

  sigval_t = record
    case Integer of
      0:  (sigval_int: cint);   // Integer value
      1:  (sigval_ptr: Pointer) // Pointer value
  end;

  sigevent_t = record
    sigev_value:              sigval_t;                             // Data passed with notification
    sigev_signo:              cint;                                 // Notification signal
    sigev_notify:             cint;                                 // Notification method
    sigev_notify_function:    procedure(sigval: sigval_t); cdecl;   // Function used for thread notification (SIGEV_THREAD)
    sigev_notify_attributes:  Pointer;                              // Attributes for notification thread (SIGEV_THREAD)
  end;
  psigevent_t = ^sigevent_t;

  timespec_t = record
    tv_sec:   time_t;
    tv_nsec:  clong;
  end;

  itimerspec_t = record
    it_interval:  timespec_t; // Timer interval
    it_value:     timespec_t; // Initial expiration
  end;
  pitimerspec_t = ^itimerspec_t;

Function timer_create(clockid: clockid_t; sevp: psigevent_t; timerid: ptimer_t): cint; cdecl; external;
Function timer_delete(timerid: timer_t): cint; cdecl; external;

Function timer_settime(timerid: timer_t; flags: cint; new_value,old_value: pitimerspec_t): cint; cdecl; external;

Function errno_ptr: pcint; cdecl; external name '__errno_location';

{-------------------------------------------------------------------------------
    TSimpleTimer - signal handler
-------------------------------------------------------------------------------}

procedure SignalHandler(const Info: TUSSignalInfo; var BreakProcessing: Boolean);
begin
If Assigned(Info.Value.PtrValue) then
  (TObject(Info.Value.PtrValue) as TSimpleTimer).TimerExpired;
BreakProcessing := False;
end;

//------------------------------------------------------------------------------

procedure RegisterSignalHandler;
begin
UtilitySignal.RegisterHandler(SI_TIMER,SignalHandler);
end;

//------------------------------------------------------------------------------

procedure UnregisterSignalHandler;
begin
UtilitySignal.UnregisterHandler(SI_TIMER,SignalHandler);
end;

{$ENDIF}

{===============================================================================
    TSimpleTimer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleTimer - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleTimer.SetInterval(Value: UInt32);
begin
{$IFDEF Windows}
If Value < USER_TIMER_MINIMUM then
  fInterval := USER_TIMER_MINIMUM
else If Value > USER_TIMER_MAXIMUM then
  fInterval := USER_TIMER_MAXIMUM
else
{$ENDIF}
  fInterval := Value;
SetupTimer;
end;

//------------------------------------------------------------------------------

procedure TSimpleTimer.SetEnabled(Value: Boolean);
begin
fEnabled := Value;
SetupTimer;
end;

//------------------------------------------------------------------------------

{$IFDEF Windows}
procedure TSimpleTimer.Initialize(Window: TUtilityWindow; TimerID: PtrUInt);
begin
If Assigned(Window) then
  begin
    fOwnsWindow := False;
    fWindow := Window;
  end
else
  begin
    fOwnsWindow := True;
    fWindow := TUtilityWindow.Create;
  end;
fWindow.OnMessage.Add(MessagesHandler);
fTimerID := TimerID;
{$ELSE}
procedure TSimpleTimer.Initialize;
var
  SignalEvent:  sigevent_t;
  NewTimerID:   timer_t;
begin
InterlockedExchange(fTimerExpired,0);
fInMainThread := MainThreadID = GetCurrentThreadID;
fTimerCreated := False; // just to be sure
// setup and create timer
FillChar(Addr(SignalEvent)^,SizeOf(sigevent_t),0);
SignalEvent.sigev_value.sigval_ptr := Pointer(Self);
SignalEvent.sigev_signo := cint(UtilitySignal.SignalNumber);
SignalEvent.sigev_notify := SIGEV_SIGNAL;
If timer_create(CLOCK_MONOTONIC,@SignalEvent,@NewTimerID) = 0 then
  begin
    fTimerID := PtrUInt(NewTimerID);
    fTimerCreated := True;
  end
else raise ESTTimerCreationError.CreateFmt('TSimpleTimer.Initialize: Failed to create timer (%d).',[errno_ptr^]);
{$ENDIF}
// set properties to default values
fInterval := 1000;
fEnabled := False;
fTag := 0;
fOnTimerEvent := nil;
fOnTimerCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TSimpleTimer.Finalize;
begin
fEnabled := False;
{$IFDEF Windows}
If Assigned(fWindow) then
  begin
    SetupTimer;
    If fOwnsWindow then
      fWindow.Free
    else
      fWindow.OnMessage.Remove(MessagesHandler);
  end;
{$ELSE}
If fTimerCreated then
  begin
    SetupTimer;
    If timer_delete(timer_t(fTimerID)) <> 0 then
      raise ESTTimerDeletionError.CreateFmt('Finalize.Initialize: Failed to delete timer (%d).',[errno_ptr^]);
  end;
{
  Note that signal handler stays assigned, but it should pose no problem as
  the timer is destroyed and will not invoke the signal handler anymore.
}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TSimpleTimer.SetupTimer;
{$IFDEF Windows}
begin
KillTimer(fWindow.WindowHandle,fTimerID);
If (fInterval > 0) and fEnabled then
  If SetTimer(fWindow.WindowHandle,fTimerID,fInterval,nil) = 0 then
    raise ESTTimerSetupError.CreateFmt('TSimpleTimer.SetupTimer: Failed to setup timer (0x%.8x).',[GetLastError]);
{$ELSE}
var
  TimerTime:  itimerspec_t;
begin
// disarm timer
FillChar(Addr(TimerTime)^,SizeOf(itimerspec_t),0);
If timer_settime(timer_t(fTimerID),0,@TimerTime,nil) <> 0 then
  raise ESTTimerSetupError.CreateFmt('TSimpleTimer.SetupTimer: Failed to disarm timer (%d).',[errno_ptr^]);
{$IFDEF LCL}
If fInMainThread then
  Application.RemoveOnIdleHandler(OnAppIdleHandler);
{$ENDIF}
// arm timer
If (fInterval > 0) and fEnabled then
  begin
    TimerTime.it_interval.tv_sec := fInterval div 1000;
    TimerTime.it_interval.tv_nsec := (fInterval mod 1000) * 1000000;
    TimerTime.it_value.tv_sec := TimerTime.it_interval.tv_sec;
    TimerTime.it_value.tv_nsec := TimerTime.it_interval.tv_nsec;
    If timer_settime(timer_t(fTimerID),0,@TimerTime,nil) <> 0 then
      raise ESTTimerSetupError.CreateFmt('TSimpleTimer.SetupTimer: Failed to arm timer (%d).',[errno_ptr^]);
  {$IFDEF LCL}
    If fInMainThread then
      Application.AddOnIdleHandler(OnAppIdleHandler,False);
  {$ENDIF}
  end;
{$ENDIF}
end;

{$IFDEF Windows}
//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSimpleTimer.MessagesHandler(var Msg: TMessage; var Handled: Boolean; Sent: Boolean);
begin
If (Msg.Msg = WM_TIMER) and (PtrUInt(Msg.wParam) = fTimerID) then
  begin
    DoOnTimer;
    Msg.Result := 0;
    Handled := True;
  end
else Handled := False;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{$ELSE}
//------------------------------------------------------------------------------

procedure TSimpleTimer.TimerExpired;
begin
InterlockedExchange(fTimerExpired,1);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSimpleTimer.OnAppIdleHandler(Sender: TObject; var Done: Boolean);
begin
ProcessMessages;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
{$ENDIF}

//------------------------------------------------------------------------------

procedure TSimpleTimer.DoOnTimer;
begin
If Assigned(fOnTimerEvent) then
  fOnTimerEvent(Self)
else If Assigned(fOnTimerCallback) then
  fOnTimerCallback(Self);
end;

{-------------------------------------------------------------------------------
    TSimpleTimer - public methods
-------------------------------------------------------------------------------}

{$IFDEF Windows}
constructor TSimpleTimer.Create(Window: TUtilityWindow = nil; TimerID: PtrUInt = 1);
{$ELSE}
constructor TSimpleTimer.Create;
{$ENDIF}
begin
inherited Create;
Initialize{$IFDEF Windows}(Window,TimerID){$ENDIF};
end;

//------------------------------------------------------------------------------

destructor TSimpleTimer.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleTimer.ProcessMessages;
begin
{$IFDEF Windows}
fWindow.ProcessMessages(False);
{$ELSE}
If InterlockedExchange(fTimerExpired,0) <> 0 then
  DoOnTimer;
{$ENDIF}
end;


{===============================================================================
--------------------------------------------------------------------------------
                        Unit initialization/finalization
--------------------------------------------------------------------------------
===============================================================================}

{$IFNDEF Windows}
initialization
  RegisterSignalHandler;

finalization
  UnregisterSignalHandler;
{$ENDIF}

end.
