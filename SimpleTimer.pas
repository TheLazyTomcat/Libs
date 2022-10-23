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

  Version 1.2 (2021-11-28)

  Last change 2022-09-14

  ©2015-2022 František Milt

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
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
  * UtilityWindow  - github.com/TheLazyTomcat/Lib.UtilityWindow
  * MulticastEvent - github.com/TheLazyTomcat/Lib.MulticastEvent
  * WndAlloc       - github.com/TheLazyTomcat/Lib.WndAlloc
  * StrRect        - github.com/TheLazyTomcat/Lib.StrRect
  * SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID

  SimpleCPUID is required only when PurePascal symbol is not defined.

  Libraries UtilityWindow, MulticastEvent, WndAlloc, StrRect and SimpleCPUID
  are required only when compiling for Windows OS.

===============================================================================}
unit SimpleTimer;

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
  AuxTypes, AuxClasses{$IFDEF Windows}, UtilityWindow{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESTException = class(Exception);

  ESTTimerSetupError    = class(ESTException);
{$IFDEF Linux}
  ESTSignalSetupError   = class(ESTException);
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
    fTimerExpired:    Boolean;  // only for internal use
    fInMainThread:    Boolean;  // -//-
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
  {$ENDIF}
  {$IFDEF Linux}
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
    procedure ProcessMassages; virtual;
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

{$IFDEF Linux}
uses
  Classes, BaseUnix, Linux{$IFDEF LCL}, Forms{$ENDIF};

{$LINKLIB RT}
{$LINKLIB C}
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
  timer_t  = cint;
  ptimer_t = ^timer_t;

  sigval_t = record
    case Integer of
      0:  (sigval_int: cint);   // Integer value
      1:  (sigval_ptr: Pointer) // Pointer value
  end;

  sigevent = record
    sigev_value:              sigval_t;                             // Data passed with notification
    sigev_signo:              cint;                                 // Notification signal
    sigev_notify:             cint;                                 // Notification method
    sigev_notify_function:    procedure(sigval: sigval_t); cdecl;   // Function used for thread notification (SIGEV_THREAD)
    sigev_notify_attributes:  Pointer;                              // Attributes for notification thread (SIGEV_THREAD)
  end;
  psigevent = ^sigevent;

  itimerspec = record
    it_interval:  timespec; // Timer interval
    it_value:     timespec; // Initial expiration
  end;
  pitimerspec = ^itimerspec;

Function timer_create(clockid: clockid_t; sevp: psigevent; timerid: ptimer_t): cint; cdecl; external;
Function timer_delete(timerid: timer_t): cint; cdecl; external;

Function timer_settime(timerid: timer_t; flags: cint; new_value,old_value: pitimerspec): cint; cdecl; external;

//------------------------------------------------------------------------------

Function errno_ptr: pcint; cdecl; external name '__errno_location';

Function __libc_current_sigrtmin: cint; cdecl; external;
Function __libc_current_sigrtmax: cint; cdecl; external;

{-------------------------------------------------------------------------------
    TSimpleTimer - signal handler
-------------------------------------------------------------------------------}

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure SignalHandler(signo: cint; siginfo: psiginfo; context: psigcontext); cdecl;
begin
If (siginfo^.si_code = SI_TIMER) and Assigned(siginfo^._sifields._rt._sigval) then
  (TObject(siginfo^._sifields._rt._sigval) as TSimpleTimer).TimerExpired;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

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
  SignalNumber:     cint;
  SignalAction:     sigactionrec;
  OldSignalAction:  sigactionrec;
  i:                cint;
  SignalEvent:      sigevent;
  NewTimerID:       timer_t;
begin
fTimerExpired := False;
fInMainThread := MainThreadID = GetCurrentThreadID;
// setup signal handler
FillChar(Addr(SignalAction)^,SizeOf(sigactionrec),0);
SignalAction.sa_handler := SignalHandler;
SignalAction.sa_flags := SA_SIGINFO;
If fpsigemptyset(SignalAction.sa_mask) <> 0 then
  raise ESTSignalSetupError.CreateFmt('TSimpleTimer.Initialize: Emptying signal set failed (%d).',[errno]);
// get free signal (or one already used for this library)
SignalNumber := Pred(__libc_current_sigrtmin);
// following is not particularly thread-safe, but meh...
For i := __libc_current_sigrtmin to __libc_current_sigrtmax do
  begin
    If fpsigaction(i,@SignalAction,@OldSignalAction) = 0 then
      begin
        If not Assigned(OldSignalAction.sa_handler) or (@OldSignalAction.sa_handler = @SignalHandler) then
          begin
            SignalNumber := i;
            Break{For i};
          end
        else
          begin
            // restore original signal handler
            If fpsigaction(i,@OldSignalAction,nil) <> 0 then
              raise ESTSignalSetupError.CreateFmt('TSimpleTimer.Initialize: Failed to restore signal action #%d (%d).',[i,errno]);
          end;
      end
    else raise ESTSignalSetupError.CreateFmt('TSimpleTimer.Initialize: Failed to setup signal action #%d (%d).',[i,errno]);
  end;
If SignalNumber < __libc_current_sigrtmin then
  raise ESTSignalSetupError.Create('TSimpleTimer.Initialize: No unused signal found.');
// setup and create timer
FillChar(Addr(SignalEvent)^,SizeOf(sigevent),0);
SignalEvent.sigev_value.sigval_ptr := Pointer(Self);
SignalEvent.sigev_signo := SignalNumber;
SignalEvent.sigev_notify := SIGEV_SIGNAL;
If timer_create(CLOCK_MONOTONIC,@SignalEvent,@NewTimerID) = 0 then
  fTimerID:= PtrUInt(NewTimerID)
else
  raise ESTTimerCreationError.CreateFmt('TSimpleTimer.Initialize: Failed to create timer (%d).',[errno_ptr^]);
{$ENDIF}
fInterval := 1000;
fEnabled := False;
end;

//------------------------------------------------------------------------------

procedure TSimpleTimer.Finalize;
begin
fEnabled := False;
SetupTimer;
{$IFDEF Windows}
If fOwnsWindow then
  fWindow.Free
else
  fWindow.OnMessage.Remove(MessagesHandler);
{$ELSE}
If timer_delete(timer_t(fTimerID)) <> 0 then
  raise ESTTimerDeletionError.CreateFmt('Finalize.Initialize: Failed to delete timer (%d).',[errno_ptr^]);
{
  Note that signal handler stays assigned, but it should pose no problem as
  the timer is destroyed and will not invoke the signal handler anymore.
}
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TSimpleTimer.SetupTimer;
{$IFDEF Linux}
var
  TimerTime:  itimerspec;
{$ENDIF}
begin
{$IFDEF Windows}
KillTimer(fWindow.WindowHandle,fTimerID);
If (fInterval > 0) and fEnabled then
  If SetTimer(fWindow.WindowHandle,fTimerID,fInterval,nil) = 0 then
    raise ESTTimerSetupError.CreateFmt('TSimpleTimer.SetupTimer: Failed to setup timer (0x%.8x).',[GetLastError]);
{$ELSE}
// disarm timer
FillChar(Addr(TimerTime)^,SizeOf(itimerspec),0);
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
fTimerExpired := True;
end;

{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF Linux}
{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSimpleTimer.OnAppIdleHandler(Sender: TObject; var Done: Boolean);
begin
ProcessMassages;
Done := True;
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

procedure TSimpleTimer.ProcessMassages;
begin
{$IFDEF Windows}
fWindow.ContinuousProcessMessages(False);
{$ELSE}
If fTimerExpired then
  begin
    DoOnTimer;
    fTimerExpired := False;
  end;
{$ENDIF}
end;

end.
