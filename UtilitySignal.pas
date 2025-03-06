{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  UtilitySignal

    Small library designed to ease setup of real-time signal handlers in Linux.
    It was designed primarily for use with posix timers (and possibly message
    queues), but can be, of course, used for other purposes too.

    I have decided to write it for two main reasons - one is to provide some
    siplified interface allowing for multiple handlers of single signal, the
    second is to limit number of used signals, of which count is very limited
    (30 or 31 per process in Linux), by allowing multiple users to use one
    signal allocated here.

    It was designed to be close in use to UtilityWindow library - so, to use
    it, create an instance of TUitilitySignal and assign events or callbacks to
    its OnSignal multi-event object. Also, to properly process the incoming
    signals (see implementation notes further) and pass them to assigned
    events/callbacks, you need to repeatedly call method ProcessSignal or
    ProcessSignals. Next, when setting-up the signal-producing system (eg. the
    timer), pass the instance as pointer signal value.

    Few words on implementation...

      At unit initialization, this library selects and allocates one unused
      real-time signal and then installs an action routine that receives all
      incoming invocations of that signal (more signals can be allocated later
      manually by calling function AllocateSignal).

        Note that this signal can be different every time the process is run.
        It can also differ between processes even if they are started from the
        same executable. Which means, among others, that this library cannot be
        used for interprocess communication, be aware of that!

        If this library is used multiple times within the same process (eg.
        when loaded with a dynamic library), this signal will be different for
        each instance. Because the number of available signals is limited, you
        should refrain from using this unit in a library or make sure one
        instance is shared across the entire process (note that current
        implementation does share the allocated signals across modules unless
        you disable symbol ModuleShared).

      Assigned action routine, when called by the system, stores the incoming
      signal into a buffer and immediately exits - the signal is not propagated
      directly to handlers because that way the async signal safety cannot be
      guaranteed (see Linux manual, signal-safety(7)).

      Buffer of incoming signals has large but invariant size (it cannot be
      enlarged), therefore there might arise situation where it becomes full -
      in this case oldest stored signals are dropped to make space for new
      ones. If symbol FailOnSignalDrop is defined, then this will later (at
      internal signal dispatching) produce an exception, otherwise it is silent.

      To pass stored signals from these buffers to desired handlers (events,
      callbacks), you need to call routines processing signals (for example
      TUitilitySignal.ProcessSignals or function ProcessOrphanSignals).

    Make sure you understand how signals work before using this library, so
    reading the linux manual (signal(7)) is strongly recommended.

  Version 2.2 (2025-03-04)

  Last change 2025-03-04

  ©2024-2025 František Milt

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
    AuxClasses        - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions     - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes          - github.com/TheLazyTomcat/Lib.AuxTypes
    InterlockedOps    - github.com/TheLazyTomcat/Lib.InterlockedOps
    MulticastEvent    - github.com/TheLazyTomcat/Lib.MulticastEvent
  * ProcessGlobalVars - github.com/TheLazyTomcat/Lib.ProcessGlobalVars
    SequentialVectors - github.com/TheLazyTomcat/Lib.SequentialVectors

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol UtilitySignal_UseAuxExceptions for details).

  Library ProcessGlobalVars is required only when symbol ModuleShared is
  defined.

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    Adler32             - github.com/TheLazyTomcat/Lib.Adler32
    AuxMath             - github.com/TheLazyTomcat/Lib.AuxMath
    BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    HashBase            - github.com/TheLazyTomcat/Lib.HashBase
    SimpleCPUID         - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream  - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils         - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo         - github.com/TheLazyTomcat/Lib.WinFileInfo

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
  {$MODESWITCH DuplicateLocals+}
  {$MODESWITCH ClassicProcVars+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------
{
  LargeBuffers
  HugeBuffers
  MassiveBuffers

  These three symbols control size of buffers and queues used internally by
  this library. Larger buffers/queues can prevent signal loss/drop, but
  they significantly increase memory consumption and may also degrade
  performance.

  LargeBuffer muptiplies default buffer size / queue length by 16, HugeBuffers
  by 128 and MassiveBuffers multiplies buffer sizes by 1024 and sets queues
  to unlimited length (technical limits are still in effect of course).

  If more than one of these three symbols are defined, then only the "largest"
  is observed.

  None is defined by default.

  To enable/define these symbols in a project without changing this library,
  define project-wide symbol UtilitySignal_LargeBuffers_On,
  UtilitySignal_HugeBuffers_On or UtilitySignal_MassiveBuffers_On.
}
{$UNDEF LargeBuffers}
{$IFDEF UtilitySignal_LargeBuffers_On}
  {$DEFINE LargeBuffers}
{$ENDIF}
{$UNDEF HugeBuffers}
{$IFDEF UtilitySignal_HugeBuffers_On}
  {$DEFINE HugeBuffers}
{$ENDIF}
{$UNDEF MassiveBuffers}
{$IFDEF UtilitySignal_MassiveBuffers_On}
  {$DEFINE MassiveBuffers}
{$ENDIF}

{
  FailOnSignalDrop

  When this symbol is defined and signal is dropped/lost due to full buffer or
  limited-length queue, an exception of class EUSSignalLost is raised. When not
  defined, nothing happens in that situation as signals are dropped silently.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol UtilitySignal_FailOnSignalDrop_On.
}
{$UNDEF FailOnSignalDrop}
{$IFDEF UtilitySignal_FailOnSignalDrop_On}
  {$DEFINE FailOnSignalDrop}
{$ENDIF}

{
  ModuleShared

  When defined, then this library will share global state with instances of
  itself in other modules loaded within current process (note that it will
  cooperate only with instances that were also compiled with this symbol).
  If used correctly, this can prevent unnecessary allocation of multiple
  signals for the same purpose in separate modules.

  If not defined, then this library will operate only within a single module
  into which it is compiled.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol UtilitySignal_ModuleShared_Off.
}
{$DEFINE ModuleShared}
{$IFDEF UtilitySignal_ModuleShared_Off}
  {$UNDEF ModuleShared}
{$ENDIF}

interface

uses
  SysUtils, BaseUnix, SyncObjs,
  SequentialVectors, AuxClasses, MulticastEvent
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EUSException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EUSSignalSetupError = class(EUSException);
  EUSSignalSetError   = class(EUSException);
  EUSSignalLost       = class(EUSException);

  EUSInvalidValue = class(EUSException);

  EUSGlobalStateOpenError    = class(EUSException);
  EUSGlobalStateCloseError   = class(EUSException);
  EUSGlobalStateMutexError   = class(EUSException);
  EUSGlobalStateModListError = class(EUSException);
  EUSGlobalStateCallError    = class(EUSException);

{===============================================================================
    Common public types
===============================================================================}
type
  TUSSignalArray = array of cint;

  TUSSignalValue = record
    case Integer of
      0: (IntValue: Integer);
      1: (PtrValue: Pointer);
  end;

  TUSSignalInfo = record
    Signal: Integer;
    Code:   Integer;
    Value:  TUSSignalValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                Utility functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Utility functions - declaration
===============================================================================}
{
  IsMasterModule

  When symbol ModuleShared is defined, then this function indicates whether
  module into which it is compiled is a master module - that is, it has signal
  action handler installed, is responsible for incoming signals processing and
  also for allocation of new signals.

  If ModuleShared is not defined, then this function always returns True.

    NOTE - slave (non-master) module can become master if current master module
           is unloaded and selects this module as new master. Therefore, do not
           assume that whatever this function returns is invariant.
}
Function IsMasterModule: Boolean;

{
  AllocatedSignals

  Returns an array of all signal IDs (numbers) that were allocated for use
  by this library.

    NOTE - the signals are in the order they were allocated and the returned
           array is never empty - it always contains at least one signal that
           was automatically allocated at library initialization.
}
Function AllocatedSignals: TUSSignalArray;

{
  GetCurrentProcessID

  Returns ID of the calling process. This can be used when sending a signal
  (see functions SendSignalTo further down).

    NOTE - this is sytem ID, not posix threads ID.
}
Function GetCurrentProcessID: pid_t;

//------------------------------------------------------------------------------
{
  SendSignalTo

  Sends selected signal to a given process with given value.

  When the sending succeeds, true is returned and output parameter Error is set
  to 0. When it fails, false is returned and Error contains Linux error code
  that describes reason of failure.

  Note that sending signals is subject to privilege checks, so it might not be
  possible, depending on what privileges the sending process have.

  The signal will arrive with code set to SI_QUEUE.

    WARNING - signals are quite deep subject, so do not use provided functions
              without considering what are you about to do. Always read your
              operating system's manual.
}
Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: TUSSignalValue; out Error: Integer): Boolean; overload;
Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: Integer; out Error: Integer): Boolean; overload;
Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: Pointer; out Error: Integer): Boolean; overload;

Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: TUSSignalValue): Boolean; overload;
Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: Integer): Boolean; overload;
Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: Pointer): Boolean; overload;

{
  SendSignalAs

  Works the same as SendSignalTo (see there for details), but it is sending
  signal back to the calling process (but not necessarily the calling thread!).
}
Function SendSignalAs(Signal: Integer; Value: TUSSignalValue; out Error: Integer): Boolean; overload;
Function SendSignalAs(Signal: Integer; Value: Integer; out Error: Integer): Boolean; overload;
Function SendSignalAs(Signal: Integer; Value: Pointer; out Error: Integer): Boolean; overload;

Function SendSignalAs(Signal: Integer; Value: TUSSignalValue): Boolean; overload;
Function SendSignalAs(Signal: Integer; Value: Integer): Boolean; overload;
Function SendSignalAs(Signal: Integer; Value: Pointer): Boolean; overload;

{
  SendSignal

  Same as SendSignalAs (see there for details), but the signal is sent using
  the first signal number allocated for this library.
}
Function SendSignal(Value: TUSSignalValue; out Error: Integer): Boolean; overload;
Function SendSignal(Value: Integer; out Error: Integer): Boolean; overload;
Function SendSignal(Value: Pointer; out Error: Integer): Boolean; overload;

Function SendSignal(Value: TUSSignalValue): Boolean; overload;
Function SendSignal(Value: Integer): Boolean; overload;
Function SendSignal(Value: Pointer): Boolean; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                 TUSSignalQueue
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUSSignalQueue - class declaration
===============================================================================}
// only for internal use
type
  TUSSignalQueue = class(TSequentialVector)
  protected
    Function GetItem(Index: Integer): TUSSignalInfo; reintroduce;
    procedure SetItem(Index: Integer; NewValue: TUSSignalInfo); reintroduce;
  {$IFDEF FailOnSignalDrop}
    procedure ItemDrop(Item: Pointer); override;
  {$ENDIF}
    procedure ItemAssign(SrcItem,DstItem: Pointer); override;
    Function ItemCompare(Item1,Item2: Pointer): Integer; override;
    Function ItemEquals(Item1,Item2: Pointer): Boolean; override;
  public
    constructor Create(MaxCount: Integer = -1);
    Function IndexOf(Item: TUSSignalInfo): Integer; reintroduce;
    Function Find(Item: TUSSignalInfo; out Index: Integer): Boolean; reintroduce;
    procedure Push(Item: TUSSignalInfo); reintroduce;
    Function Peek: TUSSignalInfo; reintroduce;
    Function Pop: TUSSignalInfo; reintroduce;
    Function Pick(Index: Integer): TUSSignalInfo; reintroduce;
    property Items[Index: Integer]: TUSSignalInfo read GetItem write SetItem;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                             TUSMulticastSignalEvent
--------------------------------------------------------------------------------
===============================================================================}
type
  TUSSignalCallback = procedure(Sender: TObject; Data: TUSSignalInfo; var BreakProcessing: Boolean);
  TUSSignalEvent = procedure(Sender: TObject; Data: TUSSignalInfo; var BreakProcessing: Boolean) of object;

{===============================================================================
    TUSMulticastSignalEvent - class declaration
===============================================================================}
// only for internal use
type
  TUSMulticastSignalEvent = class(TMulticastEvent)
  public
    Function IndexOf(const Handler: TUSSignalCallback): Integer; reintroduce; overload;
    Function IndexOf(const Handler: TUSSignalEvent): Integer; reintroduce; overload;
    Function Find(const Handler: TUSSignalCallback; out Index: Integer): Boolean; reintroduce; overload;
    Function Find(const Handler: TUSSignalEvent; out Index: Integer): Boolean; reintroduce; overload;
    Function Add(Handler: TUSSignalCallback): Integer; reintroduce; overload;
    Function Add(Handler: TUSSignalEvent): Integer; reintroduce; overload;
    Function Remove(const Handler: TUSSignalCallback): Integer; reintroduce; overload;
    Function Remove(const Handler: TUSSignalEvent): Integer; reintroduce; overload;
    procedure Call(Sender: TObject; const SignalInfo: TUSSignalInfo); reintroduce;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TUtilitySignal
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUtilitySignal - class declaration
===============================================================================}
{
  TUtilitySignal

  Instances of this class are a primary mean of receiving signals.

    WARNING - public interface of this class is not fully thread safe. With
              one exception (see property ObserveThread), you should create,
              destroy and use its methods and properties only within a single
              thread.

  Internal queue storing incoming signals before these are passed to assigned
  events/callbacks has limited size (unless symbol MassiveBuffers is defined).
  This means there is a possibility that some signals may be lost when this
  queue becomes full.
  This might produce an exception if symbol FailOnSignalDrop is defined, or
  it might go completely silent when it is not defined. Anyway, you should be
  aware of this limitation.
}
type
  TUtilitySignal = class(TCustomObject)
  protected
    fRegisteredOnIdle:  Boolean;
    fThreadLock:        TCriticalSection;
    fReceivedSignals:   TUSSignalQueue;
    fCreatorThread:     pthread_t;
    fObserveThread:     Boolean;
    fCoalesceSignals:   Boolean;
    fOnSignal:          TUSMulticastSignalEvent;
    Function GetCoalesceSignals: Boolean; virtual;
    procedure SetCoalesceSignals(Value: Boolean); virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure ThreadLock; virtual;
    procedure ThreadUnlock; virtual;
    procedure AddSignal(SignalInfo: TUSSignalInfo); virtual;
    Function CheckThread: Boolean; virtual;
    procedure OnAppIdleHandler(Sender: TObject; var Done: Boolean); virtual;
  public
  {
    Create

    When argument CanRegisterForOnIdle is set to true, then method
    RegisterForOnIdle is called within the contructor, otherwise it is not
    (see the mentioned method for explanation of what it does).
  }
    constructor Create(CanRegisterForOnIdle: Boolean = True);
    destructor Destroy; override;
  {
    RegisterForOnIdle

    When this library is compiled in program with LCL, both constructor and
    this method are executed in the context of main thread, then a handler
    calling method ProcessSignals is assigned to application's OnIdle event
    and this method returns true. Otherwise it returns false and no hadler
    is assigned.
    When the handler is assigned, you do not need to explicitly call method
    ProcessSignal(s) for current object to work properly.
  }
    Function RegisterForOnIdle: Boolean; virtual;
  {
    UnregisterFromOnIdle

    Tries to unregister handler from application's OnIdle event (only in
    programs compiled with LCL).
  }
    procedure UnregisterFromOnIdle; virtual;
  {
    ProcessSignal
    ProcessSignals

    Processes all incoming signals (copies them from pending signals buffer to
    their respective instances of TUtilitySignal) and then passes all signals
    meant for this instance to events/callbacks assigned to OnSignal.

    Signals are processed in the order they have arrived.

    ProcessSignal passes only one signal to OnSignal, whereas ProcessSignals
    passes all queued signals.

      WARNING - the entire class is NOT externally thread safe. Although it is
                possible to call ProcessSignal(s) from a different thread than
                the one that created the object (when ObserveThread is set to
                false), you cannot safely call public methods from multiple
                concurrent threads.

      NOTE - these two functions can be called from OnSignal handlers, but it
             is strongly discouraged.
  }
    procedure ProcessSignal; virtual;
    procedure ProcessSignals; virtual;
  {
    RegisteredForOnIdle

    True when handler calling ProcessSignals is assigned to appplication's
    OnIdle event, false otherwise.

    See RegisterForOnIdle and UnregisterForOnIdle for more details.
  }
    property RegisteredForOnIdle: Boolean read fRegisteredOnIdle;
  {
    CreatorThread

    ID of thread that created this instance.

      NOTE - this is pthreads ID, not system ID!
  }
    property CreatorThread: pthread_t read fCreatorThread;
  {
    ObserveThread

    If ObserveThread is True, then thread calling ProcessSignal(s) must
    be the same as is indicated by CreatorThread (ie. thread that created
    current instance), otherwise these methods fetch new pending messages but
    then exit without invoking any event or callback.
    When false, any thread can call mentioned methods - though this is not
    recommended.

      NOTE - default value is True!
  }
    property ObserveThread: Boolean read fObserveThread write fObserveThread;
  {
    CoalesceSignals

    If this is set to false (default), then each signal is processed separately
    (one signal, one call to assigned events/callbacks).

    When set to true, then signals with equal code and signal number (value
    is ignored because it will always contain reference to the current object)
    are combined into a single signal, and then, irrespective of their number,
    only one invocation of events is called.
  }
    property CoalesceSignals: Boolean read GetCoalesceSignals write SetCoalesceSignals;
    property OnSignal: TUSMulticastSignalEvent read fOnSignal;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 Orphan signals
--------------------------------------------------------------------------------
===============================================================================}
{
  Orphan signals are those signals that could not be delivered to any
  TUtilitySignal instance.

  The signals are internally buffered, to pass them to registered events/
  callbacks, call ProcessOrphanSignal(s).

  This internal buffer has limited size (unless symbol MassiveBuffers is
  defined) - if it becomes full, the oldest undelivered signals are dropped
  to make room for new ones. Note that dropping of old signal will raise an
  exception if symbol FailOnSignalDrop is defined, otherwise it is silent.

    NOTE - provided funtions are serialized and can be called from any thread.
}

procedure RegisterForOrphanSignals(Callback: TUSSignalCallback);
procedure RegisterForOrphanSignals(Event: TUSSignalEvent);

procedure UnregisterFromOrphanSignals(Callback: TUSSignalCallback);
procedure UnregisterFromOrphanSignals(Event: TUSSignalEvent);

//------------------------------------------------------------------------------

procedure ProcessOrphanSignal;
procedure ProcessOrphanSignals;

{===============================================================================
--------------------------------------------------------------------------------
                              New signal allocation
--------------------------------------------------------------------------------
===============================================================================}
{
  AllocateSignal

  Allocates (reserves) new posix real-time signal for use within this library.

    NOTE - one signal is always allocated at the library initialization - use
           that signal if possible.

    WARNING - number of signals available for allocation is very limited (in
              Linux, there is usually about 30 unused signals for each proces,
              but this number can be as low as 6 (8 minus two for NTPL)), so
              use this function sparingly and only when really necessary.

  Please consult Linux manual (signal(7)) for more information about signals.
}
Function AllocateSignal: cint;

implementation

uses
  UnixType, Classes, {$IFDEF LCL}Forms,{$ENDIF}
  AuxTypes, InterlockedOps{$IFDEF ModuleShared}, ProcessGlobalVars{$ENDIF};

{$LINKLIB C}
{$LINKLIB PTHREAD}

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                Library internals
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Library internals - system stuff
===============================================================================}

Function getpid: pid_t; cdecl; external;

Function sched_yield: cint; cdecl; external;

Function errno_ptr: pcint; cdecl; external name '__errno_location';

//==============================================================================
type
  sighandlerfce_t = procedure(signo: cint); cdecl;
  sigactionfce_t =  procedure(signo: cint; siginfo: psiginfo; context: Pointer); cdecl;

  sigset_t = array[0..Pred(1024 div (8 * SizeOf(culong)))] of culong; // 1024bits, 128bytes
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

Function allocate_rtsig(high: cint): cint; cdecl; external name '__libc_allocate_rtsig';

Function sigemptyset(_set: psigset_t): cint; cdecl; external;
Function sigaddset(_set: psigset_t; signum: cint): cint; cdecl; external;
Function sigdelset(_set: psigset_t; signum: cint): cint; cdecl; external;

Function sigqueue(pid: pid_t; sig: cint; value: sigval_t): cint; cdecl; external;
Function sigaction(signum: cint; act: psigaction_t; oact: psigaction_t): cint; cdecl; external;

Function pthread_sigmask(how: cint; newset,oldset: psigset_t): cint; cdecl; external;

Function pthread_self: pthread_t; cdecl; external;
Function pthread_equal(t1: pthread_t; t2: pthread_t): cint; cdecl; external;

//------------------------------------------------------------------------------
{$IFDEF ModuleShared}
const
  PTHREAD_MUTEX_RECURSIVE = 1;
  PTHREAD_MUTEX_ROBUST    = 1;

type
  pthread_mutexattr_p = ^pthread_mutexattr_t;
  pthread_mutex_p = ^pthread_mutex_t;

//------------------------------------------------------------------------------

Function pthread_mutexattr_init(attr: pthread_mutexattr_p): cint; cdecl; external;
Function pthread_mutexattr_destroy(attr: pthread_mutexattr_p): cint; cdecl; external;
Function pthread_mutexattr_settype(attr: pthread_mutexattr_p; _type: cint): cint; cdecl; external;
Function pthread_mutexattr_setrobust(attr: pthread_mutexattr_p; robustness: cint): cint; cdecl; external;

Function pthread_mutex_init(mutex: pthread_mutex_p; attr: pthread_mutexattr_p): cint; cdecl; external;
Function pthread_mutex_destroy(mutex: pthread_mutex_p): cint; cdecl; external;

Function pthread_mutex_trylock(mutex: pthread_mutex_p): cint; cdecl; external;
Function pthread_mutex_lock(mutex: pthread_mutex_p): cint; cdecl; external;
Function pthread_mutex_unlock(mutex: pthread_mutex_p): cint; cdecl; external;

//------------------------------------------------------------------------------
{$ENDIF}
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

{===============================================================================
    Library internals - implementation constants, types and variables
===============================================================================}
const
{$IF Defined(MassiveBuffers)}
  US_SZCOEF_BUFFER = 1024;
  US_SZCOEF_QUEUE  = -1;  // unlimited
{$ELSEIF Defined(HugeBuffers)}
  US_SZCOEF_BUFFER = 128;
  US_SZCOEF_QUEUE  = 128;
{$ELSEIF Defined(LargeBuffers)}
  US_SZCOEF_BUFFER = 16;
  US_SZCOEF_QUEUE  = 16;
{$ELSE}
  US_SZCOEF_BUFFER = 1;
  US_SZCOEF_QUEUE  = 1;
{$IFEND}

//------------------------------------------------------------------------------
type
  TUSPendingSignalsBuffer = record
    Head:     record
      Count:      Integer;
      First:      Integer;
      DropCount:  Integer;
      DispCount:  Integer;  // number of already dispatched items (for better performance)
    end;
    Signals:  array[0..Pred(US_SZCOEF_BUFFER * 1024)] of
      record
        Signal:     cint;
        Code:       cint;
        Value:      sigval_t;
        Internals:  record
          Dispatched: Boolean;
          // some other fields may be added here later
        end;
      end;
  end;
  PUSPendingSignalsBuffer = ^TUSPendingSignalsBuffer;

//------------------------------------------------------------------------------
const
{
  Maximum number of signals this implementation can allocate. System/resource
  limitations are usually reached before this limit.

  If this value is changed in the future, then also change global state
  compatibility version (US_GLOBSTATE_VERSION).
}
  US_SIGALLOC_MAX = 64;

type
  TUSAllocatedSignals = record
    Signals:  array[0..Pred(US_SIGALLOC_MAX)] of cint;
    Count:    Integer;
  end;

var
  // main global variable
  GVAR_ModuleState: record
    ProcessingLock:   TCriticalSection;
    AllocatedSignals: TUSAllocatedSignals;
    SignalMask:       sigset_t; // contains all allocated signals
    Dispatcher:       TObject;  // TUSSignalDispatcher
    PendingSignals:   record
      Lock:             Integer;
      PrimaryBuffer:    PUSPendingSignalsBuffer;
      SecondaryBuffer:  PUSPendingSignalsBuffer;
    end;
  {$IFDEF ModuleShared}
    GlobalInfo:       record
      IsMaster:         Boolean;
      HeadVariable:     TPGVVariable;
    end;
  {$ENDIF}
  end;

//------------------------------------------------------------------------------
const
  // values for GVAR_ModuleState.PendingSignals.Lock
  US_PENDSIGSLOCK_UNLOCKED = 0;
  US_PENDSIGSLOCK_LOCKED   = 1;


{===============================================================================
--------------------------------------------------------------------------------
                                Utility functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Utility functions - implementation
===============================================================================}

Function IsMasterModule: Boolean;
begin
{$IFDEF ModuleShared}
GVAR_ModuleState.ProcessingLock.Enter;
try
  Result := GVAR_ModuleState.GlobalInfo.IsMaster;
finally
  GVAR_ModuleState.ProcessingLock.Leave;
end;
{$ELSE}
Result := True;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function AllocatedSignals: TUSSignalArray;
var
  i:  Integer;
begin
GVAR_ModuleState.ProcessingLock.Enter;
try
  Result := nil;
  SetLength(Result,GVAR_ModuleState.AllocatedSignals.Count);
  For i := 0 to Pred(GVAR_ModuleState.AllocatedSignals.Count) do
    Result[i] := GVAR_ModuleState.AllocatedSignals.Signals[i];
finally
  GVAR_ModuleState.ProcessingLock.Leave;
end;
end;

//------------------------------------------------------------------------------

Function GetCurrentProcessID: pid_t;
begin
Result := getpid;
end;

//==============================================================================

Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: TUSSignalValue; out Error: Integer): Boolean;
begin
Result := SendSignalTo(ProcessID,Signal,Value.PtrValue,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: Integer; out Error: Integer): Boolean;
var
  Temp: TUSSignalValue;
begin
FillChar(Addr(Temp)^,SizeOf(Temp),0);
Temp.IntValue := Value;
Result := SendSignalTo(ProcessID,Signal,Temp.PtrValue,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: Pointer; out Error: Integer): Boolean;
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

Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: TUSSignalValue): Boolean;
var
  Error: Integer;
begin
Result := SendSignalTo(ProcessID,Signal,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: Integer): Boolean;
var
  Error: Integer;
begin
Result := SendSignalTo(ProcessID,Signal,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignalTo(ProcessID: pid_t; Signal: Integer; Value: Pointer): Boolean;
var
  Error: Integer;
begin
Result := SendSignalTo(ProcessID,Signal,Value,Error);
end;

//==============================================================================

Function SendSignalAs(Signal: Integer; Value: TUSSignalValue; out Error: Integer): Boolean;
begin
Result := SendSignalTo(getpid,Signal,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignalAs(Signal: Integer; Value: Integer; out Error: Integer): Boolean;
begin
Result := SendSignalTo(getpid,Signal,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignalAs(Signal: Integer; Value: Pointer; out Error: Integer): Boolean;
begin
Result := SendSignalTo(getpid,Signal,Value,Error);
end;

//------------------------------------------------------------------------------

Function SendSignalAs(Signal: Integer; Value: TUSSignalValue): Boolean;
var
  Error: Integer;
begin
Result := SendSignalTo(getpid,Signal,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignalAs(Signal: Integer; Value: Integer): Boolean;
var
  Error: Integer;
begin
Result := SendSignalTo(getpid,Signal,Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignalAs(Signal: Integer; Value: Pointer): Boolean;
var
  Error: Integer;
begin
Result := SendSignalTo(getpid,Signal,Value,Error);
end;

//==============================================================================

Function SendSignal(Value: TUSSignalValue; out Error: Integer): Boolean;
begin
Result := SendSignalTo(getpid,Integer(AllocatedSignals[0]),Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(Value: Integer; out Error: Integer): Boolean;
begin
Result := SendSignalTo(getpid,Integer(AllocatedSignals[0]),Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(Value: Pointer; out Error: Integer): Boolean;
begin
Result := SendSignalTo(getpid,Integer(AllocatedSignals[0]),Value,Error);
end;

//------------------------------------------------------------------------------

Function SendSignal(Value: TUSSignalValue): Boolean;
var
  Error: Integer;
begin
Result := SendSignalTo(getpid,Integer(AllocatedSignals[0]),Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(Value: Integer): Boolean;
var
  Error: Integer;
begin
Result := SendSignalTo(getpid,Integer(AllocatedSignals[0]),Value,Error);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SendSignal(Value: Pointer): Boolean;
var
  Error: Integer;
begin
Result := SendSignalTo(getpid,Integer(AllocatedSignals[0]),Value,Error);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TUSSignalQueue
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUSSignalQueue - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUSSignalQueue - protected methods
-------------------------------------------------------------------------------}

Function TUSSignalQueue.GetItem(Index: Integer): TUSSignalInfo;
begin
inherited GetItem(Index,@Result);
end;

//------------------------------------------------------------------------------

procedure TUSSignalQueue.SetItem(Index: Integer; NewValue: TUSSignalInfo);
begin
inherited SetItem(Index,@NewValue);
end;

//------------------------------------------------------------------------------
{$IFDEF FailOnSignalDrop}
procedure TUSSignalQueue.ItemDrop(Item: Pointer);
begin
raise EUSSignalLost.CreateFmt('TUSSignalQueue.ItemDrop: Dropping queued signal (%d, %d, %p).',
  [TUSSignalInfo(Item^).Signal,TUSSignalInfo(Item^).Code,TUSSignalInfo(Item^).Value.PtrValue]);
end;

//------------------------------------------------------------------------------
{$ENDIF}

procedure TUSSignalQueue.ItemAssign(SrcItem,DstItem: Pointer);
begin
TUSSignalInfo(DstItem^) := TUSSignalInfo(SrcItem^);
end;

//------------------------------------------------------------------------------

Function TUSSignalQueue.ItemCompare(Item1,Item2: Pointer): Integer;
begin
If TUSSignalInfo(Item1^).Signal > TUSSignalInfo(Item2^).Signal then
  Result := +1
else If TUSSignalInfo(Item1^).Signal < TUSSignalInfo(Item2^).Signal then
  Result := -1
else
  begin
    If TUSSignalInfo(Item1^).Code > TUSSignalInfo(Item2^).Code then
      Result := +1
    else If TUSSignalInfo(Item1^).Code < TUSSignalInfo(Item2^).Code then
      Result := -1
    else
      begin
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        If PtrUInt(TUSSignalInfo(Item1^).Value.PtrValue) > PtrUInt(TUSSignalInfo(Item2^).Value.PtrValue) then
          Result := +1
        else If PtrUInt(TUSSignalInfo(Item1^).Value.PtrValue) < PtrUInt(TUSSignalInfo(Item2^).Value.PtrValue) then
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
          Result := -1
        else
          Result := 0;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TUSSignalQueue.ItemEquals(Item1,Item2: Pointer): Boolean;
begin
Result := (TUSSignalInfo(Item1^).Signal = TUSSignalInfo(Item2^).Signal) and
          (TUSSignalInfo(Item1^).Code = TUSSignalInfo(Item2^).Code) and
          (TUSSignalInfo(Item1^).Value.PtrValue = TUSSignalInfo(Item2^).Value.PtrValue);
end;

{-------------------------------------------------------------------------------
    TUSSignalQueue - public methods
-------------------------------------------------------------------------------}

constructor TUSSignalQueue.Create(MaxCount: Integer = -1);
begin
inherited Create(omFIFO,SizeOf(TUSSignalInfo),MaxCount);
end;

//------------------------------------------------------------------------------

Function TUSSignalQueue.IndexOf(Item: TUSSignalInfo): Integer;
begin
Result := inherited IndexOf(@Item);
end;

//------------------------------------------------------------------------------

Function TUSSignalQueue.Find(Item: TUSSignalInfo; out Index: Integer): Boolean;
begin
Result := inherited Find(@Item,Index);
end;

//------------------------------------------------------------------------------

procedure TUSSignalQueue.Push(Item: TUSSignalInfo);
begin
inherited Push(@Item);
end;

//------------------------------------------------------------------------------

Function TUSSignalQueue.Peek: TUSSignalInfo;
begin
inherited Peek(@Result);
end;

//------------------------------------------------------------------------------

Function TUSSignalQueue.Pop: TUSSignalInfo;
begin
inherited Pop(@Result);
end;

//------------------------------------------------------------------------------

Function TUSSignalQueue.Pick(Index: Integer): TUSSignalInfo;
begin
inherited Pick(Index,@Result);
end;


{===============================================================================
--------------------------------------------------------------------------------
                             TUSMulticastSignalEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUSMulticastSignalEvent - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUSMulticastSignalEvent - public methods
-------------------------------------------------------------------------------}

Function TUSMulticastSignalEvent.IndexOf(const Handler: TUSSignalCallback): Integer;
begin
Result := inherited IndexOf(MulticastEvent.TCallback(Handler));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUSMulticastSignalEvent.IndexOf(const Handler: TUSSignalEvent): Integer;
begin
Result := inherited IndexOf(MulticastEvent.TEvent(Handler));
end;

//------------------------------------------------------------------------------

Function TUSMulticastSignalEvent.Find(const Handler: TUSSignalCallback; out Index: Integer): Boolean;
begin
Result := inherited Find(MulticastEvent.TCallback(Handler),Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUSMulticastSignalEvent.Find(const Handler: TUSSignalEvent; out Index: Integer): Boolean;
begin
Result := inherited Find(MulticastEvent.TEvent(Handler),Index);
end;

//------------------------------------------------------------------------------

Function TUSMulticastSignalEvent.Add(Handler: TUSSignalCallback): Integer;
begin
Result := inherited Add(MulticastEvent.TCallback(Handler),False);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUSMulticastSignalEvent.Add(Handler: TUSSignalEvent): Integer;
begin
Result := inherited Add(MulticastEvent.TEvent(Handler),False);
end;

//------------------------------------------------------------------------------

Function TUSMulticastSignalEvent.Remove(const Handler: TUSSignalCallback): Integer;
begin
Result := inherited Remove(MulticastEvent.TCallback(Handler),True);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUSMulticastSignalEvent.Remove(const Handler: TUSSignalEvent): Integer;
begin
Result := inherited Remove(MulticastEvent.TEvent(Handler),True);
end;

//------------------------------------------------------------------------------

procedure TUSMulticastSignalEvent.Call(Sender: TObject; const SignalInfo: TUSSignalInfo);
var
  i:          Integer;
  BreakProc:  Boolean;
begin
BreakProc := False;
For i := LowIndex to HighIndex do
  begin
    If fEntries[i].IsMethod then
      TUSSignalEvent(fEntries[i].HandlerMethod)(Sender,SignalInfo,BreakProc)
    else
      TUSSignalCallback(fEntries[i].HandlerProcedure)(Sender,SignalInfo,BreakProc);
    If BreakProc then
      Break{for i};
  end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TUSSignalDispatcher
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUSSignalDispatcher - class declaration
===============================================================================}
type
  TUSSignalDispatcher = class(TCustomListObject)
  protected
    fMainListLock:    TCriticalSection;
    fUtilitySignals:  array of TUtilitySignal;
    fCount:           Integer;
    fOrphansLock:     TCriticalSection;
    fOrphanSignals:   TUSSignalQueue;
    fOnOrphanSignal:  TUSMulticastSignalEvent;
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    Function UtilitySignalFind(UtilitySignal: TUtilitySignal; out Index: Integer): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function UtilitySignalRegister(UtilitySignal: TUtilitySignal): Integer; virtual;
    Function UtilitySignalUnregister(UtilitySignal: TUtilitySignal): Integer; virtual;
    procedure DispatchFrom(var Buffer: TUSPendingSignalsBuffer); virtual;
    procedure ProcessOrphans(var Buffer: TUSPendingSignalsBuffer); virtual;
    Function OrphanSignalsRegister(Callback: TUSSignalCallback): Integer; virtual;
    Function OrphanSignalsRegister(Event: TUSSignalEvent): Integer; virtual;
    Function OrphanSignalsUnregister(Callback: TUSSignalCallback): Integer; virtual;
    Function OrphanSignalsUnregister(Event: TUSSignalEvent): Integer; virtual;
    procedure ProcessOrphanSignal; virtual;
    procedure ProcessOrphanSignals; virtual;
  end;

{===============================================================================
    TUSSignalDispatcher - class implementation
===============================================================================}

Function StrIfThen(Condition: Boolean; const OnTrue,OnFalse: String): String;
begin
If Condition then
  Result := OnTrue
else
  Result := OnFalse;
end;

{-------------------------------------------------------------------------------
    TUSSignalDispatcher - protected methods
-------------------------------------------------------------------------------}

Function TUSSignalDispatcher.GetCapacity: Integer;
begin
Result := Length(fUtilitySignals);
end;

//------------------------------------------------------------------------------

procedure TUSSignalDispatcher.SetCapacity(Value: Integer);
begin
If Value < fCount then
  raise EUSInvalidValue.CreateFmt('TUSSignalDispatcher.SetCapacity: Invalid new capacity (%d).',[Value]);
If Value <> Length(fUtilitySignals) then
  SetLength(fUtilitySignals,Value);
end;

//------------------------------------------------------------------------------

Function TUSSignalDispatcher.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

procedure TUSSignalDispatcher.SetCount(Value: Integer);
begin
// just a no-op to consume the argument
If Value = fCount then
  fCount := Value;
end;

//------------------------------------------------------------------------------

procedure TUSSignalDispatcher.Initialize;
begin
fMainListLock := TCriticalSection.Create;
fUtilitySignals := nil;
fCount := 0;
fOrphansLock := TCriticalSection.Create;
fOrphanSignals := TUSSignalQueue.Create(US_SZCOEF_QUEUE * 1024);
fOnOrphanSignal := TUSMulticastSignalEvent.Create(Self);
end;

//------------------------------------------------------------------------------

procedure TUSSignalDispatcher.Finalize;
begin
FreeAndNil(fOnOrphanSignal);
FreeAndNil(fOrphanSignals);
FreeAndNil(fOrphansLock);
fCount := 0;
SetLength(fUtilitySignals,0);
FreeAndNil(fMainListLock);
end;

//------------------------------------------------------------------------------

Function TUSSignalDispatcher.UtilitySignalFind(UtilitySignal: TUtilitySignal; out Index: Integer): Boolean;
var
  i:  Integer;
begin
// thread lock must be in effect before calling this method
Result := False;
Index := -1;
For i := LowIndex to HighIndex do
  If fUtilitySignals[i] = UtilitySignal then
    begin
      Index := i;
      Result := True;
      Break{For i};
    end;
end;

{-------------------------------------------------------------------------------
    TUSSignalDispatcher - public methods
-------------------------------------------------------------------------------}

constructor TUSSignalDispatcher.Create;
begin
inherited;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TUSSignalDispatcher.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TUSSignalDispatcher.LowIndex: Integer;
begin
fMainListLock.Enter;
try
  Result := Low(fUtilitySignals);
finally
  fMainListLock.Leave;
end;
end;

//------------------------------------------------------------------------------

Function TUSSignalDispatcher.HighIndex: Integer;
begin
fMainListLock.Enter;
try
  Result := Pred(fCount);
finally
  fMainListLock.Leave;
end;
end;

//------------------------------------------------------------------------------

Function TUSSignalDispatcher.UtilitySignalRegister(UtilitySignal: TUtilitySignal): Integer;
begin
fMainListLock.Enter;
try
  If not UtilitySignalFind(UtilitySignal,Result) then
    begin
      Grow;
      Result := fCount;
      fUtilitySignals[Result] := UtilitySignal;
      Inc(fCount);
    end;
finally
  fMainListLock.Leave;
end;
end;

//------------------------------------------------------------------------------

Function TUSSignalDispatcher.UtilitySignalUnregister(UtilitySignal: TUtilitySignal): Integer;
var
  i:  Integer;
begin
fMainListLock.Enter;
try
  If UtilitySignalFind(UtilitySignal,Result) then
    begin
      For i := Result to Pred(HighIndex) do
        fUtilitySignals[i] := fUtilitySignals[i + 1];
      fUtilitySignals[HighIndex] := nil;
      Dec(fCount);
      Shrink;
    end;
finally
  fMainListLock.Leave;
end;
end;

//------------------------------------------------------------------------------

procedure TUSSignalDispatcher.DispatchFrom(var Buffer: TUSPendingSignalsBuffer);
var
  i,j,Index:    Integer;
  TempSigInfo:  TUSSignalInfo;
begin
{$IFDEF FailOnSignalDrop}
If Buffer.Head.DropCount > 0 then
  raise EUSSignalLost.CreateFmt('TUSSignalDispatcher.DispatchFrom: %d signal%s lost due to full buffer.',
    [Buffer.Head.DropCount,StrIfThen(Buffer.Head.DropCount <= 1,'','s')]);
{$ENDIF}
fMainListLock.Enter;
try
  For i := LowIndex to HighIndex do
    begin
      If Buffer.Head.DispCount >= Buffer.Head.Count then
        Break{for i};
      fUtilitySignals[i].ThreadLock;
      try
        For j := 0 to Pred(Buffer.Head.Count) do
          begin
            Index := (Buffer.Head.First + j) mod Length(Buffer.Signals);
            If not Buffer.Signals[Index].Internals.Dispatched and
              (TObject(Buffer.Signals[Index].Value.sigval_ptr) = fUtilitySignals[i]) then
              begin
                TempSigInfo.Signal := Buffer.Signals[Index].Signal;
                TempSigInfo.Code := Buffer.Signals[Index].Code;
                TempSigInfo.Value.PtrValue := Buffer.Signals[Index].Value.sigval_ptr;
                fUtilitySignals[i].AddSignal(TempSigInfo);
                Buffer.Signals[Index].Internals.Dispatched := True;
                Inc(Buffer.Head.DispCount);
              end;
          end;
      finally
        fUtilitySignals[i].ThreadUnlock;
      end;
    end;
finally
  fMainListLock.Leave;
end;
end;

//------------------------------------------------------------------------------

procedure TUSSignalDispatcher.ProcessOrphans(var Buffer: TUSPendingSignalsBuffer);
var
  i,Index:      Integer;
  TempSigInfo:  TUSSignalInfo;
begin
{$IFDEF FailOnSignalDrop}
If Buffer.Head.DropCount > 0 then
  raise EUSSignalLost.CreateFmt('TUSSignalDispatcher.ProcessOrphans: %d signal%s lost due to full buffer.',
    [Buffer.Head.DropCount,StrIfThen(Buffer.Head.DropCount <= 1,'','s')]);
{$ENDIF}
fOrphansLock.Enter;
try
  For i := 0 to Pred(Buffer.Head.Count) do
    begin
      Index := (Buffer.Head.First + i) mod Length(Buffer.Signals);
      If not Buffer.Signals[Index].Internals.Dispatched then
        begin
          TempSigInfo.Signal := Buffer.Signals[Index].Signal;
          TempSigInfo.Code := Buffer.Signals[Index].Code;
          TempSigInfo.Value.PtrValue := Buffer.Signals[Index].Value.sigval_ptr;
          fOrphanSignals.Push(TempSigInfo);
        end;
    end;
finally
  fOrphansLock.Leave;
end;
end;

//------------------------------------------------------------------------------

Function TUSSignalDispatcher.OrphanSignalsRegister(Callback: TUSSignalCallback): Integer;
begin
fOrphansLock.Enter;
try
  Result := fOnOrphanSignal.Add(Callback);
finally
  fOrphansLock.Leave;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUSSignalDispatcher.OrphanSignalsRegister(Event: TUSSignalEvent): Integer;
begin
fOrphansLock.Enter;
try
  Result := fOnOrphanSignal.Add(Event);
finally
  fOrphansLock.Leave;
end;
end;

//------------------------------------------------------------------------------

Function TUSSignalDispatcher.OrphanSignalsUnregister(Callback: TUSSignalCallback): Integer;
begin
fOrphansLock.Enter;
try
  Result := fOnOrphanSignal.Remove(Callback);
finally
  fOrphansLock.Leave;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUSSignalDispatcher.OrphanSignalsUnregister(Event: TUSSignalEvent): Integer;
begin
fOrphansLock.Enter;
try
  Result := fOnOrphanSignal.Remove(Event);
finally
  fOrphansLock.Leave;
end;
end;

//------------------------------------------------------------------------------

procedure TUSSignalDispatcher.ProcessOrphanSignal;
begin
fOrphansLock.Enter;
try
  If fOrphanSignals.Count > 0 then
    fOnOrphanSignal.Call(Self,fOrphanSignals.Pop);
finally
  fOrphansLock.Leave;
end;
end;

//------------------------------------------------------------------------------

procedure TUSSignalDispatcher.ProcessOrphanSignals;
begin
fOrphansLock.Enter;
try
  while fOrphanSignals.Count > 0 do
    fOnOrphanSignal.Call(Self,fOrphanSignals.Pop);
finally
  fOrphansLock.Leave;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                            Signal handling and setup
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Signal handling and setup - handler
===============================================================================}

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure SignalHandler(signo: cint; siginfo: psiginfo; context: Pointer); cdecl;
var
  Index:    Integer;
  ProcDone: Boolean;
begin
ProcDone := False;
repeat
  // lock the buffer (note this is a spin lock)
  If InterlockedExchange(GVAR_ModuleState.PendingSignals.Lock,US_PENDSIGSLOCK_LOCKED) = US_PENDSIGSLOCK_UNLOCKED then
  try
    // we have the lock now, store the received signal
    with GVAR_ModuleState.PendingSignals.PrimaryBuffer^ do
      begin
        Index := (Head.First + Head.Count) mod Length(Signals);
        Signals[Index].Signal := signo;
        Signals[Index].Code := siginfo^.si_code;
        Signals[Index].Value.sigval_ptr := siginfo^._sifields._rt._sigval;
        Signals[Index].Internals.Dispatched := False;
        // buffer is full, rewrite oldest entry
        If Head.Count >= Length(Signals) then
          begin
            Head.First := Succ(Head.First) mod Length(Signals);
            Head.Count := Length(Signals);
            Inc(Head.DropCount);
          end
        else Inc(Head.Count);
      end;
    ProcDone := True;
  finally
    // unlock the buffer
    InterlockedStore(GVAR_ModuleState.PendingSignals.Lock,US_PENDSIGSLOCK_UNLOCKED);
  end;
until ProcDone;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{===============================================================================
    Signal handling and setup - signal set manipulation
===============================================================================}

procedure SignalSetEmpty(var SignalSet: sigset_t);
begin
If sigemptyset(@SignalSet) <> 0 then
  raise EUSSignalSetError.CreateFmt('SignalEmptySet: Failed to empty signal set (%d).',[errno_ptr^]);
end;

//------------------------------------------------------------------------------

procedure SignalSetAdd(var SignalSet: sigset_t; Signal: cint);
begin
If sigaddset(@SignalSet,Signal) <> 0 then
  raise EUSSignalSetError.CreateFmt('SignalAddSet: Failed to add signal to set (%d).',[errno_ptr^]);
end;

//------------------------------------------------------------------------------

procedure SignalSetRemove(var SignalSet: sigset_t; Signal: cint);
begin
If sigdelset(@SignalSet,Signal) <> 0 then
  raise EUSSignalSetError.CreateFmt('SignalRemoveSet: Failed to remove signal from set (%d).',[errno_ptr^]);
end;

{===============================================================================
    Signal handling and setup - signal setup
===============================================================================}

procedure SignalBuffersAllocate;
begin
// prepare not only buffers but also their lock
GVAR_ModuleState.PendingSignals.Lock := US_PENDSIGSLOCK_UNLOCKED;
GVAR_ModuleState.PendingSignals.PrimaryBuffer := AllocMem(SizeOf(TUSPendingSignalsBuffer)); // memory is zeroed
GVAR_ModuleState.PendingSignals.SecondaryBuffer := AllocMem(SizeOf(TUSPendingSignalsBuffer));
end;

//------------------------------------------------------------------------------

procedure SignalBuffersDeallocate;
begin
FreeMem(GVAR_ModuleState.PendingSignals.PrimaryBuffer,SizeOf(TUSPendingSignalsBuffer));
FreeMem(GVAR_ModuleState.PendingSignals.SecondaryBuffer,SizeOf(TUSPendingSignalsBuffer));
end;

//==============================================================================

Function SignalAllocate: cint;
begin
// check whether we have room for a new signal
with GVAR_ModuleState.AllocatedSignals do
  If Count >= Length(Signals) then
    raise EUSSignalSetupError.Create('SignalAllocate: Too many signals allocated.');
// get unused signal number
Result := allocate_rtsig(1);
If Result < 0 then
  raise EUSSignalSetupError.CreateFmt('SignalAllocate: Failed to allocate signal number (%d).',[errno_ptr^]);
// add newly allocated signal to the global state
with GVAR_ModuleState.AllocatedSignals do
  begin
    Signals[Count] := Result;
    Inc(Count);
  end;
end;

//==============================================================================

procedure SignalActionInstall(Signal: cint; ExpectedHandler: Pointer = nil);
var
  SignalAction: sigaction_t;
begin
{
  Check that the selected signal is really unused (does not have handler
  assigned) or has handler that we expect.
}
FillChar(Addr(SignalAction)^,SizeOf(sigaction_t),0);
If sigaction(Signal,nil,@SignalAction) <> 0 then
  raise EUSSignalSetupError.CreateFmt('SignalActionInstall: Failed to probe signal action (%d).',[errno_ptr^]);
If (@SignalAction.handler.sa_sigaction <> ExpectedHandler) and
  (@SignalAction.handler.sa_handler <> Pointer(SIG_DFL)) and
  (@SignalAction.handler.sa_handler <> Pointer(SIG_IGN)) then
  raise EUSSignalSetupError.CreateFmt('SignalActionInstall: Signal (#%d) handler has unexpected value.',[Signal]);
// add the signal to global signal mask
SignalSetAdd(GVAR_ModuleState.SignalMask,Signal);
// setup signal handler
FillChar(Addr(SignalAction)^,SizeOf(sigaction_t),0);
SignalAction.handler.sa_sigaction := SignalHandler;
SignalAction.sa_flags := SA_SIGINFO or SA_RESTART;
// do not block anything (the signal for which action is installed is blocked implicitly)
SignalSetEmpty(SignalAction.sa_mask);
// install action
If sigaction(Signal,@SignalAction,nil) <> 0 then
  raise EUSSignalSetupError.CreateFmt('SignalActionInstall: Failed to setup action for signal #%d (%d).',[Signal,errno_ptr^]);
end;

//------------------------------------------------------------------------------

procedure SignalActionUninstall(Signal: cint);
var
  SignalAction: sigaction_t;
begin
// clear signal handler
FillChar(Addr(SignalAction)^,SizeOf(sigaction_t),0);
{
  Field sa_sigaction is overlayed on sa_handler (which is explicitly assigned
  later), but to be sure we clear it anyway.
}
SignalAction.handler.sa_sigaction := nil;
@SignalAction.handler.sa_handler := Pointer(SIG_DFL); // default action (terminate)
SignalSetEmpty(SignalAction.sa_mask);
If sigaction(Signal,@SignalAction,nil) <> 0 then
  raise EUSSignalSetupError.CreateFmt('SignalActionUninstall: Failed to setup action for signal #%d (%d).',[Signal,errno_ptr^]);
// remove from global signal mask
SignalSetRemove(GVAR_ModuleState.SignalMask,Signal);
end;


{$IFDEF ModuleShared}
{===============================================================================
--------------------------------------------------------------------------------
                           Global (cross-module) state
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Global state - constants, types, variables
===============================================================================}
type
  TUSGlobalStateModule = record
    IsMaster:         Boolean;
    SlaveFunctions:   record
      // called by master module on slaves...
      SignalDispatchFrom: Function(Buffer: PUSPendingSignalsBuffer): Boolean; stdcall;
      MakeMaster:         Function(ExpectedHandler: Pointer): Boolean; stdcall;
      UpdateSignals:      Function: Boolean; stdcall;
    end;
    MasterFunctions:  record
      // called by slaves on master module...
      AllocateSignal:   Function: Boolean; stdcall;
    end;
  end;
  PUSGlobalStateModule  = ^TUSGlobalStateModule;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
type
  TUSGlobalStateHead = record
    Version:            Integer;  // only assigned in initialization, no need for PGV lock
    Lock:               pthread_mutex_t;
  {
    All following fields (and the entire module list) must be protected by the
    preceding lock.
  }
    AllocatedSignals:   TUSAllocatedSignals;
    ModuleListCapacity: Integer;
    ModuleListCount:    Integer;
    ModuleList:         TPGVVariable;
  end;
  PUSGlobalStateHead = ^TUSGlobalStateHead;

const
  US_GLOBSTATE_HEAD_NAME = 'utilitysignal_shared_head';
  US_GLOBSTATE_LIST_NAME = 'utilitysignal_shared_list';

  US_GLOBSTATE_VERSION = 1;

  US_GLOBSTATE_LIST_CAPDELTA = 8;

{===============================================================================
    Global state - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Global state - auxiliary functions
-------------------------------------------------------------------------------}

Function GetModuleListSize(Capacity: Integer): TMemSize;
begin
If Capacity > 0 then
  Result := TMemSize(Capacity * SizeOf(TUSGlobalStateModule))
else
  raise EUSInvalidValue.CreateFmt('GS_GetModuleListSize: Invalid list capacity (%d).',[Capacity]);
end;

//------------------------------------------------------------------------------

Function GetModuleListItem(BaseAddress: PUSGlobalStateModule; Index: Integer): PUSGlobalStateModule;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Index > 0 then
  Result := PUSGlobalStateModule(PtrUInt(BaseAddress) + PtrUInt(Index * SizeOf(TUSGlobalStateModule)))
else If Index = 0 then
  Result := BaseAddress
else
  raise EUSInvalidValue.CreateFmt('GetModuleListItem: Invalid list index (%d).',[Index]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetModuleListNext(ItemPtr: PUSGlobalStateModule): PUSGlobalStateModule;
begin
Result := ItemPtr;
Inc(Result);
end;

{-------------------------------------------------------------------------------
    Global state - exported functions
-------------------------------------------------------------------------------}

Function SignalDispatchFromExp(Buffer: PUSPendingSignalsBuffer): Boolean; stdcall;
begin
Result := False;
try
  GVAR_ModuleState.ProcessingLock.Enter;
  try
    TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).DispatchFrom(Buffer^);
    Result := True;
  finally
    GVAR_ModuleState.ProcessingLock.Leave;
  end;
except
  // do not allow exception to escape from current module
end;
end;

//------------------------------------------------------------------------------

Function MakeMasterExp(ExpectedHandler: Pointer): Boolean; stdcall;
var
  i:  Integer;
begin
Result := False;
try
  GVAR_ModuleState.ProcessingLock.Enter;
  try
    // no need to allocate signal, it was already done by first master module
    SignalBuffersAllocate;
  {
    We should have all signals, but to be completely sure (note global state is
    already locked here)...
  }
    GVAR_ModuleState.AllocatedSignals := PUSGlobalStateHead(GVAR_ModuleState.GlobalInfo.HeadVariable^)^.AllocatedSignals;
    SignalSetEmpty(GVAR_ModuleState.SignalMask);
    For i := 0 to Pred(GVAR_ModuleState.AllocatedSignals.Count) do
      // following also (re)fills signal mask
      SignalActionInstall(GVAR_ModuleState.AllocatedSignals.Signals[i],ExpectedHandler);
    GVAR_ModuleState.GlobalInfo.IsMaster := True;
    Result := True;
  finally
    GVAR_ModuleState.ProcessingLock.Leave;
  end;
except
end;
end;

//------------------------------------------------------------------------------

Function UpdateSignalsExp: Boolean; stdcall;
var
  GSHeadPtr:  PUSGlobalStateHead;
  i:          Integer;
begin
Result := False;
try
  GVAR_ModuleState.ProcessingLock.Enter;
  try
    // get signals from global state (note that global state is already locked)
    GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
    GVAR_ModuleState.AllocatedSignals := GSHeadPtr^.AllocatedSignals;
    // refill signal mask
    SignalSetEmpty(GVAR_ModuleState.SignalMask);
    For i := 0 to Pred(GVAR_ModuleState.AllocatedSignals.Count) do
      SignalSetAdd(GVAR_ModuleState.SignalMask,GVAR_ModuleState.AllocatedSignals.Signals[i]);
    Result := True;
  finally
    GVAR_ModuleState.ProcessingLock.Leave;
  end;
except
end;
end;

//------------------------------------------------------------------------------

Function AllocateSignalExp: Boolean; stdcall;
var
  GSHeadPtr:  PUSGlobalStateHead;
  GSItemPtr:  PUSGlobalStateModule;
  i:          Integer;
begin
Result := False;
try
  GVAR_ModuleState.ProcessingLock.Enter;
  try
    // allocate new signal as usual
    SignalActionInstall(SignalAllocate,nil);
  {
    Propagate it to global state and other modules (global state is locked
    here) including the caller, but not here.
  }
    GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
    GSHeadPtr^.AllocatedSignals := GVAR_ModuleState.AllocatedSignals;
    GSItemPtr := GSHeadPtr^.ModuleList^;
    For i := 0 to Pred(GSHeadPtr^.ModuleListCount) do
      begin
        If not GSItemPtr^.IsMaster then
          begin
            If not Assigned(GSItemPtr^.SlaveFunctions.UpdateSignals) then
              raise EUSGlobalStateCallError.Create('AllocateSignalExp: UpdateSignals not assigned.');
            If not GSItemPtr^.SlaveFunctions.UpdateSignals() then
              raise EUSGlobalStateCallError.Create('AllocateSignalExp: Failed to update signals in other module.');
          end;
        Inc(GSItemPtr);
      end;
    Result := True;
  finally
    GVAR_ModuleState.ProcessingLock.Leave;
  end;
except
end;
end;

{-------------------------------------------------------------------------------
    Global state - operation functions
-------------------------------------------------------------------------------}

procedure GlobalStateLock;
var
  GSHeadPtr:  PUSGlobalStateHead;
  RetVal:     cInt;
begin
// global state head is never reallocated, so using its address directly is safe
GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
RetVal := pthread_mutex_lock(@GSHeadPtr^.Lock);
If RetVal = ESysEOWNERDEAD then
  begin
    // unlocking the mutex now makes it permanently unusable
    pthread_mutex_unlock(@GSHeadPtr^.Lock);
    raise EUSGlobalStateMutexError.Create('GlobalStateLock: Mutex owner died, global state can be damaged.');
  end
else If not PThrResChk(RetVal) then
  raise EUSGlobalStateMutexError.CreateFmt('GlobalStateLock: Failed to lock mutex (%d).',[ThrErrorCode]);
end;

//------------------------------------------------------------------------------

Function GlobalStateTryLock: Boolean;
var
  GSHeadPtr:  PUSGlobalStateHead;
  RetVal:     cInt;
begin
GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
RetVal := pthread_mutex_trylock(@GSHeadPtr^.Lock);
case RetVal of
  ESysEOWNERDEAD:
    begin
      pthread_mutex_unlock(@GSHeadPtr^.Lock);
      raise EUSGlobalStateMutexError.Create('GlobalStateTryLock: Mutex owner died, global state can be damaged.');
    end;
  ESysEBUSY:
    // mutex was locked
    Result := False;
else
  If not PThrResChk(RetVal) then
    // some "other" error
    raise EUSGlobalStateMutexError.CreateFmt('GlobalStateTryLock: Failed to try-lock mutex (%d).',[ThrErrorCode])
  else
    // mutex was unlocked and is now locked for the calling thread
    Result := True;
end;
end;

//------------------------------------------------------------------------------

procedure GlobalStateUnlock;
var
  GSHeadPtr:  PUSGlobalStateHead;
begin
GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
If not PThrResChk(pthread_mutex_unlock(@GSHeadPtr^.Lock)) then
  raise EUSGlobalStateMutexError.CreateFmt('GlobalStateUnlock: Failed to unlock mutex (%d).',[ThrErrorCode]);
end;

//------------------------------------------------------------------------------

procedure GlobalStateAdd(IsMaster: Boolean);
var
  GSHeadPtr:  PUSGlobalStateHead;
  GSListPtr:  PUSGlobalStateModule;
begin
GlobalStateLock;
try
  GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
  // reallocate module list if needed
  If GSHeadPtr^.ModuleListCount >= GSHeadPtr^.ModuleListCapacity then
    begin
      Inc(GSHeadPtr^.ModuleListCapacity,US_GLOBSTATE_LIST_CAPDELTA);
      GSHeadPtr^.ModuleList := GlobVarRealloc(US_GLOBSTATE_LIST_NAME,GetModuleListSize(GSHeadPtr^.ModuleListCapacity))^;
    end;
  Inc(GSHeadPtr^.ModuleListCount);
  GSListPtr := GetModuleListItem(GSHeadPtr^.ModuleList^,Pred(GSHeadPtr^.ModuleListCount));
  GSListPtr^.IsMaster := IsMaster;
  GSListPtr^.SlaveFunctions.SignalDispatchFrom := SignalDispatchFromExp;
  GSListPtr^.SlaveFunctions.MakeMaster := MakeMasterExp;
  GSListPtr^.SlaveFunctions.UpdateSignals := UpdateSignalsExp;
  GSListPtr^.MasterFunctions.AllocateSignal := AllocateSignalExp;
finally
  GlobalStateUnlock;
end;
end;

//------------------------------------------------------------------------------

procedure GlobalStateRemove;
var
  GSHeadPtr:  PUSGlobalStateHead;
  GSListPtr:  PUSGlobalStateModule;
  GSItemPtr:  PUSGlobalStateModule;
  i,Index:    Integer;
begin
GlobalStateLock;
try
  GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
  GSListPtr := GSHeadPtr^.ModuleList^;
  Index := -1;
  GSItemPtr := GSListPtr;
  // find where we are
  For i := 0 to Pred(GSHeadPtr^.ModuleListCount) do
    // comparing only SignalDispatchFrom should suffice
    If @GSItemPtr^.SlaveFunctions.SignalDispatchFrom = @SignalDispatchFromExp then
      begin
        Index := i;
        Break{For i};
      end
    else Inc(GSItemPtr);
  // if this module was found, remove it
  If Index >= 0 then
    begin
      GSItemPtr := GetModuleListItem(GSListPtr,Index);
      For i := Index to (GSHeadPtr^.ModuleListCount - 2) do
        begin
          GSItemPtr^ := GetModuleListNext(GSItemPtr)^;
          Inc(GSItemPtr);
        end;
      FillChar(GSItemPtr^,SizeOf(TUSGlobalStateModule),0);
      Dec(GSHeadPtr^.ModuleListCount);
    end
  else raise EUSGlobalStateModListError.Create('GlobalStateRemove: Cannot find current module.');
finally
  GlobalStateUnlock;
end;
end;

{-------------------------------------------------------------------------------
    Global state - init/final functions
-------------------------------------------------------------------------------}

procedure GlobalStateInit;
var
  GSHeadPtr:  PUSGlobalStateHead;
  MutexAttr:  pthread_mutexattr_t;
begin
// PGV must be locked before calling this function
GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
FillChar(GSHeadPtr^,SizeOf(TUSGlobalStateHead),0);
GSHeadPtr^.Version := US_GLOBSTATE_VERSION;
// create state lock
If PThrResChk(pthread_mutexattr_init(@MutexAttr)) then
  try
    // make the mutex recursive and robust (it does not need to be process-shared)
    If not PThrResChk(pthread_mutexattr_settype(@MutexAttr,PTHREAD_MUTEX_RECURSIVE)) then
      raise EUSGlobalStateMutexError.CreateFmt('GlobalStateInit: Failed to set mutex attribute type (%d).',[ThrErrorCode]);
    If not PThrResChk(pthread_mutexattr_setrobust(@MutexAttr,PTHREAD_MUTEX_ROBUST)) then
      raise EUSGlobalStateMutexError.CreateFmt('GlobalStateInit: Failed to set mutex attribute robust (%d).',[ThrErrorCode]);
    If not PThrResChk(pthread_mutex_init(@GSHeadPtr^.Lock,@MutexAttr)) then
      raise EUSGlobalStateMutexError.CreateFmt('GlobalStateInit: Failed to init mutex (%d).',[ThrErrorCode]);
  finally
    pthread_mutexattr_destroy(@MutexAttr);
  end
else raise EUSGlobalStateMutexError.CreateFmt('GlobalStateInit: Failed to init mutex attributes (%d).',[ThrErrorCode]);
// prepare non-zero fields
GSHeadPtr^.ModuleListCapacity := US_GLOBSTATE_LIST_CAPDELTA;
// allocate module list
GSHeadPtr^.ModuleList := GlobVarAlloc(US_GLOBSTATE_LIST_NAME,GetModuleListSize(GSHeadPtr^.ModuleListCapacity));
end;

//------------------------------------------------------------------------------

procedure GlobalStateOpen;
var
  VarSize:    TMemSize;
  GSHeadVar:  TPGVVariable;
  GSHeadPtr:  PUSGlobalStateHead;
  i:          Integer;
begin
GlobVarLock;
try
  VarSize := SizeOf(TUSGlobalStateHead);
  case GlobVarGet(US_GLOBSTATE_HEAD_NAME,VarSize,GSHeadVar) of
    vgrCreated:
      begin
        // we have created the gobal (shared) state, do initialization...
        // prepare module state (we are first so we are also the master)
        GVAR_ModuleState.GlobalInfo.HeadVariable := GSHeadVar;
        GVAR_ModuleState.GlobalInfo.IsMaster := True;
        // prepare first signal
        SignalBuffersAllocate;
        SignalActionInstall(SignalAllocate);
        // prepare global state
        GlobalStateInit;
        GlobalStateAdd(True);
        PUSGlobalStateHead(GSHeadVar^)^.AllocatedSignals := GVAR_ModuleState.AllocatedSignals;
        GlobVarAcquire(GSHeadVar);
        // no locks were taken, so ensure everything is wisible
        ReadWriteBarrier;
      end;
    vgrOpened:
      begin
      {
        We have opened the global state, so it already existed - this also
        means that the signals were already allocated elsewhere.
      }
        GVAR_ModuleState.GlobalInfo.HeadVariable := GSHeadVar;
        GVAR_ModuleState.GlobalInfo.IsMaster := False;
        GSHeadPtr := PUSGlobalStateHead(GSHeadVar^);
        GlobalStateLock;
        try
          // check that we got list variable with proper size
          If GlobVarSize(GSHeadPtr^.ModuleList) <> GetModuleListSize(GSHeadPtr^.ModuleListCapacity) then
            raise EUSGlobalStateOpenError.Create('GlobalStateOpen: Module list has unexpected size.');
        {
          Do not allocate signal (only assign existing) and signal buffers and
          do not install signal action.

          Since ve are accessing allocated signals in both global and module
          states, we must also lock both.
        }
          GVAR_ModuleState.ProcessingLock.Enter;
          try
            // copy existing signals and prepare signal mask
            GVAR_ModuleState.AllocatedSignals := GSHeadPtr^.AllocatedSignals;
            SignalSetEmpty(GVAR_ModuleState.SignalMask);
            For i := 0 to Pred(GVAR_ModuleState.AllocatedSignals.Count) do
              SignalSetAdd(GVAR_ModuleState.SignalMask,GVAR_ModuleState.AllocatedSignals.Signals[i]);
          finally
            GVAR_ModuleState.ProcessingLock.Leave;
          end;
          GlobalStateAdd(False);
        finally
          GlobalStateUnlock;
        end;
        GlobVarAcquire(GSHeadVar);
      end;
    vgrSizeMismatch:
      raise EUSGlobalStateOpenError.Create('GlobalStateOpen: Size mismatch when opening module-shared state.');
  else
   {vgrError}
    raise EUSGlobalStateOpenError.Create('GlobalStateOpen: Failed to open module-shared state.');
  end;
finally
  GlobVarUnlock;
end;
end;

//------------------------------------------------------------------------------

procedure GlobalStateClose;
var
  GSHeadPtr:    PUSGlobalStateHead;
  GSModulePtr:  PUSGlobalStateModule;
  i:            Integer;
begin
GlobVarLock;
try
  GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
  If GlobVarRelease(US_GLOBSTATE_HEAD_NAME) > 0 then
    begin
    {
      Some other module is still using the global state.
      Lock the global state to prevent others from calling to us.
    }
      GlobalStateLock;
      try
        GlobalStateRemove;
        If GVAR_ModuleState.GlobalInfo.IsMaster then
          begin
          {
            We are master module, transfer master status to other module - note
            that current module is no longer in the list.
          }
            If GSHeadPtr^.ModuleListCount > 0 then
              begin
                GSModulePtr := PUSGlobalStateModule(GSHeadPtr^.ModuleList^);
                If Assigned(@GSModulePtr^.SlaveFunctions.MakeMaster) then
                  begin
                    If not GSModulePtr^.SlaveFunctions.MakeMaster(@SignalHandler) then
                      raise EUSGlobalStateCallError.Create('GlobalStateClose: Failed to transfer master status.');
                    GSModulePtr^.IsMaster := True;
                  end
                else raise EUSGlobalStateCallError.Create('GlobalStateClose: MakeMaster not assigned.');
              end
            else raise EUSGlobalStateCloseError.Create('GlobalStateClose: Empty module list.');
          {
            Do not call SignalActionUninstall - the actions were replaced by
            the new master module in MakeMaster.
          }
            SignalBuffersDeallocate;
          end;
        // if we are not master, there is nothing more to be done
      finally
        GlobalStateUnlock;
      end;
    end
  else
    begin
    {
      We are the last module using the global state (we must be master),
      uninstall all signal handlers and free pending signals buffers.
    }
      For i := 0 to Pred(GVAR_ModuleState.AllocatedSignals.Count) do
        SignalActionUninstall(GVAR_ModuleState.AllocatedSignals.Signals[i]);
      SignalBuffersDeallocate;
      // destroy the global state
      GlobVarFree(US_GLOBSTATE_LIST_NAME);
      If not PThrResChk(pthread_mutex_destroy(@GSHeadPtr^.Lock)) then
        raise EUSGlobalStateMutexError.CreateFmt('GlobalStateClose: Failed to destroy mutex (%d).',[ThrErrorCode]);
      GlobVarFree(US_GLOBSTATE_HEAD_NAME);
    end;
finally
  GlobVarUnlock;
end;
end;

{-------------------------------------------------------------------------------
    Global state - dispatch functions
-------------------------------------------------------------------------------}

procedure GlobalStateDispatch(Buffer: PUSPendingSignalsBuffer);
var
  GSHeadPtr:  PUSGlobalStateHead;
  GSItemPtr:  PUSGlobalStateModule;
  i:          Integer;
begin
GlobalStateLock;
try
  GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
  If GSHeadPtr^.ModuleListCount > 1 then  // is there anyone but us?
    begin
      GSItemPtr := GSHeadPtr^.ModuleList^;
      // traverse all modules and dispatch signal buffer to each one (except ourselves)
      For i := 0 to Pred(GSHeadPtr^.ModuleListCount) do
        begin
          If not GSItemPtr^.IsMaster then
            begin
              If not Assigned(GSItemPtr^.SlaveFunctions.SignalDispatchFrom) then
                raise EUSGlobalStateCallError.Create('GlobalStateDispatch: SignalDispatchFrom not assigned.');
              If not GSItemPtr^.SlaveFunctions.SignalDispatchFrom(Buffer) then
                raise EUSGlobalStateCallError.Create('GlobalStateDispatch: Failed to dispatch signals to other module.');
            end;
          Inc(GSItemPtr);
        end;
    end;
finally
  GlobalStateUnlock;
end;
end;

{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                 Signal fetching
--------------------------------------------------------------------------------
===============================================================================}
// it is down here to remove a need for forward declarations

procedure SignalFetch;
var
  Temp:     PUSPendingSignalsBuffer;
  XchgDone: Boolean;
begin
GVAR_ModuleState.ProcessingLock.Enter;
try
{$IFDEF ModuleShared}
  // if we are not master then there is nothing to fetch (buffers are not even allocated)
  If not GVAR_ModuleState.GlobalInfo.IsMaster then
    Exit;
{$ENDIF}
  // block delivery of allocated signals so we can safely lock signal buffer
  If not PThrResChk(pthread_sigmask(SIG_BLOCK,@GVAR_ModuleState.SignalMask,nil)) then
    raise EUSSignalSetupError.CreateFmt('SignalProcess: Failed to block signal (%d).',[ThrErrorCode]);
  try
    XchgDone := False;
    repeat
      If InterlockedExchange(GVAR_ModuleState.PendingSignals.Lock,US_PENDSIGSLOCK_LOCKED) = US_PENDSIGSLOCK_UNLOCKED then
      try
        // we have the lock, exchange buffers
        Temp := GVAR_ModuleState.PendingSignals.SecondaryBuffer;
        GVAR_ModuleState.PendingSignals.SecondaryBuffer := GVAR_ModuleState.PendingSignals.PrimaryBuffer;
        GVAR_ModuleState.PendingSignals.PrimaryBuffer := Temp;
        XchgDone := True;
      finally
        // unlock the buffer
        InterlockedStore(GVAR_ModuleState.PendingSignals.Lock,US_PENDSIGSLOCK_UNLOCKED);
      end;
    until XchgDone;
  finally
    If not PThrResChk(pthread_sigmask(SIG_UNBLOCK,@GVAR_ModuleState.SignalMask,nil)) then
      raise EUSSignalSetupError.CreateFmt('SignalProcess: Failed to unblock signal (%d).',[ThrErrorCode]);
  end;
  // now signal handler has clean (empty) buffer and secondary buffer contains received signals
  TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).DispatchFrom(GVAR_ModuleState.PendingSignals.SecondaryBuffer^);
{$IFDEF ModuleShared}
  GlobalStateDispatch(GVAR_ModuleState.PendingSignals.SecondaryBuffer);
{$ENDIF}
  TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).ProcessOrphans(GVAR_ModuleState.PendingSignals.SecondaryBuffer^);
  // clear the processed buffer
  FillChar(GVAR_ModuleState.PendingSignals.SecondaryBuffer^,SizeOf(TUSPendingSignalsBuffer),0);
finally
  GVAR_ModuleState.ProcessingLock.Leave;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TUtilitySignal
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUtilitySignal - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUtilitySignal - protected methods
-------------------------------------------------------------------------------}

Function TUtilitySignal.GetCoalesceSignals: Boolean;
begin
// since this property can be accessed from multiple threads, we have to protect it
ThreadLock;
try
  Result := fCoalesceSignals;
finally
  ThreadUnlock;
end;
end;

//------------------------------------------------------------------------------

procedure TUtilitySignal.SetCoalesceSignals(Value: Boolean);
begin
ThreadLock;
try
  fCoalesceSignals := Value;
finally
  ThreadUnlock;
end;
end;

//------------------------------------------------------------------------------

procedure TUtilitySignal.Initialize;
begin
fRegisteredOnIdle := False;
fThreadLock := TCriticalSection.Create;
fReceivedSignals := TUSSignalQueue.Create(US_SZCOEF_QUEUE * 256);
fCreatorThread := pthread_self();
fObserveThread := True;
fCoalesceSignals := False;
fOnSignal := TUSMulticastSignalEvent.Create(Self);
TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).UtilitySignalRegister(Self);
end;

//------------------------------------------------------------------------------

procedure TUtilitySignal.Finalize;
begin
TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).UtilitySignalUnregister(Self);
FreeAndNil(fOnSignal);
FreeAndNil(fReceivedSignals);
FreeAndNil(fThreadLock);
end;

//------------------------------------------------------------------------------

procedure TUtilitySignal.ThreadLock;
begin
fThreadLock.Enter;
end;

//------------------------------------------------------------------------------

procedure TUtilitySignal.ThreadUnlock;
begin
fThreadLock.Leave;
end;

//------------------------------------------------------------------------------

procedure TUtilitySignal.AddSignal(SignalInfo: TUSSignalInfo);
var
  Index:  Integer;
begin
// thread lock must be activated externally
If fCoalesceSignals then
  begin
    If not fReceivedSignals.Find(SignalInfo,Index) then
      fReceivedSignals.Push(SignalInfo);
  end
else fReceivedSignals.Push(SignalInfo);
end;

//------------------------------------------------------------------------------

Function TUtilitySignal.CheckThread: Boolean;
begin
If fObserveThread then
  Result := pthread_equal(fCreatorThread,pthread_self) <> 0
else
  Result := True;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TUtilitySignal.OnAppIdleHandler(Sender: TObject; var Done: Boolean);
begin
ProcessSignals;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{-------------------------------------------------------------------------------
    TUtilitySignal - public methods
-------------------------------------------------------------------------------}

constructor TUtilitySignal.Create(CanRegisterForOnIdle: Boolean = True);
begin
inherited Create;
Initialize;
If CanRegisterForOnIdle then
  RegisterForOnIdle;
end;

//------------------------------------------------------------------------------

destructor TUtilitySignal.Destroy;
begin
UnregisterFromOnIdle;
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TUtilitySignal.RegisterForOnIdle: Boolean;
begin
Result := False;
{$IFDEF LCL}
If CheckThread and (pthread_equal(pthread_self(),MainThreadID) <> 0) then
  begin
    Application.AddOnIdleHandler(OnAppIdleHandler,False);
    fRegisteredOnIdle := True;
    Result := True;
  end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TUtilitySignal.UnregisterFromOnIdle;
begin
{$IFDEF LCL}
If fRegisteredOnIdle then
  Application.RemoveOnIdleHandler(OnAppIdleHandler);
{$ENDIF}
fRegisteredOnIdle := False;
end;

//------------------------------------------------------------------------------

procedure TUtilitySignal.ProcessSignal;
begin
SignalFetch;
If CheckThread then
  begin
    ThreadLock;
    try
      If fReceivedSignals.Count > 0 then
        fOnSignal.Call(Self,fReceivedSignals.Pop);
    finally
      ThreadUnlock;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TUtilitySignal.ProcessSignals;
begin
SignalFetch;
If CheckThread then
  begin
    ThreadLock;
    try
      while fReceivedSignals.Count > 0 do
        fOnSignal.Call(Self,fReceivedSignals.Pop);
    finally
      ThreadUnlock;
    end;
  end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Orphan signals
--------------------------------------------------------------------------------
===============================================================================}

procedure RegisterForOrphanSignals(Callback: TUSSignalCallback);
begin
TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).OrphanSignalsRegister(Callback);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RegisterForOrphanSignals(Event: TUSSignalEvent);
begin
TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).OrphanSignalsRegister(Event);
end;

//------------------------------------------------------------------------------

procedure UnregisterFromOrphanSignals(Callback: TUSSignalCallback);
begin
TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).OrphanSignalsUnregister(Callback);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure UnregisterFromOrphanSignals(Event: TUSSignalEvent);
begin
TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).OrphanSignalsUnregister(Event);
end;

//==============================================================================

procedure ProcessOrphanSignal;
begin
SignalFetch;
TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).ProcessOrphanSignal;
end;

//------------------------------------------------------------------------------

procedure ProcessOrphanSignals;
begin
SignalFetch;
TUSSignalDispatcher(GVAR_ModuleState.Dispatcher).ProcessOrphanSignals;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              New signal allocation
--------------------------------------------------------------------------------
===============================================================================}

Function AllocateSignal: cint;
{$IFDEF ModuleShared}

  Function TryAllocateSignal(out Signal: cint): Boolean;
  var
    GSHeadPtr:  PUSGlobalStateHead;
    GSItemPtr:  PUSGlobalStateModule;
    i:          Integer;
  begin
    GVAR_ModuleState.ProcessingLock.Enter;
    try
      If GVAR_ModuleState.GlobalInfo.IsMaster then
        begin
          Result := True;
          // we are master, therefore responsible for signal allocation
          Signal := SignalAllocate;
          SignalActionInstall(Signal,nil);
          // now propagate changes to global state and other modules
          GlobalStateLock;
          try
            GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
            GSHeadPtr^.AllocatedSignals := GVAR_ModuleState.AllocatedSignals;
            GSItemPtr := GSHeadPtr^.ModuleList^;
            For i := 0 to Pred(GSHeadPtr^.ModuleListCount) do
              begin
                // do not call back here
                If not GSItemPtr^.IsMaster then
                  begin
                    If not Assigned(GSItemPtr^.SlaveFunctions.UpdateSignals) then
                      raise EUSGlobalStateCallError.Create('AllocateSignal: UpdateSignals not assigned.');
                    If not GSItemPtr^.SlaveFunctions.UpdateSignals() then
                      raise EUSGlobalStateCallError.Create('AllocateSignal: Failed to update signals in other module.');
                  end;
                Inc(GSItemPtr);
              end;
          finally
            GlobalStateUnlock;
          end;
        end
      else
        begin
        {
          Slave module, we need to find master and ask him to allocate.

          ------------------------   !!! WARNING !!!   -------------------------

          This piece of code could cause a deadlock with several master-called
          routines because of lock inversion. Here, the locking sequence is
          GVAR_ModuleState.ProcessingLock > GlobalStateLock, whereas in master
          it is GlobalStateLock > (this module's)VAR_ModuleState.ProcessingLock.

          Consider following sequence of events:

            - thread #1 calls slave AllocateSignal

            - thread #1 locks GVAR_ModuleState.ProcessingLock

            - thread #2 locks GlobalStateLock via a call to some master routine

            - thread #1 arrives here and calls GlobalStateLock - this will
              block because that lock is held by thread #2

            - thread #2 arrives to local GVAR_ModuleState.ProcessingLock and
              tries to lock it - this will also block because this lock is held
              by thread #1

          ...and now both threads are blocked with no chance to be released.

          To alleviate this problem, I have reworked locking of global state
          so that if it is locked by other thread, the entire function rolls
          back, including unlocking the local ProcessingLock, and runs again
          some time later.
          Unlocking the processing lock gives chance to other thread(s) that
          might be waiting on it to acquire it and thus resolve the possible
          race condition.
        }
          If GlobalStateTryLock then
            try
              GSHeadPtr := GVAR_ModuleState.GlobalInfo.HeadVariable^;
              GSItemPtr := GSHeadPtr^.ModuleList^;
              For i := 0 to Pred(GSHeadPtr^.ModuleListCount) do
                If GSItemPtr^.IsMaster then
                  begin
                    If not Assigned(GSItemPtr^.MasterFunctions.AllocateSignal) then
                      raise EUSGlobalStateCallError.Create('AllocateSignal: AllocateSignal not assigned.');
                    If not GSItemPtr^.MasterFunctions.AllocateSignal() then
                      raise EUSGlobalStateCallError.Create('AllocateSignal: Failed to allocate signal in master.');
                    Break{For i};
                  end
                else Inc(GSItemPtr);
              Signal := GVAR_ModuleState.AllocatedSignals.Signals[Pred(GVAR_ModuleState.AllocatedSignals.Count)];
              Result := True;
            finally
              GlobalStateUnlock;
            end
          else Result := False;
        end;
    finally
      GVAR_ModuleState.ProcessingLock.Leave;
    end;
  end;

begin
while not TryAllocateSignal(Result) do
  sched_yield;
end;
{$ELSE}
begin
GVAR_ModuleState.ProcessingLock.Enter;
try
  Result := SignalAllocate;
  SignalActionInstall(Result,nil);
finally
  GVAR_ModuleState.ProcessingLock.Leave;
end;
end;
{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                      Unit initialization and finalization
--------------------------------------------------------------------------------
===============================================================================}

procedure LibraryInitialize;
begin
GVAR_ModuleState.ProcessingLock := TCriticalSection.Create;
// explicitly initialize some fileds of the global state
GVAR_ModuleState.AllocatedSignals.Count := 0;
SignalSetEmpty(GVAR_ModuleState.SignalMask);
GVAR_ModuleState.Dispatcher := TUSSignalDispatcher.Create;
{$IFDEF ModuleShared}
GlobalStateOpen;
{$ELSE}
SignalBuffersAllocate;
SignalActionInstall(SignalAllocate,nil);  // also fills signal mask
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure LibraryFinalize;
{$IFNDEF ModuleShared}
var
  i:  Integer;
{$ENDIF}
begin
{$IFDEF ModuleShared}
GlobalStateClose;
{$ELSE}
For i := 0 to Pred(GVAR_ModuleState.AllocatedSignals.Count) do
  SignalActionUninstall(GVAR_ModuleState.AllocatedSignals.Signals[i]);
SignalBuffersDeallocate;
{$ENDIF}
// there is no need to deallocate the signal (it is not possible anyway)
FreeAndNil(GVAR_ModuleState.Dispatcher);
FreeandNil(GVAR_ModuleState.ProcessingLock);
end;

//==============================================================================

initialization
  LibraryInitialize;

finalization
  LibraryFinalize;

end.
