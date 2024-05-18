{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  LinSyncObjs

    This library provides a set of classes encapsulating synchronization
    objects available in pthreads library for Linux operating system.
    It also implements some new and derived synchronization primitives that
    are not directly provided by pthreads along with a limited form of waiting
    for multiple synchronization objects (in this case waiting for multiple
    events).

    All provided objects, except for critical section, can be created either
    as thread-shared (can be used for synchronization between threads of a
    single process) or as process-shared (synchronization between any threads
    within the system, even in different processes).

    Process-shared objects reside in a shared memory and are accessed using
    their names (such object must have a non-empty name). All object types
    share the same name space, so it is not possible for multiple objects of
    different types to have the same name. Names are case sensitive.

      NOTE - the events and multi-wait were tested, but there may still be some
             problems. Use them with caution and if you find any bugs, please
             report them.

  Version 1.1 (2024-05-15)

  Last change 2024-05-15

  ©2022-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.LinSyncObjs

  Dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
    BitVector          - github.com/TheLazyTomcat/Lib.BitVector
    InterlockedOps     - github.com/TheLazyTomcat/Lib.InterlockedOps
    SharedMemoryStream - github.com/TheLazyTomcat/Lib.SharedMemoryStream
    SimpleFutex        - github.com/TheLazyTomcat/Lib.SimpleFutex

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol LinSyncObjs_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    AuxMath             - github.com/TheLazyTomcat/Lib.AuxMath
    BasicUIM            - github.com/TheLazyTomcat/Lib.BasicUIM
    BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    SimpleCPUID         - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream  - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils         - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo         - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit LinSyncObjs;
{
  LinSyncObjs_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  LinSyncObjs_UseAuxExceptions to achieve this.
}
{$IF Defined(LinSyncObjs_UseAuxExceptions)}
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
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

interface

uses
  SysUtils, BaseUnix, PThreads,
  AuxTypes, AuxClasses, BitVector, SimpleFutex, SharedMemoryStream,
  NamedSharedItems{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ELSOException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ELSOSysInitError  = class(ELSOException);
  ELSOSysFinalError = class(ELSOException);
  ELSOSysOpError    = class(ELSOException);

  ELSOOpenError       = class(ELSOException);
  ELSOInvalidLockType = class(ELSOException);
  ELSOInvalidObject   = class(ELSOException);

{===============================================================================
--------------------------------------------------------------------------------
                                TCriticalSection
--------------------------------------------------------------------------------
===============================================================================}
{
  To use the TCriticalSection object, create one instance and then pass this
  one instance to other threads that need to be synchronized.

  Make sure to only free the object once.

  You can also set the property FreeOnRelease to true (by default false) and
  then use the build-in reference counting - call method Acquire for each
  thread using the object (including the one that created it) and method
  Release every time a thread will stop using it. When reference count reaches
  zero in a call to Release, the object will be automatically freed within that
  call.
}
{===============================================================================
    TCriticalSection - class declaration
===============================================================================}
type
  TCriticalSection = class(TCustomRefCountedObject)
  protected
    fMutex:         pthread_mutex_t;
    fLockComplete:  Boolean;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function TryEnter: Boolean; virtual;
    procedure Enter; virtual;
    procedure Leave; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TLinSyncObject
--------------------------------------------------------------------------------
===============================================================================}
{
  To properly use linux synchronization object based on TLinSyncObject class
  (all that are currently provided), create an instance and use this one
  instance only in a thread where it was created.

  To access the object in other threads of the same process, create a new
  instance using DuplicateFrom constructor, passing the progenitor instance or
  any duplicate instance created previously from it as a source.
  You can also use Open or Create constructors when the object is created as
  process-shared.
  It is possible and permissible to use the same instance in multiple threads
  of single process, but this practice is not recommended as the object fields
  are not protected against concurrent access (affects eg. LastError property).

  To access the object in a different process (object must be created as
  process-shared), use Open or Create constructors using the same name as was
  used for the first instance. When opening process-shared object in the same
  process where it was created or already opened, you can use DuplicateFrom
  constructor.

  Most public functions of classes derived from TLinSyncObject that are
  operating on the object state are provided in two versions - as strict and
  non-strict.
  Unless noted otherwise, the strict functions will raise an ELSOSysOpError
  exception when the internal operation fails. Non-strict functions indicate
  success or failure via their result (true = success, false = failure).

  Value stored in LastError property is undefined after a call to any strict
  function.
  When non-strict function succeeds, the value of LastError is also undefined.
  When it fails, the LastError will contain a code of system error that caused
  the function to fail.

  Non-strict function beginning with a "Try" (eg. TryLock) might return false
  even when it technically succeeded in its operation (the object was already
  locked). In that case, LastError is explicitly set to 0.

  Timed functions will never raise an exception, all errors are indicated by
  returning wrError result and error code is stored in LastError. Value of
  LastError is undefined for results other than wrError.
}
{===============================================================================
    TLinSyncObject - public types and constants
===============================================================================}
const
  INFINITE = UInt32($FFFFFFFF); // infinite timeout

type
  TLSOSharedUserData = packed array[0..31] of Byte;
  PLSOSharedUserData = ^TLSOSharedUserData;

type
  TLSOWaitResult = (wrSignaled,wrAbandoned,wrTimeout,wrError);

  // TLSOLockType is used only internally
  TLSOLockType = (ltInvalid,ltSpinLock,ltStatelessEvent,ltSimpleManualEvent,
                  ltSimpleAutoEvent,ltEvent,ltMutex,ltSemaphore,ltRWLock,
                  ltCondVar,ltCondVarEx,ltBarrier);

{===============================================================================
    TLinSyncObject - class declaration
===============================================================================}
type
  TLinSyncObject = class(TCustomObject)
  protected
    fLastError:         Integer;
    fName:              String;
    fProcessShared:     Boolean;
    fNamedSharedItem:   TNamedSharedItem;   // unused in thread-shared mode
    fSharedData:        Pointer;
    fLockPtr:           Pointer;
    fLockPrepComplete:  Boolean;
    // getters, setters
    Function GetSharedUserDataPtr: PLSOSharedUserData; virtual;
    Function GetSharedUserData: TLSOSharedUserData; virtual;
    procedure SetSharedUserData(Value: TLSOSharedUserData); virtual;
    // lock management methods
    class Function GetLockType: TLSOLockType; virtual; abstract;
    procedure CheckAndSetLockType; virtual;
    procedure ResolveLockPtr; virtual; abstract;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); virtual;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); virtual; abstract;
    // object initialization/finalization
    procedure Initialize(const Name: String; InitializingData: PtrUInt); overload; virtual;
    // following overload can be used only to open existing process-shared objects
    procedure Initialize(const Name: String); overload; virtual;
    procedure Finalize; virtual;
  public
    constructor ProtectedCreate(const Name: String; InitializingData: PtrUInt); virtual;
    constructor Create(const Name: String); overload; virtual;
    constructor Create; overload; virtual;
    constructor Open(const Name: String); virtual;
    constructor DuplicateFrom(SourceObject: TLinSyncObject); virtual;
    destructor Destroy; override;
    // properties
    property LastError: Integer read fLastError;
    property Name: String read fName;
    property ProcessShared: Boolean read fProcessShared;
    property SharedUserDataPtr: PLSOSharedUserData read GetSharedUserDataPtr;
    property SharedUserData: TLSOSharedUserData read GetSharedUserData write SetSharedUserData;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                            TImplementorLinSynObject
--------------------------------------------------------------------------------
===============================================================================}
{
  TImplementorLinSyncObject is a base class for objects creating a distinct
  synchronization primitive that is not a simple wrapper for pthread primitives.
}
{===============================================================================
    TImplementorLinSynObject - class declaration
===============================================================================}
type
  TImplementorLinSyncObject = class(TLinSyncObject);

{===============================================================================
--------------------------------------------------------------------------------
                                     Events
--------------------------------------------------------------------------------
===============================================================================}
type
  TLSOSimpleEvent = record
    Event:  TFutexWord;
  end;
  PLSOSimpleEvent = ^TLSOSimpleEvent;

{===============================================================================
--------------------------------------------------------------------------------
                                TStatelessEvent
--------------------------------------------------------------------------------
===============================================================================}
{
  Stateless event does not have any internal state (obviously), so it cannot be
  locked (non-signaled) or unlocked (signaled).

  When a thread enters waiting on this type of event, it is not waiting for the
  event to become unlocked (signaled), it waits for some other thread to call
  method Signal or SignalStrict. When Signal* method is called, all waiting
  threads are released from waiting. Threads entering wait after this call will
  again block until next call to Signal*.
}
{===============================================================================
    TStatelessEvent - flat interface declaration
===============================================================================}

Function event_stateless_init(event: PLSOSimpleEvent): cInt;
Function event_stateless_destroy(event: PLSOSimpleEvent): cInt;

Function event_stateless_signal(event: PLSOSimpleEvent): cInt;
Function event_stateless_wait(event: PLSOSimpleEvent): cInt;
Function event_stateless_timedwait(event: PLSOSimpleEvent; timeout: cUnsigned): cInt;

{===============================================================================
    TStatelessEvent - class declaration
===============================================================================}
type
  TStatelessEvent = class(TImplementorLinSyncObject)
  protected
    class Function GetLockType: TLSOLockType; override;
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
  public
    procedure SignalStrict; virtual;
    Function Signal: Boolean; virtual;
    procedure WaitStrict; virtual;
    Function Wait: Boolean; virtual;
    Function TimedWait(Timeout: UInt32): TLSOWaitResult; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleEventBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleEventBase - class declaration
===============================================================================}
type
  TSimpleEventBase = class(TImplementorLinSyncObject)
  protected
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
    class Function WakeCount: Integer; virtual; abstract;
  public
    procedure LockStrict; virtual;
    Function Lock: Boolean; virtual;
    procedure UnlockStrict; virtual;
    Function Unlock: Boolean; virtual;
    procedure WaitStrict; virtual; abstract;
    Function Wait: Boolean; virtual; abstract;
    Function TryWaitStrict: Boolean; virtual; abstract;
    Function TryWait: Boolean; virtual; abstract;
    Function TimedWait(Timeout: UInt32): TLSOWaitResult; virtual; abstract;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleManualEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleManualEvent - flat interface declaration
===============================================================================}

Function event_manual_init(event: PLSOSimpleEvent): cInt;
Function event_manual_destroy(event: PLSOSimpleEvent): cInt;

Function event_manual_lock(event: PLSOSimpleEvent): cInt;
Function event_manual_unlock(event: PLSOSimpleEvent): cInt;

Function event_manual_wait(event: PLSOSimpleEvent): cInt;
Function event_manual_trywait(event: PLSOSimpleEvent): cInt;
Function event_manual_timedwait(event: PLSOSimpleEvent; timeout: cUnsigned): cInt;

{===============================================================================
    TSimpleManualEvent - class declaration
===============================================================================}
type
  TSimpleManualEvent = class(TSimpleEventBase)
  protected
    class Function GetLockType: TLSOLockType; override;
    class Function WakeCount: Integer; override;
  public
    procedure WaitStrict; override;
    Function Wait: Boolean; override;
    Function TryWaitStrict: Boolean; override;
    Function TryWait: Boolean; override;
    Function TimedWait(Timeout: UInt32): TLSOWaitResult; override;
  end;

  TSimpleEvent = TSimpleManualEvent;

{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleAutoEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleAutoEvent - flat interface declaration
===============================================================================}

Function event_auto_init(event: PLSOSimpleEvent): cInt;
Function event_auto_destroy(event: PLSOSimpleEvent): cInt;

Function event_auto_lock(event: PLSOSimpleEvent): cInt;
Function event_auto_unlock(event: PLSOSimpleEvent): cInt;

Function event_auto_wait(event: PLSOSimpleEvent): cInt;
Function event_auto_trywait(event: PLSOSimpleEvent): cInt;
Function event_auto_timedwait(event: PLSOSimpleEvent; timeout: cUnsigned): cInt;

{===============================================================================
    TSimpleAutoEvent - class declaration
===============================================================================}
type
  TSimpleAutoEvent = class(TSimpleEventBase)
  protected
    class Function GetLockType: TLSOLockType; override;
    class Function WakeCount: Integer; override;
  public
    procedure WaitStrict; override;
    Function Wait: Boolean; override;
    Function TryWaitStrict: Boolean; override;
    Function TryWait: Boolean; override;
    Function TimedWait(Timeout: UInt32): TLSOWaitResult; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                     TEvent
--------------------------------------------------------------------------------
===============================================================================}
type
  TLSOMultiWaitSlotIndex = Int16;

  TLSOWaiterItem = packed record
    SlotIndex:  TLSOMultiWaitSlotIndex;
    WaitAll:    Boolean;
  end;

  TLSOEvent = record
    DataLock:     TFutexWord;
    WaitFutex:    TFutexWord;
    Signaled:     Boolean;
    ManualReset:  Boolean;
    WaiterCount:  Integer;
    Waiters:      packed array[0..15] of TLSOWaiterItem;
  end;
  PLSOEvent = ^TLSOEvent;

{===============================================================================
    TEvent - flat interface declaration
===============================================================================}

Function event_init(event: PLSOEvent; manual_reset, initial_state: cBool): cInt;
Function event_destroy(event: PLSOEvent): cInt;

Function event_lock(event: PLSOEvent): cInt;
Function event_unlock(event: PLSOEvent): cInt;

Function event_wait(event: PLSOEvent): cInt;
Function event_trywait(event: PLSOEvent): cInt;
Function event_timedwait(event: PLSOEvent; timeout: cUnsigned): cInt;

{===============================================================================
    TEvent - class declaration
===============================================================================}
type
  TEvent = class(TImplementorLinSyncObject)
  protected
    class Function GetLockType: TLSOLockType; override;
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
  public
    constructor Create(const Name: String; ManualReset,InitialState: Boolean); overload; virtual;
    constructor Create(ManualReset,InitialState: Boolean); overload; virtual;
    constructor Create(const Name: String); override; // ManualReset := True, InitialState := False
    constructor Create; override;
    procedure LockStrict; virtual;
    Function Lock: Boolean; virtual;
    procedure UnlockStrict; virtual;
    Function Unlock: Boolean; virtual;
    procedure WaitStrict; virtual;
    Function Wait: Boolean; virtual;
    Function TryWaitStrict: Boolean; virtual;
    Function TryWait: Boolean; virtual;
    Function TimedWait(Timeout: UInt32): TLSOWaitResult; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              TWrapperLinSynObject
--------------------------------------------------------------------------------
===============================================================================}
{
  TWrapperLinSyncObject serves as a base class for objects that are directly
  wrapping a pthread-provided synchronization primitive.
}
{===============================================================================
    TWrapperLinSynObject - class declaration
===============================================================================}
type
  TWrapperLinSyncObject = class(TLinSyncObject);

{===============================================================================
--------------------------------------------------------------------------------
                                    TSpinLock
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpinLock - class declaration
===============================================================================}
type
  TSpinLock = class(TWrapperLinSyncObject)
  protected
    class Function GetLockType: TLSOLockType; override;
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
  public
    procedure LockStrict; virtual;
    Function Lock: Boolean; virtual;
    Function TryLockStrict: Boolean; virtual;
    Function TryLock: Boolean; virtual;
    procedure UnlockStrict; virtual;
    Function Unlock: Boolean; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                     TMutex
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMutex - class declaration
===============================================================================}
type
  TMutex = class(TWrapperLinSyncObject)
  protected
    class Function GetLockType: TLSOLockType; override;
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
  public
    procedure LockStrict; virtual;
    Function Lock: Boolean; virtual;
    Function TryLockStrict: Boolean; virtual;
    Function TryLock: Boolean; virtual;
    Function TimedLock(Timeout: UInt32): TLSOWaitResult; virtual;
    procedure UnlockStrict; virtual;
    Function Unlock: Boolean; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TSemaphore
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSemaphore - class declaration
===============================================================================}
type
  TSemaphore = class(TWrapperLinSyncObject)
  protected
    fInitialValue:  cUnsigned;
    class Function GetLockType: TLSOLockType; override;
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
  public
    constructor Create(const Name: String; InitialValue: cUnsigned); overload; virtual;
    constructor Create(InitialValue: cUnsigned); overload; virtual;
    constructor Create(const Name: String); override;
    constructor Create; override;
    Function GetValueStrict: cInt; virtual;
    Function GetValue: cInt; virtual;
    procedure WaitStrict; virtual;
    Function Wait: Boolean; virtual;
    Function TryWaitStrict: Boolean; virtual;
    Function TryWait: Boolean; virtual;
    Function TimedWait(Timeout: UInt32): TLSOWaitResult; virtual;
    procedure PostStrict; virtual;
    Function Post: Boolean; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TReadWriteLock
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TReadWriteLock - class declaration
===============================================================================}
type
  TReadWriteLock = class(TWrapperLinSyncObject)
  protected
    class Function GetLockType: TLSOLockType; override;
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
  public
    procedure ReadLockStrict; virtual;
    Function ReadLock: Boolean; virtual;
    Function TryReadLockStrict: Boolean; virtual;
    Function TryReadLock: Boolean; virtual;
    Function TimedReadLock(Timeout: UInt32): TLSOWaitResult; virtual;
    procedure WriteLockStrict; virtual;
    Function WriteLock: Boolean virtual;
    Function TryWriteLockStrict: Boolean; virtual;
    Function TryWriteLock: Boolean; virtual;
    Function TimedWriteLock(Timeout: UInt32): TLSOWaitResult; virtual;
    procedure UnlockStrict; virtual;  // there is no read- or write-specific unlock
    Function Unlock: Boolean; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TConditionVariable
--------------------------------------------------------------------------------
===============================================================================}
type
  // types for autocycle
  TLSOWakeOption = (woWakeOne,woWakeAll,woWakeBeforeUnlock);
  TLSOWakeOptions = set of TLSOWakeOption;

  TLSOPredicateCheckEvent = procedure(Sender: TObject; var Predicate: Boolean) of object;
  TLSOPredicateCheckCallback = procedure(Sender: TObject; var Predicate: Boolean);

  TLSODataAccessEvent = procedure(Sender: TObject; var WakeOptions: TLSOWakeOptions) of object;
  TLSODataAccessCallback = procedure(Sender: TObject; var WakeOptions: TLSOWakeOptions);

{===============================================================================
    TConditionVariable - class declaration
===============================================================================}
type
  TConditionVariable = class(TWrapperLinSyncObject)
  protected
    // autocycle events
    fOnPredicateCheckEvent:     TLSOPredicateCheckEvent;
    fOnPredicateCheckCallback:  TLSOPredicateCheckCallback;
    fOnDataAccessEvent:         TLSODataAccessEvent;
    fOnDataAccessCallback:      TLSODataAccessCallback;
    // autocycle methods
    Function DoOnPredicateCheck: Boolean; virtual;
    Function DoOnDataAccess: TLSOWakeOptions; virtual;
    procedure SelectWake(WakeOptions: TLSOWakeOptions); virtual;
    // inherited methods
    class Function GetLockType: TLSOLockType; override;
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
  public
    procedure WaitStrict(DataLock: ppthread_mutex_t); overload; virtual;
    procedure WaitStrict(DataLock: TMutex); overload; virtual;
    Function Wait(DataLock: ppthread_mutex_t): Boolean; overload; virtual;
    Function Wait(DataLock: TMutex): Boolean; overload; virtual;
    Function TimedWait(DataLock: ppthread_mutex_t; Timeout: UInt32): TLSOWaitResult; overload; virtual;
    Function TimedWait(DataLock: TMutex; Timeout: UInt32): TLSOWaitResult; overload; virtual;
    procedure SignalStrict; virtual;
    Function Signal: Boolean; virtual;
    procedure BroadcastStrict; virtual;
    Function Broadcast: Boolean; virtual;
    procedure AutoCycle(DataLock: ppthread_mutex_t; Timeout: UInt32); overload; virtual;
    procedure AutoCycle(DataLock: TMutex; Timeout: UInt32); overload; virtual;
    procedure AutoCycle(DataLock: ppthread_mutex_t); overload; virtual;
    procedure AutoCycle(DataLock: TMutex); overload; virtual;
    // events
    property OnPredicateCheckEvent: TLSOPredicateCheckEvent read fOnPredicateCheckEvent write fOnPredicateCheckEvent;
    property OnPredicateCheckCallback: TLSOPredicateCheckCallback read fOnPredicateCheckCallback write fOnPredicateCheckCallback;
    property OnPredicateCheck: TLSOPredicateCheckEvent read fOnPredicateCheckEvent write fOnPredicateCheckEvent;
    property OnDataAccessEvent: TLSODataAccessEvent read fOnDataAccessEvent write fOnDataAccessEvent;
    property OnDataAccessCallback: TLSODataAccessCallback read fOnDataAccessCallback write fOnDataAccessCallback;
    property OnDataAccess: TLSODataAccessEvent read fOnDataAccessEvent write fOnDataAccessEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              TConditionVariableEx
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TConditionVariableEx - class declaration
===============================================================================}
type
  TConditionVariableEx = class(TConditionVariable)
  protected
    fDataLockPtr: Pointer;
    class Function GetLockType: TLSOLockType; override;
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
  public
    procedure LockStrict; virtual;
    Function Lock: Boolean; virtual;
    procedure UnlockStrict; virtual;
    Function Unlock: Boolean; virtual;
    procedure WaitStrict; overload; virtual;
    Function Wait: Boolean; overload; virtual;
    Function TimedWait(Timeout: UInt32): TLSOWaitResult; overload; virtual;
    procedure AutoCycle(Timeout: UInt32); overload; virtual;
    procedure AutoCycle; overload; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                    TBarrier
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBarrier - class declaration
===============================================================================}
type
  TBarrier = class(TWrapperLinSyncObject)
  protected
    class Function GetLockType: TLSOLockType; override;
    procedure ResolveLockPtr; override;
    procedure InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer); override;
    procedure FinalizeLock(CompleteStage: Integer = MAXINT); override;
  public
    constructor Create(const Name: String; Count: cUnsigned); overload; virtual;
    constructor Create(Count: cUnsigned); overload; virtual;
    constructor Create(const Name: String); override;
    constructor Create; override;
    procedure WaitStrict; virtual;
    Function Wait: Boolean; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 Wait functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Given the implementation, there are limitations in effect for multi-wait
  functions, those are:

    - there is a system-wide limit of 15360 concurently running instances of
      multi-wait (ie. limit of multi-waits active at any one moment)

    - single event object can be waited upon in at most 16 multi-waits at one
      time

    - the number of events in parameter Objects is not strictly limited, but it
      is not recommended to go above 32 (there is a technical limit of about 4
      bilion objects)


  When WaitAll is set to false and the function returns wrSignaled, then the
  output parameter Index will contain zero-based index of object that caused
  the function to return.

  When wrError is returned, then the index is set to one of the following error
  codes to indicate what caused the function to fail.

  In all other cases, the value of Index is undefined.
}
const
  LSO_WAITERROR_UNKNOWN   = 1;  // some unknown or unspecified error
  LSO_WAITERROR_COUNT     = 2;  // zero object count
  LSO_WAITERROR_OBJECTS   = 3;  // invalid object(s) (unassigned, duplicate, ...)
  LSO_WAITERROR_NOSLOT    = 4;  // reached limit of concurently running instances of multi-wait
  LSO_WAITERROR_EVENTFULL = 5;  // at least one event has full waiter array and cannot be waited upon
  LSO_WAITERROR_FUTEXWAIT = 6;  // error in internla waiting mechanism

//------------------------------------------------------------------------------

Function WaitForMultipleEvents(Objects: array of PLSOEvent; WaitAll: Boolean; Timeout: DWORD; out Index: Integer): TLSOWaitResult;
Function WaitForMultipleEvents(Objects: array of PLSOEvent; WaitAll: Boolean; Timeout: DWORD): TLSOWaitResult;
Function WaitForMultipleEvents(Objects: array of PLSOEvent; WaitAll: Boolean): TLSOWaitResult;

Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean; Timeout: DWORD; out Index: Integer): TLSOWaitResult;
Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean; Timeout: DWORD): TLSOWaitResult;
Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean): TLSOWaitResult;

{===============================================================================
--------------------------------------------------------------------------------
                               Utility functions
--------------------------------------------------------------------------------
===============================================================================}
{
  WaitResultToStr returns textual representation of a given wait result.
  Meant mainly for debugging.
}
Function WaitResultToStr(WaitResult: TLSOWaitResult): String;

implementation

uses
  Math, UnixType, Linux, Errors,
  InterlockedOps, BitOps;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    Error checking and management
===============================================================================}

threadvar
  ThrErrorCode: cInt;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

Function CheckResErr(ReturnedValue: cInt): Boolean;
begin
Result := ReturnedValue = 0;
If Result then
  ThrErrorCode := 0
else
  ThrErrorCode := ReturnedValue;
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

Function CheckErr(ReturnedValue: cInt): Boolean;
begin
Result := ReturnedValue = 0;
If Result then
  ThrErrorCode := 0
else
  ThrErrorCode := errno;
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

Function errno_ptr: pcInt; cdecl; external name '__errno_location';

Function CheckErrAlt(ReturnedValue: cInt): Boolean;
begin
Result := ReturnedValue = 0;
If Result then
  ThrErrorCode := 0
else
  ThrErrorCode := errno_ptr^;
end;

{===============================================================================
--------------------------------------------------------------------------------
                                TCriticalSection
--------------------------------------------------------------------------------
===============================================================================}

Function pthread_mutex_consistent(mutex: ppthread_mutex_t): cInt; cdecl; external;

Function pthread_mutexattr_getrobust(attr: ppthread_mutexattr_t; robustness: pcInt): cInt; cdecl; external;
Function pthread_mutexattr_setrobust(attr: ppthread_mutexattr_t; robustness: cInt): cInt; cdecl; external;

const
//PTHREAD_MUTEX_STALLED = 0;  // not used anywhere
  PTHREAD_MUTEX_ROBUST  = 1;

{===============================================================================
    TCriticalSection - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCriticalSection - protected methods
-------------------------------------------------------------------------------}

procedure TCriticalSection.Initialize;
var
  MutexAttr:  pthread_mutexattr_t;
begin
fLockComplete := False;
If CheckResErr(pthread_mutexattr_init(@MutexAttr)) then
  try
    If not CheckResErr(pthread_mutexattr_settype(@MutexAttr,PTHREAD_MUTEX_RECURSIVE)) then
      raise ELSOSysOpError.CreateFmt('TCriticalSection.Initialize: ' +
        'Failed to set mutex attribute TYPE (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If not CheckResErr(pthread_mutexattr_setrobust(@MutexAttr,PTHREAD_MUTEX_ROBUST)) then
      raise ELSOSysOpError.CreateFmt('TCriticalSection.Initialize: ' +
        'Failed to set mutex attribute ROBUST (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If not CheckResErr(pthread_mutex_init(@fMutex,@MutexAttr)) then
      raise ELSOSysInitError.CreateFmt('TCriticalSection.Initialize: ' +
        'Failed to initialize mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    fLockComplete := True;
  finally
    If not CheckResErr(pthread_mutexattr_destroy(@MutexAttr)) then
      raise ELSOSysFinalError.CreateFmt('TCriticalSection.Initialize: ' +
        'Failed to destroy mutex attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end
else raise ELSOSysInitError.CreateFmt('TCriticalSection.Initialize: ' +
       'Failed to initialize mutex attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.Finalize;
begin
If fLockComplete then
  If not CheckResErr(pthread_mutex_destroy(@fMutex)) then
    raise ELSOSysFinalError.CreateFmt('TCriticalSection.Finalize: ' +
      'Failed to destroy mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TCriticalSection - public methods
-------------------------------------------------------------------------------}

constructor TCriticalSection.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TCriticalSection.Destroy;
begin
Finalize;
inherited;
end;
//------------------------------------------------------------------------------

Function TCriticalSection.TryEnter: Boolean;
var
  ReturnValue:  cInt;
begin
ReturnValue := pthread_mutex_trylock(@fMutex);
If ReturnValue = ESysEOWNERDEAD then
  begin
    If not CheckResErr(pthread_mutex_consistent(@fMutex)) then
      raise ELSOSysOpError.CreateFmt('TCriticalSection.TryEnter: ' +
        'Failed to make mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Result := True;
  end
else
  begin
    Result := CheckResErr(ReturnValue);
    If not Result and (ThrErrorCode <> ESysEBUSY) then
      raise ELSOSysOpError.CreateFmt('TCriticalSection.TryEnter: ' +
        'Failed to try-lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end;
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.Enter;
var
  ReturnValue:  cInt;
begin
ReturnValue := pthread_mutex_lock(@fMutex);
If ReturnValue = ESysEOWNERDEAD then
  begin
    If not CheckResErr(pthread_mutex_consistent(@fMutex)) then
      raise ELSOSysOpError.CreateFmt('TCriticalSection.Enter: ' +
        'Failed to make mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end
else
  begin
    If not CheckResErr(ReturnValue) then
      raise ELSOSysOpError.CreateFmt('TCriticalSection.Enter: ' +
        'Failed to lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end;
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.Leave;
begin
If not CheckResErr(pthread_mutex_unlock(@fMutex)) then
  raise ELSOSysOpError.CreateFmt('TCriticalSection.Leave: ' +
    'Failed to unlock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TLinSyncObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TLinSyncObject - internals
===============================================================================}
const
  MSECS_PER_SEC  = 1000;
  NSECS_PER_SEC  = 1000000000;
  NSECS_PER_MSEC = 1000000;

//------------------------------------------------------------------------------
const
  LSO_SHARED_NAMESPACE = 'lso_shared';

type
  TLSOConditionVariable = record
    DataLock: pthread_mutex_t;
    CondVar:  pthread_cond_t;
  end;

type
{
  Note that type TLSOSharedData has differing sizes in 32bit and 64bit
  environments, and since the size is used for name of shared memory, this in
  itself separates those two worlds - so there is no need to explicitly
  separate 32bit and 64bit process-shared synchronizers.
}
  TLSOSharedData = record
    SharedUserData: TLSOSharedUserData;
    RefCount:       Int32;
    case LockType: TLSOLockType of
      ltSpinLock:         (SpinLock:    pthread_spinlock_t);
      ltStatelessEvent,
      ltSimpleManualEvent,
      ltSimpleAutoEvent:  (SimpleEvent: TLSOSimpleEvent);
      ltEvent:            (Event:       TLSOEvent);
      ltMutex:            (Mutex:       pthread_mutex_t);
      ltSemaphore:        (Semaphore:   sem_t);
      ltRWLock:           (RWLock:      pthread_rwlock_t);
      ltCondVar,
      ltCondVarEx:        (CondVar:     TLSOConditionVariable);
      ltBarrier:          (Barrier:     pthread_barrier_t);
  end;
  PLSOSharedData = ^TLSOSharedData;

var
  // used for data integrity when creating/destroying thread-shared locks
  LSO_SHAREDDATA_THREADLOCK:  TCriticalSection;

//------------------------------------------------------------------------------

procedure GetTime(out Time: TTimeSpec);
begin
If not CheckErr(clock_gettime(CLOCK_MONOTONIC,@Time)) then
  raise ELSOSysOpError.CreateFmt('GetTime: Failed to obtain current time (%d - %s).',
    [ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function RecalculateTimeout(TimeoutFull: UInt32; StartTime: TTimeSpec; out TimeoutRemaining: UInt32): Boolean;
var
  CurrentTime:    TTimeSpec;
  MillisElapsed:  Int64;
begin
{
  Result of true means the timeout did not run out, false means the timeout
  period has elapsed.
}
case TimeoutFull of
         0: TimeoutRemaining := 0;
  INFINITE: TimeoutRemaining := INFINITE;
else
  // arbitrary timeout
  GetTime(CurrentTime);
  If CurrentTime.tv_sec >= StartTime.tv_sec then // sanity check
    begin
      MillisElapsed := (((Int64(CurrentTime.tv_sec) - StartTime.tv_sec) * MSECS_PER_SEC)  +
                        ((Int64(CurrentTime.tv_nsec) - StartTime.tv_nsec) div NSECS_PER_MSEC)) and
                       (Int64(-1) shr 1);
      If MillisElapsed < Int64(TimeoutFull) then
        TimeoutRemaining := UInt32(Int64(TimeoutFull) - MillisElapsed)
      else
        TimeoutRemaining := 0;
    end
  else TimeoutRemaining := TimeoutFull;
end;
Result := TimeoutRemaining <> 0;
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure ResolveAbsoluteTimeout(Timeout: UInt32; out TimeoutSpec: timespec);
begin
If CheckErr(clock_gettime(CLOCK_MONOTONIC,@TimeoutSpec)) then
  begin
    TimeoutSpec.tv_sec := TimeoutSpec.tv_sec + time_t(Timeout div MSECS_PER_SEC);
    TimeoutSpec.tv_nsec := TimeoutSpec.tv_nsec + clong((Timeout mod MSECS_PER_SEC) * NSECS_PER_MSEC);
    while TimeoutSpec.tv_nsec >= NSECS_PER_SEC do
      begin
        TimeoutSpec.tv_nsec := TimeoutSpec.tv_nsec - NSECS_PER_SEC;
        Inc(TimeoutSpec.tv_sec);
      end;
  end
else raise ELSOSysOpError.CreateFmt('ResolveAbsoluteTimeout: Failed to obtain current time (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{===============================================================================
    TLinSyncObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TLinSyncObject - protected methods
-------------------------------------------------------------------------------}

Function TLinSyncObject.GetSharedUserDataPtr: PLSOSharedUserData;
begin
Result := Addr(PLSOSharedData(fSharedData)^.SharedUserData);
end;

//------------------------------------------------------------------------------

Function TLinSyncObject.GetSharedUserData: TLSOSharedUserData;
begin
Move(GetSharedUserDataPtr^,Addr(Result)^,SizeOf(TLSOSharedUserData));
end;

//------------------------------------------------------------------------------

procedure TLinSyncObject.SetSharedUserData(Value: TLSOSharedUserData);
begin
Move(Value,GetSharedUserDataPtr^,SizeOf(TLSOSharedUserData));
end;

//------------------------------------------------------------------------------

procedure TLinSyncObject.CheckAndSetLockType;
begin
If PLSOSharedData(fSharedData)^.LockType <> ltInvalid then
  begin
    If PLSOSharedData(fSharedData)^.LockType <> GetLockType then
      raise ELSOInvalidLockType.CreateFmt('TLinSyncObject.CheckAndSetLockType: ' +
        'Existing lock is of incompatible type (%d).',[Ord(PLSOSharedData(fSharedData)^.LockType)]);
  end
else PLSOSharedData(fSharedData)^.LockType := GetLockType;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TLinSyncObject.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
begin
CompleteStage := 0;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TLinSyncObject.Initialize(const Name: String; InitializingData: PtrUInt);
var
  CompleteStage:  Integer;
begin
CompleteStage := 0;
fLastError := 0;
fName := Name;
fProcessShared := Length(fName) > 0;
// create/open shared data
If fProcessShared then
  begin
    try
      fNamedSharedItem := TNamedSharedItem.CreateLocked(fName,SizeOf(TLSOSharedData),LSO_SHARED_NAMESPACE);
      try
        fSharedData := fNamedSharedItem.Memory;
        Inc(PLSOSharedData(fSharedData)^.RefCount);
        try
          If PLSOSharedData(fSharedData)^.RefCount <= 1 then
            begin
              // creating new object
              PLSOSharedData(fSharedData)^.RefCount := 1;
              CheckAndSetLockType;
              InitializeLock(InitializingData,CompleteStage);
            end
          // opening existing object
          else CheckAndSetLockType;
          fLockPrepComplete := True;
        except
          FinalizeLock(CompleteStage);
          Dec(PLSOSharedData(fSharedData)^.RefCount);
          raise;
        end;
      finally
        fNamedSharedItem.GlobalUnlock;
      end;
    except
      // rollback
      fSharedData := nil;
      FreeAndnil(fNamedSharedItem); // succeeds even if fNamedSharedItem is nil
      raise;
    end;
  end
else
  begin
    LSO_SHAREDDATA_THREADLOCK.Enter;
    try
      fSharedData := AllocMem(SizeOf(TLSOSharedData));
      try
        PLSOSharedData(fSharedData)^.RefCount := 1;
        CheckAndSetLockType;
        InitializeLock(InitializingData,CompleteStage);
        fLockPrepComplete := True;
      except
        FinalizeLock(CompleteStage);
        FreeMem(fSharedData);
        fSharedData := nil;
        raise;
      end;
    finally
      LSO_SHAREDDATA_THREADLOCK.Leave;
    end;
  end;
ResolveLockPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TLinSyncObject.Initialize(const Name: String);
begin
If Length(Name) > 0 then
  begin
    fLastError := 0;
    fName := Name;
    fProcessShared := True;
    try
      fNamedSharedItem := TNamedSharedItem.CreateLocked(fName,SizeOf(TLSOSharedData),LSO_SHARED_NAMESPACE);
      try
        fSharedData := fNamedSharedItem.Memory;
        Inc(PLSOSharedData(fSharedData)^.RefCount);
        try
          If PLSOSharedData(fSharedData)^.RefCount > 1 then
            begin
              CheckAndSetLockType;
              ResolveLockPtr;
              fLockPrepComplete := True;
            end
          else raise ELSOOpenError.Create('TLinSyncObject.Initialize: Cannot open uninitialized object.');
        except
          Dec(PLSOSharedData(fSharedData)^.RefCount);
          raise;
        end;
      finally
        fNamedSharedItem.GlobalUnlock;
      end;
    except
      fSharedData := nil;
      FreeAndnil(fNamedSharedItem);
      raise;
    end;
  end
else raise ELSOOpenError.Create('TLinSyncObject.Initialize: Cannot open unnamed object.');
end;

//------------------------------------------------------------------------------

procedure TLinSyncObject.Finalize;
begin
If fProcessShared then
  begin
    // following should be all true or all false, but meh...
    If Assigned(fNamedSharedItem) and Assigned(fSharedData) and fLockPrepComplete then
      begin
        fNamedSharedItem.GlobalLock;
        try
          Dec(PLSOSharedData(fSharedData)^.RefCount);
          If PLSOSharedData(fSharedData)^.RefCount <= 0 then
            begin
              FinalizeLock;
              PLSOSharedData(fSharedData)^.RefCount := 0;
            end;
        finally
          fNamedSharedItem.GlobalUnlock;
        end;
        FreeAndNil(fNamedSharedItem);
      end
  end
else
  begin
    If Assigned(fSharedData) and fLockPrepComplete then
      begin
        LSO_SHAREDDATA_THREADLOCK.Enter;
        try
          Dec(PLSOSharedData(fSharedData)^.RefCount);
          If PLSOSharedData(fSharedData)^.RefCount <= 0 then
            begin
              FinalizeLock;
              FreeMem(fSharedData,SizeOf(TLSOSharedData));
            end;
        finally
          LSO_SHAREDDATA_THREADLOCK.Leave;
        end;
      end;
  end;
fSharedData := nil;
fLockPtr := nil;
end;

{-------------------------------------------------------------------------------
    TLinSyncObject - public methods
-------------------------------------------------------------------------------}

constructor TLinSyncObject.ProtectedCreate(const Name: String; InitializingData: PtrUInt);
begin
inherited Create;
Initialize(Name,InitializingData);
end;

//------------------------------------------------------------------------------

constructor TLinSyncObject.Create(const Name: String);
begin
ProtectedCreate(Name,0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLinSyncObject.Create;
begin
ProtectedCreate('',0);
end;

//------------------------------------------------------------------------------

constructor TLinSyncObject.Open(const Name: String);
begin
inherited Create;
Initialize(Name);
end;

//------------------------------------------------------------------------------

constructor TLinSyncObject.DuplicateFrom(SourceObject: TLinSyncObject);
begin
inherited Create;
If SourceObject.GetLockType = Self.GetLockType then
  begin
    If not SourceObject.ProcessShared then
      begin
        fLastError := 0;
        fName := '';
        fProcessShared := False;
        LSO_SHAREDDATA_THREADLOCK.Enter;
        try
          fSharedData := SourceObject.fSharedData;
        {
          Increase reference counter. If it is above 1, all is good and
          continue. But if it is below or equal to 1, it means the source was
          probably (being) destroyed - raise an exception.
        }
          Inc(PLSOSharedData(fSharedData)^.RefCount);
          try
            If PLSOSharedData(fSharedData)^.RefCount > 1 then
              begin
                CheckAndSetLockType;
                ResolveLockPtr;
                fLockPrepComplete := True;
              end
            else raise ELSOInvalidObject.Create('TLinSyncObject.DuplicateFrom: ' +
                   'Source object is in an inconsistent state.');
          except
            Dec(PLSOSharedData(fSharedData)^.RefCount);
            raise;
          end;
        finally
          LSO_SHAREDDATA_THREADLOCK.Leave;
        end;
      end
    else Initialize(SourceObject.Name); // corresponds to open constructor
  end
else raise ELSOInvalidObject.CreateFmt('TLinSyncObject.DuplicateFrom: ' +
       'Incompatible source object (%s).',[SourceObject.ClassName]);
end;

//------------------------------------------------------------------------------

destructor TLinSyncObject.Destroy;
begin
Finalize;
inherited;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TStatelessEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TStatelessEvent - flat interface implementation
===============================================================================}

Function event_stateless_init(event: PLSOSimpleEvent): cInt;
begin
try
  InterlockedStore(event^.Event,0);
  Result := 0;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_stateless_destroy(event: PLSOSimpleEvent): cInt;
begin
try
  InterlockedStore(event^.Event,0);
  Result := 0;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function event_stateless_signal(event: PLSOSimpleEvent): cInt;
begin
try
  InterlockedIncrement(event^.Event); // this can overflow
  FutexWake(event^.Event,-1);         // wake everyone
  Result := 0;
except
  Result := -1;
end;
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

Function event_stateless_wait(event: PLSOSimpleEvent): cInt;
var
  InitialState: TFutexWord;
  ExitWait:     Boolean;
begin
try
  InitialState := InterlockedLoad(event^.Event);
  repeat
    ExitWait := True;
    case FutexWait(event^.Event,InitialState) of
      fwrWoken,
      fwrValue:       Result := 0;
      fwrInterrupted: ExitWait := False;  // re-enter waiting
    else
      // fwrTimeout not allowed
      Result := -1;
    end;
  until ExitWait;
except
  Result := -1
end;
end;

//------------------------------------------------------------------------------

Function event_stateless_timedwait(event: PLSOSimpleEvent; timeout: cUnsigned): cInt;
var
  StartTime:        TTimeSpec;
  TimeoutRemaining: UInt32;
  InitialState:     TFutexWord;
  ExitWait:         Boolean;
begin
try
  GetTime(StartTime);
  TimeoutRemaining := timeout;
  InitialState := InterlockedLoad(event^.Event);
  repeat
    ExitWait := True;
    case FutexWait(event^.Event,InitialState,TimeoutRemaining) of
      fwrWoken,
      fwrValue:       Result := 0;
      fwrTimeout:     Result := ESysETIMEDOUT;
      fwrInterrupted: If RecalculateTimeout(timeout,StartTime,TimeoutRemaining) then
                        ExitWait := False   // re-enter waiting with lowered timeout
                      else
                        Result := ESysETIMEDOUT;
    else
      Result := -1;
    end;
  until ExitWait;
except
  Result := -1;
end;
end;

{===============================================================================
    TStatelessEvent - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TStatelessEvent - protected methods
-------------------------------------------------------------------------------}

class Function TStatelessEvent.GetLockType: TLSOLockType;
begin
Result := ltStatelessEvent;
end;

//------------------------------------------------------------------------------

procedure TStatelessEvent.ResolveLockPtr;
begin
fLockPtr := Addr(PLSOSharedData(fSharedData)^.SimpleEvent);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TStatelessEvent.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
begin
inherited;
If not CheckResErr(event_stateless_init(Addr(PLSOSharedData(fSharedData)^.SimpleEvent.Event))) then
  raise ELSOSysInitError.CreateFmt('TStatelessEvent.InitializeLock: ' +
    'Failed to initialize stateless event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
Inc(CompleteStage);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TStatelessEvent.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 0 then
  If not CheckResErr(event_stateless_destroy(Addr(PLSOSharedData(fSharedData)^.SimpleEvent.Event))) then
    raise ELSOSysFinalError.CreateFmt('TStatelessEvent.FinalizeLock: ' +
      'Failed to destroy stateless event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TStatelessEvent - public methods
-------------------------------------------------------------------------------}

procedure TStatelessEvent.SignalStrict;
begin
If not CheckResErr(event_stateless_signal(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TStatelessEvent.SignalStrict: ' +
    'Failed to signal stateless event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TStatelessEvent.Signal: Boolean;
begin
Result := CheckResErr(event_stateless_signal(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

procedure TStatelessEvent.WaitStrict;
begin
If not CheckResErr(event_stateless_wait(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TStatelessEvent.WaitStrict: ' +
    'Failed to wait on stateless event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TStatelessEvent.Wait: Boolean;
begin
Result := CheckResErr(event_stateless_wait(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TStatelessEvent.TimedWait(Timeout: UInt32): TLSOWaitResult;
begin
If not CheckResErr(event_stateless_timedwait(fLockPtr,Timeout)) then
  begin
    If ThrErrorCode <> ESysETIMEDOUT then
      begin
        fLastError := Integer(ThrErrorCode);
        Result := wrError;
      end
    else Result := wrTimeout;
  end
else Result := wrSignaled;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleEventBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleEventBase - flat interface implementation
===============================================================================}
const
  LSO_EVENTSTATE_LOCKED   = TFutexWord(0);
  LSO_EVENTSTATE_SIGNALED = TFutexWord(1);

//------------------------------------------------------------------------------

Function _event_simplebase_init(event: PLSOSimpleEvent): cInt;
begin
try
  InterlockedStore(event^.Event,LSO_EVENTSTATE_LOCKED);
  Result := 0;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function _event_simplebase_destroy(event: PLSOSimpleEvent): cInt;
begin
try
  InterlockedStore(event^.Event,LSO_EVENTSTATE_LOCKED);
  Result := 0;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function _event_simplebase_lock(event: PLSOSimpleEvent): cInt;
begin
try
  InterlockedStore(event^.Event,LSO_EVENTSTATE_LOCKED);
  // do not wake any threads
  Result := 0;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function _event_simplebase_unlock(event: PLSOSimpleEvent; WakeCount: Integer): cInt;
begin
try
  InterlockedStore(event^.Event,LSO_EVENTSTATE_SIGNALED);
  FutexWake(event^.Event,WakeCount);
  Result := 0;
except
  Result := -1;
end;
end;

{===============================================================================
    TSimpleEventBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleEventBase - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleEventBase.ResolveLockPtr;
begin
fLockPtr := Addr(PLSOSharedData(fSharedData)^.SimpleEvent);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSimpleEventBase.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
begin
inherited;
If not CheckResErr(_event_simplebase_init(Addr(PLSOSharedData(fSharedData)^.SimpleEvent.Event))) then
  raise ELSOSysInitError.CreateFmt('TSimpleEventBase.InitializeLock: ' +
    'Failed to initialize simple event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
Inc(CompleteStage);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSimpleEventBase.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 0 then
  If not CheckResErr(_event_simplebase_destroy(Addr(PLSOSharedData(fSharedData)^.SimpleEvent.Event))) then
    raise ELSOSysFinalError.CreateFmt('TSimpleEventBase.FinalizeLock: ' +
      'Failed to destroy stateless event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TSimpleEventBase - public methods
-------------------------------------------------------------------------------}

procedure TSimpleEventBase.LockStrict;
begin
If not CheckResErr(_event_simplebase_lock(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TSimpleEventBase.LockStrict: ' +
    'Failed to lock simple event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSimpleEventBase.Lock: Boolean;
begin
Result := CheckResErr(_event_simplebase_lock(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

procedure TSimpleEventBase.UnlockStrict;
begin
If not CheckResErr(_event_simplebase_unlock(fLockPtr,WakeCount)) then
  raise ELSOSysOpError.CreateFmt('TSimpleEventBase.UnlockStrict: ' +
    'Failed to unlock simple event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSimpleEventBase.Unlock: Boolean;
begin
Result := CheckResErr(_event_simplebase_unlock(fLockPtr,WakeCount));
fLastError := Integer(ThrErrorCode);
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleManualEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleManualEvent - flat interface declaration
===============================================================================}

Function event_manual_init(event: PLSOSimpleEvent): cInt;
begin
Result := _event_simplebase_init(event);
end;

//------------------------------------------------------------------------------

Function event_manual_destroy(event: PLSOSimpleEvent): cInt;
begin
Result := _event_simplebase_destroy(event);
end;

//------------------------------------------------------------------------------

Function event_manual_lock(event: PLSOSimpleEvent): cInt;
begin
Result := _event_simplebase_lock(event);
end;

//------------------------------------------------------------------------------

Function event_manual_unlock(event: PLSOSimpleEvent): cInt;
begin
Result := _event_simplebase_unlock(event,-1);
end;

//------------------------------------------------------------------------------

Function event_manual_wait(event: PLSOSimpleEvent): cInt;
var
  ExitWait: Boolean;
begin
try
  repeat
    Result := 0;
    ExitWait := True;
    case FutexWait(event^.Event,LSO_EVENTSTATE_LOCKED) of
      fwrWoken:       ExitWait := InterlockedLoad(event^.Event) <> LSO_EVENTSTATE_LOCKED;
      fwrValue:       ; // the futex vas not locked
      fwrInterrupted: ExitWait := False;
    else
      Result := -1;
    end;
  until ExitWait;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_manual_trywait(event: PLSOSimpleEvent): cInt;
begin
try
  If InterlockedLoad(event^.Event) <> LSO_EVENTSTATE_LOCKED then
    Result := 0
  else
    Result := ESysEBUSY;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_manual_timedwait(event: PLSOSimpleEvent; timeout: cUnsigned): cInt;
var
  TimeoutRemaining: UInt32;
  StartTime:        TTimeSpec;
  ExitWait:         Boolean;
begin
try
  TimeoutRemaining := Timeout;
  GetTime(StartTime);
  repeat
    ExitWait := True;
    case FutexWait(event^.Event,LSO_EVENTSTATE_LOCKED,TimeoutRemaining) of
    {
      Futex was woken, but that does not necessarily mean it was signaled, so
      check the state.
      Note that it is possible that the futex was woken with unlocked state,
      but before it got here, the state changed back to locked.
    }
      fwrWoken:       If InterlockedLoad(event^.Event) = LSO_EVENTSTATE_LOCKED then
                        begin
                          If RecalculateTimeout(Timeout,StartTime,TimeoutRemaining) then
                            ExitWait := False
                          else
                            Result := ESysETIMEDOUT;
                        end
                      else Result := 0;
    {
      When fwrValue is returned, it means the futex contained value other than
      LSO_EVENTSTATE_LOCKED, which means it is not locked, and therefore is
      considered signaled.
    }
      fwrValue:       Result := 0;
      fwrTimeout:     Result := ESysETIMEDOUT;
      fwrInterrupted: If RecalculateTimeout(Timeout,StartTime,TimeoutRemaining) then
                        ExitWait := False
                      else
                        Result := ESysETIMEDOUT;
    else
      Result := -1;
    end;
  until ExitWait;
except
  Result := -1;
end;
end;

{===============================================================================
    TSimpleManualEvent - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleManualEvent - protected methods
-------------------------------------------------------------------------------}

class Function TSimpleManualEvent.GetLockType: TLSOLockType;
begin
Result := ltSimpleManualEvent;
end;

//------------------------------------------------------------------------------

class Function TSimpleManualEvent.WakeCount: Integer;
begin
Result := -1;
end;

{-------------------------------------------------------------------------------
    TSimpleManualEvent - public methods
-------------------------------------------------------------------------------}

procedure TSimpleManualEvent.WaitStrict;
begin
If not CheckResErr(event_manual_wait(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TSimpleManualEvent.WaitStrict: ' +
    'Failed to wait on event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSimpleManualEvent.Wait: Boolean;
begin
Result := CheckResErr(event_manual_wait(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TSimpleManualEvent.TryWaitStrict: Boolean;
begin
Result := CheckResErr(event_manual_trywait(fLockPtr));
If not Result and (ThrErrorCode <> ESysEBUSY) then
  raise ELSOSysOpError.CreateFmt('TSimpleManualEvent.TryWaitStrict: ' +
    'Failed to try-wait on event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSimpleManualEvent.TryWait: Boolean;
begin
Result := CheckResErr(event_manual_trywait(fLockPtr));
If not Result and (ThrErrorCode = ESysEBUSY) then
  fLastError := 0
else
  fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TSimpleManualEvent.TimedWait(Timeout: UInt32): TLSOWaitResult;
begin
If not CheckResErr(event_manual_timedwait(fLockPtr,Timeout)) then
  begin
    If ThrErrorCode <> ESysETIMEDOUT then
      begin
        Result := wrError;
        fLastError := Integer(ThrErrorCode);
      end
    else Result := wrTimeout;
  end
else Result := wrSignaled;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleAutoEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleAutoEvent - flat interface implementation
===============================================================================}

Function event_auto_init(event: PLSOSimpleEvent): cInt;
begin
Result := _event_simplebase_init(event);
end;

//------------------------------------------------------------------------------

Function event_auto_destroy(event: PLSOSimpleEvent): cInt;
begin
Result := _event_simplebase_destroy(event);
end;

//------------------------------------------------------------------------------

Function event_auto_lock(event: PLSOSimpleEvent): cInt;
begin
Result := _event_simplebase_lock(event);
end;

//------------------------------------------------------------------------------

Function event_auto_unlock(event: PLSOSimpleEvent): cInt;
begin
Result := _event_simplebase_unlock(event,1);
end;

//------------------------------------------------------------------------------

Function event_auto_wait(event: PLSOSimpleEvent): cInt;
var
  ExitWait: Boolean;
begin
try
  repeat
    Result := 0;
    ExitWait := True;
    case FutexWait(event^.Event,LSO_EVENTSTATE_LOCKED) of
      fwrWoken,
      fwrValue:       ExitWait := InterlockedExchange(event^.Event,LSO_EVENTSTATE_LOCKED) <> LSO_EVENTSTATE_LOCKED;
      fwrInterrupted: ExitWait := False;
    else
      Result := -1;
    end;
  until ExitWait;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_auto_trywait(event: PLSOSimpleEvent): cInt;
begin
try
  If InterlockedExchange(event^.Event,LSO_EVENTSTATE_LOCKED) <> LSO_EVENTSTATE_LOCKED then
    Result := 0
  else
    Result := ESysEBUSY;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_auto_timedwait(event: PLSOSimpleEvent; timeout: cUnsigned): cInt;
var
  TimeoutRemaining: UInt32;
  StartTime:        TTimeSpec;
  ExitWait:         Boolean;
begin
try
  TimeoutRemaining := Timeout;
  GetTime(StartTime);
  repeat
    ExitWait := True;
    case FutexWait(event^.Event,LSO_EVENTSTATE_LOCKED,TimeoutRemaining) of
      fwrWoken,
      fwrValue:       If InterlockedExchange(event^.Event,LSO_EVENTSTATE_LOCKED) = LSO_EVENTSTATE_LOCKED then
                        begin
                          If RecalculateTimeout(Timeout,StartTime,TimeoutRemaining) then
                            ExitWait := False
                          else
                            Result := ESysETIMEDOUT;
                        end
                      else Result := 0;
      fwrTimeout:     Result := ESysETIMEDOUT;
      fwrInterrupted: If RecalculateTimeout(Timeout,StartTime,TimeoutRemaining) then
                        ExitWait := False
                      else
                        Result := ESysETIMEDOUT;
    else
      Result := -1;
    end;
  until ExitWait;
except
  Result := -1;
end;
end;

{===============================================================================
    TSimpleAutoEvent - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleAutoEvent - protected methods
-------------------------------------------------------------------------------}

class Function TSimpleAutoEvent.GetLockType: TLSOLockType;
begin
Result := ltSimpleAutoEvent;
end;

//------------------------------------------------------------------------------

class Function TSimpleAutoEvent.WakeCount: Integer;
begin
Result := 1;
end;

{-------------------------------------------------------------------------------
    TSimpleAutoEvent - public methods
-------------------------------------------------------------------------------}

procedure TSimpleAutoEvent.WaitStrict;
begin
If not CheckResErr(event_auto_wait(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TSimpleAutoEvent.WaitStrict: ' +
    'Failed to wait on event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSimpleAutoEvent.Wait: Boolean;
begin
Result := CheckResErr(event_auto_wait(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TSimpleAutoEvent.TryWaitStrict: Boolean;
begin
Result := CheckResErr(event_auto_trywait(fLockPtr));
If not Result and (ThrErrorCode <> ESysEBUSY) then
  raise ELSOSysOpError.CreateFmt('TryWaitStrict.TryWaitStrict: ' +
    'Failed to try-wait on event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSimpleAutoEvent.TryWait: Boolean;
begin
Result := CheckResErr(event_auto_trywait(fLockPtr));
If not Result and (ThrErrorCode = ESysEBUSY) then
  fLastError := 0
else
  fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TSimpleAutoEvent.TimedWait(Timeout: UInt32): TLSOWaitResult;
begin
If not CheckResErr(event_auto_timedwait(fLockPtr,Timeout)) then
  begin
    If ThrErrorCode <> ESysETIMEDOUT then
      begin
        Result := wrError;
        fLastError := Integer(ThrErrorCode);
      end
    else Result := wrTimeout;
  end
else Result := wrSignaled;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TLSOMultiWaitSlots
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TLSOMultiWaitSlots - class declaration
===============================================================================}
type
  TLSOMultiWaitSlots = class(TSharedMemory)
  protected
    fSlotCount:   Integer;
    fSlotMap:     TBitVectorStatic32;
    fSlotMemory:  Pointer;
    Function GetSlot(SlotIndex: TLSOMultiWaitSlotIndex): PFutexWord; virtual;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    constructor Create;
    // do not create destructor, Finalize is called from inherited destructor
    Function GetFreeSlotIndex(out SlotIndex: TLSOMultiWaitSlotIndex): Boolean; virtual;
    procedure InvalidateSlot(SlotIndex: TLSOMultiWaitSlotIndex); virtual;
    Function CheckIndex(SlotIndex: TLSOMultiWaitSlotIndex): Boolean; virtual;
    property Count: Integer read fSlotCount;
    property SlotPtrs[SlotIndex: TLSOMultiWaitSlotIndex]: PFutexWord read GetSlot; default;
  end;

{===============================================================================
    TLSOMultiWaitSlots - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TLSOMultiWaitSlots - protected methods
-------------------------------------------------------------------------------}

Function TLSOMultiWaitSlots.GetSlot(SlotIndex: TLSOMultiWaitSlotIndex): PFutexWord;
begin
If CheckIndex(SlotIndex) then
  Result := PFutexWord(PtrAdvance(fSlotMemory,SlotIndex,SizeOf(TFutexWord)))
else
  Result := nil;
end;

//------------------------------------------------------------------------------

procedure TLSOMultiWaitSlots.Initialize;
begin
inherited;
fSlotCount := 15 * 1024;  // 15360, *4 = 61440 bytes
fSlotMap := TBitVectorStatic32.Create(fMemory,fSlotCount);
{
  Slot map is 1920 (15360 / 8) bytes long, so we shift the slots beyond this
  offset - 2048 bytes is a good number.
}
fSlotMemory := PtrAdvance(fMemory,2048);
end;

//------------------------------------------------------------------------------

procedure TLSOMultiWaitSlots.Finalize;
begin
fSlotMemory := nil;
fSlotMap.Free;
inherited;
end;

{-------------------------------------------------------------------------------
    TLSOMultiWaitSlots - public methods
-------------------------------------------------------------------------------}

constructor TLSOMultiWaitSlots.Create;
begin
{
  65536 bytes, 64KiB - large enough to fit 15k futexes and a slot map of the
  same length.
}
inherited Create(64 * 1024,'lso_multiwaitslots');
end;

//------------------------------------------------------------------------------

Function TLSOMultiWaitSlots.GetFreeSlotIndex(out SlotIndex: TLSOMultiWaitSlotIndex): Boolean;
begin
Lock;
try
  SlotIndex := TLSOMultiWaitSlotIndex(fSlotMap.FirstClean);
  fSlotMap[SlotIndex] := True;
  Result := CheckIndex(SlotIndex);
finally
  Unlock;
end;
end;

//------------------------------------------------------------------------------

procedure TLSOMultiWaitSlots.InvalidateSlot(SlotIndex: TLSOMultiWaitSlotIndex);
begin
Lock;
try
  If CheckIndex(SlotIndex) then
    begin
      fSlotMap[SlotIndex] := False;
      PFutexWord(PtrAdvance(fSlotMemory,SlotIndex,SizeOf(TFutexWord)))^ := 0;
    end;
finally
  Unlock;
end;
end;

//------------------------------------------------------------------------------

Function TLSOMultiWaitSlots.CheckIndex(SlotIndex: TLSOMultiWaitSlotIndex): Boolean;
begin
Result := (SlotIndex >= 0) and (SlotIndex < fSlotCount);
end;

{===============================================================================
    TLSOMultiWaitSlots - global code
===============================================================================}
var
  MultiWaitSlots: TLSOMultiWaitSlots = nil;

//------------------------------------------------------------------------------

procedure InitMultiWaitSlots;
begin
MultiWaitSlots := TLSOMultiWaitSlots.Create;
end;

//------------------------------------------------------------------------------

procedure FinalMultiWaitSlots;
begin
FreeAndNil(MultiWaitSlots);
end;

//------------------------------------------------------------------------------

Function GetMultiWaitSlot(SlotIndex: TLSOMultiWaitSlotIndex): PFutexWord;
begin
Result := MultiWaitSlots[SlotIndex];
end;

{===============================================================================
--------------------------------------------------------------------------------
                                     TEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TEvent - flat interface implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TEvent - flat interface private functions
-------------------------------------------------------------------------------}

procedure _event_lockdata(event: PLSOEvent);
begin
SimpleMutexLock(event^.DataLock);
end;

//------------------------------------------------------------------------------

procedure _event_unlockdata(event: PLSOEvent);
begin
SimpleMutexUnlock(event^.DataLock);
end;

//------------------------------------------------------------------------------

Function _event_addwaiter(event: PLSOEvent; SlotIndex: TLSOMultiWaitSlotIndex; WaitAll: Boolean): Boolean;
begin
Result := False;
_event_lockdata(event);
try
  If event^.WaiterCount < Length(event^.Waiters) then
    begin
      event^.Waiters[event^.WaiterCount].SlotIndex := SlotIndex;
      event^.Waiters[event^.WaiterCount].WaitAll := WaitAll;
      If event^.Signaled then
        InterlockedDecrementIfPositive(GetMultiWaitSlot(event^.Waiters[event^.WaiterCount].SlotIndex)^);
      Inc(event^.WaiterCount);
      Result := True;
    end;
finally
  _event_unlockdata(event);
end;
end;

//------------------------------------------------------------------------------

Function _event_removewaiter(event: PLSOEvent; SlotIndex: TLSOMultiWaitSlotIndex): Boolean;
var
  i:      Integer;
  Index:  Integer;
begin
Result := False;
_event_lockdata(event);
try
  // find the futex
  Index := -1;
  For i := Low(event^.Waiters) to Pred(event^.WaiterCount) do
    If event^.Waiters[i].SlotIndex = SlotIndex then
      begin
        Index := i;
        Break{For i};
      end;
  // now remove it
  If Index >= 0 then
    begin
      For i := Index to (event^.WaiterCount - 2) do
        event^.Waiters[i] := event^.Waiters[i + 1];
      Dec(event^.WaiterCount);
      Result := True;
    end;
finally
  _event_unlockdata(event);
end;
end;

//------------------------------------------------------------------------------

procedure _event_gotlocked(event: PLSOEvent);
var
  i:  Integer;
begin
// data are expected to be locked by now
For i := Low(event^.Waiters) to Pred(event^.WaiterCount) do
  InterlockedIncrement(GetMultiWaitSlot(event^.Waiters[i].SlotIndex)^);
// do not wake the waiters
end;

//------------------------------------------------------------------------------

procedure _event_gotunlocked(event: PLSOEvent);
var
  i:  Integer;
begin
// data are expected to be locked by now
For i := Low(event^.Waiters) to Pred(event^.WaiterCount) do
  begin
    If event^.Waiters[i].WaitAll then
      begin
        If InterlockedDecrementIfPositive(GetMultiWaitSlot(event^.Waiters[i].SlotIndex)^) <= 1 then
          FutexWake(GetMultiWaitSlot(event^.Waiters[i].SlotIndex)^,1);
      end
    else
      begin
        InterlockedDecrementIfPositive(GetMultiWaitSlot(event^.Waiters[i].SlotIndex)^);
        FutexWake(GetMultiWaitSlot(event^.Waiters[i].SlotIndex)^,1);
      end;
  end;
end;

{-------------------------------------------------------------------------------
    TEvent - flat interface public functions
-------------------------------------------------------------------------------}

Function event_init(event: PLSOEvent; manual_reset, initial_state: cBool): cInt;
begin
try
  FillChar(event^,SizeOf(TLSOEvent),0);
  SimpleMutexInit(event^.DataLock);
  _event_lockdata(event);
  try
    event^.ManualReset := manual_reset;
    event^.Signaled := initial_state;
  finally
    _event_unlockdata(event);
  end;
  Result := 0;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_destroy(event: PLSOEvent): cInt;
begin
try
  FillChar(event^,SizeOf(TLSOEvent),0);
  Result := 0;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_lock(event: PLSOEvent): cInt;
var
  OrigState:  Boolean;
begin
try
  _event_lockdata(event);
  try
    OrigState := event^.Signaled;
    event^.Signaled := False;
    InterlockedStore(event^.WaitFutex,LSO_EVENTSTATE_LOCKED);
    If OrigState then
      _event_gotlocked(event);
    Result := 0;
  finally
    _event_unlockdata(event);
  end;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_unlock(event: PLSOEvent): cInt;
var
  OrigState:  Boolean;
begin
try
  _event_lockdata(event);
  try
    OrigState := event^.Signaled;
    event^.Signaled := True;
    InterlockedStore(event^.WaitFutex,LSO_EVENTSTATE_SIGNALED);
    If not OrigState then
      _event_gotunlocked(event);
  {
    All waitings (FutexWait) are immediately followed by a data lock. So if we
    wake any thread in here, it will just run into locked data lock.
    To prevent this, we requeue thread(s) waiting on this event to wait on the
    data lock. As soon as the data lock is unlocked here, one requeued waiter
    will be woken - it will exit the FutexWait call and will try to acquire the
    unlocked data lock.
  }
    If FutexCmpRequeue(event^.WaitFutex,LSO_EVENTSTATE_SIGNALED,event^.DataLock,0,IfThen(event^.ManualReset,-1,1)) >= 0 then
      Result := 0
    else
      Result := -1;
  finally
    _event_unlockdata(event); // this will wake exactly one requeued waiter (if there is any)
  end;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_wait(event: PLSOEvent): cInt;
var
  ExitWait:   Boolean;
  WaitResult: TFutexWaitResult;
begin
try
  _event_lockdata(event);
  try
    repeat
      ExitWait := True;
      If not event^.Signaled then
        begin
          _event_unlockdata(event);
          try
            // do not use FutexWaitNoInt (due to requeueing)
            WaitResult := FutexWait(event^.WaitFutex,LSO_EVENTSTATE_LOCKED);
          finally
            _event_lockdata(event);
          end;
          case WaitResult of
            fwrWoken,
            fwrValue:       ;                 // do nothing (continues with check of Signaled field)
            fwrInterrupted: Continue{repeat}; // just re-enter waiting
          else
            Result := -1;
            Break{repeat};
          end;
        end;
      If event^.Signaled then
        begin
          If not event^.ManualReset then
            begin
              event^.Signaled := False;
              InterlockedStore(event^.WaitFutex,LSO_EVENTSTATE_LOCKED);
              _event_gotlocked(event);
            end;
          Result := 0;
        end
      else ExitWait := False;
    until ExitWait;
  finally
    _event_unlockdata(event);
  end;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_trywait(event: PLSOEvent): cInt;
begin
try
  _event_lockdata(event);
  try
    If event^.Signaled then
      begin
        If not event^.ManualReset then
          begin
            event^.Signaled := False;
            InterlockedStore(event^.WaitFutex,LSO_EVENTSTATE_LOCKED);
            _event_gotlocked(event);
          end;
        Result := 0
      end
    else Result := ESysEBUSY;
  finally
    _event_unlockdata(event);
  end;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function event_timedwait(event: PLSOEvent; timeout: cUnsigned): cInt;
var
  TimeoutRemaining: UInt32;
  StartTime:        TTimeSpec;
  ExitWait:         Boolean;
  WaitResult:       TFutexWaitResult;
begin
try
  _event_lockdata(event);
  try
    TimeoutRemaining := Timeout;
    GetTime(StartTime);
    repeat
      Result := -1;
      ExitWait := True;
      If not event^.Signaled then
        begin
          _event_unlockdata(event);
          try
            WaitResult := FutexWait(event^.WaitFutex,LSO_EVENTSTATE_LOCKED,TimeoutRemaining);
          finally
            _event_lockdata(event);
          end;
          case WaitResult of
            fwrWoken,
            fwrValue:       ; // do nothing
            fwrTimeout:     Result := ESysETIMEDOUT;
            fwrInterrupted: If RecalculateTimeout(Timeout,StartTime,TimeoutRemaining) then
                              Continue{repeat}
                            else
                              Result := ESysETIMEDOUT;
          else
            Break{repeat};
          end;
        end;
      If event^.Signaled then
        begin
          If not event^.ManualReset then
            begin
              event^.Signaled := False;
              InterlockedStore(event^.WaitFutex,LSO_EVENTSTATE_LOCKED);
              _event_gotlocked(event);
            end;
          Result := 0;
        end
      else If Result <> ESysETIMEDOUT then
        begin
          If RecalculateTimeout(Timeout,StartTime,TimeoutRemaining) then
            ExitWait := False
          else
            Result := ESysETIMEDOUT;
        end;
    until ExitWait
  finally
    _event_unlockdata(event);
  end;
except
  Result := -1;
end;
end;

{===============================================================================
    TEvent - class implementation
===============================================================================}
const
  LSO_EVENT_IDB_STATE    = 0;
  LSO_EVENT_IDB_MANRESET = 1;

{-------------------------------------------------------------------------------
    TEvent - protected methods
-------------------------------------------------------------------------------}

class Function TEvent.GetLockType: TLSOLockType;
begin
Result := ltEvent;
end;

//------------------------------------------------------------------------------

procedure TEvent.ResolveLockPtr;
begin
fLockPtr := Addr(PLSOSharedData(fSharedData)^.Event);
end;

//------------------------------------------------------------------------------

procedure TEvent.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
begin
inherited;
If not CheckResErr(event_init(Addr(PLSOSharedData(fSharedData)^.Event),
  BT(InitializingData,LSO_EVENT_IDB_MANRESET),BT(InitializingData,LSO_EVENT_IDB_STATE))) then
  raise ELSOSysInitError.CreateFmt('TEvent.InitializeLock: ' +
    'Failed to initialize event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
Inc(CompleteStage);
end;

//------------------------------------------------------------------------------

procedure TEvent.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 0 then
  If not CheckResErr(event_destroy(Addr(PLSOSharedData(fSharedData)^.Event))) then
    raise ELSOSysFinalError.CreateFmt('TEvent.FinalizeLock: ' +
      'Failed to destroy event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TEvent - public methods
-------------------------------------------------------------------------------}

constructor TEvent.Create(const Name: String; ManualReset,InitialState: Boolean);
var
  InitData: PtrUInt;
begin
InitData := 0;
BitSetTo(InitData,LSO_EVENT_IDB_STATE,InitialState);
BitSetTo(InitData,LSO_EVENT_IDB_MANRESET,ManualReset);
ProtectedCreate(Name,InitData);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create(ManualReset,InitialState: Boolean);
var
  InitData: PtrUInt;
begin
InitData := 0;
BitSetTo(InitData,LSO_EVENT_IDB_STATE,InitialState);
BitSetTo(InitData,LSO_EVENT_IDB_MANRESET,ManualReset);
ProtectedCreate('',InitData);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create(const Name: String);
var
  InitData: PtrUInt;
begin
InitData := 0;
BTR(InitData,LSO_EVENT_IDB_STATE);
BTS(InitData,LSO_EVENT_IDB_MANRESET);
ProtectedCreate(Name,InitData);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create;
var
  InitData: PtrUInt;
begin
InitData := 0;
BTR(InitData,LSO_EVENT_IDB_STATE);
BTS(InitData,LSO_EVENT_IDB_MANRESET);
ProtectedCreate('',InitData);
end;

//------------------------------------------------------------------------------

procedure TEvent.LockStrict;
begin
If not CheckResErr(event_lock(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TEvent.LockStrict: ' +
    'Failed to lock event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TEvent.Lock: Boolean;
begin
Result := CheckResErr(event_lock(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

procedure TEvent.UnlockStrict;
begin
If not CheckResErr(event_unlock(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TEvent.UnlockStrict: ' +
    'Failed to unlock event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TEvent.Unlock: Boolean;
begin
Result := CheckResErr(event_unlock(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

procedure TEvent.WaitStrict;
begin
If not CheckResErr(event_wait(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TEvent.WaitStrict: ' +
    'Failed to wait on event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TEvent.Wait: Boolean;
begin
Result := CheckResErr(event_wait(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TEvent.TryWaitStrict: Boolean;
begin
Result := CheckResErr(event_trywait(fLockPtr));
If not Result and (ThrErrorCode <> ESysEBUSY) then
  raise ELSOSysOpError.CreateFmt('TEvent.TryWaitStrict: ' +
    'Failed to try-wait on event (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TEvent.TryWait: Boolean;
begin
Result := CheckResErr(event_trywait(fLockPtr));
If not Result and (ThrErrorCode = ESysEBUSY) then
  fLastError := 0
else
  fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TEvent.TimedWait(Timeout: UInt32): TLSOWaitResult;
begin
If not CheckResErr(event_timedwait(fLockPtr,Timeout)) then
  begin
    If ThrErrorCode <> ESysETIMEDOUT then
      begin
        Result := wrError;
        fLastError := Integer(ThrErrorCode);
      end
    else Result := wrTimeout;
  end
else Result := wrSignaled;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TSpinLock
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpinLock - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpinLock - protected methods
-------------------------------------------------------------------------------}

class Function TSpinLock.GetLockType: TLSOLockType;
begin
Result := ltSpinLock;
end;

//------------------------------------------------------------------------------

procedure TSpinLock.ResolveLockPtr;
begin
fLockPtr := Addr(PLSOSharedData(fSharedData)^.SpinLock);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSpinLock.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
var
  ProcShared: cInt;
begin
inherited;
If fProcessShared then
  ProcShared := PTHREAD_PROCESS_SHARED
else
  ProcShared := PTHREAD_PROCESS_PRIVATE;
If not CheckResErr(pthread_spin_init(Addr(PLSOSharedData(fSharedData)^.SpinLock),ProcShared)) then
  raise ELSOSysInitError.CreateFmt('TSpinLock.InitializeLock: ' +
    'Failed to initialize spinlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
Inc(CompleteStage);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSpinLock.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 0 then
  If not CheckResErr(pthread_spin_destroy(Addr(PLSOSharedData(fSharedData)^.SpinLock))) then
    raise ELSOSysFinalError.CreateFmt('TSpinLock.FinalizeLock: ' +
      'Failed to destroy spinlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TSpinLock - public methods
-------------------------------------------------------------------------------}

procedure TSpinLock.LockStrict;
begin
If not CheckResErr(pthread_spin_lock(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TSpinLock.LockStrict: ' +
    'Failed to lock spinlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSpinLock.Lock: Boolean;
begin
Result := CheckResErr(pthread_spin_lock(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TSpinLock.TryLockStrict: Boolean;
begin
Result := CheckResErr(pthread_spin_trylock(fLockPtr));
If not Result and (ThrErrorCode <> ESysEBUSY) then
  raise ELSOSysOpError.CreateFmt('TSpinLock.TryLockStrict: ' +
    'Failed to try-lock spinlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSpinLock.TryLock: Boolean;
begin
Result := CheckResErr(pthread_spin_trylock(fLockPtr));
If not Result and (ThrErrorCode = ESysEBUSY) then
  fLastError := 0
else
  fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

procedure TSpinLock.UnlockStrict;
begin
If not CheckResErr(pthread_spin_unlock(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TSpinLock.UnlockStrict: ' +
    'Failed to unlock spinlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSpinLock.Unlock: Boolean;
begin
Result := CheckResErr(pthread_spin_unlock(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                     TMutex
--------------------------------------------------------------------------------
===============================================================================}

Function pthread_mutex_timedlock(mutex: ppthread_mutex_t; abstime: ptimespec): cInt; cdecl; external;

{===============================================================================
    TMutex - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMutex - protected methods
-------------------------------------------------------------------------------}

class Function TMutex.GetLockType: TLSOLockType;
begin
Result := ltMutex;
end;

//------------------------------------------------------------------------------

procedure TMutex.ResolveLockPtr;
begin
fLockPtr := Addr(PLSOSharedData(fSharedData)^.Mutex);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMutex.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
var
  MutexAttr:  pthread_mutexattr_t;
begin
inherited;
If CheckResErr(pthread_mutexattr_init(@MutexAttr)) then
  try
    If not CheckResErr(pthread_mutexattr_settype(@MutexAttr,PTHREAD_MUTEX_RECURSIVE)) then
      raise ELSOSysOpError.CreateFmt('TMutex.InitializeLock: ' +
        'Failed to set mutex attribute TYPE (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If not CheckResErr(pthread_mutexattr_setrobust(@MutexAttr,PTHREAD_MUTEX_ROBUST)) then
      raise ELSOSysOpError.CreateFmt('TMutex.InitializeLock: ' +
        'Failed to set mutex attribute ROBUST (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If fProcessShared then
      If not CheckResErr(pthread_mutexattr_setpshared(@MutexAttr,PTHREAD_PROCESS_SHARED)) then
        raise ELSOSysOpError.CreateFmt('TMutex.InitializeLock: ' +
          'Failed to set mutex attribute PSHARED (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If not CheckResErr(pthread_mutex_init(Addr(PLSOSharedData(fSharedData)^.Mutex),@MutexAttr)) then
      raise ELSOSysInitError.CreateFmt('TMutex.InitializeLock: ' +
        'Failed to initialize mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Inc(CompleteStage);
  finally
    If not CheckResErr(pthread_mutexattr_destroy(@MutexAttr)) then
      raise ELSOSysFinalError.CreateFmt('TMutex.InitializeLock: ' +
        'Failed to destroy mutex attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end
else raise ELSOSysInitError.CreateFmt('TMutex.InitializeLock: ' +
       'Failed to initialize mutex attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TMutex.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 0 then
  If not CheckResErr(pthread_mutex_destroy(Addr(PLSOSharedData(fSharedData)^.Mutex))) then
    raise ELSOSysFinalError.CreateFmt('TMutex.FinalizeLock: ' +
      'Failed to destroy mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TMutex - public methods
-------------------------------------------------------------------------------}

procedure TMutex.LockStrict;
var
  ReturnValue:  cInt;
begin
ReturnValue := pthread_mutex_lock(fLockPtr);
If ReturnValue = ESysEOWNERDEAD then
  begin
    // we are now owning the mutex, but must make it consistent again
    If not CheckResErr(pthread_mutex_consistent(fLockPtr)) then
      raise ELSOSysOpError.CreateFmt('TMutex.LockStrict: ' +
        'Failed to make mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end
else
  begin
    If not CheckResErr(ReturnValue) then
      raise ELSOSysOpError.CreateFmt('TMutex.LockStrict: ' +
        'Failed to lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end;
end;

//------------------------------------------------------------------------------

Function TMutex.Lock: Boolean;
var
  ReturnValue:  cInt;
begin
ReturnValue := pthread_mutex_lock(fLockPtr);
If ReturnValue = ESysEOWNERDEAD then
  begin
    If not CheckResErr(pthread_mutex_consistent(fLockPtr)) then
      raise ELSOSysOpError.CreateFmt('TMutex.Lock: ' +
        'Failed to make mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Result := True;
    fLastError := 0;
  end
else
  begin
    Result := CheckResErr(ReturnValue);
    fLastError := Integer(ThrErrorCode);
  end;
end;

//------------------------------------------------------------------------------

Function TMutex.TryLockStrict: Boolean;
var
  ReturnValue:  cInt;
begin
ReturnValue := pthread_mutex_trylock(fLockPtr);
If ReturnValue = ESysEOWNERDEAD then
  begin
    If not CheckResErr(pthread_mutex_consistent(fLockPtr)) then
      raise ELSOSysOpError.CreateFmt('TMutex.TryLockStrict: ' +
        'Failed to make mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Result := True;
  end
else
  begin
    Result := CheckResErr(ReturnValue);
    If not Result and (ThrErrorCode <> ESysEBUSY) then
      raise ELSOSysOpError.CreateFmt('TMutex.TryLockStrict: ' +
        'Failed to try-lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end;
end;

//------------------------------------------------------------------------------

Function TMutex.TryLock: Boolean;
var
  ReturnValue:  cInt;
begin
ReturnValue := pthread_mutex_trylock(fLockPtr);
If ReturnValue = ESysEOWNERDEAD then
  begin
    If not CheckResErr(pthread_mutex_consistent(fLockPtr)) then
      raise ELSOSysOpError.CreateFmt('TMutex.TryLock: ' +
        'Failed to make mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Result := True;
    fLastError := 0;
  end
else
  begin
    Result := CheckResErr(ReturnValue);
    If not Result and (ThrErrorCode = ESysEBUSY) then
      fLastError := 0
    else
      fLastError := Integer(ThrErrorCode);
  end;
end;

//------------------------------------------------------------------------------

Function TMutex.TimedLock(Timeout: UInt32): TLSOWaitResult;
var
  TimeoutSpec:  timespec;
  ReturnValue:  cInt;
begin
try
  If Timeout <> INFINITE then
    begin
      ResolveAbsoluteTimeout(Timeout,TimeoutSpec);
      ReturnValue := pthread_mutex_timedlock(fLockPtr,@TimeoutSpec);
      If ReturnValue = ESysEOWNERDEAD then
        begin
          If not CheckResErr(pthread_mutex_consistent(fLockPtr)) then
            raise ELSOSysOpError.CreateFmt('TMutex.TimedLock: ' +
              'Failed to make mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
          Result := wrAbandoned;
        end
      else If not CheckResErr(ReturnValue) then
        begin
          If ThrErrorCode <> ESysETIMEDOUT then
            begin
              Result := wrError;
              fLastError := Integer(ThrErrorCode);
            end
          else Result := wrTimeout;
        end
      else Result := wrSignaled;
    end
  else
    begin
      If Lock then
        Result := wrSignaled
      else
        Result := wrError;
    end;
except
  Result := wrError;
end;
end;

//------------------------------------------------------------------------------

procedure TMutex.UnlockStrict;
begin
If not CheckResErr(pthread_mutex_unlock(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TMutex.UnlockStrict: ' +
    'Failed to unlock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TMutex.Unlock: Boolean;
begin
Result := CheckResErr(pthread_mutex_unlock(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TSemaphore
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSemaphore - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSemaphore - protected methods
-------------------------------------------------------------------------------}

class Function TSemaphore.GetLockType: TLSOLockType;
begin
Result := ltSemaphore;
end;

//------------------------------------------------------------------------------

procedure TSemaphore.ResolveLockPtr;
begin
fLockPtr := Addr(PLSOSharedData(fSharedData)^.Semaphore);
end;

//------------------------------------------------------------------------------

procedure TSemaphore.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
begin
inherited;
If not CheckErrAlt(sem_init(Addr(PLSOSharedData(fSharedData)^.Semaphore),Ord(fProcessShared),cUnsigned(InitializingData))) then
  raise ELSOSysInitError.CreateFmt('TSemaphore.InitializeLock: ' +
    'Failed to initialize semaphore (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
Inc(CompleteStage);
end;

//------------------------------------------------------------------------------

procedure TSemaphore.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 0 then
  If not CheckErrAlt(sem_destroy(Addr(PLSOSharedData(fSharedData)^.Semaphore))) then
    raise ELSOSysFinalError.CreateFmt('TSemaphore.FinalizeLock: ' +
      'Failed to destroy semaphore (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TSemaphore - public methods
-------------------------------------------------------------------------------}

constructor TSemaphore.Create(const Name: String; InitialValue: cUnsigned);
begin
ProtectedCreate(Name,PtrUInt(InitialValue));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSemaphore.Create(InitialValue: cUnsigned);
begin
ProtectedCreate('',PtrUInt(InitialValue))
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSemaphore.Create(const Name: String);
begin
ProtectedCreate(Name,0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSemaphore.Create;
begin
ProtectedCreate('',0);
end;

//------------------------------------------------------------------------------

Function TSemaphore.GetValueStrict: cInt;
begin
If not CheckErrAlt(sem_getvalue(fLockPtr,@Result)) then
  raise ELSOSysOpError.CreateFmt('TSemaphore.GetValueStrict: ' +
    'Failed to get semaphore value (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSemaphore.GetValue: cInt;
begin
If not CheckErrAlt(sem_getvalue(fLockPtr,@Result)) then
  begin
    fLastError := ThrErrorCode;
    Result := -1;
  end;
end;

//------------------------------------------------------------------------------

procedure TSemaphore.WaitStrict;
var
  ExitWait: Boolean;
begin
repeat
  ExitWait := True;
  If not CheckErrAlt(sem_wait(fLockPtr)) then
    begin
      If ThrErrorCode = ESysEINTR then
        ExitWait := False
      else
        raise ELSOSysOpError.CreateFmt('TSemaphore.WaitStrict: ' +
          'Failed to wait on semaphore (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    end;
until ExitWait;
end;

//------------------------------------------------------------------------------

Function TSemaphore.Wait: Boolean;
var
  ExitWait: Boolean;
begin
repeat
  Result := CheckErrAlt(sem_wait(fLockPtr));
  ExitWait := Result or (ThrErrorCode <> ESysEINTR);
until ExitWait;
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TSemaphore.TryWaitStrict: Boolean;
var
  ExitWait: Boolean;
begin
repeat
  ExitWait := True;
  Result := CheckErrAlt(sem_trywait(fLockPtr));
  If not Result then
    case ThrErrorCode of
      ESysEINTR:  ExitWait := False;
      ESysEAGAIN:;// do nothing (exit with result being false)
    else
      raise ELSOSysOpError.CreateFmt('TSemaphore.TryWaitStrict: ' +
        'Failed to try-wait on semaphore (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    end;
until ExitWait;
end;

//------------------------------------------------------------------------------

Function TSemaphore.TryWait: Boolean;
var
  ExitWait: Boolean;
begin
repeat
  Result := CheckErrAlt(sem_trywait(fLockPtr));
  ExitWait := Result or (ThrErrorCode <> ESysEINTR);
until ExitWait;
If not Result and (ThrErrorCode = ESysEAGAIN) then
  fLastError := 0
else
  fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TSemaphore.TimedWait(Timeout: UInt32): TLSOWaitResult;
var
  TimeoutSpec:  timespec;
  ExitWait:     Boolean;
begin
try
  Result := wrError;
  If Timeout <> INFINITE then
    begin
      ResolveAbsoluteTimeout(Timeout,TimeoutSpec);
      repeat
        ExitWait := True;
        If not CheckErrAlt(sem_timedwait(fLockPtr,@TimeoutSpec)) then
          case ThrErrorCode of
            ESysEINTR:      ExitWait := False;  // no need to reset timeout, it is absolute
            ESysETIMEDOUT:  Result := wrTimeout;
          else
            Result := wrError;
            fLastError := ThrErrorCode;
          end
        else Result := wrSignaled;
      until ExitWait;
    end
  else
    begin
      If Wait then
        Result := wrSignaled
      else
        Result := wrError;
    end;
except
  Result := wrError;
end;
end;

//------------------------------------------------------------------------------

procedure TSemaphore.PostStrict;
begin
If not CheckErrAlt(sem_post(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TSemaphore.PostStrict: ' +
    'Failed to post semaphore (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TSemaphore.Post: Boolean;
begin
Result := CheckErrAlt(sem_post(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TReadWriteLock
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TReadWriteLock - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TReadWriteLock - protected methods
-------------------------------------------------------------------------------}

class Function TReadWriteLock.GetLockType: TLSOLockType;
begin
Result := ltRWLock;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.ResolveLockPtr;
begin
fLockPtr := Addr(PLSOSharedData(fSharedData)^.RWLock);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TReadWriteLock.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
var
  RWLockAttr: pthread_rwlockattr_t;
begin
inherited;
If CheckResErr(pthread_rwlockattr_init(@RWLockAttr)) then
  try
    If fProcessShared then
      If not CheckResErr(pthread_rwlockattr_setpshared(@RWLockAttr,PTHREAD_PROCESS_SHARED)) then
        raise ELSOSysOpError.CreateFmt('TReadWriteLock.InitializeLock: ' +
          'Failed to set rwlock attribute PSHARED (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If not CheckResErr(pthread_rwlock_init(Addr(PLSOSharedData(fSharedData)^.RWLock),@RWLockAttr)) then
      raise ELSOSysInitError.CreateFmt('TReadWriteLock.InitializeLock: ' +
        'Failed to initialize rwlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Inc(CompleteStage);
  finally
    If not CheckResErr(pthread_rwlockattr_destroy(@RWLockAttr)) then
      raise ELSOSysFinalError.CreateFmt('TReadWriteLock.InitializeLock: ' +
        'Failed to destroy rwlock attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end
else raise ELSOSysInitError.CreateFmt('TReadWriteLock.InitializeLock: ' +
       'Failed to initialize rwlock attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TReadWriteLock.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 0 then
  If not CheckResErr(pthread_rwlock_destroy(Addr(PLSOSharedData(fSharedData)^.RWLock))) then
    raise ELSOSysFinalError.CreateFmt('TReadWriteLock.FinalizeLock: ' +
      'Failed to destroy rwlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TReadWriteLock - public methods
-------------------------------------------------------------------------------}

procedure TReadWriteLock.ReadLockStrict;
begin
If not CheckResErr(pthread_rwlock_rdlock(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TReadWriteLock.ReadLockStrict: ' +
    'Failed to read-lock rwlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.ReadLock: Boolean;
begin
Result := CheckResErr(pthread_rwlock_rdlock(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.TryReadLockStrict: Boolean;
begin
Result := CheckResErr(pthread_rwlock_tryrdlock(fLockPtr));
If not Result and (ThrErrorCode <> ESysEBUSY) then
  raise ELSOSysOpError.CreateFmt('TReadWriteLock.TryReadLockStrict: ' +
    'Failed to try-read-lock rwlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.TryReadLock: Boolean;
begin
Result := CheckResErr(pthread_rwlock_tryrdlock(fLockPtr));
If not Result and (ThrErrorCode = ESysEBUSY) then
  fLastError := 0
else
  fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.TimedReadLock(Timeout: UInt32): TLSOWaitResult;
var
  TimeoutSpec:  timespec;
begin
try
  If Timeout <> INFINITE then
    begin
      ResolveAbsoluteTimeout(Timeout,TimeoutSpec);
      If not CheckResErr(pthread_rwlock_timedrdlock(fLockPtr,@TimeoutSpec)) then
        begin
          If ThrErrorCode <> ESysETIMEDOUT then
            begin
              Result := wrError;
              fLastError := Integer(ThrErrorCode);
            end
          else Result := wrTimeout;
        end
      else Result := wrSignaled;
    end
  else
    begin
      If ReadLock then
        Result := wrSignaled
      else
        Result := wrError;
    end;
except
  Result := wrError;
end;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.WriteLockStrict;
begin
If not CheckResErr(pthread_rwlock_wrlock(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TReadWriteLock.WriteLockStrict: ' +
    'Failed to write-lock rwlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.WriteLock: Boolean;
begin
Result := CheckResErr(pthread_rwlock_wrlock(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.TryWriteLockStrict: Boolean;
begin
Result := CheckResErr(pthread_rwlock_trywrlock(fLockPtr));
If not Result and (ThrErrorCode <> ESysEBUSY) then
  raise ELSOSysOpError.CreateFmt('TReadWriteLock.TryWriteLockStrict: ' +
    'Failed to try-write-lock rwlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.TryWriteLock: Boolean;
begin
Result := CheckResErr(pthread_rwlock_trywrlock(fLockPtr));
If not Result and (ThrErrorCode = ESysEBUSY) then
  fLastError := 0
else
  fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.TimedWriteLock(Timeout: UInt32): TLSOWaitResult;
var
  TimeoutSpec:  timespec;
begin
try
  If Timeout <> INFINITE then
    begin
      ResolveAbsoluteTimeout(Timeout,TimeoutSpec);
      If not CheckResErr(pthread_rwlock_timedwrlock(fLockPtr,@TimeoutSpec)) then
        begin
          If ThrErrorCode <> ESysETIMEDOUT then
            begin
              Result := wrError;
              fLastError := Integer(ThrErrorCode);
            end
          else Result := wrTimeout;
        end
      else Result := wrSignaled;
    end
  else
    begin
      If WriteLock then
        Result := wrSignaled
      else
        Result := wrError;
    end;
except
  Result := wrError;
end;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.UnlockStrict;
begin
If not CheckResErr(pthread_rwlock_unlock(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TReadWriteLock.UnlockStrict: ' +
    'Failed to unlock rwlock (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.Unlock: Boolean;
begin
Result := CheckResErr(pthread_rwlock_unlock(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TConditionVariable
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TConditionVariable - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TConditionVariable - protected methods
-------------------------------------------------------------------------------}

Function TConditionVariable.DoOnPredicateCheck: Boolean;
begin
Result := False;
If Assigned(fOnPredicateCheckEvent) then
  fOnPredicateCheckEvent(Self,Result)
else If Assigned(fOnPredicateCheckCallback) then
  fOnPredicateCheckCallback(Self,Result);
end;

//------------------------------------------------------------------------------

Function TConditionVariable.DoOnDataAccess: TLSOWakeOptions;
begin
If Assigned(fOnDataAccessEvent) or Assigned(fOnDataAccessCallback) then
  begin
    Result := [];
    If Assigned(fOnDataAccessEvent) then
      fOnDataAccessEvent(Self,Result)
    else If Assigned(fOnDataAccessCallback) then
      fOnDataAccessCallback(Self,Result);
  end
else Result := [woWakeAll];
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.SelectWake(WakeOptions: TLSOWakeOptions);
begin
If ([woWakeOne,woWakeAll] *{intersection} WakeOptions) <> [] then
  begin
    If woWakeAll in WakeOptions then
      Broadcast
    else
      Signal;
  end;
end;

//------------------------------------------------------------------------------

class Function TConditionVariable.GetLockType: TLSOLockType;
begin
Result := ltCondVar;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.ResolveLockPtr;
begin
fLockPtr := Addr(PLSOSharedData(fSharedData)^.CondVar.CondVar);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TConditionVariable.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
var
  CondVarAttr:  pthread_condattr_t;
begin
inherited;
If CheckResErr(pthread_condattr_init(@CondVarAttr)) then
  try
    If fProcessShared then
      If not CheckResErr(pthread_condattr_setpshared(@CondVarAttr,PTHREAD_PROCESS_SHARED)) then
        raise ELSOSysOpError.CreateFmt('TConditionVariable.InitializeLock: ' +
          'Failed to set condvar attribute PSHARED (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If not CheckResErr(pthread_cond_init(Addr(PLSOSharedData(fSharedData)^.CondVar.CondVar),@CondVarAttr)) then
      raise ELSOSysInitError.CreateFmt('TConditionVariable.InitializeLock: ' +
        'Failed to initialize condvar (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Inc(CompleteStage);
  finally
    If not CheckResErr(pthread_condattr_destroy(@CondVarAttr)) then
      raise ELSOSysFinalError.CreateFmt('TConditionVariable.InitializeLock: ' +
        'Failed to destroy condvar attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end
else raise ELSOSysInitError.CreateFmt('TConditionVariable.InitializeLock: ' +
       'Failed to initialize condvar attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TConditionVariable.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 0 then
  If not CheckResErr(pthread_cond_destroy(Addr(PLSOSharedData(fSharedData)^.CondVar.CondVar))) then
    raise ELSOSysFinalError.CreateFmt('TConditionVariable.FinalizeLock: ' +
      'Failed to destroy condvar (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TConditionVariable - public methods
-------------------------------------------------------------------------------}

procedure TConditionVariable.WaitStrict(DataLock: ppthread_mutex_t);
begin
If not CheckResErr(pthread_cond_wait(fLockPtr,DataLock)) then
  raise ELSOSysOpError.CreateFmt('TConditionVariable.WaitStrict: ' +
    'Failed to wait on condvar (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TConditionVariable.WaitStrict(DataLock: TMutex);
begin
WaitStrict(DataLock.fLockPtr);
end;

//------------------------------------------------------------------------------

Function TConditionVariable.Wait(DataLock: ppthread_mutex_t): Boolean;
begin
Result := CheckResErr(pthread_cond_wait(fLockPtr,DataLock));
fLastError := Integer(ThrErrorCode);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TConditionVariable.Wait(DataLock: TMutex): Boolean;
begin
Result := Wait(DataLock.fLockPtr);
end;

//------------------------------------------------------------------------------

Function TConditionVariable.TimedWait(DataLock: ppthread_mutex_t; Timeout: UInt32): TLSOWaitResult;
var
  TimeoutSpec:  timespec;
begin
try
  If Timeout <> INFINITE then
    begin
      ResolveAbsoluteTimeout(Timeout,TimeoutSpec);
      If not CheckResErr(pthread_cond_timedwait(fLockPtr,DataLock,@TimeoutSpec)) then
        begin
          If ThrErrorCode <> ESysETIMEDOUT then
            begin
              Result := wrError;
              fLastError := Integer(ThrErrorCode);
            end
          else Result := wrTimeout;
        end
      else Result := wrSignaled;
    end
  else
    begin
      If Wait(DataLock) then
        Result := wrSignaled
      else
        Result := wrError;
    end;
except
  Result := wrError;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TConditionVariable.TimedWait(DataLock: TMutex; Timeout: UInt32): TLSOWaitResult;
begin
Result := TimedWait(DataLock.fLockPtr,Timeout);
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.SignalStrict;
begin
If not CheckResErr(pthread_cond_signal(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TConditionVariable.SignalStrict: ' +
    'Failed to signal condvar (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TConditionVariable.Signal: Boolean;
begin
Result := CheckResErr(pthread_cond_signal(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.BroadcastStrict;
begin
If not CheckResErr(pthread_cond_broadcast(fLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TConditionVariable.BroadcastStrict: ' +
    'Failed to broadcast condvar (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TConditionVariable.Broadcast: Boolean;
begin
Result := CheckResErr(pthread_cond_broadcast(fLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.AutoCycle(DataLock: ppthread_mutex_t; Timeout: UInt32);
var
  ReturnValue:  cInt;
  WakeOptions:  TLSOWakeOptions;
begin
If Assigned(fOnPredicateCheckEvent) or Assigned(fOnPredicateCheckCallback) then
  begin
    // lock synchronizer
    ReturnValue := pthread_mutex_lock(DataLock);
    If ReturnValue = ESysEOWNERDEAD then
      begin
        If not CheckResErr(pthread_mutex_consistent(DataLock)) then
          raise ELSOSysOpError.CreateFmt('TConditionVariable.AutoCycle: ' +
            'Failed to make data-lock mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
      end
    else
      begin
        If not CheckResErr(ReturnValue) then
          raise ELSOSysOpError.CreateFmt('TConditionVariable.AutoCycle: ' +
            'Failed to lock data-lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
      end;
    // test predicate and wait condition
    while not DoOnPredicateCheck do
      TimedWait(DataLock,Timeout);
    // access protected data
    WakeOptions := DoOnDataAccess;
    // wake waiters before unlock
    If (woWakeBeforeUnlock in WakeOptions) then
      SelectWake(WakeOptions);
    // unlock synchronizer
    If not CheckResErr(pthread_mutex_unlock(DataLock)) then
      raise ELSOSysOpError.CreateFmt('TConditionVariable.AutoCycle: ' +
        'Failed to unlock data-lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    // wake waiters after unlock
    If not(woWakeBeforeUnlock in WakeOptions) then
      SelectWake(WakeOptions);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TConditionVariable.AutoCycle(DataLock: TMutex; Timeout: UInt32);
var
  WakeOptions:  TLSOWakeOptions;
begin
If Assigned(fOnPredicateCheckEvent) or Assigned(fOnPredicateCheckCallback) then
  begin
    DataLock.LockStrict;
    while not DoOnPredicateCheck do
      TimedWait(DataLock,Timeout);
    WakeOptions := DoOnDataAccess;
    If (woWakeBeforeUnlock in WakeOptions) then
      SelectWake(WakeOptions);
    DataLock.UnlockStrict;
    If not(woWakeBeforeUnlock in WakeOptions) then
      SelectWake(WakeOptions);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TConditionVariable.AutoCycle(DataLock: ppthread_mutex_t);
begin
AutoCycle(DataLock,INFINITE);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TConditionVariable.AutoCycle(DataLock: TMutex);
begin
AutoCycle(DataLock,INFINITE);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              TConditionVariableEx
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TConditionVariableEx - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TConditionVariableEx - protected methods
-------------------------------------------------------------------------------}

class Function TConditionVariableEx.GetLockType: TLSOLockType;
begin
Result := ltCondVarEx;
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.ResolveLockPtr;
begin
inherited;
fDataLockPtr := Addr(PLSOSharedData(fSharedData)^.CondVar.DataLock);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TConditionVariableEx.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
var
  MutexAttr:  pthread_mutexattr_t;
begin
inherited;
// data-lock mutex
If CheckResErr(pthread_mutexattr_init(@MutexAttr)) then
  try
    If not CheckResErr(pthread_mutexattr_settype(@MutexAttr,PTHREAD_MUTEX_RECURSIVE)) then
      raise ELSOSysOpError.CreateFmt('TConditionVariableEx.InitializeLock: ' +
        'Failed to set data-lock mutex attribute TYPE (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If not CheckResErr(pthread_mutexattr_setrobust(@MutexAttr,PTHREAD_MUTEX_ROBUST)) then
      raise ELSOSysOpError.CreateFmt('TConditionVariableEx.InitializeLock: ' +
        'Failed to set data-lock mutex attribute ROBUST (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If fProcessShared then
      If not CheckResErr(pthread_mutexattr_setpshared(@MutexAttr,PTHREAD_PROCESS_SHARED)) then
        raise ELSOSysOpError.CreateFmt('TConditionVariableEx.InitializeLock: ' +
          'Failed to set data-lock mutex attribute PSHARED (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If not CheckResErr(pthread_mutex_init(Addr(PLSOSharedData(fSharedData)^.CondVar.DataLock),@MutexAttr)) then
      raise ELSOSysInitError.CreateFmt('TConditionVariableEx.InitializeLock: ' +
        'Failed to initialize data-lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Inc(CompleteStage);
  finally
    If not CheckResErr(pthread_mutexattr_destroy(@MutexAttr)) then
      raise ELSOSysFinalError.CreateFmt('TConditionVariableEx.InitializeLock: ' +
        'Failed to destroy data-lock mutex attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end
else raise ELSOSysInitError.CreateFmt('TConditionVariableEx.InitializeLock: ' +
       'Failed to initialize data-lock mutex attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TConditionVariableEx.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 1 then
  If not CheckResErr(pthread_mutex_destroy(Addr(PLSOSharedData(fSharedData)^.CondVar.DataLock))) then
    raise ELSOSysFinalError.CreateFmt('TConditionVariableEx.FinalizeLock: ' +
      'Failed to destroy data-lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
inherited;
end;

{-------------------------------------------------------------------------------
    TConditionVariableEx - public methods
-------------------------------------------------------------------------------}

procedure TConditionVariableEx.LockStrict;
var
  ReturnValue:  cInt;
begin
ReturnValue := pthread_mutex_lock(fDataLockPtr);
If ReturnValue = ESysEOWNERDEAD then
  begin
    If not CheckResErr(pthread_mutex_consistent(fDataLockPtr)) then
      raise ELSOSysOpError.CreateFmt('TConditionVariableEx.LockStrict: ' +
        'Failed to make data-lock mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end
else
  begin
    If not CheckResErr(ReturnValue) then
      raise ELSOSysOpError.CreateFmt('TConditionVariableEx.LockStrict: ' +
        'Failed to lock data-lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end;
end;

//------------------------------------------------------------------------------

Function TConditionVariableEx.Lock: Boolean;
var
  ReturnValue:  cInt;
begin
ReturnValue := pthread_mutex_lock(fDataLockPtr);
If ReturnValue = ESysEOWNERDEAD then
  begin
    If not CheckResErr(pthread_mutex_consistent(fDataLockPtr)) then
      raise ELSOSysOpError.CreateFmt('TConditionVariableEx.Lock: ' +
        'Failed to make data-lock mutex consistent (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Result := True;
    fLastError := 0;
  end
else
  begin
    Result := CheckResErr(ReturnValue);
    fLastError := Integer(ThrErrorCode);
  end;
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.UnlockStrict;
begin
If not CheckResErr(pthread_mutex_unlock(fDataLockPtr)) then
  raise ELSOSysOpError.CreateFmt('TConditionVariableEx.UnlockStrict: ' +
    'Failed to unlock data-lock mutex (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TConditionVariableEx.Unlock: Boolean;
begin
Result := CheckResErr(pthread_mutex_unlock(fDataLockPtr));
fLastError := Integer(ThrErrorCode);
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.WaitStrict;
begin
WaitStrict(fDataLockPtr);
end;

//------------------------------------------------------------------------------

Function TConditionVariableEx.Wait: Boolean;
begin
Result := Wait(fDataLockPtr);
end;

//------------------------------------------------------------------------------

Function TConditionVariableEx.TimedWait(Timeout: UInt32): TLSOWaitResult;
begin
Result := TimedWait(fDataLockPtr,Timeout);
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.AutoCycle(Timeout: UInt32);
begin
AutoCycle(fDataLockPtr,Timeout);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TConditionVariableEx.AutoCycle;
begin
AutoCycle(fDataLockPtr,INFINITE);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TBarrier
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBarrier - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBarrier - protected methods
-------------------------------------------------------------------------------}

class Function TBarrier.GetLockType: TLSOLockType;
begin
Result := ltBarrier;
end;

//------------------------------------------------------------------------------

procedure TBarrier.ResolveLockPtr;
begin
fLockPtr := Addr(PLSOSharedData(fSharedData)^.Barrier);
end;

//------------------------------------------------------------------------------

procedure TBarrier.InitializeLock(InitializingData: PtrUInt; var CompleteStage: Integer);
var
  BarrierAttr:  pthread_barrierattr_t;
begin
inherited;
If CheckResErr(pthread_barrierattr_init(@BarrierAttr)) then
  try
    If fProcessShared then
      If not CheckResErr(pthread_barrierattr_setpshared(@BarrierAttr,PTHREAD_PROCESS_SHARED)) then
        raise ELSOSysOpError.CreateFmt('TBarrier.InitializeLock: ' +
          'Failed to set barrier attribute PSHARED (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    If not CheckResErr(pthread_barrier_init(Addr(PLSOSharedData(fSharedData)^.Barrier),@BarrierAttr,cUnsigned(InitializingData))) then
      raise ELSOSysInitError.CreateFmt('TBarrier.InitializeLock: ' +
        'Failed to initialize barrier (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
    Inc(CompleteStage);
  finally
    If not CheckResErr(pthread_barrierattr_destroy(@BarrierAttr)) then
      raise ELSOSysFinalError.CreateFmt('TBarrier.InitializeLock: ' +
        'Failed to destroy barrier attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
  end
else raise ELSOSysInitError.CreateFmt('TBarrier.InitializeLock: ' +
       'Failed to initialize barrier attributes (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

procedure TBarrier.FinalizeLock(CompleteStage: Integer = MAXINT);
begin
If CompleteStage > 0 then
  If not CheckResErr(pthread_barrier_destroy(Addr(PLSOSharedData(fSharedData)^.Barrier))) then
    raise ELSOSysFinalError.CreateFmt('TBarrier.FinalizeLock: ' +
      'Failed to destroy barrier (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

{-------------------------------------------------------------------------------
    TBarrier - public methods
-------------------------------------------------------------------------------}

constructor TBarrier.Create(const Name: String; Count: cUnsigned);
begin
ProtectedCreate(Name,PtrUInt(Count));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create(Count: cUnsigned);
begin
ProtectedCreate('',PtrUInt(Count));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create(const Name: String);
begin
ProtectedCreate(Name,1);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create;
begin
ProtectedCreate('',1);
end;

//------------------------------------------------------------------------------

procedure TBarrier.WaitStrict;
begin
If not CheckResErr(pthread_barrier_wait(fLockPtr)) then
  If ThrErrorCode <> PTHREAD_BARRIER_SERIAL_THREAD then
    raise ELSOSysOpError.CreateFmt('TBarrier.WaitStrict: ' +
      'Failed to wait on barrier (%d - %s).',[ThrErrorCode,StrError(ThrErrorCode)]);
end;

//------------------------------------------------------------------------------

Function TBarrier.Wait: Boolean;
begin
Result := CheckResErr(pthread_barrier_wait(fLockPtr));
If not Result and (ThrErrorCode = PTHREAD_BARRIER_SERIAL_THREAD) then
  begin
    Result := True;
    fLastError := 0;
  end
else fLastError := Integer(ThrErrorCode);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Wait functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Each multi-wait is associated with a futex that is stored in globally shared
  memory. Since the memory is allocated only once, it has a static size,
  meaning there is a limit on how many futexes, and therefore concurrently
  running wait functions, can exist at a time. This limit is currently set to
  15360 - this number is arbitrary and can be changed in future implementations.

  Each event contains a short list of waiters (or more precisely, references to
  wait-function-associated futexes) that are waiting on that particular event.
  Since this list is stored in shared data of the event, it cannot be long,
  therefore there is a limit of 16 waiters waiting on any particular event.

  Waiting first obtains a free futex, then sets its value to a number of waited
  objects and adds itself (the obtained futex) to list of waiters in all events.
  It then enters waiting on the futex.

  Events, when they get locked or unlocked, increase or decrease value of this
  futex and, depending on its value and whether the waiter waits for all objects
  or only one, wake the waiter. The waiter then performs necessary checks and
  other actions on all waited objects an either exits with appropriate result or
  re-enters waiting. It also removes itself from all events.
}
{===============================================================================
    Wait functions - internal functions
===============================================================================}

Function WaitForMultipleEvents_Internal(Objects: array of PLSOEvent; Timeout: DWORD; WaitAll: Boolean; out Index: Integer): TLSOWaitResult;

  procedure ReturnError(ErrorCode: Integer);
  begin
    Index := ErrorCode;
    Result := wrError;
  end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

  Function CheckObjects: Boolean;
  var
    i,j:  Integer;
  begin
    Result := True;
    // check for nil
    For i := Low(Objects) to High(Objects) do
      If not Assigned(Objects[i]) then
        begin
          Result := False;
          Exit;
        end;
    // check that there are no duplicates
    For i := Low(Objects) to Pred(High(Objects)) do
      For j := Succ(i) to High(Objects) do
        If Objects[i] = Objects[j] then
          begin
            Result := False;
            Exit;
          end;
  end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
var
  WaiterFutexIdx:   TLSOMultiWaitSlotIndex;
  WaiterFutexPtr:   PFutexWord;
  TimeoutRemaining: UInt32;
  StartTime:        TTimeSpec;
  Counter:          Integer;
  ExitWait:         Boolean;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

  Function AddSelfToWaiters: Boolean;
  var
    ii:         Integer;
    FailIndex:  Integer;
  begin
  {
    First try to add this waiter to all events.
    If any addition fails, roll back and return false
  }
    Result := True;
    FailIndex := -1;
    For ii := Low(Objects) to High(Objects) do
      If not _event_addwaiter(Objects[ii],WaiterFutexIdx,WaitAll) then
        begin
          FailIndex := ii;
          Result := False;
          Break{For ii};
        end;
    If not Result then
      For ii := Low(Objects) to Pred(FailIndex) do
        _event_removewaiter(Objects[ii],WaiterFutexIdx);
  end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

  procedure RemoveSelfFromWaiters;
  var
    ii: Integer;
  begin
    For ii := Low(Objects) to High(Objects) do
      _event_removewaiter(Objects[ii],WaiterFutexIdx);
  end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

  procedure ProcessWaitAll;
  var
    ii: Integer;
  begin
    // check state of all objects
    Counter := Length(Objects);
    For ii := Low(Objects) to High(Objects) do
      If Objects[ii]^.Signaled then
        Dec(Counter);
    // if all are signaled, change state of auto-reset events to locked
    If Counter <= 0 then
      begin
        For ii := Low(Objects) to High(Objects) do
          If not Objects[ii]^.ManualReset then
            begin
              Objects[ii]^.Signaled := False;
              InterlockedStore(Objects[ii]^.WaitFutex,LSO_EVENTSTATE_LOCKED);
              _event_gotlocked(Objects[ii]);
            end;
        Result := wrSignaled;
      end
    else ExitWait := False; // spurious wakeup
  end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

  procedure ProcessWaitOne;
  var
    ii:       Integer;
    SigIndex: Integer;
  begin
    // find first signaled event
    SigIndex := -1;
    For ii := Low(Objects) to High(Objects) do
      If Objects[ii]^.Signaled then
        begin
          SigIndex := ii;
          Break{For i};
        end;
    If SigIndex >= 0 then
      begin
        If not Objects[SigIndex]^.ManualReset then
          begin
            Objects[SigIndex]^.Signaled := False;
            InterlockedStore(Objects[SigIndex]^.WaitFutex,LSO_EVENTSTATE_LOCKED);
            _event_gotlocked(Objects[SigIndex]);
          end;
        Index := SigIndex;
        Result := wrSignaled;
      end
    else ExitWait := False; // spurious wakeup
  end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

  procedure ProcessEvents;
  var
    ii: Integer;
  begin
    // lock all objects to prevent change in their state
    For ii := Low(Objects) to High(Objects) do
      _event_lockdata(Objects[ii]);
    try
      If WaitAll then
        ProcessWaitAll
      else
        ProcessWaitOne;
    finally
      // unlock all objects
      For ii := Low(Objects) to High(Objects) do
        _event_unlockdata(Objects[ii]);
    end;
  end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

begin
Index := -1;
try
  If CheckObjects then
    begin
      If MultiWaitSlots.GetFreeSlotIndex(WaiterFutexIdx) then
        try
          // init waiter futex
          WaiterFutexPtr := MultiWaitSlots[WaiterFutexIdx];
          InterlockedStore(WaiterFutexPtr^,Length(Objects));
          // add waiter futex to all events
          If AddSelfToWaiters then
            try
              // wait on the waiter futex
              TimeoutRemaining := Timeout;
              GetTime(StartTime);
              Counter := Length(Objects);
              repeat
                ExitWait := True;
                If not WaitAll then
                  Counter := Length(Objects);
                case FutexWait(WaiterFutexPtr^,TFutexWord(Counter),TimeoutRemaining) of
                  fwrWoken,
                  fwrValue:       ProcessEvents;
                  fwrTimeout:     Result := wrTimeout;  // exit with timeout
                  fwrInterrupted: ExitWait := False;    // recalculate timeout and re-enter waiting
                else
                  ReturnError(LSO_WAITERROR_FUTEXWAIT);
                end;
                // recalculate timeout if not exiting
                If not ExitWait then
                  If not RecalculateTimeout(Timeout,StartTime,TimeoutRemaining) then
                    begin
                      Result := wrTimeout;
                      ExitWait := True;
                    end;
              until ExitWait;
            finally
              // remove waiter futex from all events
              RemoveSelfFromWaiters;
            end
          else ReturnError(LSO_WAITERROR_EVENTFULL);
        finally
          // return waiter futex
          MultiWaitSlots.InvalidateSlot(WaiterFutexIdx);
        end
      else ReturnError(LSO_WAITERROR_NOSLOT);
    end
  else ReturnError(LSO_WAITERROR_OBJECTS);
except
  ReturnError(LSO_WAITERROR_UNKNOWN);
end;
end;

{===============================================================================
    Wait functions - public functions
===============================================================================}

Function WaitForMultipleEvents(Objects: array of PLSOEvent; WaitAll: Boolean; Timeout: DWORD; out Index: Integer): TLSOWaitResult;

  procedure ReturnError(ErrorCode: Integer);
  begin
    Index := ErrorCode;
    Result := wrError;
  end;

begin
Index := -1;
try
  If Length(Objects) > 1 then
     Result := WaitForMultipleEvents_Internal(Objects,Timeout,WaitAll,Index)
  else If Length(Objects) = 1 then
    begin
      Index := 0;
      If not CheckResErr(event_timedwait(Objects[0],Timeout)) then
        begin
          If ThrErrorCode = ESysETIMEDOUT then
            Result := wrTimeout
          else
            ReturnError(LSO_WAITERROR_FUTEXWAIT);
        end
      else Result := wrSignaled;
    end
  else ReturnError(LSO_WAITERROR_COUNT);
except
  ReturnError(LSO_WAITERROR_UNKNOWN);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleEvents(Objects: array of PLSOEvent; WaitAll: Boolean; Timeout: DWORD): TLSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForMultipleEvents(Objects,WaitAll,Timeout,Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleEvents(Objects: array of PLSOEvent; WaitAll: Boolean): TLSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForMultipleEvents(Objects,WaitAll,INFINITE,Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean; Timeout: DWORD; out Index: Integer): TLSOWaitResult;
var
  TempArr:  array of PLSOEvent;
  i:        Integer;
begin
TempArr := nil;
SetLength(TempArr,Length(Objects));
For i := Low(Objects) to High(Objects) do
  TempArr[i] := PLSOEvent(Objects[i].fLockPtr);
Result := WaitForMultipleEvents(TempArr,WaitAll,Timeout,Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean; Timeout: DWORD): TLSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForMultipleEvents(Objects,WaitAll,Timeout,Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean): TLSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForMultipleEvents(Objects,WaitAll,INFINITE,Index);
end;

{===============================================================================
--------------------------------------------------------------------------------
                               Utility functions
--------------------------------------------------------------------------------
===============================================================================}

Function WaitResultToStr(WaitResult: TLSOWaitResult): String;
const
  WR_STRS: array[TLSOWaitResult] of String = ('Signaled','Abandoned','Timeout','Error');
begin
If (WaitResult >= Low(TLSOWaitResult)) and (WaitResult <= High(TLSOWaitResult)) then
  Result := WR_STRS[WaitResult]
else
  Result := '<invalid>';
end;

{===============================================================================
--------------------------------------------------------------------------------
                                Multi-wait slots
--------------------------------------------------------------------------------
===============================================================================}

initialization
  LSO_SHAREDDATA_THREADLOCK := TCriticalSection.Create;
  InitMultiWaitSlots;

finalization
  FinalMultiWaitSlots;
  FreeAndNil(LSO_SHAREDDATA_THREADLOCK);

end.

