{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  WinSyncObjs

    Main aim of this library is to provide classes that are encapsulating
    synchronization objects provided by the Windows operating system.
    It currently implements classes for critical section, event, mutex,
    semaphore and waitable timer.

    Secondary function is to provide other synchronization primitives not
    available in Windows. At this point, condition variable, barrier and
    read-write lock are implemented.
    They can all be used for inter-process synchronization. Unfortunately,
    none can be used in waiting for multiple objects as they are compound or
    (in-here refered as) complex objects.

      WARNING - complex synchronization objects are not fully tested and should
                be therefore considered experimental.

    Another part of this library is encapsulation and extension of functions
    allowing waiting for multiple objects.
    The extension (functions WaitForManyHandles and WaitForManyObjects) allows
    waiting for more than 64 objects, which is otherwise a limit for system-
    provided calls.

      WARNING - waiting for many objects should only be used for waiting on
                objects with immutable state. Refer to description of these
                functions for details.

      WARNING - waiting on many objects should also be considered an
                experimental implementation.

  Version 1.2 (2022-12-26)

  Last change 2023-12-29

  ©2016-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.WinSyncObjs

  Dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps     - github.com/TheLazyTomcat/Lib.InterlockedOps
    NamedSharedItems   - github.com/TheLazyTomcat/Lib.NamedSharedItems
    SHA1               - github.com/TheLazyTomcat/Lib.SHA1
    SharedMemoryStream - github.com/TheLazyTomcat/Lib.SharedMemoryStream
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils

  Library SimpleCPUID might not be required, depending on defined symbols in
  InterlockedOps and BitOps libraries.

===============================================================================}
unit WinSyncObjs;

{$IF not(defined(MSWINDOWS) or defined(WINDOWS))}
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

{$IF Declared(CompilerVersion)}
  {$IF CompilerVersion >= 20} // Delphi 2009+
    {$DEFINE DeprecatedCommentDelphi}
  {$IFEND}
{$IFEND}

{$IF Defined(FPC) or Defined(DeprecatedCommentDelphi)}
  {$DEFINE DeprecatedComment}
{$ELSE}
  {$UNDEF DeprecatedComment}
{$IFEND}

interface

uses
  Windows, SysUtils,
  AuxTypes, AuxClasses, NamedSharedItems;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EWSOException = class(Exception);

  EWSOTimestampError         = class(EWSOException);
  EWSOTimeConversionError    = class(EWSOException);
  EWSOInitializationError    = class(EWSOException);
  EWSOHandleDuplicationError = class(EWSOException);
  EWSOOpenError              = class(EWSOException);

  EWSOEventError     = class(EWSOException);
  EWSOMutexError     = class(EWSOException);
  EWSOSemaphoreError = class(EWSOException);
  EWSOTimerError     = class(EWSOException);

  EWSOInvalidHandle = class(EWSOException);
  EWSOInvalidObject = class(EWSOException);
  EWSOInvalidValue  = class(EWSOException);

  EWSOWaitError             = class(EWSOException);
  EWSOMultiWaitError        = class(EWSOException);
  EWSOMultiWaitInvalidCount = class(EWSOException);

{===============================================================================
--------------------------------------------------------------------------------
                                TCriticalSection
--------------------------------------------------------------------------------
===============================================================================}
{
  To properly use the TCriticalSection object, create one instance and then
  pass this one instance to other threads that need to be synchronized.

  Make sure to only free it once.

  You can also set the proterty FreeOnRelease to true (by default false) and
  then use the build-in reference counting - call method Acquire for each
  thread using it (including the one that created it) and method Release every
  time a thread will stop using it. When reference count reaches zero in a
  call to Release, the object will be automatically freed.
}
{===============================================================================
    TCriticalSection - class declaration
===============================================================================}
type
  TCriticalSection = class(TCustomRefCountedObject)
  protected
    fCriticalSectionObj:  TRTLCriticalSection;
    fSpinCount:           DWORD;
    Function GetSpinCount: DWORD;
    procedure SetSpinCountProc(Value: DWORD); // only redirector to SetSpinCount (setter cannot be a function)
  public
    constructor Create; overload;
    constructor Create(SpinCount: DWORD); overload;
    destructor Destroy; override;
    Function SetSpinCount(SpinCount: DWORD): DWORD; virtual;
    Function TryEnter: Boolean; virtual;
    procedure Enter; virtual;
    procedure Leave; virtual;
  {
    If you are setting SpinCount in multiple threads, then the property might
    not necessarily contain the correct value set for the underlying system
    object.
    Set this property only in one thread or use it only for reading the value
    that was set in the constructor.
  }
    property SpinCount: DWORD read GetSpinCount write SetSpinCountProc;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TWinSyncObject                                 
--------------------------------------------------------------------------------
===============================================================================}
type
  // used intarnally for object identification
  TWSOLockType = (
    ltEvent,ltMutex,ltSemaphore,ltWaitTimer,                  // simple locks
    ltSmplBarrier,ltBarrier,ltCondVar,ltCondVarEx,ltRWLock);  // complex locks

{===============================================================================
    TWinSyncObject - class declaration
===============================================================================}
type
  TWinSyncObject = class(TCustomObject)
  protected
    fLastError: DWORD;
    fName:      String;
    Function RectifyAndSetName(const Name: String): Boolean; virtual;
    class Function GetLockType: TWSOLockType; virtual; abstract;
  public
    constructor Create;
  {
    LastError contains code of the last operating system error that has not
    resulted in an exception being raised (eg. error during waiting, release
    operations, ...).
  }
    property LastError: DWORD read fLastError;
    property Name: String read fName;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              TSimpleWinSyncObject
--------------------------------------------------------------------------------
===============================================================================}
{
  To properly use simple windows synchronization object (TEvent, TMutex,
  TSemaphore, TWaitableTimer), create one progenitor instance and use this one
  instance only in a thread where it was created. Note that it IS possible and
  permissible to use the same instance in multiple threads, but this practice
  is discouraged as none of the fields are protected against concurrent access
  (especially problematic for LastError property).

  To access the synchronizer in other threads of the same process, create a new
  instance using DuplicateFrom constructor, passing the progenitor instance or
  any duplicate instance based on it.
  You can also use Open constructors where implemented and when the object is
  created with a name.

  To access the synchronizer in different process, it is recommended to use
  Open constructors (requires named object).
  DuplicateFromProcess constructors or DuplicateForProcess methods along with
  CreateFrom constructor can also be used, but this requires some kind of IPC
  to transfer the handles between processes.
}
{
  wrFatal is only used internally, and should not be returned by any public
  funtion. If it still is, you should treat it as library error.
}
type
  TWSOWaitResult = (wrSignaled, wrAbandoned, wrIOCompletion, wrMessage, wrTimeout, wrError, wrFatal);

{===============================================================================
    TSimpleWinSyncObject - class declaration
===============================================================================}
type
  TSimpleWinSyncObject = class(TWinSyncObject)
  protected
    fHandle:  THandle;
    Function RectifyAndSetName(const Name: String): Boolean; override;
    procedure CheckAndSetHandle(Handle: THandle); virtual;
    procedure DuplicateAndSetHandle(SourceProcess: THandle; SourceHandle: THandle); virtual;
  public
    constructor CreateFrom(Handle: THandle{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    constructor DuplicateFrom(SourceHandle: THandle); overload;
    constructor DuplicateFrom(SourceObject: TSimpleWinSyncObject); overload;
  {
    When passing handle from 32bit process to 64bit process, it is safe to
    zero or sign-extend it. In the opposite direction, it is safe to truncate
    it.
  }
    constructor DuplicateFromProcess(SourceProcess: THandle; SourceHandle: THandle);
    constructor DuplicateFromProcessID(SourceProcessID: DWORD; SourceHandle: THandle);
    destructor Destroy; override;
    Function DuplicateForProcess(TargetProcess: THandle): THandle; virtual;
    Function DuplicateForProcessID(TargetProcessID: DWORD): THandle; virtual;
  {
    WARNING - the first overload of method WaitFor intentionaly does not set
              LastError property as the error code is returned in parameter
              ErrCode.
  }
    Function WaitFor(Timeout: DWORD; out ErrCode: DWORD; Alertable: Boolean = False): TWSOWaitResult; overload; virtual;
    Function WaitFor(Timeout: DWORD = INFINITE; Alertable: Boolean = False): TWSOWaitResult; overload; virtual;
    property Handle: THandle read fHandle;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                     TEvent
--------------------------------------------------------------------------------
===============================================================================}
const
  // constants for DesiredAccess parameter of Open constructor
  EVENT_MODIFY_STATE = 2;
  EVENT_ALL_ACCESS   = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or 3;

{===============================================================================
    TEvent - class declaration
===============================================================================}
type
  TEvent = class(TSimpleWinSyncObject)
  protected
    class Function GetLockType: TWSOLockType; override;
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; ManualReset, InitialState: Boolean; const Name: String); overload;
    constructor Create(ManualReset, InitialState: Boolean; const Name: String); overload;
    constructor Create(ManualReset, InitialState: Boolean); overload;
    constructor Create(const Name: String); overload; // ManualReset := True, InitialState := False
    constructor Create; overload;
    constructor Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String); overload;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); overload;
    Function WaitForAndReset(Timeout: DWORD = INFINITE; Alertable: Boolean = False): TWSOWaitResult;
    procedure SetEventStrict; virtual;
    Function SetEvent: Boolean; virtual;
    procedure ResetEventStrict; virtual;
    Function ResetEvent: Boolean; virtual;
  {
    Function PulseEvent is unreliable and should not be used. More info here:
    https://msdn.microsoft.com/en-us/library/windows/desktop/ms684914
  }
    procedure PulseEventStrict; virtual; deprecated {$IFDEF DeprecatedComment}'Unreliable, do not use.'{$ENDIF};
    Function PulseEvent: Boolean; virtual; deprecated {$IFDEF DeprecatedComment}'Unreliable, do not use.'{$ENDIF};
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                     TMutex
--------------------------------------------------------------------------------
===============================================================================}
const
  MUTANT_QUERY_STATE = 1;
  MUTANT_ALL_ACCESS  = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or MUTANT_QUERY_STATE;

  MUTEX_MODIFY_STATE = MUTANT_QUERY_STATE;
  MUTEX_ALL_ACCESS   = MUTANT_ALL_ACCESS;

{===============================================================================
    TMutex - class declaration
===============================================================================}
type
  TMutex = class(TSimpleWinSyncObject)
  protected
    class Function GetLockType: TWSOLockType; override;
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; InitialOwner: Boolean; const Name: String); overload;
    constructor Create(InitialOwner: Boolean; const Name: String); overload;
    constructor Create(InitialOwner: Boolean); overload;
    constructor Create(const Name: String); overload; // InitialOwner := False
    constructor Create; overload;
    constructor Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String); overload;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); overload;
    Function WaitForAndRelease(TimeOut: DWORD = INFINITE; Alertable: Boolean = False): TWSOWaitResult; virtual;
    procedure ReleaseMutexStrict; virtual;
    Function ReleaseMutex: Boolean; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TSemaphore
--------------------------------------------------------------------------------
===============================================================================}
const
  SEMAPHORE_MODIFY_STATE = 2;
  SEMAPHORE_ALL_ACCESS   = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or 3;
  
{===============================================================================
    TSemaphore - class declaration
===============================================================================}
type
  TSemaphore = class(TSimpleWinSyncObject)
  protected
    class Function GetLockType: TWSOLockType; override;
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; InitialCount, MaximumCount: Integer; const Name: String); overload;
    constructor Create(InitialCount, MaximumCount: Integer; const Name: String); overload;
    constructor Create(InitialCount, MaximumCount: Integer); overload;
    constructor Create(const Name: String); overload; // InitialCount := 1, MaximumCount := 1
    constructor Create; overload;
    constructor Open(DesiredAccess: LongWord; InheritHandle: Boolean; const Name: String); overload;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); overload;
    Function WaitForAndRelease(TimeOut: LongWord = INFINITE; Alertable: Boolean = False): TWSOWaitResult; virtual;
    procedure ReleaseSemaphoreStrict(ReleaseCount: Integer; out PreviousCount: Integer); overload; virtual;
    procedure ReleaseSemaphoreStrict; overload; virtual;
    Function ReleaseSemaphore(ReleaseCount: Integer; out PreviousCount: Integer): Boolean; overload; virtual;
    Function ReleaseSemaphore: Boolean; overload; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TWaitableTimer
--------------------------------------------------------------------------------
===============================================================================}
const
  TIMER_QUERY_STATE  = 1;
  TIMER_MODIFY_STATE = 2;
  TIMER_ALL_ACCESS   = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or TIMER_QUERY_STATE or TIMER_MODIFY_STATE;

{===============================================================================
    TWaitableTimer - class declaration
===============================================================================}
type
  TTimerAPCRoutine = procedure(ArgToCompletionRoutine: Pointer; TimerLowValue, TimerHighValue: DWORD); stdcall;

  TWaitableTimer = class(TSimpleWinSyncObject)
  protected
    class Function GetLockType: TWSOLockType; override;
    Function DateTimeToFileTime(DateTime: TDateTime): TFileTime; virtual;
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; ManualReset: Boolean; const Name: String); overload;
    constructor Create(ManualReset: Boolean; const Name: String); overload;
    constructor Create(ManualReset: Boolean); overload;
    constructor Create(const Name: String); overload; // ManualReset := True
    constructor Create; overload;
    constructor Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String); overload;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); overload;
    procedure SetWaitableTimerStrict(DueTime: Int64; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean); overload; virtual;
    procedure SetWaitableTimerStrict(DueTime: Int64; Period: Integer = 0); overload; virtual;
    procedure SetWaitableTimerStrict(DueTime: TDateTime; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean); overload; virtual;
    procedure SetWaitableTimerStrict(DueTime: TDateTime; Period: Integer = 0); overload; virtual;
    Function SetWaitableTimer(DueTime: Int64; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean): Boolean; overload; virtual;
    Function SetWaitableTimer(DueTime: Int64; Period: Integer = 0): Boolean; overload; virtual;
    Function SetWaitableTimer(DueTime: TDateTime; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean): Boolean; overload; virtual;
    Function SetWaitableTimer(DueTime: TDateTime; Period: Integer = 0): Boolean; overload; virtual;
    procedure CancelWaitableTimerStrict; virtual;
    Function CancelWaitableTimer: Boolean; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              TComplexWinSyncObject
--------------------------------------------------------------------------------
===============================================================================}
{
  Complex synchronization objects can be principally created in two ways - as
  thread-shared or as process-shared.

  Thread-shared objects are created without a name, or, more precisely, with an
  empty name. They can be used only for synchronization between threads within
  one process.

  Process-shared objects are created with non-empty name and can be used for
  synchronization between any threads within a system, including threads in
  different processes.
  Two instances with the same name, created in any process, will be using the
  same synchronization object. Different types of synchronizers can have the
  same name and still be considered separate, so there is no risk of naming
  conflicts (each type of synchronizer has kind-of its own namespace).

  To properly use complex windows synchronization object (TBarrier,
  TConditioVariable, TConditioVariableEx, TReadWriteLock), create one
  progenitor instance and use this instance only in the thread that created it.

  To access the synchronizer in other threads of the same process, create a new
  instance using DuplicateFrom constructor, passing the progenitor instance or
  any duplicate instance based on it as a source. If the progenitor is
  process-shared (ie. named), you can also open it using Create or Open
  construtors, specifying the same name as has the progenitor.

  To access the synchronizer in different process, the progenitor must be
  created as process-shared, that is, it must have a non-empty name. Use Create
  or Open constructors, specifying the same name as has the progenitor.
  The newly created instances will then be using the same synchronization object
  as the progenitor.

    NOTE - DuplicateFrom constructor can be used on both thread-shared and
           process-shared source object, the newly created instance will have
           the same mode as source. For process-shared (named) objects, calling
           this constructor is equivalent to caling Open constructor.
}
type
  TWSOSharedUserData = packed array[0..31] of Byte;
  PWSOSharedUserData = ^TWSOSharedUserData;

  TWSOSharedDataLockType = (sltNone,sltSection,sltMutex);

  TWSOSharedDataLock = record
    case LockType: TWSOSharedDataLockType of
      sltSection: (ThreadSharedLock:  TCriticalSection);
      sltMutex:   (ProcessSharedLock: THandle);   // mutex
      sltNone:    ();
  end;

  // all object shared data must start with this structure
  TWSOCommonSharedData = packed record
    SharedUserData: TWSOSharedUserData;
    RefCount:       Int32;
  end;
  PWSOCommonSharedData = ^TWSOCommonSharedData;

{===============================================================================
    TComplexWinSyncObject - class declaration
===============================================================================}
type
  TComplexWinSyncObject = class(TWinSyncObject)
  protected
    fProcessShared:     Boolean;
    fNamedSharedItem:   TNamedSharedItem;   // unused in thread-shared mode
    fSharedDataLock:    TWSOSharedDataLock;
    fSharedData:        Pointer;
    fFullyInitialized:  Boolean;
    Function GetSharedUserDataPtr: PWSOSharedUserData; virtual;
    Function GetSharedUserData: TWSOSharedUserData; virtual;
    procedure SetSharedUserData(Value: TWSOSharedUserData); virtual;
    Function RectifyAndSetName(const Name: String): Boolean; override;
    procedure CheckAndSetHandle(out Destination: THandle; Handle: THandle); virtual;
    procedure DuplicateAndSetHandle(out Destination: THandle; Handle: THandle); virtual;
    // shared data lock management methods
    class Function LocksSharedData: Boolean; virtual;
    procedure LockSharedData; virtual;
    procedure UnlockSharedData; virtual;
    // shared data management methods
  {
    In the following three methods, the shared data are protected by a global
    lock that ensures no new object can open the same shared data or destroy
    them. Note that this lock is distinct from shared data lock, which is
    NOT in effect here.

    InitSharedData and BindSharedData are obligued to do a full rollback when
    they fail in their respective operations.

    InitSharedData should not assume anything about the shared data.

    BindSharedData should raise an exception if the data are in any way
    inconsistent.

    FinalSharedData must be able to accept partialy initialized or completely
    uninitialized shared data. Also, this function is called only once when the
    shared data are being completely removed from the system.

      NOTE - default implementation of all these methods does nothing.
  }
    procedure InitSharedData; virtual;
    procedure BindSharedData; virtual;
    procedure FinalSharedData; virtual;
    // locks management methods
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); virtual; abstract;
    procedure CreateLocks; virtual; abstract;
    procedure OpenLocks; virtual; abstract;
    procedure CloseLocks; virtual; abstract;  // must be able to accept state where not all locks were successfuly created
    // internal creation
    procedure InternalCreate(const Name: String); virtual;
    procedure InternalOpen(const Name: String); virtual;
    procedure InternalClose; virtual;
    // naming
  {
    ClassNameSuffix must return a string in the form '@xxx_', where xxx must
    be unique for a class/locker type.

    GetDecoratedName returns the object name with appended ClassNameSuffix
    followed by Suffix parameter (this parameter must be exactly three
    characters long, not more, not less).
  }
    class Function GetNameSuffix: String; virtual; abstract;
    Function GetDecoratedName(const Suffix: String): String; virtual;
  public
    constructor Create(const Name: String); overload; virtual;
    constructor Create; overload; virtual;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); virtual;
  {
    Use DuplicateFrom to create instance that accesses the same synchronization
    primitive(s) and shared data as the source object.

    If the source object is process-shared, DuplicateFrom is equivalent to Open
    constructor.
  }
    constructor DuplicateFrom(SourceObject: TComplexWinSyncObject); virtual;
    destructor Destroy; override;
    property ProcessShared: Boolean read fProcessShared;
    property SharedUserDataPtr: PWSOSharedUserData read GetSharedUserDataPtr;
    property SharedUserData: TWSOSharedUserData read GetSharedUserData write SetSharedUserData;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TSimpleBarrier
--------------------------------------------------------------------------------
===============================================================================}
type
  TWSOSimpleBarrierSharedData = packed record
    SharedUserData:   TWSOSharedUserData;
    RefCount:         Int32;
    MaxWaitCount:     Int32;  // invariant value, set only once
    WaitCount:        Int32;  // interlocked access only
  end;
  PWSOSimpleBarrierSharedData = ^TWSOSimpleBarrierSharedData;

{===============================================================================
    TSimpleBarrier - class declaration
===============================================================================}
type
  TSimpleBarrier = class(TComplexWinSyncObject)
  protected
    fEntryLock:         THandle;  // semaphore with initial value and max value of count
    fReleaseLock:       THandle;  // manual-reset event, initially locked
    fBarrierSharedData: PWSOSimpleBarrierSharedData;
    fCount:             Integer;
    class Function LocksSharedData: Boolean; override;
    procedure InitSharedData; override;
    procedure BindSharedData; override;
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); override;
    procedure CreateLocks; override;
    procedure OpenLocks; override;
    procedure CloseLocks; override;
    class Function GetLockType: TWSOLockType; override;
    class Function GetNameSuffix: String; override;
  public
    constructor Create(const Name: String); override;
    constructor Create; override;
    constructor Create(Count: Integer; const Name: String); overload;
    constructor Create(Count: Integer); overload;
    Function Wait: Boolean; virtual;  // returns true if this call released the barrier
    property Count: Integer read fCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                    TBarrier
--------------------------------------------------------------------------------
===============================================================================}
type
  TWSOBarrierSharedData = packed record
    SharedUserData:   TWSOSharedUserData;
    RefCount:         Int32;
    MaxWaitCount:     Int32;  // invariant value, set only once
    WaitCount:        Int32;
    Releasing:        Boolean;
  end;
  PWSOBarrierSharedData = ^TWSOBarrierSharedData;

{===============================================================================
    TBarrier - class declaration
===============================================================================}
type
  TBarrier = class(TComplexWinSyncObject)
  protected
    fEntryLock:         THandle;  // manual-reset event, unlocked
    fReleaseLock:       THandle;  // manual-reset event, locked
    fBarrierSharedData: PWSOBarrierSharedData;
    fCount:             Integer;
    procedure InitSharedData; override;
    procedure BindSharedData; override;
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); override;
    procedure CreateLocks; override;
    procedure OpenLocks; override;
    procedure CloseLocks; override;
    class Function GetLockType: TWSOLockType; override;
    class Function GetNameSuffix: String; override;
  public
    constructor Create(const Name: String); override;
    constructor Create; override;
    constructor Create(Count: Integer; const Name: String); overload;
    constructor Create(Count: Integer); overload;
    Function Wait: Boolean; virtual;
  {
    Releases all waiting threads, irrespective of their count, and sets the
    barrier back to a non-signaled (blocking) state.
  }
    Function Release: Integer; virtual;
    property Count: Integer read fCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TConditionVariable
--------------------------------------------------------------------------------
===============================================================================}
{
  WARNING - be wery cautious about objects passed as DataLock parameter to
            methods Sleep and AutoCycle. If they have been locked multiple
            times before the call (affects mutexes and semaphores), it can
            create a deadlock as the lock is released only once within the
            method (so it can effectively stay locked indefinitely).
}
type
  TWSOCondSharedData = packed record
    SharedUserData: TWSOSharedUserData;
    RefCount:       Int32;
    WaitCount:      Int32;
    WakeCount:      Int32;
    Broadcasting:   Boolean;
  end;
  PWSOCondSharedData = ^TWSOCondSharedData;

  // types for autocycle
  TWSOWakeOption = (woWakeOne,woWakeAll,woWakeBeforeUnlock);
  TWSOWakeOptions = set of TWSOWakeOption;

  TWSOPredicateCheckCallback = procedure(Sender: TObject; var Predicate: Boolean);
  TWSOPredicateCheckEvent = procedure(Sender: TObject; var Predicate: Boolean) of object;

  TWSODataAccessCallback = procedure(Sender: TObject; var WakeOptions: TWSOWakeOptions);
  TWSODataAccessEvent = procedure(Sender: TObject; var WakeOptions: TWSOWakeOptions) of object;

{===============================================================================
    TConditionVariable - class declaration
===============================================================================}
type
  TConditionVariable = class(TComplexWinSyncObject)
  protected
    fWaitLock:                  THandle;  // semaphore, init 0, max $7FFFFFFF
    fBroadcastDoneLock:         THandle;  // manual-reset event, locked
    fCondSharedData:            PWSOCondSharedData;
    // autocycle events
    fOnPredicateCheckCallback:  TWSOPredicateCheckCallback;
    fOnPredicateCheckEvent:     TWSOPredicateCheckEvent;
    fOnDataAccessCallback:      TWSODataAccessCallback;
    fOnDataAccessEvent:         TWSODataAccessEvent;
    procedure InitSharedData; override;
    procedure BindSharedData; override;
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); override;
    procedure CreateLocks; override;
    procedure OpenLocks; override;
    procedure CloseLocks; override;
    class Function GetLockType: TWSOLockType; override;
    class Function GetNameSuffix: String; override;
    // autocycle events firing
    Function DoOnPredicateCheck: Boolean; virtual;
    Function DoOnDataAccess: TWSOWakeOptions; virtual;
    // utility methods
    procedure SelectWake(WakeOptions: TWSOWakeOptions); virtual;
  public
  {
    In both overloads, DataLock parameter can only be an event, mutex or
    semaphore, no other type of synchronizer is supported.
  }
    procedure Sleep(DataLock: THandle; Timeout: DWORD = INFINITE); overload; virtual;
    procedure Sleep(DataLock: TSimpleWinSyncObject; Timeout: DWORD = INFINITE); overload; virtual;
    procedure Wake; virtual;
    procedure WakeAll; virtual;
  {
    First overload of AutoCycle method only supports mutex object as DataLock
    parameter - it is because the object must be signaled and different objects
    have different functions for that, and there is no way of discerning which
    object is hidden behind the handle.
    ...well, there is a way, but it involves function NtQueryObject which
    should not be used in application code, so let's avoid it.

    Second overload allows for event, mutex and semaphore object to be used
    as data synchronizer.
  }
    procedure AutoCycle(DataLock: THandle; Timeout: DWORD = INFINITE); overload; virtual;
    procedure AutoCycle(DataLock: TSimpleWinSyncObject; Timeout: DWORD = INFINITE); overload; virtual;
    // events
    property OnPredicateCheckCallback: TWSOPredicateCheckCallback read fOnPredicateCheckCallback write fOnPredicateCheckCallback;
    property OnPredicateCheckEvent: TWSOPredicateCheckEvent read fOnPredicateCheckEvent write fOnPredicateCheckEvent;
    property OnPredicateCheck: TWSOPredicateCheckEvent read fOnPredicateCheckEvent write fOnPredicateCheckEvent;
    property OnDataAccessCallback: TWSODataAccessCallback read fOnDataAccessCallback write fOnDataAccessCallback;    
    property OnDataAccessEvent: TWSODataAccessEvent read fOnDataAccessEvent write fOnDataAccessEvent;
    property OnDataAccess: TWSODataAccessEvent read fOnDataAccessEvent write fOnDataAccessEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              TConditionVariableEx
--------------------------------------------------------------------------------
===============================================================================}
{
  Only an extension of TConditionVariable with integrated data lock (use
  methods Lock and Unlock to manipulate it). New versions of methods Sleep and
  AutoCycle without the DataLock parameter are using the integrated data lock
  for that purpose.

    WARNING - as in the case of TConditionVariable, be wary of how many times
              you lock the integrated data lock. A mutex is used internally,
              so mutliple locks can result in a deadlock in sleep method.
}
{===============================================================================
    TConditionVariableEx - class declaration
===============================================================================}
type
  TConditionVariableEx = class(TConditionVariable)
  protected
    fDataLock:  THandle;  // mutex, not owned
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); override;
    procedure CreateLocks; override;
    procedure OpenLocks; override;
    procedure CloseLocks; override;
    class Function GetLockType: TWSOLockType; override;
    class Function GetNameSuffix: String; override;
  public
    procedure Lock; virtual;
    procedure Unlock; virtual;
    procedure Sleep(Timeout: DWORD = INFINITE); overload; virtual;
    procedure AutoCycle(Timeout: DWORD = INFINITE); overload; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TReadWriteLock
--------------------------------------------------------------------------------
===============================================================================}
{
  This implementation of read-write lock does not allow for recursive locking
  nor read lock promotion - that is, you cannot acquire write lock in the same
  thread where you are already holding a read or write lock.

    WARNING - trying to acquire write lock in a thread that is currently
              holding read or write lock will create a deadlock.
}
type
  TWSORWLockSharedData = packed record
    SharedUserData: TWSOSharedUserData;
    RefCount:       Int32;
    ReadCount:      Int32;
    WriteWaitCount: Int32;
    Writing:        Boolean;
  end;
  PWSORWLockSharedData = ^TWSORWLockSharedData;

{===============================================================================
    TReadWriteLock - class declaration
===============================================================================}
type
  TReadWriteLock = class(TComplexWinSyncObject)
  protected
    fReadLock:          THandle;    // manual-reset event, unlocked
    fWriteQueueLock:    THandle;    // manual-reset event, unlocked
    fWriteLock:         THandle;    // mutex, not owned
    fRWLockSharedData:  PWSORWLockSharedData;
    procedure InitSharedData; override;
    procedure BindSharedData; override;
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); override;
    procedure CreateLocks; override;
    procedure OpenLocks; override;
    procedure CloseLocks; override;
    class Function GetLockType: TWSOLockType; override;
    class Function GetNameSuffix: String; override;
  public
  {
    ReadLock can only return wrSignaled, wrTimeout or wrError (LastError will
    contain the error code), treat any other returned value as an error (where
    LastError does not contain a valid error code).

      WARNING - the lock is acquired only when wrSignaled is returned!
  }
    Function ReadLock(Timeout: DWORD = INFINITE): TWSOWaitResult; virtual;
    procedure ReadUnlock; virtual;
  {
    See ReadLock for returned value and indication of successful lock acquire.
  }
    Function WriteLock(Timeout: DWORD = INFINITE): TWSOWaitResult; virtual;
    procedure WriteUnlock; virtual;
  end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Wait functions
--------------------------------------------------------------------------------
===============================================================================}
type
{
  Options for message waiting. See MsgWaitForMultipleObjects(Ex) documentation
  for details of message waiting.

  mwoEnable         - enable waiting on messages
  mwoInputAvailable - adds MWMO_INPUTAVAILABLE to flags when message waiting
                      is enabled.
}
  TMessageWaitOption = (mwoEnable,mwoInputAvailable);

  TMessageWaitOptions = set of TMessageWaitOption;

{
  Currently defined values for WakeMask parameter.
}
const
  QS_KEY            = $0001;
  QS_MOUSEMOVE      = $0002;
  QS_MOUSEBUTTON    = $0004;
  QS_POSTMESSAGE    = $0008;
  QS_TIMER          = $0010;
  QS_PAINT          = $0020;
  QS_SENDMESSAGE    = $0040;
  QS_HOTKEY         = $0080;
  QS_ALLPOSTMESSAGE = $0100;
  QS_RAWINPUT       = $0400;
  QS_TOUCH          = $0800;
  QS_POINTER        = $1000;
  QS_MOUSE          = QS_MOUSEMOVE or QS_MOUSEBUTTON;
  QS_INPUT          = QS_MOUSE or QS_KEY or QS_RAWINPUT or QS_TOUCH or QS_POINTER;
  QS_ALLEVENTS      = QS_INPUT or QS_POSTMESSAGE or QS_TIMER or QS_PAINT or QS_HOTKEY;
  QS_ALLINPUT       = QS_INPUT or QS_POSTMESSAGE or QS_TIMER or QS_PAINT or QS_HOTKEY or QS_SENDMESSAGE;

{===============================================================================
--------------------------------------------------------------------------------
                            Wait functions (N <= MAX)
--------------------------------------------------------------------------------
===============================================================================}
{
  Waits on multiple handles - the function does not return until wait criteria
  are met, an error occurs or the wait times-out (which of these occurred is
  indicated by the result).
  
  Handles of the following windows system objects are allowed:

    Change notification
    Console input
    Event
    Memory resource notification
    Mutex
    Process
    Semaphore
    Thread
    Waitable timer

  Handle array must not be empty and length (Count) must be less than or equal
  to 64 (63 when message waiting is enabled), otherwise an exception of type
  EWSOMultiWaitInvalidCount will be raised.

  If WaitAll is set to true, the function will return wrSignaled only when ALL
  objects are signaled. When set to false, it will return wrSignaled when at
  least one object becomes signaled.

  Timeout is in milliseconds. Default value for Timeout is INFINITE.

  When WaitAll is false, Index indicates which object was signaled or abandoned
  when wrSignaled or wrAbandoned is returned. In case of wrError, the Index
  contains a system error number. For other results, the value of Index is
  undefined.
  When WaitAll is true, value of Index is undefined except for wrError, where
  it again contains system error number.

  If Alertable is true, the function will also return if APC has been queued
  to the waiting thread. Default value for Alertable is False.

  Use set argument MsgWaitOptions to enable and configure message waiting.
  Default value is an empty set, meaning message waiting is disabled.

  Argument WakeMask is used without change when message waiting is enabled
  (it prescribes which messages to observe), otherwise it is ignored. Use
  bitwise OR to combine multiple values. Default value is zero.
}
Function WaitForMultipleHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD = QS_ALLINPUT): TWSOWaitResult; overload;
Function WaitForMultipleHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForMultipleHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForMultipleHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean): TWSOWaitResult; overload;

//------------------------------------------------------------------------------
{
  Following functions are behaving the same as the ones accepting pointer to
  handle array, see there for details.
}
Function WaitForMultipleHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD = QS_ALLINPUT): TWSOWaitResult; overload;
Function WaitForMultipleHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForMultipleHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForMultipleHandles(Handles: array of THandle; WaitAll: Boolean): TWSOWaitResult; overload;

//------------------------------------------------------------------------------
{
  Functions WaitForMultipleObjects are, again, behaving exactly the same as
  WaitForMultipleHandles.

    NOTE - LastError property of the passed objects is not set by these
           functions. Possible error code is returned in Index output parameter.
}
Function WaitForMultipleObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD = QS_ALLINPUT): TWSOWaitResult; overload;
Function WaitForMultipleObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForMultipleObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForMultipleObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean): TWSOWaitResult; overload;

{===============================================================================
--------------------------------------------------------------------------------
                            Wait functions (N > MAX)
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
                              >>> WARNING <<<
--------------------------------------------------------------------------------

  Waiting on many objects is not completely tested and, given its complexity,
  should be considered strictly experimental.

--------------------------------------------------------------------------------

  Waiting for many objects - generally, these functions behave the same as
  WaitForMultipleObjects and can accept the same object types. But given the
  implementation (described further), there are serious limitations that,
  unfortunately, cannot be eliminated.

  First, it is not possible to use them for mutexes. Mutex is owned by a thread
  that finished waiting on it. And since the implementation works with waiter
  threads, the ownership is granted to the waiter thread and not the thread that
  invoked the waiting. The waiter threads are created and destroyed ad-hoc, so
  when the waiting returns, the mutex will already be abandoned.

  Second, when waiting on only one object to become signaled, it cannot be
  guaranteed that the (by index) indicated object was the first to become
  signaled. In fact, it cannot be guaranteed that only one object was signaled
  before the waiting ended. This is especially problematic for objects with
  mutable state (eg. synchronization objects) - the waiting changes their state
  and, because there are multiple independent waitings, more than one object
  can be changed. Moreover, it cannot be discerned which objects, or how many
  of them, were affected beyond the indicated one.

  To sum it up, it is highly discouraged to use waiting on many objects with
  mutable state. You can safely use it for waiting on threads, processes, and
  to some extent on manual-reset events and timers.

    NOTE - When calling WaitForMany[Handles/Objects] with number of wait
           objects/handles lower than the maximum enforced for the system
           calls, it will only call the WaitForMultipleHandles funtion.

  And now for some implementation details...

    Since Windows API function WaitForMultipleObjects does not allow for more
    than 64 or 63 objects to be waited on (63 in case of message waiting - that
    is, waiting that can return when a message is delivered to the waiting
    thread), we have to do our own implementation for cases where waiting on
    more objects is required.
    Generally, waiting on more than about a dozen objects is a sign of bad
    design, but here we go...

      The handles we want to wait on are split equally between waiter treads
      and we are then waiting on these threads to finish.

      Each waiter thread either waits on its share of handles directly, if they
      fit into system wait call, or spawns another waiter threads and splits
      handles again between them.

      It is also possible that any waiter thread combines both options (waits
      on some handles and the rest is passed to another thread(s)) if the
      number of handles allows it.

      This creates a tree-like waiting structure.
}
Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD = QS_ALLINPUT): TWSOWaitResult; overload;
Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean): TWSOWaitResult; overload;

//------------------------------------------------------------------------------

Function WaitForManyHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD = QS_ALLINPUT): TWSOWaitResult; overload;
Function WaitForManyHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForManyHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForManyHandles(Handles: array of THandle; WaitAll: Boolean): TWSOWaitResult; overload;

//------------------------------------------------------------------------------
{
  Functions WaitForManyObjects are behaving exactly the same as functions
  WaitForManyHandles, refer there for details.

  These functions are implemented only for the completeness sake, as using them
  is not recommended (see description of WaitForManyHandles for why).

    NOTE - LastError property of the passed objects is not set by these
           functions. Possible error code is returned in Index output parameter.
}
Function WaitForManyObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD = QS_ALLINPUT): TWSOWaitResult; overload;
Function WaitForManyObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForManyObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForManyObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean): TWSOWaitResult; overload;

//------------------------------------------------------------------------------

type
  TWSOManyWaitDebugInfo = record
    Succeeded:    Boolean;
    ThreadCount:  Integer;
    BranchWaits:  Integer;
    LeafWaits:    Integer;
  end;

// only for debuging purposes, but is left public in release code   
Function WaitForManyHandles_GetDebugInfo: TWSOManyWaitDebugInfo;


{===============================================================================
--------------------------------------------------------------------------------
                               Utility functions
--------------------------------------------------------------------------------
===============================================================================}
{
  WaitResultToStr simply returns textual representation of a given wait result.

  It is meant mainly for debugging purposes.
}
Function WaitResultToStr(WaitResult: TWSOWaitResult): String;

implementation

uses
  Classes, Math,
  StrRect, InterlockedOps, UInt64Utils;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5057:={$WARN 5057 OFF}} // Local variable "$1" does not seem to be initialized
  {$DEFINE W5058:={$WARN 5058 OFF}} // Variable "$1" does not seem to be initialized
{$ENDIF}

{===============================================================================
    Internals
===============================================================================}
type
  TWSOTimestamp = Int64;

{$IF not Declared(UNICODE_STRING_MAX_CHARS)}
const
  UNICODE_STRING_MAX_CHARS = 32767;
{$IFEND}

//------------------------------------------------------------------------------
{
  Workaround for known WinAPI bug - CreateMutex only recognizes BOOL(1) as
  true, everything else, including what Delphi/FPC puts there (ie. BOOL(-1)),
  is wrongly seen as false. :/
}
Function RectBool(Value: Boolean): BOOL;
begin
If Value then Result := BOOL(1)
  else Result := BOOL(0);
end;

//------------------------------------------------------------------------------

Function GetTimestamp: TWSOTimestamp;
begin
Result := 0;
If not QueryPerformanceCounter(Result) then
  raise EWSOTimestampError.CreateFmt('GetTimestamp: Cannot obtain time stamp (%d).',[GetLastError]);
Result := Result and $7FFFFFFFFFFFFFFF; // mask out sign bit
end;

//------------------------------------------------------------------------------

Function RecalculateTimeout(TimeoutFull: UInt32; StartTime: TWSOTimestamp; out TimeoutRemaining: UInt32): Boolean;

  Function GetElapsedMillis: UInt32;
  var
    CurrentTime:  TWSOTimestamp;
    Temp:         Int64;
  begin
    CurrentTime := GetTimestamp;
    If CurrentTime >= StartTime then
      begin
        Temp := 1;
        If QueryPerformanceFrequency(Temp) then
          Temp := Trunc(((CurrentTime - StartTime) / Temp) * 1000)
        else
          raise EWSOTimestampError.CreateFmt('RecalculateTimeout.GetElapsedMillis: Failed to obtain timer frequency (%d).',[GetLastError]);
        If Temp < INFINITE then
          Result := UInt32(Temp)
        else
          Result := INFINITE;
      end
    else Result := INFINITE;
  end;

var
  ElapsedMillis:  UInt32;
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
  ElapsedMillis := GetElapsedMillis;
  If ElapsedMillis < TimeoutFull then
    TimeoutRemaining := TimeoutFull - ElapsedMillis
  else
    TimeoutRemaining := 0;
end;
Result := TimeoutRemaining <> 0;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TCriticalSection
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCriticalSection - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCriticalSection - protected methods
-------------------------------------------------------------------------------}

Function TCriticalSection.GetSpinCount: DWORD;
begin
Result := InterlockedLoad(fSpinCount);
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.SetSpinCountProc(Value: DWORD);
begin
SetSpinCount(Value);
end;

{-------------------------------------------------------------------------------
    TCriticalSection - public methods
-------------------------------------------------------------------------------}

constructor TCriticalSection.Create;
begin
inherited Create;
fSpinCount := 0;
InitializeCriticalSection(fCriticalSectionObj);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCriticalSection.Create(SpinCount: DWORD);
begin
inherited Create;
fSpinCount := SpinCount;
If not InitializeCriticalSectionAndSpinCount(fCriticalSectionObj,SpinCount) then
  raise EWSOInitializationError.CreateFmt('TCriticalSection.Create: Failed to initialize critical section with spin count (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

destructor TCriticalSection.Destroy;
begin
DeleteCriticalSection(fCriticalSectionObj);
inherited;
end;

//------------------------------------------------------------------------------

Function TCriticalSection.SetSpinCount(SpinCount: DWORD): DWORD;
begin
InterlockedStore(fSpinCount,SpinCount);
Result := SetCriticalSectionSpinCount(fCriticalSectionObj,SpinCount);
end;

//------------------------------------------------------------------------------

Function TCriticalSection.TryEnter: Boolean;
begin
Result := TryEnterCriticalSection(fCriticalSectionObj);
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.Enter;
begin
EnterCriticalSection(fCriticalSectionObj);
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.Leave;
begin
LeaveCriticalSection(fCriticalSectionObj);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TWinSyncObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinSyncObject - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TWinSyncObject - protected methods
-------------------------------------------------------------------------------}

Function TWinSyncObject.RectifyAndSetName(const Name: String): Boolean;
begin
fName := Name;
Result := True;
end;

{-------------------------------------------------------------------------------
    TWinSyncObject - public methods
-------------------------------------------------------------------------------}

constructor TWinSyncObject.Create;
begin
inherited Create;
fLastError := 0;
fName := '';
end;

{===============================================================================
--------------------------------------------------------------------------------
                              TSimpleWinSyncObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleWinSyncObject - class implentation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleWinSyncObject - protected methods
-------------------------------------------------------------------------------}

Function TSimpleWinSyncObject.RectifyAndSetName(const Name: String): Boolean;
begin
inherited RectifyAndSetName(Name);  // only sets the fName field
{
  Names should not contain backslashes (\, #92), but they can separate
  prefixes, so in theory they are allowed - do not replace them, leave this
  responsibility on the user.

  Wide-string version of WinAPI functions are used, but there is still strict
  limit of 32767 wide characters in a string.
}
If Length(fName) > UNICODE_STRING_MAX_CHARS then
  SetLength(fName,UNICODE_STRING_MAX_CHARS);
Result := Length(fName) > 0;
end;

//------------------------------------------------------------------------------

procedure TSimpleWinSyncObject.CheckAndSetHandle(Handle: THandle);
begin
If Handle <> 0 then
  fHandle := Handle
else
  raise EWSOInvalidHandle.CreateFmt('TSimpleWinSyncObject.CheckAndSetHandle: Null handle (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

procedure TSimpleWinSyncObject.DuplicateAndSetHandle(SourceProcess: THandle; SourceHandle: THandle);
var
  NewHandle:  THandle;
begin
If DuplicateHandle(SourceProcess,SourceHandle,GetCurrentProcess,@NewHandle,0,False,DUPLICATE_SAME_ACCESS) then
  CheckAndSetHandle(NewHandle)
else
  raise EWSOHandleDuplicationError.CreateFmt('TSimpleWinSyncObject.DuplicateAndSetHandle: Handle duplication failed (%d).',[GetLastError]);
end;

{-------------------------------------------------------------------------------
    TSimpleWinSyncObject - public methods
-------------------------------------------------------------------------------}

constructor TSimpleWinSyncObject.CreateFrom(Handle: THandle{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
fHandle := Handle;
If fHandle = 0 then
  raise EWSOInvalidHandle.Create('TSimpleWinSyncObject.CreateFrom: Null handle.');
end;

//------------------------------------------------------------------------------

constructor TSimpleWinSyncObject.DuplicateFrom(SourceHandle: THandle);
begin
inherited Create;
DuplicateAndSetHandle(GetCurrentProcess,SourceHandle);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSimpleWinSyncObject.DuplicateFrom(SourceObject: TSimpleWinSyncObject);
begin
inherited Create;
If SourceObject.GetLockType = Self.GetLockType then
  DuplicateAndSetHandle(GetCurrentProcess,SourceObject.Handle)
else
  raise EWSOInvalidObject.CreateFmt('TSimpleWinSyncObject.DuplicateFrom: Incompatible source object (%s).',[SourceObject.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TSimpleWinSyncObject.DuplicateFromProcess(SourceProcess: THandle; SourceHandle: THandle);
begin
inherited Create;
DuplicateAndSetHandle(SourceProcess,SourceHandle);
end;

//------------------------------------------------------------------------------

constructor TSimpleWinSyncObject.DuplicateFromProcessID(SourceProcessID: DWORD; SourceHandle: THandle);
var
  SourceProcess:  THandle;
begin
inherited Create;
SourceProcess := OpenProcess(PROCESS_DUP_HANDLE,False,SourceProcessID);
If SourceProcess <> 0 then
  try
    DuplicateAndSetHandle(SourceProcess,SourceHandle);
  finally
    CloseHandle(SourceProcess);
  end
else raise EWSOHandleDuplicationError.CreateFmt('TSimpleWinSyncObject.DuplicateFromProcessID: Failed to open source process (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

destructor TSimpleWinSyncObject.Destroy;
begin
CloseHandle(fHandle);
inherited;
end;

//------------------------------------------------------------------------------

Function TSimpleWinSyncObject.DuplicateForProcess(TargetProcess: THandle): THandle;
begin
If not DuplicateHandle(GetCurrentProcess,fHandle,TargetProcess,@Result,0,False,DUPLICATE_SAME_ACCESS) then
  raise EWSOHandleDuplicationError.CreateFmt('TSimpleWinSyncObject.DuplicateForProcess: Handle duplication failed (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

Function TSimpleWinSyncObject.DuplicateForProcessID(TargetProcessID: DWORD): THandle;
var
  TargetProcess:  THandle;
begin
TargetProcess := OpenProcess(PROCESS_DUP_HANDLE,False,TargetProcessID);
If TargetProcess <> 0 then
  try
    Result := DuplicateForProcess(TargetProcess);
  finally
    CloseHandle(TargetProcess);
  end
else raise EWSOHandleDuplicationError.CreateFmt('TSimpleWinSyncObject.DuplicateForProcessID: Failed to open target process (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

Function TSimpleWinSyncObject.WaitFor(Timeout: DWORD; out ErrCode: DWORD; Alertable: Boolean = False): TWSOWaitResult;
begin
ErrCode := 0;
case WaitForSingleObjectEx(fHandle,Timeout,Alertable) of
  WAIT_OBJECT_0:      Result := wrSignaled;
  WAIT_ABANDONED:     Result := wrAbandoned;
  WAIT_IO_COMPLETION: Result := wrIOCompletion;
  WAIT_TIMEOUT:       Result := wrTimeout;
  WAIT_FAILED:        begin
                        Result := wrError;
                        ErrCode := GetLastError;
                      end;
else
  Result := wrError;
  ErrCode := GetLastError;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TSimpleWinSyncObject.WaitFor(Timeout: DWORD = INFINITE; Alertable: Boolean = False): TWSOWaitResult;
begin
Result := WaitFor(Timeout,fLastError,Alertable);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                     TEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TEvent - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TEvent - protected methods
-------------------------------------------------------------------------------}

class Function TEvent.GetLockType: TWSOLockType;
begin
Result := ltEvent;
end;

{-------------------------------------------------------------------------------
    TEvent - public methods
-------------------------------------------------------------------------------}

constructor TEvent.Create(SecurityAttributes: PSecurityAttributes; ManualReset, InitialState: Boolean; const Name: String);
begin
inherited Create;
If RectifyAndSetName(Name) then
  CheckAndSetHandle(CreateEventW(SecurityAttributes,ManualReset,InitialState,PWideChar(StrToWide(fName))))
else
  CheckAndSetHandle(CreateEventW(SecurityAttributes,ManualReset,InitialState,nil));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create(ManualReset, InitialState: Boolean; const Name: String);
begin
Create(nil,ManualReset,InitialState,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create(ManualReset, InitialState: Boolean);
begin
Create(nil,ManualReset,InitialState,'');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create(const Name: String);
begin
Create(nil,True,False,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create;
begin
Create(nil,True,False,'');
end;

//------------------------------------------------------------------------------

constructor TEvent.Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String);
begin
inherited Create;
If RectifyAndSetName(Name) then
  CheckAndSetHandle(OpenEventW(DesiredAccess,InheritHandle,PWideChar(StrToWide(fName))))
else
  raise EWSOOpenError.Create('TEvent.Open: Empty name not allowed.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
Open(SYNCHRONIZE or EVENT_MODIFY_STATE,False,Name);
end;

//------------------------------------------------------------------------------

Function TEvent.WaitForAndReset(Timeout: DWORD = INFINITE; Alertable: Boolean = False): TWSOWaitResult;
begin
Result := WaitFor(Timeout,Alertable);
If Result = wrSignaled then
  ResetEventStrict;
end;

//------------------------------------------------------------------------------

procedure TEvent.SetEventStrict;
begin
If not Windows.SetEvent(fHandle) then
  raise EWSOEventError.CreateFmt('TEvent.SetEventStrict: Failed to set event (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

Function TEvent.SetEvent: Boolean;
begin
Result := Windows.SetEvent(fHandle);
If not Result then
  fLastError := GetLastError;
end;

//------------------------------------------------------------------------------

procedure TEvent.ResetEventStrict;
begin
If not Windows.ResetEvent(fHandle) then
  raise EWSOEventError.CreateFmt('TEvent.ResetEventStrict: Failed to reset event (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

Function TEvent.ResetEvent: Boolean;
begin
Result := Windows.ResetEvent(fHandle);
If not Result then
  fLastError := GetLastError;
end;

//------------------------------------------------------------------------------

{$WARN SYMBOL_DEPRECATED OFF}
procedure TEvent.PulseEventStrict;
{$WARN SYMBOL_DEPRECATED ON}
begin
If not Windows.PulseEvent(fHandle) then
  raise EWSOEventError.CreateFmt('TEvent.PulseEventStrict: Failed to pulse event (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

{$WARN SYMBOL_DEPRECATED OFF}
Function TEvent.PulseEvent: Boolean;
{$WARN SYMBOL_DEPRECATED ON}
begin
Result := Windows.PulseEvent(fHandle);
If not Result then
  fLastError := GetLastError;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                     TMutex
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMutex - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMutex - protected methods
-------------------------------------------------------------------------------}

class Function TMutex.GetLockType: TWSOLockType;
begin
Result := ltMutex;
end;

{-------------------------------------------------------------------------------
    TMutex - public methods
-------------------------------------------------------------------------------}

constructor TMutex.Create(SecurityAttributes: PSecurityAttributes; InitialOwner: Boolean; const Name: String);
begin
inherited Create;
If RectifyAndSetName(Name) then
  CheckAndSetHandle(CreateMutexW(SecurityAttributes,RectBool(InitialOwner),PWideChar(StrToWide(fName))))
else
  CheckAndSetHandle(CreateMutexW(SecurityAttributes,RectBool(InitialOwner),nil));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMutex.Create(InitialOwner: Boolean; const Name: String);
begin
Create(nil,InitialOwner,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMutex.Create(InitialOwner: Boolean);
begin
Create(nil,InitialOwner,'');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMutex.Create(const Name: String);
begin
Create(nil,False,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMutex.Create;
begin
Create(nil,False,'');
end;

//------------------------------------------------------------------------------

constructor TMutex.Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String);
begin
inherited Create;
If RectifyAndSetName(Name) then
  CheckAndSetHandle(OpenMutexW(DesiredAccess,InheritHandle,PWideChar(StrToWide(fName))))
else
  raise EWSOOpenError.Create('TMutex.Open: Empty name not allowed.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMutex.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
Open(SYNCHRONIZE or MUTEX_MODIFY_STATE,False,Name);
end;

//------------------------------------------------------------------------------

Function TMutex.WaitForAndRelease(TimeOut: DWORD = INFINITE; Alertable: Boolean = False): TWSOWaitResult;
begin
Result := WaitFor(Timeout,Alertable);
If Result in [wrSignaled,wrAbandoned] then
  ReleaseMutexStrict;
end;

//------------------------------------------------------------------------------

procedure TMutex.ReleaseMutexStrict;
begin
If not Windows.ReleaseMutex(fHandle) then
  raise EWSOMutexError.CreateFmt('TMutex.ReleaseMutexStrict: Failed to release mutex (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

Function TMutex.ReleaseMutex: Boolean;
begin
Result := Windows.ReleaseMutex(fHandle);
If not Result then
  fLastError := GetLastError;
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

class Function TSemaphore.GetLockType: TWSOLockType;
begin
Result := ltSemaphore;
end;

{-------------------------------------------------------------------------------
    TSemaphore - public methods
-------------------------------------------------------------------------------}

constructor TSemaphore.Create(SecurityAttributes: PSecurityAttributes; InitialCount, MaximumCount: Integer; const Name: String);
begin
inherited Create;
If RectifyAndSetName(Name) then
  CheckAndSetHandle(CreateSemaphoreW(SecurityAttributes,InitialCount,MaximumCount,PWideChar(StrToWide(fName))))
else
  CheckAndSetHandle(CreateSemaphoreW(SecurityAttributes,InitialCount,MaximumCount,nil));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSemaphore.Create(InitialCount, MaximumCount: Integer; const Name: String);
begin
Create(nil,InitialCount,MaximumCount,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSemaphore.Create(InitialCount, MaximumCount: Integer);
begin
Create(nil,InitialCount,MaximumCount,'');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSemaphore.Create(const Name: String);
begin
Create(nil,1,1,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSemaphore.Create;
begin
Create(nil,1,1,'');
end;

//------------------------------------------------------------------------------

constructor TSemaphore.Open(DesiredAccess: LongWord; InheritHandle: Boolean; const Name: String);
begin
inherited Create;
If RectifyAndSetName(Name) then
  CheckAndSetHandle(OpenSemaphoreW(DesiredAccess,InheritHandle,PWideChar(StrToWide(fName))))
else
  raise EWSOOpenError.Create('TSemaphore.Open: Empty name not allowed.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSemaphore.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
Open(SYNCHRONIZE or SEMAPHORE_MODIFY_STATE,False,Name);
end;
 
//------------------------------------------------------------------------------

Function TSemaphore.WaitForAndRelease(TimeOut: LongWord = INFINITE; Alertable: Boolean = False): TWSOWaitResult;
begin
Result := WaitFor(Timeout,Alertable);
If Result = wrSignaled then
  ReleaseSemaphoreStrict;
end;

//------------------------------------------------------------------------------

procedure TSemaphore.ReleaseSemaphoreStrict(ReleaseCount: Integer; out PreviousCount: Integer);
begin
If not Windows.ReleaseSemaphore(fHandle,ReleaseCount,@PreviousCount) then
  raise EWSOSemaphoreError.CreateFmt('TSemaphore.ReleaseSemaphoreStrict: Failed to release semaphore (%d).',[GetLastError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSemaphore.ReleaseSemaphoreStrict;
var
  Dummy:  Integer;
begin
ReleaseSemaphoreStrict(1,Dummy);
end;

//------------------------------------------------------------------------------

Function TSemaphore.ReleaseSemaphore(ReleaseCount: Integer; out PreviousCount: Integer): Boolean;
begin
Result := Windows.ReleaseSemaphore(fHandle,ReleaseCount,@PreviousCount);
If not Result then
  fLastError := GetLastError;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TSemaphore.ReleaseSemaphore: Boolean;
var
  Dummy:  Integer;
begin
Result := ReleaseSemaphore(1,Dummy);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TWaitableTimer
--------------------------------------------------------------------------------
===============================================================================}

Function WinSetWaitableTimer(
  hTimer:                   THandle;
  pDueTime:                 PInt64;
  lPeriod:                  LongInt;
  pfnCompletionRoutine:     TTimerAPCRoutine; // TTimerAPCRoutine is internally a pointer
  lpArgToCompletionRoutine: Pointer;
  fResume:                  BOOL): BOOL; stdcall; external kernel32 name 'SetWaitableTimer';

{===============================================================================
    TWaitableTimer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TWaitableTimer - protected methods
-------------------------------------------------------------------------------}

class Function TWaitableTimer.GetLockType: TWSOLockType;
begin
Result := ltWaitTimer;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
Function TWaitableTimer.DateTimeToFileTime(DateTime: TDateTime): TFileTime;
var
  LocalTime:  TFileTime;
  SystemTime: TSystemTime;
begin
Result.dwLowDateTime := 0;
Result.dwHighDateTime := 0;
DateTimeToSystemTime(DateTime,SystemTime);
If SystemTimeToFileTime(SystemTime,LocalTime) then
  begin
    If not LocalFileTimeToFileTime(LocalTime,Result) then
      raise EWSOTimeConversionError.CreateFmt('TWaitableTimer.DateTimeToFileTime: LocalFileTimeToFileTime failed (%d).',[GetLastError]);
  end
else raise EWSOTimeConversionError.CreateFmt('TWaitableTimer.DateTimeToFileTime: SystemTimeToFileTime failed (%d).',[GetLastError]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{-------------------------------------------------------------------------------
    TWaitableTimer - public methods
-------------------------------------------------------------------------------}

constructor TWaitableTimer.Create(SecurityAttributes: PSecurityAttributes; ManualReset: Boolean; const Name: String);
begin
inherited Create;
If RectifyAndSetName(Name) then
  CheckAndSetHandle(CreateWaitableTimerW(SecurityAttributes,ManualReset,PWideChar(StrToWide(fName))))
else
  CheckAndSetHandle(CreateWaitableTimerW(SecurityAttributes,ManualReset,nil));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TWaitableTimer.Create(ManualReset: Boolean; const Name: String);
begin
Create(nil,ManualReset,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TWaitableTimer.Create(ManualReset: Boolean);
begin
Create(nil,ManualReset,'');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TWaitableTimer.Create(const Name: String);
begin
Create(nil,True,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TWaitableTimer.Create;
begin
Create(nil,True,'');
end;

//------------------------------------------------------------------------------

constructor TWaitableTimer.Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String);
begin
inherited Create;
If RectifyAndSetName(Name) then
  CheckAndSetHandle(OpenWaitableTimerW(DesiredAccess,InheritHandle,PWideChar(StrToWide(fName))))
else
  raise EWSOOpenError.Create('TWaitableTimer.Open: Empty name not allowed.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TWaitableTimer.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
Open(SYNCHRONIZE or TIMER_MODIFY_STATE,False,Name);
end;

//------------------------------------------------------------------------------

procedure TWaitableTimer.SetWaitableTimerStrict(DueTime: Int64; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean);
begin
If not WinSetWaitableTimer(fHandle,@DueTime,Period,CompletionRoutine,ArgToCompletionRoutine,Resume) then
  raise EWSOTimerError.CreateFmt('TWaitableTimer.SetWaitableTimerStrict: Failed to set timer (%d).',[GetLastError]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TWaitableTimer.SetWaitableTimerStrict(DueTime: Int64; Period: Integer = 0);
begin
SetWaitableTimerStrict(DueTime,Period,nil,nil,False);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TWaitableTimer.SetWaitableTimerStrict(DueTime: TDateTime; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean);
begin
SetWaitableTimerStrict(Int64(DateTimeToFileTime(DueTime)),Period,CompletionRoutine,ArgToCompletionRoutine,Resume);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TWaitableTimer.SetWaitableTimerStrict(DueTime: TDateTime; Period: Integer = 0);
begin
SetWaitableTimerStrict(DueTime,Period,nil,nil,False);
end;

//------------------------------------------------------------------------------

Function TWaitableTimer.SetWaitableTimer(DueTime: Int64; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean): Boolean;
begin
Result := WinSetWaitableTimer(fHandle,@DueTime,Period,CompletionRoutine,ArgToCompletionRoutine,Resume);
If not Result then
  fLastError := GetLastError;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TWaitableTimer.SetWaitableTimer(DueTime: Int64; Period: Integer = 0): Boolean;
begin
Result := SetWaitableTimer(DueTime,Period,nil,nil,False);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TWaitableTimer.SetWaitableTimer(DueTime: TDateTime; Period: Integer; CompletionRoutine: TTimerAPCRoutine; ArgToCompletionRoutine: Pointer; Resume: Boolean): Boolean;
begin
Result := SetWaitableTimer(Int64(DateTimeToFileTime(DueTime)),Period,CompletionRoutine,ArgToCompletionRoutine,Resume);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TWaitableTimer.SetWaitableTimer(DueTime: TDateTime; Period: Integer = 0): Boolean;
begin
Result := SetWaitableTimer(DueTime,Period,nil,nil,False);
end;

//------------------------------------------------------------------------------

procedure TWaitableTimer.CancelWaitableTimerStrict;
begin
If not Windows.CancelWaitableTimer(fHandle) then
  raise EWSOTimerError.CreateFmt('TWaitableTimer.CancelWaitableTimerStrict: Failed to cancel timer (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

Function TWaitableTimer.CancelWaitableTimer: Boolean;
begin
Result := Windows.CancelWaitableTimer(fHandle);
If not Result then
  fLastError := GetLastError;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              TComplexWinSyncObject
--------------------------------------------------------------------------------
===============================================================================}
const
  WSO_CPLX_SHARED_NAMESPACE = 'wso_shared';

  WSO_CPLX_SUFFIX_LENGTH = 8; // all suffixes must have the same length

const
  WSO_CPLX_SUFFIX_SHAREDDATA     = 'shr';
  WSO_CPLX_SUFFIX_SHAREDDATALOCK = 'slk';

//------------------------------------------------------------------------------

Function WSO_CPLX_SHARED_ITEMSIZE: Integer; // originally a constant
begin
Result := MaxIntValue([
  SizeOf(TWSOBarrierSharedData),
  SizeOf(TWSOCondSharedData),
  SizeOf(TWSORWLockSharedData)]);
end;

//------------------------------------------------------------------------------

// because there are incorrect declarations...
Function SignalObjectAndWait(
  hObjectToSignal:  THandle;
  hObjectToWaitOn:  THandle;
  dwMilliseconds:   DWORD;
  bAlertable:       BOOL): DWORD; stdcall; external kernel32;

//------------------------------------------------------------------------------
var
  // used for data integrity when creating/destroying thread-shared locks
  WSO_SHAREDDATA_THREADLOCK:  TCriticalSection;

{===============================================================================
    TComplexWinSyncObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TComplexWinSyncObject - protected methods
-------------------------------------------------------------------------------}

Function TComplexWinSyncObject.GetSharedUserDataPtr: PWSOSharedUserData;
begin
Result := Addr(PWSOCommonSharedData(fSharedData)^.SharedUserData);
end;

//------------------------------------------------------------------------------

Function TComplexWinSyncObject.GetSharedUserData: TWSOSharedUserData;
begin
Move(GetSharedUserDataPtr^,Addr(Result)^,SizeOf(TWSOSharedUserData));
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.SetSharedUserData(Value: TWSOSharedUserData);
begin
Move(Value,GetSharedUserDataPtr^,SizeOf(TWSOSharedUserData));
end;

//------------------------------------------------------------------------------

Function TComplexWinSyncObject.RectifyAndSetName(const Name: String): Boolean;
begin
inherited RectifyAndSetName(Name);  // should always return true
If (Length(fName) + WSO_CPLX_SUFFIX_LENGTH) > UNICODE_STRING_MAX_CHARS then
  SetLength(fName,UNICODE_STRING_MAX_CHARS - WSO_CPLX_SUFFIX_LENGTH);
fProcessShared := Length(fName) > 0;
Result := fProcessShared;
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.CheckAndSetHandle(out Destination: THandle; Handle: THandle);
begin
If Handle <> 0 then
  Destination := Handle
else
  raise EWSOInvalidHandle.CreateFmt('TComplexWinSyncObject.CheckAndSetHandle: Null handle (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.DuplicateAndSetHandle(out Destination: THandle; Handle: THandle);
var
  NewHandle:  THandle;
begin
If DuplicateHandle(GetCurrentProcess,Handle,GetCurrentProcess,@NewHandle,0,False,DUPLICATE_SAME_ACCESS) then
  CheckAndSetHandle(Destination,NewHandle)
else
  raise EWSOHandleDuplicationError.CreateFmt('TComplexWinSyncObject.DuplicateAndSetHandle: Handle duplication failed (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

class Function TComplexWinSyncObject.LocksSharedData: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.LockSharedData;
begin
case fSharedDataLock.LockType of
  sltSection: fSharedDataLock.ThreadSharedLock.Enter;
  sltMutex:   case WaitForSingleObject(fSharedDataLock.ProcessSharedLock,INFINITE) of
                WAIT_OBJECT_0,
                WAIT_ABANDONED:;  // good result, do nothing
                WAIT_FAILED:
                  raise EWSOWaitError.CreateFmt('TComplexWinSyncObject.LockSharedData: Data lock not acquired (%d).',[GetLastError]);
              else
                raise EWSOWaitError.Create('TComplexWinSyncObject.LockSharedData: Data lock not acquired.');
              end;
end;
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.UnlockSharedData;
begin
case fSharedDataLock.LockType of
  sltSection: fSharedDataLock.ThreadSharedLock.Leave;
  sltMutex:   If not ReleaseMutex(fSharedDataLock.ProcessSharedLock) then
                raise EWSOMutexError.CreateFmt('TComplexWinSyncObject.UnlockSharedData: Data lock not released (%d).',[GetLastError]);
end;
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.InitSharedData;
begin
// do nothing
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.BindSharedData;
begin
// do nothing
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.FinalSharedData;
begin
// do nothing
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.InternalCreate(const Name: String);
begin
fFullyInitialized := False; // just to be sure it is really false 
If RectifyAndSetName(Name) then
  begin
    // process-shared object
    // alocate shared data
    fNamedSharedItem := TNamedSharedItem.CreateLocked(GetDecoratedName(WSO_CPLX_SUFFIX_SHAREDDATA),
      WSO_CPLX_SHARED_ITEMSIZE,WSO_CPLX_SHARED_NAMESPACE);
    try
      try
        // create shared data lock
        If LocksSharedData then
          begin
            CheckAndSetHandle(fSharedDataLock.ProcessSharedLock,
              CreateMutexW(nil,RectBool(False),PWideChar(StrToWide(GetDecoratedName(WSO_CPLX_SUFFIX_SHAREDDATALOCK)))));
            fSharedDataLock.LockType := sltMutex;
          end
        else fSharedDataLock.LockType := sltNone;
        // init shared data
        fSharedData := fNamedSharedItem.Memory;
        Inc(PWSOCommonSharedData(fSharedData)^.RefCount);
        try
          If PWSOCommonSharedData(fSharedData)^.RefCount > 1 then
            begin
              // data were opened
              BindSharedData;
              OpenLocks;    // open main locks
            end
          else
            begin
              // data were newly created
              InitSharedData;
              CreateLocks;  // create main locks
            end;
          fFullyInitialized := True;
        except
          // inner rollback
          If Assigned(fSharedData) then
            begin
              CloseLocks;
              Dec(PWSOCommonSharedData(fSharedData)^.RefCount);
              If PWSOCommonSharedData(fSharedData)^.RefCount <= 0 then
                begin
                  PWSOCommonSharedData(fSharedData)^.RefCount := 0;
                  FinalSharedData;
                end;
            end;
          raise;  // re-raise the exception
        end;
      except
        // outer rollback
        fSharedData := nil;
        If fSharedDataLock.LockType = sltMutex then
          CloseHandle(fSharedDataLock.ProcessSharedLock);
        FreeAndNil(fNamedSharedItem);
        raise;
      end;
    finally
      fNamedSharedItem.GlobalUnlock;
    end;
  end
else
  begin
    // thread-shared object
    WSO_SHAREDDATA_THREADLOCK.Enter;
    try
      try
        If LocksSharedData then
          begin
            fSharedDataLock.ThreadSharedLock := TCriticalSection.Create;
            fSharedDataLock.ThreadSharedLock.FreeOnRelease := True;
            fSharedDataLock.ThreadSharedLock.Acquire;
            fSharedDataLock.LockType := sltSection;
          end
        else fSharedDataLock.LockType := sltNone;
        fSharedData := AllocMem(WSO_CPLX_SHARED_ITEMSIZE);
        PWSOCommonSharedData(fSharedData)^.RefCount := 1;
        InitSharedData;
        CreateLocks;
        fFullyInitialized := True;
      except
        If Assigned(fSharedData) then
          begin
            CloseLocks;
            FinalSharedData;
            PWSOCommonSharedData(fSharedData)^.RefCount := 0;
            FreeMem(fSharedData,WSO_CPLX_SHARED_ITEMSIZE);
            fSharedData := nil;
          end;
        If Assigned(fSharedDataLock.ThreadSharedLock) then
          FreeAndNil(fSharedDataLock.ThreadSharedLock);
        raise;
      end;
    finally
      WSO_SHAREDDATA_THREADLOCK.Leave;
    end;    
  end;
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.InternalOpen(const Name: String);
begin
fFullyInitialized := False;
If RectifyAndSetName(Name) then
  begin
    // allocate/open shared data
    fNamedSharedItem := TNamedSharedItem.CreateLocked(GetDecoratedName(WSO_CPLX_SUFFIX_SHAREDDATA),
      WSO_CPLX_SHARED_ITEMSIZE,WSO_CPLX_SHARED_NAMESPACE);
    try
      try
        // open shared data lock
        If LocksSharedData then
          begin
            CheckAndSetHandle(fSharedDataLock.ProcessSharedLock,
              OpenMutexW(SYNCHRONIZE or MUTEX_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_CPLX_SUFFIX_SHAREDDATALOCK)))));
            fSharedDataLock.LockType := sltMutex;
          end
        else fSharedDataLock.LockType := sltNone;
        // bind shared data
        fSharedData := fNamedSharedItem.Memory;
        Inc(PWSOCommonSharedData(fSharedData)^.RefCount);
        try
          If PWSOCommonSharedData(fSharedData)^.RefCount <= 1 then
            raise EWSOOpenError.Create('TComplexWinSyncObject.InternalOpen: Shared data not initialized.');
          BindSharedData;
          // open main locks
          OpenLocks;
          fFullyInitialized := True;
        except
          // inner rollback
          If Assigned(fSharedData) then
            begin
              CloseLocks;
              Dec(PWSOCommonSharedData(fSharedData)^.RefCount);
              If PWSOCommonSharedData(fSharedData)^.RefCount <= 0 then
                PWSOCommonSharedData(fSharedData)^.RefCount := 0;
              // do not finalize the shared data, we have no business of doing so here
            end;
          raise;
        end;
      except
        // outer rollback
        fSharedData := nil;
        If fSharedDataLock.LockType = sltMutex then
          CloseHandle(fSharedDataLock.ProcessSharedLock);
        FreeAndNil(fNamedSharedItem);
        raise;
      end;
    finally
      fNamedSharedItem.GlobalUnlock
    end;
  end
else raise EWSOOpenError.Create('TComplexWinSyncObject.InternalOpen: Cannot open unnamed object.');
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.InternalClose;

  procedure InternalCloseCommon;
  begin
    CloseLocks;
    Dec(PWSOCommonSharedData(fSharedData)^.RefCount);
    If PWSOCommonSharedData(fSharedData)^.RefCount <= 0 then
      begin
        PWSOCommonSharedData(fSharedData)^.RefCount := 0;
        FinalSharedData;
        If not fProcessShared then
          FreeMem(fSharedData,WSO_CPLX_SHARED_ITEMSIZE);
      end;
    fSharedData := nil;
  end;

begin
If fFullyInitialized then
  begin
    If fProcessShared then
      begin
        fNamedSharedItem.GlobalLock;
        try
          InternalCloseCommon;
          If fSharedDataLock.LockType = sltMutex then
            CloseHandle(fSharedDataLock.ProcessSharedLock);
        finally
          fNamedSharedItem.GlobalUnlock;
        end;
        FreeAndNil(fNamedSharedItem);
      end
    else
      begin
        WSO_SHAREDDATA_THREADLOCK.Enter;
        try
          InternalCloseCommon;
          If fSharedDataLock.LockType = sltSection then
            fSharedDataLock.ThreadSharedLock.Release; // auto-free is on
        finally
          WSO_SHAREDDATA_THREADLOCK.Leave;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TComplexWinSyncObject.GetDecoratedName(const Suffix: String): String;
begin
If Length(Suffix) = 3 then
  Result := fName + GetNameSuffix + Suffix
else
  raise EWSOInvalidValue.CreateFmt('TComplexWinSyncObject.GetDecoratedName: Invalid suffix "%s".',[Suffix]);
end;

{-------------------------------------------------------------------------------
    TComplexWinSyncObject - public methods
-------------------------------------------------------------------------------}

constructor TComplexWinSyncObject.Create(const Name: String);
begin
inherited Create;
InternalCreate(Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TComplexWinSyncObject.Create;
begin
Create('');
end;

//------------------------------------------------------------------------------

constructor TComplexWinSyncObject.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
InternalOpen(Name);
end;

//------------------------------------------------------------------------------

constructor TComplexWinSyncObject.DuplicateFrom(SourceObject: TComplexWinSyncObject);
begin
inherited Create;
If SourceObject.GetLockType = Self.GetLockType then
  begin
    If not SourceObject.ProcessShared then
      begin
        fProcessShared := False;
        fFullyInitialized := False;
        WSO_SHAREDDATA_THREADLOCK.Enter;
        try
          // source object exists, so its field fSharedData must be assigned
          try
            If LocksSharedData then
              begin
                fSharedDataLock.ThreadSharedLock := SourceObject.fSharedDataLock.ThreadSharedLock;
                try
                  fSharedDataLock.ThreadSharedLock.Acquire;
                except
                  fSharedDataLock.ThreadSharedLock := nil;
                  raise;
                end;
                fSharedDataLock.LockType := sltSection;
              end
            else fSharedDataLock.LockType := sltNone;
            fSharedData := SourceObject.fSharedData;
          {
            Increase reference count. If it is above 1, all is good and we can
            continue.
            But if it is below or equal to 1, it means something is wrong and
            the source is in an inconsistent state - raise exception.
          }
            Inc(PWSOCommonSharedData(fSharedData)^.RefCount);
            try
              If PWSOCommonSharedData(SourceObject.fSharedData)^.RefCount <= 1 then
                raise EWSOOpenError.Create('TComplexWinSyncObject.DuplicateFrom: Shared data not initialized.');
              BindSharedData;
              DuplicateLocks(SourceObject);
              fFullyInitialized := True;
            except
              // inner rollback
              If Assigned(fSharedData) then
                begin
                  CloseLocks;
                  Dec(PWSOCommonSharedData(fSharedData)^.RefCount);
                  If PWSOCommonSharedData(fSharedData)^.RefCount <= 0 then
                    PWSOCommonSharedData(fSharedData)^.RefCount := 0;
                end;
              raise;
            end;
          except
            // outer rollback
            fSharedData := nil;
            If Assigned(fSharedDataLock.ThreadSharedLock) then
              fSharedDataLock.ThreadSharedLock.Release;
            raise;
          end;
        finally
          WSO_SHAREDDATA_THREADLOCK.Leave;
        end;
      end
    else InternalOpen(SourceObject.Name);
  end
else raise EWSOInvalidObject.CreateFmt('TComplexWinSyncObject.DuplicateFrom: Incompatible source object (%s).',[SourceObject.ClassName]);
end;

//------------------------------------------------------------------------------

destructor TComplexWinSyncObject.Destroy;
begin
InternalClose;
inherited;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TSimpleBarrier
--------------------------------------------------------------------------------
===============================================================================}
const
  WSO_SBARR_SUFFIX  = '@sbr_';

  WSO_SBARR_SUFFIX_ENTRYLOCK   = 'elk';
  WSO_SBARR_SUFFIX_RELEASELOCK = 'rlk';

{===============================================================================
    TSimpleBarrier - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleBarrier - protected methods
-------------------------------------------------------------------------------}

class Function TSimpleBarrier.LocksSharedData: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

procedure TSimpleBarrier.InitSharedData;
begin
inherited;
fBarrierSharedData := PWSOSimpleBarrierSharedData(fSharedData);
fBarrierSharedData^.MaxWaitCount := fCount;
InterlockedStore(fBarrierSharedData^.WaitCount,0);
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure TSimpleBarrier.BindSharedData;
begin
inherited;
fBarrierSharedData := PWSOSimpleBarrierSharedData(fSharedData);
fCount := fBarrierSharedData^.MaxWaitCount;
end;

//------------------------------------------------------------------------------

procedure TSimpleBarrier.DuplicateLocks(SourceObject: TComplexWinSyncObject);
begin
DuplicateAndSetHandle(fEntryLock,TSimpleBarrier(SourceObject).fEntryLock);
DuplicateAndSetHandle(fReleaseLock,TSimpleBarrier(SourceObject).fReleaseLock);
end;

//------------------------------------------------------------------------------

procedure TSimpleBarrier.CreateLocks;
begin
If fProcessShared then
  begin
    CheckAndSetHandle(fEntryLock,CreateSemaphoreW(nil,fBarrierSharedData^.MaxWaitCount,
      fBarrierSharedData^.MaxWaitCount,PWideChar(StrToWide(GetDecoratedName(WSO_SBARR_SUFFIX_ENTRYLOCK)))));
    CheckAndSetHandle(fReleaseLock,CreateEventW(nil,True,False,PWideChar(StrToWide(GetDecoratedName(WSO_SBARR_SUFFIX_RELEASELOCK)))));
  end
else
  begin
    CheckAndSetHandle(fEntryLock,CreateSemaphoreW(nil,fCount,fCount,nil));
    CheckAndSetHandle(fReleaseLock,CreateEventW(nil,True,False,nil));
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleBarrier.OpenLocks;
begin
CheckAndSetHandle(fEntryLock,
  OpenSemaphoreW(SYNCHRONIZE or SEMAPHORE_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_SBARR_SUFFIX_ENTRYLOCK)))));
CheckAndSetHandle(fReleaseLock,
  OpenEventW(SYNCHRONIZE or Event_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_SBARR_SUFFIX_RELEASELOCK)))));
end;

//------------------------------------------------------------------------------

procedure TSimpleBarrier.CloseLocks;
begin
CloseHandle(fReleaseLock);
CloseHandle(fEntryLock);
end;

//------------------------------------------------------------------------------

class Function TSimpleBarrier.GetLockType: TWSOLockType;
begin
Result := ltSmplBarrier;
end;

//------------------------------------------------------------------------------

class Function TSimpleBarrier.GetNameSuffix: String;
begin
Result := WSO_SBARR_SUFFIX;
end;

{-------------------------------------------------------------------------------
    TSimpleBarrier - public methods
-------------------------------------------------------------------------------}

constructor TSimpleBarrier.Create(const Name: String);
begin
{
  Barrier with count of 1 is seriously pointless, but if you call a constructor
  without specifying the count, what do you expect to happen?!
}
Create(1,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSimpleBarrier.Create;
begin
Create(1,'');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSimpleBarrier.Create(Count: Integer; const Name: String);
begin
If Count > 0 then
  begin
    fCount := Count;
    inherited Create(Name);
  end
else raise EWSOInvalidValue.CreateFmt('TSimpleBarrier.Create: Invalid count (%d).',[Count]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSimpleBarrier.Create(Count: Integer);
begin
Create(Count,'');
end;

//------------------------------------------------------------------------------

Function TSimpleBarrier.Wait: Boolean;
begin
{
  If MaxWaitCount is 1 or less, then just ignore everything and immediately
  return (with result being true).
}
If fBarrierSharedData^.MaxWaitCount > 1 then
  begin
    Result := False;
    {
      First do entry waiting on the barrier. If blocked, it means the entry
      semaphore is zero and a release is in progress.
    }
    case WaitForSingleObject(fEntryLock,INFINITE) of
      WAIT_OBJECT_0:;
      WAIT_FAILED:
        raise EWSOWaitError.CreateFmt('TSimpleBarrier.Wait: Failed to enter barrier (%d).',[GetLastError]);
    else
      raise EWSOWaitError.Create('TSimpleBarrier.Wait: Failed to enter barrier.');
    end;
    // increment wait counter...
    If InterlockedIncrement(fBarrierSharedData^.WaitCount) >= fBarrierSharedData^.MaxWaitCount then
      begin
        // barrier is full (fEntryLock semaphore should be locked by now), start releasing
        If not SetEvent(fReleaseLock) then
          raise EWSOEventError.CreateFmt('TSimpleBarrier.Wait: Failed to start release (%d).',[GetLastError]);
        Result := True;
      end
    else
      begin
        // barrier not full, enter waiting for a release
        case WaitForSingleObject(fReleaseLock,INFINITE) of
          WAIT_OBJECT_0:;
          WAIT_FAILED:
            raise EWSOWaitError.CreateFmt('TSimpleBarrier.Wait: Failed release wait (%d).',[GetLastError]);
        else
          raise EWSOWaitError.Create('TSimpleBarrier.Wait: Failed release wait.');
        end;
      end;
    {
      Now we are released, decrement wait counter and if it reaches zero, stop
      release and unlock the entry semaphore to allow more waiters to enter the
      barrier.
    }
    If InterlockedDecrement(fBarrierSharedData^.WaitCount) <= 0 then
      begin
        If not ResetEvent(fReleaseLock) then
          raise EWSOEventError.CreateFmt('TSimpleBarrier.Wait: Failed to stop release (%d).',[GetLastError]);
        If not ReleaseSemaphore(fEntryLock,fBarrierSharedData^.MaxWaitCount,nil) then
          raise EWSOSemaphoreError.CreateFmt('TSimpleBarrier.Wait: Failed to unlock entry (%d).',[GetLastError]);
      end;
  end
else Result := True;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TBarrier
--------------------------------------------------------------------------------
===============================================================================}
const
  WSO_BARR_SUFFIX = '@brr_';

  WSO_BARR_SUFFIX_ENTRYLOCK   = 'elk';
  WSO_BARR_SUFFIX_RELEASELOCK = 'rlk';

{===============================================================================
    TBarrier - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBarrier - protected methods
-------------------------------------------------------------------------------}

procedure TBarrier.InitSharedData;
begin
inherited;
fBarrierSharedData := PWSOBarrierSharedData(fSharedData);
fBarrierSharedData^.MaxWaitCount := fCount;
fBarrierSharedData^.WaitCount := 0;
fBarrierSharedData^.Releasing := False;
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure TBarrier.BindSharedData;
begin
inherited;
fBarrierSharedData := PWSOBarrierSharedData(fSharedData);
fCount := fBarrierSharedData^.MaxWaitCount;
end;

//------------------------------------------------------------------------------

procedure TBarrier.DuplicateLocks(SourceObject: TComplexWinSyncObject);
begin
DuplicateAndSetHandle(fEntryLock,TBarrier(SourceObject).fEntryLock);
DuplicateAndSetHandle(fReleaseLock,TBarrier(SourceObject).fReleaseLock);
end;

//------------------------------------------------------------------------------

procedure TBarrier.CreateLocks;
begin
If fProcessShared then
  begin
    CheckAndSetHandle(fEntryLock,
      CreateEventW(nil,True,True,PWideChar(StrToWide(GetDecoratedName(WSO_BARR_SUFFIX_ENTRYLOCK)))));
    CheckAndSetHandle(fReleaseLock,
      CreateEventW(nil,True,False,PWideChar(StrToWide(GetDecoratedName(WSO_BARR_SUFFIX_RELEASELOCK)))));
  end
else
  begin
    CheckAndSetHandle(fEntryLock,CreateEventW(nil,True,True,nil));
    CheckAndSetHandle(fReleaseLock,CreateEventW(nil,True,False,nil));
  end;
end;

//------------------------------------------------------------------------------

procedure TBarrier.OpenLocks;
begin
CheckAndSetHandle(fEntryLock,
  OpenEventW(SYNCHRONIZE or EVENT_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_BARR_SUFFIX_ENTRYLOCK)))));
CheckAndSetHandle(fReleaseLock,
  OpenEventW(SYNCHRONIZE or EVENT_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_BARR_SUFFIX_RELEASELOCK)))));
end;

//------------------------------------------------------------------------------

procedure TBarrier.CloseLocks;
begin
CloseHandle(fReleaseLock);
CloseHandle(fEntryLock);
end;

//------------------------------------------------------------------------------

class Function TBarrier.GetLockType: TWSOLockType;
begin
Result := ltBarrier;
end;

//------------------------------------------------------------------------------

class Function TBarrier.GetNameSuffix: String;
begin
Result := WSO_BARR_SUFFIX;
end;

{-------------------------------------------------------------------------------
    TBarrier - public methods
-------------------------------------------------------------------------------}

constructor TBarrier.Create(const Name: String);
begin
Create(1,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create;
begin
Create(1,'');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create(Count: Integer; const Name: String);
begin
If Count > 0 then
  begin
    fCount := Count;
    inherited Create(Name);
  end
else raise EWSOInvalidValue.CreateFmt('TBarrier.Create: Invalid count (%d).',[Count]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create(Count: Integer);
begin
Create(Count,'');
end;

//------------------------------------------------------------------------------

Function TBarrier.Wait: Boolean;
var
  ExitWait: Boolean;
begin
If fBarrierSharedData^.MaxWaitCount > 1 then
  begin
    Result := False;
    repeat
      LockSharedData;
      If fBarrierSharedData^.Releasing then
        begin
        {
          Releasing is in progress, so this thread cannot queue on the barrier.
          Unlock shared data and wait for fEntryLock to become signaled, which
          happens at the end of releasing.
        }
          UnlockSharedData;
          case WaitForSingleObject(fEntryLock,INFINITE) of
            WAIT_OBJECT_0:;
            WAIT_FAILED:
              raise EWSOWaitError.CreateFmt('TBarrier.Wait: Failed to enter barrier (%d).',[GetLastError]);
          else
            raise EWSOWaitError.Create('TBarrier.Wait: Failed to enter barrier.');
          end;
          ExitWait := False;
          // Releasing should be done by this point. Re-enter waiting.
        end
      else
        begin
          // Releasing is currently not running.
          Inc(fBarrierSharedData^.WaitCount);
          If fBarrierSharedData^.WaitCount >= fBarrierSharedData^.MaxWaitCount then
            begin
            {
              Maximum number of waiting threads for this barrier has been
              reached.

              First prevent other threads from queueing on this barrier by
              resetting fEntryLock and indicating the fact in shared data.
            }
              If not ResetEvent(fEntryLock) then
                raise EWSOEventError.CreateFmt('TBarrier.Wait: Failed to lock entry (%d).',[GetLastError]);
              fBarrierSharedData^.Releasing := True;
              Dec(fBarrierSharedData^.WaitCount); // remove self from waiting count
            {
              Now unlock shared data and release all waiting threads from
              fReleaseLock.

              Unlocking shared data at this point is secure because any thread
              that will acquire them will encounter Releasing field to be true
              and will therefore enter waiting on fEntryLock, which is now
              non-signaled.
            }
              UnlockSharedData;
              If not SetEvent(fReleaseLock) then
                raise EWSOEventError.CreateFmt('TBarrier.Wait: Failed to start release (%d).',[GetLastError]);
              Result := True; // indicate we have released the barrier
            end
          else
            begin
            {
              Maximum number of waiters not reached.

              Just unlock the shared data and enter waiting on fReleaseLock.
            }
              UnlockSharedData;
              case WaitForSingleObject(fReleaseLock,INFINITE) of
                WAIT_OBJECT_0:;
                WAIT_FAILED:
                  raise EWSOWaitError.CreateFmt('TBarrier.Wait: Failed release wait (%d).',[GetLastError]);
              else
                raise EWSOWaitError.Create('TBarrier.Wait: Failed release wait.');
              end;
            {
              The release lock has been set to signaled, so the barrier is
              releasing.

              Remove self from waiting threads count and, if we are last to be
              released, stop releasing and signal end of releasing to threads
              waiting on fEntryLock and also mark it in shared data.
            }
              LockSharedData;
              try
                Dec(fBarrierSharedData^.WaitCount);
                If fBarrierSharedData^.WaitCount <= 0 then
                  begin
                    fBarrierSharedData^.WaitCount := 0;
                    fBarrierSharedData^.Releasing := False;
                    If not ResetEvent(fReleaseLock) then
                      raise EWSOEventError.CreateFmt('TBarrier.Wait: Failed to stop release (%d).',[GetLastError]);
                    If not SetEvent(fEntryLock) then
                      raise EWSOEventError.CreateFmt('TBarrier.Wait: Failed to unlock entry (%d).',[GetLastError]);
                  end;
              finally
                UnlockSharedData;
              end;
            end;
          ExitWait := True;
        end;
    until ExitWait;
  end
else Result := True;
end;

//------------------------------------------------------------------------------

Function TBarrier.Release: Integer;
var
  SetReleaseLock: Boolean;
begin
// no need to check for max wait count
SetReleaseLock := False;
LockSharedData;
try
  If not fBarrierSharedData^.Releasing then
    begin
      If fBarrierSharedData^.WaitCount > 0 then
        begin
          SetReleaseLock := True;
          Result := fBarrierSharedData^.WaitCount;
          If not ResetEvent(fEntryLock) then
            raise EWSOEventError.CreateFmt('TBarrier.Release: Failed to lock entry (%d).',[GetLastError]);
          fBarrierSharedData^.Releasing := True;
        end
      else Result := 0;
    end
  else Result := -1;
finally
  UnlockSharedData;
end;
{
  At this point (if SetReleaseLock is true), releasing is active and no new
  thread can queue on the barrier - so it is safe to unlock shared data before
  setting the event.
}
If SetReleaseLock then
  If not SetEvent(fReleaseLock) then
    raise EWSOEventError.CreateFmt('TBarrier.Release: Failed to start release (%d).',[GetLastError]);
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TConditionVariable
--------------------------------------------------------------------------------
===============================================================================}
const
  WSO_COND_SUFFIX = '@cnd_';

  WSO_COND_SUFFIX_WAITLOCK   = 'wlk';
  WSO_COND_SUFFIX_BDONELOCK  = 'blk'; // broadcast done

{===============================================================================
    TConditionVariable - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TConditionVariable - protected methods
-------------------------------------------------------------------------------}

procedure TConditionVariable.InitSharedData;
begin
inherited;
fCondSharedData := PWSOCondSharedData(fSharedData);
fCondSharedData^.WaitCount := 0;
fCondSharedData^.WakeCount := 0;
fCondSharedData^.Broadcasting := False;
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.BindSharedData;
begin
inherited;
fCondSharedData := PWSOCondSharedData(fSharedData);
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.DuplicateLocks(SourceObject: TComplexWinSyncObject);
begin
DuplicateAndSetHandle(fWaitLock,TConditionVariable(SourceObject).fWaitLock);
DuplicateAndSetHandle(fBroadcastDoneLock,TConditionVariable(SourceObject).fBroadcastDoneLock);
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.CreateLocks;
begin
If fProcessShared then
  begin
  {
    $7FFFFFFF is maximum for semaphores in Windows, anything higher will cause
    invalid parameter error.

    But seriously, if you manage to enter waiting in more than two billion
    threads, you are doing something wrong... or stop using this ancient
    heretical code on Windows 40K, God Emperor of Mankind does not approve!
  }
    CheckAndSetHandle(fWaitLock,
      CreateSemaphoreW(nil,0,DWORD($7FFFFFFF),PWideChar(StrToWide(GetDecoratedName(WSO_COND_SUFFIX_WAITLOCK)))));
    CheckAndSetHandle(fBroadcastDoneLock,
      CreateEventW(nil,True,False,PWideChar(StrToWide(GetDecoratedName(WSO_COND_SUFFIX_BDONELOCK)))));
  end
else
  begin
    CheckAndSetHandle(fWaitLock,CreateSemaphoreW(nil,0,DWORD($7FFFFFFF),nil));
    CheckAndSetHandle(fBroadcastDoneLock,CreateEventW(nil,True,False,nil));
  end;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.OpenLocks;
begin
CheckAndSetHandle(fWaitLock,
  OpenSemaphoreW(SYNCHRONIZE or SEMAPHORE_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_COND_SUFFIX_WAITLOCK)))));
CheckAndSetHandle(fBroadcastDoneLock,
  OpenEventW(SYNCHRONIZE or EVENT_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_COND_SUFFIX_BDONELOCK)))));
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.CloseLocks;
begin
CloseHandle(fBroadcastDoneLock);
CloseHandle(fWaitLock);
end;

//------------------------------------------------------------------------------

class Function TConditionVariable.GetLockType: TWSOLockType;
begin
Result := ltCondVar;
end;

//------------------------------------------------------------------------------

class Function TConditionVariable.GetNameSuffix: String;
begin
Result := WSO_COND_SUFFIX;
end;

//------------------------------------------------------------------------------

Function TConditionVariable.DoOnPredicateCheck: Boolean;
begin
Result := False;
If Assigned(fOnPredicateCheckEvent) then
  fOnPredicateCheckEvent(Self,Result)
else If Assigned(fOnPredicateCheckCallback) then
  fOnPredicateCheckCallback(Self,Result);
end;

//------------------------------------------------------------------------------

Function TConditionVariable.DoOnDataAccess: TWSOWakeOptions;
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

procedure TConditionVariable.SelectWake(WakeOptions: TWSOWakeOptions);
begin
If ([woWakeOne,woWakeAll] *{intersection} WakeOptions) <> [] then
  begin
    If woWakeAll in WakeOptions then
      WakeAll
    else
      Wake;
  end;
end;

{-------------------------------------------------------------------------------
    TConditionVariable - public methods
-------------------------------------------------------------------------------}

procedure TConditionVariable.Sleep(DataLock: THandle; Timeout: DWORD = INFINITE);
var
  StartTime:        TWSOTimestamp;
  TimeoutRemaining: DWORD;
  FirstWait:        Boolean;
  ExitWait:         Boolean;

  Function InternalWait: DWORD;
  begin
    If FirstWait then
      Result := SignalObjectAndWait(DataLock,fWaitLock,TimeoutRemaining,False)
    else
      Result := WaitForSingleObject(fWaitLock,TimeoutRemaining);
  end;

begin
StartTime := GetTimestamp;
TimeoutRemaining := Timeout;
LockSharedData;
try
  Inc(fCondSharedData^.WaitCount);
finally
  UnlockSharedData;
end;
FirstWait := True;
ExitWait := False;
repeat
  case InternalWait of
    WAIT_OBJECT_0:{signaled}
      begin
        LockSharedData;
        try
          If fCondSharedData^.WakeCount > 0 then
            begin
              // waking
              Dec(fCondSharedData^.WakeCount);
              If fCondSharedData^.Broadcasting and (fCondSharedData^.WakeCount <= 0) then
                begin
                  // broadcasting and this waiter is last, end broadcast
                  fCondSharedData^.WakeCount := 0;
                  fCondSharedData^.Broadcasting := False;
                  If not SetEvent(fBroadcastDoneLock) then
                    raise EWSOEventError.CreateFmt('TConditionVariable.Sleep: Failed to end broadcast (%d).',[GetLastError]);
                end;
              ExitWait := True; // normal wakeup
            end
          // not waking, re-enter waiting (recalcualte timeout)
          else If not RecalculateTimeout(Timeout,StartTime,TimeoutRemaining) then
            ExitWait := True;
        finally
          UnlockSharedData;
        end;
      end;
    WAIT_FAILED:
      raise EWSOWaitError.CreateFmt('TConditionVariable.Sleep: Wait failed (%d)',[GetLastError]);
  else
    // timeout or spurious wakeup (eg. APC)
    ExitWait := True;
  end;
  FirstWait := False; // in case the cycle repeats and re-enters waiting (so the DataLock is not signaled again)
until ExitWait;
// lock the DataLock synchronizer
case WaitForSingleObject(DataLock,INFINITE) of
  WAIT_OBJECT_0,
  WAIT_ABANDONED:;
  WAIT_FAILED:
    raise EWSOWaitError.CreateFmt('TConditionVariable.Sleep: Failed to lock data synchronizer (%d).',[GetLastError]);
else
  raise EWSOWaitError.Create('TConditionVariable.Sleep: Failed to lock data synchronizer.');
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TConditionVariable.Sleep(DataLock: TSimpleWinSyncObject; Timeout: DWORD = INFINITE);
begin
If DataLock.GetLockType in [ltEvent,ltMutex,ltSemaphore] then
  Sleep(DataLock.Handle,Timeout)
else
  raise EWSOInvalidObject.CreateFmt('TConditionVariable.Sleep: Unsupported data synchronizer type (%s),',[DataLock.ClassName]);
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.Wake;
begin
LockSharedData;
try
  If fCondSharedData^.WaitCount > 0 then
    begin
    {
      Wait count must be decremented here, because if it would be done by the
      waiter, there is a chance of another wake before the waiter get a chance
      to do the decrement. This would create a discrepancy in counters.
    }
      Dec(fCondSharedData^.WaitCount);
      Inc(fCondSharedData^.WakeCount);
      If not ReleaseSemaphore(fWaitLock,1,nil) then
        raise EWSOSemaphoreError.CreateFmt('TConditionVariable.Wake: Release failed (%d).',[GetLastError]);
    end;
finally
  UnlockSharedData;
end;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.WakeAll;
var
  Waiters:  Int32;
begin
LockSharedData;
try
  Waiters := fCondSharedData^.WaitCount;
  If Waiters > 0 then
    begin
      fCondSharedData^.WaitCount := 0;
      Inc(fCondSharedData^.WakeCount,Waiters);
      If not fCondSharedData^.Broadcasting then
        If not ResetEvent(fBroadcastDoneLock) then
          raise EWSOEventError.CreateFmt('TConditionVariable.WakeAll: Failed to start broadcast (%d).',[GetLastError]);
      fCondSharedData^.Broadcasting := True;
      If not ReleaseSemaphore(fWaitLock,Waiters,nil) then
        raise EWSOSemaphoreError.CreateFmt('TConditionVariable.WakeAll: Release failed (%d).',[GetLastError]);
    end;
finally
  UnlockSharedData;
end;  
If Waiters > 0 then
  case WaitForSingleObject(fBroadcastDoneLock,INFINITE) of
    WAIT_OBJECT_0:;
    WAIT_FAILED:
      raise EWSOWaitError.CreateFmt('TConditionVariable.WakeAll: Broadcast wait failed (%d).',[GetLastError]);
  else
    raise EWSOWaitError.Create('TConditionVariable.WakeAll: Broadcast wait failed.');
  end;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.AutoCycle(DataLock: THandle; Timeout: DWORD = INFINITE);
var
  WakeOptions:  TWSOWakeOptions;
begin
If Assigned(fOnPredicateCheckEvent) or Assigned(fOnPredicateCheckCallback) then
  begin
    // lock synchronizer
    case WaitForSingleObject(DataLock,INFINITE) of
      WAIT_OBJECT_0,
      WAIT_ABANDONED:
        begin
          // test predicate and wait condition
          while not DoOnPredicateCheck do
            Sleep(DataLock,Timeout);
          // access protected data
          WakeOptions := DoOnDataAccess;
          // wake waiters before unlock
          If (woWakeBeforeUnlock in WakeOptions) then
            SelectWake(WakeOptions);
          // unlock synchronizer
          If not ReleaseMutex(DataLock) then
            raise EWSOMutexError.CreateFmt('TConditionVariable.AutoCycle: Failed to unlock data synchronizer (%d).',[GetLastError]);
          // wake waiters after unlock
          If not(woWakeBeforeUnlock in WakeOptions) then
            SelectWake(WakeOptions);
        end;
      WAIT_FAILED:
        raise EWSOWaitError.CreateFmt('TConditionVariable.AutoCycle: Failed to lock data synchronizer (%d).',[GetLastError]);
    else
      raise EWSOWaitError.Create('TConditionVariable.AutoCycle: Failed to lock data synchronizer.');
    end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TConditionVariable.AutoCycle(DataLock: TSimpleWinSyncObject; Timeout: DWORD = INFINITE);
var
  WaitResult:   TWSOWaitResult;
  WakeOptions:  TWSOWakeOptions;
  ErrorCode:    DWORD;
begin
If Assigned(fOnPredicateCheckEvent) or Assigned(fOnPredicateCheckCallback) then
  begin
    If DataLock.GetLockType in [ltEvent,ltMutex,ltSemaphore]  then
      begin
        // lock synchronizer
        WaitResult := DataLock.WaitFor(INFINITE,ErrorCode,False);
        case WaitResult of
          wrSignaled,
          wrAbandoned:
            // abandoned is allowed only for mutexes
            If (WaitResult <> wrAbandoned) or (DataLock is TMutex) then
              begin
                // test predicate and wait condition
                while not DoOnPredicateCheck do
                  Sleep(DataLock,Timeout);
                // access protected data
                WakeOptions := DoOnDataAccess;
                // wake waiters before unlock
                If (woWakeBeforeUnlock in WakeOptions) then
                  SelectWake(WakeOptions);
                // unlock synchronizer
                case DataLock.GetLockType of
                  ltEvent:      TEvent(DataLock).SetEventStrict;
                  ltMutex:      TMutex(DataLock).ReleaseMutexStrict;
                  ltSemaphore:  TSemaphore(DataLock).ReleaseSemaphoreStrict;
                end;
                // wake waiters after unlock
                If not(woWakeBeforeUnlock in WakeOptions) then
                  SelectWake(WakeOptions);
              end
            else raise EWSOWaitError.Create('TConditionVariable.AutoCycle: Failed to lock data synchronizer.');
          wrError:
            raise EWSOWaitError.CreateFmt('TConditionVariable.AutoCycle: Failed to lock data synchronizer (%d).',[ErrorCode]);
        else
          raise EWSOWaitError.Create('TConditionVariable.AutoCycle: Failed to lock data synchronizer.');
        end;
      end
    else raise EWSOInvalidObject.CreateFmt('TConditionVariable.AutoCycle: Unsupported data synchronizer type (%s),',[DataLock.ClassName]);
  end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              TConditionVariableEx
--------------------------------------------------------------------------------
===============================================================================}
const
  WSO_CONDEX_SUFFIX = '@cde_';

  WSO_CONDEX_SUFFIX_DATALOCK = 'dlk';
  
{===============================================================================
    TConditionVariableEx - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TConditionVariableEx - public methods
-------------------------------------------------------------------------------}

procedure TConditionVariableEx.DuplicateLocks(SourceObject: TComplexWinSyncObject);
begin
inherited DuplicateLocks(SourceObject);
DuplicateAndSetHandle(fDataLock,TConditionVariableEx(SourceObject).fDataLock);
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.CreateLocks;
begin
inherited CreateLocks;
If fProcessShared then
  CheckAndSetHandle(fDataLock,CreateMutexW(nil,RectBool(False),PWideChar(StrToWide(GetDecoratedName(WSO_CONDEX_SUFFIX_DATALOCK)))))
else
  CheckAndSetHandle(fDataLock,CreateMutexW(nil,RectBool(False),nil));
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.OpenLocks;
begin
inherited OpenLocks;
CheckAndSetHandle(fDataLock,
  OpenMutexW(SYNCHRONIZE or MUTEX_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_CONDEX_SUFFIX_DATALOCK)))));
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.CloseLocks;
begin
CloseHandle(fDataLock);
inherited;
end;

//------------------------------------------------------------------------------

class Function TConditionVariableEx.GetLockType: TWSOLockType;
begin
Result := ltCondVarEx;
end;

//------------------------------------------------------------------------------

class Function TConditionVariableEx.GetNameSuffix: String;
begin
Result := WSO_CONDEX_SUFFIX;
end;

{-------------------------------------------------------------------------------
    TConditionVariableEx - public methods
-------------------------------------------------------------------------------}

procedure TConditionVariableEx.Lock;
begin
case WaitForSingleObject(fDataLock,INFINITE) of
  WAIT_OBJECT_0,
  WAIT_ABANDONED:;
  WAIT_FAILED:
    raise EWSOWaitError.CreateFmt('TConditionVariableEx.Lock: Failed to lock data synchronizer (%d).',[GetLastError]);
else
  raise EWSOWaitError.Create('TConditionVariableEx.Lock: Failed to lock data synchronizer.');
end;
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.Unlock;
begin
If not ReleaseMutex(fDataLock) then
  raise EWSOMutexError.CreateFmt('TConditionVariableEx.Lock: Failed to unlock data synchronizer (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.Sleep(Timeout: DWORD = INFINITE);
begin
Sleep(fDataLock,Timeout);
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.AutoCycle(Timeout: DWORD = INFINITE);
begin
AutoCycle(fDataLock,Timeout);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TReadWriteLock
--------------------------------------------------------------------------------
===============================================================================}
const
  WSO_RWLOCK_SUFFIX = '@rwl_';

  WSO_RWLOCK_SUFFIX_READLOCK       = 'rlk';
  WSO_RWLOCK_SUFFIX_WRITEQUEUELOCK = 'qlk';
  WSO_RWLOCK_SUFFIX_WRITELOCK      = 'wlk';

{===============================================================================
    TReadWriteLock - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TReadWriteLock - protected methods
-------------------------------------------------------------------------------}

procedure TReadWriteLock.InitSharedData;
begin
inherited;
fRWLockSharedData := PWSORWLockSharedData(fSharedData);
fRWLockSharedData^.ReadCount := 0;
fRWLockSharedData^.WriteWaitCount := 0;
fRWLockSharedData^.Writing := False;
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.BindSharedData;
begin
inherited;
fRWLockSharedData := PWSORWLockSharedData(fSharedData);
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.DuplicateLocks(SourceObject: TComplexWinSyncObject);
begin
DuplicateAndSetHandle(fReadLock,TReadWriteLock(SourceObject).fReadLock);
DuplicateAndSetHandle(fWriteQueueLock,TReadWriteLock(SourceObject).fWriteQueueLock);
DuplicateAndSetHandle(fWriteLock,TReadWriteLock(SourceObject).fWriteLock);
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.CreateLocks;
begin
If fProcessShared then
  begin
    CheckAndSetHandle(fReadLock,
      CreateEventW(nil,True,True,PWideChar(StrToWide(GetDecoratedName(WSO_RWLOCK_SUFFIX_READLOCK)))));
    CheckAndSetHandle(fWriteQueueLock,
      CreateEventW(nil,True,True,PWideChar(StrToWide(GetDecoratedName(WSO_RWLOCK_SUFFIX_WRITEQUEUELOCK)))));
    CheckAndSetHandle(fWriteLock,
      CreateMutexW(nil,RectBool(False),PWideChar(StrToWide(GetDecoratedName(WSO_RWLOCK_SUFFIX_WRITELOCK)))));
  end
else
  begin
    CheckAndSetHandle(fReadLock,CreateEventW(nil,True,True,nil));
    CheckAndSetHandle(fWriteQueueLock,CreateEventW(nil,True,True,nil));
    CheckAndSetHandle(fWriteLock,CreateMutexW(nil,RectBool(False),nil));
  end;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.OpenLocks;
begin
CheckAndSetHandle(fReadLock,
  OpenEventW(SYNCHRONIZE or EVENT_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_RWLOCK_SUFFIX_READLOCK)))));
CheckAndSetHandle(fWriteQueueLock,
  OpenEventW(SYNCHRONIZE or EVENT_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_RWLOCK_SUFFIX_WRITEQUEUELOCK)))));
CheckAndSetHandle(fWriteLock,
  OpenMutexW(SYNCHRONIZE or MUTEX_MODIFY_STATE,False,PWideChar(StrToWide(GetDecoratedName(WSO_RWLOCK_SUFFIX_WRITELOCK)))));
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.CloseLocks;
begin
CloseHandle(fWriteLock);
CloseHandle(fWriteQueueLock);
CloseHandle(fReadLock);
end;

//------------------------------------------------------------------------------

class Function TReadWriteLock.GetLockType: TWSOLockType;
begin
Result := ltRWLock;
end;

//------------------------------------------------------------------------------

class Function TReadWriteLock.GetNameSuffix: String;
begin
Result := WSO_RWLOCK_SUFFIX;
end;

{-------------------------------------------------------------------------------
    TReadWriteLock - public methods
-------------------------------------------------------------------------------}

Function TReadWriteLock.ReadLock(Timeout: DWORD = INFINITE): TWSOWaitResult;
var
  StartTime:        TWSOTimestamp;
  TimeoutRemaining: DWORD;
  ExitWait:         Boolean;
begin
StartTime := GetTimestamp;
TimeoutRemaining := Timeout;
repeat
  ExitWait := True;
  Result := wrError;
  case WaitForSingleObject(fReadLock,TimeoutRemaining) of
    WAIT_OBJECT_0:
      begin
        LockSharedData;
        try
          If (fRWLockSharedData^.WriteWaitCount <= 0) and not fRWLockSharedData^.Writing then
            begin
              // nobody is writing or waiting for write, we can enter reading
              Inc(fRWLockSharedData^.ReadCount);
              If not ResetEvent(fWriteQueueLock) then
                raise EWSOEventError.CreateFmt('TReadWriteLock.ReadLock: Failed to lock write queue (%d).',[GetLastError]);
              Result := wrSignaled;  
            end
          else
            begin
              // someone is either writing or waiting for write, recalculate timeout and perhaps re-enter waiting
              If RecalculateTimeout(Timeout,StartTime,TimeoutRemaining) then
                ExitWait := False
              else
                Result := wrTimeout;
            end;
        finally
          UnlockSharedData;
        end;
      end;
    WAIT_TIMEOUT:
      Result := wrTimeout;
    WAIT_FAILED:
      fLastError := GetLastError;
  else
    raise EWSOWaitError.Create('TReadWriteLock.ReadLock: Read wait failed.');
  end;
until ExitWait;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.ReadUnlock;
begin
LockSharedData;
try
  Dec(fRWLockSharedData^.ReadCount);
  If fRWLockSharedData^.ReadCount <= 0 then
    If not SetEvent(fWriteQueueLock) then
      raise EWSOEventError.CreateFmt('TReadWriteLock.ReadUnlock: Failed to unlock write queue (%d).',[GetLastError]);
finally
  UnlockSharedData;
end;
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.WriteLock(Timeout: DWORD = INFINITE): TWSOWaitResult;
var
  StartTime:        TWSOTimestamp;
  TimeoutRemaining: DWORD;
begin
StartTime := GetTimestamp;
TimeoutRemaining := Timeout;
// enter write queue - this prevents any new reader to acquire read lock
LockSharedData;
try
  Inc(fRWLockSharedData^.WriteWaitCount);
  If not ResetEvent(fReadLock) then
    raise EWSOEventError.CreateFmt('TReadWriteLock.WriteLock: Failed to lock reading (%d).',[GetLastError]);
finally
  UnlockSharedData;
end;
try
  Result := wrError;
  // the following will block while there is any active reader
  case WaitForSingleObject(fWriteQueueLock,TimeoutRemaining) of
    WAIT_OBJECT_0:
      begin
        RecalculateTimeout(Timeout,StartTime,TimeoutRemaining);
        // there is no reader now, try enter writing (note that abandoned is not a good result)
        case WaitForSingleObject(fWriteLock,TimeoutRemaining) of
          WAIT_OBJECT_0:
            begin
              // we have the write lock now
              LockSharedData;
              try
                // locking the write wait will prevent recursive write locking
                If not ResetEvent(fWriteQueueLock) then
                  raise EWSOEventError.CreateFmt('TReadWriteLock.WriteLock: Failed to lock write queue (%d).',[GetLastError]);
                fRWLockSharedData^.Writing := True;
                Result := wrSignaled;
              finally
                UnlockSharedData;
              end;
            end;
          WAIT_TIMEOUT:
            Result := wrTimeout;
          WAIT_FAILED:
            fLastError := GetLastError;
        else
          raise EWSOWaitError.Create('TReadWriteLock.WriteLock: Write wait failed.');
        end;
      end;
    WAIT_TIMEOUT:
      Result := wrTimeout;
    WAIT_FAILED:
      fLastError := GetLastError;
  else
    raise EWSOWaitError.Create('TReadWriteLock.WriteLock: Write queue wait failed.');
  end;
finally
  LockSharedData;
  try
    Dec(fRWLockSharedData^.WriteWaitCount);
    If (fRWLockSharedData^.WriteWaitCount <= 0) and not fRWLockSharedData^.Writing then
      If not SetEvent(fReadLock) then
        raise EWSOEventError.CreateFmt('TReadWriteLock.WriteLock: Failed to unlock reading (%d).',[GetLastError]);
  finally
    UnlockSharedData;
  end;
end;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.WriteUnlock;
begin
LockSharedData;
try
  fRWLockSharedData^.Writing := False;
  If not ReleaseMutex(fWriteLock) then
    raise EWSOMutexError.CreateFmt('TReadWriteLock.WriteUnlock: Failed to unlock writing (%d).',[GetLastError]);
  // no need to check for readers count, if we had write lock there can be none
  If not SetEvent(fWriteQueueLock) then
    raise EWSOEventError.CreateFmt('TReadWriteLock.WriteUnlock: Failed to unlock write queue (%d).',[GetLastError]);
  // if there is no thread waiting for write, allow readers to acquire their read locks
  If fRWLockSharedData^.WriteWaitCount <= 0 then
    If not SetEvent(fReadLock) then
      raise EWSOEventError.CreateFmt('TReadWriteLock.WriteUnlock: Failed to unlock reading (%d).',[GetLastError]);
finally
  UnlockSharedData;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Wait functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Wait functions - implementation constants
===============================================================================}
const
  MWMO_WAITALL        = DWORD($00000001);
  MWMO_ALERTABLE      = DWORD($00000002);
  MWMO_INPUTAVAILABLE = DWORD($00000004);

{
  Following constant is here only for debuging of multi-wait.
  The entire line should normally be commented-out.
}
//  MAXIMUM_WAIT_OBJECTS = 3; {$MESSAGE WARN 'debug, do not set below 3'}

{===============================================================================
    Wait functions - internal functions
===============================================================================}

Function RectifiedMaxWaitCount(MsgWaitOptions: TMessageWaitOptions): Integer;
begin
If mwoEnable in MsgWaitOptions then
  Result := MAXIMUM_WAIT_OBJECTS - 1
else
  Result := MAXIMUM_WAIT_OBJECTS;
end;

{===============================================================================
--------------------------------------------------------------------------------
                            Wait functions (N <= MAX)
--------------------------------------------------------------------------------
===============================================================================} 
{
  There are some inconsistencies in Delphi, so to be sure I am redeclaring the
  external fuctions.
}
Function MsgWaitForMultipleObjectsEx(
  nCount:         DWORD;
  pHandles:       PHandle;
  dwMilliseconds: DWORD;
  dwWakeMask:     DWORD;
  dwFlags:        DWORD): DWORD; stdcall; external user32;

Function WaitForMultipleObjectsEx(
  nCount:         DWORD;
  lpHandles:      PHandle;
  bWaitAll:       BOOL;
  dwMilliseconds: DWORD;
  bAlertable:     BOOL): DWORD; stdcall; external kernel32;

{===============================================================================
    Wait functions (N <= MAX) - internal functions
===============================================================================}

Function WaitForMultipleHandles_Sys(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
var
  WaitResult: DWORD;
  MsgFlags:   DWORD;
begin
If (Count > 0) and (Count <= RectifiedMaxWaitCount(MsgWaitOptions)) then
  begin
    Index := -1;
    If mwoEnable in MsgWaitOptions then
      begin
        // waiting with messages, construct flags
        MsgFlags := 0;
        If WaitAll then
          MsgFlags := MsgFlags or MWMO_WAITALL;
        If Alertable then
          MsgFlags := MsgFlags or MWMO_ALERTABLE;
        If mwoInputAvailable in MsgWaitOptions then
          MsgFlags := MsgFlags or MWMO_INPUTAVAILABLE;
        WaitResult := MsgWaitForMultipleObjectsEx(DWORD(Count),Handles,Timeout,WakeMask,MsgFlags);
      end
    // "normal" waiting
    else WaitResult := WaitForMultipleObjectsEx(DWORD(Count),Handles,WaitAll,Timeout,Alertable);
    // process result
    case WaitResult of
      WAIT_OBJECT_0..
      Pred(WAIT_OBJECT_0 + MAXIMUM_WAIT_OBJECTS):
        If not(mwoEnable in MsgWaitOptions) or (Integer(WaitResult - WAIT_OBJECT_0) < Count) then
          begin
            Result := wrSignaled;
            If not WaitAll then
              Index := Integer(WaitResult - WAIT_OBJECT_0);
          end
        else Result := wrMessage;
      WAIT_ABANDONED_0..
      Pred(WAIT_ABANDONED_0 + MAXIMUM_WAIT_OBJECTS):
        If Integer(WaitResult - WAIT_ABANDONED_0) < Count then
          begin
            Result := wrAbandoned;
            If not WaitAll then
              Index := Integer(WaitResult - WAIT_ABANDONED_0);
          end
        else raise EWSOMultiWaitError.CreateFmt('WaitForMultipleHandles_Sys: ' +
          'Invalid wait result (abandoned + %d (%d)).',[Integer(WaitResult - WAIT_ABANDONED_0),Count]);
      WAIT_IO_COMPLETION:
        Result := wrIOCompletion;
      WAIT_TIMEOUT:
        Result := wrTimeout;
      WAIT_FAILED:
        Result := wrError;   
    else
      raise EWSOMultiWaitError.CreateFmt('WaitForMultipleHandles_Sys: Invalid wait result (%d).',[WaitResult]);
    end;
    If Result = wrError then
      Index := Integer(GetLastError);
  end
else raise EWSOMultiWaitInvalidCount.CreateFmt('WaitForMultipleHandles_Sys: Invalid handle count (%d).',[Count]);
end;

//------------------------------------------------------------------------------

Function WaitForMultipleObjects_Internal(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
var
  Handles:  array of THandle;
  i:        Integer;
begin
If Length(Objects) > 0 then
  begin
    Handles := nil;
    SetLength(Handles,Length(Objects));
    For i := Low(Objects) to High(Objects) do
      Handles[i] := Objects[i].Handle;
    Result := WaitForMultipleHandles_Sys(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,Timeout,Index,Alertable,MsgWaitOptions,WakeMask);
  end
else raise EWSOMultiWaitInvalidCount.CreateFmt('WaitForMultipleObjects_Internal: Invalid object count (%d).',[Length(Objects)]);
end;

{===============================================================================
    Wait functions (N <= MAX) - public functions
===============================================================================}

Function WaitForMultipleHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
begin
Result := WaitForMultipleHandles_Sys(Handles,Count,WaitAll,Timeout,Index,Alertable,MsgWaitOptions,WakeMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult;
begin
Result := WaitForMultipleHandles_Sys(Handles,Count,WaitAll,Timeout,Index,Alertable,[],0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForMultipleHandles_Sys(Handles,Count,WaitAll,Timeout,Index,Alertable,[],0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean): TWSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForMultipleHandles_Sys(Handles,Count,WaitAll,INFINITE,Index,False,[],0);
end;

//------------------------------------------------------------------------------

Function WaitForMultipleHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
begin
If Length(Handles) > 0 then
  Result := WaitForMultipleHandles_Sys(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,Timeout,Index,Alertable,MsgWaitOptions,WakeMask)
else
  raise EWSOMultiWaitInvalidCount.Create('WaitForMultipleHandles: Empty handle array.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult;
begin
If Length(Handles) > 0 then
  Result := WaitForMultipleHandles_Sys(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,Timeout,Index,Alertable,[],0)
else
  raise EWSOMultiWaitInvalidCount.Create('WaitForMultipleHandles: Empty handle array.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult;
var
  Index:  Integer;
begin
If Length(Handles) > 0 then
  Result := WaitForMultipleHandles_Sys(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,Timeout,Index,Alertable,[],0)
else
  raise EWSOMultiWaitInvalidCount.Create('WaitForMultipleHandles: Empty handle array.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleHandles(Handles: array of THandle; WaitAll: Boolean): TWSOWaitResult;
var
  Index:  Integer;
begin
If Length(Handles) > 0 then
  Result := WaitForMultipleHandles_Sys(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,INFINITE,Index,False,[],0)
else
  raise EWSOMultiWaitInvalidCount.Create('WaitForMultipleHandles: Empty handle array.');
end;

//------------------------------------------------------------------------------

Function WaitForMultipleObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
begin
Result := WaitForMultipleObjects_Internal(Objects,WaitAll,Timeout,Index,Alertable,MsgWaitOptions,WakeMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult;
begin
Result := WaitForMultipleObjects_Internal(Objects,WaitAll,Timeout,Index,Alertable,[],0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForMultipleObjects_Internal(Objects,WaitAll,Timeout,Index,Alertable,[],0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean): TWSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForMultipleObjects_Internal(Objects,WaitAll,INFINITE,Index,False,[],0);
end;


{===============================================================================
--------------------------------------------------------------------------------
                            Wait functions (N > MAX)
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Wait functions (N > MAX) - implementation types and variables
===============================================================================}
type
  TWSOWaitParams = record
    WaitAll:        Boolean;
    Timeout:        DWORD;
    Alertable:      Boolean;
    MsgWaitOptions: TMessageWaitOptions;
    WakeMask:       DWORD;
  end;

  TWSOWaitInternals = record
    ReadyEvent:   THandle;  // manual-reset event, not signaled
    ReleaseEvent: THandle;  // manual-reset event, not signaled
    FirstDone:    Pointer;  // interlocked access only
    FatalError:   Boolean;  // -||-
    // statistics for debuging...
    DebugInfo:    TWSOManyWaitDebugInfo;
  end;
  PWSOWaitInternals = ^TWSOWaitInternals;

  TWSOWaiterParams = record
    Handles:      array of THandle;
    IndexBase:    Integer;
    SpawnThread:  Boolean;
  end;

  TWSOWaiterProc = procedure of object;

threadvar
  ThreadDebugInfo:  TWSOManyWaitDebugInfo;

{===============================================================================
    Wait functions (N > MAX) - TWSOWaiterThread class declaration
===============================================================================}
type
  TWSOWaiterThread = class(TThread)
  protected
    fWaiterProc:  TWSOWaiterProc;
    procedure Execute; override;
  public
    constructor Create(WaiterProc: TWSOWaiterProc);
  end;

{===============================================================================
    Wait functions (N > MAX) - TWSOWaiterThread class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOWaiterThread protected methods
-------------------------------------------------------------------------------}

procedure TWSOWaiterThread.Execute;
begin
fWaiterProc;
end;

{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOWaiterThread public methods
-------------------------------------------------------------------------------}

constructor TWSOWaiterThread.Create(WaiterProc: TWSOWaiterProc);
begin
inherited Create(False);
FreeOnTerminate := False;
fWaiterProc := WaiterProc;
end;

{===============================================================================
    Wait functions (N > MAX) - TWSOWaiter class declaration
===============================================================================}
type
  TWSOWaiter = class(TObject)
  protected
    fWaitParams:        TWSOWaitParams;
    fWaitInternalsPtr:  PWSOWaitInternals;
    fHandles:           array of THandle;
    fWaiters:           array of TWSOWaiter;
    fIndexBase:         Integer;
    fRunningThread:     TWSOWaiterThread;
    fWaitResult:        TWSOWaitResult;
    fIndex:             Integer;
    procedure Initialize(WaitParams: TWSOWaitParams; WaitInternalsPtr: PWSOWaitInternals; WaiterParams: TWSOWaiterParams); virtual;
    procedure Finalize; virtual;
    procedure WaitAbort(const Msg: String); virtual;
    procedure WaitEnter; virtual;
    procedure WaitRelease; virtual;
    procedure WaitLeave; virtual;
    procedure WaitPostprocess; virtual;
  public
    constructor Create(WaitParams: TWSOWaitParams; WaitInternalsPtr: PWSOWaitInternals; WaiterParams: TWSOWaiterParams);
    destructor Destroy; override;
    procedure Run; virtual;
    property RunningThread: TWSOWaiterThread read fRunningThread;
    property WaitResult: TWSOWaitResult read fWaitResult;
    property Index: Integer read fIndex;
  end;

{===============================================================================
    Wait functions (N > MAX) - TWSOWaiter class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOWaiter protected methods
-------------------------------------------------------------------------------}

procedure TWSOWaiter.Initialize(WaitParams: TWSOWaitParams; WaitInternalsPtr: PWSOWaitInternals; WaiterParams: TWSOWaiterParams);

  Function RectCap(Count: Integer): Integer;
  begin
    If WaitParams.WaitAll then
      Result := Count
    else
      Result := Pred(Count);  // need one position for releaser
  end;

var
  i:                Integer;
  HandlesCnt:       Integer;
  ProcessedCnt:     Integer;
  HandlesPerWaiter: Integer;
  SubWaiterParams:  TWSOWaiterParams;
begin
fWaitParams := WaitParams;
fWaitInternalsPtr := WaitInternalsPtr;
fIndexBase := WaiterParams.IndexBase;
// now for the handles...
If Length(WaiterParams.Handles) > RectCap(RectifiedMaxWaitCount(fWaitParams.MsgWaitOptions)) then
  begin
    // more than an allowable maximum - branching wait
    // rectify wait params for sub-waits
    WaitParams.Alertable := False;
    WaitParams.MsgWaitOptions := [];
    WaitParams.WakeMask := 0;
    // prepare sub-waits and split handles between them
    SetLength(fHandles,RectifiedMaxWaitCount(fWaitParams.MsgWaitOptions));
    SetLength(fWaiters,Length(fHandles));
  {
    Calculate on how many handles we will be waiting directly.

      H - number of handles
      W - maximum wait slots (handles), excluding releaser and message wait 
      S - maximum wait slots (handles) in sub-wait, excluding releaser
      h - number of waited handles (we need this)

                  W = ((H - h) / S) + h
                  W = ((H - h) / S) + (Sh / S)
                  W = (H - h + Sh) / S
                 WS = H - h + Sh
             WS - H = Sh - h
             Sh - h = WS - H
           h(S - 1) = WS - H
                  h = (WS - H) / (S - 1)

      negative h -> all subwaits filled
  }
    HandlesCnt := (RectCap(Length(fHandles)) * RectCap(RectifiedMaxWaitCount([])) -
      Length(WaiterParams.Handles)) div Pred(RectCap(RectifiedMaxWaitCount([])));
    // put directly waited handles to the wait array
    If HandlesCnt > 0 then
      For i := 0 to Pred(HandlesCnt) do
        begin
          fHandles[i] := WaiterParams.Handles[i];
          fWaiters[i] := nil;
        end
    else HandlesCnt := 0;
    ProcessedCnt := HandlesCnt;
    // number of handles per waiter  
    HandlesPerWaiter := Ceil((Length(WaiterParams.Handles) - HandlesCnt){remaining handles} /
                             (RectCap(Length(fHandles)) - HandlesCnt){number of waiters});
    // create sub-waiters
    For i := HandlesCnt to RectCap(High(fHandles)) do
      begin
        // first prepare waiters parameters
        If i >= RectCap(High(fHandles)) then
          SubWaiterParams.Handles := Copy(WaiterParams.Handles,ProcessedCnt,Length(WaiterParams.Handles) - ProcessedCnt)
        else
          SubWaiterParams.Handles := Copy(WaiterParams.Handles,ProcessedCnt,HandlesPerWaiter);
        SubWaiterParams.IndexBase := fIndexBase + ProcessedCnt;
        SubWaiterParams.SpawnThread := True;
        // create the waiter and get handle of its thread
        fWaiters[i] := TWSOWaiter.Create(WaitParams,WaitInternalsPtr,SubWaiterParams);
        fHandles[i] := fWaiters[i].RunningThread.Handle;
        Inc(ProcessedCnt,Length(SubWaiterParams.Handles));
      end;
    InterlockedIncrement(WaitInternalsPtr^.DebugInfo.BranchWaits);
  end
else
  begin
    // below the maximum - leaf wait
    // prepare handles
    If WaitParams.WaitAll then
      SetLength(fHandles,Length(WaiterParams.Handles))
    else
      SetLength(fHandles,Length(WaiterParams.Handles) + 1);
    For i := Low(WaiterParams.Handles) to High(WaiterParams.Handles) do
      fHandles[i] := WaiterParams.Handles[i];
    // set up waiters (none assigned)
    SetLength(fWaiters,Length(fHandles));
    For i := Low(fWaiters) to High(fWaiters) do
      fWaiters[i] := nil;
    InterlockedIncrement(WaitInternalsPtr^.DebugInfo.LeafWaits);
  end;
// add releaser if necessary
If not WaitParams.WaitAll then
  fHandles[High(fHandles)] := WaitInternalsPtr^.ReleaseEvent;
// spawn thread that will execute the wait operation
If WaiterParams.SpawnThread then
  begin
    InterlockedIncrement(fWaitInternalsPtr^.DebugInfo.ThreadCount);
    fRunningThread := TWSOWaiterThread.Create(Self.Run);
  end
else fRunningThread := nil;
end;

//------------------------------------------------------------------------------

procedure TWSOWaiter.Finalize;
var
  i:  Integer;
begin
For i := Low(fWaiters) to High(fWaiters) do
  If Assigned(fWaiters[i]) then
    FreeAndNil(fWaiters[i]);
If Assigned(fRunningThread) then
  FreeAndNil(fRunningThread);
end;

//------------------------------------------------------------------------------

procedure TWSOWaiter.WaitAbort(const Msg: String);
begin
raise EWSOMultiWaitError.Create(Msg);
end;

//------------------------------------------------------------------------------

procedure TWSOWaiter.WaitEnter;
begin
If not InterlockedLoad(fWaitInternalsPtr^.FatalError) then
  begin
    If Assigned(fRunningThread) then
      begin
        If WaitForSingleObject(fWaitInternalsPtr^.ReadyEvent,INFINITE) <> WAIT_OBJECT_0 then
          WaitAbort('Failed wait-enter.');
      end
    else SetEvent(fWaitInternalsPtr^.ReadyEvent);
    If InterlockedLoad(fWaitInternalsPtr^.FatalError) then
      WaitAbort('Fatal error before main waiting.');
  end
else WaitAbort('Fatal error before wait-enter.');
end;

//------------------------------------------------------------------------------

procedure TWSOWaiter.WaitRelease;
begin
If not fWaitParams.WaitAll then
  If not SetEvent(fWaitInternalsPtr^.ReleaseEvent) then
    WaitAbort('Failed to release waits.');
end;

//------------------------------------------------------------------------------

procedure TWSOWaiter.WaitLeave;
var
  i:  Integer;
begin
If not InterlockedLoad(fWaitInternalsPtr^.FatalError) then
  begin
    For i := Low(fWaiters) to High(fWaiters) do
      If Assigned(fWaiters[i]) then
        fWaiters[i].RunningThread.WaitFor;
  end
else WaitAbort('Fatal error before wait-leave.');
end;

//------------------------------------------------------------------------------

procedure TWSOWaiter.WaitPostprocess;
var
  i:  Integer;
begin
// set self wait result to the "worst" result of sub-waits
If fWaitParams.WaitAll then
  For i := Low(fWaiters) to High(fWaiters) do
    If Assigned(fWaiters[i]) then
      If fWaiters[i].WaitResult > fWaitResult then
        fWaitResult := fWaiters[i].WaitResult;
end;

{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOWaiter public methods
-------------------------------------------------------------------------------}

constructor TWSOWaiter.Create(WaitParams: TWSOWaitParams; WaitInternalsPtr: PWSOWaitInternals; WaiterParams: TWSOWaiterParams);
begin
inherited Create;
Initialize(WaitParams,WaitInternalsPtr,WaiterParams);
end;

//------------------------------------------------------------------------------

destructor TWSOWaiter.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TWSOWaiter.Run;
begin
try
  If Length(fHandles) > 0 then
    begin
      // wait for signal to enter the main waiting
      WaitEnter;
      // main waiting
      fWaitResult := WaitForMultipleHandles_Sys(Addr(fHandles[Low(fHandles)]),Length(fHandles),fWaitParams.WaitAll,
        fWaitParams.Timeout,fIndex,fWaitParams.Alertable,fWaitParams.MsgWaitOptions,fWaitParams.WakeMask);
      // if first, store self reference for further processing
      InterlockedCompareExchange(fWaitInternalsPtr^.FirstDone,Pointer(Self),nil);
      // release all other waiters if waiting for one
      WaitRelease;
      // rectify the index
      If not fWaitParams.WaitAll and (fWaitResult in [wrSignaled,wrAbandoned]) then
        fIndex := fIndex + fIndexBase;
      // wait for all sub-waiters (if any) to exit
      WaitLeave;
      // process sub-results
      WaitPostprocess;
    end
  else WaitAbort('Empty handle array.');
except
  InterlockedStore(fWaitInternalsPtr^.FatalError,True);
  fWaitResult := wrFatal;
  WaitRelease;
end;
end;

{===============================================================================
    Wait functions (N > MAX) - internal functions
===============================================================================}

Function WaitForManyHandles_Internal(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
var
  WaitParams:     TWSOWaitParams;
  WaitInternals:  TWSOWaitInternals;
  WaiterParams:   TWSOWaiterParams;
  MainWaiter:     TWSOWaiter;
begin
Index := -1;
If Count > 0 then
  begin
    If Count > RectifiedMaxWaitCount(MsgWaitOptions) then
      begin
        // more than maximum waited objects (64/63), prepare data for waiters...
        // wait parameters
        WaitParams.WaitAll := WaitAll;
        WaitParams.Timeout := Timeout;
        WaitParams.Alertable := Alertable;
        WaitParams.MsgWaitOptions := MsgWaitOptions;
        WaitParams.WakeMask := WakeMask;
        // wait internals
        WaitInternals.ReadyEvent := CreateEventW(nil,True,False,nil);
        If not WaitAll then
          WaitInternals.ReleaseEvent := CreateEventW(nil,True,False,nil)
        else
          WaitInternals.ReleaseEvent := INVALID_HANDLE_VALUE;
        WaitInternals.FirstDone := nil;
        WaitInternals.FatalError := False;
        FillChar(WaitInternals.DebugInfo,SizeOf(TWSOManyWaitDebugInfo),0);
        // waiter parameters
        WaiterParams.Handles := nil;
        SetLength(WaiterParams.Handles,Count);
        Move(Handles^,Addr(WaiterParams.Handles[Low(WaiterParams.Handles)])^,Count * SizeOf(THandle));
        WaiterParams.IndexBase := 0;
        WaiterParams.SpawnThread := False;
        ReadWriteBarrier;
        try
          // create a waiter, creation of sub-waiters is done within the call
          MainWaiter := TWSOWaiter.Create(WaitParams,@WaitInternals,WaiterParams);
          try
            // wait using the main waiter
            MainWaiter.Run;
            ReadWriteBarrier;
            If not WaitInternals.FatalError and Assigned(WaitInternals.FirstDone) then
              begin
                // waiting went ok
                If not WaitAll then
                  begin
                    Result := TWSOWaiter(WaitInternals.FirstDone).WaitResult;
                    Index := TWSOWaiter(WaitInternals.FirstDone).Index;
                  end
                else Result := MainWaiter.WaitResult;
                WaitInternals.DebugInfo.Succeeded := True;
              end
            // waiting has failed miserably (index is left at default -1)
            else Result := wrError;
            // copy debug info into thread-local storage
            ThreadDebugInfo := WaitInternals.DebugInfo;
          finally
            MainWaiter.Free;
          end;
        finally
          If not WaitAll then
            CloseHandle(WaitInternals.ReleaseEvent);
          CloseHandle(WaitInternals.ReadyEvent);
        end;
      end
    // there is less than or equal to maximum number of objects, do simple wait
    else Result := WaitForMultipleHandles_Sys(Handles,Count,WaitAll,Timeout,Index,Alertable,MsgWaitOptions,WakeMask);      
  end
else raise EWSOMultiWaitInvalidCount.Create('WaitForManyHandles_Internal: Empty handle array.');
end;

//------------------------------------------------------------------------------

Function WaitForManyObjects_Internal(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
var
  Handles:  array of THandle;
  i:        Integer;
begin
If Length(Objects) > 0 then
  begin
    Handles := nil;
    SetLength(Handles,Length(Objects));
    For i := Low(Objects) to High(Objects) do
      Handles[i] := Objects[i].Handle;
    Result := WaitForManyHandles_Internal(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,Timeout,Index,Alertable,MsgWaitOptions,WakeMask);
  end
else raise EWSOMultiWaitInvalidCount.CreateFmt('WaitForManyObjects_Internal: Invalid object count (%d).',[Length(Objects)]);
end;

{===============================================================================
    Wait functions (N > MAX) - public functions
===============================================================================}

Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
begin
Result := WaitForManyHandles_Internal(Handles,Count,WaitAll,Timeout,Index,Alertable,MsgWaitOptions,WakeMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult;
begin
Result := WaitForManyHandles_Internal(Handles,Count,WaitAll,Timeout,Index,Alertable,[],0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForManyHandles_Internal(Handles,Count,WaitAll,Timeout,Index,Alertable,[],0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean): TWSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForManyHandles_Internal(Handles,Count,WaitAll,INFINITE,Index,False,[],0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
begin
If Length(Handles) > 0 then
  Result := WaitForManyHandles_Internal(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,Timeout,Index,Alertable,MsgWaitOptions,WakeMask)
else
  raise EWSOMultiWaitInvalidCount.Create('WaitForManyHandles: Empty handle array.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult;
begin
If Length(Handles) > 0 then
  Result := WaitForManyHandles_Internal(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,Timeout,Index,Alertable,[],0)
else
  raise EWSOMultiWaitInvalidCount.Create('WaitForManyHandles: Empty handle array.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyHandles(Handles: array of THandle; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult;
var
  Index:  Integer;
begin
If Length(Handles) > 0 then
  Result := WaitForManyHandles_Internal(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,Timeout,Index,Alertable,[],0)
else
  raise EWSOMultiWaitInvalidCount.Create('WaitForManyHandles: Empty handle array.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyHandles(Handles: array of THandle; WaitAll: Boolean): TWSOWaitResult;
var
  Index:  Integer;
begin
If Length(Handles) > 0 then
  Result := WaitForManyHandles_Internal(Addr(Handles[Low(Handles)]),Length(Handles),WaitAll,INFINITE,Index,False,[],0)
else
  raise EWSOMultiWaitInvalidCount.Create('WaitForManyHandles: Empty handle array.');
end;

//------------------------------------------------------------------------------

Function WaitForManyObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD = QS_ALLINPUT): TWSOWaitResult;
begin
Result := WaitForManyObjects_Internal(Objects,WaitAll,Timeout,Index,Alertable,MsgWaitOptions,WakeMask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult;
begin
Result := WaitForManyObjects_Internal(Objects,WaitAll,Timeout,Index,Alertable,[],0);
end;
  
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForManyObjects_Internal(Objects,WaitAll,Timeout,Index,Alertable,[],0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForManyObjects(Objects: array of TSimpleWinSyncObject; WaitAll: Boolean): TWSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForManyObjects_Internal(Objects,WaitAll,INFINITE,Index,False,[],0);
end;

//------------------------------------------------------------------------------

Function WaitForManyHandles_GetDebugInfo: TWSOManyWaitDebugInfo;
begin
Result := ThreadDebugInfo;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               Utility functions
--------------------------------------------------------------------------------
===============================================================================}

Function WaitResultToStr(WaitResult: TWSOWaitResult): String;
const
  WR_STRS: array[TWSOWaitResult] of String = ('Signaled','Abandoned','IOCompletion','Message','Timeout','Error','Fatal');
begin
If (WaitResult >= Low(TWSOWaitResult)) and (WaitResult <= High(TWSOWaitResult)) then
  Result := WR_STRS[WaitResult]
else
  Result := '<invalid>';
end;


{===============================================================================
--------------------------------------------------------------------------------
                               Unit initialization
--------------------------------------------------------------------------------
===============================================================================}

procedure HandleArrayItemsStrideCheck(Handles: array of THandle);
var
  TestArray:  array of THandle;
begin
{
  Check whether array of handles (including open array) does or does not have
  some gaps (due to alignment) between items - current implementation assumes
  it doesn't.
}
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If (PtrUInt(Addr(Handles[Succ(Low(Handles))])) - PtrUInt(Addr(Handles[Low(Handles)]))) <> SizeOf(THandle) then
  raise EWSOException.Create('HandleArrayItemsStrideCheck: Unsupported implementation detail (open array items alignment).');
TestArray := nil;
SetLength(TestArray,2);
If (PtrUInt(Addr(TestArray[Succ(Low(TestArray))])) - PtrUInt(Addr(TestArray[Low(TestArray)]))) <> SizeOf(THandle) then
  raise EWSOException.Create('HandleArrayItemsStrideCheck: Unsupported implementation detail (array items alignment).');
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure UnitInitialize;
begin
WSO_SHAREDDATA_THREADLOCK := TCriticalSection.Create;
HandleArrayItemsStrideCheck([THandle(0),THandle(1)]);
end;

//------------------------------------------------------------------------------

procedure UnitFinalize;
begin
FreeAndNil(WSO_SHAREDDATA_THREADLOCK);
end;

//------------------------------------------------------------------------------

initialization
  UnitInitialize;

finalization
  UnitFinalize;

end.

