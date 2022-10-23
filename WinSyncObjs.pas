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

  Version 1.2 (2022-..-..)

  Last change 2022-..-..

  ©2016-2022 František Milt

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

  EWSOOpenError              = class(EWSOException);
  EWSOHandleDuplicationError = class(EWSOException);

  EWSOInvalidHandle = class(EWSOException);
  EWSOInvalidObject = class(EWSOException);
  EWSOInvalidValue  = class(EWSOException);

  EWSOInitializationError = class(EWSOException);
  EWSOWaitError           = class(EWSOException);
  //EWSOAutoCycleError      = class(EWSOException);

  EWSOEventError     = class(EWSOException);
  EWSOMutexError     = class(EWSOException);
  EWSOSemaphoreError = class(EWSOException);
  EWSOTimerError     = class(EWSOException);

  EWSOMultiWaitInvalidCount = class(EWSOException);
  EWSOMultiWaitError        = class(EWSOException);

  EWSOTimeConversionError = class(EWSOException);  

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
  private
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
{===============================================================================
    TWinSyncObject - class declaration
===============================================================================}
type
  TWinSyncObject = class(TCustomObject)
  protected
    fLastError: DWORD;
    fName:      String;
    Function RectifyAndSetName(const Name: String): Boolean; virtual;
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
  funtion. If it still is, you should treat it as really serious error.
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

    WARNING - TConditionVariable and TConditionVariableEx are considered one
              type of synchronizer, the latter being only an extension of the
              former, so they occupy the same namespace. But it is NOT possible
              to create an instace of TConditionVariableEx by opening (using
              Open or DuplicateFrom constructor) instance of TConditionVariable.


  To properly use complex windows synchronization object (TConditioVariable,
  TConditioVariableEx, TBarrier, TReadWriteLock), create one progenitor
  instance and use this instance only in the thread that created it.

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

  TWSOSharedDataLock = record
    case Boolean of
      True:   (ProcessSharedLock: THandle);   // mutex
      False:  (ThreadSharedLock:  TCriticalSection);
  end;

  // all object shared data must start with this structure
  TWSOCommonSharedData = packed record
    SharedUserData: TWSOSharedUserData;
    RefCount:       Int32;                // used only in thread-shared mode
  end;
  PWSOCommonSharedData = ^TWSOCommonSharedData;

{===============================================================================
    TComplexWinSyncObject - class declaration
===============================================================================}
type
  TComplexWinSyncObject = class(TWinSyncObject){$IFNDEF CompTest}{$message 'revisit'}{$ENDIF}
  protected
    fProcessShared:   Boolean;
    fSharedDataLock:  TWSOSharedDataLock;
    fNamedSharedItem: TNamedSharedItem;   // unused in thread-shared mode
    fSharedData:      Pointer;
    Function GetSharedUserDataPtr: PWSOSharedUserData; virtual;
    Function GetSharedUserData: TWSOSharedUserData; virtual;
    procedure SetSharedUserData(Value: TWSOSharedUserData); virtual;
    Function RectifyAndSetName(const Name: String): Boolean; override;
    procedure CheckAndSetHandle(out Destination: THandle; Handle: THandle); virtual;
    procedure DuplicateAndSetHandle(out Destination: THandle; Handle: THandle); virtual;
    procedure InternalOpen(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String); virtual;
    // shared data lock management methods
    class Function GetSharedDataLockSuffix: String; virtual; abstract;
    procedure CreateSharedDataLock(SecurityAttributes: PSecurityAttributes); virtual;
    procedure OpenSharedDataLock(DesiredAccess: DWORD; InheritHandle: Boolean); virtual;
    procedure DestroySharedDataLock; virtual;
    procedure LockSharedData; virtual;
    procedure UnlockSharedData; virtual;
    // shared data management methods
    procedure AllocateSharedData; virtual;
    procedure FreeSharedData; virtual;
    // locks management methods (internal workings)
    procedure CreateLocks(SecurityAttributes: PSecurityAttributes); virtual; abstract;
    procedure OpenLocks(DesiredAccess: DWORD; InheritHandle: Boolean); virtual; abstract;
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); virtual; abstract; // should also set class-specific shared data pointer
    procedure DestroyLocks; virtual; abstract;
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; const Name: String); overload; virtual;
    constructor Create(const Name: String); overload; virtual;
    constructor Create; overload; virtual;
    constructor Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String); overload; virtual;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF}); overload; virtual;
  {
    Use DuplicateFrom to create instance that accesses the same synchronization
    primitive(s) and shared data as the source object.

    If the source object is process-shared, DuplicateFrom is equivalent to Open
    constructor.

    WARNING - duplication is NOT thread safe, make sure you do not free the
              duplicated object before the duplication finishes (constructor
              returns).
  }
    constructor DuplicateFrom(SourceObject: TComplexWinSyncObject); virtual;
    destructor Destroy; override;
    property ProcessShared: Boolean read fProcessShared;
    property SharedUserDataPtr: PWSOSharedUserData read GetSharedUserDataPtr;
    property SharedUserData: TWSOSharedUserData read GetSharedUserData write SetSharedUserData;
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

  TWSOPredicateCheckEvent = procedure(Sender: TObject; var Predicate: Boolean) of object;
  TWSOPredicateCheckCallback = procedure(Sender: TObject; var Predicate: Boolean);

  TWSODataAccessEvent = procedure(Sender: TObject; var WakeOptions: TWSOWakeOptions) of object;
  TWSODataAccessCallback = procedure(Sender: TObject; var WakeOptions: TWSOWakeOptions);

{===============================================================================
    TConditionVariable - class declaration
===============================================================================}
type
  TConditionVariable = class(TComplexWinSyncObject){$IFNDEF CompTest}{$message 'revisit'}{$ENDIF}
  protected
    fWaitLock:                  THandle;              // semaphore
    fBroadcastDoneLock:         THandle;              // manual-reset event
    fCondSharedData:            PWSOCondSharedData;
    // autocycle events
    fOnPredicateCheckEvent:     TWSOPredicateCheckEvent;
    fOnPredicateCheckCallback:  TWSOPredicateCheckCallback;
    fOnDataAccessEvent:         TWSODataAccessEvent;
    fOnDataAccessCallback:      TWSODataAccessCallback;
    class Function GetSharedDataLockSuffix: String; override;
    // shared data management methods
    procedure AllocateSharedData; override;
    procedure FreeSharedData; override;
    // locks management methods
    procedure CreateLocks(SecurityAttributes: PSecurityAttributes); override;
    procedure OpenLocks(DesiredAccess: DWORD; InheritHandle: Boolean); override;
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); override;
    procedure DestroyLocks; override;
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
    procedure Sleep(DataLock: THandle; Timeout: DWORD = INFINITE; Alertable: Boolean = False); overload; virtual;
    procedure Sleep(DataLock: TSimpleWinSyncObject; Timeout: DWORD = INFINITE; Alertable: Boolean = False); overload; virtual;
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
    procedure AutoCycle(DataLock: THandle; Timeout: DWORD = INFINITE; Alertable: Boolean = False); overload; virtual;
    procedure AutoCycle(DataLock: TSimpleWinSyncObject; Timeout: DWORD = INFINITE; Alertable: Boolean = False); overload; virtual;
    // events
    property OnPredicateCheckEvent: TWSOPredicateCheckEvent read fOnPredicateCheckEvent write fOnPredicateCheckEvent;
    property OnPredicateCheckCallback: TWSOPredicateCheckCallback read fOnPredicateCheckCallback write fOnPredicateCheckCallback;
    property OnPredicateCheck: TWSOPredicateCheckEvent read fOnPredicateCheckEvent write fOnPredicateCheckEvent;
    property OnDataAccessEvent: TWSODataAccessEvent read fOnDataAccessEvent write fOnDataAccessEvent;    
    property OnDataAccessCallback: TWSODataAccessCallback read fOnDataAccessCallback write fOnDataAccessCallback;
    property OnDataAccess: TWSODataAccessEvent read fOnDataAccessEvent write fOnDataAccessEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              TConditionVariableEx
--------------------------------------------------------------------------------
===============================================================================}
{
  Only an extension of TConditionVariable with integrated data lock (use methods
  Lock and Unlock to manipulate it). New versions of methods Sleep and AutoCycle
  without the DataLock parameter are using the integrated data lock for that
  purpose.

    WARNING - as in the case of TConditionVariable, be wary of how many times
              you lock the integrated data lock. A mutex is used internally, so
              mutliple locks can result in a deadlock in sleep method.
}
{===============================================================================
    TConditionVariableEx - class declaration
===============================================================================}
type
  TConditionVariableEx = class(TConditionVariable){$IFNDEF CompTest}{$message 'revisit'}{$ENDIF}
  protected
    fDataLock:  THandle;  // mutex
    procedure CreateLocks(SecurityAttributes: PSecurityAttributes); override;
    procedure OpenLocks(DesiredAccess: DWORD; InheritHandle: Boolean); override;
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); override;
    procedure DestroyLocks; override;    
  public
    procedure Lock; virtual;
    procedure Unlock; virtual;
    procedure Sleep(Timeout: DWORD = INFINITE; Alertable: Boolean = False); overload; virtual;
    procedure AutoCycle(Timeout: DWORD = INFINITE; Alertable: Boolean = False); overload; virtual; // uses internal data synchronizer
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
    MaxWaitCount:     Int32;    // invariant value, set only once
    WaitCount:        Int32;
    MaxWaitCountSet:  Boolean;  // invariant value, set only once  
    Releasing:        Boolean;
  end;
  PWSOBarrierSharedData = ^TWSOBarrierSharedData;

{===============================================================================
    TBarrier - class declaration
===============================================================================}
type
  TBarrier = class(TComplexWinSyncObject){$IFNDEF CompTest}{$message 'revisit'}{$ENDIF}
  protected
    fReleaseLock:       THandle;                // manual-reset event
    fWaitLock:          THandle;                // manual-reset event
    fBarrierSharedData: PWSOBarrierSharedData;
    Function GetCount: Integer; virtual;
    class Function GetSharedDataLockSuffix: String; override;
    // shared data management methods
    procedure AllocateSharedData; override;
    procedure FreeSharedData; override;
    // locks management methods
    procedure CreateLocks(SecurityAttributes: PSecurityAttributes); override;
    procedure OpenLocks(DesiredAccess: DWORD; InheritHandle: Boolean); override;
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); override;
    procedure DestroyLocks; override;
  public
    constructor Create(SecurityAttributes: PSecurityAttributes; const Name: String); override;
    constructor Create(const Name: String); override;
    constructor Create; override;
    constructor Create(SecurityAttributes: PSecurityAttributes; Count: Integer; const Name: String); overload;
    constructor Create(Count: Integer; const Name: String); overload;
    constructor Create(Count: Integer); overload;
    Function Wait: Boolean; virtual;
  {
    Releases all waiting threads, irrespective of their count, and sets the
    barrier back to a non-signaled (blocking) state.  
  }
    Function Release: Integer; virtual;
    property Count: Integer read GetCount;
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
    WriteCount:     Int32;
  end;
  PWSORWLockSharedData = ^TWSORWLockSharedData;

{===============================================================================
    TReadWriteLock - class declaration
===============================================================================}
type
  TReadWriteLock = class(TComplexWinSyncObject){$IFNDEF CompTest}{$message 'revisit'}{$ENDIF}
  protected
    fReadLock:          THandle;    // manual-reset event
    fWriteWaitLock:     THandle;    // manual-reset event
    fWriteLock:         THandle;    // mutex
    fRWLockSharedData:  PWSORWLockSharedData;
    class Function GetSharedDataLockSuffix: String; override;
    // shared data management methods
    procedure AllocateSharedData; override;
    procedure FreeSharedData; override;
    // locks management methods
    procedure CreateLocks(SecurityAttributes: PSecurityAttributes); override;
    procedure OpenLocks(DesiredAccess: DWORD; InheritHandle: Boolean); override;
    procedure DuplicateLocks(SourceObject: TComplexWinSyncObject); override;
    procedure DestroyLocks; override;
  public
    procedure ReadLock; virtual;
    procedure ReadUnlock; virtual;
    procedure WriteLock; virtual;
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
  are met, an error occurs or the wait times-out (which of these occured is
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

  Waiting on many objects is not completely tested and, given its complexity
  and somewhat "thick" code, should be considered strictly experimental.

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
  to some extent on manual-reset events.

    NOTE - When calling WaitForManyHandles with number of wait objects (handles)
           lower than the maximum enforced for the system calls, it will behave
           exactly the same as WaitForMultipleObjects. In fact, it will redirect
           to that set of functions, so you can safely use it in such scenario.

  And now for some implementation details...

    Since Windows API function WaitForMultipleObjects does not allow for more
    than 64 or 63 objects to be waited on (63 in case of message waiting - that
    is, waiting that can return when a message is delivered to the waiting
    thread), we have to do our own implementation for cases where waiting on
    more objects is required.
    Generally, waiting on more than about a dozen objects is a sign of bad
    design, but that will not stop us, right?

    The handles we want to wait on are split into wait groups, each containing
    at most 64 objects/handles. For each wait group, a waiter thread is created,
    this thread will wait on its wait group using normal (limited-count)
    waiting.
    We will then wait on the created waiter threads to finish. This is done in
    a so-called wait level. Note that, in this waiting, the number of threads
    we will wait for is too limited to 64.

    If there are more wait groups than 64, the selected solution is to create
    a new thread that, instead of waiting for one wait group, will wait for
    another wait level. This thread will be added in current level to a waiting
    for waiter threads which are already waiting on wait groups.

    Lets use some graphics to clarify:

      invoker - wait_level_0 --- waiter_thread - wait_group_0
                              |- waiter_thread - wait_group_1
                             ...
                              |- waiter_thread - wait_group_n
                              |- waiter_thread - wait_level_1 --- waiter_thread - wait_group_n+1
                                                               |- waiter_thread - wait_group_n+2
                                                              ...
                                                               |- waiter_thread - wait_group_m
                                                               |- waiter_thread - wait_level_2 --- waiter_thread - wait_group_m+1
                                                                                                |- waiter_thread - wait_group_m+2
                                                                                               ...

    ...and so on. The tree can be as deep as resources allow. Note that the tree
    is not balanced - that is, only one branch in a level can be another level.
    This was selected to simplify implementation - since each level can wait for
    more than 4000 objects, it is unlikely that any sane use will spawn more
    than one level.
}
Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD = QS_ALLINPUT): TWSOWaitResult; overload;
Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; Alertable: Boolean = False): TWSOWaitResult; overload;
Function WaitForManyHandles(Handles: PHandle; Count: Integer; WaitAll: Boolean): TWSOWaitResult; overload;
{$IFNDEF CompTest}{$message 'revisit'}{$ENDIF}
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

{===============================================================================
--------------------------------------------------------------------------------
                                TCriticalSection
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCriticalSection - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCriticalSection - private methods
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
If SourceObject is Self.ClassType then
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

Function WSO_CPLX_SHARED_ITEMSIZE: Integer; // orignally a constant
begin
Result := MaxIntValue([
  SizeOf(TWSOCondSharedData),
  SizeOf(TWSOBarrierSharedData),
  SizeOf(TWSORWLockSharedData)]);
end;

//------------------------------------------------------------------------------

// because there are incorrect declarations...
Function SignalObjectAndWait(
  hObjectToSignal:  THandle;
  hObjectToWaitOn:  THandle;
  dwMilliseconds:   DWORD;
  bAlertable:       BOOL): DWORD; stdcall; external kernel32;

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

procedure TComplexWinSyncObject.InternalOpen(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String);
begin
If RectifyAndSetName(Name) then
  begin
    OpenSharedDataLock(DesiredAccess,InheritHandle);
    AllocateSharedData;
    OpenLocks(DesiredAccess,InheritHandle);
  end
else raise EWSOOpenError.Create('TComplexWinSyncObject.InternalOpen: Cannot open unnamed object.');
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.CreateSharedDataLock(SecurityAttributes: PSecurityAttributes);
begin
If fProcessShared then
  begin
    CheckAndSetHandle(fSharedDataLock.ProcessSharedLock,
      CreateMutexW(SecurityAttributes,RectBool(False),PWideChar(StrToWide(fName + GetSharedDataLockSuffix))));
  end
else
  begin
    fSharedDataLock.ThreadSharedLock := TCriticalSection.Create;
    fSharedDataLock.ThreadSharedLock.FreeOnRelease := True;
    fSharedDataLock.ThreadSharedLock.Acquire;
  end;
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.OpenSharedDataLock(DesiredAccess: DWORD; InheritHandle: Boolean);
begin
CheckAndSetHandle(fSharedDataLock.ProcessSharedLock,
  OpenMutexW(DesiredAccess or MUTEX_MODIFY_STATE,InheritHandle,PWideChar(StrToWide(fName + GetSharedDataLockSuffix))));
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.DestroySharedDataLock;
begin
If fProcessShared then
  CloseHandle(fSharedDataLock.ProcessSharedLock)
else
  If Assigned(fSharedDataLock.ThreadSharedLock) then
    fSharedDataLock.ThreadSharedLock.Release; // auto-free should be on
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.LockSharedData;
begin
If fProcessShared then
  begin
    case WaitForSingleObject(fSharedDataLock.ProcessSharedLock,INFINITE) of
      WAIT_OBJECT_0,
      WAIT_ABANDONED:;  // good result, do nothing
      WAIT_FAILED:
        raise EWSOWaitError.CreateFmt('TComplexWinSyncObject.LockSharedData: Lock not acquired, cannot proceed (%d).',[GetLastError]);
    else
      raise EWSOWaitError.Create('TComplexWinSyncObject.LockSharedData: Lock not acquired, cannot proceed.');
    end;  
  end
else fSharedDataLock.ThreadSharedLock.Enter;
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.UnlockSharedData;
begin
If fProcessShared then
  begin
    If not ReleaseMutex(fSharedDataLock.ProcessSharedLock) then
      raise EWSOMutexError.CreateFmt('TComplexWinSyncObject.UnlockSharedData: Lock not released, cannot proceed (%d).',[GetLastError]);
  end
else fSharedDataLock.ThreadSharedLock.Leave;
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.AllocateSharedData;
begin
If fProcessShared then
  begin
    fNamedSharedItem := TNamedSharedItem.Create(fName,WSO_CPLX_SHARED_ITEMSIZE,WSO_CPLX_SHARED_NAMESPACE);
    fSharedData := fNamedSharedItem.Memory;
  end
else
  begin
    fSharedData := AllocMem(WSO_CPLX_SHARED_ITEMSIZE);
    InterlockedStore(PWSOCommonSharedData(fSharedData)^.RefCount,1);
  end;
end;

//------------------------------------------------------------------------------

procedure TComplexWinSyncObject.FreeSharedData;
begin
If Assigned(fSharedData) then
  begin
    If fProcessShared then
      begin
        If Assigned(fNamedSharedItem) then
          FreeAndNil(fNamedSharedItem);
      end
    else
      begin
        If InterlockedDecrement(PWSOCommonSharedData(fSharedData)^.RefCount) <= 0 then
          FreeMem(fSharedData,WSO_CPLX_SHARED_ITEMSIZE);
      end;
    fSharedData := nil;
  end;
end;

{-------------------------------------------------------------------------------
    TComplexWinSyncObject - public methods
-------------------------------------------------------------------------------}

constructor TComplexWinSyncObject.Create(SecurityAttributes: PSecurityAttributes; const Name: String);
begin
inherited Create;
RectifyAndSetName(Name);
// following is the same for process-shared and thread-shared
CreateSharedDataLock(SecurityAttributes);
AllocateSharedData;
CreateLocks(SecurityAttributes);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TComplexWinSyncObject.Create(const Name: String);
begin
Create(nil,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TComplexWinSyncObject.Create;
begin
Create(nil,'');
end;

//------------------------------------------------------------------------------

constructor TComplexWinSyncObject.Open(DesiredAccess: DWORD; InheritHandle: Boolean; const Name: String);
begin
inherited Create;
InternalOpen(DesiredAccess,InheritHandle,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TComplexWinSyncObject.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
Open(SYNCHRONIZE,False,Name);
end;

//------------------------------------------------------------------------------

constructor TComplexWinSyncObject.DuplicateFrom(SourceObject: TComplexWinSyncObject);
begin
inherited Create;
If SourceObject is Self.ClassType then
  begin
    If not SourceObject.ProcessShared then
      begin
        fProcessShared := False;
        {
          Increase reference counter. If it is above 1, all is good and
          continue.
          But if it is below or equal to 1, it means the source was probably
          (being) destroyed - raise an exception.
        }
        If InterlockedIncrement(PWSOCommonSharedData(SourceObject.fSharedData)^.RefCount) > 1 then
          begin
            SourceObject.fSharedDataLock.ThreadSharedLock.Acquire;
            fSharedDataLock.ThreadSharedLock := SourceObject.fSharedDataLock.ThreadSharedLock;
            fSharedData := SourceObject.fSharedData;
            DuplicateLocks(SourceObject);
          end
        else raise EWSOInvalidObject.Create('TComplexWinSyncObject.DuplicateFrom: Source object is in an inconsistent state.');
      end
    else InternalOpen(SYNCHRONIZE,False,SourceObject.Name);
  end
else raise EWSOInvalidObject.CreateFmt('TComplexWinSyncObject.DuplicateFrom: Incompatible source object (%s).',[SourceObject.ClassName]);
end;

//------------------------------------------------------------------------------

destructor TComplexWinSyncObject.Destroy;
begin
DestroyLocks;
FreeSharedData;
DestroySharedDataLock;
inherited;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TConditionVariable
--------------------------------------------------------------------------------
===============================================================================}
const
  WSO_COND_SUFFIX_SHAREDDATA = '@cnd_slk';
  WSO_COND_SUFFIX_WAITLOCK   = '@cnd_wlk';
  WSO_COND_SUFFIX_BDONELOCK  = '@cnd_blk';
  WSO_COND_SUFFIX_DATALOCK   = '@cnd_dlk';

{===============================================================================
    TConditionVariable - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TConditionVariable - protected methods
-------------------------------------------------------------------------------}

class Function TConditionVariable.GetSharedDataLockSuffix: String;
begin
Result := WSO_COND_SUFFIX_SHAREDDATA;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.AllocateSharedData;
begin
inherited;
fCondSharedData := PWSOCondSharedData(fSharedData);
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.FreeSharedData;
begin
fCondSharedData := nil;
inherited;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.CreateLocks(SecurityAttributes: PSecurityAttributes);
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
      CreateSemaphoreW(SecurityAttributes,0,DWORD($7FFFFFFF),PWideChar(StrToWide(fName + WSO_COND_SUFFIX_WAITLOCK))));
    CheckAndSetHandle(fBroadcastDoneLock,
      CreateEventW(SecurityAttributes,True,False,PWideChar(StrToWide(fName + WSO_COND_SUFFIX_BDONELOCK))));
  end
else
  begin
    CheckAndSetHandle(fWaitLock,CreateSemaphoreW(SecurityAttributes,0,DWORD($7FFFFFFF),nil));
    CheckAndSetHandle(fBroadcastDoneLock,CreateEventW(SecurityAttributes,True,False,nil));
  end;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.OpenLocks(DesiredAccess: DWORD; InheritHandle: Boolean);
begin
CheckAndSetHandle(fWaitLock,
  OpenSemaphoreW(DesiredAccess or SEMAPHORE_MODIFY_STATE,InheritHandle,PWideChar(StrToWide(fName + WSO_COND_SUFFIX_WAITLOCK))));
CheckAndSetHandle(fBroadcastDoneLock,
  OpenEventW(DesiredAccess or EVENT_MODIFY_STATE,InheritHandle,PWideChar(StrToWide(fName + WSO_COND_SUFFIX_BDONELOCK))));
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.DuplicateLocks(SourceObject: TComplexWinSyncObject);
begin
fCondSharedData := PWSOCondSharedData(fSharedData);
DuplicateAndSetHandle(fWaitLock,TConditionVariable(SourceObject).fWaitLock);
DuplicateAndSetHandle(fBroadcastDoneLock,TConditionVariable(SourceObject).fBroadcastDoneLock);
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.DestroyLocks;
begin
CloseHandle(fBroadcastDoneLock);
CloseHandle(fWaitLock);
end;

//------------------------------------------------------------------------------

Function TConditionVariable.DoOnPredicateCheck: Boolean;
begin
Result := False;
If Assigned(fOnPredicateCheckEvent) then
  fOnPredicateCheckEvent(Self,Result);
If Assigned(fOnPredicateCheckCallback) then
  fOnPredicateCheckCallback(Self,Result);
end;

//------------------------------------------------------------------------------

Function TConditionVariable.DoOnDataAccess: TWSOWakeOptions;
begin
If Assigned(fOnDataAccessEvent) or Assigned(fOnDataAccessCallback) then
  begin
    Result := [];
    If Assigned(fOnDataAccessEvent) then
      fOnDataAccessEvent(Self,Result);
    If Assigned(fOnDataAccessCallback) then
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

procedure TConditionVariable.Sleep(DataLock: THandle; Timeout: DWORD = INFINITE; Alertable: Boolean = False);

  Function InternalWait(IsFirstWait: Boolean): Boolean;
  var
    WaitResult: DWORD;
  begin
    If IsFirstWait then
      WaitResult := SignalObjectAndWait(DataLock,fWaitLock,Timeout,Alertable)
    else
      WaitResult := WaitForSingleObjectEx(fWaitLock,Timeout,Alertable);
    // note that we are waiting on semaphore, so abandoned is not a good result  
    Result := WaitResult = WAIT_OBJECT_0{signaled};
    If WaitResult = WAIT_FAILED then
      raise EWSOWaitError.CreateFmt('TConditionVariable.Sleep.InternalWait: Sleep failed (%d)',[GetLastError]);
  end;

var
  FirstWait:  Boolean;
  ExitWait:   Boolean;
begin
LockSharedData;
try
  Inc(fCondSharedData^.WaitCount);
finally
  UnlockSharedData;
end;
FirstWait := True;
ExitWait := False;
repeat
  If InternalWait(FirstWait) then
    begin
      LockSharedData;
      try
        If fCondSharedData^.WakeCount > 0 then
          begin
            Dec(fCondSharedData^.WakeCount);
            If fCondSharedData^.Broadcasting and (fCondSharedData^.WakeCount <= 0) then
              begin
                fCondSharedData^.WakeCount := 0;
                fCondSharedData^.Broadcasting := False;
                If not SetEvent(fBroadcastDoneLock) then
                  raise EWSOEventError.CreateFmt('TConditionVariable.Sleep: Failed to set broadcast-done-lock event (%d).',[GetLastError]);
              end;
            ExitWait := True; // normal wakeup               
          end;
        // if the WakeCount was 0, then re-enter waiting
        {$IFNDEF CompTest}{$message 'recalculate timeout'}{$ENDIF}
      finally
        UnlockSharedData;
      end;
    end
  else ExitWait := True;  // timeout or spurious wakeup (eg. APC)
  FirstWait := False;     // in case the cycle repeats and re-enters waiting (so the DataLock is not signaled again)
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

procedure TConditionVariable.Sleep(DataLock: TSimpleWinSyncObject; Timeout: DWORD = INFINITE; Alertable: Boolean = False);
begin
If (DataLock is TEvent) or (DataLock is TMutex) or (DataLock is TSemaphore) then
  Sleep(DataLock.Handle,Timeout,Alertable)
else
  raise EWSOInvalidObject.CreateFmt('TConditionVariable.Sleep: Unsupported data synchronizer object type (%s),',[DataLock.ClassName]);
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.Wake;
begin
LockSharedData;
try
  If fCondSharedData^.WaitCount > 0 then
    begin
      Dec(fCondSharedData^.WaitCount);
      Inc(fCondSharedData^.WakeCount);
      If not ReleaseSemaphore(fWaitLock,1,nil) then
        raise EWSOSemaphoreError.CreateFmt('TConditionVariable.Wake: Failed to release wait-lock semaphore (%d).',[GetLastError]);
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
          raise EWSOEventError.CreateFmt('TConditionVariable.WakeAll: Failed to reset broadcast-done-lock event (%d).',[GetLastError]);
      fCondSharedData^.Broadcasting := True;
      If not ReleaseSemaphore(fWaitLock,Waiters,nil) then
        raise EWSOSemaphoreError.CreateFmt('TConditionVariable.WakeAll: Failed to release wait-lock semaphore (%d).',[GetLastError]);
    end;
finally
  UnlockSharedData;
end;  
If Waiters > 0 then
  case WaitForSingleObject(fBroadcastDoneLock,INFINITE) of
    WAIT_OBJECT_0:;
    WAIT_FAILED:
      raise EWSOWaitError.CreateFmt('TConditionVariable.WakeAll: Wait for broadcast failed (%d).',[GetLastError]);
  else
    raise EWSOWaitError.Create('TConditionVariable.WakeAll: Wait for broadcast failed.');
  end;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.AutoCycle(DataLock: THandle; Timeout: DWORD = INFINITE; Alertable: Boolean = False);
var
  WakeOptions:  TWSOWakeOptions;
begin
If Assigned(fOnPredicateCheckEvent) or Assigned(fOnPredicateCheckEvent) then
  begin
    // lock synchronizer
    case WaitForSingleObject(DataLock,INFINITE) of
      WAIT_OBJECT_0,
      WAIT_ABANDONED:
        begin
          // test predicate and wait condition
          while not DoOnPredicateCheck do
            Sleep(DataLock,Timeout,Alertable);
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

procedure TConditionVariable.AutoCycle(DataLock: TSimpleWinSyncObject; Timeout: DWORD = INFINITE; Alertable: Boolean = False);
var
  WaitResult:   TWSOWaitResult;
  WakeOptions:  TWSOWakeOptions;
begin
If Assigned(fOnPredicateCheckEvent) or Assigned(fOnPredicateCheckCallback) then
  begin
    If (DataLock is TEvent) or (DataLock is TMutex) or (DataLock is TSemaphore) then
      begin
        // lock synchronizer
        WaitResult := DataLock.WaitFor(INFINITE,False);
        case WaitResult of
          wrSignaled,
          wrAbandoned:
            // abandoned is allowed only for mutexes
            If (WaitResult <> wrAbandoned) or (DataLock is TMutex) then
              begin
                // test predicate and wait condition
                while not DoOnPredicateCheck do
                  Sleep(DataLock,Timeout,Alertable);
                // access protected data
                WakeOptions := DoOnDataAccess;
                // wake waiters before unlock
                If (woWakeBeforeUnlock in WakeOptions) then
                  SelectWake(WakeOptions);
                // unlock synchronizer
                If DataLock is TEvent then
                  TEvent(DataLock).SetEventStrict
                else If DataLock is TMutex then
                  TMutex(DataLock).ReleaseMutexStrict
                else
                  TSemaphore(DataLock).ReleaseSemaphoreStrict;
                // wake waiters after unlock
                If not(woWakeBeforeUnlock in WakeOptions) then
                  SelectWake(WakeOptions);
              end
            else raise EWSOWaitError.Create('TConditionVariable.AutoCycle: Failed to lock data synchronizer.');
          wrError:
            raise EWSOWaitError.CreateFmt('TConditionVariable.AutoCycle: Failed to lock data synchronizer (%d).',[DataLock.LastError]);
        else
          raise EWSOWaitError.Create('TConditionVariable.AutoCycle: Failed to lock data synchronizer.');
        end;
      end
    else raise EWSOInvalidObject.CreateFmt('TConditionVariable.AutoCycle: Unsupported data synchronizer object type (%s),',[DataLock.ClassName]);
  end;
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
    TConditionVariableEx - public methods
-------------------------------------------------------------------------------}

procedure TConditionVariableEx.CreateLocks(SecurityAttributes: PSecurityAttributes);
begin
inherited CreateLocks(SecurityAttributes);
If fProcessShared then
  CheckAndSetHandle(fDataLock,CreateMutexW(SecurityAttributes,RectBool(False),PWideChar(StrToWide(fName + WSO_COND_SUFFIX_DATALOCK))))
else
  CheckAndSetHandle(fDataLock,CreateMutexW(SecurityAttributes,RectBool(False),nil));
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.OpenLocks(DesiredAccess: DWORD; InheritHandle: Boolean);
begin
inherited OpenLocks(DesiredAccess,InheritHandle);
CheckAndSetHandle(fDataLock,
  OpenMutexW(DesiredAccess or MUTEX_MODIFY_STATE,InheritHandle,PWideChar(StrToWide(fName + WSO_COND_SUFFIX_DATALOCK))));
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.DuplicateLocks(SourceObject: TComplexWinSyncObject);
begin
inherited DuplicateLocks(SourceObject);
DuplicateAndSetHandle(fDataLock,TConditionVariableEx(SourceObject).fDataLock);
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.DestroyLocks;
begin
inherited;
CloseHandle(fDataLock);
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

procedure TConditionVariableEx.Sleep(Timeout: DWORD = INFINITE; Alertable: Boolean = False);
begin
Sleep(fDataLock,Timeout,Alertable);
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.AutoCycle(Timeout: DWORD = INFINITE; Alertable: Boolean = False);
begin
AutoCycle(fDataLock,Timeout,Alertable);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TBarrier
--------------------------------------------------------------------------------
===============================================================================}
const
  WSO_BARR_SUFFIX_SHAREDDATA = '@brr_slk';
  WSO_BARR_SUFFIX_RELLOCK    = '@brr_rlk';
  WSO_BARR_SUFFIX_WAITLOCK   = '@brr_wlk';

{===============================================================================
    TBarrier - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBarrier - protected methods
-------------------------------------------------------------------------------}

Function TBarrier.GetCount: Integer;
begin
Result := InterlockedLoad(fBarrierSharedData^.MaxWaitCount);
end;

//------------------------------------------------------------------------------

class Function TBarrier.GetSharedDataLockSuffix: String;
begin
Result := WSO_BARR_SUFFIX_SHAREDDATA;
end;

//------------------------------------------------------------------------------

procedure TBarrier.AllocateSharedData;
begin
inherited;
fBarrierSharedData := PWSOBarrierSharedData(fSharedData);
end;

//------------------------------------------------------------------------------

procedure TBarrier.FreeSharedData;
begin
fBarrierSharedData := nil;
inherited;
end;

//------------------------------------------------------------------------------

procedure TBarrier.CreateLocks(SecurityAttributes: PSecurityAttributes);
begin
If fProcessShared then
  begin
    CheckAndSetHandle(fReleaseLock,
      CreateEventW(SecurityAttributes,True,False,PWideChar(StrToWide(fName + WSO_BARR_SUFFIX_RELLOCK))));
    CheckAndSetHandle(fWaitLock,
      CreateEventW(SecurityAttributes,True,False,PWideChar(StrToWide(fName + WSO_BARR_SUFFIX_WAITLOCK))));
  end
else
  begin
    CheckAndSetHandle(fReleaseLock,CreateEventW(SecurityAttributes,True,False,nil));
    CheckAndSetHandle(fWaitLock,CreateEventW(SecurityAttributes,True,False,nil));
  end;
end;

//------------------------------------------------------------------------------

procedure TBarrier.OpenLocks(DesiredAccess: DWORD; InheritHandle: Boolean);
begin
CheckAndSetHandle(fReleaseLock,
  OpenEventW(DesiredAccess or EVENT_MODIFY_STATE,InheritHandle,PWideChar(StrToWide(fName + WSO_BARR_SUFFIX_RELLOCK))));
CheckAndSetHandle(fWaitLock,
  OpenEventW(DesiredAccess or EVENT_MODIFY_STATE,InheritHandle,PWideChar(StrToWide(fName + WSO_BARR_SUFFIX_WAITLOCK))));
end;

//------------------------------------------------------------------------------

procedure TBarrier.DuplicateLocks(SourceObject: TComplexWinSyncObject);
begin
fBarrierSharedData := PWSOBarrierSharedData(fSharedData);
DuplicateAndSetHandle(fReleaseLock,TBarrier(SourceObject).fReleaseLock);
DuplicateAndSetHandle(fWaitLock,TBarrier(SourceObject).fWaitLock);
end;

//------------------------------------------------------------------------------

procedure TBarrier.DestroyLocks;
begin
CloseHandle(fWaitLock);
CloseHandle(fReleaseLock);
end;

{-------------------------------------------------------------------------------
    TBarrier - public methods
-------------------------------------------------------------------------------}

constructor TBarrier.Create(SecurityAttributes: PSecurityAttributes; const Name: String);
begin
{
  Barrier with count of 1 is seriously pointless, but if you call a constructor
  without specifying the count, what do you expect to happen?!
}
Create(SecurityAttributes,1,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create(const Name: String);
begin
Create(nil,1,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create;
begin
Create(nil,1,'');
end;


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create(SecurityAttributes: PSecurityAttributes; Count: Integer; const Name: String);
begin
inherited Create(SecurityAttributes,Name);
If Count > 0 then
  begin
    LockSharedData;
    try
      If not fBarrierSharedData^.MaxWaitCountSet then
        begin
          fBarrierSharedData^.MaxWaitCountSet := True;
          InterlockedStore(fBarrierSharedData^.MaxWaitCount,Count);
        end;
    finally
      UnlockSharedData;
    end;
  end
else raise EWSOInvalidValue.CreateFmt('TBarrier.Create: Invalid count (%d).',[Count]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create(Count: Integer; const Name: String);
begin
Create(nil,Count,Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create(Count: Integer);
begin
Create(nil,Count,'');
end;

//------------------------------------------------------------------------------

Function TBarrier.Wait: Boolean;
var
  MaxWaitCount: Int32;
  ExitWait:     Boolean;
begin
{
  If MaxWaitCount is 1 or less, then just ignore everything and immediately
  return (with result being true).
}
MaxWaitCount := InterlockedLoad(fBarrierSharedData^.MaxWaitCount);
If MaxWaitCount > 1 then
  begin
    Result := False;
    repeat
      LockSharedData;
      If fBarrierSharedData^.Releasing then
        begin
        {
          Releasing is in progress, so this thread cannot queue on the barrier.
          Unlock shared data and wait for fReleaseLock to become signaled,
          which happens at the end of releasing.
        }
          UnlockSharedData;
          case WaitForSingleObject(fReleaseLock,INFINITE) of
            WAIT_OBJECT_0:;
            WAIT_FAILED:
              raise EWSOWaitError.CreateFmt('TBarrier.Wait: Failed waiting on release lock (%d).',[GetLastError]);
          else
            raise EWSOWaitError.Create('TBarrier.Wait: Failed waiting on release lock.');
          end;
          ExitWait := False;
          // Releasing should be done by this point. Re-enter waiting.
        end
      else
        begin
          // Releasing is currently not running.
          Inc(fBarrierSharedData^.WaitCount);
          If fBarrierSharedData^.WaitCount >= MaxWaitCount then
            begin
            {
              Maximum number of waiting threads for this barrier has been
              reached.

              First prevent other threads from queueing on this barrier by
              resetting fReleaseLock and indicating the fact in shared data.
            }
              If not ResetEvent(fReleaseLock) then
                raise EWSOEventError.CreateFmt('TBarrier.Wait: Failed to reset release-lock event (%d).',[GetLastError]);
              fBarrierSharedData^.Releasing := True;
              Dec(fBarrierSharedData^.WaitCount); // remove self from waiting count
            {
              Now unlock shared data and release all waiting threads from
              fWaitLock.

              Unlocking shared data at this point is secure because any thread
              that will acquire them will encounter Releasing field to be true
              and will therefore enter waiting on fReleaseLock, which is now
              non-signaled.
            }
              UnlockSharedData;
              If not SetEvent(fWaitLock) then
                raise EWSOEventError.CreateFmt('TBarrier.Wait: Failed to set wait-lock event (%d).',[GetLastError]);
              Result := True; // indicate we have released the barrier
            end
          else
            begin
            {
              Maximum number of waiters not reached.

              Just unlock the shared data and enter waiting on fWaitLock.
            }
              UnlockSharedData;
              case WaitForSingleObject(fWaitLock,INFINITE) of
                WAIT_OBJECT_0:;
                WAIT_FAILED:
                  raise EWSOWaitError.CreateFmt('TBarrier.Wait: Failed waiting on the barrier (%d).',[GetLastError]);
              else
                raise EWSOWaitError.Create('TBarrier.Wait: Failed waiting on the barrier.');
              end;
            {
              The wait lock has been set to signaled, so the barrier is
              releasing.

              Remove self from waiting threads count and, if we are last to be
              released, stop releasing and signal end of releasing to threads
              waiting on fReleaseLock and also mark it in shared data.
            }
              LockSharedData;
              try
                Dec(fBarrierSharedData^.WaitCount);
                If fBarrierSharedData^.WaitCount <= 0 then
                  begin
                    fBarrierSharedData^.WaitCount := 0;
                    fBarrierSharedData^.Releasing := False;
                    If not ResetEvent(fWaitLock) then
                      raise EWSOEventError.CreateFmt('TBarrier.Wait: Failed to reset wait-lock event (%d).',[GetLastError]);
                    If not SetEvent(fReleaseLock) then
                      raise EWSOEventError.CreateFmt('TBarrier.Wait: Failed to set release-lock event (%d).',[GetLastError]);
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
  SetWaitLock:  Boolean;
begin
// no need to check for max wait count
SetWaitLock := False;
LockSharedData;
try
  If not fBarrierSharedData^.Releasing then
    begin
      If fBarrierSharedData^.WaitCount > 0 then
        begin
          SetWaitLock := True;
          Result := fBarrierSharedData^.WaitCount;
          If not ResetEvent(fReleaseLock) then
            raise EWSOEventError.CreateFmt('TBarrier.Release: Failed to reset release-lock event (%d).',[GetLastError]);
          fBarrierSharedData^.Releasing := True;
        end
      else Result := 0;
    end
  else Result := -1;
finally
  UnlockSharedData;
end;
{
  At this point (if SetWaitLock is true), releasing is active and no new thread
  can queue on the barrier - so it is safe to unlock shared data before setting
  the event.
}
If SetWaitLock then
  If not SetEvent(fWaitLock) then
    raise EWSOEventError.CreateFmt('TBarrier.Release: Failed to set wait-lock event (%d).',[GetLastError]);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TReadWriteLock
--------------------------------------------------------------------------------
===============================================================================}
const
  WSO_RWLOCK_SUFFIX_SHAREDDATA    = '@rwl_slk';
  WSO_RWLOCK_SUFFIX_READLOCK      = '@rwl_rlk';
  WSO_RWLOCK_SUFFIX_WRITEWAITLOCK = '@rwl_alk';
  WSO_RWLOCK_SUFFIX_WRITELOCK     = '@rwl_wlk';

{===============================================================================
    TReadWriteLock - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TReadWriteLock - protected methods
-------------------------------------------------------------------------------}

class Function TReadWriteLock.GetSharedDataLockSuffix: String;
begin
Result := WSO_RWLOCK_SUFFIX_SHAREDDATA;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.AllocateSharedData;
begin
inherited;
fRWLockSharedData := PWSORWLockSharedData(fSharedData);
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.FreeSharedData;
begin
fRWLockSharedData := nil;
inherited;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.CreateLocks(SecurityAttributes: PSecurityAttributes);
begin
If fProcessShared then
  begin
    CheckAndSetHandle(fReadLock,
      CreateEventW(SecurityAttributes,True,True,PWideChar(StrToWide(fName + WSO_RWLOCK_SUFFIX_READLOCK))));
    CheckAndSetHandle(fWriteWaitLock,
      CreateEventW(SecurityAttributes,True,True,PWideChar(StrToWide(fName + WSO_RWLOCK_SUFFIX_WRITEWAITLOCK))));
    CheckAndSetHandle(fWriteLock,
      CreateMutexW(SecurityAttributes,False,PWideChar(StrToWide(fName + WSO_RWLOCK_SUFFIX_WRITELOCK))));
  end
else
  begin
    CheckAndSetHandle(fReadLock,CreateEventW(SecurityAttributes,True,True,nil));
    CheckAndSetHandle(fWriteWaitLock,CreateEventW(SecurityAttributes,True,True,nil));
    CheckAndSetHandle(fWriteLock,CreateMutexW(SecurityAttributes,False,nil));
  end;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.OpenLocks(DesiredAccess: DWORD; InheritHandle: Boolean);
begin
CheckAndSetHandle(fReadLock,
  OpenEventW(DesiredAccess or EVENT_MODIFY_STATE,InheritHandle,PWideChar(StrToWide(fName + WSO_RWLOCK_SUFFIX_READLOCK))));
CheckAndSetHandle(fWriteWaitLock,
  OpenEventW(DesiredAccess or EVENT_MODIFY_STATE,InheritHandle,PWideChar(StrToWide(fName + WSO_RWLOCK_SUFFIX_WRITEWAITLOCK))));
CheckAndSetHandle(fWriteLock,
  OpenMutexW(DesiredAccess or MUTEX_MODIFY_STATE,InheritHandle,PWideChar(StrToWide(fName + WSO_RWLOCK_SUFFIX_WRITELOCK))));
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.DuplicateLocks(SourceObject: TComplexWinSyncObject);
begin
fRWLockSharedData := PWSORWLockSharedData(fSharedData);
DuplicateAndSetHandle(fReadLock,TReadWriteLock(SourceObject).fReadLock);
DuplicateAndSetHandle(fWriteWaitLock,TReadWriteLock(SourceObject).fWriteWaitLock);
DuplicateAndSetHandle(fWriteLock,TReadWriteLock(SourceObject).fWriteLock);
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.DestroyLocks;
begin
CloseHandle(fWriteLock);
CloseHandle(fWriteWaitLock);
CloseHandle(fReadLock);
end;

{-------------------------------------------------------------------------------
    TReadWriteLock - public methods
-------------------------------------------------------------------------------}

procedure TReadWriteLock.ReadLock;
var
  ExitWait: Boolean;
begin
ExitWait := False;
repeat
  case WaitForSingleObject(fReadLock,INFINITE) of
    WAIT_OBJECT_0:;
    WAIT_FAILED:
      raise EWSOWaitError.CreateFmt('TReadWriteLock.ReadLock: Failed waiting on read lock (%d).',[GetLastError]);
  else
    raise EWSOWaitError.Create('TReadWriteLock.ReadLock: Failed waiting on read lock.');
  end;
  LockSharedData;
  try
    If (fRWLockSharedData^.WriteWaitCount <= 0) and (fRWLockSharedData^.WriteCount <= 0) then
      begin
        Inc(fRWLockSharedData^.ReadCount);
        If not ResetEvent(fWriteWaitLock) then
          raise EWSOEventError.CreateFmt('TReadWriteLock.ReadLock: Failed to reset write-wait-lock event (%d).',[GetLastError]);
        ExitWait := True;
      end
    else ExitWait := False;
  finally
    UnlockSharedData;
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
    If not SetEvent(fWriteWaitLock) then
      raise EWSOEventError.CreateFmt('TReadWriteLock.ReadUnlock: Failed to set write-wait-lock event (%d).',[GetLastError]);
finally
  UnlockSharedData;
end;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.WriteLock;
begin
LockSharedData;
try
  Inc(fRWLockSharedData^.WriteWaitCount);
  If not ResetEvent(fReadLock) then
    raise EWSOEventError.CreateFmt('TReadWriteLock.WriteLock: Failed to reset raad-lock event (%d).',[GetLastError]);
finally
  UnlockSharedData;
end;
case WaitForSingleObject(fWriteWaitLock,INFINITE) of
  WAIT_OBJECT_0:;
  WAIT_FAILED:
    raise EWSOWaitError.CreateFmt('TReadWriteLock.WriteLock: Failed waiting on write-wait lock (%d).',[GetLastError]);
else
  raise EWSOWaitError.Create('TReadWriteLock.WriteLock: Failed waiting on write-wait lock.');
end;
case WaitForSingleObject(fWriteLock,INFINITE) of
  WAIT_OBJECT_0,
  WAIT_ABANDONED_0:;
  WAIT_FAILED:
    raise EWSOWaitError.CreateFmt('TReadWriteLock.WriteLock: Failed waiting on write lock (%d).',[GetLastError]);
else
  raise EWSOWaitError.Create('TReadWriteLock.WriteLock: Failed waiting on write lock.');
end;
LockSharedData;
try
  If not ResetEvent(fWriteWaitLock) then
    raise EWSOEventError.CreateFmt('TReadWriteLock.WriteLock: Failed to reset write-wait-lock event (%d).',[GetLastError]);
  Dec(fRWLockSharedData^.WriteWaitCount);
  Inc(fRWLockSharedData^.WriteCount);
finally
  UnlockSharedData;
end;
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.WriteUnlock;
begin
LockSharedData;
try
  Dec(fRWLockSharedData^.WriteCount);
  If not ReleaseMutex(fWriteLock) then
    raise EWSOMutexError.CreateFmt('TReadWriteLock.WriteUnlock: Failed to release write-lock mutex (%d).',[GetLastError]);
  If fRWLockSharedData^.WriteWaitCount <= 0 then
    If not SetEvent(fReadLock) then
      raise EWSOEventError.CreateFmt('TReadWriteLock.WriteUnlock: Failed to set read-lock event (%d).',[GetLastError]);
  If not SetEvent(fWriteWaitLock) then
    raise EWSOEventError.CreateFmt('TReadWriteLock.WriteUnlock: Failed to set write-wait-lock event (%d).',[GetLastError]);
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

const
  MAXIMUM_WAIT_OBJECTS = 3; {$IFNDEF CompTest}{$message 'debug'}{$ENDIF}

{===============================================================================
    Wait functions - internal functions
===============================================================================}

//Function MaxWaitObjCount(MsgWaitOptions: TMessageWaitOptions): Integer;
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
        If not(mwoEnable in MsgWaitOptions) or (Integer(WaitResult - WAIT_ABANDONED_0) < Count) then
          begin
            Result := wrAbandoned;
            If not WaitAll then
              Index := Integer(WaitResult - WAIT_ABANDONED_0);
          end
        else Result := wrError;
      WAIT_IO_COMPLETION:
        Result := wrIOCompletion;
      WAIT_TIMEOUT:
        Result := wrTimeout;
      WAIT_FAILED:
        Result := wrError;
    else
      Result := wrError;
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
    Wait functions (N > MAX) - implementation types
===============================================================================}
type
  TWSOWaitParams = record
    WaitAll:        Boolean;
    Timeout:        DWORD;
    Alertable:      Boolean;
    MsgWaitOptions: TMessageWaitOptions;
    WakeMask:       DWORD;
  end;

  TWSOWaitGroupHandles = array[0..Pred(MAXIMUM_WAIT_OBJECTS)] of THandle;

  TWSOWaitGroup = record
    Handles:    TWSOWaitGroupHandles;
    HandlesPtr: PHandle;
    Count:      Integer;        // must be strictly above zero
    IndexBase:  Integer;        // index of the first item in the original array
    // group wait result
    WaitResult: TWSOWaitResult; // init to wrFatal
    Index:      Integer;        // init to -1
  end;

  TWSOWaitLevel = record
    WaitGroups: array of TWSOWaitGroup;
    // level wait result
    WaitResult: TWSOWaitResult; // init to wrFatal
    Index:      Integer;        // init to -1
  end;

  TWSOWaitInternals = record
  {
    ReadyCounter is initally set to a number of all waits within the tree
    (including level waits). It is atomically decremented before each wait.
    When it is above zero after the decrement, enter waiting on ReadyEvent.
    When it reaches zero, waiting is not entered and, instead, the ReadyEvent
    is set (to signaled), which will release all waiting threads at once and
    they will then enter the main waiting.

    This is done to minimize time windows in which waiter threads are entering
    their waitings.
  }
    ReadyCounter:   Integer;
    ReadyEvent:     THandle;
  {
    DoneCounter is again initialized to a total number of waits.
    It is decremented each time a waiting for wait group or in a wait level
    ends. When it reaches zero, the DoneEvent is set to signaled.

    The DoneEvent is created non-signaled. When a level wait executed by the
    invoker returns, the invoker immediately enters waiting on this event.
    It is set when all waitings end their execution, then the invoker is
    released from waiting on this event and can continue its work (processing
    and returning results).
    It is meant as a memory integrity protection - the waiter threads are
    accessing memory that belongs to a variable which is local to the invoker.
    This ensures all threads will end before the invoker can invalidate this
    variable.
  }
    DoneCounter:    Integer;
    DoneEvent:      THandle;
    ReleaserEvent:  THandle;
    FirstDone:      UInt64; // higher 32bits for level index, lower for group index
  end;

  TWSOWaitArgs = record
    WaitParams:     TWSOWaitParams;  
    WaitLevels:     array of TWSOWaitLevel;
    WaitInternals:  TWSOWaitInternals;
  end;   
  PWSOWaitArgs = ^TWSOWaitArgs;

{===============================================================================
    Wait functions (N > MAX) - TWSOWaiterThread class declaration
===============================================================================}
type
  TWSOWaiterThread = class(TThread)
  protected
    fFreeMark:        Integer;
    fWaitArgs:        PWSOWaitArgs;
    procedure Execute; override;
  public
    constructor Create(WaitArgs: PWSOWaitArgs);
    Function MarkForAutoFree: Boolean; virtual;
  end;

{===============================================================================
    Wait functions (N > MAX) - TWSOWaiterThread class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOWaiterThread protected methods
-------------------------------------------------------------------------------}

procedure TWSOWaiterThread.Execute;
begin
If InterlockedExchange(fFreeMark,-1) <> 0 then
  FreeOnTerminate := True;
end;

{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOWaiterThread public methods
-------------------------------------------------------------------------------}

constructor TWSOWaiterThread.Create(WaitArgs: PWSOWaitArgs);
begin
inherited Create(False);
FreeOnTerminate := False;
InterlockedStore(fFreeMark,0);
fWaitArgs := WaitArgs;
end;

//------------------------------------------------------------------------------

Function TWSOWaiterThread.MarkForAutoFree: Boolean;
begin
Result := InterlockedExchange(fFreeMark,-1) = 0;
end;

{===============================================================================
    Wait functions (N > MAX) - TWSOGroupWaiterThread class declaration
===============================================================================}
type
  TWSOGroupWaiterThread = class(TWSOWaiterThread)
  protected
    fWaitLevelIndex:  Integer;
    fWaitGroupIndex:  Integer;
    procedure Execute; override;
  public
    constructor Create(WaitArgs: PWSOWaitArgs; WaitLevelIndex, WaitGroupIndex: Integer);
  end;

{===============================================================================
    Wait functions (N > MAX) - TWSOGroupWaiterThread class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOGroupWaiterThread protected methods
-------------------------------------------------------------------------------}

procedure TWSOGroupWaiterThread.Execute;
begin
try
  If InterlockedDecrement(fWaitArgs^.WaitInternals.ReadyCounter) > 0 then
    begin
      case WaitForSingleObject(fWaitArgs^.WaitInternals.ReadyEvent,INFINITE) of
        WAIT_OBJECT_0:;
        WAIT_FAILED:
          raise EWSOWaitError.CreateFmt('TWSOGroupWaiterThread.Execute: Failed to wait on ready event (%d).',[GetLastError]);
      else
        raise EWSOWaitError.Create('TWSOGroupWaiterThread.Execute: Failed to wait on ready event.');
      end;
    end
  else
    begin
      If not SetEvent(fWaitArgs^.WaitInternals.ReadyEvent) then
        raise EWSOEventError.CreateFmt('TWSOGroupWaiterThread.Execute: Failed to set ready event (%d).',[GetLastError]);
    end;
{
  No alertable or message waiting is allowed here, therefore do not pass those
  arguments to final wait (alertable and message waits are observed only in
  the first level wait, as it is executed by the invoking thread).
}
  with fWaitArgs^.WaitParams, fWaitArgs^.WaitLevels[fWaitLevelIndex].WaitGroups[fWaitGroupIndex] do
    WaitResult := WaitForMultipleHandles_Sys(HandlesPtr,Count,WaitAll,Timeout,Index,False,[],0);
  // if the FirstDone is still in its initial state (-1), assign indices of this group and level
  InterlockedCompareExchange(fWaitArgs^.WaitInternals.FirstDone,UInt64Get(fWaitLevelIndex,fWaitGroupIndex),UInt64(-1));
  // if waiting for one object, signal other waits that we have already got the "winner"
  If not fWaitArgs^.WaitParams.WaitAll then
    If not SetEvent(fWaitArgs^.WaitInternals.ReleaserEvent) then
      raise EWSOEventError.CreateFmt('TWSOGroupWaiterThread.Execute: Failed to set releaser event (%d).',[GetLastError]);
except
  // eat-up all exceptions, so they will not kill the thread
  fWaitArgs^.WaitLevels[fWaitLevelIndex].WaitGroups[fWaitGroupIndex].WaitResult := wrFatal;
  fWaitArgs^.WaitLevels[fWaitLevelIndex].WaitGroups[fWaitGroupIndex].Index := -1;
end;
{
  Following must be here, not sooner, to ensure the fWaitArgs is not accessed
  after the invoking thread ends its wait and frees it.
}
If InterlockedDecrement(fWaitArgs^.WaitInternals.DoneCounter) <= 0 then
  If not SetEvent(fWaitArgs^.WaitInternals.DoneEvent) then
    raise EWSOEventError.CreateFmt('TWSOGroupWaiterThread.Execute: Failed to set done event (%d).',[GetLastError]);
inherited;
end;

{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOGroupWaiterThread public methods
-------------------------------------------------------------------------------}

constructor TWSOGroupWaiterThread.Create(WaitArgs: PWSOWaitArgs; WaitLevelIndex, WaitGroupIndex: Integer);
begin
inherited Create(WaitArgs);
fWaitLevelIndex := WaitLevelIndex;
fWaitGroupIndex := WaitGroupIndex;
end;

{===============================================================================
    Wait functions (N > MAX) - TWSOLevelWaiterThread class declaration
===============================================================================}
type
  TWSOLevelWaiterThread = class(TWSOWaiterThread)
  protected
    fWaitLevelIndex:  Integer;
    procedure Execute; override;
  public
    constructor Create(WaitArgs: PWSOWaitArgs; WaitLevelIndex: Integer);
  end;

{===============================================================================
    Wait functions (N > MAX) - TWSOLevelWaiterThread class implementation
===============================================================================}

procedure WaitForManyHandles_Level(WaitArgs: PWSOWaitArgs; WaitLevelIndex: Integer); forward;

{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOLevelWaiterThread protected methods
-------------------------------------------------------------------------------}

procedure TWSOLevelWaiterThread.Execute;
begin
// exception catching is part of called function
WaitForManyHandles_Level(fWaitArgs,fWaitLevelIndex);
inherited;
end;

{-------------------------------------------------------------------------------
    Wait functions (N > MAX) - TWSOLevelWaiterThread public methods
-------------------------------------------------------------------------------}

constructor TWSOLevelWaiterThread.Create(WaitArgs: PWSOWaitArgs; WaitLevelIndex: Integer);
begin
inherited Create(WaitArgs);
fWaitLevelIndex := WaitLevelIndex;
end;

{===============================================================================
    Wait functions (N > MAX) - internal functions
===============================================================================}

procedure WaitForManyHandles_Level(WaitArgs: PWSOWaitArgs; WaitLevelIndex: Integer);
var
  IsFirstLevel:         Boolean;
  IsLastLevel:          Boolean;
  WaiterThreads:        array of TWSOWaiterThread;
  WaiterThreadsHandles: array of THandle;
  i:                    Integer;
begin
try
{
  For each wait group in this level, a waiter thread is spawned. This thread
  will then wait on a portion of handles assigned to that group and we will
  wait on those spawned threads.

  When handles will become signaled or their waiting ends in other ways (error,
  timeout, ...), the waiter threads will end their waiting and become in turn
  signaled too, at which point this function finishes and exits.

  If this is not last level, we also add another thread to be waited on. This
  thread will, instead of wait group, wait on the next level.
}
  IsFirstLevel := WaitLevelIndex <= Low(WaitArgs^.WaitLevels);
  IsLastLevel := WaitLevelIndex >= High(WaitArgs^.WaitLevels);
  // prepare array of waiter threads / wait handles
  If IsLastLevel then
    SetLength(WaiterThreads,Length(WaitArgs^.WaitLevels[WaitLevelIndex].WaitGroups))
  else
    SetLength(WaiterThreads,Length(WaitArgs^.WaitLevels[WaitLevelIndex].WaitGroups) + 1); // + wait on the next level
  SetLength(WaiterThreadsHandles,Length(WaiterThreads));
  // create and fill waiter threads
  For i := Low(WaiterThreads) to High(WaiterThreads) do
    begin
      If not IsLastLevel and (i >= High(WaiterThreads)) then
        WaiterThreads[i] := TWSOLevelWaiterThread.Create(WaitArgs,Succ(WaitLevelIndex))
      else
        WaiterThreads[i] := TWSOGroupWaiterThread.Create(WaitArgs,WaitLevelIndex,i);
    {
      Threads enter their internal waiting immediately.

      Any waiter thread can finish even before its handle is obtained, but the
      handle should still be walid and usable in waiting since no thread is
      freed automatically at this point.
    }
      WaiterThreadsHandles[i] := WaiterThreads[i].Handle;
    end;
{
  Now wait on all waiter threads with no timeout - the timeout is in effect
  for objects we are waiting on, which the waiter threads are not.
}
  If InterlockedDecrement(WaitArgs^.WaitInternals.ReadyCounter) > 0 then
    begin
      case WaitForSingleObject(WaitArgs^.WaitInternals.ReadyEvent,INFINITE) of
        WAIT_OBJECT_0:;
        WAIT_FAILED:
          raise EWSoWaitError.CreateFmt('WaitForManyHandles_All: Failed to wait on ready event (%d).',[GetLastError]);
      else
        raise EWSoWaitError.Create('WaitForManyHandles_All: Failed to wait on ready event.');
      end;
    end
  else
    begin
      If not SetEvent(WaitArgs^.WaitInternals.ReadyEvent) then
        raise EWSOEventError.CreateFmt('WaitForManyHandles_Level: Failed to set ready event (%d).',[GetLastError]);
    end;
  // the waiting itself...
  If IsFirstLevel then
    // first level, use Alertable and Message wait settings
    WaitArgs^.WaitLevels[WaitLevelIndex].WaitResult := WaitForMultipleHandles_Sys(
      Addr(WaiterThreadsHandles[Low(WaiterThreadsHandles)]),
      Length(WaiterThreadsHandles),
      WaitArgs^.WaitParams.WaitAll,
      INFINITE,
      WaitArgs^.WaitLevels[WaitLevelIndex].Index,
      WaitArgs^.WaitParams.Alertable,
      WaitArgs^.WaitParams.MsgWaitOptions,
      WaitArgs^.WaitParams.WakeMask)
  else
    // second+ level, ignore Alertable and Message wait settings
    WaitArgs^.WaitLevels[WaitLevelIndex].WaitResult := WaitForMultipleHandles_Sys(
      Addr(WaiterThreadsHandles[Low(WaiterThreadsHandles)]),
      Length(WaiterThreadsHandles),
      WaitArgs^.WaitParams.WaitAll,
      INFINITE,
      WaitArgs^.WaitLevels[WaitLevelIndex].Index,
      False,
      [],
      0);
  // process (possibly invalid) result
  case WaitArgs^.WaitLevels[WaitLevelIndex].WaitResult of
    wrSignaled:;    // all is good, continue
    wrAbandoned:    raise EWSoMultiWaitError.Create('WaitForManyHandles_Level: Invalid wait result (abandoned).');
    wrIOCompletion: If not IsFirstLevel then
                      raise EWSoMultiWaitError.Create('WaitForManyHandles_Level: Alertablee waiting not allowed here.');
    wrMessage:      If not IsFirstLevel then
                      raise EWSoMultiWaitError.Create('WaitForManyHandles_Level: Message waiting not allowed here.');
    wrTimeout:      raise EWSoMultiWaitError.Create('WaitForManyHandles_Level: Wait timeout not allowed here.');
    wrError:        raise EWSoMultiWaitError.CreateFmt('WaitForManyHandles_Level: Wait error (%d).',[WaitArgs^.WaitLevels[WaitLevelIndex].Index]);
    wrFatal:        raise EWSoMultiWaitError.Create('WaitForManyHandles_Level: Fatal error during waiting.');
  else
   raise EWSoMultiWaitError.CreateFmt('WaitForManyHandles_Level: Invalid wait result (%d).',[Ord(WaitArgs^.WaitLevels[WaitLevelIndex].WaitResult)]);
  end;
  If WaitArgs^.WaitParams.WaitAll then
    begin
    {
      If the waiting ended with wrSignaled, then all waiter threads have ended
      by this time. But, if waiting ended with other result, then some waiter
      threads might be still waiting.
      We do not need them anymore and forcefull termination is not a good idea,
      so we mark them for automatic freeing at their termination and leave them
      to their fate.
      If thread already finished its waiting (in which case its method
      MarkForAutoFree returns false), then it is freed immediately as it cannot
      free itself anymore.

      Note that the invoking waiting will not end until all the threads finish,
      this is to protect shared memory.
    }
      For i := Low(WaiterThreads) to High(WaiterThreads) do
        If not WaiterThreads[i].MarkForAutoFree then
          begin
            WaiterThreads[i].WaitFor;
            WaiterThreads[i].Free;
          end;
    end
  else
    begin
    {
      The releaser is set in waiter threads, but in case the waiting for those
      threads itself ended with non-signaled result (error, message, ...), we
      have to set it here too to release all the waitings.

      After that, wait for all waiter threads to finish and free them.
    }
      If not SetEvent(WaitArgs^.WaitInternals.ReleaserEvent) then
        raise EWSOEventError.CreateFmt('WaitForManyHandles_Level: Failed to set releaser event (%d).',[GetLastError]);
      For i := Low(WaiterThreads) to High(WaiterThreads) do
        begin
          WaiterThreads[i].WaitFor;
          WaiterThreads[i].Free;
        end;
    end;
except
{
  Something really bad happened...

  Set ready event to signaled and signal fatal error.

  The ready counter might not have been decremented if this exception occured
  before that operation took place. If that is the case, then the ready counter
  cannot reach zero and consequently, the ready event will never be set. This
  means all other wait threads will be waiting on ready counter indefinitely.

  To prevent this deadlock, we set the ready event now - the waiting is
  erroneous anyway, so there is no point in timing protection.
}
  If not SetEvent(WaitArgs^.WaitInternals.ReadyEvent) then
    raise EWSOEventError.CreateFmt('WaitForManyHandles_Level: Failed to set ready event (%d).',[GetLastError]);
  WaitArgs^.WaitLevels[WaitLevelIndex].WaitResult := wrFatal;
  WaitArgs^.WaitLevels[WaitLevelIndex].Index := -1;
end;
{
  Following must be the last operation because it protects acces to shared
  variable memory.
}
If InterlockedDecrement(WaitArgs^.WaitInternals.DoneCounter) <= 0 then
  If not SetEvent(WaitArgs^.WaitInternals.DoneEvent) then
    raise EWSOEventError.CreateFmt('WaitForManyHandles_Level: Failed to set done event (%d).',[GetLastError]);
end;

//------------------------------------------------------------------------------

procedure WaitForManyHandles_Preprocess(var WaitArgs: TWSOWaitArgs; Handles: PHandle; Count: Integer);

  Function WaitGroupHandlesHigh(const HandlesArray: TWSOWaitGroupHandles): Integer;
  begin
    If WaitArgs.WaitParams.WaitAll then
      Result := High(HandlesArray)
    else
      Result := High(HandlesArray) - 1;
  end;

var
  GroupCount:   Integer;
  Cntr:         Integer;
  i,j,k:        Integer;
  LevelGroups:  Integer;
  TempHPtr:     PHandle;
begin
// wait parameters in WaitArgs are expected to be filled
{
  Calculate how many groups will be there.

  Each group can contain MAXIMUM_WAIT_OBJECTS of waited objects if WaitAll is
  true. But if it is false (waiting for one object), each group must also
  contain an releaser event (put at the end of the array).
}
If WaitArgs.WaitParams.WaitAll then
  GroupCount := Ceil(Count / MAXIMUM_WAIT_OBJECTS)
else
  GroupCount := Ceil(Count / Pred(MAXIMUM_WAIT_OBJECTS));
{
  Given the group count, calculate number of levels.

  Each level can wait on MAXIMUM_WAIT_OBJECTS of groups (waiter threads).
  But if there is another level after the current one, the current must also
  wait on that next level.
  If message waiting is enabled, the first level (executed in the context of
  invoker thread) must also wait for messages, which further decreases the
  limit.

    G ... number of groups
    L ... numger of levels
    M ... maximum number of waited objects per level

    Message waiting

      Total number of waited objects in levels will be number of groups plus
      all waits for next levels (in all levels except the last one, so L - 1),
      plus 1 for messages wait in the first level.

                L = (G + (L - 1) + 1) / M
                L = (G + L) / M
               ML = G + L
           ML - L = G
         L(M - 1) = G
                L = G / (M - 1)   <<<

    Non-message waiting

      Total number of waited objects in levels is number of groups plus all
      waits for next levels (L - 1).

                L = (G + (L - 1)) / M
               ML = G + L - 1
           ML - L = G - 1
       L(M   - 1) = G - 1
                L = (G - 1) / (M - 1)   <<<
}
If mwoEnable in WaitArgs.WaitParams.MsgWaitOptions then
  SetLength(WaitArgs.WaitLevels,Ceil(GroupCount / Pred(MAXIMUM_WAIT_OBJECTS)))
else
  SetLength(WaitArgs.WaitLevels,Ceil(Pred(GroupCount) / Pred(MAXIMUM_WAIT_OBJECTS)));
// prepare groups
Cntr := GroupCount;
For i := Low(WaitArgs.WaitLevels) to High(WaitArgs.WaitLevels) do
  begin
    LevelGroups := Min(MAXIMUM_WAIT_OBJECTS,Cntr);
    If i < High(WaitArgs.WaitLevels) then
      Dec(LevelGroups); // wait for next level
    If (mwoEnable in WaitArgs.WaitParams.MsgWaitOptions) and (i <= Low(WaitArgs.WaitLevels)) then
      Dec(LevelGroups); // message wait
    SetLength(WaitArgs.WaitLevels[i].WaitGroups,LevelGroups);
    Dec(Cntr,LevelGroups);
    // init level wait result
    WaitArgs.WaitLevels[i].WaitResult := wrFatal;
    WaitArgs.WaitLevels[i].Index := -1;
  end;
// prepare groups and fill their handle arrays
TempHPtr := Handles;
Cntr := 0;
For i := Low(WaitArgs.WaitLevels) to High(WaitArgs.WaitLevels) do
  For j := Low(WaitArgs.WaitLevels[i].WaitGroups) to High(WaitArgs.WaitLevels[i].WaitGroups) do
    begin
      WaitArgs.WaitLevels[i].WaitGroups[j].HandlesPtr := Addr(WaitArgs.WaitLevels[i].WaitGroups[j].Handles);
      WaitArgs.WaitLevels[i].WaitGroups[j].Count := 0;
      WaitArgs.WaitLevels[i].WaitGroups[j].IndexBase := Cntr;
      WaitArgs.WaitLevels[i].WaitGroups[j].WaitResult := wrFatal;
      WaitArgs.WaitLevels[i].WaitGroups[j].Index := -1;
      For k := Low(WaitArgs.WaitLevels[i].WaitGroups[j].Handles) to
               WaitGroupHandlesHigh(WaitArgs.WaitLevels[i].WaitGroups[j].Handles) do
        begin
          WaitArgs.WaitLevels[i].WaitGroups[j].Handles[k] := TempHPtr^;
          Inc(WaitArgs.WaitLevels[i].WaitGroups[j].Count);
          Inc(Cntr);
          Inc(TempHPtr); // should advance the pointer by a size of THandle
          If Cntr >= Count then
            Break{For j};
        end;
    end;
// initialize internals
WaitArgs.WaitInternals.ReadyCounter := Length(WaitArgs.WaitLevels);
WaitArgs.WaitInternals.DoneCounter := Length(WaitArgs.WaitLevels);
For i := Low(WaitArgs.WaitLevels) to High(WaitArgs.WaitLevels) do
  begin
    Inc(WaitArgs.WaitInternals.ReadyCounter,Length(WaitArgs.WaitLevels[i].WaitGroups));
    Inc(WaitArgs.WaitInternals.DoneCounter,Length(WaitArgs.WaitLevels[i].WaitGroups));
  end;
WaitArgs.WaitInternals.ReadyEvent := CreateEventW(nil,True,False,nil);
WaitArgs.WaitInternals.DoneEvent := CreateEventW(nil,True,False,nil);
If not WaitArgs.WaitParams.WaitAll then
  begin
    // releaser is used only when not waiting for all objects to be signaled
    WaitArgs.WaitInternals.ReleaserEvent := CreateEventW(nil,True,False,nil);
    // add releaser handle to all groups
    For i := Low(WaitArgs.WaitLevels) to High(WaitArgs.WaitLevels) do
      For j := Low(WaitArgs.WaitLevels[i].WaitGroups) to High(WaitArgs.WaitLevels[i].WaitGroups) do
        begin
          WaitArgs.WaitLevels[i].WaitGroups[j].Handles[WaitArgs.WaitLevels[i].WaitGroups[j].Count] :=
            WaitArgs.WaitInternals.ReleaserEvent;
          Inc(WaitArgs.WaitLevels[i].WaitGroups[j].Count);
        end;
  end;
WaitArgs.WaitInternals.FirstDone := UInt64(-1);
end;

//------------------------------------------------------------------------------

Function WaitForManyHandles_Postprocess(var WaitArgs: TWSOWaitArgs; out Index: Integer): TWSOWaitResult;
var
  i,j:            Integer;
  WaitGroupTemp:  TWSOWaitGroup;
begin
// clear internals
CloseHandle(WaitArgs.WaitInternals.ReadyEvent);
CloseHandle(WaitArgs.WaitInternals.DoneEvent);
If not WaitArgs.WaitParams.WaitAll then
  CloseHandle(WaitArgs.WaitInternals.ReleaserEvent);
// check for fatals in levels, then groups
For i := Low(WaitArgs.WaitLevels) to High(WaitArgs.WaitLevels) do
  If WaitArgs.WaitLevels[i].WaitResult = wrFatal then
    raise EWSOMultiWaitError.CreateFmt('WaitForManyHandles_Postprocess: Fatal error in wait level %d.',[i]);
For i := Low(WaitArgs.WaitLevels) to High(WaitArgs.WaitLevels) do
  For j := Low(WaitArgs.WaitLevels[i].WaitGroups) to High(WaitArgs.WaitLevels[i].WaitGroups) do
    If WaitArgs.WaitLevels[i].WaitGroups[j].WaitResult = wrFatal then
      raise EWSOMultiWaitError.CreateFmt('WaitForManyHandles_Postprocess: Fatal error in wait group %d-%d.',[i,j]);
// check for invalid results in all groups (message, IO completion)
For i := Low(WaitArgs.WaitLevels) to High(WaitArgs.WaitLevels) do
  For j := Low(WaitArgs.WaitLevels[i].WaitGroups) to High(WaitArgs.WaitLevels[i].WaitGroups) do
    If WaitArgs.WaitLevels[i].WaitGroups[j].WaitResult in [wrIOCompletion,wrMessage] then
      raise EWSOMultiWaitError.CreateFmt('WaitForManyHandles_Postprocess: Invalid result in wait group %d-%d (%d).',
                                         [i,j,Ord(WaitArgs.WaitLevels[i].WaitGroups[j].WaitResult)]);
// check for errors in groups
For i := Low(WaitArgs.WaitLevels) to High(WaitArgs.WaitLevels) do
  For j := Low(WaitArgs.WaitLevels[i].WaitGroups) to High(WaitArgs.WaitLevels[i].WaitGroups) do
    If WaitArgs.WaitLevels[i].WaitGroups[j].WaitResult = wrError then
      begin
        Result := WaitArgs.WaitLevels[i].WaitGroups[j].WaitResult;
        Index := WaitArgs.WaitLevels[i].WaitGroups[j].Index;
        Exit;
      end;
// check for message or apc in the first level
If WaitArgs.WaitLevels[Low(WaitArgs.WaitLevels)].WaitResult in [wrIOCompletion,wrMessage] then
  begin
    Result := WaitArgs.WaitLevels[Low(WaitArgs.WaitLevels)].WaitResult;
    Index := -1;
    Exit;
  end;
// take the first-done and resolve index and result (timeout/signaled/abandoned/error)
If WaitArgs.WaitInternals.FirstDone <> UInt64(-1) then
  begin
    WaitGroupTemp := WaitArgs.WaitLevels[Integer(UInt64Rec(WaitArgs.WaitInternals.FirstDone).Hi)].
                       WaitGroups[Integer(UInt64Rec(WaitArgs.WaitInternals.FirstDone).Lo)];
    Result := WaitGroupTemp.WaitResult;
    If WaitArgs.WaitParams.WaitAll or not(Result in [wrSignaled,wrAbandoned]) then
      Index := -1
    else
      Index := WaitGroupTemp.IndexBase + WaitGroupTemp.Index;
  end
else raise EWSOMultiWaitError.Create('WaitForManyHandles_Postprocess: Waiting failed.');
end;

//------------------------------------------------------------------------------

Function WaitForManyHandles_Internal(Handles: PHandle; Count: Integer; WaitAll: Boolean; Timeout: DWORD; out Index: Integer; Alertable: Boolean; MsgWaitOptions: TMessageWaitOptions; WakeMask: DWORD): TWSOWaitResult;
var
  WaitArgs: TWSOWaitArgs;
begin
Index := -1;
If Count > 0 then
  begin
    If Count > RectifiedMaxWaitCount(MsgWaitOptions) then
      begin
      {
        More than maximum waited objects (64/63).

        Split handles to levels and groups.  
        If message waiting is enabled, the first level (index 0) will wait for
        them, other levels and groups can just ignore this setting.
      }
        FillChar(Addr(WaitArgs)^,SizeOf(TWSOWaitArgs),0);
        // assign wait parameters
        WaitArgs.WaitParams.WaitAll := WaitAll;
        WaitArgs.WaitParams.Timeout := Timeout;
        WaitArgs.WaitParams.Alertable := Alertable;
        WaitArgs.WaitParams.MsgWaitOptions := MsgWaitOptions;
        WaitArgs.WaitParams.WakeMask := WakeMask;
        // do other preprocessing
        WaitForManyHandles_Preprocess(WaitArgs,Handles,Count);
        // do the waiting
        WaitForManyHandles_Level(@WaitArgs,Low(WaitArgs.WaitLevels));
        If not WaitAll then
        {
          Waiting for at least one object became signaled.

          As the waiting is split into multiple threads, the first wait thread
          returning caused the wait call to return too, but other threads might
          be still waiting.
          So we set the releaser to signaled, which will release all threads
          that are still waiting (they will be automatically freed).
        }
          If not SetEvent(WaitArgs.WaitInternals.ReleaserEvent) then
            raise EWSOEventError.CreateFmt('WaitForManyHandles_Internal: Failed to set releaser event (%d).',[GetLastError]);
      {
        Wait for all waits to return.
        Necessary for memory integrity - otherwise still running threads might
        access non-exiting memory, namely WaitArgs variable.
      }
        case WaitForSingleObject(WaitArgs.WaitInternals.DoneEvent,INFINITE) of
          WAIT_OBJECT_0:;
          WAIT_FAILED:
            raise EWSOWaitError.CreateFmt('WaitForManyHandles_Internal: Failed to wait for done event (%d).',[GetLastError]);
        else
          raise EWSOWaitError.Create('WaitForManyHandles_Internal: Failed to wait for done event.');
        end;
        // process result(s)
        Result := WaitForManyHandles_Postprocess(WaitArgs,Index);
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
SetLength(TestArray,2);
If (PtrUInt(Addr(TestArray[Succ(Low(TestArray))])) - PtrUInt(Addr(TestArray[Low(TestArray)]))) <> SizeOf(THandle) then
  raise EWSOException.Create('HandleArrayItemsStrideCheck: Unsupported implementation detail (array items alignment).');
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Initialize;
begin
HandleArrayItemsStrideCheck([THandle(0),THandle(1)]);
end;

//------------------------------------------------------------------------------

initialization
  Initialize;

end.

