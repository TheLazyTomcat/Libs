{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  FastLocks

    Simple non-blocking synchronization objects based on interlocked functions
    operating on locking counters.

    WARNING >>>

      This library was written for a specific scenario, where there was tens
      of thousands of separate data structures, each of which could have been
      accessed by several threads, and where parallel access was rare but very
      possible and dangerous. When a simultaneous access occured, it was almost
      always reading.

      Creating RW lock for each of the structure was unfeasible, so this
      library was written to provide some light-weight locking mechanism with
      minimal memory and OS resources footprint. Implementation is therefore
      maximally simple, which causes many limitations.

    <<< WARNING

    Non-blocking behaviour means that any attempt to acquire lock will return
    immediatelly, and resulting value of this attempt indicates whether the
    lock was really acquired or not.

    At this point, four synchronization primitives/objects are implenented -
    event, semaphore, mutex and read-write (RW) lock. More might be added
    later, but currently it is unlikely.
    For details about how any of the object works and what are its limitations,
    refer to its declaration.

    In its basic form, each in-here implemented synchronizer is just an integer
    residing in the memory. Within this library, this integer is called sync
    word.
    It is used to store the locking flags and counters and interlocked
    functions are used to atomically probe and change stored values and to
    decide state of the object and required action.

      WARNING - all implemented synchronizers are operating on the same sync
                word type (TFLSyncWord), but they are not mutually compatible.
                So always use one sync word for only one type of synchronizer,
                never mix them on one variable.

      WARNING - do not directly read or write sync word variables, use only
                functions provided for individual synchronizers.

    All synchronizers can be used either directly, where you declare or
    allocate a variable of type TFLSyncWord and then operate on it using
    procedural interface (eg. FastMutexAcquire, FastRWLockBeginRead, ...),
    or indirectly, creating an instance of provided descendants of TFastLock
    class and using its methods (see description of TFastLock for more details
    and options).

    If the sync word variable is located in a shared memory, then all provided
    synchronizers can be used for inter-process synchronization.

    Here is a small example how a non-blocking synchronization can be used:

                <unsynchronized_code>
           -->  If FastMutex.Acquire then
           |      try
           |        <synchronized_code>
           |      finally
           |        FastMutex.Release;
           |      end
           |    else
           |      begin           
           |        <code_not_needing_sync>
           |        synchronization not possible, do other things that
           |        do not need to be synchronized
           |      end;
           --   repeat from start and try synchronization again if needed
                <unsynchronized_code>

    If you want to use wating, do the following:

                <unsynchronized_code>
           -->  If FastMutex.WaitToAcquire(500) = wrAcquired then
           |      try
           |        <synchronized_code>
           |      finally
           |        FastMutex.Release;
           |      end
           |    else
           |      begin
           |        <code_not_needing_sync>
           |      end;
           --   <repeat_if_needed>
                <unsynchronized_code>

    Some more important notes on the implementation and use:

      - none of the provided synchronizers is robust (when a thread holding
        a lock ends without releasing it, it will stay locked indefinitely)

      - none of the provided synchronizers is recursive (when attempting to
        acquire a lock second time in the same thread, it will always fail)

      - there is very limited deadlock prevention - be extremely carefull when
        trying to acquire synchronizer in more than one place in a single thread
        (trying to acquire synchronizer second time in the same thread will
        always fail, with exception being rw-lock reading, which is given by
        concept of multiple readers access)

      - use provided waiting and spinning only when necessary - synchronizers
        are intended to be primarily used as non-blocking

      - waiting is always active (spinning) - do not wait for prolonged time
        intervals as it might starve other threads, use infinite waiting only
        in extreme cases and only when really necessary

      - use synchronization by provided objects only on very short (in time,
        not code) routines - do not use to synchronize code that is executing
        longer than few milliseconds

      - every successful acquire of a synchronizer MUST be paired by a release
        (events migh pose exception here, see their description), synhronizers
        are not automalically released

  Version 2.0.1 (2026-06-09)

  Last change 2026-06-09

  ©2016-2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.FastLocks

  Dependencies:
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions  - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol FastLocks_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    ListUtils   - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit FastLocks;
{
  FastLocks_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and only this unit will be compiled in PurePascal mode.
}
{$IFDEF FastLocks_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{
  FastLocks_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  FastLocks_UseAuxExceptions to achieve this.
}
{$IF Defined(FastLocks_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}
//------------------------------------------------------------------------------

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
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
  {$MODESWITCH ClassicProcVars+}
  {$DEFINE CanInline}
  {$INLINE ON}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
{$ELSE}
  {$IF CompilerVersion >= 17} // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------
{
  SyncWord64

  When this symbol is defined, the type used for sync word (TFLSyncWord), and
  therefore the sync word itself, is 64 bits wide, otherwise it is 32 bits wide.
  This is true on all systems, irrespective of whether they are 32bit or 64bit.

    NOTE - 64bit sync words require that library InterlockedOps provides
           support for 64bit arguments, see there for details.

  By default NOT defined.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol FastLocks_SyncWord64_On.
}
{$UNDEF SyncWord64}
{$IFDEF FastLocks_SyncWord64_On}
  {$DEFINE SyncWord64}
{$ENDIF}

interface

uses
  SysUtils,
  AuxTypes, AuxClasses{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EFLException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EFLClockError   = class(EFLException);
  EFLInvalidState = class(EFLException);
  EFLInvalidValue = class(EFLException);

{===============================================================================
--------------------------------------------------------------------------------
                                   Fast locks
--------------------------------------------------------------------------------
===============================================================================}
type
  TFLSyncWord = {$IFDEF SyncWord64}Int64{$ELSE}Int32{$ENDIF};
  PFLSyncWord = ^TFLSyncWord;
  
//------------------------------------------------------------------------------
{
  TFLSpinParams

  Structure used to pass parameters that control spinning.

  Spinning functions accepting this structure instead of only spin count are
  provided to allow for finer control over the spinning, as more parameters can
  be varied. Functions accepting only the spin count are using default values
  for other variables (see constant DefaultSpinParams).

  Few words on how spinning, as implemented here, works:

    When entering spinning, the thread first tries to directly acquire lock,
    even if there are other threads already waiting. If this is successful,
    the called function will return immediately, returning wrAcquired. If this
    cannot be done, then the thread is queued to spin on the object and the
    spinning starts. Note that the enqueue can also fail...

      Each synchronizer tracks how many threads are waiting on it. This, among
      others, means number of waiters is limited (because the counter that
      tracks them must fit into the sync word). Currently, all provided objects
      allow for at most 1023 threads to be waiting at a time. Since no thread
      can realistically enter multiple waits, this number should suffice for
      projected use cases.
      If this number is reached (unlikely, but possible to happen), all future
      threads trying to enqueue will fail and the spinning function will return
      wrTryAgain. In that situation you should do some other work and retry the
      spinning later - hopefully some waiters will be served and the counter
      lowered below its maximum.
      Note that wrTryAgain can also be returned in other situations specific
      to some synchronizer types - see their description for details.

    During spinning, a cycle is performed. In each iteration of this cycle, an
    attempt to acquire the object is tried. If it is not successful, a delaying
    action is executed and then the cycle repeats.

      WARNING - this means that, while spinning, the thread continues to load
                the processor and memory, be aware of that.

    Maximum number of these iterations is limited by value of SpinCount (as
    explicit parameter or field within SpinParams), unless it is set to
    INFINITE, in which case the cycle never terminates.

    The delaying action is a small piece of code with no external effects that
    is executed multiple times to make this delaying longer. Number of these
    executions is controlled by field DelayCount.

    If the object can be acquired during spinning, it is done so and the
    spinning call exits, returning wrAcquired. If it cannot be done the entire
    time spinning runs (and spin count is not infinite), them the spinning
    call exits when all cycles are consumed and returns wrTimeout.

    If more than one thread is spinning to acquire some object, then there
    is no guarantee the first thread that entered this cycle will also acquire
    the object first. The order in which spinning threads are granted locks is
    undefined and pretty much random.

    While any thread is spinning to acquire object, no other thread can acquire
    it by calling corresponding asynchronous (non-blocking) function. This is
    to assure that blocked threads are served before threads which are using
    the object asynchronously.
}
  TFLSpinParams = record
    SpinCount:  UInt32;
    DelayCount: UInt32;
  end;
  PFLSpinParams = ^TFLSpinParams;

{
  TFLWaitDelayMethod

  Used to select a method used for delaying action in waiting (see description
  of type TFLWaitParams for more info).

    dmNone          - No delaying action is performed. Use this only in
                      situations where you know the synchronizer will not
                      stay locked for long.

    dmSpin          - Executes delaying action as described in spinning. See
                      description of types TFLSpinParams and TFLWaitParams for
                      more details about spinning.

                        This is the default operation.

    dmYield         - An attempt to yield execution of current thread is made.
                      If system has another thread that can be run, the current
                      thread is suspended, rescheduled and next thread is run.
                      If there is no thread awaiting execution, then the
                      current thread is not suspended, continues execution and
                      pretty much performs spinning.

                        WARNING - use with caution, as it can cause spinning
                                  with rapid calls to thread yielding on
                                  uncontested CPU.

    dmSleep         - The current thread suspends its own execution (using a
                      call to function Sleep) for number of milliseconds given
                      in field SleepTime of WaitParams.
                      Note that time the thread is suspended is usually
                      slightly longer than requested because of granularity
                      of scheduling timers.

    dmSleepEx       - Behaves the same as dmSleep, but the thread can be
                      awakened by APC or I/O completion calls.

                        NOTE - useful only on Windows, everywhere else it
                               behaves the same as dmSleep.

    dmYieldSleep    - Combination od dmYield and dmSleep - when the thread
                      is not yielded (eg. because no other thread is awaiting
                      execution), a sleep is performed.

                        NOTE - useful only on Windows, everywhere else it
                               behaves the same as dmSleep.                      

    dmYieldSleepEx  - Works the same as dmYieldSleep, but the sleep allows
                      for thread wakeup by APC or I/O completion calls.

                        NOTE - useful only on Windows, everywhere else it
                               behaves the same as dmYieldSleep.
}
  TFLWaitDelayMethod = (dmNone,dmSpin,dmYield,dmSleep,dmSleepEx,dmYieldSleep,
                        dmYieldSleepEx);

{
  TFLWaitParams

  Used to pass parameters controlling waiting.

  Waiting is very similar to spinning (see description of TFLSpinParams for
  more information) in that it runs in a cycle, but the number of iteration
  is not given explicitly, it depends on a timeout interval (in milliseconds).
  Timeout can be set to INFINITE, making the waiting to never run out of time.

  In each iteration, and attempt to acquire is made, and, when not successful,
  a delaying action is performed. Nature of this action can be selected by
  field DelayMethod.

    One possible delaying action is spinning. In this case, a delaying action
    that is normally used in spinning is performed and value for DelayCount is
    taken from variant field of the same name here.

    If any delaying action that is performing sleep is selected, you can define
    number of milliseconds to sleep in variant field SleepTime.

  This type of blocking is here to allow for better control over the time spent,
  because how much actual time is spent in spinning greatly depends on system
  performance (eg. CPU clock, instruction troughput and latency, optimizations
  of used instructions, exact behaviour of PAUSE instruction, and so on...).

    NOTE - waiting is, similarly to spinning, active, meaning the thread will
           still run and load the processor and not enter any kind of suspended
           state (unless in some specific delay methods, see description of
           TFLWaitDelayMethod for details).
}
  TFLWaitParams = record
    Timeout:          UInt32;
    case DelayMethod: TFLWaitDelayMethod of
      dmSpin: (
        DelayCount:     UInt32);
      dmSleep,
      dmSleepEx,
      dmYieldSleep,
      dmYieldSleepEx: (
        SleepTime:      UInt32);
  end;
  PFLWaitParams = ^TFLWaitParams;

const
  // infinite spin count or timeout interval
  INFINITE = UInt32(-1);

  DefaultSpinParams: TFLSpinParams = (
    SpinCount:  INFINITE;
    DelayCount: 5000);

  DefaultWaitParams: TFLWaitParams = (
    Timeout:      INFINITE;
    DelayMethod:  dmSpin;
    DelayCount:   5000);

//------------------------------------------------------------------------------
{
  TFLWaitResult

  Used to indicate result of blocking (spinning or waiting) functions.

    wrAcquired - The synchronizer object was signaled (unlocked). Current state
                 of the synchronizer object depends on its type and settings.

    wrTimeout  - Spinning or waiting timed-out, ie. the synchronizer did
                 not became signaled in a given timeout period or number
                 of spinning cycles (was non-signaled the whole time).

    wrTryAgain - Returned when the synchronizer is in a state that temporarily
                 precludes spinning or waiting (for example when event is
                 pulsing or there is too many threads already waiting).
                 You should try to aquire the synchronizer again later.

    wrDeadlock - Informs the caller that the object is in a state which might
                 lead to an infinite blocking and it cannot be automatically
                 resolved. That being said, there is always solution, but which
                 requires the user to perform some actions.
                 Currently, this can only be returned by rw-lock when promoting
                 read lock to a write lock - see its description for details,
                 including solutions.

    wrError    - Unknown or external error has ocurred, the object might be in
                 an inconsistent state and should not be used anymore.
                 In current implementation, this is never returned as all
                 erroneous states lead to an exception being raised.
}
type
  TFLWaitResult = (wrAcquired,wrTimeout,wrTryAgain,wrDeadlock,wrError);

{
  WaitResultToStr

  Resturns textual representation of provided wait result.

  It is inteded mainly for debugging purposes.
}
Function WaitResultToStr(WaitResult: TFLWaitResult): String;

{===============================================================================
--------------------------------------------------------------------------------
                                    TFastLock
--------------------------------------------------------------------------------
===============================================================================}
type
  TFastLockMode = (flmOwner,flmSlave,flmWrapper);

{===============================================================================
    TFastLock - class declaration
===============================================================================}
{
  TFastLock

  TFastLock is a common ancestor for all classes implemented by this library
  that are encapsulating procedural interfaces of provided synchronization
  primitives into object form.

  These objects can be created in three different modes - Owner, Slave and
  Wrapper.

    Owner object uses its own internal field to provide sync word and therefore
    does not need it to be allocated or declared externally. You simply create
    it using no-parameter constructor and that is all

      NOTE - some synchronizers may provide constructors accepting parameters
             that specify properties of that primitive. Simply put, owner mode
             object is created when you use constructor that does NOT expect
             sync word variable or other (master) instance of TFastLock or its
             descendant.  

    Slave object does not have its own sync word, instead it uses sync word
    provided by master object passed to constructor. This mechanism is here to
    allow for effective sharing of one lock between multiple instances of fast
    lock objects - you create owner object in one (possibly main) thread and
    to synchronize in other threads you just provide them with slave objects
    created using the owner object as their master. Also note that the master
    object does not need to be created in owner mode - it can be another slave
    or even wrapper instance (yep, you can create a tree of slaves, but better
    avoid that).
    Slave objects inherit spin and wait parameters from master object (they
    copy them during construction, changes made to master object's settings
    after that point are not propagated to its slaves).
    
      WARNING - to ensure that master objects are not destroyed while being
                used by their slaves, all objects are reference counted.
                Everytime any instance is used as master, its reference count
                is incremented and, when the slave object is destroyed, it gets
                decremented.
                If you call destructor of object that is currently being used
                as master, it will not be freed within that call, only its
                reference count will be decremented. When last slave using it
                is being destroyed, this master will be destroyed too.

    Wrapper object also does not have its own sync word, but instead of using
    master object it accepts reference to any sync word wariable and uses that
    one. Lifetime of this variable must be managed by external means. You can
    use single variable in any number of wrapper instances, they will all be
    mutually synchronized.
    The variable can be initialized and finalized externally, but if you set
    constructor parameter InitSyncWord to True, the object will automatically
    initialize and also finalize it - be carefull and make sure you do not
    re-initialize already used sync word.
}
type
  TFastLock = class(TCustomObject)
  protected
    fMode:            TFastLockMode;
    fRefCount:        Integer;
    fMaster:          TFastLock;
    fSyncWord:        TFLSyncWord;
    fSyncWordPtr:     PFLSyncWord;
    fIsInitializer:   Boolean;
    fSpinParams:      TFLSpinParams;
    fWaitParams:      TFLWaitParams;
    fCanFreeInstance: Boolean;
    Function GetReferenceCount: Integer; virtual;
    Function GetSpinParamsPtr: PFLSpinParams; virtual;
    Function GetWaitParamsPtr: PFLWaitParams; virtual;
    Function AcquireReference: Integer; virtual;
    Function ReleaseReference: Integer; virtual;
    procedure SyncWordInit(const InitArgs: array of const); virtual; abstract;
    procedure SyncWordFinal; virtual; abstract;
    procedure Initialize(SyncWordPtr: PFLSyncWord; InitSyncWord: Boolean; const InitArgs: array of const); virtual;
    procedure Finalize; virtual;
    class Function ArgTypePresent(const InitArgs: array of const; Index: Integer; VType: Byte): Boolean; virtual;
  public
    procedure FreeInstance; override;
  {
    Dummy parameters are there because of newer Delphi - otherwise they issue
    warning, and I quote:

      "Duplicate constructor ... with identical parameters will be inacessible from C++"

    ...like anyone cares about C++ here. >;/
  }
    constructor CreateBase({$IFNDEF FPC}Dummy: TFLSyncWord = 0{$ENDIF}); // static constructor
    constructor CreateOwner({$IFNDEF FPC}DummyA: TFLSyncWord = 0; DummyB: TFLSyncWord = 0{$ENDIF}); virtual;
    constructor CreateSlave(Master: TFastLock{$IFNDEF FPC}; Dummy: TFLSyncWord = 0{$ENDIF}); virtual;
    constructor CreateWrapper(var SyncWord: TFLSyncWord; InitSyncWord: Boolean = True{$IFNDEF FPC}; Dummy: TFLSyncWord = 0{$ENDIF}); virtual;
    constructor Create; overload; virtual;
    constructor Create(Master: TFastLock); overload; virtual;
    destructor Destroy; override;
    property Mode: TFastLockMode read fMode;    
    property ReferenceCount: Integer read GetReferenceCount;
    property IsInitializer: Boolean read fIsInitializer;
    property SpinParams: TFLSpinParams read fSpinParams write fSpinParams;
    property SpinParamsPtr: PFLSpinParams read GetSpinParamsPtr;
    property WaitParams: TFLWaitParams read fWaitParams write fWaitParams;
    property WaitParamsPtr: PFLWaitParams read GetWaitParamsPtr;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   Fast event                                   
--------------------------------------------------------------------------------
===============================================================================}
{
  Synchronizer that roughly corresponds to events provided by Windows OS (see
  their documentation for details on how and where to use events).

  It is an object whose state can be explicitly manipulated (set to signaled
  or reset to non-signaled) from any thread and which can be used eg. to inform
  other threads that some event has occurred/passed.

  Note that current implementation actually presents three possible states
  for the event - signaled, non-signaled and pulsing (non-signaled state that
  allows currently blocked threads to pass).

  For more information, refer to description of individual functions.
}
{===============================================================================
    Fast event - procedural interface declaration
===============================================================================}
{
  FastEventInit

  Initializes the event synchronizer and sets its state according to passed
  settings.

  If InitialState is set to True, then the state of initialized event will
  be signaled, otherwise (False) it will be non-signaled.

  For details about manual-reset versus auto-reset, please refer to description
  of FastEvent*Pass functions.

  If already initialized word is passed here, it will be re-initialized and
  its current state lost.
}
procedure FastEventInit(out SyncWord: TFLSyncWord; ManualReset: Boolean = False; InitialState: Boolean = False);

{
  FastEventFinal

  Finalizes the event object and sets sync word to a value that precludes its
  further use (an invalid value). Can accept uninitialized sync words.

  If any thread is spinning or waiting on this word, the spin or wait function
  will raise an EFLInvalidState exception next time it probes the event (next
  cycle).
}
procedure FastEventFinal(var SyncWord: TFLSyncWord);

{
  FastEventSet

  Sets the event to signaled state.

  If the event is currently pulsing, the pulsing is abandoned. If it was already
  signaled, then nothing changes.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has invalid value in general.
}
procedure FastEventSet(var SyncWord: TFLSyncWord);

{
  FastEventReset

  Resets the event to non-signaled state.

  If the event is currently pulsing, the pulsing is abandoned. If it was in
  a non-signaled state, then nothing changes.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has invalid value.
}
procedure FastEventReset(var SyncWord: TFLSyncWord);

{
  FastEventPulse

  If no thread is spinning or waiting on this event, then it is reset to a
  non-signaled state (equivalent to calling FastEventReset). If any thread
  is spinning or waiting, then the event is set to pulsing state.

    In pulsing state, threads that were already spinning or waiting (and only
    those threads) are allowed to pass the event. If it is an auto-reset event,
    then first thread that passes resets it to a non-signaled state, disabling
    pulsing. For manual-reset event, only when last of the spinning or waiting
    threads passes the event is reset to non-signaled state.

    During pulsing, no new thread can pass it or begin spinning or waiting
    (wrTryLater will be returned).

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has invalid value.
}
procedure FastEventPulse(var SyncWord: TFLSyncWord);

{
  FastEventPass

  Probes the provided event whether it can be passed or not. Passing in this
  context means that the probing thread is granted a signaled state.

    For auto-reset events, the call passes (true is returned) when the event
    is in a signaled state (note that pulsing event is NOT signaled), and no
    other thread is spinning or waiting on it. Also, if passed, then the event
    is reset to non-signaled state, otherwise its state is left unchanged.

    Manual-reset event can be passed whenever it is in a signaled state and its
    state is not implicitly changed (it has to be changed manually).

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has invalid value.
}
Function FastEventPass(var SyncWord: TFLSyncWord): Boolean;

{
  FastEventSpinToPass

  Tries to pass the event and, if not successful, enters spinning. The spinning
  ends when the event can be passed (becomes signaled or pulsing) or prescribed
  number of spinning cycles (SpinCount) is performed.

  If the event becomes signaled or pulsing during waiting, or if it is passed
  without even starting spinning, then wrAcquired is returned.

  If the event was in pulsing state before the call, then wrTryAgain is
  returned - pulsing state precludes new threads to begin spinning. You should
  try to enter spinning again after some time.

  See description of type TFLSpinParams for more details regarding spinning.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has invalid value.
}
Function FastEventSpinToPass(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult; overload;
Function FastEventSpinToPass(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult; overload;

{
  FastEventWaitToPass

  Tries to pass the event and, if not successful, enters waiting. Waiting ends
  when the event becomes signaled or pulsing (wrAcquired is returned), or the
  prescribed number of milliseconds (Timeout) elapses (wrTimeout is returned).

  Can return wrTryAgain if the event was pulsing before the call as pulsing
  precludes new threads to enter waiting. In this case try the call later.

  See description of types TFLWaitDelayMethod and TFLWaitParams for more
  details regarding waiting.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has invalid value.
}
Function FastEventWaitToPass(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult; overload;
Function FastEventWaitToPass(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                   TFastEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastEvent - class declaration
===============================================================================}
type
  TFastEvent = class(TFastLock)
  protected
    procedure SyncWordInit(const InitArgs: array of const); override;
    procedure SyncWordFinal; override;
  public
    constructor Create(ManualReset: Boolean; InitialState: Boolean); overload; virtual;
    // following overload WILL initialize the provided sync word
    constructor Create(var SyncWord: TFLSyncWord; ManualReset: Boolean; InitialState: Boolean); overload; virtual;
  {
    Unfortunatelly, "set" is a reserved word in pascal, therefore it cannot
    be used as method name. I chose to provide several methods whose names
    at least start with "set", plus methods Signal and Unlock doing the same,
    so you can select what you like the most.
  }
    procedure Set_; virtual;
    procedure SetEvent; virtual;
    procedure SetSignaled; virtual;
    procedure Signal; virtual;
    procedure Unlock; virtual;
    procedure Reset; virtual;
    procedure Pulse; virtual;
    Function Pass: Boolean; virtual;
    Function SpinToPass(SpinCount: UInt32): TFLWaitResult; overload; virtual;
    Function SpinToPass: TFLWaitResult; overload; virtual;
    Function WaitToPass(Timeout: UInt32): TFLWaitResult; overload; virtual;
    Function WaitToPass: TFLWaitResult; overload; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 Fast semaphore
--------------------------------------------------------------------------------
===============================================================================}
{
  This is more-or-less classical semaphore, that is a synchronizer whose state
  consists purely of a counter. If the counter is above zero, the object is
  signaled, if zero then it is non-signaled (locked).

  In current implementation, the counter is limited - its maximum value can be
  2097151 for 32bit sync words and 9007199254740991 for 64bit sync words (see
  symbol SyncWord64 above).
}
{===============================================================================
    Fast semaphore - procedural interface declaration
===============================================================================}
{
  FastSemaphoreInit

  Initializes the semaphore and sets its count to a passed value.

  If InitialCount is set to a negative or too large value (above implementation
  limit - see above), then an EFLInvalidValue exception is raised.

  If already initialized sync word is used, it will be re-initialized and its
  current state lost.
}
procedure FastSemaphoreInit(out SyncWord: TFLSyncWord; InitialCount: TFLSyncWord = 0);

{
  FastSemaphoreFinal

  Finalizes the semaphore object and sets sync word to a value that precludes
  its further use (an invalid value). Can accept uninitialized sync words.

  If any thread is spinning or waiting on this word, the spin or wait function
  will raise an EFLInvalidState exception in next cycle.
}
procedure FastSemaphoreFinal(var SyncWord: TFLSyncWord);

{
  FastSemaphoreCount

  Returns current value of semaphore's counter.

  Note that the returned value might not reflect reality by the time the
  function returns and therefore should not be relied upon. This is because
  between time the value is obtained and time when this function returns,
  another thread can change it.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastSemaphoreCount(var SyncWord: TFLSyncWord): TFLSyncWord;

{
  FastSemaphoreAcquire

  Tries to decrement the semaphore counter by given amount (AcquireCount).
  If it succeeds then true is returned, false otherwise (in that case the
  counter is NOT decremented).

  It can only succeed if curent counter is larger than or equal to acquire
  count and also there is no other thread waiting or spinning to acquire this
  semaphore.

  AcquireCount must be larger than zero and smaller than maximum value for
  internal counter, otherwise an EFLInvalidValue exception is raised.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastSemaphoreAcquire(var SyncWord: TFLSyncWord; AcquireCount: TFLSyncWord = 1): Boolean;

{
  FastSemaphoreRelease

  Tries to increment semaphore counter by given amount (ReleaseCount) and
  returns true when it succeeds, false when not (then the counter is not
  changed).

  Can succeed only if current counter plus the release count produces number
  smaller or at most equal to counter maximum.

  ReleaseCount must be larger than zero and smaller than maximum value for
  internal counter, otherwise an EFLInvalidValue exception is raised.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastSemaphoreRelease(var SyncWord: TFLSyncWord; ReleaseCount: TFLSyncWord = 1): Boolean;

{
  FastSemaphoreSpinToAcquire

  Tries to acquire the semaphore (see FastSemaphoreAcquire for details). If not
  successful, the it will enter spinning cycle.

  See description of type TFLSpinParams for more details regarding spinning.

  Raises an EFLInvalidValue exception if AcquireCount is smaller than zero or
  larger than maximum value for internal counter.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastSemaphoreSpinToAcquire(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams; AcquireCount: TFLSyncWord = 1): TFLWaitResult; overload;
Function FastSemaphoreSpinToAcquire(var SyncWord: TFLSyncWord; SpinCount: UInt32; AcquireCount: TFLSyncWord = 1): TFLWaitResult; overload;

{
  FastSemaphoreWaitToAcquire

  Tries to acquire the semaphore and, if not successful, enters waiting.

  See description of types TFLWaitDelayMethod and TFLWaitParams for more
  details regarding waiting.

  Raises an EFLInvalidValue exception if AcquireCount is smaller than zero or
  larger than maximum value for internal counter.  

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastSemaphoreWaitToAcquire(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams; AcquireCount: TFLSyncWord = 1): TFLWaitResult; overload;
Function FastSemaphoreWaitToAcquire(var SyncWord: TFLSyncWord; Timeout: UInt32; AcquireCount: TFLSyncWord = 1): TFLWaitResult; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                 TFastSemaphore
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastSemaphore - class declaration
===============================================================================}
type
  TFastSemaphore = class(TFastLock)
  protected
    procedure SyncWordInit(const InitArgs: array of const); override;
    procedure SyncWordFinal; override;
  public
    constructor Create(InitialCount: TFLSyncWord); overload; virtual;
    // following overload will initialize the provided sync word
    constructor Create(var SyncWord: TFLSyncWord; InitialCount: TFLSyncWord); overload; virtual;
    Function Count: TFLSyncWord; virtual;
    Function Acquire(AcquireCount: TFLSyncWord = 1): Boolean; virtual;
    Function Release(ReleaseCount: TFLSyncWord = 1): Boolean; virtual;
    Function SpinToAcquireBy(SpinCount: UInt32; AcquireCount: TFLSyncWord): TFLWaitResult; overload; virtual;
    Function SpinToAcquireBy(AcquireCount: TFLSyncWord): TFLWaitResult; overload; virtual;
    Function SpinToAcquire(SpinCount: UInt32): TFLWaitResult; overload; virtual; // acquire count is 1
    Function SpinToAcquire: TFLWaitResult; overload; virtual;
    Function WaitToAcquireBy(Timeout: UInt32; AcquireCount: TFLSyncWord): TFLWaitResult; overload; virtual;
    Function WaitToAcquireBy(AcquireCount: TFLSyncWord): TFLWaitResult; overload; virtual;
    Function WaitToAcquire(Timeout: UInt32): TFLWaitResult; overload; virtual;
    Function WaitToAcquire: TFLWaitResult; overload; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   Fast mutex
--------------------------------------------------------------------------------
===============================================================================}
{
  This is only the most basic synchronizer for mutual exclusion.

  Unlike most of the serious implementations, it does not contain any kind of
  thread ownership mechanism, it is either signaled (unlocked) or non-signaled
  (locked), nothing more - it does not care who locked it.
}
{===============================================================================
    Fast mutex - procedural interface declaration
===============================================================================}
{
  FastMutexInit

  Initializes the mutex synchronizer and sets its state according to passed
  settings.

  If InitialState is set to True, then the state of initialized mutex will
  be signaled, otherwise (False, default value) it will be non-signaled.

  If already initialized word is passed here, it will be re-initialized and
  its current state lost.
}
procedure FastMutexInit(out SyncWord: TFLSyncWord; InitialState: Boolean = False);

{
  FastMutexFinal

  Finalizes the mutex object and sets sync word to a value that precludes its
  further use (an invalid value). Can accept uninitialized sync words.

  If any thread is spinning or waiting on this word, the spin or wait function
  will raise an EFLInvalidState exception in next cycle.
}
procedure FastMutexFinal(var SyncWord: TFLSyncWord);

{
  FastMutexAcquire

  Tries to acquire the mutex. If succesfull, then true is returned and mutex
  is set to a non-signaled state, otherwise false is returned and state of the
  mutex is left unchanged.

  Mutex can only be succesfully acquired if in signaled (unlocked) state and
  there is not other thread spinning or waiting on it.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastMutexAcquire(var SyncWord: TFLSyncWord): Boolean;

{
  FastMutexRelease

  Sets the mustex to a signaled (unlocked) state. This functions always
  succeeds.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.  
}
procedure FastMutexRelease(var SyncWord: TFLSyncWord);

{
  FastMutexSpinToAcquire

  Tries to acquire the mutex and, if not successful, enters spinning cycle.

  See description of type TFLSpinParams for more details regarding spinning.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastMutexSpinToAcquire(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult; overload;
Function FastMutexSpinToAcquire(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult; overload;

{
  FastMutexWaitToAcquire

  Tries to acquire the mutex. If not successful then it enters waiting.

  See description of types TFLWaitDelayMethod and TFLWaitParams for more
  details regarding waiting.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastMutexWaitToAcquire(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult; overload;
Function FastMutexWaitToAcquire(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                   TFastMutex
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastMutex - class declaration
===============================================================================}
type
  TFastMutex = class(TFastLock)
  protected
    procedure SyncWordInit(const InitArgs: array of const); override;
    procedure SyncWordFinal; override;
  public
    constructor Create(InitialState: Boolean); overload; virtual;
    // following overload will initialize the provided sync word
    constructor Create(var SyncWord: TFLSyncWord; InitialState: Boolean); overload; virtual;
    Function Acquire: Boolean; virtual;
    procedure Release; virtual;
    Function SpinToAcquire(SpinCount: UInt32): TFLWaitResult; overload; virtual;
    Function SpinToAcquire: TFLWaitResult; overload; virtual;
    Function WaitToAcquire(Timeout: UInt32): TFLWaitResult; overload; virtual;
    Function WaitToAcquire: TFLWaitResult; overload; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Fast read-write lock
--------------------------------------------------------------------------------
===============================================================================}
{
  This synchronizer can be locked in two ways - for reading or for writing.
  It is means for situations where the protected resources are often read
  but only seldomly written.

  Read lock is shared, meaning it can be acquired by multiple threads at the
  same time. It is possible, but higly discouraged, for a single thread to
  acquire multiple read locks. Because it allows multiple concurrent accesses
  to protected resources, it must only be used for operations that do no change
  the resources (reading).
  Number of read locks is limited to 65535 in current implementation.

  Write lock, on the other hand, is exclusive - it can be acquired only by
  single thread - and is meant for situations when the protected data needs
  to be changed.
}
{===============================================================================
    Fast read-write lock - procedural interface declaration
===============================================================================}
{
  FastRWLockInit

  Initializes the rw-lock synchronizer.

  If already initialized word is passed here, it will be re-initialized and
  its current state lost.
}
procedure FastRWLockInit(out SyncWord: TFLSyncWord);

{
  FastRWLockFinal

  Finalizes the rw-lock object and sets sync word to a value that precludes
  its further use (an invalid value). Can accept uninitialized sync words.

  If any thread is spinning or waiting on this word, the spin or wait function
  will raise an EFLInvalidState exception in next cycle.
}
procedure FastRWLockFinal(var SyncWord: TFLSyncWord);

{
  FastRWLockBeginRead

  Tries to acquire read lock. Returns true if successful, false when not.

  Read lock can only be obtained when the synchronizer is not write-locked,
  there is no thread spinning or waiting for write lock (including pending
  lock promotion) and current number of readers is bellow allowed maximum
  (see abowe).

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockBeginRead(var SyncWord: TFLSyncWord): Boolean;

{
  FastRWLockEndRead

  Removes single read lock, ie. decrements read counter by one.

  If there is no reader then it does nothing.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.  
}
procedure FastRWLockEndRead(var SyncWord: TFLSyncWord);

{
  FastRWLockSpinToRead

  Tries to acquire read lock. If it cannot be acquired then it enters spinning.

  Note that spinning for read does NOT preclude other threads from acquiring
  read lock, write lock or lock promotion, even using asynchronous calls.

  See descripntion of FastRWLockBeginRead for conditions needed to obtain
  read lock.

  See description of type TFLSpinParams for more details regarding spinning.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockSpinToRead(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult; overload;
Function FastRWLockSpinToRead(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult; overload;

{
  FastRWLockWaitToRead

  Tries to acquire read lock and, when unsucessful, enters wating.

  Similarly to spinning, waiting for read lock will not preclude other threads
  from acquiring read lock, write lock or lock promotion.

  See descripntion of FastRWLockBeginRead for conditions needed to obtain
  read lock.  

  See description of types TFLWaitDelayMethod and TFLWaitParams for more
  details regarding waiting.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockWaitToRead(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult; overload;
Function FastRWLockWaitToRead(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult; overload;

{
  FastRWLockBeginWrite

  Attempts to acquire write lock. If it succeeds then true is returned, false
  otherwise.

  Write lock can only be obtained if the object is not already write-locked,
  there is no read lock and no other thread is spinning or waiting for write
  lock (that includes wait for lock promotion).

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockBeginWrite(var SyncWord: TFLSyncWord): Boolean;

{
  FastRWLockEndWrite

  Removes write lock from the object. This function always succeeds.

  If the object is currently not write-locked, then it does nothing.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.  
}
procedure FastRWLockEndWrite(var SyncWord: TFLSyncWord);

{
  FastRWLockSpinToWrite

  Tries to acquire write lock and, when not successful, enters spinning.

  Here write lock can only be obtained if not write-locked, there is no reader
  and no other thread is spinning or waiting for lock promotion.

  See description of type TFLSpinParams for more details regarding spinning.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockSpinToWrite(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult; overload;
Function FastRWLockSpinToWrite(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult; overload;

{
  FastRWLockWaitToWrite

  Tries to acquire write lock. If it cannot be obtained then it enters waiting.

  Write lock can be granted only if not write-locked, there is no reader and no
  other thread is spinning or waiting for lock promotion.

  See description of types TFLWaitDelayMethod and TFLWaitParams for more
  details regarding waiting.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockWaitToWrite(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult; overload;
Function FastRWLockWaitToWrite(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult; overload;

{
  FastRWLockPromote

  Attempts to promote (escalate) read lock into write lock. Returns wrAcquired
  when successful.

  Promotion using this call can only succeed if the object is not write-locked,
  there is no thread spinning or waiting for write lock (includes waiting for
  promotion) and exactly one read lock is active.

    WARNING - thread calling this function must already have exactly one
              (no more, no less) read lock. Calling it without previously
              acquiring read lock results in undefined behavior, and will
              almost certainly lead to corruption of the synchronizer and
              other problems.
              If called while holding more than one read lock, then it will
              always fail to promote the lock.

  When the function succeeds, then the rw-lock will be write-locked and number
  of read locks will be decremented by one (becomes zero). If it fails, then
  the pre-existing read lock stays in effect - that being said, take EXTREME
  care what the function returns (only possible values, other than wrAcquired,
  are wrTryAgain and wrDeadlock)...

    When wrTryAgain is returned, it means the promotion cannot be done because
    either there is invalid number of readers (not equal to one), the object is
    write-locked or there are other threads spinning or waiting for write lock.
    In this situation, it is enough to just wait for a moment and try the lock
    promotion again.

    --- WARNING ---

    If wrDeadlock is returned, it means other thread is currently spinning or
    waiting for its own lock promotion - this constitues a deadlock situation.
    The waiting thread needs that there are no read locks other than its own
    one, but we are also holding one, therefore the waiter can never be granted
    the promotion, and neither can we while someone is waiting.

      There is only one solution to this, we have to drop our read lock, which
      will (eventuallly) lead to condition that allows the waiting thread to
      be granted its promotion. As we drop our read lock, we can immediately
      spin or wait to acquire it again - this will not block the waiter because
      we will not acquire new read lock until the waiter gets its promotion,
      does its work, and releases or demotes write lock.

      This all can be done manually, but you can use function FastRWLockRecover
      that will do it for you - see its description for more information.
      Mentioned recover function is not automatically called because it blocks,
      and this function is supposed to be non-blocking.

    Other wait result values are not returned - if they are for some bizzare
    reason, you should consider it a fatal error and the object to be in an
    unusable state.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockPromote(var SyncWord: TFLSyncWord): TFLWaitResult;

{
  FastRWLockDemote

  If the object is write-locked then this function removes this write lock,
  adds one read lock and returns true. Otherwise it returns false.

  So, if it returns true, you can be certain that the calling thread now have
  acquired a read lock - remember to release it after use (by calling function
  FastRWLockEndRead). When false is returned, the thread does NOT get read lock
  (this can happen only if there is no write lock).

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockDemote(var SyncWord: TFLSyncWord): Boolean;

{
  FastRWLockRecover

  Tries to recover the synchronizer object from deadlock (see description of
  FastRWLock[SpinTo/WaitTo]Promote for details about when this happens).

    WARNING - call this function only immediately after any promoting function
              returns wrDeadlock, never in other situations, otherwise you migh
              damage the object's state.

  This function will remove one read lock and then tries to re-acquire it using
  infinite spinning.

    NOTE - abovementioned means that this function will block, possibly for a
           long time if the synchronizer is used improperly.

  When it returns true, then the calling thread is granted a read lock (release
  it after use), but it does not neccessarily mean there is no more a deadlock,
  merely that the one caused by current thread was resolved.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockRecover(var SyncWord: TFLSyncWord): Boolean;

{
  FastRWLockSpinToPromote

  Tries to promote read lock into a write lock and, when not immediately
  successful, enters spinning.

  To successcully promote here, there can be no write lock, exactly one read
  lock and no other thread can be spinning or waiting for lock promotion.

  Returns wrDeadlock if some other thread is currently spinning or waiting
  for read lock promotion, meaning only one thread can be spinning or waiting
  at a time. This limitation might seem a little strict, but let me explain...

    Let's imagine we allow more than one thread to spin or wait for read lock
    promotion - two threads enter this blocking (almost) simultaneously, the
    first thread is granted promotion and the second is still waiting. If the
    second thread exits its wait while the first is still holding write lock
    (eg. becauso of timeout), we now have situation with one thread having
    write lock and the other that just exitted holding read lock at the same
    time (when it entered wait, it had read lock, so it expects still having
    it). Not good.

    I can think of several solutions to this problem from top of my head:

        - do not allow promotion (nope, we are doing this to allow it)
        - make promotions completely asynchronous, ie. do not allow spinning
          or waiting (rejected, I want to allow spinning or waiting)
        - strip read lock from the thread as soon as it enters the spin or wait
          (unfeasible, mainly because any thread can hold multiple read locks,
          and I really do not want to have per-thread state that is holding
          the count)
        - make spinning and waiting for promotion always infinite, ie. remove
          spin count and timeout (rejected, this is supposed to be asynchronous
          library, infinite waiting somewhat destroys that notion)
        - limit waits to one thread, but this introduces possible deadlock

     The last one was selected because it does not break caller's expectations
     and style of this library, and the dadlock can be easily identified and
     rectified.

  See FastRWLockPromote for details about promotion and mainly about what to
  do when wrDeadlock is returned.

  See description of type TFLSpinParams for more details regarding spinning.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockSpinToPromote(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult; overload;
Function FastRWLockSpinToPromote(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult; overload;

{
  FastRWLockWaitToPromote

  Tries to promote read lock into a write lock. If it cannot be promoted
  immediatelly, then it enters waiting.

  To promote, the object must not be write-locked, there must be exactly one
  read lock (the one being promoted) and no other thread can be spinning or
  waiting for lock promotion.

  Returns wrDeadlock if some other thread is currently spinning or waiting
  for read lock promotion, therefore only one thread can be spinning or waiting
  for lock promotion at a time.

  See FastRWLockPromote for details about promotion and what to do when
  wrDeadlock is returned.

  See description of types TFLWaitDelayMethod and TFLWaitParams for more
  details regarding waiting.

  Raises an EFLInvalidState exception if the sync word is not initialized or
  has an invalid value.
}
Function FastRWLockWaitToPromote(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult; overload;
Function FastRWLockWaitToPromote(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                   TFastRWLock
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastRWLock - class declaration
===============================================================================}
type
  TFastRWLock = class(TFastLock)
  protected
    procedure SyncWordInit(const InitArgs: array of const); override;
    procedure SyncWordFinal; override;
  public
  {
    This overload has to be here and not in the base class - if there, it would
    be in conflict with constructors in some descendants (eg. in TFastMutex).
  }
    constructor Create(var SyncWord: TFLSyncWord); overload; virtual;
    Function BeginRead: Boolean; virtual;
    procedure EndRead; virtual;
    Function SpinToRead(SpinCount: UInt32): TFLWaitResult; overload; virtual;
    Function SpinToRead: TFLWaitResult; overload; virtual;
    Function WaitToRead(Timeout: UInt32): TFLWaitResult; overload; virtual;
    Function WaitToRead: TFLWaitResult; overload; virtual;
    Function BeginWrite: Boolean; virtual;
    procedure EndWrite; virtual;
    Function SpinToWrite(SpinCount: UInt32): TFLWaitResult; overload; virtual;
    Function SpinToWrite: TFLWaitResult; overload; virtual;
    Function WaitToWrite(Timeout: UInt32): TFLWaitResult; overload; virtual;
    Function WaitToWrite: TFLWaitResult; overload; virtual;
    Function Promote: TFLWaitResult; virtual;
    Function Demote: Boolean; virtual;
    Function Recover: Boolean; virtual;
    Function SpinToPromote(SpinCount: UInt32): TFLWaitResult; overload; virtual;
    Function SpinToPromote: TFLWaitResult; overload; virtual;
    Function WaitToPromote(Timeout: UInt32): TFLWaitResult; overload; virtual;
    Function WaitToPromote: TFLWaitResult; overload; virtual;
  end;

implementation

uses
{$IFDEF Windows} Windows,{$ELSE} baseunix, linux,{$ENDIF}
  InterlockedOps;

{===============================================================================
--------------------------------------------------------------------------------
                                   Fast locks
--------------------------------------------------------------------------------
===============================================================================}
const
  FL_INITVAL = TFLSyncWord(0);

  FL_WORDHIBIT = {$IFDEF SyncWord64}63{$ELSE}31{$ENDIF};

  FL_COMMON_MASK_VALID = TFLSyncWord(1) shl FL_WORDHIBIT;

//------------------------------------------------------------------------------

Function ConsumeArg(Arg: TFLSyncWord): TFLSyncWord;
begin
Result := Arg + 0;
end;

//------------------------------------------------------------------------------

Function ConsumeArgs(const Args: array of const): TFLSyncWord;
begin
Result := TFLSyncWord(High(Args));
end;

//------------------------------------------------------------------------------

Function IsValid(SyncWord: TFLSyncWord): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := (SyncWord and FL_COMMON_MASK_VALID) <> 0;
end;

//------------------------------------------------------------------------------

Function WaitResultToStr(WaitResult: TFLWaitResult): String;
const
  WR_STRS: array[TFLWaitResult] of String = ('Acquired','Timeout','TryAgain','Deadlock','Error');
begin
If (WaitResult >= Low(TFLWaitResult)) and (WaitResult <= High(TFLWaitResult)) then
  Result := WR_STRS[WaitResult]
else
  Result := '<invalid>';
end;

{===============================================================================
    Fast locks - spinning and waiting infrastructure
===============================================================================}
type
  TFLQueueResult = (qrAcquired,qrQueued,qrFailed,qrDeadlock);

type
  TFLWaitParamsInternal = record
    PublicParams:     record case Boolean of
      False: (SpinParams: TFLSpinParams);
      True:  (WaitParams: TFLWaitParams)
    end;
    SyncWordPtr:      PFLSyncWord;
    EnqueueFce:       Function(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): TFLQueueResult;
    QueuedAcquireFce: Function(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): Boolean;
    DequeueFce:       procedure(var SyncWord: TFLSyncWord; CallData: TFLSyncWord);
    CallData:         TFLSyncWord;  // passed as CallData param to above funtions
  end;

//==============================================================================
{
  Just do some contained, relatively long, but othervise pointless operation
  that has no side effects.
}
Function SpinDelayAction(Divisor: UInt32): UInt32;{$IFNDEF PurePascal} register; assembler;
asm
{
  Assembly implementation is here only to utilize PAUSE instruction. It is
  otherwise equivalent to pascal code.
}
{$IFDEF x64}
  {$IFDEF Windows}
    // Divisor is already in ECX
  {$ELSE}
    MOV     ECX, EDI
  {$ENDIF}
{$ELSE}
    MOV     ECX, EAX
{$ENDIF}
    MOV     EAX, 3895731025
    XOR     EDX, EDX

    DIV     ECX

    PAUSE   // instruction specifically intended for spin loops
end;
{$ELSE}
begin
Result := UInt32(3895731025) div Divisor;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure SpinDelay(Count: UInt32);
var
  i:  UInt32;
begin
{
  Repeatedly call delaying action - iterator must not start at 0 because it is
  used as divisor in SpinDelayAction.
}
For i := 1 to Count do
  SpinDelayAction(i);
end;

//------------------------------------------------------------------------------
threadvar
  THRVAR_ClockFrequency: Int64; // automatically intialized to zero

Function QueryClockFrequency: Int64;
{$IFNDEF Windows}
var
  Time: TTimeSpec;
begin
Result := 1000000000{ns^-1, 1GHz};
If clock_getres(CLOCK_MONOTONIC_RAW,@Time) <> 0 then
  raise EFLClockError.CreateFmt('GetClockFrequencyl: Unable to obtain clock frequency (code: ).',[errno]);
{$ELSE}
begin
Result := 0;
If not QueryPerformanceFrequency(Result) then
  raise EFLClockError.CreateFmt('GetClockFrequencyl: Unable to obtain clock frequency (code: ).',[GetLastError]);
{$ENDIF}
If Result and Int64($1000000000000000) <> 0 then
  raise EFLClockError.CreateFmt('GetClockFrequency: Unsupported frequency value (0x%.16x)',[Result]);
THRVAR_ClockFrequency := Result;
end;

//------------------------------------------------------------------------------

Function GetClockFrequency: Int64;
begin
Result := THRVAR_ClockFrequency;
If Result <= 0 then
  Result := QueryClockFrequency;
end;

//------------------------------------------------------------------------------

Function GetClockValue: Int64;
{$IFNDEF Windows}
var
  Time: TTimeSpec;
begin
If clock_gettime(CLOCK_MONOTONIC_RAW,@Time) = 0 then
  Result := Int64(Time.tv_sec) * 1000000000 + Int64(Time.tv_nsec)
else
  raise EFLClockError.CreateFmt('GetClockValue: Unable to obtain clock value (code: %d).',[errno]);
{$ELSE}
begin
Result := 0;
If not QueryPerformanceCounter(Result) then
  raise EFLClockError.CreateFmt('GetClockValue: Unable to obtain clock value (code: %d).',[GetLastError]);
{$ENDIF}
// mask out bit 63 to prevent problems with signed 64bit integer
Result := Result and Int64($7FFFFFFFFFFFFFFF);
end;

//------------------------------------------------------------------------------

Function GetElapsedMillis(FromClock,Frequency: Int64): UInt32;
var
  CurrentClock: Int64;
begin
CurrentClock := GetClockValue;
If CurrentClock < FromClock then
  // clock seems to have overflown
  Result := UInt32(((High(Int64) - FromClock + CurrentClock + 1{overflow tick}) * 1000) div Frequency)
else
  Result := UInt32(((CurrentClock - FromClock) * 1000) div Frequency);
end;

//------------------------------------------------------------------------------

{$IFDEF Windows}
Function SwitchToThread: BOOL; stdcall; external kernel32;
{$ELSE}
{
  FPC declares sched_yield as procedure without result, which afaik does not
  correspond to linux man.
}
Function sched_yield: cint; cdecl; external;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function YieldThread: Boolean;{$IFDEF CanInline} inline;{$ENDIF}
begin
{$IFDEF Windows}
Result := SwitchToThread;
{$ELSE}
Result := sched_yield = 0;
{$ENDIF}
end;

{===============================================================================
    Fast locks - spinning and waiting implementation
===============================================================================}

Function ExecuteSpinning(WaitParamsInternal: TFLWaitParamsInternal): TFLWaitResult;

  Function SpinInternal: TFLWaitResult;
  begin
    // SpinInternal can only return wrAcquired or wrTimeout, nothing else
    while not WaitParamsInternal.QueuedAcquireFce(WaitParamsInternal.SyncWordPtr^,WaitParamsInternal.CallData) do
      begin
      {
        Could not acquire the object - do spinning, check count and exit or
        repeat, depending on counters.
      }
        SpinDelay(WaitParamsInternal.PublicParams.SpinParams.DelayCount);
        If WaitParamsInternal.PublicParams.SpinParams.SpinCount <> INFINITE then
          begin
            Dec(WaitParamsInternal.PublicParams.SpinParams.SpinCount);
            If WaitParamsInternal.PublicParams.SpinParams.SpinCount <= 0 then
              begin
                // we must explicitly dequeue
                WaitParamsInternal.DequeueFce(WaitParamsInternal.SyncWordPtr^,WaitParamsInternal.CallData);
                Result := wrTimeout;
                Exit;
              end;
          end;
      end;
    // if here, acquire was successful
    Result := wrAcquired;
  end;

begin
case WaitParamsInternal.EnqueueFce(WaitParamsInternal.SyncWordPtr^,WaitParamsInternal.CallData) of
  qrAcquired: Result := wrAcquired;
  qrQueued:   If WaitParamsInternal.PublicParams.SpinParams.SpinCount > 0 then
                Result := SpinInternal
              else
                Result := wrTimeout;
  qrDeadlock: Result := wrDeadlock;
else
 {qrFailed}   Result := wrTryAgain;
end;
end;

//------------------------------------------------------------------------------

Function ExecuteWaiting(WaitParamsInternal: TFLWaitParamsInternal): TFLWaitResult;

  Function WaitInternal: TFLWaitResult;
  var
    ClockFreq:  Int64;
    ClockStart: Int64;
  begin
    ClockFreq := GetClockFrequency;
    ClockStart := GetClockValue;
    while not WaitParamsInternal.QueuedAcquireFce(WaitParamsInternal.SyncWordPtr^,WaitParamsInternal.CallData) do
      If (WaitParamsInternal.PublicParams.WaitParams.Timeout = INFINITE) or
         (GetElapsedMillis(ClockStart,ClockFreq) < WaitParamsInternal.PublicParams.WaitParams.Timeout) then
        // infinite wait or timeout has not elapsed yet
        case WaitParamsInternal.PublicParams.WaitParams.DelayMethod of
          dmNone:;        // do nothing
          dmYield:        YieldThread;
        {$IFDEF Windows}
          dmSleep:        Sleep(WaitParamsInternal.PublicParams.WaitParams.SleepTime);
          dmSleepEx:      SleepEx(WaitParamsInternal.PublicParams.WaitParams.SleepTime,True);
          dmYieldSleep:   If not YieldThread then
                            Sleep(WaitParamsInternal.PublicParams.WaitParams.SleepTime);
          dmYieldSleepEx: If not YieldThread then
                            SleepEx(WaitParamsInternal.PublicParams.WaitParams.SleepTime,True);
        {$ELSE}
          dmSleep,
          dmSleepEx,
          dmYieldSleep,
          dmYieldSleepEx: Sleep(WaitParamsInternal.PublicParams.WaitParams.SleepTime);
        {$ENDIF}
        else
         {dmSpin}
          SpinDelay(WaitParamsInternal.PublicParams.WaitParams.DelayCount);
        end
      else
        begin
          // not in infinite wait and timeout has elapsed
          WaitParamsInternal.DequeueFce(WaitParamsInternal.SyncWordPtr^,WaitParamsInternal.CallData);
          Result := wrTimeout;
          Exit;
        end;
    Result := wrAcquired;
  end;

begin
case WaitParamsInternal.EnqueueFce(WaitParamsInternal.SyncWordPtr^,WaitParamsInternal.CallData) of
  qrAcquired: Result := wrAcquired;
  qrQueued:   If WaitParamsInternal.PublicParams.WaitParams.Timeout > 0 then
                Result := WaitInternal
              else
                Result := wrTimeout;
  qrDeadlock: Result := wrDeadlock;  
else
 {qrFailed}   Result := wrTryAgain;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TFastLock
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastLock - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TFastLock - protected methods implementation
-------------------------------------------------------------------------------}

Function TFastLock.GetReferenceCount: Integer;
begin
Result := InterlockedLoad(fRefCount);
end;

//------------------------------------------------------------------------------

Function TFastLock.GetSpinParamsPtr: PFLSpinParams;
begin
Result := @fSpinParams;
end;

//------------------------------------------------------------------------------

Function TFastLock.GetWaitParamsPtr: PFLWaitParams;
begin
Result := @fWaitParams;
end;

//------------------------------------------------------------------------------

Function TFastLock.AcquireReference: Integer;
begin
Result := InterlockedIncrement(fRefCount);
end;

//------------------------------------------------------------------------------

Function TFastLock.ReleaseReference: Integer;
begin
Result := InterlockedDecrement(fRefCount);
end;

//------------------------------------------------------------------------------

procedure TFastLock.Initialize(SyncWordPtr: PFLSyncWord; InitSyncWord: Boolean; const InitArgs: array of const);
begin
// do not touch reference counter
InterlockedStore(fSyncWord,FL_INITVAL);
fSyncWordPtr := SyncWordPtr;
fIsInitializer := InitSyncWord;
If fIsInitializer then
  SyncWordInit(InitArgs);
If fMode = flmSlave then
  begin
    fSpinParams := fMaster.SpinParams;
    fWaitParams := fMaster.WaitParams;
  end
else
  begin
    fSpinParams := DefaultSpinParams;
    fWaitParams := DefaultWaitParams;
  end;
end;

//------------------------------------------------------------------------------

procedure TFastLock.Finalize;
begin
If fIsInitializer then
  SyncWordFinal;
end;

//------------------------------------------------------------------------------

class Function TFastLock.ArgTypePresent(const InitArgs: array of const; Index: Integer; VType: Byte): Boolean;
begin
Result := False;
If Length(InitArgs) > Index then
  Result := VType = InitArgs[Index].VType;
end;

{-------------------------------------------------------------------------------
    TFastLock - public methods implementation
-------------------------------------------------------------------------------}

procedure TFastLock.FreeInstance;
begin
If fCanFreeInstance then
  inherited FreeInstance;
end;

//------------------------------------------------------------------------------

constructor TFastLock.CreateBase({$IFNDEF FPC}Dummy: TFLSyncWord = 0{$ENDIF});
begin
{$IFNDEF FPC}ConsumeArg(Dummy);{$ENDIF}
inherited Create;
end;

//------------------------------------------------------------------------------

constructor TFastLock.CreateOwner({$IFNDEF FPC}DummyA: TFLSyncWord = 0; DummyB: TFLSyncWord = 0{$ENDIF});
begin
{$IFNDEF FPC}
ConsumeArg(DummyA);
ConsumeArg(DummyB);
{$ENDIF}
CreateBase;
fMode := flmOwner;
Initialize(@fSyncWord,True,[]);
AcquireReference;
end;

//------------------------------------------------------------------------------

constructor TFastLock.CreateSlave(Master: TFastLock{$IFNDEF FPC}; Dummy: TFLSyncWord = 0{$ENDIF});
begin
{$IFNDEF FPC}ConsumeArg(Dummy);{$ENDIF}
CreateBase;
fMode := flmSlave;
If not (Master is Self.ClassType) then
  raise EFLInvalidValue.CreateFmt('TFastLock.CreateSlave: Master object is of incompatible class (%s).',[Master.ClassName]);
fMaster := Master;
If fMaster.AcquireReference <= 1 then
  raise EFLInvalidState.Create('TFastLock.CreateSlave: Master object is being destroyed.');
Initialize(fMaster.fSyncWordPtr,False,[]);
AcquireReference;
end;

//------------------------------------------------------------------------------

constructor TFastLock.CreateWrapper(var SyncWord: TFLSyncWord; InitSyncWord: Boolean = True{$IFNDEF FPC}; Dummy: TFLSyncWord = 0{$ENDIF});
begin
{$IFNDEF FPC}ConsumeArg(Dummy);{$ENDIF}
CreateBase;
fMode := flmWrapper;
Initialize(@SyncWord,InitSyncWord,[]);
AcquireReference;
end;

//------------------------------------------------------------------------------

constructor TFastLock.Create;
begin
CreateOwner;
end;

//------------------------------------------------------------------------------

constructor TFastLock.Create(Master: TFastLock);
begin
CreateSlave(Master);
end;

//------------------------------------------------------------------------------

destructor TFastLock.Destroy;
begin
{
  FreeInstance is called at the end of this function, but we need to suppress
  it if we are not actually freeing - fCanFreeInstance is used for that and
  method FreeInstance is overriden to check its value.
}
fCanFreeInstance := ReleaseReference <= 0;
If fCanFreeInstance then
  begin
    Finalize;
    If (fMode = flmSlave) and Assigned(fMaster) then
      If fMaster.ReleaseReference <= 0 then
        FreeAndNil(fMaster);
    inherited Destroy;
  end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   Fast event                                   
--------------------------------------------------------------------------------
===============================================================================}
{
      Hi  ... highest bit in the sync word (31 for 32bit words and 63 for
              64bit words)
      <x> ... immutable bits (set in initialization and then only read, usually
              used for lock settings)

  Following is a specification of bits and fields in sync word for fast events
  in current implementation (note that it can be changed without warning in
  future revisions, so do not depend on it):

           Hi      - (V) validity bit, must be 1
           Hi-1    - (L) lock bit (0 signaled, 1 non signaled)
           Hi-2    - (P) pulsing (0 not pulsing, 1 pulsing)
          <Hi-3>   - (M) manual reset (0 auto reset, 1 manual reset)
     10 .. Hi-4    -     unused
      0 .. 9       - (W) wait counter (max 1023 waiters)
}
const
  FL_EVENT_MASK_VALID       = FL_COMMON_MASK_VALID;
  FL_EVENT_MASK_LOCK        = TFLSyncWord(1) shl (FL_WORDHIBIT - 1);
  FL_EVENT_MASK_PULSING     = TFLSyncWord(1) shl (FL_WORDHIBIT - 2);
  FL_EVENT_MASK_MANUALRESET = TFLSyncWord(1) shl (FL_WORDHIBIT - 3);
  FL_EVENT_MASK_WAITCOUNTER = (TFLSyncWord(1) shl 10) - 1;

  FL_EVENT_IOPRES_INVALID = -1;
  FL_EVENT_IOPRES_SUCCESS = 0;
  FL_EVENT_IOPRES_LOCKED  = 1;
  FL_EVENT_IOPRES_PULSING = 2;
  FL_EVENT_IOPRES_WAITERS = 3;
  FL_EVENT_IOPRES_QUEUED  = 4;

{===============================================================================
    Fast event - internal functions implementation
===============================================================================}

Function FastEventResetIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
// L := 1,  P := 0
If (SyncWord and FL_EVENT_MASK_VALID) <> 0 then
  begin
    SyncWord := (SyncWord or FL_EVENT_MASK_LOCK) and not FL_EVENT_MASK_PULSING;
    Result := FL_EVENT_IOPRES_SUCCESS;
  end
else Result := FL_EVENT_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastEventPulseIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
// L := 1,  P := (W <> 0)
If (SyncWord and FL_EVENT_MASK_VALID) <> 0 then
  begin
    If (SyncWord and FL_EVENT_MASK_WAITCOUNTER) <> 0 then
      SyncWord := SyncWord or (FL_EVENT_MASK_LOCK or FL_EVENT_MASK_PULSING)
    else
      SyncWord := (SyncWord or FL_EVENT_MASK_LOCK) and not FL_EVENT_MASK_PULSING;
    Result := FL_EVENT_IOPRES_SUCCESS;
  end
else Result := FL_EVENT_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastEventPassIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_EVENT_MASK_VALID) <> 0 then
  begin
    If (SyncWord and FL_EVENT_MASK_LOCK) = 0 then
      begin
        // event is signaled (unlocked)
        If (SyncWord and FL_EVENT_MASK_PULSING) = 0 then
          begin
            // so, are there any waiters?
            If (SyncWord and FL_EVENT_MASK_WAITCOUNTER) = 0 then
              begin
                // no waiters, manage auto-reset and report success
                If (SyncWord and FL_EVENT_MASK_MANUALRESET) = 0 then
                  SyncWord := SyncWord or FL_EVENT_MASK_LOCK;
                Result := FL_EVENT_IOPRES_SUCCESS;
              end
          {
            There are waiters. We can pass only if this is manual-reset event
            (waiters have precedence over us and we would block them in auto-
            reset event).
          }
            else If (SyncWord and FL_EVENT_MASK_MANUALRESET) <> 0 then
              Result := FL_EVENT_IOPRES_SUCCESS
            else
              Result := FL_EVENT_IOPRES_WAITERS;
          end
        // if L = 0, then P must also be 0
        else Result := FL_EVENT_IOPRES_INVALID;
      end
    // locked - we have failed in any case, but report if pulsing
    else If (SyncWord and FL_EVENT_MASK_PULSING) <> 0 then
      Result := FL_EVENT_IOPRES_PULSING
    else
      Result := FL_EVENT_IOPRES_LOCKED;
  end
else Result := FL_EVENT_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastEventEnqueueIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_EVENT_MASK_VALID) <> 0 then
  begin
    If (SyncWord and FL_EVENT_MASK_LOCK) = 0 then
      begin
        // event is signaled, we can actually pass it
        If (SyncWord and FL_EVENT_MASK_PULSING) = 0 then
          begin
            // no need to check for waiters, we are waiter
            If (SyncWord and FL_EVENT_MASK_MANUALRESET) = 0 then
              SyncWord := SyncWord or FL_EVENT_MASK_LOCK;
            Result := FL_EVENT_IOPRES_SUCCESS;
          end
        else Result := FL_EVENT_IOPRES_INVALID;
      end
    else
      begin
        // event is non-signaled (locked), add us to the queue
        If (SyncWord and FL_EVENT_MASK_PULSING) = 0 then
          begin
            If (SyncWord and FL_EVENT_MASK_WAITCOUNTER) < FL_EVENT_MASK_WAITCOUNTER then
              begin
                Inc(SyncWord);
                Result := FL_EVENT_IOPRES_QUEUED;
              end
            else Result := FL_EVENT_IOPRES_WAITERS;
          end
        // cannot enqueue if the event is pulsing
        else Result := FL_EVENT_IOPRES_PULSING;
      end;
  end
else Result := FL_EVENT_IOPRES_INVALID;  
end;

//------------------------------------------------------------------------------

Function FastEventQueuedAcquireIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
// this can only be called after successfull queueing
If ((SyncWord and FL_EVENT_MASK_VALID) <> 0) and ((SyncWord and FL_EVENT_MASK_WAITCOUNTER) <> 0) then
  begin
    If (SyncWord and FL_EVENT_MASK_LOCK) = 0 then
      begin
        If (SyncWord and FL_EVENT_MASK_PULSING) = 0 then
          begin
            If (SyncWord and FL_EVENT_MASK_MANUALRESET) = 0 then
              SyncWord := SyncWord or FL_EVENT_MASK_LOCK;
            // dequeue  
            Dec(SyncWord);
            Result := FL_EVENT_IOPRES_SUCCESS;
          end
        else Result := FL_EVENT_IOPRES_INVALID;
      end
    else
      begin
        // non-signaled, as we are already queued, we can potentially pass on pulsing
        If (SyncWord and FL_EVENT_MASK_PULSING) <> 0 then
          begin
            // pulsing event, we are queued so it is in effect for us too, but first dequeue
            Dec(SyncWord);
            If (SyncWord and FL_EVENT_MASK_MANUALRESET) <> 0 then
              begin
                // manual-reset event, end pulsing only if no other thread is waiting
                If (SyncWord and FL_EVENT_MASK_WAITCOUNTER) = 0 then
                  SyncWord := (SyncWord or FL_EVENT_MASK_LOCK) and not FL_EVENT_MASK_PULSING;
              end
            // auto-reset event, end pulsing now and reset state
            else SyncWord := (SyncWord or FL_EVENT_MASK_LOCK) and not FL_EVENT_MASK_PULSING;
            Result := FL_EVENT_IOPRES_SUCCESS;
          end
        else Result := FL_EVENT_IOPRES_LOCKED;
      end;
  end
else Result := FL_EVENT_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastEventDequeueIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
// must only be called after successfull queueing
If ((SyncWord and FL_EVENT_MASK_VALID) <> 0) and ((SyncWord and FL_EVENT_MASK_WAITCOUNTER) <> 0) then
  begin
    // just dequeue, ignore everything else
    Dec(SyncWord);
    Result := FL_EVENT_IOPRES_SUCCESS;
  end
else Result := FL_EVENT_IOPRES_INVALID;
end;

//==============================================================================

Function FastEventEnqueue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): TFLQueueResult;
begin
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastEventEnqueueIOP) of
  FL_EVENT_IOPRES_SUCCESS:  Result := qrAcquired;
  FL_EVENT_IOPRES_PULSING,
  FL_EVENT_IOPRES_WAITERS:  Result := qrFailed;
  FL_EVENT_IOPRES_QUEUED:   Result := qrQueued;
else
 {FL_EVENT_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastEventEnqueue: Invalid state of event sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastEventQueuedAcquire(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): Boolean;
begin
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastEventQueuedAcquireIOP) of
  FL_EVENT_IOPRES_SUCCESS:  Result := True;
  FL_EVENT_IOPRES_LOCKED:   Result := False;
else
 {FL_EVENT_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastEventQueuedAcquire: Invalid state of event sync word.');
end;
end;

//------------------------------------------------------------------------------

procedure FastEventDequeue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord);
begin
ConsumeArg(CallData);
If InterlockedOperation(SyncWord,FastEventDequeueIOP) <> FL_EVENT_IOPRES_SUCCESS then
  raise EFLInvalidState.Create('FastEventDequeue: Invalid state of event sync word.');
end;

{===============================================================================
    Fast event - procedural interface implementation
===============================================================================}

procedure FastEventInit(out SyncWord: TFLSyncWord; ManualReset: Boolean = False; InitialState: Boolean = False);
var
  SyncWordValue:  TFLSyncWord;
begin
// M := ManualReset, L := not InitialState
SyncWordValue := TFLSyncWord(FL_INITVAL or FL_EVENT_MASK_VALID);
If ManualReset then
  SyncWordValue := SyncWordValue or FL_EVENT_MASK_MANUALRESET;
If not InitialState then
  SyncWordValue := SyncWordValue or FL_EVENT_MASK_LOCK;
InterlockedStore(TFLSyncWord((@SyncWord)^),SyncWordValue);
end;

//------------------------------------------------------------------------------

procedure FastEventFinal(var SyncWord: TFLSyncWord);
begin
InterlockedStore(SyncWord,FL_INITVAL);
end;

//------------------------------------------------------------------------------

procedure FastEventSet(var SyncWord: TFLSyncWord);
begin
If not IsValid(InterlockedAnd(SyncWord,not(FL_EVENT_MASK_LOCK or FL_EVENT_MASK_PULSING))) then
  raise EFLInvalidState.Create('FastEventSet: Invalid state of event sync word.');
end;

//------------------------------------------------------------------------------

procedure FastEventReset(var SyncWord: TFLSyncWord);
begin
If InterlockedOperation(SyncWord,FastEventResetIOP) = FL_EVENT_IOPRES_INVALID then
  raise EFLInvalidState.Create('FastEventReset: Invalid state of event sync word.');
end;

//------------------------------------------------------------------------------

procedure FastEventPulse(var SyncWord: TFLSyncWord);
begin
If InterlockedOperation(SyncWord,FastEventPulseIOP) = FL_EVENT_IOPRES_INVALID then
  raise EFLInvalidState.Create('FastEventPulse: Invalid state of event sync word.');
end;

//------------------------------------------------------------------------------

Function FastEventPass(var SyncWord: TFLSyncWord): Boolean;
begin
case InterlockedOperation(SyncWord,FastEventPassIOP) of
  FL_EVENT_IOPRES_SUCCESS:  Result := True;
  FL_EVENT_IOPRES_LOCKED,
  FL_EVENT_IOPRES_PULSING,
  FL_EVENT_IOPRES_WAITERS:  Result := False;
else
 {FL_EVENT_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastEventPass: Invalid state of event sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastEventSpinToPass(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.SpinParams := SpinParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastEventEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastEventQueuedAcquire;
WaitParamsInternal.DequeueFce := FastEventDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteSpinning(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastEventSpinToPass(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult;
var
  SpinParams: TFLSpinParams;
begin
SpinParams := DefaultSpinParams;
SpinParams.SpinCount := SpinCount;
Result := FastEventSpinToPass(SyncWord,SpinParams);
end;

//------------------------------------------------------------------------------

Function FastEventWaitToPass(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.WaitParams := WaitParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastEventEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastEventQueuedAcquire;
WaitParamsInternal.DequeueFce := FastEventDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteWaiting(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastEventWaitToPass(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult;
var
  WaitParams: TFLWaitParams;
begin
WaitParams := DefaultWaitParams;
WaitParams.Timeout := Timeout;
Result := FastEventWaitToPass(SyncWord,WaitParams);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TFastEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastEvent - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TFastEvent - protected methods implementation
-------------------------------------------------------------------------------}

procedure TFastEvent.SyncWordInit(const InitArgs: array of const);
var
  ManualReset:  Boolean;
  InitialState: Boolean;
begin
ManualReset := False;
If ArgTypePresent(InitArgs,0,vtBoolean) then
  ManualReset := InitArgs[0].VBoolean;
InitialState := False;
If ArgTypePresent(InitArgs,1,vtBoolean) then
  InitialState := InitArgs[1].VBoolean;
FastEventInit(fSyncWordPtr^,ManualReset,InitialState);
end;

//------------------------------------------------------------------------------

procedure TFastEvent.SyncWordFinal;
begin
FastEventFinal(fSyncWordPtr^);
end;

{-------------------------------------------------------------------------------
    TFastEvent - public methods implementation
-------------------------------------------------------------------------------}

constructor TFastEvent.Create(ManualReset: Boolean; InitialState: Boolean);
begin
CreateBase;
fMode := flmOwner;
Initialize(@fSyncWord,True,[ManualReset,InitialState]);
AcquireReference;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFastEvent.Create(var SyncWord: TFLSyncWord; ManualReset: Boolean; InitialState: Boolean);
begin
CreateBase;
fMode := flmWrapper;
Initialize(@SyncWord,True,[ManualReset,InitialState]);
AcquireReference;
end;

//------------------------------------------------------------------------------

procedure TFastEvent.Set_;
begin
FastEventSet(fSyncWordPtr^);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TFastEvent.SetEvent;
begin
FastEventSet(fSyncWordPtr^);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TFastEvent.SetSignaled;
begin
FastEventSet(fSyncWordPtr^);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TFastEvent.Signal;
begin
FastEventSet(fSyncWordPtr^);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TFastEvent.Unlock;
begin
FastEventSet(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastEvent.Reset;
begin
FastEventReset(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastEvent.Pulse;
begin
FastEventPulse(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastEvent.Pass: Boolean;
begin
Result := FastEventPass(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastEvent.SpinToPass(SpinCount: UInt32): TFLWaitResult;
var
  LocalSpinParams:  TFLSpinParams;
begin
LocalSpinParams := fSpinParams;
LocalSpinParams.SpinCount := SpinCount;
Result := FastEventSpinToPass(fSyncWordPtr^,LocalSpinParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastEvent.SpinToPass: TFLWaitResult;
begin
Result := FastEventSpinToPass(fSyncWordPtr^,fSpinParams);
end;

//------------------------------------------------------------------------------

Function TFastEvent.WaitToPass(Timeout: UInt32): TFLWaitResult;
var
  LocalWaitParams:  TFLWaitParams;
begin
LocalWaitParams := fWaitParams;
LocalWaitParams.Timeout := Timeout;
Result := FastEventWaitToPass(fSyncWordPtr^,LocalWaitParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastEvent.WaitToPass: TFLWaitResult;
begin
Result := FastEventWaitToPass(fSyncWordPtr^,fWaitParams);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Fast semaphore
--------------------------------------------------------------------------------
===============================================================================}
{
               Hi      - (V) validity bit, must be 1
      Hi-10 .. Hi-1    - (W) wait counter (max 1023 waiters)
          0 .. Hi-11   - (C) main counter (0 non signaled, >0 signaled, max
                             value 2097151 or 9007199254740991)
}
const
  FL_SEMAPHORE_MASK_VALID       = FL_COMMON_MASK_VALID;
  FL_SEMAPHORE_MASK_WAITCOUNTER = ((TFLSyncWord(1) shl 10) - 1) shl (FL_WORDHIBIT - 10);
  FL_SEMAPHORE_MASK_MAINCOUNTER = (TFLSyncWord(1) shl (FL_WORDHIBIT - 10)) - 1;

  FL_SEMAPHORE_DELTA_WAIT = TFLSyncWord(1) shl (FL_WORDHIBIT - 10);

  FL_SEMAPHORE_IOPRES_INVALID  = -1;
  FL_SEMAPHORE_IOPRES_SUCCESS  = 0;
  FL_SEMAPHORE_IOPRES_LOCKED   = 1;
  FL_SEMAPHORE_IOPRES_WAITERS  = 2;
  FL_SEMAPHORE_IOPRES_OVERFLOW = 3;
  FL_SEMAPHORE_IOPRES_QUEUED   = 4;

{===============================================================================
    Fast semaphore - internal functions implementation
===============================================================================}

Function FastSemaphoreAcquireIOP(var SyncWord: TFLSyncWord; AcquireCount: TFLSyncWord): TFLSyncWord; register;
begin
// AcquireCount must be properly masked or bound-checked but unshifted before passing it here
If (SyncWord and FL_SEMAPHORE_MASK_VALID) <> 0 then
  begin
  {
    Asynchronous acquire can be done only when there is no waiter and of
    course the state is signaled (main counter must be at least equal to
    acquire count).
  }
    If (SyncWord and FL_SEMAPHORE_MASK_MAINCOUNTER) >= AcquireCount then
      begin
        If (SyncWord and FL_SEMAPHORE_MASK_WAITCOUNTER) = 0 then
          begin
            SyncWord := SyncWord - AcquireCount;
            Result := FL_SEMAPHORE_IOPRES_SUCCESS;
          end
        else Result := FL_SEMAPHORE_IOPRES_WAITERS;
      end
    else If (SyncWord and FL_SEMAPHORE_MASK_MAINCOUNTER) <> 0 then
      Result := FL_SEMAPHORE_IOPRES_OVERFLOW
    else
      Result := FL_SEMAPHORE_IOPRES_LOCKED;
  end
else Result := FL_SEMAPHORE_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastSemaphoreReleaseIOP(var SyncWord: TFLSyncWord; ReleaseCount: TFLSyncWord): TFLSyncWord; register;
begin
// ReleaseCount must be checked externally
If (SyncWord and FL_SEMAPHORE_MASK_VALID) <> 0 then
  begin
    // allow release only if release count cannot overflow counter
    If (SyncWord and FL_SEMAPHORE_MASK_MAINCOUNTER) <= (FL_SEMAPHORE_MASK_MAINCOUNTER - ReleaseCount) then
      begin
        SyncWord := SyncWord + ReleaseCount;
        Result := FL_SEMAPHORE_IOPRES_SUCCESS;
      end
    else Result := FL_SEMAPHORE_IOPRES_OVERFLOW;
  end
else Result := FL_SEMAPHORE_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastSemaphoreEnqueueIOP(var SyncWord: TFLSyncWord; AcquireCount: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_SEMAPHORE_MASK_VALID) <> 0 then
  begin
    If (SyncWord and FL_SEMAPHORE_MASK_MAINCOUNTER) >= AcquireCount then
      begin
        // we can acquire the semaphore directly (do not check for waiters)
        SyncWord := SyncWord - AcquireCount;
        Result := FL_SEMAPHORE_IOPRES_SUCCESS;
      end
    // try to enqueue
    else If (SyncWord and FL_SEMAPHORE_MASK_WAITCOUNTER) < FL_SEMAPHORE_MASK_WAITCOUNTER then
      begin
        SyncWord := SyncWord + FL_SEMAPHORE_DELTA_WAIT;
        Result := FL_SEMAPHORE_IOPRES_QUEUED;
      end
    else Result := FL_SEMAPHORE_IOPRES_WAITERS;
  end
else Result := FL_SEMAPHORE_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastSemaphoreQueuedAcquireIOP(var SyncWord: TFLSyncWord; AcquireCount: TFLSyncWord): TFLSyncWord; register;
begin
If ((SyncWord and FL_SEMAPHORE_MASK_VALID) <> 0) and ((SyncWord and FL_SEMAPHORE_MASK_WAITCOUNTER) <> 0) then
  begin
    If (SyncWord and FL_SEMAPHORE_MASK_MAINCOUNTER) >= AcquireCount then
      begin
        // can acquire...
        SyncWord := SyncWord - AcquireCount;
        // dequeue
        SyncWord := SyncWord - FL_SEMAPHORE_DELTA_WAIT;
        Result := FL_SEMAPHORE_IOPRES_SUCCESS;
      end
    else If (SyncWord and FL_SEMAPHORE_MASK_MAINCOUNTER) <> 0 then
      Result := FL_SEMAPHORE_IOPRES_OVERFLOW
    else
      Result := FL_SEMAPHORE_IOPRES_LOCKED;
  end
else Result := FL_SEMAPHORE_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastSemaphoreDequeueIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If ((SyncWord and FL_SEMAPHORE_MASK_VALID) <> 0) and ((SyncWord and FL_SEMAPHORE_MASK_WAITCOUNTER) <> 0) then
  begin
    SyncWord := SyncWord - FL_SEMAPHORE_DELTA_WAIT;
    Result := FL_SEMAPHORE_IOPRES_SUCCESS;
  end
else  Result := FL_SEMAPHORE_IOPRES_INVALID;
end;

//==============================================================================

Function FastSemaphoreEnqueue(var SyncWord: TFLSyncWord; AcquireCount: TFLSyncWord): TFLQueueResult;
begin
case InterlockedOperation(SyncWord,AcquireCount,FastSemaphoreEnqueueIOP) of
  FL_SEMAPHORE_IOPRES_SUCCESS:  Result := qrAcquired;
  FL_SEMAPHORE_IOPRES_WAITERS:  Result := qrFailed;
  FL_SEMAPHORE_IOPRES_QUEUED:   Result := qrQueued;
else
 {FL_SEMAPHORE_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastSemaphoreEnqueue: Invalid state of semaphore sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastSemaphoreQueuedAcquire(var SyncWord: TFLSyncWord; AcquireCount: TFLSyncWord): Boolean;
begin
case InterlockedOperation(SyncWord,AcquireCount,FastSemaphoreQueuedAcquireIOP) of
  FL_SEMAPHORE_IOPRES_SUCCESS:  Result := True;
  FL_SEMAPHORE_IOPRES_LOCKED,
  FL_SEMAPHORE_IOPRES_OVERFLOW: Result := False;
else
 {FL_SEMAPHORE_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastSemaphoreQueuedAcquire: Invalid state of semaphore sync word.');
end;
end;

//------------------------------------------------------------------------------

procedure FastSemaphoreDequeue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord);
begin
ConsumeArg(CallData);
If InterlockedOperation(SyncWord,FastSemaphoreDequeueIOP) <> FL_SEMAPHORE_IOPRES_SUCCESS then
  raise EFLInvalidState.Create('FastSemaphoreDequeue: Invalid state of semaphore sync word.');
end;

{===============================================================================
    Fast semaphore - procedural interface implementation
===============================================================================}

procedure FastSemaphoreInit(out SyncWord: TFLSyncWord; InitialCount: TFLSyncWord = 0);
var
  SyncWordValue:  TFLSyncWord;
begin
SyncWordValue := TFLSyncWord(FL_INITVAL or FL_SEMAPHORE_MASK_VALID);
// check bounds for initial value of main counter
If (InitialCount < 0) or (InitialCount > FL_SEMAPHORE_MASK_MAINCOUNTER) then
  raise EFLInvalidValue.CreateFmt('FastSemaphoreInit: Invalid semaphore count (%d)',[InitialCount]);
SyncWordValue := SyncWordValue or (InitialCount and FL_SEMAPHORE_MASK_MAINCOUNTER);
InterlockedStore(TFLSyncWord((@SyncWord)^),SyncWordValue);
end;

//------------------------------------------------------------------------------

procedure FastSemaphoreFinal(var SyncWord: TFLSyncWord);
begin
InterlockedStore(SyncWord,FL_INITVAL);
end;

//------------------------------------------------------------------------------

Function FastSemaphoreCount(var SyncWord: TFLSyncWord): TFLSyncWord;
begin
Result := InterlockedLoad(SyncWord);
If IsValid(Result) then
  Result := Result and FL_SEMAPHORE_MASK_MAINCOUNTER
else
  raise EFLInvalidState.Create('FastSemaphoreCount: Invalid state of semaphore sync word.');
end;

//------------------------------------------------------------------------------

Function FastSemaphoreAcquire(var SyncWord: TFLSyncWord; AcquireCount: TFLSyncWord = 1): Boolean;
begin
If (AcquireCount <= 0) or (AcquireCount > FL_SEMAPHORE_MASK_MAINCOUNTER) then
  raise EFLInvalidValue.CreateFmt('FastSemaphoreAcquire: Invalid semaphore acquire count (%d)',[AcquireCount]);
case InterlockedOperation(SyncWord,AcquireCount,FastSemaphoreAcquireIOP) of
  FL_SEMAPHORE_IOPRES_SUCCESS:  Result := True;
  FL_SEMAPHORE_IOPRES_LOCKED,
  FL_SEMAPHORE_IOPRES_WAITERS,
  FL_SEMAPHORE_IOPRES_OVERFLOW: Result := False;
else
 {FL_SEMAPHORE_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastSemaphoreAcquire: Invalid state of semaphore sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastSemaphoreRelease(var SyncWord: TFLSyncWord; ReleaseCount: TFLSyncWord = 1): Boolean;
begin
If (ReleaseCount <= 0) or (ReleaseCount > FL_SEMAPHORE_MASK_MAINCOUNTER) then
  raise EFLInvalidValue.CreateFmt('FastSemaphoreAcquire: Invalid semaphore release count (%d)',[ReleaseCount]);  
case InterlockedOperation(SyncWord,ReleaseCount,FastSemaphoreReleaseIOP) of
  FL_SEMAPHORE_IOPRES_SUCCESS:  Result := True;
  FL_SEMAPHORE_IOPRES_OVERFLOW: Result := False;
else
 {FL_SEMAPHORE_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastSemaphoreRelease: Invalid state of semaphore sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastSemaphoreSpinToAcquire(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams; AcquireCount: TFLSyncWord = 1): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
If (AcquireCount <= 0) or (AcquireCount > FL_SEMAPHORE_MASK_MAINCOUNTER) then
  raise EFLInvalidValue.CreateFmt('FastSemaphoreSpinToAcquire: Invalid semaphore acquire count (%d)',[AcquireCount]);
WaitParamsInternal.PublicParams.SpinParams := SpinParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastSemaphoreEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastSemaphoreQueuedAcquire;
WaitParamsInternal.DequeueFce := FastSemaphoreDequeue;
WaitParamsInternal.CallData := AcquireCount;
Result := ExecuteSpinning(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastSemaphoreSpinToAcquire(var SyncWord: TFLSyncWord; SpinCount: UInt32; AcquireCount: TFLSyncWord = 1): TFLWaitResult;
var
  SpinParams: TFLSpinParams;
begin
// acquire count is checked in called overload of FastSemaphoreSpinToAcquire
SpinParams := DefaultSpinParams;
SpinParams.SpinCount := SpinCount;
Result := FastSemaphoreSpinToAcquire(SyncWord,SpinParams,AcquireCount);
end;

//------------------------------------------------------------------------------

Function FastSemaphoreWaitToAcquire(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams; AcquireCount: TFLSyncWord = 1): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.WaitParams := WaitParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastSemaphoreEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastSemaphoreQueuedAcquire;
WaitParamsInternal.DequeueFce := FastSemaphoreDequeue;
WaitParamsInternal.CallData := AcquireCount;
Result := ExecuteWaiting(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastSemaphoreWaitToAcquire(var SyncWord: TFLSyncWord; Timeout: UInt32; AcquireCount: TFLSyncWord = 1): TFLWaitResult;
var
  WaitParams: TFLWaitParams;
begin
If (AcquireCount <= 0) or (AcquireCount > FL_SEMAPHORE_MASK_MAINCOUNTER) then
  raise EFLInvalidValue.CreateFmt('FastSemaphoreWaitToAcquire: Invalid semaphore acquire count (%d)',[AcquireCount]);
WaitParams := DefaultWaitParams;
WaitParams.Timeout := Timeout;
Result := FastSemaphoreWaitToAcquire(SyncWord,WaitParams,AcquireCount);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TFastSemaphore
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastSemaphore - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFastSemaphore - protected methods implementation
-------------------------------------------------------------------------------}

procedure TFastSemaphore.SyncWordInit(const InitArgs: array of const);
var
  InitialCount: TFLSyncWord;
begin
InitialCount := 0;
{$IFDEF SyncWord64}
If ArgTypePresent(InitArgs,0,vtInt64) then
  InitialCount := InitArgs[0].VInt64^;  // int64 is stored only as reference
{$ELSE}
If ArgTypePresent(InitArgs,0,vtInteger) then
  InitialCount := InitArgs[0].VInteger;
{$ENDIF}
FastSemaphoreInit(fSyncWordPtr^,InitialCount);
end;

//------------------------------------------------------------------------------

procedure TFastSemaphore.SyncWordFinal;
begin
FastSemaphoreFinal(fSyncWordPtr^);
end;

{-------------------------------------------------------------------------------
    TFastSemaphore - public methods implementation
-------------------------------------------------------------------------------}

constructor TFastSemaphore.Create(InitialCount: TFLSyncWord);
begin
CreateBase;
fMode := flmOwner;
Initialize(@fSyncWord,True,[InitialCount]);
AcquireReference;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFastSemaphore.Create(var SyncWord: TFLSyncWord; InitialCount: TFLSyncWord);
begin
CreateBase;
fMode := flmWrapper;
Initialize(@SyncWord,True,[InitialCount]);
AcquireReference;
end;

//------------------------------------------------------------------------------

Function TFastSemaphore.Count: TFLSyncWord;
begin
Result := FastSemaphoreCount(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastSemaphore.Acquire(AcquireCount: TFLSyncWord = 1): Boolean;
begin
Result := FastSemaphoreAcquire(fSyncWordPtr^,AcquireCount);
end;

//------------------------------------------------------------------------------

Function TFastSemaphore.Release(ReleaseCount: TFLSyncWord = 1): Boolean;
begin
Result := FastSemaphoreRelease(fSyncWordPtr^,ReleaseCount);
end;

//------------------------------------------------------------------------------

Function TFastSemaphore.SpinToAcquireBy(SpinCount: UInt32; AcquireCount: TFLSyncWord): TFLWaitResult;
var
  LocalSpinParams:  TFLSpinParams;
begin
LocalSpinParams := fSpinParams;
LocalSpinParams.SpinCount := SpinCount;
Result := FastSemaphoreSpinToAcquire(fSyncWordPtr^,LocalSpinParams,AcquireCount);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastSemaphore.SpinToAcquireBy(AcquireCount: TFLSyncWord): TFLWaitResult;
begin
Result := FastSemaphoreSpinToAcquire(fSyncWordPtr^,fSpinParams,AcquireCount);
end;

//------------------------------------------------------------------------------

Function TFastSemaphore.SpinToAcquire(SpinCount: UInt32): TFLWaitResult;
begin
Result := SpinToAcquireBy(SpinCount,1);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastSemaphore.SpinToAcquire: TFLWaitResult;
begin
Result := SpinToAcquireBy(1);
end;

//------------------------------------------------------------------------------

Function TFastSemaphore.WaitToAcquireBy(Timeout: UInt32; AcquireCount: TFLSyncWord): TFLWaitResult;
var
  LocalWaitParams:  TFLWaitParams;
begin
LocalWaitParams := fWaitParams;
LocalWaitParams.Timeout := Timeout;
Result := FastSemaphoreWaitToAcquire(fSyncWordPtr^,LocalWaitParams,AcquireCount);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastSemaphore.WaitToAcquireBy(AcquireCount: TFLSyncWord): TFLWaitResult;
begin
Result := FastSemaphoreWaitToAcquire(fSyncWordPtr^,fWaitParams,AcquireCount);
end;

//------------------------------------------------------------------------------

Function TFastSemaphore.WaitToAcquire(Timeout: UInt32): TFLWaitResult;
begin
Result := WaitToAcquireBy(Timeout,1);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastSemaphore.WaitToAcquire: TFLWaitResult;
begin
Result := WaitToAcquireBy(1);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   Fast mutex
--------------------------------------------------------------------------------
===============================================================================}
{
           Hi      - (V) validity bit, must be 1
           Hi-1    - (L) lock bit (0 signaled, 1 non signaled)
     10 .. Hi-2    -     unused
      0 .. 9       - (W) wait counter (max 1023 waiters)
}
const
  FL_MUTEX_MASK_VALID       = FL_COMMON_MASK_VALID;
  FL_MUTEX_MASK_LOCK        = TFLSyncWord(1) shl (FL_WORDHIBIT - 1);
  FL_MUTEX_MASK_WAITCOUNTER = (TFLSyncWord(1) shl 10) - 1;

  FL_MUTEX_IOPRES_INVALID = -1;
  FL_MUTEX_IOPRES_SUCCESS = 0;
  FL_MUTEX_IOPRES_LOCKED  = 1;
  FL_MUTEX_IOPRES_WAITERS = 2;
  FL_MUTEX_IOPRES_QUEUED  = 3;

{===============================================================================
    Fast mutex - internal functions implementation
===============================================================================}

Function FastMutexAcquireIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_MUTEX_MASK_VALID) <> 0 then
  begin
    If (SyncWord and FL_MUTEX_MASK_LOCK) = 0 then
      begin
        // mutex is signaled, check for waiters (they are prioritized)
        If (SyncWord and FL_MUTEX_MASK_WAITCOUNTER) = 0 then
          begin
            // no wiaters, we can acquire
            SyncWord := SyncWord or FL_MUTEX_MASK_LOCK;
            Result := FL_MUTEX_IOPRES_SUCCESS;
          end
        else Result := FL_MUTEX_IOPRES_WAITERS;
      end
    else Result := FL_MUTEX_IOPRES_LOCKED
  end
else Result := FL_MUTEX_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastMutexEnqueueIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_MUTEX_MASK_VALID) <> 0 then
  begin
    If (SyncWord and FL_MUTEX_MASK_LOCK) = 0 then
      begin
        // can acquire mutex directly
        SyncWord := SyncWord or FL_MUTEX_MASK_LOCK;
        Result := FL_MUTEX_IOPRES_SUCCESS;
      end
    // cannot acquire, try to enqueue
    else If (SyncWord and FL_MUTEX_MASK_WAITCOUNTER) < FL_MUTEX_MASK_WAITCOUNTER then
      begin
        Inc(SyncWord);
        Result := FL_MUTEX_IOPRES_QUEUED;
      end
    else Result := FL_MUTEX_IOPRES_WAITERS;
  end
else Result := FL_MUTEX_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastMutexQueuedAcquireIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If ((SyncWord and FL_MUTEX_MASK_VALID) <> 0) and ((SyncWord and FL_MUTEX_MASK_WAITCOUNTER) <> 0) then
  begin
    If (SyncWord and FL_MUTEX_MASK_LOCK) = 0 then
      begin
        SyncWord := SyncWord or FL_MUTEX_MASK_LOCK;
        Dec(SyncWord);
        Result := FL_MUTEX_IOPRES_SUCCESS;
      end
    else Result := FL_MUTEX_IOPRES_LOCKED;
  end
else Result := FL_MUTEX_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastMutexDequeueIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If ((SyncWord and FL_MUTEX_MASK_VALID) <> 0) and ((SyncWord and FL_MUTEX_MASK_WAITCOUNTER) <> 0) then
  begin
    Dec(SyncWord);
    Result := FL_MUTEX_IOPRES_SUCCESS;
  end
else Result := FL_MUTEX_IOPRES_INVALID;
end;

//==============================================================================

Function FastMutexEnqueue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): TFLQueueResult;
begin
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastMutexEnqueueIOP) of
  FL_MUTEX_IOPRES_SUCCESS:  Result := qrAcquired;
  FL_MUTEX_IOPRES_WAITERS:  Result := qrFailed;
  FL_MUTEX_IOPRES_QUEUED:   Result := qrQueued;
else
 {FL_MUTEX_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastMutexEnqueue: Invalid state of mutex sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastMutexQueuedAcquire(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): Boolean;
begin
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastMutexQueuedAcquireIOP) of
  FL_MUTEX_IOPRES_SUCCESS:  Result := True;
  FL_MUTEX_IOPRES_LOCKED:   Result := False;
else
 {FL_MUTEX_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastMutexQueuedAcquire: Invalid state of mutex sync word.');
end;
end;

//------------------------------------------------------------------------------

procedure FastMutexDequeue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord);
begin
ConsumeArg(CallData);
If InterlockedOperation(SyncWord,FastMutexDequeueIOP) <> FL_MUTEX_IOPRES_SUCCESS then
  raise EFLInvalidState.Create('FastMutexDequeue: Invalid state of mutex sync word.');
end;

{===============================================================================
    Fast mutex - procedural interface implementation
===============================================================================}

procedure FastMutexInit(out SyncWord: TFLSyncWord; InitialState: Boolean = False);
var
  SyncWordValue:  TFLSyncWord;
begin
SyncWordValue := TFLSyncWord(FL_INITVAL or FL_MUTEX_MASK_VALID);
If not InitialState then
  SyncWordValue := SyncWordValue or FL_MUTEX_MASK_LOCK;
InterlockedStore(TFLSyncWord((@SyncWord)^),SyncWordValue);
end;

//------------------------------------------------------------------------------

procedure FastMutexFinal(var SyncWord: TFLSyncWord);
begin
InterlockedStore(SyncWord,FL_INITVAL);
end;

//------------------------------------------------------------------------------

Function FastMutexAcquire(var SyncWord: TFLSyncWord): Boolean;
begin
case InterlockedOperation(SyncWord,FastMutexAcquireIOP) of
  FL_MUTEX_IOPRES_SUCCESS:  Result := True;
  FL_MUTEX_IOPRES_LOCKED,
  FL_MUTEX_IOPRES_WAITERS:  Result := False;
else
 {FL_MUTEX_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastMutexAcquire: Invalid state of mutex sync word.');
end;
end;

//------------------------------------------------------------------------------

procedure FastMutexRelease(var SyncWord: TFLSyncWord);
begin
If not IsValid(InterlockedAnd(SyncWord,not TFLSyncWord(FL_MUTEX_MASK_LOCK))) then
  raise EFLInvalidState.Create('FastMutexRelease: Invalid state of mutex sync word.');
end;

//------------------------------------------------------------------------------

Function FastMutexSpinToAcquire(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.SpinParams := SpinParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastMutexEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastMutexQueuedAcquire;
WaitParamsInternal.DequeueFce := FastMutexDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteSpinning(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastMutexSpinToAcquire(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult;
var
  SpinParams: TFLSpinParams;
begin
SpinParams := DefaultSpinParams;
SpinParams.SpinCount := SpinCount;
Result := FastMutexSpinToAcquire(SyncWord,SpinParams);
end;

//------------------------------------------------------------------------------

Function FastMutexWaitToAcquire(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.WaitParams := WaitParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastMutexEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastMutexQueuedAcquire;
WaitParamsInternal.DequeueFce := FastMutexDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteWaiting(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastMutexWaitToAcquire(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult;
var
  WaitParams: TFLWaitParams;
begin
WaitParams := DefaultWaitParams;
WaitParams.Timeout := Timeout;
Result := FastMutexWaitToAcquire(SyncWord,WaitParams);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TFastMutex
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastMutex - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFastMutex - protected methods implementation
-------------------------------------------------------------------------------}

procedure TFastMutex.SyncWordInit(const InitArgs: array of const);
var
  InitialState: Boolean;
begin
InitialState := False;
If ArgTypePresent(InitArgs,0,vtBoolean) then
  InitialState := InitArgs[0].VBoolean;
FastMutexInit(fSyncWordPtr^,InitialState);
end;

//------------------------------------------------------------------------------

procedure TFastMutex.SyncWordFinal;
begin
FastMutexFinal(fSyncWordPtr^);
end;

{-------------------------------------------------------------------------------
    TFastMutex - public methods implementation
-------------------------------------------------------------------------------}

constructor TFastMutex.Create(InitialState: Boolean);
begin
CreateBase;
fMode := flmOwner;
Initialize(@fSyncWord,True,[InitialState]);
AcquireReference;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFastMutex.Create(var SyncWord: TFLSyncWord; InitialState: Boolean);
begin
CreateBase;
fMode := flmWrapper;
Initialize(@SyncWord,True,[InitialState]);
AcquireReference;
end;

//------------------------------------------------------------------------------

Function TFastMutex.Acquire: Boolean;
begin
Result := FastMutexAcquire(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastMutex.Release;
begin
FastMutexRelease(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastMutex.SpinToAcquire(SpinCount: UInt32): TFLWaitResult;
var
  LocalSpinParams:  TFLSpinParams;
begin
LocalSpinParams := fSpinParams;
LocalSpinParams.SpinCount := SpinCount;
Result := FastMutexSpinToAcquire(fSyncWordPtr^,LocalSpinParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastMutex.SpinToAcquire: TFLWaitResult;
begin
Result := FastMutexSpinToAcquire(fSyncWordPtr^,fSpinParams);
end;

//------------------------------------------------------------------------------

Function TFastMutex.WaitToAcquire(Timeout: UInt32): TFLWaitResult;
var
  LocalWaitParams:  TFLWaitParams;
begin
LocalWaitParams := fWaitParams;
LocalWaitParams.Timeout := Timeout;
Result := FastMutexWaitToAcquire(fSyncWordPtr^,LocalWaitParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastMutex.WaitToAcquire: TFLWaitResult;
begin
Result := FastMutexWaitToAcquire(fSyncWordPtr^,fWaitParams);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Fast read-write lock
--------------------------------------------------------------------------------
===============================================================================}
{
               Hi      - (V) validity bit, must be 1
               Hi-1    - (L) write lock bit (0 unlocked, 1 locked)
               Hi-2    - (P) promoting wait bit
      Hi-12 .. Hi-3    - (W) write wait counter (max 1023 waiters)
         16 .. Hi-13   -     unused
          0 .. 15      - (R) read counter (max 65535 readers)
}
const
  FL_RWLOCK_MASK_VALID       = FL_COMMON_MASK_VALID;
  FL_RWLOCK_MASK_LOCK        = TFLSyncWord(1) shl (FL_WORDHIBIT - 1);
  FL_RWLOCK_MASK_PROMOTING   = TFLSyncWord(1) shl (FL_WORDHIBIT - 2);
  FL_RWLOCK_MASK_WAITCOUNTER = ((TFLSyncWord(1) shl 10) - 1) shl (FL_WORDHIBIT - 12);
  FL_RWLOCK_MASK_READCOUNTER = (TFLSyncWord(1) shl 16) - 1;

  FL_RWLOCK_DELTA_WAIT = TFLSyncWord(1) shl (FL_WORDHIBIT - 11);

  FL_RWLOCK_IOPRES_INVALID   = -1;
  FL_RWLOCK_IOPRES_SUCCESS   = 0;
  FL_RWLOCK_IOPRES_LOCKED    = 1;
  FL_RWLOCK_IOPRES_WAITERS   = 2;
  FL_RWLOCK_IOPRES_PROMOTING = 3;
  FL_RWLOCK_IOPRES_READERS   = 4;
  FL_RWLOCK_IOPRES_UNLOCKED  = 5;
  FL_RWLOCK_IOPRES_QUEUED    = 6;

{===============================================================================
    Fast read-write lock - internal functions implementation
===============================================================================}

Function FastRWLockBeginReadIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_RWLOCK_MASK_VALID) <> 0 then
  begin
  {
    We can acquire read lock only if not write-locked, there is no waiter
    (including pending promotion) and when there are readers, their number
    is below allowed maximum.
  }
    If (SyncWord and FL_RWLOCK_MASK_LOCK) = 0 then
      begin
        If (SyncWord and FL_RWLOCK_MASK_WAITCOUNTER) = 0 then
          begin
            If (SyncWord and FL_RWLOCK_MASK_READCOUNTER) < FL_RWLOCK_MASK_READCOUNTER then
              begin
                Inc(SyncWord);
                Result := FL_RWLOCK_IOPRES_SUCCESS;
              end
            else Result := FL_RWLOCK_IOPRES_READERS;
          end
        // there are waiters, check if any is waiting for lock promotion
        else If (SyncWord and FL_RWLOCK_MASK_PROMOTING) <> 0 then
          Result := FL_RWLOCK_IOPRES_PROMOTING
        else
          Result := FL_RWLOCK_IOPRES_WAITERS;
      end
    // the object is locked for writing, no readers allowed
    else Result := FL_RWLOCK_IOPRES_LOCKED;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockEndReadIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_RWLOCK_MASK_VALID) <> 0 then
  begin
    // report success even if there is no reader
    If (SyncWord and FL_RWLOCK_MASK_READCOUNTER) <> 0 then
      Dec(SyncWord);
    Result := FL_RWLOCK_IOPRES_SUCCESS;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockBeginWriteIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_RWLOCK_MASK_VALID) <> 0 then
  begin
  {
    To acquire write lock, it must not be already locked and there can be no
    waiter (incl. lock promotion) or reader.

    And yes, I am aware that all these AND checks can be coalesced into one.
    I just want this to return specific values for specific situations, not
    just all-encompassing "failed".
  }
    If (SyncWord and FL_RWLOCK_MASK_LOCK) = 0 then
      begin
        If (SyncWord and FL_RWLOCK_MASK_WAITCOUNTER) = 0 then
          begin
            If (SyncWord and FL_RWLOCK_MASK_READCOUNTER) = 0 then
              begin
                SyncWord := SyncWord or FL_RWLOCK_MASK_LOCK;
                Result := FL_RWLOCK_IOPRES_SUCCESS;
              end
            else Result := FL_RWLOCK_IOPRES_READERS;
          end
        else If (SyncWord and FL_RWLOCK_MASK_PROMOTING) <> 0 then
          Result := FL_RWLOCK_IOPRES_PROMOTING
        else
          Result := FL_RWLOCK_IOPRES_WAITERS;
      end
    else Result := FL_RWLOCK_IOPRES_LOCKED;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockWriteEnqueueIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_RWLOCK_MASK_VALID) <> 0 then
  begin
    If (SyncWord and (FL_RWLOCK_MASK_LOCK or FL_RWLOCK_MASK_PROMOTING or FL_RWLOCK_MASK_READCOUNTER)) = 0 then
      begin
      {
        Not write locked, no pending lock promotion and no reader, so we can
        acquire write lock directly (do not check for waiters, we are one too).
      }
        SyncWord := SyncWord or FL_RWLOCK_MASK_LOCK;
        Result := FL_RWLOCK_IOPRES_SUCCESS;
      end
    {
      Either there are readers, object is locked for writing, or someone is
      waiting for lock promotion, none of that precludes waiting.
    }
    else If (SyncWord and FL_RWLOCK_MASK_WAITCOUNTER) < FL_RWLOCK_MASK_WAITCOUNTER then
      begin
        SyncWord := SyncWord + FL_RWLOCK_DELTA_WAIT;
        Result := FL_RWLOCK_IOPRES_QUEUED;
      end
    else Result := FL_RWLOCK_IOPRES_WAITERS;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockWriteQueuedAcquireIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If ((SyncWord and FL_RWLOCK_MASK_VALID) <> 0) and ((SyncWord and FL_RWLOCK_MASK_WAITCOUNTER) <> 0) then
  begin
    If (SyncWord and (FL_RWLOCK_MASK_LOCK or FL_RWLOCK_MASK_PROMOTING or FL_RWLOCK_MASK_READCOUNTER)) = 0 then
      begin
        SyncWord := SyncWord or FL_RWLOCK_MASK_LOCK;  // lock
        SyncWord := SyncWord - FL_RWLOCK_DELTA_WAIT;  // dequeue
        Result := FL_RWLOCK_IOPRES_SUCCESS;
      end
    else If (SyncWord and FL_RWLOCK_MASK_PROMOTING) <> 0 then
      Result := FL_RWLOCK_IOPRES_PROMOTING
    else If (SyncWord and FL_RWLOCK_MASK_READCOUNTER) <> 0 then
      Result := FL_RWLOCK_IOPRES_READERS
    else
      Result := FL_RWLOCK_IOPRES_LOCKED;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockWriteDequeueIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If ((SyncWord and FL_RWLOCK_MASK_VALID) <> 0) and ((SyncWord and FL_RWLOCK_MASK_WAITCOUNTER) <> 0) then
  begin
    SyncWord := SyncWord - FL_RWLOCK_DELTA_WAIT;
    Result := FL_RWLOCK_IOPRES_SUCCESS;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockPromoteIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_RWLOCK_MASK_VALID) <> 0 then
  begin
  {
    Conditions for lock promotion are similar to write lock (no write lock,
    no waiter, incl. no pending promotion), but instead of no reader there
    must be exactly one reader present (that one of which read lock is being
    promoted to write lock).
  }
    If (SyncWord and FL_RWLOCK_MASK_LOCK) = 0 then
      begin
        If (SyncWord and FL_RWLOCK_MASK_WAITCOUNTER) = 0 then
          begin
            If (SyncWord and FL_RWLOCK_MASK_READCOUNTER) = 1 then
              begin
                // lock for write and remove promoted read lock
                SyncWord := SyncWord or FL_RWLOCK_MASK_LOCK;
                Dec(SyncWord);
                Result := FL_RWLOCK_IOPRES_SUCCESS;
              end
            else Result := FL_RWLOCK_IOPRES_READERS;
          end
        else If (SyncWord and FL_RWLOCK_MASK_PROMOTING) <> 0 then
          Result := FL_RWLOCK_IOPRES_PROMOTING
        else
          Result := FL_RWLOCK_IOPRES_WAITERS;
      end
    else Result := FL_RWLOCK_IOPRES_LOCKED;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockDemoteIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_RWLOCK_MASK_VALID) <> 0 then
  begin
    // to demote a lock, it must be write locked, no other conditions are required
    If (SyncWord and FL_RWLOCK_MASK_LOCK) <> 0 then
      begin
        // remove write lock and increment read counter
        SyncWord := SyncWord and not FL_RWLOCK_MASK_LOCK;
        Inc(SyncWord);
        Result := FL_RWLOCK_IOPRES_SUCCESS;
      end
    else Result := FL_RWLOCK_IOPRES_UNLOCKED;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockPromoteEnqueueIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If (SyncWord and FL_RWLOCK_MASK_VALID) <> 0 then
  begin
    // there has to be at least one reader, otherwise we cannot event think of queueing
    If (SyncWord and FL_RWLOCK_MASK_READCOUNTER) <> 0 then
      begin
        // fail if anyone is already awaiting lock promotion
        If (SyncWord and FL_RWLOCK_MASK_PROMOTING) = 0 then
          begin
            If ((SyncWord and FL_RWLOCK_MASK_LOCK) = 0) and ((SyncWord and FL_RWLOCK_MASK_READCOUNTER) = 1) then
              begin
                SyncWord := SyncWord or FL_RWLOCK_MASK_LOCK;
                Dec(SyncWord);  // remove promoted read lock
                Result := FL_RWLOCK_IOPRES_SUCCESS;
              end
            else If (SyncWord and FL_RWLOCK_MASK_WAITCOUNTER) < FL_RWLOCK_MASK_WAITCOUNTER then
              begin
                SyncWord := SyncWord + FL_RWLOCK_DELTA_WAIT;
                SyncWord := SyncWord or FL_RWLOCK_MASK_PROMOTING;
                Result := FL_RWLOCK_IOPRES_QUEUED;
              end
            else Result := FL_RWLOCK_IOPRES_WAITERS;
          end
        else Result := FL_RWLOCK_IOPRES_PROMOTING;
      end
    else Result := FL_RWLOCK_IOPRES_READERS;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockPromoteQueuedAcquireIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If ((SyncWord and FL_RWLOCK_MASK_VALID) <> 0) and
   ((SyncWord and FL_RWLOCK_MASK_PROMOTING) <> 0) and
   ((SyncWord and FL_RWLOCK_MASK_WAITCOUNTER) <> 0) then
  begin
    If ((SyncWord and FL_RWLOCK_MASK_LOCK) = 0) and ((SyncWord and FL_RWLOCK_MASK_READCOUNTER) = 1) then
      begin
        // lock, dequeue and remove promoted read lock
        SyncWord := SyncWord or FL_RWLOCK_MASK_LOCK;
        SyncWord := SyncWord - FL_RWLOCK_DELTA_WAIT;
        SyncWord := SyncWord and not FL_RWLOCK_MASK_PROMOTING;
        Dec(SyncWord);
        Result := FL_RWLOCK_IOPRES_SUCCESS;
      end
    else If (SyncWord and FL_RWLOCK_MASK_READCOUNTER) <> 1 then
      Result := FL_RWLOCK_IOPRES_READERS
    else
      Result := FL_RWLOCK_IOPRES_LOCKED;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//------------------------------------------------------------------------------

Function FastRWLockPromoteDequeueIOP(var SyncWord: TFLSyncWord): TFLSyncWord; register;
begin
If ((SyncWord and FL_RWLOCK_MASK_VALID) <> 0) and ((SyncWord and FL_RWLOCK_MASK_WAITCOUNTER) <> 0) then
  begin
    SyncWord := SyncWord - FL_RWLOCK_DELTA_WAIT;
    SyncWord := SyncWord and not FL_RWLOCK_MASK_PROMOTING;
    // do not increment read count, it was not decremented when queueing
    Result := FL_RWLOCK_IOPRES_SUCCESS;
  end
else Result := FL_RWLOCK_IOPRES_INVALID;
end;

//==============================================================================

Function FastRWLockReadEnqueue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): TFLQueueResult;
begin
{
  This will never return qrFailed simply because there is no read-wait counter
  and also no state that completely precludes waiting to read.
}
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastRWLockBeginReadIOP) of
  FL_RWLOCK_IOPRES_SUCCESS:   Result := qrAcquired;
  FL_RWLOCK_IOPRES_LOCKED,
  FL_RWLOCK_IOPRES_WAITERS,
  FL_RWLOCK_IOPRES_PROMOTING,
  FL_RWLOCK_IOPRES_READERS:   Result := qrQueued;
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockReadEnqueue: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastRWLockReadQueuedAcquire(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): Boolean;
begin
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastRWLockBeginReadIOP) of
  FL_RWLOCK_IOPRES_SUCCESS:   Result := True;
  FL_RWLOCK_IOPRES_LOCKED,
  FL_RWLOCK_IOPRES_WAITERS,
  FL_RWLOCK_IOPRES_PROMOTING,
  FL_RWLOCK_IOPRES_READERS:   Result := False;
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockReadQueuedAcquire: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

procedure FastRWLockReadDequeue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord);
begin
ConsumeArg(CallData);
// no read queue, no need to dequeue, but do sanity checks
If not IsValid(InterlockedLoad(SyncWord)) then
  raise EFLInvalidState.Create('FastRWLockReadDequeue: Invalid state of rw-lock sync word.');
end;

//------------------------------------------------------------------------------

Function FastRWLockWriteEnqueue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): TFLQueueResult;
begin
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastRWLockWriteEnqueueIOP) of
  FL_RWLOCK_IOPRES_SUCCESS: Result := qrAcquired;
  FL_RWLOCK_IOPRES_WAITERS: Result := qrFailed;
  FL_RWLOCK_IOPRES_QUEUED:  Result := qrQueued;
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockWriteEnqueue: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastRWLockWriteQueuedAcquire(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): Boolean;
begin
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastRWLockWriteQueuedAcquireIOP) of
  FL_RWLOCK_IOPRES_SUCCESS:   Result := True;
  FL_RWLOCK_IOPRES_LOCKED,
  FL_RWLOCK_IOPRES_PROMOTING,
  FL_RWLOCK_IOPRES_READERS:   Result := False;
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockWriteQueuedAcquire: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

procedure FastRWLockWriteDequeue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord);
begin
ConsumeArg(CallData);
If InterlockedOperation(SyncWord,FastRWLockWriteDequeueIOP) <> FL_RWLOCK_IOPRES_SUCCESS then
  raise EFLInvalidState.Create('FastRWLockWriteDequeue: Invalid state of rw-lock sync word.');
end;

//------------------------------------------------------------------------------

Function FastRWLockPromoteEnqueue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): TFLQueueResult;
begin
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastRWLockPromoteEnqueueIOP) of
  FL_RWLOCK_IOPRES_SUCCESS:   Result := qrAcquired; 
  FL_RWLOCK_IOPRES_WAITERS,                         // <- too many waiters
  FL_RWLOCK_IOPRES_READERS:   Result := qrFailed;   // <- no reader
  FL_RWLOCK_IOPRES_PROMOTING: Result := qrDeadlock; // <- someone else is waiting for lock promotion  
  FL_RWLOCK_IOPRES_QUEUED:    Result := qrQueued;
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockPromoteEnqueue: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastRWLockPromoteQueuedAcquire(var SyncWord: TFLSyncWord; CallData: TFLSyncWord): Boolean;
begin
ConsumeArg(CallData);
case InterlockedOperation(SyncWord,FastRWLockPromoteQueuedAcquireIOP) of
  FL_RWLOCK_IOPRES_SUCCESS: Result := True;
  FL_RWLOCK_IOPRES_LOCKED,
  FL_RWLOCK_IOPRES_READERS: Result := False;  // <- too many readers
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockPromoteQueuedAcquire: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

procedure FastRWLockPromoteDequeue(var SyncWord: TFLSyncWord; CallData: TFLSyncWord);
begin
ConsumeArg(CallData);
If InterlockedOperation(SyncWord,FastRWLockPromoteDequeueIOP) <> FL_RWLOCK_IOPRES_SUCCESS then
  raise EFLInvalidState.Create('FastRWLockPromoteDequeue: Invalid state of rw-lock sync word.');
end;

{===============================================================================
    Fast read-write lock - procedural interface implementation
===============================================================================}

procedure FastRWLockInit(out SyncWord: TFLSyncWord);
begin
InterlockedStore(TFLSyncWord((@SyncWord)^),TFLSyncWord(FL_INITVAL or FL_RWLOCK_MASK_VALID));
end;

//------------------------------------------------------------------------------

procedure FastRWLockFinal(var SyncWord: TFLSyncWord);
begin
InterlockedStore(SyncWord,FL_INITVAL);
end;

//------------------------------------------------------------------------------

Function FastRWLockBeginRead(var SyncWord: TFLSyncWord): Boolean;
begin
case InterlockedOperation(SyncWord,FastRWLockBeginReadIOP) of
  FL_RWLOCK_IOPRES_SUCCESS:   Result := True;
  FL_RWLOCK_IOPRES_LOCKED,
  FL_RWLOCK_IOPRES_WAITERS,
  FL_RWLOCK_IOPRES_PROMOTING,
  FL_RWLOCK_IOPRES_READERS:   Result := False;
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockBeginRead: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

procedure FastRWLockEndRead(var SyncWord: TFLSyncWord);
begin
If InterlockedOperation(SyncWord,FastRWLockEndReadIOP) <> FL_RWLOCK_IOPRES_SUCCESS then
  raise EFLInvalidState.Create('FastRWLockEndRead: Invalid state of rw-lock sync word.');
end;

//------------------------------------------------------------------------------

Function FastRWLockSpinToRead(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.SpinParams := SpinParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastRWLockReadEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastRWLockReadQueuedAcquire;
WaitParamsInternal.DequeueFce := FastRWLockReadDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteSpinning(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastRWLockSpinToRead(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult;
var
  SpinParams: TFLSpinParams;
begin
SpinParams := DefaultSpinParams;
SpinParams.SpinCount := SpinCount;
Result := FastRWLockSpinToRead(SyncWord,SpinParams);
end;

//------------------------------------------------------------------------------

Function FastRWLockWaitToRead(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult; 
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.WaitParams := WaitParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastRWLockReadEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastRWLockReadQueuedAcquire;
WaitParamsInternal.DequeueFce := FastRWLockReadDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteWaiting(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastRWLockWaitToRead(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult;
var
  WaitParams: TFLWaitParams;
begin
WaitParams := DefaultWaitParams;
WaitParams.Timeout := Timeout;
Result := FastRWLockWaitToRead(SyncWord,WaitParams);
end;

//------------------------------------------------------------------------------

Function FastRWLockBeginWrite(var SyncWord: TFLSyncWord): Boolean;
begin
case InterlockedOperation(SyncWord,FastRWLockBeginWriteIOP) of
  FL_RWLOCK_IOPRES_SUCCESS:   Result := True;
  FL_RWLOCK_IOPRES_LOCKED,
  FL_RWLOCK_IOPRES_WAITERS,
  FL_RWLOCK_IOPRES_PROMOTING,
  FL_RWLOCK_IOPRES_READERS:   Result := False;
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockBeginWrite: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

procedure FastRWLockEndWrite(var SyncWord: TFLSyncWord);
begin
If not IsValid(InterlockedAnd(SyncWord,not TFLSyncWord(FL_RWLock_MASK_LOCK))) then
  raise EFLInvalidState.Create('FastRWLockEndWrite: Invalid state of rw-lock sync word.');
end;

//------------------------------------------------------------------------------

Function FastRWLockSpinToWrite(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.SpinParams := SpinParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastRWLockWriteEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastRWLockWriteQueuedAcquire;
WaitParamsInternal.DequeueFce := FastRWLockWriteDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteSpinning(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastRWLockSpinToWrite(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult;
var
  SpinParams: TFLSpinParams;
begin
SpinParams := DefaultSpinParams;
SpinParams.SpinCount := SpinCount;
Result := FastRWLockSpinToWrite(SyncWord,SpinParams);
end;

//------------------------------------------------------------------------------

Function FastRWLockWaitToWrite(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.WaitParams := WaitParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastRWLockWriteEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastRWLockWriteQueuedAcquire;
WaitParamsInternal.DequeueFce := FastRWLockWriteDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteWaiting(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastRWLockWaitToWrite(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult;
var
  WaitParams: TFLWaitParams;
begin
WaitParams := DefaultWaitParams;
WaitParams.Timeout := Timeout;
Result := FastRWLockWaitToWrite(SyncWord,WaitParams);
end;

//------------------------------------------------------------------------------

Function FastRWLockPromote(var SyncWord: TFLSyncWord): TFLWaitResult;
begin
case InterlockedOperation(SyncWord,FastRWLockPromoteIOP) of
  FL_RWLOCK_IOPRES_SUCCESS:   Result := wrAcquired;
  FL_RWLOCK_IOPRES_LOCKED,
  FL_RWLOCK_IOPRES_WAITERS,
  FL_RWLOCK_IOPRES_READERS:   Result := wrTryAgain;
  FL_RWLOCK_IOPRES_PROMOTING: REsult := wrDeadlock;
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockPromote: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastRWLockDemote(var SyncWord: TFLSyncWord): Boolean;
begin
case InterlockedOperation(SyncWord,FastRWLockDemoteIOP) of
  FL_RWLOCK_IOPRES_SUCCESS:   Result := True;
  FL_RWLOCK_IOPRES_UNLOCKED:  Result := False;
else
 {FL_RWLOCK_IOPRES_INVALID}
  raise EFLInvalidState.Create('FastRWLockDemote: Invalid state of rw-lock sync word.');
end;
end;

//------------------------------------------------------------------------------

Function FastRWLockRecover(var SyncWord: TFLSyncWord): Boolean;
begin
FastRWLockEndRead(SyncWord);
Result := FastRWLockSpinToRead(SyncWord,INFINITE) = wrAcquired;
end;

//------------------------------------------------------------------------------

Function FastRWLockSpinToPromote(var SyncWord: TFLSyncWord; SpinParams: TFLSpinParams): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.SpinParams := SpinParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastRWLockPromoteEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastRWLockPromoteQueuedAcquire;
WaitParamsInternal.DequeueFce := FastRWLockPromoteDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteSpinning(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastRWLockSpinToPromote(var SyncWord: TFLSyncWord; SpinCount: UInt32): TFLWaitResult;
var
  SpinParams: TFLSpinParams;
begin
SpinParams := DefaultSpinParams;
SpinParams.SpinCount := SpinCount;
Result := FastRWLockSpinToPromote(SyncWord,SpinParams);
end;

//------------------------------------------------------------------------------

Function FastRWLockWaitToPromote(var SyncWord: TFLSyncWord; WaitParams: TFLWaitParams): TFLWaitResult;
var
  WaitParamsInternal: TFLWaitParamsInternal;
begin
WaitParamsInternal.PublicParams.WaitParams := WaitParams;
WaitParamsInternal.SyncWordPtr := Addr(SyncWord);
WaitParamsInternal.EnqueueFce := FastRWLockPromoteEnqueue;
WaitParamsInternal.QueuedAcquireFce := FastRWLockPromoteQueuedAcquire;
WaitParamsInternal.DequeueFce := FastRWLockPromoteDequeue;
WaitParamsInternal.CallData := 0;
Result := ExecuteWaiting(WaitParamsInternal);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FastRWLockWaitToPromote(var SyncWord: TFLSyncWord; Timeout: UInt32): TFLWaitResult;
var
  WaitParams: TFLWaitParams;
begin
WaitParams := DefaultWaitParams;
WaitParams.Timeout := Timeout;
Result := FastRWLockWaitToPromote(SyncWord,WaitParams);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TFastRWLock
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastRWLock - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFastRWLock - protected methods implementation
-------------------------------------------------------------------------------}

procedure TFastRWLock.SyncWordInit(const InitArgs: array of const);
begin
ConsumeArgs(InitArgs);
FastRWLockInit(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastRWLock.SyncWordFinal;
begin
FastRWLockFinal(fSyncWordPtr^);
end;

{-------------------------------------------------------------------------------
    TFastRWLock - protected methods implementation
-------------------------------------------------------------------------------}

constructor TFastRWLock.Create(var SyncWord: TFLSyncWord);
begin
CreateBase;
fMode := flmWrapper;
Initialize(@SyncWord,True,[]);
AcquireReference;
end;

//------------------------------------------------------------------------------

Function TFastRWLock.BeginRead: Boolean;
begin
Result := FastRWLockBeginRead(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastRWLock.EndRead;
begin
FastRWLockEndRead(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.SpinToRead(SpinCount: UInt32): TFLWaitResult;
var
  LocalSpinParams:  TFLSpinParams;
begin
LocalSpinParams := fSpinParams;
LocalSpinParams.SpinCount := SpinCount;
Result := FastRWLockSpinToRead(fSyncWordPtr^,LocalSpinParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastRWLock.SpinToRead: TFLWaitResult;
begin
Result := FastRWLockSpinToRead(fSyncWordPtr^,fSpinParams);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.WaitToRead(Timeout: UInt32): TFLWaitResult;
var
  LocalWaitParams:  TFLWaitParams;
begin
LocalWaitParams := fWaitParams;
LocalWaitParams.Timeout := Timeout;
Result := FastRWLockWaitToRead(fSyncWordPtr^,LocalWaitParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastRWLock.WaitToRead: TFLWaitResult;
begin
Result := FastRWLockWaitToRead(fSyncWordPtr^,fWaitParams);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.BeginWrite: Boolean;
begin
Result := FastRWLockBeginWrite(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastRWLock.EndWrite;
begin
FastRWLockEndWrite(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.SpinToWrite(SpinCount: UInt32): TFLWaitResult;
var
  LocalSpinParams:  TFLSpinParams;
begin
LocalSpinParams := fSpinParams;
LocalSpinParams.SpinCount := SpinCount;
Result := FastRWLockSpinToWrite(fSyncWordPtr^,LocalSpinParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastRWLock.SpinToWrite: TFLWaitResult;
begin
Result := FastRWLockSpinToWrite(fSyncWordPtr^,fSpinParams);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.WaitToWrite(Timeout: UInt32): TFLWaitResult;
var
  LocalWaitParams:  TFLWaitParams;
begin
LocalWaitParams := fWaitParams;
LocalWaitParams.Timeout := Timeout;
Result := FastRWLockWaitToWrite(fSyncWordPtr^,LocalWaitParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastRWLock.WaitToWrite: TFLWaitResult;
begin
Result := FastRWLockWaitToWrite(fSyncWordPtr^,fWaitParams);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.Promote: TFLWaitResult;
begin
Result := FastRWLockPromote(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.Demote: Boolean;
begin
Result := FastRWLockDemote(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.Recover: Boolean;
begin
Result := FastRWLockRecover(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.SpinToPromote(SpinCount: UInt32): TFLWaitResult;
var
  LocalSpinParams:  TFLSpinParams;
begin
LocalSpinParams := fSpinParams;
LocalSpinParams.SpinCount := SpinCount;
Result := FastRWLockSpinToPromote(fSyncWordPtr^,LocalSpinParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastRWLock.SpinToPromote: TFLWaitResult;
begin
Result := FastRWLockSpinToPromote(fSyncWordPtr^,fSpinParams);
end;

//------------------------------------------------------------------------------

Function TFastRWLock.WaitToPromote(Timeout: UInt32): TFLWaitResult;
var
  LocalWaitParams:  TFLWaitParams;
begin
LocalWaitParams := fWaitParams;
LocalWaitParams.Timeout := Timeout;
Result := FastRWLockWaitToPromote(fSyncWordPtr^,LocalWaitParams);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFastRWLock.WaitToPromote: TFLWaitResult;
begin
Result := FastRWLockWaitToPromote(fSyncWordPtr^,fWaitParams);
end;

end.


