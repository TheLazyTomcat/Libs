{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple futex

    Main aim of this library is to provide wrappers for futexes (synchronization
    primitives in Linux) and some very simple complete synchronization objects
    based on them - currently simple mutex, simple semaphore and simple robust
    mutex are implemented.

      NOTE - since proper implementation of futexes is not particularly easy,
             there are probably errors. If you find any, please let me know.

      WARNING - simple robust mutex might not be provided by this unit if
                dependecy library InterlockedOps does not provide functions
                accepting 64bit arguments (see there for details).
                Public constant SF_SRM_AVAILABLE can be used to probe whether
                SRM is or isn't provided (as it is a true constant, it can be
                used in conditional compilation).

  Version 1.2 (2024-08-23)

  Last change 2024-09-09

  ©2021-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleFutex

  Dependencies:
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions  - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol SimpleFutex_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit SimpleFutex;
{
  SimpleFutex_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  SimpleFutex_UseAuxExceptions to achieve this.
}
{$IF Defined(SimpleFutex_UseAuxExceptions)}
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
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

//------------------------------------------------------------------------------
{
  RobustMutexThreadTimeCheck

  Changes how the simple robust mutex (SRM) checks whether thread that locked
  the SRM still lives - see description of SRM for more details and explanation
  of effects this symbol has.

  Has meaning only if simple robust mutexes are provided.

    WARNING - libraries compiled with and without this symbol defined are
              mutually completely incompatible (the SRMs, that is).

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol SimpleFutex_RobustMutexThreadTimeCheck_ON.
}
{$UNDEF RobustMutexThreadTimeCheck}
{$IFDEF SimpleFutex_RobustMutexThreadTimeCheck_ON}
  {$DEFINE RobustMutexThreadTimeCheck}
{$ENDIF}

interface

uses
  SysUtils, UnixType,
  AuxTypes, AuxClasses, InterlockedOps
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{$IF ILO_64BIT_VARS}  // constant from InterlockedOps
  {$DEFINE SF_SimpleRobustMutex}
{$ELSE}
  {$UNDEF SF_SimpleRobustMutex}
{$IFEND}

{$IFDEF RobustMutexThreadTimeCheck}
  {$DEFINE SF_SRM_TimeCheck}
{$ELSE}
  {$UNDEF SF_SRM_TimeCheck}
{$ENDIF}

{===============================================================================
    Informative public constants
===============================================================================}
const
  SF_SRM_AVAILABLE = {$IFDEF SF_SimpleRobustMutex}True{$ELSE}False{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESFException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ESFTimeError    = class(ESFException);
  ESFFutexError   = class(ESFException);
  ESFInvalidValue = class(ESFException);
  ESFSignalError  = class(ESFException);
  ESFParsingError = class(ESFException);

{===============================================================================
--------------------------------------------------------------------------------
                                 Futex wrappers
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Futex wrappers - constants and types
===============================================================================}
const
  INFINITE = UInt32(-1);  // infinite timeout

  FUTEX_BITSET_MATCH_ANY = UInt32($FFFFFFFF); // the name says it all :P

type
  TFutexWord = cInt;
  PFutexWord = ^TFutexWord;

  // used only for casting to unsigned counter in simple semaphore
  TFutexUWord = cUInt;
  PFutexUWord = ^TFutexUWord;

  TFutex = TFutexWord;
  PFutex = ^TFutex;

//------------------------------------------------------------------------------
{
  Values returned from waiting on futex.

    fwrWoken       - the waiting was ended by FUTEX_WAKE

    fwrValue       - value of futex did not match parameter Value at the time
                     of the call

    fwrTimeout     - waiting timed out

    fwrInterrupted - waiting was interrupted (eg. by a signal)
}
type
  TFutexWaitResult = (fwrWoken,fwrValue,fwrTimeout,fwrInterrupted);

//------------------------------------------------------------------------------

type
  TFutexOperation = (fopSet,fopAdd,fopOR,forANDN,fopXOR);

  TFutexComparison = (focEqual,focNotEqual,focLess,focLessOrEqual,focGreater,
                      focGreaterOrEqual);

{===============================================================================
    Futex wrappers - declaration
===============================================================================}
{
  FutexWait

  Waits on futex until the thread is woken by FUTEX_WAKE, signal, spurious
  wakeup or the timeout period elapses.

    If the parameter Value does not match content of the futex at the time of
    call, the function returns immediately, returning fwrValue.

    Setting Private to true indicates that the futex is used only withing
    current process and allows system to do some optimizations.

    When Realtime is true, the timing will use CLOCK_REALTIME clock instead of
    CLOCK_MONOTONIC. Supported only from Linux 4.5 up.

    What caused the function to return is indicated by returned value.

      WARNING - even when the function returns fwrWoken, it does not
                necessarily mean the waiter was explicitly woken,
                always consider it being a spurious wakeup.
}
Function FutexWait(var Futex: TFutexWord; Value: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;

{
  FutexWaitNoInt

  Behaves the same as FutexWait, but it will never return fwrInterrupted.
  If the waiting is ended by a cause that falls into that category, the
  function recalculates timeout in relation to already elapsed time and
  re-enters waiting.

    WARNING - do not use if the waiting can be requeued to a different futex.
              If the waiting is requeued and then is interrupted, this call
              will re-enter waiting on the original futex, not on the one
              to which it was requeued.
}
Function FutexWaitNoInt(var Futex: TFutexWord; Value: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;

{
  FutexWake

  Wakes at least one and at most Count threads waiting on the given futex.

    If passed Count is negative, it will try to wake MAXINT (2147483647)
    threads.

    Private indicates whether the futex is used only withing the current
    process.

    Returs number of woken waiters.
}
Function FutexWake(var Futex: TFutexWord; Count: Integer; Private: Boolean = False): Integer;

{
  FutexFD

  Creates a file descriptor that is associated with the futex.

    Value is used for asynchronous notification via signals (value stored here,
    when non-zero, denotes the signal number), for details refer to futex
    documentation.

    Private indicates whether the futex is used only withing the current
    process.

    Returs created file descriptor.

    WARNING - support for this call was removed in linux 2.6.26.
}
Function FutexFD(var Futex: TFutexWord; Value: Integer; Private: Boolean = False): Integer;

{
  FutexRequeue

  Requeues waiting from one futex (Futex) to another (Futex2).
  Please refer to futex documentation for detailed description of requeue
  operation.

    WakeCount is maximum number of woken waiters (any negative value is
    translated to MAXINT).

    RequeueCount is maximum number of requeued waiters (any negative value is
    translated to MAXINT).

    Private indicates whether the futex is used only withing the current
    process.

    Returns number of woken waiters.
}
Function FutexRequeue(var Futex: TFutexWord; var Futex2: TFutexWord; WakeCount,RequeueCount: Integer; Private: Boolean = False): Integer;

{
  FutexCmpRequeue

  Works the same as FutexRequeue, but it first checks whether Futex contains
  value passed in parameter Value. If not, then the function will immediately
  exit, returning a negative value.
  For detailed description of compare-requeue operation, refer to futex
  documentation.

    WakeCount gives maximum number of woken waiters (any negative value is
    translated to MAXINT).

    RequeueCount is maximum number of requeued waiters (any negative value is
    translated to MAXINT).

    Private indicates whether the futex is used only withing the current
    process.

    When FutexCmpRequeue returns any negative number, it indicates that value
    of Futex variable did not match Value parameter at the time of call.
    Otherwise it returns a sum of woken and requeued waiters.
}
Function FutexCmpRequeue(var Futex: TFutexWord; Value: TFutexWord; var Futex2: TFutexWord; WakeCount,RequeueCount: Integer; Private: Boolean = False): Integer;

{
  FutexWakeOp

  Executes the following sequence atomically and totally ordered in respect to
  other futex operations on any of the two supplied futex words:

      uint32_t oldval = *(uint32_t *) uaddr2;
      *(uint32_t *) uaddr2 = oldval op oparg;
      futex(uaddr, FUTEX_WAKE, val, 0, 0, 0);
      if (oldval cmp cmparg)
        futex(uaddr2, FUTEX_WAKE, val2, 0, 0, 0);

  ...or, in local nomenclature:

      _OldVal := Futex2;
      Futex2 := _OldVal Op OpArg;
      FutexWake(Futex,Count);
      If _OldVal Cmp CmpArg then
        FutexWake(Futex2,Count2);

  For more details, refer to documentation of futexes (FUTEX_WAKE_OP).

    If passed Count or Count2 is negative, it will be translated to MAXINT
    (2147483647).

    Private indicates whether the futexes are used only withing the current
    process.

    Returns a sum of woken waiters from both futexes.
}
Function FutexWakeOp(var Futex: TFutexWord; Count: Integer; var Futex2: TFutexWord; Count2: Integer;
                     Op: TFutexOperation; OpArg: Integer; Cmp: TFutexComparison; CmpArg: Integer;
                     OpArgShift: Boolean = False; Private: Boolean = False): Integer;

{
  FutexWaitBitSet

  Works the same as FutexWait, but the waiter has a bitmask stored in its
  in-kernel state.

    NOTE - the BitMask must not be zero.
}
Function FutexWaitBitSet(var Futex: TFutexWord; Value: TFutexWord; BitMask: UInt32; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;

{
  FutexWaitBitSet

  Works the same as FutexWaitBitSet, but it will never return fwrInterrupted
  (see FutexWaitNoInt).

    NOTE - the BitMask must not be zero.

    WARNING - do not use if the waiting can be requeued to a different futex.
}
Function FutexWaitBitSetNoInt(var Futex: TFutexWord; Value: TFutexWord; BitMask: UInt32; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;

{
  FutexWakeBitSet

  Works the same as FutexWake, but it wakes only waiters that were added to
  waiting with a bitmask that has at least one set bit common with BitMask
  parameter passed in here (ie. bit-wise AND of BitMask and mask passed to
  FutexWaitBitSet(NoInt) must produce non-zero result).
  If you set BitMask to FUTEX_BITSET_MATCH_ANY (all bits set), then the call is
  equivalent to FutexWake.

    NOTE - the BitMask must not be zero.
}
Function FutexWakeBitSet(var Futex: TFutexWord; Count: Integer; BitMask: UInt32 = FUTEX_BITSET_MATCH_ANY; Private: Boolean = False): Integer;

{
  Priority-inheritance futexes

  For descriptions of PI operations, refer to official documentation of futexes.

    NOTE - if FutexLockPI(2) returns fwrValue, you should try the lock again
           (it indicates that the owner of lock is about to exit but has not
           yet cleared the internal state).
}
Function FutexLockPI(var Futex: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False): TFutexWaitResult;
Function FutexLockPI2(var Futex: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;

Function FutexTryLockPI(var Futex: TFutexWord; Private: Boolean = False): Boolean;

Function FutexUnlockPI(var Futex: TFutexWord; Private: Boolean = False): Boolean;

Function FutexCmpRequeuePI(var Futex: TFutexWord; Value: TFutexWord; var Futex2: TFutexWord; RequeueCount: Integer; Private: Boolean = False): Integer;

Function FutexWaitRequeuePI(var Futex: TFutexWord; Value: TFutexWord; var Futex2: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;

{===============================================================================
--------------------------------------------------------------------------------
                                  Simple mutex
--------------------------------------------------------------------------------
===============================================================================}
{
  Simple mutex (SM) behaves like a basic mutex or critical section - only one
  thread can lock it and no other thread can lock it again until it is unlocked.

  If the SM is locked, the SimpleMutexLock function will block until the SM is
  unlocked by other thread.

  A call to SimpleMutexLock must be paired with SimpleMutexUnlock.

  Calling SimpleMutexUnlock on an unlocked SM is allowed.

    WARNING - Simple mutex is not recursive. Calling SimpleMutexLock on a
              locked mutex in the same thread will block indefinitely, creating
              a deadlock.

    WARNING - Simple mutex is not robust. If a thread fails to unlock the SM,
              it will stay locked indefinitely (but note that it can be
              unlocked by any thread - SM is not a classical mutex with thread
              ownership).

    NOTE - Simple mutex does not neeed to be explicitly initialized if it is
           set to all zero by other means (eg. memory initialization).
}
{===============================================================================
    Simple Mutex - declaration
===============================================================================}

procedure SimpleMutexInit(out Futex: TFutexWord);

procedure SimpleMutexLock(var Futex: TFutexWord);
procedure SimpleMutexUnlock(var Futex: TFutexWord);

{===============================================================================
--------------------------------------------------------------------------------
                                Simple semaphore
--------------------------------------------------------------------------------
===============================================================================}
{
  Only very basic implementation of semaphore (counter synchronizer) is
  provided.

  If count is greater than zero, it is signaled (unlocked). If zero, it is
  non-signaled (locked).

  SimpleSemaphoreWait decrements (with unsigned saturation) the count. If the
  count was zero before the call, it will enter waiting and blocks until the
  semaphore counter becomes positive again trough a call to SimpleSemaphorePost.

  SimpleSemaphorePost increments (with unsigned saturation) the count and tries
  to wake as many waiters as is the current count.

  Simple semaphore does not need to be initialized explicitly - it is enough
  to set it to any integer or zero, which can be done eg. through memory
  initialization.
}
{===============================================================================
    Simple semaphore - declaration
===============================================================================}

procedure SimpleSemaphoreInit(out Futex: TFutexWord; InitialCount: UInt32);

procedure SimpleSemaphoreWait(var Futex: TFutexWord);
procedure SimpleSemaphorePost(var Futex: TFutexWord);

{$IFDEF SF_SimpleRobustMutex}
{===============================================================================
--------------------------------------------------------------------------------
                               Simple robust mutex
--------------------------------------------------------------------------------
===============================================================================}
{
  Simple robust mutex (SRM) behaves like simple mutex declared above - only one
  thread can lock it and no other thread can lock it again until it is unlocked.
  But unlike simple mutex, this object is quasi-robust (full robustness would
  be provided by the OS, here it is not - see further). That means it can be
  locked again if thread that locked it previously exits without unlocking it.

  If the SRM is locked, the SimpleRobustMutexLock function will block until the
  SRM is unlocked by other thread or the thread that locked it previously exits
  (by any means - it can end, crash, exit, be terminated, whatever causes it to
  cease to exist). If the SimpleRobustMutexLock returns, the mutex is guaranteed
  to be locked for use by the calling thread.

  A call to SimpleRobustMutexLock must be paired with SimpleRobustMutexUnlock.

  Calling SimpleRobustMutexUnlock on an already unlocked SRM is allowed and
  does not pose any problem.

  SimpleRobustMutexInit initializes the mutex to an unlocked state.

    WARNING - SRM is not recursive. Calling SimpleRobustMutexLock on a locked
              mutex in the same thread will block indefinitely, creating a
              deadlock.

    NOTE - Simple robust mutex does not neeed to be explicitly initialized if
           it is set to all zero by other means (eg. memory initialization),
           but, in such a case, calling memory fencing instruction is highly
           recommended (you can use eg. function ReadWriteBarrier from library
           InterlockedOps - function SimpleRobustMutexInit calls this function
           automatically).

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  And now few words on the robustness...

  As mentioned previously, this object is not fully robust - full robustness
  would necessitate a cooperation with operating system, and unfortunatelly
  there is no way I am aware of to achieve this.
  Note that I know about robust futex lists provided by Linux (functions
  get_robust_list and set_robust_list and things around it), but the way they
  are implemented precludes them from being used by multiple libraries. And
  since glibc already uses them for posix mutexes, we cannot use them here.

  So, to emulate the robustness, current implementation is following:

    Initialy, the internal state of the SRM is set to all zero, which indicates
    that the mutex is unlocked.

    When any thread calls a locking function (that is, any overload of function
    SimpleRobustMutexLock), and the SRM is unlocked by that point, the calling
    thread stores some information about itself into the state, marking it as
    locked, and returns immediately.

    If SRM is already locked when another thread tries to lock it, then this
    thread enters a waiting - note that unlike in simple mutex, this waiting is
    semi-active.
    The waiting thread is entering passive waiting on futex, but is periodically
    awakened to check whether thread that previously locked the mutex still
    exists within the system. Length of this periode can be changed by setting
    CheckInterval parameter (in milliseconds). Method used to discern whether
    the thread still lives depends on symbol RobustMutexThreadTimeCheck and
    value of parameter CheckMethod (see further).
    This semi-active waiting is necessary because when a lock-holding thread
    dies, other threads that could be in infinite passive waiting would NOT be
    awakened, creating a deadlock.
    When waiter finds that lock-holding thread does not exist, it relocks the
    mutex for itself and normally returns.

    Methods used to check existence of lock-holding thread are following (note
    that libraries compiled with and without the RobustMutexThreadTimeCheck
    symbol defined are mutually incompatible):

    > Symbol RobustMutexThreadTimeCheck defined

        Parameter CheckMethod is ignored.

        The SRM state contains locking thread's ID and its time of creation
        (lower 32bits - see notes in implementation for details), as stored
        in file "/proc/[tid]/stat".

        First, it is checked whether the file "/proc/[tid]/stat" actually
        exists. If it doesn't, it is assumed the thread does not live anymore.
        When it does exist, it is opened and the creation time is parsed from
        it. This obtained time is then checked against a value stored in the
        SRM state. If they match, then the lock-holding thread still lives and
        waiter enters passive period before checking again. If they do not
        match, then it is assumed the file belongs to other thread than the one
        that locked the mutex (it just have the same TID) and the original
        locker is dead.

        If the stat file cannot be opened (raises an exception), but it still
        exists, then this situation is counted towards a consecutive failure
        count.
        Parameter MaxConsecFailCount limits how many times this can happen in a
        row before the exception is re-raised and the locking function crashes
        with it. Whenever the file is successfully accessed, this count is
        reset to zero.
        This is here to protect against situation where the stat file is only
        temporarily inaccessible.

        This check is slightly more reliable than the following ones, but it
        does depend on stat file being accessible, and also is slightly slower
        than other methods.

    > Symbol RobustMutexThreadTimeCheck not defined

        WARNING - all methods in this group assume that IDs are not frequently
                  or even readily reused for new threads, and that situation
                  where thread of specific ID is running within a process of
                  specific ID is extremely rare and unlikely to happen
                  multiple-times on short timescales.

        Here, the SRM state stores locking thread's ID and ID of process this
        thread is running within.

        Parameter CheckMethod is used to select one of the following methods:

        > tcmDefault, tcmSignal

            Existence of a thread is checked by trying to send a signal to that
            thread using system call kill(tid,0) (this in fact does not send
            anything, it just checks recipient existence).
            If this call succeeds, then it is assumed the thread of given ID
            (tid) exists. If the call fails with error ESRCH, it is assumed the
            thread does not exist. In case of failure with other error, an
            exception of class ESFSignalError is raised.

            But checking that the thread exists is not enough to be sure it is
            the thread that made the lock. Therefore, the abovementioned
            algorith is first used to check whether the locking thread (thread
            ID) exists, then whether its parent process exists (process ID)
            and, if both do exist, whether the given process is really a parent
            process of that thread.

            Only if all this holds true, then the locking thread is asumed to
            be still alive, othervise it is presumed dead.

        > tcmProc

            This method is simple - a check is made whether directory
            "/proc/[pid]/task/[tid]" (tid = ID if locking thread, pid = ID of
            the locking thread's parent process) exists or not. When it does,
            the thread exists, when it doesn't, the locking thread is presumed
            dead.

            This method of course rely on accessibility of mentioned directory.
}
{===============================================================================
    Simple robust mutex - declaration
===============================================================================}
type
  TSimpleRobustMutexState = record
    case Integer of
      0: (FullWidth:    UInt64);
      1: (ThreadID:     pid_t;
          ProcessID:    pid_t);
      2: (FutexWord:    TFutexWord;
          ThreadCTime:  UInt32);  // creation time of the locking thread
  end;
  PSimpleRobustMutexState = ^TSimpleRobustMutexState;

{$IF SizeOf(TSimpleRobustMutexState) <> 8} // just a slight paranoia
  {$MESSAGE FATAL 'Invalid size of simple robust mutex record.'}
{$IFEND}

//------------------------------------------------------------------------------
type
  // tcmDefault is equivalent to tcmSignal
  TThreadCheckMethod = (tcmDefault,tcmSignal,tcmProc);

{
  TSimpleRobustMutexData

  This structure is used to pass data in and out of SimpleRobustMutexLock call.

  WARNING - this structure can be changed in the future, and therefore using
            it and its overload of SimpleRobustMutexLock is not recommended.

    CheckInterval (in)      - Number of milliseconds to wait between checks
                              whether the thread that previously locked the
                              mutex we are trying to acquire still lives.

    CheckMethod (in,out)    - Method that should be used to check whether the
                              locking thread still lives.
                              When symbol RobustMutexThreadTimeCheck is defined,
                              then it will be always changed to tcmDefault upon
                              return from the locking call, irrespective of its
                              previous value. If this symbol is not defined,
                              then it will not change during the call.

    CheckCount (out)        - Indicates how many times the call checked whether
                              the locking thread still lives. It will be zero
                              if the SRM was unlocked and immediately acquired.

    FailCount (out)         - Contains total number of failures to obtain
                              locking thread creation time.
                              Used only when RobustMutexThreadTimeCheck is
                              defined, otherwise it will always be zero upon
                              return.

    MaxConsecFailCount (in) - Maximum number of consecutive failures to read
                              lock-holding thread creation time before an
                              exception is raised.
                              Used only when RobustMutexThreadTimeCheck is
                              defined, otherwise it is ignored.

    ConsecFailCount (out)   - Number of consecutive failures to obtain thread
                              creation time in the last failure sequence.
                              This field is used only internally. It is reset
                              everytime a successful read is done, therefore it
                              will usually contain zero.
                              Used only when RobustMutexThreadTimeCheck is
                              defined.

  If some details in field descriptions are not clear, then refer higher to
  description of SRM itself, part where robustness is talked about.
}
  TSimpleRobustMutexData = record
    CheckInterval:      UInt32;
    CheckMethod:        TThreadCheckMethod;
    CheckCount:         Integer;
    FailCount:          Integer;
    MaxConsecFailCount: Integer;
    ConsecFailCount:    Integer;
  end;

//------------------------------------------------------------------------------

procedure SimpleRobustMutexInit(out RobustMutex: TSimpleRobustMutexState);

{
  The first overload of SimpleRobustMutexLock (the one with Data parameter)
  is intended for debugging. You can use it if you wish, but it is generally
  not recommended as the type TSimpleRobustMutexData can change in the future.
}
procedure SimpleRobustMutexLock(var RobustMutex: TSimpleRobustMutexState; var Data: TSimpleRobustMutexData); overload;
procedure SimpleRobustMutexLock(var RobustMutex: TSimpleRobustMutexState; CheckInterval: UInt32 = 100; CheckMethod: TThreadCheckMethod = tcmDefault); overload;
procedure SimpleRobustMutexLock(var RobustMutex: TSimpleRobustMutexState; CheckMethod: TThreadCheckMethod); overload;

procedure SimpleRobustMutexUnlock(var RobustMutex: TSimpleRobustMutexState);

{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleSynchronizer
--------------------------------------------------------------------------------
===============================================================================}
{
  Common ancestor class for wrappers around implemented simple synchronization
  primitives.

  Instance can either be created as standalone or as shared.

    Standalone instance is created by constructor that does not expect an
    external state variable. The state is completely internal and is managed
    automatically. To properly use it, create one instance and use this one
    object in all synchronizing threads.

    Shared instace expects pre-existing state variable (be it futex word for
    simple mutex and simple semaphore, or TSimpleRobustMutexState variable
    for simple robust mutex) to be passed to the constructor. This state is
    then used for locking. To use this mode, allocate a state variable and
    create new instance from this one state for each synchronizing thread.
    Note that you are responsible for state management (initialization,
    finalization) - you can use methods Init and Final to do so if you do not
    want to use procedural interface (note that these methods will always
    initialize and finalize the state whether it is in a shared instance or
    standalone instance).
}
type
{$IFDEF SF_SimpleRobustMutex}
  TSimpleSynchronizerState = array[0..Pred(SizeOf(TSimpleRobustMutexState))] of Byte;
{$ELSE}
  TSimpleSynchronizerState = array[0..Pred(SizeOf(TFutexWord))] of Byte;
{$ENDIF}
  PSimpleSynchronizerState = ^TSimpleSynchronizerState;

{===============================================================================
    TSimpleSynchronizer - class declaration
===============================================================================}
type
  TSimpleSynchronizer = class(TCustomObject)
  protected
    fLocalState:  TSimpleSynchronizerState;
    fStatePtr:    PSimpleSynchronizerState;
    fOwnsState:   Boolean;
    procedure Initialize(StatePtr: PSimpleSynchronizerState); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(var Futex: TFutexWord); overload; virtual;
  {$IFDEF SF_SimpleRobustMutex}
    constructor Create(var SimpleRobustMutexState: TSimpleRobustMutexState); overload; virtual;
  {$ENDIF}
    constructor Create; overload; virtual;
    destructor Destroy; override;
    procedure Init; virtual;
    procedure Final; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TSimpleMutex
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleMutex - class declaration
===============================================================================}
type
  TSimpleMutex = class(TSimpleSynchronizer)
  protected
    procedure Initialize(StatePtr: PSimpleSynchronizerState); override;
  public
    procedure Init; override;
    procedure Enter; virtual;
    procedure Leave; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleSemaphore
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleSemaphore - class declaration
===============================================================================}
type
  TSimpleSemaphore = class(TSimpleSynchronizer)
  protected
    fInitialCount:  UInt32;
    procedure Initialize(StatePtr: PSimpleSynchronizerState); override;
  public
    constructor CreateAndInitCount(InitialCount: UInt32); overload; virtual;
    procedure Init; override; // initialzes to a value of property InitialCount
    procedure Acquire; virtual;
    procedure Release; virtual;
  {
    Note that if this object is created using constructors without InitialCount
    argument (ie. initial count is not explicitly given during construction),
    then property InitialCount is set to a value present in the semaphore's
    state during the construction, which might not be a value you expect.
  }
    property InitialCount: UInt32 read fInitialCount write fInitialCount;
  end;

{$IFDEF SF_SimpleRobustMutex}
{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleRobustMutex
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRobustMutex - class declaration
===============================================================================}
type
  TSimpleRobustMutex = class(TSimpleSynchronizer)
  protected
    procedure Initialize(StatePtr: PSimpleSynchronizerState); override;
  public
    procedure Init; override;
    procedure Enter; virtual;
    procedure Leave; virtual;
  end;
{$ENDIF}

implementation

uses
  BaseUnix, Linux, Errors{$IFDEF SF_SimpleRobustMutex}, Syscall{$ENDIF},
  Classes, Math;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

{===============================================================================
    Futex system constants
===============================================================================}
const
  FUTEX_WAIT            = 0;
  FUTEX_WAKE            = 1;
  FUTEX_FD              = 2;  // removed in Linux 2.6.26
  FUTEX_REQUEUE         = 3;
  FUTEX_CMP_REQUEUE     = 4;
  FUTEX_WAKE_OP         = 5;
  FUTEX_LOCK_PI         = 6;
  FUTEX_UNLOCK_PI       = 7;
  FUTEX_TRYLOCK_PI      = 8;
  FUTEX_WAIT_BITSET     = 9;
  FUTEX_WAKE_BITSET     = 10;
  FUTEX_WAIT_REQUEUE_PI = 11;
  FUTEX_CMP_REQUEUE_PI  = 12;
  FUTEX_LOCK_PI2        = 13; // since Linux 5.14

  FUTEX_PRIVATE_FLAG   = 128;
  FUTEX_CLOCK_REALTIME = 256;

  FUTEX_OP_SET  = 0;  // uaddr2 = oparg
  FUTEX_OP_ADD  = 1;  // uaddr2 += oparg
  FUTEX_OP_OR   = 2;  // uaddr2 |= oparg
  FUTEX_OP_ANDN = 3;  // uaddr2 &= ~oparg
  FUTEX_OP_XOR  = 4;  // uaddr2 ^= oparg

  FUTEX_OP_ARG_SHIFT = 8; // use (1 << oparg) as operand

  FUTEX_OP_CMP_EQ = 0;  // if (oldval == cmparg) wake
  FUTEX_OP_CMP_NE = 1;  // if (oldval != cmparg) wake
  FUTEX_OP_CMP_LT = 2;  // if (oldval < cmparg) wake
  FUTEX_OP_CMP_LE = 3;  // if (oldval <= cmparg) wake
  FUTEX_OP_CMP_GT = 4;  // if (oldval > cmparg) wake
  FUTEX_OP_CMP_GE = 5;  // if (oldval >= cmparg) wake


{===============================================================================
--------------------------------------------------------------------------------
                                 Futex wrappers
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Futex wrappers - internals
===============================================================================}
const
  MSECS_PER_SEC  = 1000;
  NSECS_PER_SEC  = 1000000000;
  NSECS_PER_MSEC = 1000000;

//------------------------------------------------------------------------------

Function FutexOp(Op: cInt; Private: Boolean; Realtime: Boolean = False): cInt;
begin
Result := Op;
If Private then
  Result := Result or FUTEX_PRIVATE_FLAG;
If Realtime then
  Result := Result or FUTEX_CLOCK_REALTIME;
end;

//------------------------------------------------------------------------------

Function RectCount(Count: Integer): cInt;
begin
If Count < 0 then
  Result := cInt(MAXINT)
else
  Result := cInt(Count);
end;

//------------------------------------------------------------------------------

Function RectCountPtr(Count: Integer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Count < 0 then
  Result := Pointer(PtrInt(MAXINT))
else
  Result := Pointer(PtrInt(cInt(Count)));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure GetTime(out Time: TTimeSpec; Realtime: Boolean);
var
  CallRes:      cInt;
  ErrorNumber:  cInt;
begin
If Realtime then
  CallRes := clock_gettime(CLOCK_REALTIME,@Time)
else
  CallRes := clock_gettime(CLOCK_MONOTONIC,@Time);
If CallRes <> 0 then
  begin
    ErrorNumber := errno;
    raise ESFTimeError.CreateFmt('FutexWait.GetTime: Unable to obtain time (%d - %s).',
      [ErrorNumber,StrError(ErrorNumber)]);
  end;
end;

//------------------------------------------------------------------------------

procedure TranslateTimeout(Timeout: UInt32; out TimeoutSpec: TTimeSpec; AbsoluteTime,Realtime: Boolean);
begin
If AbsoluteTime then
  begin
    GetTime(TimeoutSpec,Realtime);
    TimeoutSpec.tv_sec := TimeoutSpec.tv_sec + time_t(Timeout div MSECS_PER_SEC);
    TimeoutSpec.tv_nsec := TimeoutSpec.tv_nsec + clong((Timeout mod MSECS_PER_SEC) * NSECS_PER_MSEC);
    while TimeoutSpec.tv_nsec >= NSECS_PER_SEC do
      begin
        TimeoutSpec.tv_nsec := TimeoutSpec.tv_nsec - NSECS_PER_SEC;
        Inc(TimeoutSpec.tv_sec);
      end;
  end
else
  begin
    TimeoutSpec.tv_sec := Timeout div MSECS_PER_SEC;
    TimeoutSpec.tv_nsec := (Timeout mod MSECS_PER_SEC) * NSECS_PER_MSEC;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function GetElapsedMillis(From: TTimeSpec; Realtime: Boolean): UInt32;
var
  CurrentTime:  TTimeSpec;
  Temp:         Int64;
begin
GetTime(CurrentTime,Realtime);
If CurrentTime.tv_sec >= From.tv_sec then // sanity check
  begin
    Temp := (((Int64(CurrentTime.tv_sec) - From.tv_sec) * MSECS_PER_SEC) +
             ((Int64(CurrentTime.tv_nsec) - From.tv_nsec) div NSECS_PER_MSEC)) and
            (Int64(-1) shr 1);
    If Temp < INFINITE then
      Result := UInt32(Temp)
    else
      Result := INFINITE;
  end
else Result := INFINITE;  // time overflowed
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

{===============================================================================
    Futex wrappers - implementation
===============================================================================}

Function FutexWait(var Futex: TFutexWord; Value: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;
var
  ResVal:       cInt;
  TimeoutSpec:  TTimeSpec;
  ErrorNumber:  cInt;
begin
If Timeout <> INFINITE then
  begin
    TranslateTimeout(Timeout,TimeoutSpec,False,Realtime);
    ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_WAIT,Private,Realtime),Value,@TimeoutSpec);
  end
else ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_WAIT,Private,Realtime),Value,nil);
If ResVal <> 0 then
  begin
    // an error occurred
    ErrorNumber := errno;
    case ErrorNumber of
      ESysEWOULDBLOCK:  Result := fwrValue;
      ESysETIMEDOUT:    Result := fwrTimeout;
      ESysEINTR:        Result := fwrInterrupted;
    else
      // some other error
      raise ESFFutexError.CreateFmt('FutexWait: Wait failed (%d - %s).',
        [ErrorNumber,StrError(ErrorNumber)]);
    end;
  end
else Result := fwrWoken;
end;

//------------------------------------------------------------------------------

Function FutexWaitNoInt(var Futex: TFutexWord; Value: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;
var
  StartTime:        TTimeSpec;
  TimeoutRemaining: UInt32;
  ElapsedMillis:    UInt32;
begin
GetTime(StartTime,Realtime);
TimeoutRemaining := Timeout;
while True do
  begin
    Result := FutexWait(Futex,Value,TimeoutRemaining,Private,Realtime);
    If Result = fwrInterrupted then
      begin
        // no need to recalculate timeout if we are waiting for infinite period
        If Timeout <> INFINITE then
          begin
            // recalculate timeout, ignore time spent in recalculation
            ElapsedMillis := GetElapsedMillis(StartTime,Realtime);
            If Timeout <= ElapsedMillis then
              begin
                Result := fwrTimeout;
                Break{while};
              end
            else TimeoutRemaining := Timeout - ElapsedMillis;
          end;
      end
    else Break{while};
  end;
end;

//------------------------------------------------------------------------------

Function FutexWake(var Futex: TFutexWord; Count: Integer; Private: Boolean = False): Integer;
var
  ErrorNumber:  cInt;
begin
Result := Integer(Linux.Futex(@Futex,FutexOp(FUTEX_WAKE,Private),RectCount(Count),nil));
If Result = -1 then
  begin
    ErrorNumber := errno;
    raise ESFFutexError.CreateFmt('FutexWake: Waking failed (%d - %s).',
      [ErrorNumber,StrError(ErrorNumber)]);
  end;
end;

//------------------------------------------------------------------------------

Function FutexFD(var Futex: TFutexWord; Value: Integer; Private: Boolean = False): Integer;
var
  ErrorNumber:  cInt;
begin
Result := Integer(Linux.Futex(@Futex,FutexOp(FUTEX_FD,Private),cInt(Value),nil));
If Result = -1 then
  begin
    ErrorNumber := errno;
    raise ESFFutexError.CreateFmt('FutexFD: File descriptor creation failed (%d - %s).',
      [ErrorNumber,StrError(ErrorNumber)]);
  end;
end;

//------------------------------------------------------------------------------

Function FutexRequeue(var Futex: TFutexWord; var Futex2: TFutexWord; WakeCount,RequeueCount: Integer; Private: Boolean = False): Integer;
var
  ErrorNumber:  cInt;
begin
Result := Integer(Linux.Futex(@Futex,FutexOp(FUTEX_REQUEUE,Private),RectCount(WakeCount),RectCountPtr(RequeueCount),@Futex2,0));
If Result = -1 then
  begin
    ErrorNumber := errno;
    raise ESFFutexError.CreateFmt('FutexRequeue: Requeue failed (%d - %s).',
      [ErrorNumber,StrError(ErrorNumber)]);
  end;
end;

//------------------------------------------------------------------------------

Function FutexCmpRequeue(var Futex: TFutexWord; Value: TFutexWord; var Futex2: TFutexWord; WakeCount,RequeueCount: Integer; Private: Boolean = False): Integer;
var
  ErrorNumber:  cInt;
begin
Result := Integer(Linux.Futex(@Futex,FutexOp(FUTEX_CMP_REQUEUE,Private),RectCount(WakeCount),RectCountPtr(RequeueCount),@Futex2,Value));
If Result = -1 then
  begin
    ErrorNumber := errno;
    If ErrorNumber = ESysEAGAIN then
      Result := -1
    else
      raise ESFFutexError.CreateFmt('FutexCmpRequeue: Requeue failed (%d - %s).',
        [ErrorNumber,StrError(ErrorNumber)]);
  end;
end;

//------------------------------------------------------------------------------

Function FutexWakeOp(var Futex: TFutexWord; Count: Integer; var Futex2: TFutexWord; Count2: Integer;
                     Op: TFutexOperation; OpArg: Integer; Cmp: TFutexComparison; CmpArg: Integer;
                     OpArgShift: Boolean = False; Private: Boolean = False): Integer;

  Function GetVal3: cInt;
  begin
  {
      FUTEX_OP(op, oparg, cmp, cmparg) \
        (((op & 0xf) << 28) | ((cmp & 0xf) << 24) | \
        ((oparg & 0xfff) << 12) | (cmparg & 0xfff))
  }
    // operation
    case Op of
      fopSet:   Result := FUTEX_OP_SET shl 28;
      fopAdd:   Result := FUTEX_OP_ADD shl 28;
      fopOR:    Result := FUTEX_OP_OR shl 28;
      forANDN:  Result := FUTEX_OP_ANDN shl 28;
      fopXOR:   Result := FUTEX_OP_XOR shl 28;
    else
      raise ESFInvalidValue.CreateFmt('FutexWakeOp.GetVal3: Invalid operation (%d).',[Ord(Op)]);
    end;
    // operation argument shift
    If OpArgShift then
      Result := Result or cInt(FUTEX_OP_ARG_SHIFT shl 28);
    // comparison
    case Cmp of
      focEqual:           Result := Result or (FUTEX_OP_CMP_EQ shl 24);
      focNotEqual:        Result := Result or (FUTEX_OP_CMP_NE shl 24);
      focLess:            Result := Result or (FUTEX_OP_CMP_LT shl 24);
      focLessOrEqual:     Result := Result or (FUTEX_OP_CMP_LE shl 24);
      focGreater:         Result := Result or (FUTEX_OP_CMP_GT shl 24);
      focGreaterOrEqual:  Result := Result or (FUTEX_OP_CMP_GE shl 24);
    else
      raise ESFInvalidValue.CreateFmt('FutexWakeOp.GetVal3: Invalid comparison (%d).',[Ord(Cmp)]);
    end;
    // arguments
    Result := Result or ((OpArg and $FFF) shl 12) or (CmpArg and $FFF);
  end;

var
  ErrorNumber:  cInt;
begin
Result := Integer(Linux.Futex(@Futex,FutexOp(FUTEX_WAKE_OP,Private),RectCount(Count),RectCountPtr(Count2),@Futex2,GetVal3));
If Result = -1 then
  begin
    ErrorNumber := errno;
    raise ESFFutexError.CreateFmt('FutexWakeOp: Waking failed (%d - %s).',
      [ErrorNumber,StrError(ErrorNumber)]);
  end;
end;

//------------------------------------------------------------------------------

Function FutexWaitBitSet(var Futex: TFutexWord; Value: TFutexWord; BitMask: UInt32; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;
var
  ResVal:       cInt;
  TimeoutSpec:  TTimeSpec;
  ErrorNumber:  cInt;
begin
If Timeout <> INFINITE then
  begin
    TranslateTimeout(Timeout,TimeoutSpec,True,Realtime);
    ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_WAIT_BITSET,Private,Realtime),Value,@TimeoutSpec,nil,cInt(BitMask));
  end
else ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_WAIT_BITSET,Private,Realtime),Value,nil,nil,cInt(BitMask));
If ResVal = -1 then
  begin
    ErrorNumber := errno;
    case ErrorNumber of
      ESysEAGAIN:     Result := fwrValue;
      ESysETIMEDOUT:  Result := fwrTimeout;
      ESysEINTR:      Result := fwrInterrupted;
    else
      raise ESFFutexError.CreateFmt('FutexWaitBitSet: Wait failed (%d - %s).',
        [ErrorNumber,StrError(ErrorNumber)]);
    end;
  end
else Result := fwrWoken;
end;

//------------------------------------------------------------------------------

Function FutexWaitBitSetNoInt(var Futex: TFutexWord; Value: TFutexWord; BitMask: UInt32; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;
var
  StartTime:        TTimeSpec;
  TimeoutRemaining: UInt32;
  ElapsedMillis:    UInt32;
begin
GetTime(StartTime,Realtime);
TimeoutRemaining := Timeout;
while True do
  begin
    Result := FutexWaitBitSet(Futex,Value,BitMask,TimeoutRemaining,Private,Realtime);
    If Result = fwrInterrupted then
      begin
        If Timeout <> INFINITE then
          begin
            ElapsedMillis := GetElapsedMillis(StartTime,Realtime);
            If Timeout <= ElapsedMillis then
              begin
                Result := fwrTimeout;
                Break{while};
              end
            else TimeoutRemaining := Timeout - ElapsedMillis;
          end;
      end
    else Break{while};
  end;
end;

//------------------------------------------------------------------------------

Function FutexWakeBitSet(var Futex: TFutexWord; Count: Integer; BitMask: UInt32 = FUTEX_BITSET_MATCH_ANY; Private: Boolean = False): Integer;
var
  ErrorNumber:  cInt;
begin
Result := Integer(Linux.Futex(@Futex,FutexOp(FUTEX_WAKE_BITSET,Private),RectCount(Count),nil,nil,cInt(BitMask)));
If Result = -1 then
  begin
    ErrorNumber := errno;
    raise ESFFutexError.CreateFmt('FutexWakeBitSet: Waking failed (%d - %s).',
      [ErrorNumber,StrError(ErrorNumber)]);
  end;
end;

//------------------------------------------------------------------------------

Function FutexLockPI(var Futex: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False): TFutexWaitResult;
var
  ResVal:       cInt;
  TimeoutSpec:  TTimeSpec;
  ErrorNumber:  cInt;
begin
If Timeout <> INFINITE then
  begin
    TranslateTimeout(Timeout,TimeoutSpec,True,False);
    ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_LOCK_PI,Private),0,@TimeoutSpec);
  end
else ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_LOCK_PI,Private),0,nil);
If ResVal <> 0 then
  begin
    ErrorNumber := errno;
    case ErrorNumber of
      ESysEAGAIN:     Result := fwrValue;
      ESysETIMEDOUT:  Result := fwrTimeout;
    else
      raise ESFFutexError.CreateFmt('FutexLockPI: Lock failed (%d - %s).',
        [ErrorNumber,StrError(ErrorNumber)]);
    end;
  end
else Result := fwrWoken;
end;

//------------------------------------------------------------------------------

Function FutexLockPI2(var Futex: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;
var
  ResVal:       cInt;
  TimeoutSpec:  TTimeSpec;
  ErrorNumber:  cInt;
begin
If Timeout <> INFINITE then
  begin
    TranslateTimeout(Timeout,TimeoutSpec,True,Realtime);
    ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_LOCK_PI2,Private,Realtime),0,@TimeoutSpec);
  end
else ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_LOCK_PI2,Private,Realtime),0,nil);
If ResVal <> 0 then
  begin
    ErrorNumber := errno;
    case ErrorNumber of
      ESysEAGAIN:     Result := fwrValue;
      ESysETIMEDOUT:  Result := fwrTimeout;
    else
      raise ESFFutexError.CreateFmt('FutexLockPI2: Lock failed (%d - %s).',
        [ErrorNumber,StrError(ErrorNumber)]);
    end;
  end
else Result := fwrWoken;
end;

//------------------------------------------------------------------------------

Function FutexTryLockPI(var Futex: TFutexWord; Private: Boolean = False): Boolean;
var
  ResVal:       cInt;
  ErrorNumber:  cInt;
begin
ResVal := Integer(Linux.Futex(@Futex,FutexOp(FUTEX_TRYLOCK_PI,Private),0,nil));
If ResVal = -1 then
  begin
    Result := False;
    ErrorNumber := errno;
    If ErrorNumber <> ESysEAGAIN then
      raise ESFFutexError.CreateFmt('FutexTryLockPI: Try-lock failed (%d - %s).',
        [ErrorNumber,StrError(ErrorNumber)]);
  end
else Result := ResVal = 0;
end;

//------------------------------------------------------------------------------

Function FutexUnlockPI(var Futex: TFutexWord; Private: Boolean = False): Boolean;
var
  ResVal:       cInt;
  ErrorNumber:  cInt;
begin
ResVal := Integer(Linux.Futex(@Futex,FutexOp(FUTEX_UNLOCK_PI,Private),0,nil));
If ResVal = -1 then
  begin
    Result := False;
    ErrorNumber := errno;
    raise ESFFutexError.CreateFmt('FutexUnlockPI: Unlock failed (%d - %s).',
      [ErrorNumber,StrError(ErrorNumber)]);
  end
else Result := ResVal = 0;
end;

//------------------------------------------------------------------------------

Function FutexCmpRequeuePI(var Futex: TFutexWord; Value: TFutexWord; var Futex2: TFutexWord; RequeueCount: Integer; Private: Boolean = False): Integer;
var
  ErrorNumber:  cInt;
begin
Result := Integer(Linux.Futex(@Futex,FutexOp(FUTEX_CMP_REQUEUE_PI,Private),1,RectCountPtr(RequeueCount),@Futex2,Value));
If Result = -1 then
  begin
    ErrorNumber := errno;
    If ErrorNumber = ESysEAGAIN then
      Result := -1  // Futex <> Value
    else
      raise ESFFutexError.CreateFmt('FutexCmpRequeuePI: Requeue failed (%d - %s).',
        [ErrorNumber,StrError(ErrorNumber)]);
  end;
end;

//------------------------------------------------------------------------------

Function FutexWaitRequeuePI(var Futex: TFutexWord; Value: TFutexWord; var Futex2: TFutexWord; Timeout: UInt32 = INFINITE; Private: Boolean = False; Realtime: Boolean = False): TFutexWaitResult;
var
  ResVal:       cInt;
  TimeoutSpec:  TTimeSpec;
  ErrorNumber:  cInt;
begin
If Timeout <> INFINITE then
  begin
    TranslateTimeout(Timeout,TimeoutSpec,True,Realtime);
    ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_WAIT_REQUEUE_PI,Private,Realtime),cInt(Value),@TimeoutSpec,@Futex2,0);
  end
else ResVal := Linux.Futex(@Futex,FutexOp(FUTEX_WAIT_REQUEUE_PI,Private,Realtime),cInt(Value),nil,@Futex2,0);
If ResVal <> 0 then
  begin
    ErrorNumber := errno;
    case ErrorNumber of
      ESysEAGAIN:     Result := fwrValue;
      ESysETIMEDOUT:  Result := fwrTimeout;
    else
      raise ESFFutexError.CreateFmt('FutexWaitRequeuePI: Requeue wait failed (%d - %s).',
        [ErrorNumber,StrError(ErrorNumber)]);
    end;
  end
else Result := fwrWoken;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  Simple mutex
--------------------------------------------------------------------------------
===============================================================================}
const
  SF_SM_UNLOCKED = 0;
  SF_SM_LOCKED   = -1;

{===============================================================================
    Simple mutex - implementation
===============================================================================}

procedure SimpleMutexInit(out Futex: TFutexWord);
begin
Futex := SF_SM_UNLOCKED;
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure SimpleMutexLock(var Futex: TFutexWord);
var
  OrigState:  TFutexWord;
begin
{
  Conditionally set value of mutex (futex word) to locked if it was previously
  unlocked and store its previous value.

  In theory, unconditional exchange can be used here, but meh...
}
OrigState := InterlockedCompareExchange(Futex,SF_SM_LOCKED,SF_SM_UNLOCKED);
{
  If previous value of mutex was unlocked, then it means it is now locked for
  us and we can just return.

  If it was not unlocked, then it means some other thread has it locked for
  itself...
}
while OrigState <> SF_SM_UNLOCKED do
  begin
  {
    ...therefore we enter waiting.

    Note that if the mutex has bad value (not SF_STATE_LOCKED), then the
    FutexWait returns immediatelly. Such situation would lead to rapid calls
    to FutexWait if next exchange was conditional - for that reason we use
    unconditional exchange next.
  }
    FutexWait(Futex,SF_SM_LOCKED);
  {
    At this point, if value of mutex was unlocked, then we lock it for us and
    exit. If it was locked, then nothing is changed (at most bad lock value is
    replaced with a correct one) and the while cycle repeats.
  }
    OrigState := InterlockedExchange(Futex,SF_SM_LOCKED);
  end;
end;

//------------------------------------------------------------------------------

procedure SimpleMutexUnlock(var Futex: TFutexWord);
begin
{
  We only set the mutex to unlocked, nothing more is done (simple mutex is not
  recursive or ony of such complications).

  If the mutex was locked, then always try to wake one waiter, even if there
  can be none (keep it simple). If it was unlocked (this function was called
  erroneously), then do nothing.
}
If InterlockedExchange(Futex,SF_SM_UNLOCKED) <> SF_SM_UNLOCKED then
  FutexWake(Futex,1);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                Simple semaphore
--------------------------------------------------------------------------------
===============================================================================}

Function SemPostItrLckOp(A: TFutexUWord; var ItrLckResult: TFutexUWord): TFutexUWord; register;
begin
If A < TFutexUWord($FFFFFFFF) then
  Result := A + 1
else
  Result := TFutexUWord($FFFFFFFF);
ItrLckResult := Result;
end;

{===============================================================================
    Simple semaphore - implementation
===============================================================================}

procedure SimpleSemaphoreInit(out Futex: TFutexWord; InitialCount: UInt32);
begin
Futex := InitialCount;
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure SimpleSemaphoreWait(var Futex: TFutexWord);
var
  OldCount: TFutexUWord;
begin
repeat
{
  Decrement the semaphore counter and if it was 0, enter waiting.

  If it was above zero, then just return since the semaphore was signaled.
}
  OldCount := InterlockedDecrementIfPositive(TFutexUWord(Futex));
  If OldCount <= 0 then
    FutexWait(Futex,0);
until OldCount > 0;
end;

//------------------------------------------------------------------------------

procedure SimpleSemaphorePost(var Futex: TFutexWord);
var
  NewValue: TFutexUWord;
begin
{
  Atomically increments the futex word, but only when it is below a maximum
  value it can hold, if at this value then nothing is done (the word is not
  incremented to prevent overflow to zero).
}
NewValue := InterlockedOperation(TFutexUWord(Futex),@SemPostItrLckOp);
// wake as many waiters as the semaphore can serve
while NewValue > 0 do
  begin
    FutexWake(Futex,Integer(NewValue and $7FFFFFFF));
    NewValue := NewValue - (NewValue and $7FFFFFFF);
  end;
end;


{$IFDEF SF_SimpleRobustMutex}
{===============================================================================
--------------------------------------------------------------------------------
                               Simple robust mutex
--------------------------------------------------------------------------------
===============================================================================}
const
  SF_SRM_UNLOCKED = 0;

{===============================================================================
    Simple robust mutex - externals
===============================================================================}

Function errno_ptr: pcInt; cdecl; external name '__errno_location';

Function kill(pid: pid_t; sig: cint): cint; cdecl; external;

Function getpgid(pid: pid_t): pid_t; cdecl; external;

Function getpid: pid_t; cdecl; external;

//------------------------------------------------------------------------------

Function gettid: pid_t;
begin
Result := do_syscall(syscall_nr_gettid);
end;

{===============================================================================
    Simple robust mutex - internals
===============================================================================}
{$IFDEF SF_SRM_TimeCheck}
threadvar
  TVAR_ThreadCreationTime:      UInt32;
  TVAR_HaveThreadCreationTime:  Boolean;

//------------------------------------------------------------------------------

Function GetThreadCreationTime(ThreadID: pid_t): UInt32;
const
  READ_SIZE = 1024;
var
  BufferString:   AnsiString;
  SlidingBuffer:  PAnsiChar;
  BytesRead:      Integer;
  TokenCount:     Integer;
  i:              TStrOff;
  TokenStart:     TStrOff;
  TokenLength:    TStrSize;
begin
{
  Load the file - we must use following contrived reading because the stat file
  is opened with zero size.
}
with TFileStream.Create(Format('/proc/%d/stat',[ThreadID]),fmOpenRead or fmShareDenyWrite) do
try
  SetLength(BufferString,READ_SIZE);
  repeat
    SlidingBuffer := PAnsiChar(Addr(BufferString[Succ(Length(BufferString) - READ_SIZE)]));
    BytesRead := Read(SlidingBuffer^,READ_SIZE);
    If BytesRead >= READ_SIZE then
      SetLength(BufferString,Length(BufferString) + READ_SIZE)
    else
      SetLength(BufferString,Length(BufferString) - READ_SIZE + BytesRead);
  until BytesRead < READ_SIZE;
finally
  Free;
end;
// parse-out the start time (22nd field)
Result := 0;
If Length(BufferString) > 0 then
  begin
  {
    Fields in the string are separated with spaces (#32), we are interested
    only in the field 22, which contains time of creation of the given thread.

    Note that we are taking only lower 32 bits of the number, higher places are
    ignored - since the time is usually stored with resolution of 10ms, the
    lower places we are taking will only overflow in more than one year, which
    should be long enough time for our purpose.
  }
    TokenCount := 0;
    TokenStart := 1;
    TokenLength := 0;
    For i := 1 to Length(BufferString) do
      If Ord(BufferString[i]) = 32 then
        begin
          Inc(TokenCount);
          If TokenCount = 22 then
            begin
              If TokenLength <= 0 then
                raise ESFParsingError.Create('GetThreadCreationTime: Empty token.');
              Result := UInt32(StrToQWord(Copy(BufferString,TokenStart,TokenLength)));
              Exit; // we have what we need
            end;
          TokenStart := Succ(i);
          TokenLength := 0;
        end
      else Inc(TokenLength);
    // if here, it means the time was not parsed out
    raise ESFParsingError.Create('GetThreadCreationTime: Thread creation time not found.');
  end
else raise ESFParsingError.Create('GetThreadCreationTime: No data for parsing.');
end;

{$ELSE}//-----------------------------------------------------------------------

Function TrySignalThread(ThreadID: pid_t): Boolean;
var
  ErrorNumber:  cint;
begin
Result := kill(ThreadID,0) = 0;
If not Result then
  begin
    ErrorNumber := errno_ptr^;
    If ErrorNumber <> ESysESRCH then
      raise ESFSignalError.CreateFmt('TrySignalThread: Failed to probe process (%d).',[ErrorNumber]);
  end;
end;

{$ENDIF}
//------------------------------------------------------------------------------

Function LockingThreadLives(MutexValue: TSimpleRobustMutexState; var Data: TSimpleRobustMutexData): Boolean;
begin
Inc(Data.CheckCount);
{$IFDEF SF_SRM_TimeCheck}
try
  If FileExists(Format('/proc/%d/stat',[MutexValue.ThreadID])) then
    begin
      Result := GetThreadCreationTime(MutexValue.ThreadID) = MutexValue.ThreadCTime;
      Data.ConsecFailCount := 0;
    end
  else Result := False;
except
  If FileExists(Format('/proc/%d/stat',[MutexValue.ThreadID])) then
    begin
      If Data.ConsecFailCount < Data.MaxConsecFailCount then
        begin
          Inc(Data.ConsecFailCount);
          Inc(Data.FailCount);
        end
      else raise; // re-raise the exception
    end
  else Result := False;
end;
{$ELSE}
case Data.CheckMethod of
  tcmProc:  Result := DirectoryExists(Format('/proc/%d/task/%d',[MutexValue.ProcessID,MutexValue.ThreadID]));
else
 {tcmDefault,tcmSignal}
  If TrySignalThread(MutexValue.ProcessID) and TrySignalThread(MutexValue.ThreadID) then
    Result := getpgid(MutexValue.ThreadID) = MutexValue.ProcessID
  else
    Result := False;
end;
{$ENDIF}
end;

{===============================================================================
    Simple robust mutex - implementation
===============================================================================}

procedure SimpleRobustMutexInit(out RobustMutex: TSimpleRobustMutexState);
begin
RobustMutex.FullWidth := 0;
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure SimpleRobustMutexLock(var RobustMutex: TSimpleRobustMutexState; var Data: TSimpleRobustMutexData);
var
  NewValue:     TSimpleRobustMutexState;
  OldValue:     TSimpleRobustMutexState;
{$IFDEF SF_SRM_TimeCheck}
  FailCounter:  Integer;
{$ENDIF}
begin
// prepare variables
{$IFDEF SF_SRM_TimeCheck}
Data.CheckMethod := tcmDefault;
{$ENDIF}
Data.CheckCount := 0;
Data.FailCount := 0;
Data.ConsecFailCount := 0;
NewValue.ThreadID := gettid;
{$IFDEF SF_SRM_TimeCheck}
If not TVAR_HaveThreadCreationTime then
  begin
    // make sure we get the time
    FailCounter := 100;
    while True do
      try
        TVAR_ThreadCreationTime := GetThreadCreationTime(NewValue.ThreadID);
        Break{while}
      except
        If FailCounter > 0 then
          begin
            Dec(FailCounter); // eat the errors
            Sleep(10);
          end
        else raise;
      end;
    TVAR_HaveThreadCreationTime := True;
  end;
NewValue.ThreadCTime := TVAR_ThreadCreationTime;
{$ELSE}
NewValue.ProcessID := getpid;
{$ENDIF}
// and now the locking...
OldValue.FullWidth := InterlockedCompareExchange(RobustMutex.FullWidth,NewValue.FullWidth,SF_SRM_UNLOCKED);
while OldValue.FullWidth <> SF_SRM_UNLOCKED do
  begin
    If not LockingThreadLives(OldValue,Data) then
      If InterlockedCompareExchange(RobustMutex.FullWidth,NewValue.FullWidth,OldValue.FullWidth) = OldValue.FullWidth then
        Break{while};
    FutexWait(RobustMutex.FutexWord,OldValue.FutexWord,Data.CheckInterval);
    OldValue.FullWidth := InterlockedCompareExchange(RobustMutex.FullWidth,NewValue.FullWidth,SF_SRM_UNLOCKED);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SimpleRobustMutexLock(var RobustMutex: TSimpleRobustMutexState; CheckInterval: UInt32 = 100; CheckMethod: TThreadCheckMethod = tcmDefault);
var
  TempData:  TSimpleRobustMutexData;
begin
TempData.CheckInterval := CheckInterval;
TempData.CheckMethod := CheckMethod;
If CheckInterval <> 0 then
  // give it at least one second of time...
  TempData.MaxConsecFailCount := Ceil(1000 / CheckInterval)
else
  // someone wants to play dirty...
  TempData.MaxConsecFailCount := 10;
SimpleRobustMutexLock(RobustMutex,TempData);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SimpleRobustMutexLock(var RobustMutex: TSimpleRobustMutexState; CheckMethod: TThreadCheckMethod);
begin
SimpleRobustMutexLock(RobustMutex,250,CheckMethod);
end;

//------------------------------------------------------------------------------

procedure SimpleRobustMutexUnlock(var RobustMutex: TSimpleRobustMutexState);
begin
If InterlockedExchange(RobustMutex.FullWidth,SF_SRM_UNLOCKED) <> SF_SRM_UNLOCKED then
  FutexWake(RobustMutex.FutexWord,1);
end;


{$ENDIF}
{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleSynchronizer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleSynchronizer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleSynchronizer - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleSynchronizer.Initialize(StatePtr: PSimpleSynchronizerState);
begin
FillChar(fLocalState,SizeOf(TSimpleSynchronizerState),0);
fStatePtr := StatePtr;
fOwnsState := fStatePtr = Addr(fLocalState);
end;

//------------------------------------------------------------------------------

procedure TSimpleSynchronizer.Finalize;
begin
// nothing to do atm.
end;

{-------------------------------------------------------------------------------
    TSimpleSynchronizer - public methods
-------------------------------------------------------------------------------}

constructor TSimpleSynchronizer.Create(var Futex: TFutexWord);
begin
inherited Create;
Initialize(PSimpleSynchronizerState(@Futex));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF SF_SimpleRobustMutex}
constructor TSimpleSynchronizer.Create(var SimpleRobustMutexState: TSimpleRobustMutexState);
begin
inherited Create;
Initialize(PSimpleSynchronizerState(@SimpleRobustMutexState));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{$ENDIF}

constructor TSimpleSynchronizer.Create;
begin
inherited Create;
Initialize(@fLocalState);
end;

//------------------------------------------------------------------------------

destructor TSimpleSynchronizer.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleSynchronizer.Init;
begin
// do nothing here, implement in descendants when necessary
end;

//------------------------------------------------------------------------------

procedure TSimpleSynchronizer.Final;
begin
// do nothing here, implement in descendants when necessary
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TSimpleMutex
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleMutex - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleMutex - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleMutex.Initialize(StatePtr: PSimpleSynchronizerState);
begin
inherited;
If fOwnsState then
  SimpleMutexInit(PFutexWord(fStatePtr)^);
end;

{-------------------------------------------------------------------------------
    TSimpleMutex - public methods
-------------------------------------------------------------------------------}

procedure TSimpleMutex.Init;
begin
SimpleMutexInit(PFutexWord(fStatePtr)^);
end;

//------------------------------------------------------------------------------

procedure TSimpleMutex.Enter;
begin
SimpleMutexLock(PFutexWord(fStatePtr)^);
end;

//------------------------------------------------------------------------------

procedure TSimpleMutex.Leave;
begin
SimpleMutexUnlock(PFutexWord(fStatePtr)^);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSimpleSemaphore
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleSemaphore - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleSemaphore - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleSemaphore.Initialize(StatePtr: PSimpleSynchronizerState);
begin
inherited Initialize(StatePtr);
If fOwnsState then
  SimpleSemaphoreInit(PFutexWord(fStatePtr)^,0);
fInitialCount := UInt32(InterlockedLoad(PFutexWord(fStatePtr)^));
end;

{-------------------------------------------------------------------------------
    TSimpleSemaphore - public methods
-------------------------------------------------------------------------------}

constructor TSimpleSemaphore.CreateAndInitCount(InitialCount: UInt32);
begin
inherited Create;
SimpleSemaphoreInit(PFutexWord(fStatePtr)^,InitialCount);
fInitialCount := InitialCount;
end;

//------------------------------------------------------------------------------

procedure TSimpleSemaphore.Init;
begin
SimpleSemaphoreInit(PFutexWord(fStatePtr)^,fInitialCount);
end;

//------------------------------------------------------------------------------

procedure TSimpleSemaphore.Acquire;
begin
SimpleSemaphoreWait(PFutexWord(fStatePtr)^);
end;

//------------------------------------------------------------------------------

procedure TSimpleSemaphore.Release;
begin
SimpleSemaphorePost(PFutexWord(fStatePtr)^);
end;


{$IFDEF SF_SimpleRobustMutex}
{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleRobustMutex
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRobustMutex - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleRobustMutex - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleRobustMutex.Initialize(StatePtr: PSimpleSynchronizerState);
begin
inherited Initialize(StatePtr);
If fOwnsState then
  SimpleRobustMutexInit(PSimpleRobustMutexState(fStatePtr)^);
end;

{-------------------------------------------------------------------------------
    TSimpleRobustMutex - public methods
-------------------------------------------------------------------------------}

procedure TSimpleRobustMutex.Init;
begin
SimpleRobustMutexInit(PSimpleRobustMutexState(fStatePtr)^);
end;

//------------------------------------------------------------------------------

procedure TSimpleRobustMutex.Enter;
begin
SimpleRobustMutexLock(PSimpleRobustMutexState(fStatePtr)^);
end;

//------------------------------------------------------------------------------

procedure TSimpleRobustMutex.Leave;
begin
SimpleRobustMutexUnlock(PSimpleRobustMutexState(fStatePtr)^);
end;

{$ENDIF}

end.

