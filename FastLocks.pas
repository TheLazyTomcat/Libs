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
      of thousand of separate data structures, each of which could have been
      accessed by several threads, and where concurrent access was rare but
      very possible and dangerous. When a simultaneous access occured, it was
      almost always reading.

      Creating RW lock for each of the structure was unfeasible, so this
      library was written to provide some light-weight locking mechanism with
      minimal memory and OS resources footprint. Implementation is therefore
      maximally simple, which causes many limitations.

    <<< WARNING

    Non-blocking behaviour means that any attempt to acquire lock will return
    immediatelly, and resulting value of this attempt indicates whether the
    lock was really acquired or not.

    At this point, only two synchronization primitives/objects are implenented,
    critical section and an RW lock (multiple-read exclusive-write
    synchronizer). More might be added later, but currently it is unlikely.
    For details about how any of the object works and what are its limitations,
    refer to its declaration.

    In its basic form, each in-here implemented synchronizer is just an integer
    residing in the memory. Within this library, this integer is called sync
    word.
    It is used to store the locking counters and interlocked functions are used
    to atomically change and probe stored values and to decide state of the
    object and required action.

      WARNING - all implemented synchronizers are operating on the same sync
                word type (TFLSyncWord), but they are not mutually compatible.
                So always use one sync word for only one type of synchronizer,
                never mix them on one variable.

    All synchronizers can be used either directly, where you allocate a variable
    of type TFLSyncWord and then operate on it using procedural interface (eg.
    FastCriticalSectionEnter, FastMREWBeginRead, ...), or indirectly,
    by creating an instance of provided class and using its methods.

    When creating the class instance, you can either provide preallocated sync
    word variable or leave its complete management on the instance itself.
    This gives you more freedom in deciding how to use the sychnonization - you
    can either allocate common sync word and create new instance for each
    syhcnronizing thread, or you can create one common instance and use >it< in
    all threads.

      NOTE - if the sync word variable is located in a shared memory, the
             synchronizers can then be used for inter-process synchronization.

    Here is a small example how a non-blocking synchronization can be used:

                <unsynchronized_code>
           -->  If CritSect.Enter then
           |      try
           |        <synchronized_code>
           |      finally
           |        CritSect.Leave;
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
           -->  If CritSect.WaitToEnter(500) = wrAcquired then
           |      try
           |        <synchronized_code>
           |      finally
           |        CritSect.Leave;
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

      - there is absolutely no deadlock prevention - be extremely carefull when
        trying to acquire synchronizer in more than one place in a single thread
        (trying to acquire synchronizer second time in the same thread will
        always fail, with exception being MREW reading, which is given by
        concept of multiple readers access)

      - use provided waiting and spinning only when necessary - synchronizers
        are intended to be used as non-blocking

      - waiting is always active (spinning) - do not wait for prolonged time
        intervals as it might starve other threads, use infinite waiting only
        in extreme cases and only when really necessary

      - use synchronization by provided objects only on very short (in time,
        not code) routines - do not use to synchronize code that is executing
        longer than few milliseconds

      - every successful acquire of a synchronizer MUST be paired by a release,
        synhronizers are not automalically released

  Version 1.3.1 (2021-12-24)

  Last change 2022-06-25

  ©2016-2022 František Milt

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
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps
  * SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID

  SimpleCPUID might not be required, see library InterlockedOps for details.

===============================================================================}
unit FastLocks;

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
  {$INLINE ON}
  {$DEFINE CanInline}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
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
  This holds true on all systems.

  By default NOT defined.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol FastLocks_SyncWord64_On.
}
{.$DEFINE SyncWord64}
{$IFDEF FastLocks_SyncWord64_On}
  {$DEFINE SyncWord64}
{$ENDIF}

interface

uses
  SysUtils,
  AuxTypes, AuxClasses;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EFLException = class(Exception);

  EFLCounterError = class(EFLException);
  EFLInvalidValue = class(EFLException);

{===============================================================================
--------------------------------------------------------------------------------
                                   Fast locks                                    
--------------------------------------------------------------------------------
===============================================================================}

const
  FL_DEF_SPIN_DELAY_CNT = 1000; // default value of SpinDelayCount
  FL_DEF_WAIT_SPIN_CNT  = 1500; // default value of WaitSpinCount

  INFINITE = UInt32(-1);  // infinite timeout interval

type
  TFLSyncWord = {$IFDEF SyncWord64}UInt64{$ELSE}UInt32{$ENDIF};
  PFLSyncWord = ^TFLSyncWord;

{
  Returned as a result of spinning or waiting.
  
  Informs whether the object was acquired and locked, and if not, for what
  reason the locking failed.

    wrAcquired - The object was acquired and is now locked. Remember to release
                 it after the lock is no longer needed.

    wrTimeout  - Spinning/waiting timed-out, ie. locking was not successful in
                 a given timeout period.

    wrReserved - Spinning/waiting failed and the object was not locked because
                 it was reserved or the reserve count reached its maximum.
                 This is not an error, just a limitation of this implementation,
                 you should try the waiting again after some time.

    wrError    - Unknown or external error has ocurred, the object might be in
                 an inconsistent state and should not be used anymore.
}
  TFLWaitResult = (wrAcquired,wrTimeout,wrReserved,wrError);

{
  In waiting, the function blocks by executing a cycle. Each iteration of this
  cycle contains a try to acquire the lock, a check for timeout and a delaying
  part that prevents rapid calls to acquire and timers.

  TFLWaitDelayMethod enumeration is here to select a method used for this
  delaying.

    wdNone        No delaying action is performed.

    wdSpin        A spinning will be performed. This is the default operation.

    wdYield       An attempt to yield execution of current thread is made.
                  If system has another thread that can be run, the current
                  thread is suspended, rescheduled and the next thread is run.
                  If there is no thread awaiting execution, then the current
                  thread is not suspended and continues execution and pretty
                  much performs spinning.

                    WARNING - use with caution, as it can cause spinning with
                              rapid calls to thread yielding on uncontested CPU.

    wdSleep       The current thread stops execution (call to Sleep) for no
                  less than 10ms. Note that this time might actually be longer
                  because of granularity of scheduling timers, resulting in
                  slightly longer wait time than is requested.

    wdSleepEx     Behaves the same as wdSleep, but the thread can be awakened
                  by APC or I/O completion calls.

                    NOTE - works only on Windows, everywhere else it behaves
                           the same as wdSleep.

    wdYieldSleep  Combination od wdYield and wdSleep - when the thread is not
                  yielded (eg. because no thread is waiting execution), a sleep
                  is performed.

                    NOTE - works only on Windows, everywhere else it behaves
                           the same as wdSleep.
}
  TFLWaitDelayMethod = (wdNone,wdSpin,wdYield,wdSleep,wdSleepEx,wdYieldSleep);

{===============================================================================
--------------------------------------------------------------------------------
                                    TFastLock
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastLock - class declaration
===============================================================================}
type
  TFastLock = class(TCustomObject)
  protected
    fSyncWord:        TFLSyncWord;
    fSyncWordPtr:     PFLSyncWord;
    fOwnsSyncWord:    Boolean;
    fWaitDelayMethod: UInt32;
    fWaitSpinCount:   UInt32;
    fSpinDelayCount:  UInt32;
    fCounterFreq:     Int64;
    Function GetWaitDelayMethod: TFLWaitDelayMethod; virtual;
    procedure SetWaitDelayMethod(Value: TFLWaitDelayMethod); virtual;
    Function GetWaitSpinCount: UInt32; virtual;
    procedure SetWaitSpinCount(Value: UInt32); virtual;
    Function GetSpinDelayCount: UInt32; virtual;
    procedure SetSpinDelayCount(Value: UInt32); virtual;
    procedure Initialize(SyncWordPtr: PFLSyncWord); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(var SyncWord: TFLSyncWord); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;
    property OwnsSyncWord: Boolean read fOwnsSyncWord;
    property WaitDelayMethod: TFLWaitDelayMethod read GetWaitDelayMethod write SetWaitDelayMethod;
    property WaitSpinCount: UInt32 read GetWaitSpinCount write SetWaitSpinCount;
    property SpinDelayCount: UInt32 read GetSpinDelayCount write SetSpinDelayCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Fast critical section
--------------------------------------------------------------------------------
===============================================================================}
{
  Classical critical section - only one thread can acquire the object, and
  while it is locked all subsequent attemps to acquire it will fail.

  When spinning or waiting, there is no guarantee that the first thread that
  entered this cycle will also acquire the object. The order in which waiting
  threads enter the section is undefined and more or less random.
  Note that while any thread is in spinning or waiting cycle, the section can
  only be entered by spinning or waiting threads, not by a call to enter. This
  assures that blocked threads are served before threads which are using the
  object asynchronously (as it should be).
}
{===============================================================================
    Fast critical section - procedural interface declaration
===============================================================================}

procedure FastCriticalSectionInit(out SyncWord: TFLSyncWord);
procedure FastCriticalSectionFinal(var SyncWord: TFLSyncWord);

Function FastCriticalSectionEnter(var SyncWord: TFLSyncWord): Boolean;
procedure FastCriticalSectionLeave(var SyncWord: TFLSyncWord);
 
{
  A small note on spinning and waiting implementation...

  Spinning:

    In spinning, a cycle is performed. In each iteration of this cycle, an
    attempt to acquire the object is tried. If it is not successful, a delaying
    action is executed and then the cycle repeats.

    Maximum number of iterations is limited by a parameter SpinCount, unless it
    is set to INFINITE, in which case the cycle never terminates.

    The delaying action is a small piece of code with no external effects that
    is executed multiple times to make this delaying longer. Number of
    executions is given in parameter SpinDelayCount.

  Waiting:

    Waiting is very similar to spinning in that it runs in a cycle, but the
    number of iteration is not given explicitly, it depends on a timeout
    interval.

    In each iteration, and attempt to acquire is made, and when not successful
    a delaying action is performed. Nature of this action can be selected by
    a parameter WaitDelayMethod.

    One possible delaying action is spinning. In this case, a spin as described
    above is performed, with a spin count set to WaitSpinCount.
}

Function FastCriticalSectionSpinToEnter(var SyncWord: TFLSyncWord; SpinCount: UInt32; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;
Function FastCriticalSectionWaitToEnter(var SyncWord: TFLSyncWord; Timeout: UInt32; WaitDelayMethod: TFLWaitDelayMethod = wdSpin;
  WaitSpinCount: UInt32 = FL_DEF_WAIT_SPIN_CNT; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;

{===============================================================================
--------------------------------------------------------------------------------
                              TFastCriticalSection
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastCriticalSection - class declaration
===============================================================================}
type
  TFastCriticalSection = class(TFastLock)
  protected
    procedure Initialize(SyncWordPtr: PFLSyncWord); override;
    procedure Finalize; override;
  public
    Function Enter: Boolean; virtual;
    procedure Leave; virtual;
    Function SpinToEnter(SpinCount: UInt32): TFLWaitResult; virtual;
    Function WaitToEnter(Timeout: UInt32): TFLWaitResult; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                    Fast MREW
--------------------------------------------------------------------------------
===============================================================================}
{
  This object can be locked in two principal ways - for reading (read lock) or
  for writing (write lock).
  Unlike for write lock, where only one can be present at a time, read locks
  have counter that allows multiple readers to acquire a read lock.

  Acquiring the object for write can only be successful if no reader have a
  read lock.

  No reader can acquire read lock while the object is locked for writing, or
  any thread is spinning or waiting for a write lock (this prevents starving
  of writers by readers - waiting writer excludes any reader to acquire read
  lock).

  The read lock cannot be promoted to write lock - an attempt to acquire write
  lock while there is any read lock will always fail.

  While waiting for a read lock, it is entirely possible the object will be
  locked by other thread for writing. But, as mentioned before, during wait for
  write lock, no reader can acquire read lock, even through waiting to read.

  The order in which waiting or spinning threads acquire their locks is
  undefined.

    WARNING - number of readers is limited, 2047 for 32bit sync words (default),
              2147483647 for 64bit sync words.
}
{===============================================================================
    Fast MREW - procedural interface declaration
===============================================================================}

procedure FastMREWInit(out SyncWord: TFLSyncWord);
procedure FastMREWFinal(var SyncWord: TFLSyncWord);

Function FastMREWBeginRead(var SyncWord: TFLSyncWord): Boolean;
procedure FastMREWEndRead(var SyncWord: TFLSyncWord);

Function FastMREWSpinToRead(var SyncWord: TFLSyncWord; SpinCount: UInt32; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;
Function FastMREWWaitToRead(var SyncWord: TFLSyncWord; Timeout: UInt32; WaitDelayMethod: TFLWaitDelayMethod = wdSpin;
  WaitSpinCount: UInt32 = FL_DEF_WAIT_SPIN_CNT; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;

Function FastMREWBeginWrite(var SyncWord: TFLSyncWord): Boolean;
procedure FastMREWEndWrite(var SyncWord: TFLSyncWord);

Function FastMREWSpinToWrite(var SyncWord: TFLSyncWord; SpinCount: UInt32; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;
Function FastMREWWaitToWrite(var SyncWord: TFLSyncWord; Timeout: UInt32; WaitDelayMethod: TFLWaitDelayMethod = wdSpin;
  WaitSpinCount: UInt32 = FL_DEF_WAIT_SPIN_CNT; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;

{===============================================================================
--------------------------------------------------------------------------------
                                    TFastMREW
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastMREW - class declaration
===============================================================================}
type
  TFastMREW = class(TFastLock)
  protected
    procedure Initialize(SyncWordPtr: PFLSyncWord); override;
    procedure Finalize; override;    
  public
    Function BeginRead: Boolean; virtual;
    procedure EndRead; virtual;
    Function BeginWrite: Boolean; virtual;
    procedure EndWrite; virtual;
    Function SpinToRead(SpinCount: UInt32): TFLWaitResult; virtual;
    Function WaitToRead(Timeout: UInt32): TFLWaitResult; virtual;
    Function SpinToWrite(SpinCount: UInt32): TFLWaitResult; virtual;
    Function WaitToWrite(Timeout: UInt32): TFLWaitResult; virtual;
  end;

  // full-name alias
  TFastMultiReadExclusiveWriteSynchronizer = TFastMREW;

implementation

uses
{$IFDEF Windows}
  Windows,
{$ELSE}
  baseunix, linux,
{$ENDIF}
  InterlockedOps;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   Fast locks
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Fast locks - internal functions
===============================================================================}

Function SpinDelay(Divisor: UInt32): UInt32;  // do not inline
begin
// just some contained, relatively long, but othervise pointless operation
Result := UInt32(3895731025) div Divisor;
end;

//------------------------------------------------------------------------------

Function GetCounterFrequency(out Freq: Int64): Boolean;
{$IFNDEF Windows}
var
  Time: TTimeSpec;
{$ENDIF}
begin
{$IFDEF Windows}
Freq := 0;
Result := QueryPerformanceFrequency(Freq);
{$ELSE}
Freq := 1000000000{ns};
Result := clock_getres(CLOCK_MONOTONIC_RAW,@Time) = 0;
{$ENDIF}
If Freq and Int64($1000000000000000) <> 0 then
  raise EFLInvalidValue.CreateFmt('GetCounterFrequency: Unsupported frequency value (0x%.16x)',[Freq]);
end;

//------------------------------------------------------------------------------

Function GetCounterValue(out Count: Int64): Boolean;
{$IFNDEF Windows}
var
  Time: TTimeSpec;
{$ENDIF}
begin
{$IFDEF Windows}
Count := 0;
Result := QueryPerformanceCounter(Count);
{$ELSE}
Result := clock_gettime(CLOCK_MONOTONIC_RAW,@Time) = 0;
Count := Int64(Time.tv_sec) * 1000000000 + Time.tv_nsec;
{$ENDIF}
// mask out bit 63 to prevent problems with signed 64bit integer
Count := Count and Int64($7FFFFFFFFFFFFFFF);
end;

//------------------------------------------------------------------------------

{$IFDEF Windows}
{$IF not Declared(SwitchToThread)}
Function SwitchToThread: BOOL; stdcall; external kernel32;
{$IFEND}
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
    Fast locks - imlementation constants
===============================================================================}
const
  FL_UNLOCKED = TFLSyncWord(0);

  FL_INVALID = TFLSyncWord(-1); // used to finalize the objects

{===============================================================================
    Fast locks - waiting and spinning implementation
===============================================================================}
type
  TFLSpinParams = record
    SyncWordPtr:    PFLSyncWord;
    SpinCount:      UInt32;
    SpinDelayCount: UInt32;
    Reserve:        Boolean;
    Reserved:       Boolean;
    FceReserve:     Function(var SyncWord: TFLSyncWord): Boolean;
    FceUnreserve:   procedure(var SyncWord: TFLSyncWord);
    FceAcquire:     Function(var SyncWord: TFLSyncWord; Reserved: Boolean; out FailedDueToReservation: Boolean): Boolean;
  end;

//------------------------------------------------------------------------------

Function _DoSpin(Params: TFLSpinParams): TFLWaitResult;

  Function InternalSpin(Reserved: Boolean): TFLWaitResult;

    Function SpinDelayAndCount: Boolean;
    var
      i:  Integer;
    begin
      // do some delaying and decrease spin count if not in infinite spinning
      For i := 1 to Params.SpinDelayCount do
        SpinDelay(i);
      If Params.SpinCount <> INFINITE then
        Dec(Params.SpinCount);
      Result := Params.SpinCount > 0;
    end;
    
  var
    FailedDueToReservation: Boolean;
  begin
    while not Params.FceAcquire(Params.SyncWordPtr^,Reserved,FailedDueToReservation) do
      If not FailedDueToReservation then
        begin
          // acquire failed for other reason than reservation
          If not SpinDelayAndCount then
            begin
              // spin count reached zero
              Result := wrTimeout;
              Exit;
            end;
        end
      else
        begin
          // acquire failed due to reservation
          Result := wrReserved;
          Exit;
        end;
    // if we are here, acquire was successful
    Result := wrAcquired;
  end;

begin
try
  If Params.Reserve then
    begin
      If Params.FceReserve(Params.SyncWordPtr^) then
        try
          Result := InternalSpin(True);
        finally
          Params.FceUnreserve(Params.SyncWordPtr^);
        end
      else Result := wrReserved;
    end
  else Result := InternalSpin(Params.Reserved);
except
  Result := wrError;
end;
end;

//==============================================================================

type
  TFLWaitParams = record
    SyncWordPtr:      PFLSyncWord;
    Timeout:          UInt32;
    WaitDelayMethod:  TFLWaitDelayMethod;
    WaitSpinCount:    UInt32;
    SpinDelayCount:   UInt32;
    Reserve:          Boolean;
    FceReserve:       Function(var SyncWord: TFLSyncWord): Boolean;
    FceUnreserve:     procedure(var SyncWord: TFLSyncWord);
    FceAcquire:       Function(var SyncWord: TFLSyncWord; Reserved: Boolean; out FailedDueToReservation: Boolean): Boolean;
    CounterFrequency: Int64;    
    StartCount:       Int64;
  end;

//------------------------------------------------------------------------------

Function _DoWait(Params: TFLWaitParams): TFLWaitResult;

  Function InternalWait(Reserved: Boolean): TFLWaitResult;

    Function GetElapsedMillis: UInt32;
    var
      CurrentCount: Int64;
    begin
      If GetCounterValue(CurrentCount) then
        begin
          If CurrentCount < Params.StartCount then
            Result := ((High(Int64) - Params.StartCount + CurrentCount) * 1000) div Params.CounterFrequency
          else
            Result := ((CurrentCount - Params.StartCount) * 1000) div Params.CounterFrequency;
        end
      else Result := UInt32(-1);
    end;

  var
    FailedDueToReservation: Boolean;
    SpinParams:             TFLSpinParams;
  begin
    while not Params.FceAcquire(Params.SyncWordPtr^,Reserved,FailedDueToReservation) do
      If not FailedDueToReservation then
        begin
          // acquire failed for other reason than reservation, check elapsed time
          If (Params.TimeOut <> INFINITE) and (GetElapsedMillis >= Params.TimeOut) then
            begin
              // timeout elapsed
              Result := wrTimeout;
              Exit;
            end
          else
            begin
              // still in timeout period, do delaying
              case Params.WaitDelayMethod of
                wdNone:;      // do nothing;
                wdYield:      YieldThread;
              {$IFDEF Windows}
                wdSleep:      Sleep(10);
                wdSleepEx:    SleepEx(10,True);
                wdYieldSleep: If not YieldThread then
                                Sleep(10);
              {$ELSE}
                wdSleep,
                wdSleepEx,
                wdYieldSleep: Sleep(10);
              {$ENDIF}
              else
               {wdSpin}
                // fill parameters for spinning
                SpinParams.SyncWordPtr := Params.SyncWordPtr;
                SpinParams.SpinCount := Params.WaitSpinCount;
                SpinParams.SpinDelayCount := Params.SpinDelayCount;
                SpinParams.Reserve := False;
                SpinParams.Reserved := Reserved;
                SpinParams.FceReserve := Params.FceReserve;
                SpinParams.FceUnreserve := Params.FceUnreserve;
                SpinParams.FceAcquire := Params.FceAcquire;
                case _DoSpin(SpinParams) of
                  wrAcquired:   Break{while};
                  wrTimeout:;   // just continue, spinning completed without acquire
                  wrReserved:   begin
                                  Result := wrReserved;
                                  Exit;
                                end;
                else
                  Result := wrError;
                  Exit;
                end;
              end
            end;
        end
      else
        begin
          // acquire failed due to reservation
          Result := wrReserved;
          Exit;
        end;
    Result := wrAcquired;
  end;

begin
If GetCounterValue(Params.StartCount) then
  begin
    If Params.Reserve then
      begin
        If Params.FceReserve(Params.SyncWordPtr^) then
          try
            Result := InternalWait(True);
          finally
            Params.FceUnreserve(Params.SyncWordPtr^);
          end
        else Result := wrReserved;
      end
    else Result := InternalWait(False);
  end
else Result := wrError; 
end;

{===============================================================================
--------------------------------------------------------------------------------
                                    TFastLock
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastLock - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFastLock - protected methods
-------------------------------------------------------------------------------}

Function TFastLock.GetWaitDelayMethod: TFLWaitDelayMethod;
begin
Result := TFLWaitDelayMethod(InterlockedLoad(fWaitDelayMethod));
end;

//------------------------------------------------------------------------------

procedure TFastLock.SetWaitDelayMethod(Value: TFLWaitDelayMethod);
begin
InterlockedStore(fWaitDelayMethod,UInt32(Ord(Value)));
end;

//------------------------------------------------------------------------------

Function TFastLock.GetWaitSpinCount: UInt32;
begin
Result := InterlockedLoad(fWaitSpinCount);
end;

//------------------------------------------------------------------------------

procedure TFastLock.SetWaitSpinCount(Value: UInt32);
begin
InterlockedStore(fWaitSpinCount,Value);
end;

//------------------------------------------------------------------------------

Function TFastLock.GetSpinDelayCount: UInt32;
begin
Result := InterlockedLoad(fSpinDelayCount);
end;

//------------------------------------------------------------------------------

procedure TFastLock.SetSpinDelayCount(Value: UInt32);
begin
InterlockedStore(fSpinDelayCount,Value);
end;

//------------------------------------------------------------------------------

procedure TFastLock.Initialize(SyncWordPtr: PFLSyncWord);
begin
fSyncWord := FL_UNLOCKED;
fSyncWordPtr := SyncWordPtr;
fOwnsSyncWord := fSyncWordPtr = Addr(fSyncWord);
SetWaitDelayMethod(wdSpin);
SetWaitSpinCount(FL_DEF_WAIT_SPIN_CNT);
SetSpinDelayCount(FL_DEF_SPIN_DELAY_CNT);
If not GetCounterFrequency(fCounterFreq) then
  raise EFLCounterError.CreateFmt('TFastLock.Initialize: Cannot obtain counter frequency (0x%.8x).',
                                  [{$IFDEF Windows}GetLastError{$ELSE}errno{$ENDIF}]);
end;

//------------------------------------------------------------------------------

procedure TFastLock.Finalize;
begin
fSyncWordPtr := nil;
end;

{-------------------------------------------------------------------------------
    TFastLock - public methods
-------------------------------------------------------------------------------}

constructor TFastLock.Create(var SyncWord: TFLSyncWord);
begin
inherited Create;
Initialize(@SyncWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFastLock.Create;
begin
inherited Create;
Initialize(@fSyncWord);
end;

//------------------------------------------------------------------------------

destructor TFastLock.Destroy;
begin
Finalize;
inherited
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Fast critical section
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Fast critical section - imlementation constants
===============================================================================}
{
  Meaning of bits in sync word for fast critical section:

    32bit     64bit
     0..15     0..31    - acquire count
    16..31    32..63    - reserve count
}
const
{$IFDEF SyncWord64}

  FL_CS_ACQUIRE_DELTA = TFLSyncWord($0000000000000001);
  FL_CS_ACQUIRE_MASK  = TFLSyncWord($00000000FFFFFFFF);
  FL_CS_ACQUIRE_MAX   = TFLSyncWord(2147483647);  // 0x7FFFFFFF
  FL_CS_ACQUIRE_SHIFT = 0;

  FL_CS_RESERVE_DELTA = TFLSyncWord($0000000100000000);
  FL_CS_RESERVE_MASK  = TFLSyncWord($FFFFFFFF00000000);
  FL_CS_RESERVE_MAX   = TFLSyncWord(2147483647);  // 0x7FFFFFFF
  FL_CS_RESERVE_SHIFT = 32;

{$ELSE} //-  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

  FL_CS_ACQUIRE_DELTA = TFLSyncWord($00000001);
  FL_CS_ACQUIRE_MASK  = TFLSyncWord($0000FFFF);
  FL_CS_ACQUIRE_MAX   = TFLSyncWord(32767);       // 0x7FFF
  FL_CS_ACQUIRE_SHIFT = 0;

  FL_CS_RESERVE_DELTA = TFLSyncWord($00010000);
  FL_CS_RESERVE_MASK  = TFLSyncWord($FFFF0000);
  FL_CS_RESERVE_MAX   = TFLSyncWord(32767);       // 0x7FFF
  FL_CS_RESERVE_SHIFT = 16;

{$ENDIF}  

{===============================================================================
    Fast critical section - procedural interface implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Fast critical section - internal functions
-------------------------------------------------------------------------------}

Function _FastCriticalSectionReserve(var SyncWord: TFLSyncWord): Boolean;
var
  OldSyncWord:  TFLSyncWord;
begin
OldSyncWord := InterlockedExchangeAdd(SyncWord,FL_CS_RESERVE_DELTA);
Result := ((OldSyncWord and FL_CS_RESERVE_MASK) shr FL_CS_RESERVE_SHIFT) < FL_CS_RESERVE_MAX;
If not Result then
  InterlockedExchangeSub(SyncWord,FL_CS_RESERVE_DELTA);
end;

//------------------------------------------------------------------------------

procedure _FastCriticalSectionUnreserve(var SyncWord: TFLSyncWord);
begin
InterlockedExchangeSub(SyncWord,FL_CS_RESERVE_DELTA);
end;

//------------------------------------------------------------------------------

Function _FastCriticalSectionEnter(var SyncWord: TFLSyncWord; Reserved: Boolean; out FailedDueToReservation: Boolean): Boolean;
var
  OldSyncWord:  TFLSyncWord;
begin
FailedDueToReservation := False;
OldSyncWord := InterlockedExchangeAdd(SyncWord,FL_CS_ACQUIRE_DELTA);
If ((OldSyncWord and FL_CS_ACQUIRE_MASK) shr FL_CS_ACQUIRE_SHIFT) < FL_CS_ACQUIRE_MAX then
  begin
    If Reserved then
      Result := (((OldSyncWord and FL_CS_RESERVE_MASK) shr FL_CS_RESERVE_SHIFT) <> 0) and
                (((OldSyncWord and FL_CS_ACQUIRE_MASK) shr FL_CS_ACQUIRE_SHIFT) = 0)
    else
      Result := OldSyncWord = 0;
  end
else Result := False;
If not Result then
  InterlockedExchangeSub(SyncWord,FL_CS_ACQUIRE_DELTA);
end;

//------------------------------------------------------------------------------

Function _FastCriticalSectionWaitToEnter(var SyncWord: TFLSyncWord; Timeout: UInt32; WaitDelayMethod: TFLWaitDelayMethod; WaitSpinCount, SpinDelayCount: UInt32; CounterFrequency: Int64): TFLWaitResult;
var
  WaitParams: TFLWaitParams;
begin
WaitParams.SyncWordPtr := @SyncWord;
WaitParams.Timeout := Timeout;
WaitParams.WaitDelayMethod := WaitDelayMethod;
WaitParams.WaitSpinCount := WaitSpinCount;
WaitParams.SpinDelayCount := SpinDelayCount;
WaitParams.Reserve := True;
WaitParams.FceReserve := _FastCriticalSectionReserve;
WaitParams.FceUnreserve := _FastCriticalSectionUnreserve;
WaitParams.FceAcquire := _FastCriticalSectionEnter;
WaitParams.CounterFrequency := CounterFrequency;
WaitParams.StartCount := 0;
Result := _DoWait(WaitParams);
end;

{-------------------------------------------------------------------------------
    Fast critical section - public functions
-------------------------------------------------------------------------------}

procedure FastCriticalSectionInit(out SyncWord: TFLSyncWord);
begin
{$IFDEF SyncWord64}
InterlockedStore64(@SyncWord,FL_UNLOCKED);
{$ELSE}
InterlockedStore32(@SyncWord,FL_UNLOCKED);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure FastCriticalSectionFinal(var SyncWord: TFLSyncWord);
begin
InterlockedStore(SyncWord,FL_INVALID);
end;

//------------------------------------------------------------------------------

Function FastCriticalSectionEnter(var SyncWord: TFLSyncWord): Boolean;
var
  FailedDueToReservation: Boolean;
begin
Result := _FastCriticalSectionEnter(SyncWord,False,FailedDueToReservation);
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure FastCriticalSectionLeave(var SyncWord: TFLSyncWord);
begin
ReadWriteBarrier;
InterlockedExchangeSub(SyncWord,FL_CS_ACQUIRE_DELTA);
end;

//------------------------------------------------------------------------------

Function FastCriticalSectionSpinToEnter(var SyncWord: TFLSyncWord; SpinCount: UInt32; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;
var
  SpinParams: TFLSpinParams;
begin
SpinParams.SyncWordPtr := @SyncWord;
SpinParams.SpinCount := SpinCount;
SpinParams.SpinDelayCount := SpinDelayCount;
SpinParams.Reserve := True;
SpinParams.Reserved := False;
SpinParams.FceReserve := _FastCriticalSectionReserve;
SpinParams.FceUnreserve := _FastCriticalSectionUnreserve;
SpinParams.FceAcquire := _FastCriticalSectionEnter;
Result := _DoSpin(SpinParams);
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

Function FastCriticalSectionWaitToEnter(var SyncWord: TFLSyncWord; Timeout: UInt32; WaitDelayMethod: TFLWaitDelayMethod = wdSpin;
  WaitSpinCount: UInt32 = FL_DEF_WAIT_SPIN_CNT; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;
var
  CounterFrequency: Int64;
begin
If GetCounterFrequency(CounterFrequency) then
  begin
    Result := _FastCriticalSectionWaitToEnter(SyncWord,Timeout,WaitDelayMethod,WaitSpinCount,SpinDelayCount,CounterFrequency);
    ReadWriteBarrier;
  end
else raise EFLCounterError.CreateFmt('FastCriticalSectionWaitToEnter: Cannot obtain counter frequency (0x%.8x).',
                                     [{$IFDEF Windows}GetLastError{$ELSE}errno{$ENDIF}]);
end;

{===============================================================================
--------------------------------------------------------------------------------
                              TFastCriticalSection
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastCriticalSection - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFastCriticalSection - protected methods
-------------------------------------------------------------------------------}

procedure TFastCriticalSection.Initialize(SyncWordPtr: PFLSyncWord);
begin
inherited Initialize(SyncWordPtr);
If fOwnsSyncWord then
  FastCriticalSectionInit(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastCriticalSection.Finalize;
begin
If fOwnsSyncWord then
  FastCriticalSectionFinal(fSyncWordPtr^);
inherited;
end;

{-------------------------------------------------------------------------------
    TFastCriticalSection - public methods
-------------------------------------------------------------------------------}

Function TFastCriticalSection.Enter: Boolean;
begin
Result := FastCriticalSectionEnter(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastCriticalSection.Leave;
begin
FastCriticalSectionLeave(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastCriticalSection.SpinToEnter(SpinCount: UInt32): TFLWaitResult;
begin
Result := FastCriticalSectionSpinToEnter(fSyncWordPtr^,SpinCount,GetSpinDelayCount);
end;

//------------------------------------------------------------------------------

Function TFastCriticalSection.WaitToEnter(Timeout: UInt32): TFLWaitResult;
begin
Result := _FastCriticalSectionWaitToEnter(fSyncWordPtr^,Timeout,GetWaitDelayMethod,GetWaitSpinCount,GetSpinDelayCount,fCounterFreq);
ReadWriteBarrier;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    Fast MREW
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Fast MREW - implementation constants
===============================================================================}
{
  Meaning of bits in sync word for fast MREW:

    32bit     64bit
     0..11     0..31    - read count
    12..21    32..47    - write count
    22..31    48..63    - write reserve count
}
const
{$IFDEF SyncWord64}

  FL_MREW_READ_DELTA = TFLSyncWord($0000000000000001);
  FL_MREW_READ_MASK  = TFLSyncWord($00000000FFFFFFFF);
  FL_MREW_READ_MAX   = TFLSyncWord(2147483647); // 0x7FFFFFFF
  FL_MREW_READ_SHIFT = 0;

  FL_MREW_WRITE_DELTA = TFLSyncWord($0000000100000000);
  FL_MREW_WRITE_MASK  = TFLSyncWord($0000FFFF00000000);
  FL_MREW_WRITE_MAX   = TFLSyncWord(32767);     // 0x7FFF
  FL_MREW_WRITE_SHIFT = 32;

  FL_MREW_RESERVE_DELTA = TFLSyncWord($0001000000000000);
  FL_MREW_RESERVE_MASK  = TFLSyncWord($FFFF000000000000);
  FL_MREW_RESERVE_MAX   = TFLSyncWord(32767);   // 0x7FFF
  FL_MREW_RESERVE_SHIFT = 48;

{$ELSE}

  FL_MREW_READ_DELTA = TFLSyncWord($00000001);
  FL_MREW_READ_MASK  = TFLSyncWord($00000FFF);
  FL_MREW_READ_MAX   = TFLSyncWord(2047);       // 0x7FF
  FL_MREW_READ_SHIFT = 0;

  FL_MREW_WRITE_DELTA = TFLSyncWord($00001000);
  FL_MREW_WRITE_MASK  = TFLSyncWord($003FF000);
  FL_MREW_WRITE_MAX   = TFLSyncWord(512);       // 0x1FF
  FL_MREW_WRITE_SHIFT = 12;

  FL_MREW_RESERVE_DELTA = TFLSyncWord($00400000);
  FL_MREW_RESERVE_MASK  = TFLSyncWord($FFC00000);
  FL_MREW_RESERVE_MAX   = TFLSyncWord(512);     // 0x1FF
  FL_MREW_RESERVE_SHIFT = 22;  

{$ENDIF}

{===============================================================================
    Fast MREW - procedural interface implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Fast MREW - internal functions
-------------------------------------------------------------------------------}

Function _FastMREWReserveWrite(var SyncWord: TFLSyncWord): Boolean;
var
  OldSyncWord:  TFLSyncWord;
begin
OldSyncWord := InterlockedExchangeAdd(SyncWord,FL_MREW_RESERVE_DELTA);
// note that reservation is allowed if there are readers, but no new reader can enter
Result := ((OldSyncWord and FL_MREW_RESERVE_MASK) shr FL_MREW_RESERVE_SHIFT) < FL_MREW_RESERVE_MAX;
If not Result then
  InterlockedExchangeSub(SyncWord,FL_MREW_RESERVE_DELTA);
end;

//------------------------------------------------------------------------------

procedure _FastMREWUnreserveWrite(var SyncWord: TFLSyncWord);
begin
InterlockedExchangeSub(SyncWord,FL_MREW_RESERVE_DELTA);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function _FastMREWBeginRead(var SyncWord: TFLSyncWord; Reserved: Boolean; out FailedDueToReservation: Boolean): Boolean;
var
  OldSyncWord:  TFLSyncWord;
begin
FailedDueToReservation := False;
OldSyncWord := InterlockedExchangeAdd(SyncWord,FL_MREW_READ_DELTA);
{
  Do not mask or shift the read count. If there is any writer or reservation
  (which would manifest as count being above reader maximum), straight up fail.
}
If OldSyncWord >= FL_MREW_READ_MAX then
  begin
    InterlockedExchangeSub(SyncWord,FL_MREW_READ_DELTA);
    // indicate whether this failed solely due to reservation
    FailedDueToReservation := (((OldSyncWord and FL_MREW_WRITE_MASK) shr FL_MREW_WRITE_SHIFT) = 0) and
                              (((OldSyncWord and FL_MREW_RESERVE_MASK) shr FL_MREW_RESERVE_SHIFT) <> 0);
    Result := False;
  end
else Result := True;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function _FastMREWWaitToRead(var SyncWord: TFLSyncWord; Timeout: UInt32; WaitDelayMethod: TFLWaitDelayMethod; WaitSpinCount, SpinDelayCount: UInt32; CounterFrequency: Int64): TFLWaitResult;
var
  WaitParams: TFLWaitParams;
begin
WaitParams.SyncWordPtr := @SyncWord;
WaitParams.Timeout := Timeout;
WaitParams.WaitDelayMethod := WaitDelayMethod;
WaitParams.WaitSpinCount := WaitSpinCount;
WaitParams.SpinDelayCount := SpinDelayCount;
WaitParams.Reserve := False;
WaitParams.FceReserve := _FastMREWReserveWrite;
WaitParams.FceUnreserve := _FastMREWUnreserveWrite;
WaitParams.FceAcquire := _FastMREWBeginRead;
WaitParams.CounterFrequency := CounterFrequency;
WaitParams.StartCount := 0;
Result := _DoWait(WaitParams);
end;

//------------------------------------------------------------------------------

Function _FastMREWBeginWrite(var SyncWord: TFLSyncWord; Reserved: Boolean; out FailedDueToReservation: Boolean): Boolean;
var
  OldSyncWord:  TFLSyncWord;
begin
FailedDueToReservation := False;
OldSyncWord := InterlockedExchangeAdd(SyncWord,FL_MREW_WRITE_DELTA);
// there can be no reader if writer is to be allowed to enter
If (((OldSyncWord and FL_MREW_READ_MASK) shr FL_MREW_READ_SHIFT) = 0) and
   (((OldSyncWord and FL_MREW_WRITE_MASK) shr FL_MREW_WRITE_SHIFT) < FL_MREW_WRITE_MAX) then
  begin
    If Reserved then
      Result := ((OldSyncWord and FL_MREW_WRITE_MASK) shr FL_MREW_WRITE_SHIFT = 0) and
                ((OldSyncWord and FL_MREW_RESERVE_MASK) shr FL_MREW_RESERVE_SHIFT <> 0)
    else
      Result := OldSyncWord = 0;
  end
else Result := False;
If not Result then
  InterlockedExchangeSub(SyncWord,FL_MREW_WRITE_DELTA);
end;

//------------------------------------------------------------------------------

Function _FastMREWWaitToWrite(var SyncWord: TFLSyncWord; Timeout: UInt32; WaitDelayMethod: TFLWaitDelayMethod; WaitSpinCount, SpinDelayCount: UInt32; CounterFrequency: Int64): TFLWaitResult;
var
  WaitParams: TFLWaitParams;
begin
WaitParams.SyncWordPtr := @SyncWord;
WaitParams.Timeout := Timeout;
WaitParams.WaitDelayMethod := WaitDelayMethod;
WaitParams.WaitSpinCount := WaitSpinCount;
WaitParams.SpinDelayCount := SpinDelayCount;
WaitParams.Reserve := True;
WaitParams.FceReserve := _FastMREWReserveWrite;
WaitParams.FceUnreserve := _FastMREWUnreserveWrite;
WaitParams.FceAcquire := _FastMREWBeginWrite;
WaitParams.CounterFrequency := CounterFrequency;
WaitParams.StartCount := 0;
Result := _DoWait(WaitParams);
end;

{-------------------------------------------------------------------------------
    Fast MREW - public functions
-------------------------------------------------------------------------------}

procedure FastMREWInit(out SyncWord: TFLSyncWord);
begin
{$IFDEF SyncWord64}
InterlockedStore64(@SyncWord,FL_UNLOCKED);
{$ELSE}
InterlockedStore32(@SyncWord,FL_UNLOCKED);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure FastMREWFinal(var SyncWord: TFLSyncWord);
begin
InterlockedStore(SyncWord,FL_INVALID);
end;

//------------------------------------------------------------------------------

Function FastMREWBeginRead(var SyncWord: TFLSyncWord): Boolean;
var
  FailedDueToReservation: Boolean;
begin
Result := _FastMREWBeginRead(SyncWord,False,FailedDueToReservation);
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure FastMREWEndRead(var SyncWord: TFLSyncWord);
begin
ReadWriteBarrier;
InterlockedExchangeSub(SyncWord,FL_MREW_READ_DELTA);
end;

//------------------------------------------------------------------------------

Function FastMREWSpinToRead(var SyncWord: TFLSyncWord; SpinCount: UInt32; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;
var
  SpinParams: TFLSpinParams;
begin
SpinParams.SyncWordPtr := @SyncWord;
SpinParams.SpinCount := SpinCount;
SpinParams.SpinDelayCount := SpinDelayCount;
SpinParams.Reserve := False;
SpinParams.Reserved := False;
SpinParams.FceReserve := _FastMREWReserveWrite;
SpinParams.FceUnreserve := _FastMREWUnreserveWrite;
SpinParams.FceAcquire := _FastMREWBeginRead;
Result := _DoSpin(SpinParams);
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

Function FastMREWWaitToRead(var SyncWord: TFLSyncWord; Timeout: UInt32; WaitDelayMethod: TFLWaitDelayMethod = wdSpin;
  WaitSpinCount: UInt32 = FL_DEF_WAIT_SPIN_CNT; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;
var
  CounterFrequency: Int64;
begin
If GetCounterFrequency(CounterFrequency) then
  begin
    Result := _FastMREWWaitToRead(SyncWord,Timeout,WaitDelayMethod,WaitSpinCount,SpinDelayCount,CounterFrequency);
    ReadWriteBarrier;
  end
else raise EFLCounterError.CreateFmt('FastMREWWaitToRead: Cannot obtain counter frequency (0x%.8x).',
                                     [{$IFDEF Windows}GetLastError{$ELSE}errno{$ENDIF}]);
end;

//------------------------------------------------------------------------------

Function FastMREWBeginWrite(var SyncWord: TFLSyncWord): Boolean;
var
  FailedDueToReservation: Boolean;
begin
Result := _FastMREWBeginWrite(SyncWord,False,FailedDueToReservation);
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure FastMREWEndWrite(var SyncWord: TFLSyncWord);
begin
ReadWriteBarrier;
InterlockedExchangeSub(SyncWord,FL_MREW_WRITE_DELTA);
end;

//------------------------------------------------------------------------------

Function FastMREWSpinToWrite(var SyncWord: TFLSyncWord; SpinCount: UInt32; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;
var
  SpinParams: TFLSpinParams;
begin
SpinParams.SyncWordPtr := @SyncWord;
SpinParams.SpinCount := SpinCount;
SpinParams.SpinDelayCount := SpinDelayCount;
SpinParams.Reserve := True;
SpinParams.Reserved := False;
SpinParams.FceReserve := _FastMREWReserveWrite;
SpinParams.FceUnreserve := _FastMREWUnreserveWrite;
SpinParams.FceAcquire := _FastMREWBeginWrite;
Result := _DoSpin(SpinParams);
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

Function FastMREWWaitToWrite(var SyncWord: TFLSyncWord; Timeout: UInt32; WaitDelayMethod: TFLWaitDelayMethod = wdSpin;
  WaitSpinCount: UInt32 = FL_DEF_WAIT_SPIN_CNT; SpinDelayCount: UInt32 = FL_DEF_SPIN_DELAY_CNT): TFLWaitResult;
var
  CounterFrequency: Int64;
begin
If GetCounterFrequency(CounterFrequency) then
  begin
    Result := _FastMREWWaitToWrite(SyncWord,Timeout,WaitDelayMethod,WaitSpinCount,SpinDelayCount,CounterFrequency);
    ReadWriteBarrier;
  end
else raise EFLCounterError.CreateFmt('FastMREWWaitToWrite: Cannot obtain counter frequency (0x%.8x).',
                                     [{$IFDEF Windows}GetLastError{$ELSE}errno{$ENDIF}]);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                    TFastMREW
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFastMREW - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFastMREW - protected methods
-------------------------------------------------------------------------------}

procedure TFastMREW.Initialize(SyncWordPtr: PFLSyncWord);
begin
inherited Initialize(SyncWordPtr);
If fOwnsSyncWord then
  FastMREWInit(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastMREW.Finalize;
begin
If fOwnsSyncWord then
  FastMREWFinal(fSyncWordPtr^);
inherited;
end;

{-------------------------------------------------------------------------------
    TFastMREW - public methods
-------------------------------------------------------------------------------}

Function TFastMREW.BeginRead: Boolean;
begin
Result := FastMREWBeginRead(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastMREW.EndRead;
begin
FastMREWEndRead(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastMREW.BeginWrite: Boolean;
begin
Result := FastMREWBeginWrite(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

procedure TFastMREW.EndWrite;
begin
FastMREWEndWrite(fSyncWordPtr^);
end;

//------------------------------------------------------------------------------

Function TFastMREW.SpinToRead(SpinCount: UInt32): TFLWaitResult;
begin
Result := FastMREWSpinToRead(fSyncWordPtr^,SpinCount,GetSpinDelayCount);
end;

//------------------------------------------------------------------------------

Function TFastMREW.WaitToRead(Timeout: UInt32): TFLWaitResult;
begin
Result := _FastMREWWaitToRead(fSyncWordPtr^,Timeout,GetWaitDelayMethod,GetWaitSpinCount,GetSpinDelayCount,fCounterFreq);
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

Function TFastMREW.SpinToWrite(SpinCount: UInt32): TFLWaitResult;
begin
Result := FastMREWSpinToWrite(fSyncWordPtr^,SpinCount,GetSpinDelayCount);
end;

//------------------------------------------------------------------------------

Function TFastMREW.WaitToWrite(Timeout: UInt32): TFLWaitResult;
begin
Result := _FastMREWWaitToWrite(fSyncWordPtr^,Timeout,GetWaitDelayMethod,GetWaitSpinCount,GetSpinDelayCount,fCounterFreq);
ReadWriteBarrier;
end;

end.

