{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SimpleTSC

    Provides means of reading Time-Stamp Counter (TSC) Model-Specific Register
    (MSR), which can be used for high-resolution time measurements.

      NOTE - this register is present only on IA-32 and AMD64 processors.

    Because proper use of TSC requires manipulation of thread affinity and
    potentially also priority, a set of auxiliary functions manipulation those
    thread properties is also provided.

    The TSC register is 64 bits wide, but time-stamp type used here is declared
    as Int64 (that is, a signed value). This could pose problems in comparisons
    and arithmetics, if the bit 63 of the TSC would be set. Therefore, all time-
    stamps returned by implemented functions (with notable exception being
    STSC_GetTSCFull) are masked so that bit 63 (sign bit) is always clear (0).

    The TSC is NOT guaranteed to be present and/or enabled on all systems, and
    most functions also require another instructions (memory fences) which too
    are not guaranteed to be supported everywhere.
    So, in initialization section of this unit, it is discerned whether the
    register is present on the CPU, is enabled (not disabled) by the OS and
    whether other required instructions are also supported.
    Because this unit also provides some auxiliry funtions that do not depend
    on presence of this register, it has been decided to not raise an exception
    if the TSC is not supported.
    That being said, you, as a user of this library, must check whether the TSC
    is fully supported or not before making any call. Do it by checking a set
    returned by function STSC_SupportedFeatures.

      If this set contains tscEnabled, you can safely call only the following
      functions:

        STSC_GetTSC
        STSC_GetTSCFull

      If the set contains tscSupported, you can, in addition, also call
      following functions:

        STSC_GetTSCEnter
        STSC_GetTSCLeave
        STSC_GetTSCFence
        STSC_Start
        STSC_TimePoint
        STSC_End
        STSC_MeasureCall (both overloads)

      If the set does not contain tscEnabled or tscSupported, do not make any
      calls to abovementioned functions.

      Auxiliary functions do not depend on TSC support, so you can call them
      even if the returned set is empty.

    TSC is a counter register, which means it is monotonically incremented each
    cycle, but frequency of of these cycles might not be constant.

      In very old processors (Pentium 4, Pentium M, ...), the register is
      incremented in every internal processor clock cycle. If CPU frequency
      changes, TSC frequency will change with it.

      In newer processors, the register runs at constant rate, but this rate
      can and will change if processor power state changes. Therefore, the
      frequency is not fully contant and the TSC still cannot be used to
      measure real time (eg. microseconds).

      Only if the TSC is implemented as invariant (indicated when tscInvariant
      is in the set returned by STSC_SupportedFeatures), the frequency is truly
      constant over all power states and can therefore be used for time keeping.

    In any way, obtaining current frequency of the TSC can be a major headache,
    therefore this library does not provide it.

    Given all that, SimpleTSC was not created to be a universal time measurement
    tool, it is intended to be used for testing of RELATIVE time or performace
    of code (ie. whether some function is faster than another one), please use
    it as such.

    And finally, be aware that TSC is usually implemented per-core, and there
    is no synchronization between them. This means TSC on one core/logical
    processor can have wildly different value than on other cores.
    So if you run your mesurement on multi-processor system, make sure the
    measurement runs the whole time only on one processor core (use provided
    auxiliary functions to set thread affinity).

  Version 1.1 (2023-10-22)

  Last change (2023-10-22)

  ©2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleTSC

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID

===============================================================================}
unit SimpleTSC;
{
  SimpleTSC_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.

    NOTE - this unit cannot be compiled without asm, it is here for the sake of
           completeness.
}
{$IFDEF SimpleTSC_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported CPU.'}
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
  {$MODESWITCH PointerToProcVar+}
  {$ASMMODE Intel}
{$ENDIF}
{$H+}

{$IF Defined(PurePascal) and not Defined(CompTest)}
  {$MESSAGE WARN 'This unit cannot be compiled without ASM.'}
{$IFEND}

interface

uses
  SysUtils, {$IFNDEF Windows}baseunix,{$ENDIF}
  AuxTypes;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESTSCException = class(Exception);

  ESTSCInvalidValue       = class(ESTSCException);
  ESTSCInvalidState       = class(ESTSCException);
  ESTSCIndexOutOfBounds   = class(ESTSCException);
  ESTSCSystemError        = class(ESTSCException);
  ESTSCCallNotImplemented = class(ESTSCException);

{===============================================================================
--------------------------------------------------------------------------------
                                  TSC functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Core functions - declaration
===============================================================================}
type
{
  tscPresent      indicates that the executing CPU supports Time-Stamp Counter
                  (TSC) Model-Specific Register (MSR) (CPUID.1:EDX.TSC[4] = 1)

  tscEnabled      TSC is enabled by the operating system, ie. instruction
                  RDTSC is not disabled (implies tscPresent)

  tscSupported    instructions LFENCE and MFENCE (both are part of SSE2) are
                  supported by the CPU and OS - they are used in most functions
                  (implies tscEnabled)

  tscInvariant    TSC is invariant, that is, it does not change frequency and
                  can be used to measure real time (CPUID.80000007H:EDX[8] = 1)
                  (implies tscPresent)

  tscSysProcID    function STSC_GetThreadProcessor is using a system call to
                  obtain the thread processor ID
}
  TSTSCSupportedFeature = (tscPresent,tscEnabled,tscSupported,tscInvariant,tscSysProcID);
  TSTSCSupportedFeatures = set of TSTSCSupportedFeature;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  STSC_SupportedFeatures

  Returns set of TSC features supported by the current hardware and operating
  system. See description of TSTSCSupportedFeature type for details about
  individual features.

  Use this function to detect what functions can be safely called.
}
Function STSC_SupportedFeatures: TSTSCSupportedFeatures;

//------------------------------------------------------------------------------
type
  TSTSCTimeStamp = Int64;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  STSC_GetTSC

  Returns value of time-stamp counter (TSC) register.
}
Function STSC_GetTSC: TSTSCTimeStamp; register; assembler;

{
  STSC_GetTSCUnmasked

  Returns full value of time-stamp counter (TSC) register, without masking
  bit #63 in the result.
}
Function STSC_GetTSCFull: TSTSCTimeStamp; register; assembler;

{
  STSC_GetTSCEnter

  Returns value of time-stamp counter (TSC) register while ensuring that the
  instruction reading the value (instruction RDTSC) is executed prior to
  execution of any subsequent instruction (including any memory accesses).
}
Function STSC_GetTSCEnter: TSTSCTimeStamp; register; assembler;

{
  STSC_GetTSCLeave

  Returns value of time-stamp counter (TSC) register while ensuring that the
  instruction reading the value is executed only after all previous instructions
  have executed and all previous memory loads and stores are globally visible.
}
Function STSC_GetTSCLeave: TSTSCTimeStamp; register; assembler;

{
  STSC_GetTSCFence

  Returns value of time-stamp counter (TSC) register while ensuring that the
  instruction reading the value is executed only after all previous instructions
  have executed and all previous memory loads and stores are globally visible
  and, at the same time, prior to execution of any subsequent instruction
  (including any memory accesses).
}
Function STSC_GetTSCFence: TSTSCTimeStamp; register; assembler;

//------------------------------------------------------------------------------
{
  STSC_GetDistance

  Returns distance (forward difference) between two given time-stamps.
  If TimeStampThen is higher than TimeStampNow, it is assumed the lower 63 bits
  of TSC owerflowed (unlikely, but possible) and the distance is calculated as
  such (note that only SINGLE overflow event is assumed, because overflowing
  multiple times would take at least decades, if not centuries - as per Intel's
  documentation, which states that the counter should not overflow within 10
  years).
}
Function STSC_TicksBetween(TimeStampNow,TimeStampThen: TSTSCTimeStamp): TSTSCTimeStamp;


{===============================================================================
    Continuous measurement - declaration
===============================================================================}
{
  Use following types and functions for standard and continuous measurement
  (measurement of several sequential intervals).

    For example, if you want to measure three intervals, do following:

      STSC_Start(Measurement,2);
      -first_interval-
      STSC_TimePoint(Measurement,0);
      -second_interval-
      STSC_TimePoint(Measurement,1);
      -third_interval-
      STSC_End(Measurement);

    You can then get the distances (intervals length) this way:

       first ... Measurement.TimePoints[0].DistanceFromPrevious
      second ... Measurement.TimePoints[1].DistanceFromPrevious
       third ... Measurement.EndTimePoints.DistanceFromPrevious

  WARNING -  these functions have unavoidable overhead (they take some time to
             execute), so if you want to do more precise measurements, use core
             functions instead and manage the time-stamps yourself.
}
type
  TSTSCTimePoint = record
    TimeStamp:            TSTSCTimeStamp;
    IsAssigned:           Boolean;
    DistanceFromStart:    TSTSCTimeStamp;
    DistanceFromPrevious: TSTSCTimeStamp;
  end;

  TSTSCMeasurement = record
    Initialized:    Boolean;
    StartTimeStamp: TSTSCTimeStamp;
    TimePoints:     array of TSTSCTimePoint;
    EndTimePoint:   TSTSCTimePoint;
  end;
  PSTSCMeasurement = ^TSTSCMeasurement;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  STSC_Start

  Initializes Measurement structure, allocates array of time points and then
  obtains start time-stamp.

  Never set TimePointCount to value below 0, doing so will raise an exception
  of class ESTSCInvalidValue.
}
procedure STSC_Start(out Measurement: TSTSCMeasurement; TimePointCount: Integer = 0);

{
  STSC_TimePoint

  Obtains time-stamp for selected time point. Distances are not calculated here
  for the sake of perforamnce.

  Remember that time point indices are zero-based (they start at zero, end at
  count - 1). Invalid index will raise an ESTSCIndexOutOfBounds exception.

  Measurement must be intialized, otherwise an exception of ESTSCInvalidState
  class is raised.
}
procedure STSC_TimePoint(var Measurement: TSTSCMeasurement; TimePointIndex: Integer);

{
  STSC_End

  Obtains ending time-stamp and then finalizes the Measurement. Also calculates
  distances for endpoint and all assigned time points.

  Measurement must be intialized, otherwise an exception of ESTSCInvalidState
  class is raised.
}
procedure STSC_End(var Measurement: TSTSCMeasurement);


{===============================================================================
    Call measurement - declaration
===============================================================================}
{
  These functions and types are designed to measure short time interval it
  takes to execute a single function call, if resolution of TSC allows it.

  The call must fully conform to signature of prodedural type TSTSCMeasuredCall,
  otherwice the behavior is completely undefined and will most probably result
  in nasty errors or, in the worst case, memory corruption.
}
type
  TSTSCTimeStamps = record
    StartTimeStamp: TSTSCTimeStamp;
    EndTimeStamp:   TSTSCTimeStamp;
  end;
  PSTSCTimeStamps = ^TSTSCTimeStamps;

  TSTSCMeasuredCall = procedure(Param: Pointer); register;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  STSC_MeasureCall

  Measures a time it takes to execute a function referenced by parameter Call.

  Start time-stamp and end time-stamp are stored in TimeStamps variable.

    NOTE - this function is entirely implemented in assembly to minimize
           overhead time when calling the provided function call.
}
procedure STSC_MeasureCall(Call: TSTSCMeasuredCall; CallParam: Pointer; out TimeStamps: TSTSCTimeStamps); overload; register; assembler;

{
  STSC_MeasureCall

  Measures a time it takes to execute a given call and returns the measured
  distance (number of ticks).
}
Function STSC_MeasureCall(Call: TSTSCMeasuredCall; CallParam: Pointer): Int64; overload;


{===============================================================================
--------------------------------------------------------------------------------
                               Auxiliary functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    CPU affinity functions - declaration
===============================================================================}
type
{$IFDEF Windows}
  TSTSCProcessorMask = PtrUInt;
{$ELSE}
  TSTSCProcessorMask = array[0..Pred(128 div SizeOf(PtrUInt))] of PtrUInt;
{$ENDIF}
  PSTSCProcessorMask = ^TSTSCProcessorMask;

  TSTSCProcessID = {$IFDEF Windows}DWORD{$ELSE}pid_t{$ENDIF};
  TSTSCThreadID  = {$IFDEF Windows}DWORD{$ELSE}pid_t{$ENDIF};

{-------------------------------------------------------------------------------
    CPU affinity functions - processor mask manipulation
-------------------------------------------------------------------------------}
{
  STSC_GetProcessorMaskBit

  Returns true when selected Bit in the ProcessorMask is 1, false otherwise.

  When Bit is out of allowable range, an exception of type ESTSCInvalidValue is
  raised.
}
Function STSC_GetProcessorMaskBit(const ProcessorMask: TSTSCProcessorMask; Bit: Integer): Boolean;

{
  STSC_SetProcessorMaskBit

  Sets selected Bit in the ProcessorMask variable to 1.

  When Bit is out of allowable range, an exception of type ESTSCInvalidValue is
  raised.
}
procedure STSC_SetProcessorMaskBit(var ProcessorMask: TSTSCProcessorMask; Bit: Integer);

{
  STSC_ClrProcessorMaskBit

  Sets selected Bit in the ProcessorMask variable to 0.

  When Bit is out of allowable range, an exception of type ESTSCInvalidValue is
  raised.
}
procedure STSC_ClrProcessorMaskBit(var ProcessorMask: TSTSCProcessorMask; Bit: Integer);

{-------------------------------------------------------------------------------
    CPU affinity functions - logical processor availability
-------------------------------------------------------------------------------}
{
  STSC_GetNumberOfProcessors

  Returns number of logical processors currently available in the system.
}
Function STSC_GetNumberOfProcessors: Integer;

{
  STSC_GetAvailableProcessors

  Returns affinity mask of the current process. This can be used to discern
  which CPU(s) can be used by a thread.
}
Function STSC_GetAvailableProcessors: TSTSCProcessorMask;

{
  STSC_ProcessorAvailable

  Returns true when processor of given ID (number) is configured for the
  current process (ie. is present in its affinity mask), false otherwise.

  When ProcessorID is out of allowable range, the function will return false.
}
Function STSC_ProcessorAvailable(ProcessorID: Integer): Boolean;

{-------------------------------------------------------------------------------
    CPU affinity functions - thread affinity
-------------------------------------------------------------------------------}
{
  STSC_GetThreadAffinity

  Returns affinity mask of the calling thread.
}
Function STSC_GetThreadAffinity: TSTSCProcessorMask;

{
  STSC_SetThreadAffinity

  Sets affinity mask of the calling thread according to parameter AffinityMask
  and returns its previous value.
}
Function STSC_SetThreadAffinity(AffinityMask: TSTSCProcessorMask): TSTSCProcessorMask;

{
  STSC_GetThreadProcessor

  Returns processor ID (number) that executed this call (more precisely, the
  system call that obtained the value).

  Linux

    The number is obtained using function sched_getcpu.

  Windows

    There is a function GetCurrentProcessorNumber exported by kernel32.dll, but
    it is available only from Windows Vista onwards (Windows XP 64bit also
    seems to have it), and since I am writing this library so it can run in
    Windows XP 32bit too, use of that function cannot be hardcoded. So, in unit
    initialization, the kernel32.dll is probed for this funtion.

    When it is there, is gets binded and is then used to obtain the number.

    If it is not present, then the number is obtained using SimpleCPUID library
    (which is required by this unit anyway), more specifically from a mapping
    of local APIC IDs to processor numbers (this mapping is constructed at the
    unit initialization). If the APIC ID cannot be mapped then processor 0 is
    returned.
}
Function STSC_GetThreadProcessor: Integer;

{
  STSC_SetThreadProcessor

  Sets affinity of calling thread so that it will run only on the selected
  processor ID and returns previous affinity mask.

  If the selected processor cannot be used (is not configured for current
  process), then an ESTSCInvalidValue exception is raised and affinity is not
  changed.
}
Function STSC_SetThreadProcessor(ProcessorID: Integer): TSTSCProcessorMask;

{===============================================================================
    Priority funtions - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Priority funtions - process priority
-------------------------------------------------------------------------------}
type
{
  TSCSCPriorityClass

  Priority classes are meaningless in Linux.
  
  Values pcProcModeBcgrBegin and pcProcModeBcgrEnd are never returned as
  process priority class. Use them only when setting priority class, but first
  consult Windows SDK documentation for details.
}
  TSCSCPriorityClass = (pcIdle,pcBelowNormal,pcNormal,pcAboveNormal,pcHigh,
                        pcRealtime,pcProcModeBcgrBegin,pcProcModeBcgrEnd);

//------------------------------------------------------------------------------
{
  STSC_GetPriorityClass

  Returns priority class of current process. Has meaning only in Windows OS,
  in Linux it always returns pcNormal.

  For details about priority classes, refer to Windows SDK documentation.
}
Function STSC_GetPriorityClass: TSCSCPriorityClass;

{
  STSC_SetPriorityClass

  Sets priority class of current process and returns its previous value. Has
  meaning only in Windows OS, in Linux it does nothing and always returns
  pcNormal.

  For details about priority classes, refer to Windows SDK documentation.
}
Function STSC_SetPriorityClass(PriorityClass: TSCSCPriorityClass): TSCSCPriorityClass;

{-------------------------------------------------------------------------------
    Priority funtions - thread priority
-------------------------------------------------------------------------------}
type
{
  TSTSCSchedPolicy

  Scheduling policy has meaning only in Linux. There is nothing even vaguely
  similar in Windows.

  For details about sheduling policies, please refer Linux manual.

    NOTE - spNormal and spOther are denoting the same policy (SCHED_OTHER).
}
  TSTSCSchedPolicy = (spUnknown,spNormal,spOther,spBatch,spIdle,spFifo,spRR,spDeadline);

//------------------------------------------------------------------------------
{
  STSC_GetThreadSchedulingPolicy

  Returns current scheduling policy of the calling thread. Has meaning only in
  Linux, in Windows it will always return spUnknown.
}
Function STSC_GetThreadSchedulingPolicy: TSTSCSchedPolicy;

{
  STSC_SetThreadSchedulingPolicy

  Sets selected scheduling policy for the calling thread and returns the
  previous one. Has meaning only in Linux, in Windows it does nothing and
  always returns spUnknown.

  When setting spFifo or spRR policy, the scheduling priority is set to 50.
  In all other cases it is set to zero.

  Note that spDeadline policy cannot be selected using this function (if will
  fail).

    WARNING - Unprivileged process might not be able to change its own
              scheduling policy, depending on current and selected policy
              (eg. it seems to be possible to change from SCHED_OTHER to
              SCHED_IDLE or SCHED_BATCH, I need to do some research into this).
}
Function STSC_SetThreadSchedulingPolicy(SchedulingPolicy: TSTSCSchedPolicy): TSTSCSchedPolicy;

//==============================================================================
{
  STSC_GetSysThreadPriority

  Returns priority of the calling thread as it is represented in the system.

  Windows

    Higher number means higher priority. Numerical value of the priority can
    be in range -15..+15. Value of 0 is normal priority.

    For more details, consult Windows SDK.

  Linux

    Number returned in Linux highly depends on current scheduling priority.

    For spNormal and spOther (which is the same), a value of nice is returned.
    This can be from interval +19..-20, where lower number means higher
    priority. Value of 0 is normal priority.

      NOTE - According to POSIX specification, nice is a per-process setting,
             not per-thread. Most (if not all) current Linux implementations
             diverge from this and implement nice per-thread, meaning each
             thread in a process can have different nice value. But note that
             this can change in the future if Linux moves closer to the
             standard.

      WARNING - Unprivileged process/thread cannot increase its own nice value,
                even if it was lowered previously and now is only returned to
                original value.
                Since kernel 2.6.12, it should be posssible to decrease nice
                value from an unprivileged process depending on a soft limit
                RLIMIT_NICE. But this limit is usually zero anyway, and again
                only privileged process can change this limit, so not much
                changes.

    For spFifo and spRR, the scheduling priority is returned. This can range
    from 1 up to 99, where higher value means higher priority. Value of 50 can
    be considered a normal priority.

    For all other policies, zero is returned as they do not have priorities.
}
Function STSC_GetSysThreadPriority: Integer;

{
  STSC_SetSysThreadPriority

  Sets priority of calling thread and returns its previous value.

  For details about usable values, refer to STSC_GetSysThreadPriority.
}
Function STSC_SetSysThreadPriority(SysThreadPriority: Integer): Integer;

//------------------------------------------------------------------------------
type
{
  TSTSCThreadPriority

  Windows

    All values from this enum can be used in Windows, but note that version of
    Windows running the program might not support all of them (consult Windows
    SDK documentation for details).

      Values tpThrdModeBcgrBegin and tpThrdModeBcgrEnd are never returned as
      thread priority value, but can be used when setting thread priority.

      Values tpLowestRTx and tpHighestRTx are returned and can be set only when
      the current process has pcRealtime priority class (when you set them for
      a different class, an ESTSCInvalidValue exception is raised).
      Number in the name corresponds to a numerical value of underlying system
      thread priority (negative value for tpLowestRTx and positive value for
      tpHighestRTx).

  Linux

    Only values from tpIdle up to tpTimeCritical are valid in Linux.

    If you use tpThrdModeBcgrBegin or tpThrdModeBcgrEnd, then an exception of
    class ESTSCInvalidValue will be raised.

    Values tpLowestRTx are silently converted to tpLowest and tpHighestRTx are
    converted to tpHighest.

    Following table shows how system priority value (either nice or scheduling
    priority) is converted to (getting priority) and from (setting priority)
    type TSTSCThreadPriority. When converting from system value, a range of
    values is converted to a single enum, when converting to system value, the
    enum is converted to a single numerical value.

                      |     getting priority    ||     setting priority    |
                      |-----------------------------------------------------
                      |    nice    |    prio    ||    nice    |    prio    |
    ------------------------------------------------------------------------
      tpIdle          |     19     |      1     ||     19     |      1     |
      tpLowest        |   10..18   |    2..25   ||     18     |      2     |
      tpBelowNormal   |    1..9    |   26..49   ||      9     |     26     |
      tpNormal        |      0     |     50     ||      0     |     50     |
      tpAboveNormal   |  -10..-1   |   51..74   ||    -10     |     74     |
      tpHighest       |  -19..-11  |   75..98   ||    -19     |     98     |
      tpTimeCritical  |    -20     |     99     ||    -20     |     99     |
}
  TSTSCThreadPriority = (tpIdle,tpLowest,tpBelowNormal,tpNormal,tpAboveNormal,
    tpHighest,tpTimeCritical,tpThrdModeBcgrBegin,tpThrdModeBcgrEnd,tpLowestRT7,
    tpLowestRT6,tpLowestRT5,tpLowestRT4,tpLowestRT3,tpHighestRT3,tpHighestRT4,
    tpHighestRT5,tpHighestRT6);

//------------------------------------------------------------------------------
{
  STSC_GetThreadPriority

  Returns calling thread priority converted to TSTSCThreadPriority type. Refer
  to this type for details about the conversion (mainly in Linux).
}
Function STSC_GetThreadPriority: TSTSCThreadPriority;

{
  STSC_SetThreadPriority

  Sets priority of the calling thread and returns its original value.

    WARNING - In Linux, all limitations mentioned in the description of function
              STSC_GetSysThreadPriority still apply here.
}
Function STSC_SetThreadPriority(ThreadPriority: TSTSCThreadPriority): TSTSCThreadPriority;

implementation

uses
{$IFDEF Windows}
  Windows,
{$ELSE}
  syscall, pthreads,
{$ENDIF}
  SimpleCPUID;

{===============================================================================
--------------------------------------------------------------------------------
                                  TSC functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Core functions - implementation
===============================================================================}
var
  VAR_SupportedFeatures:  TSTSCSupportedFeatures = [];

//------------------------------------------------------------------------------

Function STSC_SupportedFeatures: TSTSCSupportedFeatures;
begin
Result := VAR_SupportedFeatures;
end;

//==============================================================================

Function STSC_GetTSC: TSTSCTimeStamp;
asm
{
  RDTSC loads lower 32 bits of TSC (time-stamp counter register) into EAX and
  higher 32 bits into EDX. In 64bit mode it behaves the same and high 32 bits
  of both registers are cleared.

    NOTE - because the stamps are in-here declared as signed 64bit integers
           (unsigned is not fully supported everywhere), the highest bit (#63)
           of the result is masked (removed) to prevent problems in arithmetics
           and comparisons.

  In 32bit environment, the result is returned the same way it is loaded by
  RDTSC (lower 32bits in EAX and higher in EDX). In 64bit, the result is
  returned in RAX register (in all systems).
}
    RDTSC

    AND   EDX, $7FFFFFFF  // mask bit 63 of the result
{$IFDEF x64}
    SHL   RDX, 32
    OR    RAX, RDX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_GetTSCFull: TSTSCTimeStamp; register; assembler;
asm
    RDTSC

{$IFDEF x64}
    SHL   RDX, 32
    OR    RAX, RDX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_GetTSCEnter: TSTSCTimeStamp; register; assembler;
asm
    RDTSC
    LFENCE

    AND   EDX, $7FFFFFFF
{$IFDEF x64}
    SHL   RDX, 32
    OR    RAX, RDX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_GetTSCLeave: TSTSCTimeStamp; register; assembler;
asm
    MFENCE
    LFENCE
    RDTSC

    AND   EDX, $7FFFFFFF
{$IFDEF x64}
    SHL   RDX, 32
    OR    RAX, RDX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_GetTSCFence: TSTSCTimeStamp; register; assembler;
asm
    MFENCE
    LFENCE
    RDTSC
    LFENCE

    AND   EDX, $7FFFFFFF
{$IFDEF x64}
    SHL   RDX, 32
    OR    RAX, RDX
{$ENDIF}
end;

//==============================================================================

Function STSC_TicksBetween(TimeStampNow,TimeStampThen: TSTSCTimeStamp): TSTSCTimeStamp;
begin
If TimeStampNow < TimeStampThen then
  Result := (High(Int64) - TimeStampThen) + TimeStampNow + 1{wraparound}
else
  Result := TimeStampNow - TimeStampThen;
end;


{===============================================================================
    Continuous measurement - implementation
===============================================================================}

procedure STSC_Start(out Measurement: TSTSCMeasurement; TimePointCount: Integer = 0);
begin
SetLength(Measurement.TimePoints,0);
FillChar(Measurement,SizeOf(TSTSCMeasurement),0);
If TimePointCount >= 0 then
  SetLength(Measurement.TimePoints,TimePointCount)
else
  raise ESTSCInvalidValue.CreateFmt('STSC_Start: Invalid time point count (%d).',[TimePointCount]);
Measurement.StartTimeStamp := STSC_GetTSCEnter;
Measurement.Initialized := True;
end;

//------------------------------------------------------------------------------

procedure STSC_TimePoint(var Measurement: TSTSCMeasurement; TimePointIndex: Integer);
begin
If Measurement.Initialized then
  begin
    If (TimePointIndex >= Low(Measurement.TimePoints)) and (TimePointIndex <= High(Measurement.TimePoints)) then
      begin
        Measurement.TimePoints[TimePointIndex].TimeStamp := STSC_GetTSCFence;
        Measurement.TimePoints[TimePointIndex].IsAssigned := True;
      end
    else raise ESTSCIndexOutOfBounds.CreateFmt('STSC_TimePoint: Time point index (%d) out of bounds.',[TimePointIndex]);
  end
else raise ESTSCInvalidState.Create('STSC_TimePoint: Measurement not initialized.');
end;

//------------------------------------------------------------------------------

procedure STSC_End(var Measurement: TSTSCMeasurement);
var
  i:            Integer;
  LastAssigned: Integer;
begin
If Measurement.Initialized then
  begin
    Measurement.EndTimePoint.TimeStamp := STSC_GetTSCLeave;
    Measurement.EndTimePoint.IsAssigned := True;
    // calcualte time distances for time points
    LastAssigned := -1;
    For i := Low(Measurement.TimePoints) to High(Measurement.TimePoints) do
      with Measurement.TimePoints[i] do
        If IsAssigned then
          begin
            DistanceFromStart := STSC_TicksBetween(TimeStamp,Measurement.StartTimeStamp);
            If LastAssigned >= 0 then
              DistanceFromPrevious := STSC_TicksBetween(TimeStamp,Measurement.TimePoints[LastAssigned].TimeStamp)
            else
              DistanceFromPrevious := DistanceFromStart;
            LastAssigned := i;
          end;
    // calculate distances for end point
    with Measurement.EndTimePoint do
      begin
        DistanceFromStart := STSC_TicksBetween(TimeStamp,Measurement.StartTimeStamp);
        If LastAssigned >= 0 then
          DistanceFromPrevious := STSC_TicksBetween(TimeStamp,Measurement.TimePoints[LastAssigned].TimeStamp)
        else
         DistanceFromPrevious := DistanceFromStart;
      end;
  end
else raise ESTSCInvalidState.Create('STSC_End: Measurement not initialized.');
end;


{===============================================================================
    Call measurement - implementation
===============================================================================}

procedure STSC_MeasureCall(Call: TSTSCMeasuredCall; CallParam: Pointer; out TimeStamps: TSTSCTimeStamps);
asm
{-------------------------------------------------------------------------------
  Parameters are passed as such:

                          Win32/Lin32   Win64     Lin64
                  Call        EAX        RCX       RDI
             CallParam        EDX        RDX       RSI
      Addr(TimeStamps)        ECX        R8        RDX
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    // 64bit Windows

    PUSH  RSI
    PUSH  RDI
    PUSH  R8          // Addr(TimeStamps)

    // align stack (16B/128b alignment) and allocate shadow space
    MOV   R9, RSP
    SUB   RSP, 8
    AND   RSP, $FFFFFFFFFFFFFFF0
    MOV   qword ptr [RSP], R9
    SUB   RSP, 32     // allocate shadow space

    MOV   RSI, RDX    // RSI := CallParam

    RDTSC
    LFENCE

    XCHG  RAX, RSI    // RAX := CallParam   RSI := TSC[0..31]
    MOV   RDI, RDX    // RDI := TSC[32..63]
    XCHG  RAX, RCX    // RAX := Call        RCX := CallParam

    CALL  RAX         // Call[RAX](CallParam[RCX])

    MFENCE
    LFENCE
    RDTSC

    ADD   RSP, 32     // remove shadow space
    POP   RSP
    POP   RCX         // Addr(TimeStamps)

    AND   EDX, $7FFFFFFF
    AND   EDI, $7FFFFFFF
    MOV   dword ptr [RCX], ESI      // lower 32bits of start TSC
    MOV   dword ptr [RCX + 4], EDI  // higher 32bits of start TSC
    MOV   dword ptr [RCX + 8], EAX  // lower 32bits of end TSC
    MOV   dword ptr [RCX + 12], EDX // higher 32bits of end TSC

    POP   RDI
    POP   RSI

  {$ELSE}//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // 64bit Linux

    PUSH  R12
    PUSH  R13
    PUSH  RDX         // Addr(TimeStamps)

    XCHG  RDI, RSI    // RDI := CallParam   RSI := Call

    // align stack (16B/128b alignment)
    MOV   R9, RSP
    SUB   RSP, 8
    AND   RSP, $FFFFFFFFFFFFFFF0
    MOV   qword ptr [RSP], R9

    RDTSC
    LFENCE

    MOV   R12, RAX    // R12 := TSC[0..31]
    MOV   R13, RDX    // R13 := TSC[31..63]

    CALL  RSI         // Call[RSI](CallParam[RDI])

    MFENCE
    LFENCE
    RDTSC

    POP   RSP
    POP   RCX         // Addr(TimeStamps)

    AND   R13D, $7FFFFFFF
    AND   EDX, $7FFFFFFF
    MOV   dword ptr [RCX], R12D     // lower 32bits of start TSC
    MOV   dword ptr [RCX + 4], R13D // higher 32bits of start TSC
    MOV   dword ptr [RCX + 8], EAX  // lower 32bits of end TSC
    MOV   dword ptr [RCX + 12], EDX // higher 32bits of end TSC

    POP   R13
    POP   R12

  {$ENDIF}
{$ELSE}//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // 32bit Windows and Linux

    PUSH  ESI
    PUSH  EDI
    PUSH  ECX         // Addr(TimeStamps)

  {$IFNDEF Windows}
    // align stack for linux (16B/128b alignment)
    MOV   ECX, ESP
    SUB   ESP, 4
    AND   ESP, $FFFFFFF0
    MOV   dword ptr [ESP], ECX
  {$ENDIF}

    MOV   EDI, EAX    // EDI := Call
    MOV   ESI, EDX    // ESI := CallParam

    RDTSC
    LFENCE

    XCHG  EAX, ESI    // EAX := CallParam   ESI := TSC[0..31]
    XCHG  EDX, EDI    // EDX := Call        EDI := TSC[32..63]

    CALL  EDX         // Call[EDX](CallParam[EAX])

    MFENCE
    LFENCE
    RDTSC

  {$IFNDEF Windows}
    POP   ESP
  {$ENDIF}
    POP   ECX         // Addr(TimeStamps)

    AND   EDX, $7FFFFFFF
    AND   EDI, $7FFFFFFF
    MOV   dword ptr [ECX], ESI      // lower 32bits of start TSC
    MOV   dword ptr [ECX + 4], EDI  // higher 32bits of start TSC
    MOV   dword ptr [ECX + 8], EAX  // lower 32bits of end TSC
    MOV   dword ptr [ECX + 12], EDX // higher 32bits of end TSC

    POP   EDI
    POP   ESI

{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function STSC_MeasureCall(Call: TSTSCMeasuredCall; CallParam: Pointer): Int64;
var
  TimeStamps: TSTSCTimeStamps;
begin
STSC_MeasureCall(Call,CallParam,TimeStamps);
Result := STSC_TicksBetween(TimeStamps.EndTimeStamp,TimeStamps.StartTimeStamp);
end;


{===============================================================================
--------------------------------------------------------------------------------
                               Auxiliary functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    External/system functions
===============================================================================}

{$IFDEF Windows}

Function GetProcessAffinityMask(hProcess: THandle; lpProcessAffinityMask,lpSystemAffinityMask: PPtrUInt): BOOL; stdcall; external kernel32;
procedure GetNativeSystemInfo(lpSystemInfo: PSystemInfo); stdcall; external kernel32;

{$ELSE}

Function getpid: pid_t; cdecl; external;

Function errno_ptr: pcInt; cdecl; external name '__errno_location';

Function sched_getaffinity(pid: pid_t; cpusetsize: size_t; mask: PCPUSet): cint; cdecl; external;
Function sched_setaffinity(pid: pid_t; cpusetsize: size_t; mask: PCPUSet): cint; cdecl; external;
Function sched_getcpu: cInt; cdecl; external;
Function sched_getscheduler(pid: pid_t): cint; cdecl; external;
Function sched_setscheduler(pid: pid_t; policy: cint; param: psched_param): cint; cdecl; external;
Function sched_getparam(pid: pid_t; param: psched_param): cint; cdecl; external;
Function sched_setparam(pid: pid_t; param: psched_param): cint; cdecl; external;

Function sysconf(name: cInt): cLong; cdecl; external;

Function getpriority(which: cInt; who: cInt): cInt; cdecl external;
Function setpriority(which: cInt; who: cInt; prio: cInt): cInt; cdecl external;

const
  SCHED_OTHER    = 0;
  SCHED_FIFO     = 1;
  SCHED_RR       = 2;
  SCHED_BATCH    = 3;
  SCHED_IDLE     = 5;
//SCHED_DEADLINE = ;  // need to get the number...

  _SC_NPROCESSORS_ONLN = 84;

{$ENDIF}

{===============================================================================
    Internal functions
===============================================================================}
{$IFDEF Windows}
var
  VAR_ProcessorIDMap: record
    Available:  Boolean;
    ProcNums:   array[Byte{Local APIC ID is used as index}] of Integer;
  end;

//------------------------------------------------------------------------------  

Function GetCurrentProcessorNumberCPUID: DWORD; stdcall;
var
  CPUIDResult:  TCPUIDResult;
  Temp:         Integer;
begin
If VAR_ProcessorIDMap.Available then
  begin
    // if VAR_ProcessorIDMap.Available is true then CPUID is supported
    CPUID(1,@CPUIDResult);
    Temp := VAR_ProcessorIDMap.ProcNums[Byte(CPUIDResult.EBX shr 24)];
    If Temp >= 0 then
      Result := DWORD(Temp)
    else
      Result := 0;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------
var
  VAR_GetCurrentProcessorNumber: Function: DWORD; stdcall = GetCurrentProcessorNumberCPUID;

{$ELSE}//-----------------------------------------------------------------------

Function gettid: pid_t;
begin
Result := do_syscall(syscall_nr_gettid);
end;

//------------------------------------------------------------------------------
threadvar
  ThrErrorCode: cInt;

//------------------------------------------------------------------------------

Function CheckErr(ReturnedValue: cInt): Boolean;
begin
Result := ReturnedValue = 0;
If Result then
  ThrErrorCode := 0
else
  ThrErrorCode := errno_ptr^;
end;

//------------------------------------------------------------------------------

Function GetLastError: Integer;
begin
Result := Integer(ThrErrorCode);
end;

{$ENDIF}

{===============================================================================
    CPU affinity functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    CPU affinity functions - processor mask manipulation
-------------------------------------------------------------------------------}

Function STSC_GetProcessorMaskBit(const ProcessorMask: TSTSCProcessorMask; Bit: Integer): Boolean;
begin
If (Bit >= 0) and (Bit < (SizeOf(TSTSCProcessorMask) * 8)) then
{$IFDEF Windows}
  Result := ((ProcessorMask shr Bit) and 1) <> 0
{$ELSE}
  {$IFDEF x64}
  Result := ((ProcessorMask[Bit shr 6] shr (Bit and 63)) and 1) <> 0
  {$ELSE}
  Result := ((ProcessorMask[Bit shr 5] shr (Bit and 31)) and 1) <> 0
  {$ENDIF}
{$ENDIF}
else
  raise ESTSCInvalidValue.CreateFmt('STSC_GetProcessorMaskBit: Invalid bit (%d) selected.',[Bit]);
end;

//------------------------------------------------------------------------------

procedure STSC_SetProcessorMaskBit(var ProcessorMask: TSTSCProcessorMask; Bit: Integer);
begin
If (Bit >= 0) and (Bit < (SizeOf(TSTSCProcessorMask) * 8)) then
{$IFDEF Windows}
  ProcessorMask := ProcessorMask or PtrUInt(PtrUInt(1) shl Bit)
{$ELSE}
  {$IFDEF x64}
  ProcessorMask[Bit shr 6] := ProcessorMask[Bit shr 6] or PtrUInt(PtrUInt(1) shl (Bit and 63))
  {$ELSE}
  ProcessorMask[Bit shr 5] := ProcessorMask[Bit shr 5] or PtrUInt(PtrUInt(1) shl (Bit and 31))
  {$ENDIF}
{$ENDIF}
else
  raise ESTSCInvalidValue.CreateFmt('STSC_SetProcessorMaskBit: Invalid bit (%d) selected.',[Bit]);
end;

//------------------------------------------------------------------------------

procedure STSC_ClrProcessorMaskBit(var ProcessorMask: TSTSCProcessorMask; Bit: Integer);
begin
If (Bit >= 0) and (Bit < (SizeOf(TSTSCProcessorMask) * 8)) then
{$IFDEF Windows}
  ProcessorMask := ProcessorMask and not PtrUInt(PtrUInt(1) shl Bit)
{$ELSE}
  {$IFDEF x64}
  ProcessorMask[Bit shr 6] := ProcessorMask[Bit shr 6] and not PtrUInt(PtrUInt(1) shl (Bit and 63))
  {$ELSE}
  ProcessorMask[Bit shr 5] := ProcessorMask[Bit shr 5] and not PtrUInt(PtrUInt(1) shl (Bit and 31))
  {$ENDIF}
{$ENDIF}
else
  raise ESTSCInvalidValue.CreateFmt('STSC_ClrProcessorMaskBit: Invalid bit (%d) selected.',[Bit]);
end;

{-------------------------------------------------------------------------------
    CPU affinity functions - logical processor availability
-------------------------------------------------------------------------------}

Function STSC_GetNumberOfProcessors: Integer;
{$IFDEF Windows}
var
  SysInfo:  TSystemInfo;
begin
GetNativeSystemInfo(@SysInfo);
Result := Integer(SysInfo.dwNumberOfProcessors);
If Result < 1 then
  Result := 1;
end;
{$ELSE}
begin
Result := sysconf(_SC_NPROCESSORS_ONLN);
If Result < 1 then
  Result := 1;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function STSC_GetAvailableProcessors: TSTSCProcessorMask;
{$IFDEF Windows}
var
  SystemAffinityMask: TSTSCProcessorMask;
begin
If not GetProcessAffinityMask(GetCurrentProcess,@Result,@SystemAffinityMask) then
{$ELSE}
begin
// sched_getaffinity called with process id (getpid) returns mask of main thread (process mask)
If not CheckErr(sched_getaffinity(getpid,SizeOf(TSTSCProcessorMask),@Result)) then
{$ENDIF}
  raise ESTSCSystemError.CreateFmt('STSC_GetAvailableProcessors: Failed to get process affinity mask (%d).',[Integer(GetLastError)]);
end;

//------------------------------------------------------------------------------

Function STSC_ProcessorAvailable(ProcessorID: Integer): Boolean;
begin
If (ProcessorID >= 0) and (ProcessorID < (SizeOf(TSTSCProcessorMask) * 8)) then
  Result := STSC_GetProcessorMaskBit(STSC_GetAvailableProcessors,ProcessorID)
else
  Result := False;
end;

{-------------------------------------------------------------------------------
    CPU affinity functions - thread affinity
-------------------------------------------------------------------------------}

Function STSC_GetThreadAffinity: TSTSCProcessorMask;
begin
{$IFDEF Windows}
Result := SetThreadAffinityMask(GetCurrentThread,STSC_GetAvailableProcessors);
If Result <> 0 then
  begin
    // restore the original mask
    If SetThreadAffinityMask(GetCurrentThread,Result) = 0 then
      raise ESTSCSystemError.CreateFmt('STSC_GetThreadAffinity: Failed to restore thread affinity mask (%u).',[GetLastError]);
  end
else
{$ELSE}
If not CheckErr(sched_getaffinity(0{calling thread},SizeOf(TSTSCProcessorMask),@Result)) then
{$ENDIF}
  raise ESTSCSystemError.CreateFmt('STSC_GetThreadAffinity: Failed to get thread affinity mask (%d).',[Integer(GetLastError)]);
end;

//------------------------------------------------------------------------------

Function STSC_SetThreadAffinity(AffinityMask: TSTSCProcessorMask): TSTSCProcessorMask;
begin
{$IFDEF Windows}
Result := SetThreadAffinityMask(GetCurrentThread,AffinityMask);
If Result = 0 then
{$ELSE}
Result := STSC_GetThreadAffinity;
If not CheckErr(sched_setaffinity(0{calling thread},SizeOf(TSTSCProcessorMask),@AffinityMask)) then
{$ENDIF}
  raise ESTSCSystemError.CreateFmt('STSC_SetThreadAffinity: Failed to set thread affinity mask (%d).',[Integer(GetLastError)]);
end;

//------------------------------------------------------------------------------

Function STSC_GetThreadProcessor: Integer;
begin
{$IFDEF Windows}
If Assigned(VAR_GetCurrentProcessorNumber) then
  Result := Integer(VAR_GetCurrentProcessorNumber)
else
  raise ESTSCCallNotImplemented.Create('STSC_GetThreadProcessor: Cannot obtain thread processor ID.');
{$ELSE}
Result := Integer(sched_getcpu);
If Result = -1{error} then
  raise ESTSCSystemError.CreateFmt('STSC_GetThreadProcessor: Cannot obtain thread processor ID (%d).',[errno_ptr^]);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_SetThreadProcessor(ProcessorID: Integer): TSTSCProcessorMask;
var
  AffinityMask: TSTSCProcessorMask;
begin
If STSC_ProcessorAvailable(ProcessorID) then
  begin
    FillChar(Addr(AffinityMask)^,SizeOf(TSTSCProcessorMask),0);
    STSC_SetProcessorMaskBit(AffinityMask,ProcessorID);
    Result := STSC_SetThreadAffinity(AffinityMask);
  end
else raise ESTSCInvalidValue.CreateFmt('STSC_SetThreadProcessor: Selected processor (%d) not available.',[ProcessorID]);
end;


{===============================================================================
    Priority funtions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Priority funtions - process priority
-------------------------------------------------------------------------------}
{$IFDEF Windows}
const
{
  Following constants are missing in old Delphi.
}
  BELOW_NORMAL_PRIORITY_CLASS = $00004000;
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;

  PROCESS_MODE_BACKGROUND_BEGIN = $00100000;
  PROCESS_MODE_BACKGROUND_END   = $00200000;

  THREAD_MODE_BACKGROUND_BEGIN = $00010000;
  THREAD_MODE_BACKGROUND_END   = $00020000;
{$ENDIF}

//------------------------------------------------------------------------------

Function STSC_GetPriorityClass: TSCSCPriorityClass;
{$IFDEF Windows}
var
  SysPriorityClass: DWORD;
begin
SysPriorityClass := GetPriorityClass(GetCurrentProcess);
If SysPriorityClass <> 0 then
  case SysPriorityClass of
    IDLE_PRIORITY_CLASS:          Result := pcIdle;
    BELOW_NORMAL_PRIORITY_CLASS:  Result := pcBelowNormal;
    ABOVE_NORMAL_PRIORITY_CLASS:  Result := pcAboveNormal;
    HIGH_PRIORITY_CLASS:          Result := pcHigh;
    REALTIME_PRIORITY_CLASS:      Result := pcRealtime;
  else
   {NORMAL_PRIORITY_CLASS}
    Result := pcNormal;
  end
else raise ESTSCSystemError.CreateFmt('STSC_GetPriorityClass: Failed to get process priority class (%u).',[GetLastError]);
end;
{$ELSE}
begin
Result := pcNormal;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function STSC_SetPriorityClass(PriorityClass: TSCSCPriorityClass): TSCSCPriorityClass;
{$IFDEF Windows}
var
  SysPriorityClass: DWORD;
begin
Result := STSC_GetPriorityClass;
case PriorityClass of
  pcIdle:               SysPriorityClass := IDLE_PRIORITY_CLASS;
  pcBelowNormal:        SysPriorityClass := BELOW_NORMAL_PRIORITY_CLASS;
  pcNormal:             SysPriorityClass := NORMAL_PRIORITY_CLASS;
  pcAboveNormal:        SysPriorityClass := ABOVE_NORMAL_PRIORITY_CLASS;
  pcHigh:               SysPriorityClass := HIGH_PRIORITY_CLASS;
  pcRealtime:           SysPriorityClass := REALTIME_PRIORITY_CLASS;
  pcProcModeBcgrBegin:  SysPriorityClass := PROCESS_MODE_BACKGROUND_BEGIN;
  pcProcModeBcgrEnd:    SysPriorityClass := PROCESS_MODE_BACKGROUND_END;
else
  raise ESTSCInvalidValue.CreateFmt('STSC_SetPriorityClass: Invalid priority class (%d).',[Ord(PriorityClass)]);
end;
If not SetPriorityClass(GetCurrentProcess,SysPriorityClass) then
  raise ESTSCSystemError.CreateFmt('STSC_SetPriorityClass: Failed to set process priority class (%u).',[GetLastError]);
end;
{$ELSE}
begin
// following "code" is here to prevent FPC warning about unused parameters
PriorityClass := pcNormal;
Result := PriorityClass;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
    Priority funtions - thread priority
-------------------------------------------------------------------------------}

Function STSC_GetThreadSchedulingPolicy: TSTSCSchedPolicy;
{$IFNDEF Windows}
var
  SchedPolicy:  cInt;
begin
SchedPolicy := sched_getscheduler(0);
case SchedPolicy of
  -1:
    raise ESTSCSystemError.CreateFmt('STSC_GetThreadSchedulingPolicy: Failed to obtain thread scheduling policy (%d).',[errno_ptr^]);
  SCHED_OTHER:    Result := spOther;
  SCHED_FIFO:     Result := spFifo;
  SCHED_RR:       Result := spRR;
  SCHED_BATCH:    Result := spBatch;
  SCHED_IDLE:     Result := spIdle;
//SCHED_DEADLINE: Result := spDeadline;
else
  raise ESTSCInvalidValue.CreateFmt('STSC_GetThreadSchedulingPolicy: Unknown scheduling policy (%d)',[SchedPolicy]);
end;
{$ELSE}
begin
Result := spUnknown;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_SetThreadSchedulingPolicy(SchedulingPolicy: TSTSCSchedPolicy): TSTSCSchedPolicy;
{$IFNDEF Windows}
var
  SchedPolicy:  cInt;
  Param:        sched_param;
begin
Result := STSC_GetThreadSchedulingPolicy;
If Result <> SchedulingPolicy then
  begin
    case SchedulingPolicy of
      spNormal,
      spOther:    SchedPolicy := SCHED_OTHER;
      spFifo:     SchedPolicy := SCHED_FIFO;
      spRR:       SchedPolicy := SCHED_RR;
      spBatch:    SchedPolicy := SCHED_BATCH;
      spIdle:     SchedPolicy := SCHED_IDLE;
    //spDeadline: SchedPolicy := SCHED_DEADLINE;
    else
      raise ESTSCInvalidValue.CreateFmt('STSC_SetThreadSchedulingPolicy: Unknown scheduling policy (%d)',[Ord(SchedulingPolicy)]);
    end;
    If SchedulingPolicy in [spFifo,spRR] then
      Param.sched_priority := 50
    else
      Param.sched_priority := 0;
    If not CheckErr(sched_setscheduler(0,SchedPolicy,@Param)) then
      raise ESTSCSystemError.CreateFmt('STSC_SetThreadSchedulingPolicy: Failed to set thread scheduling policy (%d).',[GetLastError]);
  end;
{$ELSE}
begin
SchedulingPolicy := spUnknown;
Result := SchedulingPolicy;
{$ENDIF}
end;

//==============================================================================

Function STSC_GetSysThreadPriority: Integer;
{$IFDEF Windows}
begin
Result := GetThreadPriority(GetCurrentThread);
If Result = THREAD_PRIORITY_ERROR_RETURN then
  raise ESTSCSystemError.CreateFmt('STSC_GetSysThreadPriority: Failed to get thread priority (%u).',[GetLastError]);
{$ELSE}
var
  SchedPolicy:  TSTSCSchedPolicy;
  Param:        sched_param;
begin
SchedPolicy := STSC_GetThreadSchedulingPolicy;
case SchedPolicy of
  spNormal,spOther: // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    begin
      errno_ptr^ := 0;
      Result := Integer(getpriority(PRIO_PROCESS,0));
      If (Result = -1) and (errno_ptr^ <> 0) then
        raise ESTSCSystemError.CreateFmt('STSC_GetSysThreadPriority: Failed to get nice value (%d).',[errno_ptr^]);
    end;
  spFifo,spRR:  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    If CheckErr(sched_getparam(0,@Param)) then
      Result := Integer(Param.sched_priority)
    else
      raise ESTSCSystemError.CreateFmt('STSC_GetSysThreadPriority: Failed to get scheduling priority (%d).',[GetLastError]);
  spBatch,spIdle,spDeadline:  // - - - - - - - - - - - - - - - - - - - - - - - -
    Result := 0;
else
  raise ESTSCInvalidValue.CreateFmt('STSC_GetSysThreadPriority: Unknown scheduling policy (%d)',[Ord(SchedPolicy)]);
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_SetSysThreadPriority(SysThreadPriority: Integer): Integer;
{$IFNDEF Windows}
var
  SchedPolicy:  TSTSCSchedPolicy;
  Param:        sched_param;
{$ENDIF}
begin
Result := STSC_GetSysThreadPriority;
{$IFDEF Windows}
If not SetThreadPriority(GetCurrentThread,SysThreadPriority) then
  raise ESTSCSystemError.CreateFmt('STSC_SetThreadPriority: Failed to set thread priority (%u).',[GetLastError]);
{$ELSE}
SchedPolicy := STSC_GetThreadSchedulingPolicy;
case SchedPolicy of
  spNormal,spOther: // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    If not CheckErr(setpriority(PRIO_PROCESS,0,cInt(SysThreadPriority))) then
      raise ESTSCSystemError.CreateFmt('STSC_SetSysThreadPriority: Failed to set nice value (%d).',[GetLastError]);
  spFifo,spRR:  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    begin
      Param.sched_priority := cInt(SysThreadPriority);
      If not CheckErr(sched_setparam(0,@Param)) then
        raise ESTSCSystemError.CreateFmt('STSC_SetSysThreadPriority: Failed to set scheduling priority (%d).',[GetLastError]);
    end;
  spBatch,spIdle,spDeadline:  // - - - - - - - - - - - - - - - - - - - - - - - -
    ; // do nothing, these policies do not support priority
else
  raise ESTSCInvalidValue.CreateFmt('STSC_GetSysThreadPriority: Unknown scheduling policy (%d)',[Ord(SchedPolicy)]);
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_GetThreadPriority: TSTSCThreadPriority;
{$IFDEF Windows}
var
  SysThreadPriority:  Integer;
begin
SysThreadPriority := STSC_GetSysThreadPriority;
case SysThreadPriority of
  THREAD_PRIORITY_IDLE:           Result := tpIdle;
  THREAD_PRIORITY_LOWEST:         Result := tpLowest;
  THREAD_PRIORITY_BELOW_NORMAL:   Result := tpBelowNormal;
  THREAD_PRIORITY_NORMAL:         Result := tpNormal;
  THREAD_PRIORITY_ABOVE_NORMAL:   Result := tpAboveNormal;
  THREAD_PRIORITY_HIGHEST:        Result := tpHighest;
  THREAD_PRIORITY_TIME_CRITICAL:  Result := tpTimeCritical;
  THREAD_PRIORITY_ERROR_RETURN:
    raise ESTSCSystemError.CreateFmt('STSC_GetThreadPriority: Failed to get thread priority (%u).',[GetLastError]);
else
  If STSC_GetPriorityClass = pcRealtime then
    case SysThreadPriority of
      -7: Result := tpLowestRT7;
      -6: Result := tpLowestRT6;
      -5: Result := tpLowestRT5;
      -4: Result := tpLowestRT4;
      -3: Result := tpLowestRT3;
       3: Result := tpHighestRT3;
       4: Result := tpHighestRT4;
       5: Result := tpHighestRT5;
       6: Result := tpHighestRT6;
    else
      raise ESTSCInvalidValue.CreateFmt('STSC_GetThreadPriority: Unknown system thread priority (%d).',[SysThreadPriority]);
    end
  else raise ESTSCInvalidValue.CreateFmt('STSC_GetThreadPriority: Unknown system thread priority (%d).',[SysThreadPriority]);
end;
end;
{$ELSE}
var
  SchedPolicy:        TSTSCSchedPolicy;
  SysThreadPriority:  cInt;
begin
SchedPolicy := STSC_GetThreadSchedulingPolicy;
SysThreadPriority := STSC_GetSysThreadPriority;
case SchedPolicy of
  spNormal,spOther: // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    case SysThreadPriority of
           -20: Result := tpTimeCritical;
      -19..-11: Result := tpHighest;
       -10..-1: Result := tpAboveNormal;
             0: Result := tpNormal;
          1..9: Result := tpBelowNormal;
        10..18: Result := tpLowest;
            19: Result := tpIdle;
    else
      raise ESTSCInvalidValue.CreateFmt('STSC_GetThreadPriority: Invalid nice value (%d).',[SysThreadPriority]);
    end;
  spFifo,spRR:  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    case SysThreadPriority of
             1: Result := tpIdle;
         2..25: Result := tpLowest;
        26..49: Result := tpBelowNormal;
            50: Result := tpNormal;
        51..74: Result := tpAboveNormal;
        75..98: Result := tpHighest;
            99: Result := tpTimeCritical;
    else
      raise ESTSCInvalidValue.CreateFmt('STSC_GetThreadPriority: Invalid scheduling priority (%d).',[SysThreadPriority]);
    end;
  spBatch,spIdle,spDeadline:  // - - - - - - - - - - - - - - - - - - - - - - - -
    Result := tpNormal;
else
  raise ESTSCInvalidValue.CreateFmt('STSC_GetThreadPriority: Unknown scheduling policy (%d)',[Ord(SchedPolicy)]);
end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function STSC_SetThreadPriority(ThreadPriority: TSTSCThreadPriority): TSTSCThreadPriority;
{$IFDEF Windows}
var
  SysThreadPriority:  Integer;
begin
Result := STSC_GetThreadPriority;
case ThreadPriority of
  tpIdle:               SysThreadPriority := THREAD_PRIORITY_IDLE;
  tpLowest:             SysThreadPriority := THREAD_PRIORITY_LOWEST;
  tpBelowNormal:        SysThreadPriority := THREAD_PRIORITY_BELOW_NORMAL;
  tpNormal:             SysThreadPriority := THREAD_PRIORITY_NORMAL;
  tpAboveNormal:        SysThreadPriority := THREAD_PRIORITY_ABOVE_NORMAL;
  tpHighest:            SysThreadPriority := THREAD_PRIORITY_HIGHEST;
  tpTimeCritical:       SysThreadPriority := THREAD_PRIORITY_TIME_CRITICAL;
  tpThrdModeBcgrBegin:  SysThreadPriority := THREAD_MODE_BACKGROUND_BEGIN;
  tpThrdModeBcgrEnd:    SysThreadPriority := THREAD_MODE_BACKGROUND_END;
else
  If STSC_GetPriorityClass = pcRealtime then
    case ThreadPriority of
      tpLowestRT7:  SysThreadPriority := -7;
      tpLowestRT6:  SysThreadPriority := -6;
      tpLowestRT5:  SysThreadPriority := -5;
      tpLowestRT4:  SysThreadPriority := -4;
      tpLowestRT3:  SysThreadPriority := -3;
      tpHighestRT3: SysThreadPriority := 3;
      tpHighestRT4: SysThreadPriority := 4;
      tpHighestRT5: SysThreadPriority := 5;
      tpHighestRT6: SysThreadPriority := 6;
    else
      raise ESTSCInvalidValue.CreateFmt('STSC_SetThreadPriority: Invalid thread priority (%d).',[Ord(ThreadPriority)]);
    end
  else raise ESTSCInvalidValue.CreateFmt('STSC_SetThreadPriority: Invalid thread priority (%d).',[Ord(ThreadPriority)]);
end;
STSC_SetSysThreadPriority(SysThreadPriority);
end;
{$ELSE}
var
  SchedPolicy:  TSTSCSchedPolicy;
begin
Result := STSC_GetThreadPriority;
SchedPolicy := STSC_GetThreadSchedulingPolicy;
case SchedPolicy of
  spNormal,spOther: // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    case ThreadPriority of
      tpIdle:         STSC_SetSysThreadPriority(19);
      tpLowestRT7,
      tpLowestRT6,
      tpLowestRT5,
      tpLowestRT4,
      tpLowestRT3,
      tpLowest:       STSC_SetSysThreadPriority(18);
      tpBelowNormal:  STSC_SetSysThreadPriority(9);
      tpNormal:       STSC_SetSysThreadPriority(0);
      tpAboveNormal:  STSC_SetSysThreadPriority(-10);
      tpHighestRT3,
      tpHighestRT4,
      tpHighestRT5,
      tpHighestRT6,
      tpHighest:      STSC_SetSysThreadPriority(-19);
      tpTimeCritical: STSC_SetSysThreadPriority(-20);
    else
      raise ESTSCInvalidValue.CreateFmt('STSC_SetThreadPriority: Invalid thread priority (%d).',[Ord(ThreadPriority)]);
    end;
  spFifo,spRR:  // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    case ThreadPriority of
      tpIdle:         STSC_SetSysThreadPriority(1);
      tpLowestRT7,
      tpLowestRT6,
      tpLowestRT5,
      tpLowestRT4,
      tpLowestRT3,
      tpLowest:       STSC_SetSysThreadPriority(2);
      tpBelowNormal:  STSC_SetSysThreadPriority(26);
      tpNormal:       STSC_SetSysThreadPriority(50);
      tpAboveNormal:  STSC_SetSysThreadPriority(74);
      tpHighestRT3,
      tpHighestRT4,
      tpHighestRT5,
      tpHighestRT6,
      tpHighest:      STSC_SetSysThreadPriority(98);
      tpTimeCritical: STSC_SetSysThreadPriority(99);
    else
      raise ESTSCInvalidValue.CreateFmt('STSC_SetThreadPriority: Invalid thread priority (%d).',[Ord(ThreadPriority)]);
    end;
  spBatch,spIdle,spDeadline:  // - - - - - - - - - - - - - - - - - - - - - - - -
    ; // do nothing
else
  raise ESTSCInvalidValue.CreateFmt('STSC_SetThreadPriority: Unknown scheduling policy (%d)',[Ord(SchedPolicy)]);
end;
end;
{$ENDIF}


{===============================================================================
    Unit initialization
===============================================================================}
{
  CheckRDTSC is here to check whether the RDTSC instruction is enabled (or,
  more precisely, not disabled) by the operating system.

  When it raises an exception, we assume it is disabled. If no exception or
  error is encountered, we assume it is enabled.

  The correct way to discern this would be by checking current protection level
  and probing CR0.PE (bit 0) and CR4.TSD (bit 2). But this is not possible from
  normal application (maybe CR0.PE could be obtained by SMSW instruction),
  because instructions needed for that (eg. "MOV <gpr>, CRx") can only be
  executed at privilege level 0 (only OS can do that).
}
procedure CheckRDTSC; register; assembler;
asm
    RDTSC
end;

//------------------------------------------------------------------------------

{$IFDEF Windows}
procedure BuildProcessorIDMap;
var
  AffinityMask: TSTSCProcessorMask;
  i:            Integer;
  CPUIDResult:  TCPUIDResult;
begin
FillChar(VAR_ProcessorIDMap,SizeOf(VAR_ProcessorIDMap),0);
If CPUIDSupported then
  begin
    VAR_ProcessorIDMap.Available := True;
    // init proc nums with invalid values
    For i := Low(VAR_ProcessorIDMap.ProcNums) to High(VAR_ProcessorIDMap.ProcNums) do
      VAR_ProcessorIDMap.ProcNums[i] := -1;
    // get apic id to cpu number map
    AffinityMask := 1;
    For i := Low(VAR_ProcessorIDMap.ProcNums) to Pred(STSC_GetNumberOfProcessors) do
      begin
        STSC_SetThreadAffinity(AffinityMask);
        CPUID(1,@CPUIDResult);
        VAR_ProcessorIDMap.ProcNums[Byte(CPUIDResult.EBX shr 24)] := i;
        AffinityMask := AffinityMask shl 1;
      end;
  end
else VAR_ProcessorIDMap.Available := False;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure UnitInitialization;
{$IFDEF Windows}
var
  ModuleHandle:     THandle;
  FunctionAddress:  Pointer;
{$ENDIF}
begin
VAR_SupportedFeatures := [];
with TSimpleCPUID.Create do
try
  If Info.ProcessorFeatures.TSC then
    begin
      Include(VAR_SupportedFeatures,tscPresent);
      try
        CheckRDTSC;
        Include(VAR_SupportedFeatures,tscEnabled);
        If Info.SupportedExtensions.SSE2{LFENCE, MFENCE} then
          Include(VAR_SupportedFeatures,tscSupported);
      except
        // eat all exceptions
      end;
      If Info.ExtendedProcessorFeatures.ITSC then
        Include(VAR_SupportedFeatures,tscInvariant);
    end;
finally
  Free;
end;
{$IFDEF Windows}
ModuleHandle := GetModuleHandle('kernel32.dll');
If ModuleHandle <> 0 then
  begin
    FunctionAddress := GetProcAddress(ModuleHandle,'GetCurrentProcessorNumber');
    If Assigned(FunctionAddress) then
      begin
        VAR_GetCurrentProcessorNumber := FunctionAddress;
        Include(VAR_SupportedFeatures,tscSysProcID);
      end
    else BuildProcessorIDMap;
  end
else raise ESTSCSystemError.CreateFmt('UnitInitialization: System library kernel32.dll not loaded (%u).',[GetLastError]);
{$ELSE}
Include(VAR_SupportedFeatures,tscSysProcID);
{$ENDIF}
end;

//------------------------------------------------------------------------------

initialization
  UnitInitialization;

end.
