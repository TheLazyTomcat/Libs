{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Frame Clock

    Frame clock is intended to be used primarily as a mean of measuring distance
    between two points in time with high resolution. But please note that,
    given the implementation, the high resolution does not automatically imply
    high precission, especially on long time intervals.
    It is meant to be used to measure only very short intervals (at most
    minutes). But it can, of course, exists for indefinite time, while only
    measuring the last frame.

    Within this library, the measured point is called just that, a point.
    Space between two points is called a frame (hence frame clock).
    Length of a frame, or time between two points, is called a distance.    

    The measurement is done by obtaining values from monotonic high-resolution
    or performace counters and then calculating difference between them.
    The frame clock stores three such values called creation point, previous
    point and current point. It also stores distance (elapsed time between)
    previous and current point as a current frame.
    Creation point is stored at the object creation. Previous and current
    points are both initialized to the same value as creation point.
    Each time method Tick is called, value from current point is moved to
    previous point and current point is filled by an actual value from
    preformance counter. The current frame distance is also calculated at that
    point.

                                   a frame
                                   |
        creation point          |-----|                 previous point
        |                       |     |                 |
        P-----P-----P-----P-----P-----P-----P-----P-----P-----P...
                                 <--->                     |  |
        time >>>                   |                       |  current point
                                   a distance              |
                                                           current frame

    The distance is measured in ticks. Length of these ticks is implementation
    and system dependent, so do not assume anything about them. It is just a
    number that has meaning only within the object that produced it.
    Do not pass these values between different instances of the frame clock!

    To obtain the distance in usual units, use properties or methods designed
    for that purpose (eg. CurrentFrame, TimeStampDistance, ...) - they,
    in most cases, return value of type TFCTime which contains usable units.

    Frame clock is using performace counter or its alternative that is available
    on the current system for time measurement. If none of such clock or counter
    is available, it defaults to normal time functions - these usually do not
    provide resolution better than few milliseconds.
    Whether the frame clock is running with high resolution can be discerned by
    examining HighResolution property after instantiation.
    If you force high resolution while creating an instance of frame clock, and
    high-res timer is not available on the system, the constructor will raise
    an EFCHighResolutionFail exception. Forcing low resolution cannot fail, it
    just prevents the clock from even attempting to obtain high-res counter.

    Recommended use of the frame clock should look like this:

        Clock := TFrameClock.Create;
        try
          Clock.Tick;
          // measured interval #1
          Clock.Tick;
          // CurrentFrame property now contains length of measured interval #1
          <some_code>
          Clock.Tick;
          // measured interval #2
          Clock.Tick;
          // CurrentFrame property now contains length of measured interval #2
          // measured interval #3
          Clock.Tick;
          // CurrentFrame property now contains length of measured interval #3
          ...
        finally
          Clock.Free;
        end;

    WARNING - Do continuous measurements on one instance of the frame clock
              only. Never use points from previous or paralel class instances,
              it is not guaranteed to work reliably. Measuring across system
              reboots will straight-up fail to produce anything sensible.

  Version 1.0.3 (2024-01-14)

  Last change 2024-01-28

  ©2020-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.FrameClock

  Dependencies:
    AuxClasses    - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxMath       - github.com/TheLazyTomcat/Lib.AuxMath

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol FrameClock_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit FrameClock;
{
  FrameClock_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  FrameClock_UseAuxExceptions to achieve this.
}
{$IF Defined(FrameClock_UseAuxExceptions)}
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
  {$MODESWITCH DuplicateLocals+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes, AuxClasses{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

type
  EFCException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EFCHighResolutionFail = class(EFCException);
  EFCSystemException    = class(EFCException);
  EFCInvalidList        = class(EFCException);
  EFCIndexOutOfBounds   = class(EFCException);

{===============================================================================
--------------------------------------------------------------------------------
                                   TFrameClock
--------------------------------------------------------------------------------
===============================================================================}
type
  TFCTicks = Int64;

  TFCTime = record
    Ticks:  TFCTicks;
    Sec:    Double;   // seconds
    MiS:    Double;   // milliseconds
    UiS:    Double;   // microseconds
    iSec:   Int64;    // integral seconds
    iMiS:   Int64;    // integral milliseconds
    iUiS:   Int64;    // integral microseconds
  end;

  TFCForcedResolution = (frForceHigh,frForceLow,frDontForce);

{===============================================================================
    TFrameClock - class declaration
===============================================================================}
{
  A note on corrections...

  Correction is a distance (in ticks, itself not corrected) between two
  consecutive calls to method Tick. This distance more or less corresponds
  to a time the method Tick requires to execute. It is there for instances
  where it is desirable to remove overhead time from measured intervals.

  When ApplyCorrection property is se to true, value stored in Correction
  property is subtracted from ticks within method FillFromTicks before the
  ticks are converted to normal units. If the correction is larger than the
  ticks, the ticks are set to 0.

  To obtain actual value for correction, call method MeasureCorrection. This
  method reapeatedly measures distance between two calls to Tick and returns
  an average of this time. You can change number of repeated measurements by
  altering Repeats parameter - but note that setting it to large number may
  cause the method to execute for a long time. Note that measuring the
  correction itself does not change any clock properties.

  If you want to activate correction and at the same time set it to measured
  value, call method ApplyMeasuredCorrection.

  By default, ApplyCorrection is set to false and Correction is set to 0.
  You have to change those properties if you want to apply corrections to your
  measurements.

  Correction, when applied, affects all values of type TFCTime returned from
  the clock, including timestamp distances and accumulators in TFrameClockEx.

  Standalone functions hawe corrections disabled.  
}
type
  TFrameClock = class(TCustomMultiListObject)
  protected
    fHighResolution:  Boolean;
    fFrequency:       Int64;
    fResolution:      Int64;
    fFrameCounter:    UInt64;
    fCreationPoint:   TFCTicks;
    fPreviousPoint:   TFCTicks;
    fCurrentPoint:    TFCTicks;
    fCurrentFrame:    TFCTime;
    fApplyCorrection: Boolean;    
    fCorrection:      TFCTicks;
    //- lists methods ---
    Function GetCapacity(List: Integer): Integer; override;
    procedure SetCapacity(List,Value: Integer); override;
    Function GetCount(List: Integer): Integer; override;
    procedure SetCount(List,Value: Integer); override;
    //- other protected methods ---
    procedure InitializeTime(LowResOnly: Boolean); virtual;
    procedure Initialize(ForcedResolution: TFCForcedResolution); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(ForcedResolution: TFCForcedResolution = frDontForce);
    destructor Destroy; override;
    Function LowIndex(List: Integer): Integer; override;
    Function HighIndex(List: Integer): Integer; override;
    Function Tick: TFCTime; virtual;                            // returns frame distance
    Function PointDistance(Point: TFCTicks): TFCTime; virtual;  // time between given point and current point
    Function CreationDistance: TFCTime; virtual;                // time from object creation to current point
    Function PreviousDistance: TFCTime; virtual;                // time from previous point to immediate time
    Function CurrentDistance: TFCTime; virtual;                 // time from current point to immediate time
    //--- utility functions ---
    Function GetActualPoint: TFCTicks; virtual;
    Function GetPointsDifference(A,B: TFCTicks): TFCTicks; virtual;
    Function GetPointsDistance(A,B: TFCTicks): TFCTime; virtual;
    Function MeasureCorrection(Repeats: Integer = 1000): TFCTicks; virtual;
    Function ApplyMeasuredCorrection: TFCTicks; virtual;
    procedure FillFromTicks(var Time: TFCTime); virtual;
    //--- properties ---
    property HighResolution: Boolean read fHighResolution;
    property Frequency: Int64 read fFrequency;                  // [Hz]
    property Resolution: Int64 read fResolution;                // [ns] can be 0, which means resolution better than 1 ns
    property FrameCounter: UInt64 read fFrameCounter;           // number of measured frames
    property CreationPoint: TFCTicks read fCreationPoint;       // time point when an instance was created
    property PreviousPoint: TFCTicks read fPreviousPoint;       // previous time point (second lass call to TickFrame)
    property CurrentPoint: TFCTicks read fCurrentPoint;         // current time point (lass call to TickFrame)
    property CurrentFrame: TFCTime read fCurrentFrame;          // distance between previous and current points
    property ApplyCorrection: Boolean read fApplyCorrection write fApplyCorrection;
    property Correction: TFCTicks read fCorrection write fCorrection;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TFrameClockEx
--------------------------------------------------------------------------------
===============================================================================}

// types and constants for lists...
type
  TFCTimeStamp = record
    Name:     String;
    Value:    TFCTicks; // stores tisck at which the time stamp was made
    UserData: PtrInt;
  end;
  PFCTimeStamp = ^TFCTimeStamp;

  TFCAccumulator = record
    Name:     String;
    Value:    TFCTicks; // stores number of ticks accumulated (ie. a length of time)
    UserData: PtrInt;
  end;
  PFCAccumulator = ^TFCAccumulator;

const
  FCE_LIST_IDX_TIMESTAMPS   = 0;
  FCE_LIST_IDX_ACCUMULATORS = 1;

{===============================================================================
    TFrameClockEx - class declaration
===============================================================================}

type
  TFrameClockEx = class(TFrameClock)
  protected
    fTimeStamps:        array of TFCTimeStamp;
    fTimeStampCount:    Integer;
    fAccumulators:      array of TFCAccumulator;
    fAccumulatorCount:  Integer;
    //- lists getters/setters ---
    Function GetTimeStamp(Index: Integer): TFCTimeStamp; virtual;
    procedure SetTimeStamp(Index: Integer; Value: TFCTimeStamp); virtual;
    Function GetTimeStampPtr(Index: Integer): PFCTimeStamp; virtual;
    Function GetAccumulator(Index: Integer): TFCAccumulator; virtual;
    procedure SetAccumulator(Index: Integer; Value: TFCAccumulator); virtual;
    Function GetAccumulatorPtr(Index: Integer): PFCAccumulator; virtual;
    //- inherited list methods ---
    Function GetCapacity(List: Integer): Integer; override;
    procedure SetCapacity(List,Value: Integer); override;
    Function GetCount(List: Integer): Integer; override;
    procedure SetCount(List,Value: Integer); override;
    //- other methods ---
    procedure Initialize(ForcedResolution: TFCForcedResolution); override;
    procedure Finalize; override;
  public
    constructor Create(ForcedResolution: TFCForcedResolution = frDontForce);
    Function LowIndex(List: Integer): Integer; override;
    Function HighIndex(List: Integer): Integer; override;
    //- timestamps list ---
    Function TimeStampLowIndex: Integer; virtual;
    Function TimeStampHighIndex: Integer; virtual;
    Function TimeStampCheckIndex(Index: Integer): Boolean; virtual;
    Function TimeStampIndexOf(const Name: String): Integer; overload; virtual;
    Function TimeStampIndexOf(Value: TFCTicks): Integer; overload; virtual;
    Function TimeStampIndexOf(const Name: String; Value: TFCTicks): Integer; overload; virtual;
    Function TimeStampAdd(const Name: String; Value: TFCTicks; UserData: PtrInt = 0): Integer; virtual;
    Function TimeStampAddCurrent(const Name: String; UserData: PtrInt = 0): Integer; virtual; // adds current point as a new timestamp
    procedure TimeStampInsert(Index: Integer; const Name: String; Value: TFCTicks; UserData: PtrInt = 0); virtual;
    Function TimeStampRemove(const Name: String): Integer; overload; virtual;
    Function TimeStampRemove(Value: TFCTicks): Integer; overload; virtual;
    Function TimeStampRemove(const Name: String; Value: TFCTicks): Integer; overload; virtual;
    procedure TimeStampDelete(Index: Integer); virtual;
    procedure TimeStampClear; virtual;
    Function TimeStampDistance(Index: Integer): TFCTime; overload; virtual;    // distance between selected timestamp and current point
    Function TimeStampDistance(const Name: String): TFCTime; overload; virtual;
    //- accumulators list ---
    Function AccumulatorLowIndex: Integer; virtual;
    Function AccumulatorHighIndex: Integer; virtual;
    Function AccumulatorCheckIndex(Index: Integer): Boolean; virtual;
    Function AccumulatorIndexOf(const Name: String): Integer; overload; virtual;
    Function AccumulatorIndexOf(Value: TFCTicks): Integer; overload; virtual;
    Function AccumulatorIndexOf(const Name: String; Value: TFCTicks): Integer; overload; virtual;
    Function AccumulatorAdd(const Name: String; InitialValue: TFCTicks = 0; UserData: PtrInt = 0): Integer; virtual;
    procedure AccumulatorInsert(Index: Integer; const Name: String; InitialValue: TFCTicks = 0; UserData: PtrInt = 0); virtual;
    Function AccumulatorRemove(const Name: String): Integer; overload; virtual;
    Function AccumulatorRemove(Value: TFCTicks): Integer; overload; virtual;
    Function AccumulatorRemove(const Name: String; Value: TFCTicks): Integer; overload; virtual;
    procedure AccumulatorDelete(Index: Integer); virtual;
    procedure AccumulatorClear; virtual;
    procedure AccumulatorReset(Index: Integer); virtual;
    Function AccumulatorAccumulate(Index: Integer; Delta: TFCTicks): TFCTime; overload; virtual;
    Function AccumulatorAccumulate(Index: Integer): TFCTime; overload; virtual;
    Function AccumulatorAccumulate(const Name: String; Delta: TFCTicks): TFCTime; overload; virtual;
    Function AccumulatorAccumulate(const Name: String): TFCTime; overload; virtual;
    procedure AccumulatorAccumulateAll(Delta: TFCTicks); overload; virtual;
    procedure AccumulatorAccumulateAll; overload; virtual;
    Function AccumulatorDistance(Index: Integer): TFCTime; overload; virtual; // how much distance has been accumulated
    Function AccumulatorDistance(const Name: String): TFCTime; overload; virtual;
    //- timestamp properties ---
    property TimeStampCount: Integer index FCE_LIST_IDX_TIMESTAMPS read GetCount write SetCount;
    property TimeStampCapacity: Integer index FCE_LIST_IDX_TIMESTAMPS read GetCapacity write SetCapacity;
    property TimeStamps[Index: Integer]: TFCTimeStamp read GetTimeStamp write SetTimeStamp;
    property TimeStampPtrs[Index: Integer]: PFCTimeStamp read GetTimeStampPtr;
    //- accumulator properties ---
    property AccumulatorCount: Integer index FCE_LIST_IDX_ACCUMULATORS read GetCount write SetCount;
    property AccumulatorCapacity: Integer index FCE_LIST_IDX_ACCUMULATORS read GetCapacity write SetCapacity;
    property Accumulators[Index: Integer]: TFCAccumulator read GetAccumulator write SetAccumulator;
    property AccumulatorPtrs[Index: Integer]: PFCAccumulator read GetAccumulatorPtr;
  end;

{===============================================================================
    Standalone functions - declaration
===============================================================================}

type
  TClockContext = type Pointer;

  TClockUnit = (cuTick,cuSecond,cuMilli,cuMicro);

procedure ClockStart(out Context: TClockContext);

Function ClockTick(var Context: TClockContext; ReturnUnit: TClockUnit = cuMilli): Int64;
Function ClockTickF(var Context: TClockContext; ReturnUnit: TClockUnit = cuMilli): Double;

{
  Do NOT call ClockTick before ClockEnd if you want to use its return value to
  measure interval just before the call to ClockEnd, it calls ClockTick
  implicitly.
}
Function ClockEnd(var Context: TClockContext; ReturnUnit: TClockUnit = cuMilli): Int64;
Function ClockEndF(var Context: TClockContext; ReturnUnit: TClockUnit = cuMilli): Double;

implementation

uses
{$IFDEF Windows}Windows{$ELSE}baseunix, linux{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TFrameClock
--------------------------------------------------------------------------------
===============================================================================}

const
  FC_MILLIS_PER_SEC = 1000;         // milliseconds per second
  FC_MICROS_PER_SEC = 1000000;      // microseconds per second
  FC_NANOS_PER_SEC  = 1000000000;   // nanoseconds per second

  FC_NANOS_PER_MILLI = 1000000;     // nanoseconds per millisecond
  FC_MILLIS_PER_DAY  = 86400000;    // milliseconds per day

{===============================================================================
    TFrameClock - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFrameClock - protected methods
-------------------------------------------------------------------------------}

Function TFrameClock.GetCapacity(List: Integer): Integer;
begin
{$IFDEF FPC}Result := 0;{$ENDIF}
raise EFCInvalidList.CreateFmt('TFrameClock.GetCapacity: Invalid list (%d).',[List]);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TFrameClock.SetCapacity(List,Value: Integer);
begin
raise EFCInvalidList.CreateFmt('TFrameClock.SetCapacity: Invalid list (%d).',[List]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TFrameClock.GetCount(List: Integer): Integer;
begin
{$IFDEF FPC}Result := 0;{$ENDIF}
raise EFCInvalidList.CreateFmt('TFrameClock.GetCount: Invalid list (%d).',[List]);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TFrameClock.SetCount(List,Value: Integer);
begin
raise EFCInvalidList.CreateFmt('TFrameClock.SetCount: Invalid list (%d).',[List]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TFrameClock.InitializeTime(LowResOnly: Boolean);
{$IFNDEF Windows}
var
  Time: TTimeSpec;
{$ENDIF}
begin
{$IFDEF Windows}
If not LowResOnly and QueryPerformanceFrequency(fFrequency) then
  begin
    fHighResolution := True;
    fFrequency := fFrequency and $7FFFFFFFFFFFFFFF; // mask out sign bit
    fResolution := Trunc((1 / fFrequency) * FC_NANOS_PER_SEC);
  end
{$ELSE}
If not LowResOnly and (clock_getres(CLOCK_MONOTONIC_RAW,@Time) = 0) then
  begin
    fHighResolution := True;
    fFrequency := FC_NANOS_PER_SEC; // frequency is hardcoded for nanoseconds
    fResolution := (Int64(Time.tv_sec) * FC_NANOS_PER_SEC) + Time.tv_nsec;
  end
{$ENDIF}
else
  begin
    fHighResolution := False;
    fFrequency := FC_MILLIS_PER_SEC;
    fResolution := FC_NANOS_PER_MILLI;
  end;
end;

//------------------------------------------------------------------------------

procedure TFrameClock.Initialize(ForcedResolution: TFCForcedResolution);
begin
InitializeTime(ForcedResolution = frForceLow);
If (ForcedResolution = frForceHigh) and not fHighResolution then
  raise EFCHighResolutionFail.Create('TFrameClock.Create: Failed to obtain high resolution timer.');
fFrameCounter := 0;
fCreationPoint := GetActualPoint;
fPreviousPoint := fCreationPoint;
fCurrentPoint := fCreationPoint;
FillChar(fCurrentFrame,SizeOf(TFCTime),0);
fApplyCorrection := False;
fCorrection := 0;
end;

//------------------------------------------------------------------------------

procedure TFrameClock.Finalize;
begin
// nothing to do here
end;

{-------------------------------------------------------------------------------
    TFrameClock - public methods
-------------------------------------------------------------------------------}

constructor TFrameClock.Create(ForcedResolution: TFCForcedResolution = frDontForce);
begin
inherited Create(0);
Initialize(ForcedResolution);
end;

//------------------------------------------------------------------------------

destructor TFrameClock.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TFrameClock.LowIndex(List: Integer): Integer;
begin
{$IFDEF FPC}Result := 0;{$ENDIF}
raise EFCInvalidList.CreateFmt('TFrameClock.LowIndex: Invalid list (%d).',[List]);
end;

//------------------------------------------------------------------------------

Function TFrameClock.HighIndex(List: Integer): Integer;
begin
{$IFDEF FPC}Result := -1;{$ENDIF}
raise EFCInvalidList.CreateFmt('TFrameClock.HighIndex: Invalid list (%d).',[List]);
end;

//------------------------------------------------------------------------------

Function TFrameClock.Tick: TFCTime;
begin
Inc(fFrameCounter);
fPreviousPoint := fCurrentPoint;
fCurrentPoint := GetActualPoint;
fCurrentFrame := GetPointsDistance(fPreviousPoint,fCurrentPoint);
Result := fCurrentFrame;
end;

//------------------------------------------------------------------------------

Function TFrameClock.PointDistance(Point: TFCTicks): TFCTime;
begin
Result := GetPointsDistance(Point,fCurrentPoint);
end;

//------------------------------------------------------------------------------

Function TFrameClock.CreationDistance: TFCTime;
begin
Result := GetPointsDistance(fCreationPoint,fCurrentPoint);
end;
 
//------------------------------------------------------------------------------

Function TFrameClock.PreviousDistance: TFCTime;
begin
Result := GetPointsDistance(fPreviousPoint,GetActualPoint);
end;

//------------------------------------------------------------------------------

Function TFrameClock.CurrentDistance: TFCTime;
begin
Result := GetPointsDistance(fCurrentPoint,GetActualPoint);
end;

//------------------------------------------------------------------------------

Function TFrameClock.GetActualPoint: TFCTicks;
{$IFNDEF Windows}
var
  Time: TTimeSpec;
{$ENDIF}
begin
Result := 0;
If fHighResolution then
  begin
  {$IFDEF Windows}
    If QueryPerformanceCounter(Result) then
      Result := Result and $7FFFFFFFFFFFFFFF  // mask out sign bit
    else
      raise EFCSystemException.CreateFmt('TFrameClock.GetCurrentTicks: System error 0x%.8x.',[GetLastError]);
  {$ELSE}
    If clock_gettime(CLOCK_MONOTONIC_RAW,@Time) = 0 then
      Result := (Int64(Time.tv_sec) * FC_NANOS_PER_SEC) + Time.tv_nsec
    else
      raise EFCSystemException.CreateFmt('TFrameClock.GetCurrentTicks: System error %d.',[errno]);
  {$ENDIF}
  end
else Result := Int64(Trunc(Now * FC_MILLIS_PER_DAY));
end;

//------------------------------------------------------------------------------

Function TFrameClock.GetPointsDifference(A,B: TFCTicks): TFCTicks;
begin
If A < B then
  Result := B - A
else If A > B then
  Result := High(Int64) - A + B + 1{overflow tick}
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function TFrameClock.GetPointsDistance(A,B: TFCTicks): TFCTime;
begin
Result.Ticks := GetPointsDifference(A,B);
FillFromTicks(Result);
end;
 
//------------------------------------------------------------------------------

Function TFrameClock.MeasureCorrection(Repeats: Integer = 1000): TFCTicks;
var
  SavedFrameCounter:    UInt64;
  SavedPreviousPoint:   TFCTicks;
  SavedCurrentPoint:    TFCTicks;
  SavedCurrentFrame:    TFCTime;
  SavedApplyCorrection: Boolean;
  i:                    Integer;
  Counter:              TFCTicks;
begin
If Repeats > 0 then
  begin
    // save fields that will change during testing
    SavedFrameCounter := fFrameCounter;
    SavedPreviousPoint := fPreviousPoint;
    SavedCurrentPoint := fCurrentPoint;
    SavedCurrentFrame := fCurrentFrame;
    SavedApplyCorrection := fApplyCorrection;
    try
      fApplyCorrection := False;
      Counter := 0;
      For i := 1 to Repeats do
        begin
          Tick; Tick;
          Counter := Counter + fCurrentFrame.Ticks;
        end;
      Result := Round(Counter / Repeats);
    finally
      // restore changed fields
      fFrameCounter := SavedFrameCounter;
      fPreviousPoint := SavedPreviousPoint;
      fCurrentPoint := SavedCurrentPoint;
      fCurrentFrame := SavedCurrentFrame;
      fApplyCorrection := SavedApplyCorrection;
    end;
  end
else Result := 0
end;

//------------------------------------------------------------------------------

Function TFrameClock.ApplyMeasuredCorrection: TFCTicks;
begin
fApplyCorrection := True;
fCorrection := MeasureCorrection;
Result := fCorrection;
end;

//------------------------------------------------------------------------------

procedure TFrameClock.FillFromTicks(var Time: TFCTime);
var
  Temp: Extended;
begin
If fApplyCorrection then
  begin
    If Time.Ticks >= fCorrection then
      Time.Ticks := Time.Ticks - fCorrection
    else
      Time.Ticks := 0;
  end;
Temp := Time.Ticks / fFrequency;
Time.Sec := Temp;
Time.MiS := Temp * FC_MILLIS_PER_SEC;
Time.UiS := Temp * FC_MICROS_PER_SEC;
Time.iSec := Trunc(Temp);
Time.iMiS := Trunc(Temp * FC_MILLIS_PER_SEC);
Time.iUiS := Trunc(Temp * FC_MICROS_PER_SEC);
end;
     

{===============================================================================
--------------------------------------------------------------------------------
                                  TFrameClockEx
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFrameClockEx - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFrameClockEx - protected methods
-------------------------------------------------------------------------------}

Function TFrameClockEx.GetTimeStamp(Index: Integer): TFCTimeStamp;
begin
If TimeStampCheckIndex(Index) then
  Result := fTimeStamps[Index]
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.GetTimeStamp: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.SetTimeStamp(Index: Integer; Value: TFCTimeStamp);
begin
If TimeStampCheckIndex(Index) then
  fTimeStamps[Index] := Value
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.SetTimeStamp: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.GetTimeStampPtr(Index: Integer): PFCTimeStamp;
begin
If TimeStampCheckIndex(Index) then
  Result := Addr(fTimeStamps[Index])
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.GetTimeStampPtr: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.GetAccumulator(Index: Integer): TFCAccumulator;
begin
If AccumulatorCheckIndex(Index) then
  Result := fAccumulators[Index]
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.GetAccumulator: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.SetAccumulator(Index: Integer; Value: TFCAccumulator);
begin
If AccumulatorCheckIndex(Index) then
  fAccumulators[Index] := Value
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.SetAccumulator: Index (%d) out of bounds.',[Index]);
end;
 
//------------------------------------------------------------------------------

Function TFrameClockEx.GetAccumulatorPtr(Index: Integer): PFCAccumulator;
begin
If AccumulatorCheckIndex(Index) then
  Result := Addr(fAccumulators[Index])
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.GetAccumulatorPtr: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.GetCapacity(List: Integer): Integer;
begin
case List of
  FCE_LIST_IDX_TIMESTAMPS:    Result := Length(fTimeStamps);
  FCE_LIST_IDX_ACCUMULATORS:  Result := Length(fAccumulators);
else
  Result := inherited GetCapacity(List);
end;
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.SetCapacity(List,Value: Integer);
begin
case List of
  FCE_LIST_IDX_TIMESTAMPS:    begin
                                If Value < fTimeStampCount then
                                  fTimeStampCount := Value;
                                SetLength(fTimeStamps,Value);
                              end;
  FCE_LIST_IDX_ACCUMULATORS:  begin
                                If Value < fAccumulatorCount then
                                  fAccumulatorCount := Value;
                                SetLength(fAccumulators,Value);
                              end;
else
  inherited SetCapacity(List,Value);
end;
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.GetCount(List: Integer): Integer;
begin
case List of
  FCE_LIST_IDX_TIMESTAMPS:    Result := fTimeStampCount;
  FCE_LIST_IDX_ACCUMULATORS:  Result := fAccumulatorCount;
else
  Result := inherited GetCount(List);
end;
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.SetCount(List,Value: Integer);
begin
case List of
  FCE_LIST_IDX_TIMESTAMPS:;   // do nothing
  FCE_LIST_IDX_ACCUMULATORS:; // do nothing
else
  inherited SetCount(List,Value);
end;
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.Initialize(ForcedResolution: TFCForcedResolution);
begin
inherited Initialize(ForcedResolution);
SetLength(fTimeStamps,0);
fTimeStampCount := 0;
SetLength(fAccumulators,0);
fAccumulatorCount := 0;
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.Finalize;
begin
TimeStampClear;
AccumulatorClear;
inherited;
end;

{-------------------------------------------------------------------------------
    TFrameClockEx - public methods
-------------------------------------------------------------------------------}

constructor TFrameClockEx.Create(ForcedResolution: TFCForcedResolution = frDontForce);
begin
inherited Create(ForcedResolution);
ListCount := ListCount + 2;
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.LowIndex(List: Integer): Integer;
begin
case List of
  FCE_LIST_IDX_TIMESTAMPS:    Result := Low(fTimeStamps);
  FCE_LIST_IDX_ACCUMULATORS:  Result := Low(fAccumulators);
else
  Result := inherited LowIndex(List);
end;
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.HighIndex(List: Integer): Integer;
begin
case List of
  FCE_LIST_IDX_TIMESTAMPS:    Result := Pred(fTimeStampCount);
  FCE_LIST_IDX_ACCUMULATORS:  Result := Pred(fAccumulatorCount);
else
  Result := inherited HighIndex(List);
end;
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.TimeStampLowIndex: Integer;
begin
Result := LowIndex(FCE_LIST_IDX_TIMESTAMPS);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.TimeStampHighIndex: Integer;
begin
Result := HighIndex(FCE_LIST_IDX_TIMESTAMPS);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.TimeStampCheckIndex(Index: Integer): Boolean;
begin
Result := CheckIndex(FCE_LIST_IDX_TIMESTAMPS,Index);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.TimeStampIndexOf(const Name: String): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := TimeStampLowIndex to TimeStampHighIndex do
  If AnsiSameStr(fTimeStamps[i].Name,Name) then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.TimeStampIndexOf(Value: TFCTicks): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := TimeStampLowIndex to TimeStampHighIndex do
  If fTimeStamps[i].Value = Value then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.TimeStampIndexOf(const Name: String; Value: TFCTicks): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := TimeStampLowIndex to TimeStampHighIndex do
  If (fTimeStamps[i].Value = Value) and AnsiSameStr(fTimeStamps[i].Name,Name) then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.TimeStampAdd(const Name: String; Value: TFCTicks; UserData: PtrInt = 0): Integer;
begin
Grow(FCE_LIST_IDX_TIMESTAMPS);
Result := fTimeStampCount;
fTimeStamps[Result].Name := Name;
UniqueString(fTimeStamps[Result].Name);
fTimeStamps[Result].Value := Value;
fTimeStamps[Result].UserData := UserData;
Inc(fTimeStampCount);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.TimeStampAddCurrent(const Name: String; UserData: PtrInt = 0): Integer;
begin
Result := TimeStampAdd(Name,fCurrentPoint,UserData);
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.TimeStampInsert(Index: Integer; const Name: String; Value: TFCTicks; UserData: PtrInt = 0);
var
  i:  Integer;
begin
If TimeStampCheckIndex(Index) then
  begin
    Grow(FCE_LIST_IDX_TIMESTAMPS);
    For i := TimeStampHighIndex downto Index do
      fTimeStamps[i + 1] := fTimeStamps[i];
    fTimeStamps[Index].Name := Name;
    UniqueString(fTimeStamps[Index].Name);
    fTimeStamps[Index].Value := Value;
    fTimeStamps[Index].UserData := UserData;
    Inc(fTimeStampCount);
  end
else If Index = fTimeStampCount then
  TimeStampAdd(Name,Value,UserData)
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.TimeStampInsert: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.TimeStampRemove(const Name: String): Integer;
begin
Result := TimeStampIndexOf(Name);
If TimeStampCheckIndex(Result) then
  TimeStampDelete(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.TimeStampRemove(Value: TFCTicks): Integer;
begin
Result := TimeStampIndexOf(Value);
If TimeStampCheckIndex(Result) then
  TimeStampDelete(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.TimeStampRemove(const Name: String; Value: TFCTicks): Integer;
begin
Result := TimeStampIndexOf(Name,Value);
If TimeStampCheckIndex(Result) then
  TimeStampDelete(Result);
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.TimeStampDelete(Index: Integer);
var
  i:  Integer;
begin
If TimeStampCheckIndex(Index) then
  begin
    For i := Index to Pred(TimeStampHighIndex) do
      fTimeStamps[i] := fTimeStamps[i + 1];
    Dec(fTimeStampCount);
    Shrink(FCE_LIST_IDX_TIMESTAMPS);
  end
else raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.TimeStampDelete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.TimeStampClear;
begin
SetLength(fTimeStamps,0);
fTimeStampCount := 0;
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.TimeStampDistance(Index: Integer): TFCTime;
begin
If TimeStampCheckIndex(Index) then
  Result := GetPointsDistance(fTimeStamps[Index].Value,fCurrentPoint)
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.TimeStampTime: Index (%d) out of bounds.',[Index]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.TimeStampDistance(const Name: String): TFCTime;
var
  Index:  Integer;
begin
Index := TimeStampIndexOf(Name);
If TimeStampCheckIndex(Index) then
  Result := TimeStampDistance(Index)
else
  FillChar(Addr(Result)^,SizeOf(TFCTime),0);
{
  Addr(Result)^ is there as a workaround for nonsensical warning in FPC about
  result being not set.
}
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.AccumulatorLowIndex: Integer;
begin
Result := LowIndex(FCE_LIST_IDX_ACCUMULATORS);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.AccumulatorHighIndex: Integer;
begin
Result := HighIndex(FCE_LIST_IDX_ACCUMULATORS);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.AccumulatorCheckIndex(Index: Integer): Boolean;
begin
Result := CheckIndex(FCE_LIST_IDX_ACCUMULATORS,Index);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.AccumulatorIndexOf(const Name: String): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := AccumulatorLowIndex to AccumulatorHighIndex do
  If AnsiSameStr(fAccumulators[i].Name,Name) then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.AccumulatorIndexOf(Value: TFCTicks): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := AccumulatorLowIndex to AccumulatorHighIndex do
  If fAccumulators[i].Value = Value then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.AccumulatorIndexOf(const Name: String; Value: TFCTicks): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := AccumulatorLowIndex to AccumulatorHighIndex do
  If (fAccumulators[i].Value = Value) and AnsiSameStr(fAccumulators[i].Name,Name) then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.AccumulatorAdd(const Name: String; InitialValue: TFCTicks = 0; UserData: PtrInt = 0): Integer;
begin
Grow(FCE_LIST_IDX_ACCUMULATORS);
Result := fAccumulatorCount;
fAccumulators[Result].Name := Name;
UniqueString(fAccumulators[Result].Name);
fAccumulators[Result].Value := InitialValue;
fAccumulators[Result].UserData := UserData;
Inc(fAccumulatorCount);
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.AccumulatorInsert(Index: Integer; const Name: String; InitialValue: TFCTicks = 0; UserData: PtrInt = 0);
var
  i:  Integer;
begin
If AccumulatorCheckIndex(Index) then
  begin
    Grow(FCE_LIST_IDX_ACCUMULATORS);
    For i := AccumulatorHighIndex downto Index do
      fAccumulators[i + 1] := fAccumulators[i];
    fAccumulators[Index].Name := Name;
    UniqueString(fAccumulators[Index].Name);
    fAccumulators[Index].Value := InitialValue;
    fAccumulators[Index].UserData := UserData;
    Inc(fAccumulatorCount);
  end
else If Index = fAccumulatorCount then
  AccumulatorAdd(Name,InitialValue,UserData)
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.AccumulatorInsert: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.AccumulatorRemove(const Name: String): Integer;
begin
Result := AccumulatorIndexOf(Name);
If AccumulatorCheckIndex(Result) then
  AccumulatorDelete(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.AccumulatorRemove(Value: TFCTicks): Integer;
begin
Result := AccumulatorIndexOf(Value);
If AccumulatorCheckIndex(Result) then
  AccumulatorDelete(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.AccumulatorRemove(const Name: String; Value: TFCTicks): Integer;
begin
Result := AccumulatorIndexOf(Name,Value);
If AccumulatorCheckIndex(Result) then
  AccumulatorDelete(Result);
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.AccumulatorDelete(Index: Integer);
var
  i:  Integer;
begin
If AccumulatorCheckIndex(Index) then
  begin
    For i := Index to Pred(AccumulatorHighIndex) do
      fAccumulators[i] := fAccumulators[i + 1];
    Dec(fAccumulatorCount);
    Shrink(FCE_LIST_IDX_ACCUMULATORS);
  end
else raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.AccumulatorDelete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.AccumulatorClear;
begin
SetLength(fAccumulators,0);
fAccumulatorCount := 0;
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.AccumulatorReset(Index: Integer);
begin
If AccumulatorCheckIndex(Index) then
  fAccumulators[Index].Value := 0
else
  raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.AccumulatorReset: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.AccumulatorAccumulate(Index: Integer; Delta: TFCTicks): TFCTime;
begin
If AccumulatorCheckIndex(Index) then
  begin
    fAccumulators[Index].Value := fAccumulators[Index].Value + Delta;
    Result.Ticks := fAccumulators[Index].Value;
    FillFromTicks(Result);
  end
else raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.AccumulatorAccumulate: Index (%d) out of bounds.',[Index]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.AccumulatorAccumulate(Index: Integer): TFCTime;
begin
Result := AccumulatorAccumulate(Index,fCurrentFrame.Ticks);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.AccumulatorAccumulate(const Name: String; Delta: TFCTicks): TFCTime;
var
  Index:  Integer;
begin
Index := AccumulatorIndexOf(Name);
If AccumulatorCheckIndex(Index) then
  Result := AccumulatorAccumulate(Index,Delta)
else
  FillChar(Addr(Result)^,SizeOf(TFCTime),0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.AccumulatorAccumulate(const Name: String): TFCTime;
begin
Result := AccumulatorAccumulate(Name,fCurrentFrame.Ticks);
end;

//------------------------------------------------------------------------------

procedure TFrameClockEx.AccumulatorAccumulateAll(Delta: TFCTicks);
var
  i:  Integer;
begin
For i := AccumulatorLowIndex to AccumulatorHighIndex do
  fAccumulators[i].Value := fAccumulators[i].Value + Delta; 
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TFrameClockEx.AccumulatorAccumulateAll;
begin
AccumulatorAccumulateAll(fCurrentFrame.Ticks);
end;

//------------------------------------------------------------------------------

Function TFrameClockEx.AccumulatorDistance(Index: Integer): TFCTime;
begin
If AccumulatorCheckIndex(Index) then
  begin
    Result.Ticks := fAccumulators[Index].Value;
    FillFromTicks(Result);
  end
else raise EFCIndexOutOfBounds.CreateFmt('TFrameClockEx.AccumulatorTime: Index (%d) out of bounds.',[Index]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TFrameClockEx.AccumulatorDistance(const Name: String): TFCTime;
var
  Index:  Integer;
begin
Index := AccumulatorIndexOf(Name);
If AccumulatorCheckIndex(Index) then
  Result := AccumulatorDistance(Index)
else
  FillChar(Addr(Result)^,SizeOf(TFCTime),0);
end;


{===============================================================================
    Standalone functions - implementation
===============================================================================}

procedure ClockStart(out Context: TClockContext);
begin
Context := TClockContext(TFrameClock.Create);
TFrameClock(Context).Tick;
end;

//------------------------------------------------------------------------------

Function ClockTick(var Context: TClockContext; ReturnUnit: TClockUnit = cuMilli): Int64;
begin
try
  TFrameClock(Context).Tick;
  case ReturnUnit of
    cuSecond: Result := TFrameClock(Context).CurrentFrame.iSec;
    cuMilli:  Result := TFrameClock(Context).CurrentFrame.iMiS;
    cuMicro:  Result := TFrameClock(Context).CurrentFrame.iUiS;
  else
   {mruTick}
    Result := TFrameClock(Context).CurrentFrame.Ticks;
  end;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function ClockTickF(var Context: TClockContext; ReturnUnit: TClockUnit = cuMilli): Double;
begin
try
  TFrameClock(Context).Tick;
  case ReturnUnit of
    cuSecond: Result := TFrameClock(Context).CurrentFrame.Sec;
    cuMilli:  Result := TFrameClock(Context).CurrentFrame.MiS;
    cuMicro:  Result := TFrameClock(Context).CurrentFrame.UiS;
  else
   {mruTick}
    Result := TFrameClock(Context).CurrentFrame.Ticks;
  end;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function ClockEnd(var Context: TClockContext; ReturnUnit: TClockUnit = cuMilli): Int64;
begin
try
  Result := ClockTick(Context,ReturnUnit);
  TFrameClock(Context).Free;
  Context := nil;
except
  Result := -1;
end;
end;

//------------------------------------------------------------------------------

Function ClockEndF(var Context: TClockContext; ReturnUnit: TClockUnit = cuMilli): Double;
begin
try
  Result := ClockTickF(Context,ReturnUnit);
  TFrameClock(Context).Free;
  Context := nil;
except
  Result := -1;
end;
end;

end.
