{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CrossSyncObjs

    Provides wrapper classes for synchronization primitives and wait functions
    from libraries WinSynObjs and LinSyncObjs.

    Classes in WinSynObjs and LinSyncObjs are sticking to a nomenclature used
    by system synchronization objects they are based upon, as a result the
    corresponding synchronizers in both libraries have differently named
    methods for the same functionality.

    Wrappers in this library are hiding these differences behind an unified
    interface. Note that only functionality common for both wrapped libraries
    is provided - specialities (eg. releasing a semaphore by more than 1,
    alertable waiting, ...) are not implemented or default value is used.

      WARNING - unless noted otherwise, do not assume any features the
                synchronizers might provide (eg. recursive locking, lock
                promotion, ...), as they are usually only available in some
                systems, and therefore cannot be relied upon. Expect only
                bare-bone functionality.

      WARNING - remembed that all system-specific limitations still apply here
                (eg. max 64 events in WaitForMultipleEvents on Windows).

  Version 1.1 (2022-12-26)

  Last change 2024-02-03

  ©2022-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.CrossSyncObjs

  Dependencies:
    AuxClasses  - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
  * LinSyncObjs - github.com/TheLazyTomcat/Lib.LinSyncObjs
  * WinSyncObjs - github.com/TheLazyTomcat/Lib.WinSyncObjs

  Library LinSyncObjs is required only when compiling for Linux OS.

  Library WinSyncObjs is required only when compiling for Windows OS.

  Indirect dependencies:
    AuxExceptions       - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxMath             - github.com/TheLazyTomcat/Lib.AuxMath
    BasicUIM            - github.com/TheLazyTomcat/Lib.BasicUIM
    BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite
    BitOps              - github.com/TheLazyTomcat/Lib.BitOps
    BitVector           - github.com/TheLazyTomcat/Lib.BitVector
    HashBase            - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps      - github.com/TheLazyTomcat/Lib.InterlockedOps
    NamedSharedItems    - github.com/TheLazyTomcat/Lib.NamedSharedItems
    SHA1                - github.com/TheLazyTomcat/Lib.SHA1
    SharedMemoryStream  - github.com/TheLazyTomcat/Lib.SharedMemoryStream
    SimpleCPUID         - github.com/TheLazyTomcat/Lib.SimpleCPUID
    SimpleFutex         - github.com/TheLazyTomcat/Lib.SimpleFutex
    StaticMemoryStream  - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect             - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils         - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo         - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit CrossSyncObjs;

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
  {$MODESWITCH ClassicProcVars+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes, SyncObjs,
  AuxTypes, AuxClasses, {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF};

type
  TCSOWaitResult = (wrSignaled,wrTimeout,wrError);

const
  INFINITE = UInt32(-1);  // infinite timeout

{===============================================================================
--------------------------------------------------------------------------------
                                 TRTLSyncObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TRTLSyncObject - class declaration
===============================================================================}
type
  TRTLSyncObject = class(TCustomRefCountedObject);

{===============================================================================
--------------------------------------------------------------------------------
                               TRTLCriticalSection
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TRTLCriticalSection - class declaration
===============================================================================}
type
  TRTLCriticalSection = class(TRTLSyncObject)
  protected
    fSync:  SyncObjs.TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock; virtual;
    procedure Unlock; virtual;
  end;

  TCriticalSectionRTL = TRTLCriticalSection;

{===============================================================================
--------------------------------------------------------------------------------
                     TRTLMultiReadExclusiveWriteSynchronizer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TRTLMultiReadExclusiveWriteSynchronizer - class declaration
===============================================================================}
type
  TRTLMultiReadExclusiveWriteSynchronizer = class(TRTLSyncObject)
  protected
    fSync:  SysUtils.TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadLock; virtual;
    procedure ReadUnlock; virtual;
    procedure WriteLock; virtual;
    procedure WriteUnlock; virtual;
  end;

  TMultiReadExclusiveWriteSynchronizerRTL = TRTLMultiReadExclusiveWriteSynchronizer;

  TRTLMREW = TRTLMultiReadExclusiveWriteSynchronizer;
  TMREWRTL = TRTLMREW;

{===============================================================================
--------------------------------------------------------------------------------
                                TLocalSyncObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TLocalSyncObject - class declaration
===============================================================================}
type
  TLocalSyncObject = class(TCustomRefCountedObject);

{===============================================================================
--------------------------------------------------------------------------------
                                TCriticalSection
--------------------------------------------------------------------------------
===============================================================================}
{
  Always allows recursive locking.
}
{===============================================================================
    TCriticalSection - class declaration
===============================================================================}
type
  TCriticalSection = class(TLocalSyncObject)
  protected
    fSync:  {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock; virtual;
    Function TryLock: Boolean; virtual;
    procedure Unlock; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TAdvSyncObject
--------------------------------------------------------------------------------
===============================================================================}  
{===============================================================================
    TAdvSyncObject - class declaration
===============================================================================}
type
  TAdvSyncObject = class(TCustomObject)
  protected
    Function GetName: String; virtual; abstract;
  public
    property Name: String read GetName;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                     TEvent
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TEvent - class declaration
===============================================================================}
type
  TEvent = class(TAdvSyncObject)
  protected
    fSync:  {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TEvent;
    Function GetName: String; override;
  public
    constructor Create(const Name: String; ManualReset, InitialState: Boolean); overload;
    constructor Create(const Name: String); overload;  
    constructor Create(ManualReset, InitialState: Boolean); overload;
    constructor Create; overload;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    constructor DuplicateFrom(Source: TEvent);
    destructor Destroy; override;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    Function Wait(Timeout: UInt32 = INFINITE): TCSOWaitResult; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                     TMutex
--------------------------------------------------------------------------------
===============================================================================}
{
  Allows for recursive locking.
}
{===============================================================================
    TMutex - class declaration
===============================================================================}
type
  TMutex = class(TAdvSyncObject)
  protected
    fSync:  {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TMutex;
    Function GetName: String; override;
  public
    constructor Create(const Name: String); overload;
    constructor Create; overload;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    constructor DuplicateFrom(Source: TMutex);
    destructor Destroy; override;
    Function Lock(Timeout: UInt32 = INFINITE): TCSOWaitResult; virtual;
    procedure Unlock; virtual;
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
  TSemaphore = class(TAdvSyncObject)
  protected
    fSync:  {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TSemaphore;
    Function GetName: String; override;
  public
    constructor Create(const Name: String; InitialCount: Integer); overload;
    constructor Create(InitialCount: Integer); overload;
    constructor Open(const Name: String);
    constructor DuplicateFrom(Source: TSemaphore);
    destructor Destroy; override;
    Function Lock(Timeout: UInt32 = INFINITE): TCSOWaitResult; virtual;
    procedure Unlock; virtual;
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
  TReadWriteLock = class(TAdvSyncObject)
  protected
    fSync:  {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TReadWriteLock;
    Function GetName: String; override;
  public
    constructor Create(const Name: String); overload;
    constructor Create; overload;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    constructor DuplicateFrom(Source: TReadWriteLock);
    destructor Destroy; override;
    Function ReadLock(Timeout: UInt32 = INFINITE): TCSOWaitResult; virtual;
    procedure ReadUnlock; virtual;
    Function WriteLock(Timeout: UInt32 = INFINITE): TCSOWaitResult; virtual;
    procedure WriteUnlock; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TConditionVariable
--------------------------------------------------------------------------------
===============================================================================}
type
  TConditionVariableClass = class of TConditionVariable;  // original class from WinSyncObjs or LinSyncObjs

type
  // types for autocycle
  TCSOWakeOption = (woWakeOne,woWakeAll,woWakeBeforeUnlock);
  TCSOWakeOptions = set of TCSOWakeOption;

  TCSOPredicateCheckEvent = procedure(Sender: TObject; var Predicate: Boolean) of object;
  TCSOPredicateCheckCallback = procedure(Sender: TObject; var Predicate: Boolean);

  TCSODataAccessEvent = procedure(Sender: TObject; var WakeOptions: TCSOWakeOptions) of object;
  TCSODataAccessCallback = procedure(Sender: TObject; var WakeOptions: TCSOWakeOptions);
  
{===============================================================================
    TConditionVariable - class declaration
===============================================================================}
type
  TConditionVariable = class(TAdvSyncObject)
  protected
    fSync:                      {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TConditionVariable;
    // autocycle events
    fOnPredicateCheckEvent:     TCSOPredicateCheckEvent;
    fOnPredicateCheckCallback:  TCSOPredicateCheckCallback;
    fOnDataAccessEvent:         TCSODataAccessEvent;
    fOnDataAccessCallback:      TCSODataAccessCallback;
    // getters, setters
    Function GetName: String; override;
    // autocycle handlers
    procedure PredicateCheckHandler(Sender: TObject; var Predicate: Boolean); virtual;
    procedure DataAccessHandler(Sender: TObject; var WakeOptions: {$IFDEF Windows}TWSOWakeOptions{$ELSE}TLSOWakeOptions{$ENDIF}); virtual;
    // othes
    class Function GetActualSyncClass: TConditionVariableClass; virtual;
  public
    constructor Create(const Name: String); overload;
    constructor Create; overload;
    constructor Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    constructor DuplicateFrom(Source: TConditionVariable);
    destructor Destroy; override;
    procedure Wait(DataLock: TMutex; Timeout: UInt32 = INFINITE); overload; virtual;
    procedure Wake; virtual;
    procedure WakeAll; virtual;
    procedure AutoCycle(DataLock: TMutex; Timeout: DWORD = INFINITE); overload; virtual;
    // events
    property OnPredicateCheckEvent: TCSOPredicateCheckEvent read fOnPredicateCheckEvent write fOnPredicateCheckEvent;
    property OnPredicateCheckCallback: TCSOPredicateCheckCallback read fOnPredicateCheckCallback write fOnPredicateCheckCallback;
    property OnPredicateCheck: TCSOPredicateCheckEvent read fOnPredicateCheckEvent write fOnPredicateCheckEvent;
    property OnDataAccessEvent: TCSODataAccessEvent read fOnDataAccessEvent write fOnDataAccessEvent;    
    property OnDataAccessCallback: TCSODataAccessCallback read fOnDataAccessCallback write fOnDataAccessCallback;
    property OnDataAccess: TCSODataAccessEvent read fOnDataAccessEvent write fOnDataAccessEvent;
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
    class Function GetActualSyncClass: TConditionVariableClass; override;  
  public
    procedure Lock; virtual;
    procedure Unlock; virtual;
    procedure Wait(Timeout: UInt32 = INFINITE); overload; virtual;
    procedure AutoCycle(Timeout: DWORD = INFINITE); overload; virtual;
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
  TBarrier = class(TAdvSyncObject)
  protected
    fSync:  {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TBarrier;
    Function GetName: String; override;
  public
    constructor Create(const Name: String; Count: Integer); overload;
    constructor Create(Count: Integer); overload;
    constructor Open(const Name: String);
    constructor DuplicateFrom(Source: TBarrier);
    destructor Destroy; override;
    procedure Wait; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 Wait functions
--------------------------------------------------------------------------------
===============================================================================}

Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean; Timeout: DWORD; out Index: Integer): TCSOWaitResult; overload;
Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean; Timeout: DWORD): TCSOWaitResult; overload;
Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean): TCSOWaitResult; overload;

{===============================================================================
--------------------------------------------------------------------------------
                               Utility functions
--------------------------------------------------------------------------------
===============================================================================}
{
  WaitResultToStr returns textual representation of a given wait result.
  Meant mainly for debugging.
}
Function WaitResultToStr(WaitResult: TCSOWaitResult): String;


implementation

{$IFNDEF Windows}
uses
  UnixType;
{$ENDIF}

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                               Internal functions
--------------------------------------------------------------------------------
===============================================================================}

Function TranslateWaitResult(WaitResult: {$IFDEF Windows}TWSOWaitResult{$ELSE}TLSOWaitResult{$ENDIF}): TCSOWaitResult;
begin
case WaitResult of
{$IFDEF Windows}
  WinSyncObjs.wrSignaled,
  WinSyncObjs.wrAbandoned:  Result := wrSignaled;
  WinSyncObjs.wrTimeout:    Result := wrTimeout;
{$ELSE}
  LinSyncObjs.wrSignaled:   Result := wrSignaled;
  LinSyncObjs.wrTimeout:    Result := wrTimeout;
{$ENDIF}
else
 {wrIOCompletion, wrMessage, wrError, wrFatal}
  Result := wrError;
end;
end;

//------------------------------------------------------------------------------

Function TranslateWakeOptions(WakeOptions: {$IFDEF Windows}TWSOWakeOptions{$ELSE}TLSOWakeOptions{$ENDIF}): TCSOWakeOptions; overload;
begin
Result := [];
If {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.woWakeOne in WakeOptions then
  Include(Result,woWakeOne);
If {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.woWakeAll in WakeOptions then
  Include(Result,woWakeAll);
If {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.woWakeBeforeUnlock in WakeOptions then
  Include(Result,woWakeBeforeUnlock);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TranslateWakeOptions(WakeOptions: TCSOWakeOptions): {$IFDEF Windows}TWSOWakeOptions{$ELSE}TLSOWakeOptions{$ENDIF}; overload;
begin
Result := [];
If woWakeOne in WakeOptions then
  Include(Result,{$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.woWakeOne);
If woWakeAll in WakeOptions then
  Include(Result,{$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.woWakeAll);
If woWakeBeforeUnlock in WakeOptions then
  Include(Result,{$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.woWakeBeforeUnlock);
end;

{===============================================================================
--------------------------------------------------------------------------------
                               TRTLCriticalSection
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TRTLCriticalSection - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TRTLCriticalSection - public methods
-------------------------------------------------------------------------------}

constructor TRTLCriticalSection.Create;
begin
inherited Create;
fSync := SyncObjs.TCriticalSection.Create;
end;

//------------------------------------------------------------------------------

destructor TRTLCriticalSection.Destroy;
begin
fSync.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TRTLCriticalSection.Lock;
begin
fSync.Enter;
end;

//------------------------------------------------------------------------------

procedure TRTLCriticalSection.Unlock;
begin
fSync.Leave;
end;


{===============================================================================
--------------------------------------------------------------------------------
                     TRTLMultiReadExclusiveWriteSynchronizer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TRTLMultiReadExclusiveWriteSynchronizer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TRTLMultiReadExclusiveWriteSynchronizer - public methods
-------------------------------------------------------------------------------}

constructor TRTLMultiReadExclusiveWriteSynchronizer.Create;
begin
inherited Create;
fSync := SysUtils.TMultiReadExclusiveWriteSynchronizer.Create;
end;

//------------------------------------------------------------------------------

destructor TRTLMultiReadExclusiveWriteSynchronizer.Destroy;
begin
fSync.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TRTLMultiReadExclusiveWriteSynchronizer.ReadLock;
begin
fSync.BeginRead;
end;

//------------------------------------------------------------------------------

procedure TRTLMultiReadExclusiveWriteSynchronizer.ReadUnlock;
begin
fSync.EndRead;
end;

//------------------------------------------------------------------------------

procedure TRTLMultiReadExclusiveWriteSynchronizer.WriteLock;
begin
fSync.BeginWrite;
end;

//------------------------------------------------------------------------------

procedure TRTLMultiReadExclusiveWriteSynchronizer.WriteUnlock;
begin
fSync.EndWrite
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
    TCriticalSection - public methods
-------------------------------------------------------------------------------}

constructor TCriticalSection.Create;
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TCriticalSection.Create;
end;

//------------------------------------------------------------------------------

destructor TCriticalSection.Destroy;
begin
fSync.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.Lock;
begin
fSync.Enter;
end;

//------------------------------------------------------------------------------

Function TCriticalSection.TryLock: Boolean;
begin
Result := fSync.TryEnter;
end;

//------------------------------------------------------------------------------

procedure TCriticalSection.Unlock;
begin
fSync.Leave;
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

Function TEvent.GetName: String;
begin
Result := fSync.Name;
end;

{-------------------------------------------------------------------------------
    TEvent - public methods
-------------------------------------------------------------------------------}

constructor TEvent.Create(const Name: String; ManualReset, InitialState: Boolean);
begin
inherited Create;
{$IFDEF Windows}
fSync := WinSyncObjs.TEvent.Create(ManualReset,InitialState,Name);
{$ELSE}
fSync := LinSyncObjs.TEvent.Create(Name,ManualReset,InitialState);
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create(const Name: String);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TEvent.Create(Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create(ManualReset, InitialState: Boolean);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TEvent.Create(ManualReset,InitialState);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEvent.Create;
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TEvent.Create;
end;

//------------------------------------------------------------------------------

constructor TEvent.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TEvent.Open(Name);
end;

//------------------------------------------------------------------------------

constructor TEvent.DuplicateFrom(Source: TEvent);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TEvent.DuplicateFrom(Source.fSync);
end;

//------------------------------------------------------------------------------

destructor TEvent.Destroy;
begin
fSync.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TEvent.Lock;
begin
{$IFDEF Windows}
fSync.ResetEventStrict;
{$ELSE}
fSync.LockStrict;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TEvent.Unlock;
begin
{$IFDEF Windows}
fSync.SetEventStrict;
{$ELSE}
fSync.UnlockStrict;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TEvent.Wait(Timeout: UInt32 = INFINITE): TCSOWaitResult;
begin
{$IFDEF Windows}
Result := TranslateWaitResult(fSync.WaitFor(Timeout));
{$ELSE}
case Timeout of
  0:        If fSync.TryWaitStrict then
              Result := wrSignaled
            else
              Result := wrTimeout;
  INFINITE: begin
              fSync.WaitStrict;
              Result := wrSignaled;
            end;
else
  Result := TranslateWaitResult(fSync.TimedWait(Timeout));
end;
{$ENDIF}
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

Function TMutex.GetName: String;
begin
Result := fSync.Name;
end;

{-------------------------------------------------------------------------------
    TMutex - public methods
-------------------------------------------------------------------------------}

constructor TMutex.Create(const Name: String);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TMutex.Create(Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMutex.Create;
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TMutex.Create;
end;

//------------------------------------------------------------------------------

constructor TMutex.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TMutex.Open(Name);
end;

//------------------------------------------------------------------------------

constructor TMutex.DuplicateFrom(Source: TMutex);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TMutex.DuplicateFrom(Source.fSync);
end;

//------------------------------------------------------------------------------

destructor TMutex.Destroy;
begin
fSync.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TMutex.Lock(Timeout: UInt32 = INFINITE): TCSOWaitResult;
begin
{$IFDEF Windows}
Result := TranslateWaitResult(fSync.WaitFor(Timeout));
{$ELSE}
case Timeout of
  0:        If fSync.TryLockStrict then
              Result := wrSignaled
            else
              Result := wrTimeout;
  INFINITE: begin
              fSync.LockStrict;
              Result := wrSignaled;
            end;
else
  Result := TranslateWaitResult(fSync.TimedLock(Timeout));
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TMutex.Unlock;
begin
{$IFDEF Windows}
fSync.ReleaseMutexStrict;
{$ELSE}
fSync.UnlockStrict;
{$ENDIF}
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

Function TSemaphore.GetName: String;
begin
Result := fSync.Name;
end;

{-------------------------------------------------------------------------------
    TSemaphore - public methods
-------------------------------------------------------------------------------}

constructor TSemaphore.Create(const Name: String; InitialCount: Integer);
begin
inherited Create;
{$IFDEF Windows}
fSync := WinSyncObjs.TSemaphore.Create(InitialCount,Integer($7FFFFFFF),Name);
{$ELSE}
fSync := LinSyncObjs.TSemaphore.Create(Name,cUnsigned(InitialCount));
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSemaphore.Create(InitialCount: Integer);
begin
inherited Create;
{$IFDEF Windows}
fSync := WinSyncObjs.TSemaphore.Create(InitialCount,Integer($7FFFFFFF));
{$ELSE}
fSync := LinSyncObjs.TSemaphore.Create(cUnsigned(InitialCount));
{$ENDIF}
end;

//------------------------------------------------------------------------------

constructor TSemaphore.Open(const Name: String);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TSemaphore.Open(Name);
end;

//------------------------------------------------------------------------------

constructor TSemaphore.DuplicateFrom(Source: TSemaphore);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TSemaphore.DuplicateFrom(Source.fSync);
end;

//------------------------------------------------------------------------------

destructor TSemaphore.Destroy;
begin
fSync.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TSemaphore.Lock(Timeout: UInt32 = INFINITE): TCSOWaitResult;
begin
{$IFDEF Windows}
Result := TranslateWaitResult(fSync.WaitFor(Timeout));
{$ELSE}
case Timeout of
  0:        If fSync.TryWaitStrict then
              Result := wrSignaled
            else
              Result := wrTimeout;
  INFINITE: begin
              fSync.WaitStrict;
              Result := wrSignaled;
            end;
else
  Result := TranslateWaitResult(fSync.TimedWait(Timeout));
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TSemaphore.Unlock;
begin
{$IFDEF Windows}
fSync.ReleaseSemaphoreStrict;
{$ELSE}
fSync.PostStrict;
{$ENDIF}
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TReadWriteLock                                
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TReadWriteLock - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TReadWriteLock - protected methods
-------------------------------------------------------------------------------}

Function TReadWriteLock.GetName: String;
begin
Result := fSync.Name;
end;

{-------------------------------------------------------------------------------
    TReadWriteLock - public methods
-------------------------------------------------------------------------------}

constructor TReadWriteLock.Create(const Name: String);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TReadWriteLock.Create(Name);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TReadWriteLock.Create;
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TReadWriteLock.Create;
end;

//------------------------------------------------------------------------------

constructor TReadWriteLock.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TReadWriteLock.Open(Name);
end;

//------------------------------------------------------------------------------

constructor TReadWriteLock.DuplicateFrom(Source: TReadWriteLock);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TReadWriteLock.DuplicateFrom(Source.fSync);
end;

//------------------------------------------------------------------------------

destructor TReadWriteLock.Destroy;
begin
fSync.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TReadWriteLock.ReadLock(Timeout: UInt32 = INFINITE): TCSOWaitResult;
begin
{$IFDEF Windows}
Result := TranslateWaitResult(fSync.ReadLock(Timeout));
{$ELSE}
case Timeout of
  0:        If fSync.TryReadLockStrict then
              Result := wrSignaled
            else
              Result := wrTimeout;
  INFINITE: begin
              fSync.ReadLockStrict;
              Result := wrSignaled;
            end;
else
  Result := TranslateWaitResult(fSync.TimedReadLock(Timeout));
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.ReadUnlock;
begin
{$IFDEF Windows}
fSync.ReadUnlock;
{$ELSE}
fSync.UnlockStrict;
{$ENDIF}
end;
 
//------------------------------------------------------------------------------

Function TReadWriteLock.WriteLock(Timeout: UInt32 = INFINITE): TCSOWaitResult;
begin
{$IFDEF Windows}
Result := TranslateWaitResult(fSync.WriteLock(Timeout));
{$ELSE}
fSync.WriteLockStrict;
case Timeout of
  0:        If fSync.TryWriteLockStrict then
              Result := wrSignaled
            else
              Result := wrTimeout;
  INFINITE: begin
              fSync.WriteLockStrict;
              Result := wrSignaled;
            end;
else
  Result := TranslateWaitResult(fSync.TimedWriteLock(Timeout));
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TReadWriteLock.WriteUnlock;
begin
{$IFDEF Windows}
fSync.WriteUnlock
{$ELSE}
fSync.UnlockStrict;
{$ENDIF}
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

Function TConditionVariable.GetName: String;
begin
Result := fSync.Name;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TConditionVariable.PredicateCheckHandler(Sender: TObject; var Predicate: Boolean);
begin
If Assigned(fOnPredicateCheckEvent) then
  fOnPredicateCheckEvent(Self,Predicate)
else If Assigned(fOnPredicateCheckCallback) then
  fOnPredicateCheckCallback(Self,Predicate);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
  
//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TConditionVariable.DataAccessHandler(Sender: TObject; var WakeOptions: {$IFDEF Windows}TWSOWakeOptions{$ELSE}TLSOWakeOptions{$ENDIF});
var
  WakeOptionsInternal:  TCSOWakeOptions;
begin
WakeOptionsInternal := TranslateWakeOptions(WakeOptions);
If Assigned(fOnDataAccessEvent) then
  fOnDataAccessEvent(Self,WakeOptionsInternal)
else If Assigned(fOnDataAccessCallback) then
  fOnDataAccessCallback(Self,WakeOptionsInternal);
WakeOptions := TranslateWakeOptions(WakeOptionsInternal);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

class Function TConditionVariable.GetActualSyncClass: TConditionVariableClass;
begin
Result := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TConditionVariable;
end;

{-------------------------------------------------------------------------------
    TConditionVariable - public methods
-------------------------------------------------------------------------------}

constructor TConditionVariable.Create(const Name: String);
begin
inherited Create;
fSync := GetActualSyncClass.Create(Name);
fSync.OnPredicateCheckEvent := PredicateCheckHandler;
fSync.OnDataAccessEvent := DataAccessHandler;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TConditionVariable.Create;
begin
inherited Create;
fSync := GetActualSyncClass.Create;
fSync.OnPredicateCheckEvent := PredicateCheckHandler;
fSync.OnDataAccessEvent := DataAccessHandler;
end;

//------------------------------------------------------------------------------

constructor TConditionVariable.Open(const Name: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create;
fSync := GetActualSyncClass.Open(Name);
fSync.OnPredicateCheckEvent := PredicateCheckHandler;
fSync.OnDataAccessEvent := DataAccessHandler;
end;

//------------------------------------------------------------------------------

constructor TConditionVariable.DuplicateFrom(Source: TConditionVariable);
begin
inherited Create;
fSync := GetActualSyncClass.DuplicateFrom(Source.fSync);
fSync.OnPredicateCheckEvent := PredicateCheckHandler;
fSync.OnDataAccessEvent := DataAccessHandler;
end;
 
//------------------------------------------------------------------------------

destructor TConditionVariable.Destroy;
begin
fSync.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.Wait(DataLock: TMutex; Timeout: UInt32 = INFINITE);
begin
{$IFDEF Windows}
fSync.Sleep(DataLock.fSync,Timeout);
{$ELSE}
If Timeout <> INFINITE then
  fSync.TimedWait(DataLock.fSync,Timeout)
else
  fSync.WaitStrict(DataLock.fSync);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.Wake;
begin
{$IFDEF Windows}
fSync.Wake;
{$ELSE}
fSync.SignalStrict;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.WakeAll;
begin
{$IFDEF Windows}
fSync.WakeAll;
{$ELSE}
fSync.BroadcastStrict;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TConditionVariable.AutoCycle(DataLock: TMutex; Timeout: DWORD = INFINITE);
begin
If Assigned(fOnPredicateCheckEvent) or Assigned(fOnPredicateCheckCallback) then
  fSync.AutoCycle(DataLock.fSync,Timeout);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              TConditionVariableEx
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TConditionVariableEx - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TConditionVariableEx - protected methods
-------------------------------------------------------------------------------}

class Function TConditionVariableEx.GetActualSyncClass: TConditionVariableClass;
begin
// do not call inherited code
Result := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TConditionVariableEx;
end;

{-------------------------------------------------------------------------------
    TConditionVariableEx - public methods
-------------------------------------------------------------------------------}

procedure TConditionVariableEx.Lock;
begin
{$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TConditionVariableEx(fSync).Lock;
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.Unlock;
begin
{$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TConditionVariableEx(fSync).Unlock;
end;

//------------------------------------------------------------------------------

procedure TConditionVariableEx.Wait(Timeout: UInt32 = INFINITE);
begin
{$IFDEF Windows}
WinSyncObjs.TConditionVariableEx(fSync).Sleep(Timeout);
{$ELSE}
If Timeout <> INFINITE then
  LinSyncObjs.TConditionVariableEx(fSync).TimedWait(Timeout)
else
  LinSyncObjs.TConditionVariableEx(fSync).WaitStrict;
{$ENDIF}
end;
 
//------------------------------------------------------------------------------

procedure TConditionVariableEx.AutoCycle(Timeout: DWORD = INFINITE);
begin
If Assigned(fOnPredicateCheckEvent) or Assigned(fOnPredicateCheckCallback) then
  {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TConditionVariableEx(fSync).AutoCycle(Timeout);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                    TBarrier
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBarrier - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TBarrier - protected methods
-------------------------------------------------------------------------------}

Function TBarrier.GetName: String;
begin
Result := fSync.Name;
end;

{-------------------------------------------------------------------------------
    TBarrier - public methods
-------------------------------------------------------------------------------}

constructor TBarrier.Create(const Name: String; Count: Integer);
begin
inherited Create;
{$IFDEF Windows}
fSync := WinSyncObjs.TBarrier.Create(Count,Name);
{$ELSE}
fSync := LinSyncObjs.TBarrier.Create(Name,cUnsigned(Count));
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TBarrier.Create(Count: Integer);
begin
inherited Create;
{$IFDEF Windows}
fSync := WinSyncObjs.TBarrier.Create(Count);
{$ELSE}
fSync := LinSyncObjs.TBarrier.Create(cUnsigned(Count));
{$ENDIF}
end;

//------------------------------------------------------------------------------

constructor TBarrier.Open(const Name: String);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TBarrier.Open(Name);
end;

//------------------------------------------------------------------------------

constructor TBarrier.DuplicateFrom(Source: TBarrier);
begin
inherited Create;
fSync := {$IFDEF Windows}WinSyncObjs{$ELSE}LinSyncObjs{$ENDIF}.TBarrier.DuplicateFrom(Source.fSync);
end;
 
//------------------------------------------------------------------------------

destructor TBarrier.Destroy;
begin
fSync.Free;
inherited;
end;

//------------------------------------------------------------------------------

procedure TBarrier.Wait;
begin
fSync.Wait;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Wait functions
--------------------------------------------------------------------------------
===============================================================================}

Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean; Timeout: DWORD; out Index: Integer): TCSOWaitResult;
var
  SyncObjects:  array of {$IFDEF Windows}WinSyncObjs.TSimpleWinSyncObject{$ELSE}LinSyncObjs.TEvent{$ENDIF};
  i:            Integer;
begin
SyncObjects := nil;
SetLength(SyncObjects,Length(Objects));
For i := Low(Objects) to High(Objects) do
  SyncObjects[i] := Objects[i].fSync;
{$IFDEF Windows}
Result := TranslateWaitResult(WinSyncObjs.WaitForMultipleObjects(SyncObjects,WaitAll,Timeout,Index));
{$ELSE}
Result := TranslateWaitResult(LinSyncObjs.WaitForMultipleEvents(SyncObjects,WaitAll,Timeout,Index));
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean; Timeout: DWORD): TCSOWaitResult;
var
  Index:  Integer;
begin
Result := WaitForMultipleEvents(Objects,WaitAll,Timeout,Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WaitForMultipleEvents(Objects: array of TEvent; WaitAll: Boolean): TCSOWaitResult;
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

Function WaitResultToStr(WaitResult: TCSOWaitResult): String;
const
  WR_STRS: array[TCSOWaitResult] of String = ('Signaled','Timeout','Error');
begin
If (WaitResult >= Low(TCSOWaitResult)) and (WaitResult <= High(TCSOWaitResult)) then
  Result := WR_STRS[WaitResult]
else
  Result := '<invalid>';
end;

end.
