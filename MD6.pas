{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  MD6 calculation

    Provides classes and functions for calculation of MD6 hash. All hash widths
    that are a multiple of 8 are supported (meaning only whole bytes, arbitrary
    bitstreams are not supported). Also, all mandatory and optional hash inputs
    can be varied (key, mode control, number of rounds).

      Default implementation provided here (class TMD6Hash and its descendants)
      is only single-threaded, but as the MD6 can be heavily parallelized, an
      attemp was made to provide MD6 hashing that can use multiple execution
      paths.

      This is done in the form of TMD6HashParallel class (note that this class
      is NOT a descendant of THashBase, but rather a standalone class).
      The used code is purely experimental and probably bugged, but it offers a
      full freedom in hash input values (there is a limiting factor that
      protects against excessive memory use for combination of long messages
      and low mode control - property MaxSequentialNodes - but it can be
      changed as needed).
      The only strict limit is, that the entire message must be available at
      the start of processing, so only hashing of memory buffer or file is
      provided.

        WARNING - The parallel code is not fully tested, use it at your own
                  risk. Also, performance gain and scaling is uncertain, as
                  I was not able to test the code on more than dual-core CPU.

  Version 1.1 (2023-01-13)

  Last change 2023-01-13

  ©2022-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.MD6

  Dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
  * BitVector          - github.com/TheLazyTomcat/Lib.BitVector
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps     - github.com/TheLazyTomcat/Lib.InterlockedOps
  * LinSyncObjs        - github.com/TheLazyTomcat/Lib.LinSyncObjs
    NamedSharedItems   - github.com/TheLazyTomcat/Lib.NamedSharedItems
    SHA1               - github.com/TheLazyTomcat/Lib.SHA1
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
  * SimpleFutex        - github.com/TheLazyTomcat/Lib.SimpleFutex
    SharedMemoryStream - github.com/TheLazyTomcat/Lib.SharedMemoryStream
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
  * UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
  * WinSyncObjs        - github.com/TheLazyTomcat/Lib.WinSyncObjs

  Libraries UInt64Utils and WinSyncObjs are required only when compiling for
  Windows OS.

  Libraries BitVector, LinSyncObjs and SimpleFutex are required only when
  compiling for Linux OS.

  Library SimpleCPUID might not be required, depending on defined symbols in
  InterlockedOps and BitOps libraries.

===============================================================================}
unit MD6;

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE CPU32bit}
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
  {$MODESWITCH DuplicateLocals+}
  {$MODESWITCH ClassicProcVars+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses, HashBase, CrossSyncObjs;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EMD6Exception = class(EHASHException);

  EMD6InvalidValue      = class(EMD6Exception);
  EMD6InvalidState      = class(EMD6Exception);
  EMD6InvalidHashLength = class(EMD6Exception);

  EMD6IncompatibleClass = class(EMD6Exception);
  EMD6SizeMismatch      = class(EMD6Exception);

  EMD6ProcessingError     = class(EMD6Exception);
  EMD6OperationNotAllowed = class(EMD6Exception);

  EMD6ParallelInternal = class(EMD6Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in all MD6 hashes are always ordered from the most significant byte to
  the least significant byte (big endian).

  MD6 does not differ in little and big endian form, as it is not a single
  quantity, therefore methods like MD6ToLE or MD6ToBE do nothing and are
  present only for the sake of completeness.
}
type
  TMD6 = packed array of UInt8;

  TMD6_224 = packed array[0..27] of UInt8;    PMD6_224 = ^TMD6_224;
  TMD6_256 = packed array[0..31] of UInt8;    PMD6_256 = ^TMD6_256;
  TMD6_384 = packed array[0..47] of UInt8;    PMD6_384 = ^TMD6_384;
  TMD6_512 = packed array[0..63] of UInt8;    PMD6_512 = ^TMD6_512;

  TMD6Key = packed array of UInt8;

//------------------------------------------------------------------------------

const
  ZeroMD6_224: TMD6_224 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroMD6_256: TMD6_256 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroMD6_384: TMD6_384 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroMD6_512: TMD6_512 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

  MD6_BITS_DEFAULT = 512;                           

{===============================================================================
--------------------------------------------------------------------------------
                                    TMD6Hash
--------------------------------------------------------------------------------
===============================================================================}
type
  TMD6Word = UInt64;

  TMD6ProcessingBlock = array of TMD6Word;

  TMD6ProcessingNode = record
    Index:  Int64;
    Bytes:  TMemSize; // number of bytes already stored in block
    Block:  TMD6ProcessingBlock;
  end;

  TMD6ProcessingState = record
    Levels: array of TMD6ProcessingNode;
  end;

{===============================================================================
    TMD6Hash - class declaration
===============================================================================}
type
  TMD6Hash = class(TBlockHash)
  protected
    fMD6:         TMD6;
    fHashBits:    Integer;  // whole bytes only (div 8)
    fKey:         TMD6Key;  // max length 64
    fRounds:      Integer;  // default is 40 + (fHashBits / 4)
    fRoundsDef:   Boolean;  // indicates if rounds were set explicitly (false) or implicitly (true)
    fModeControl: Integer;  // >= 0 (explicitly limited to 255)
    fState:       TMD6ProcessingState;
    fProcessing:  Boolean;
    // getters setters
    Function GetMD6: TMD6; virtual;
    procedure SetMD6(Value: TMD6); virtual; // not used as a setter in any property
    procedure SetHashBits(Value: Integer); virtual;
    Function GetKey: TMD6Key; virtual;
    procedure PutKey(Value: TMD6Key); virtual;
    procedure SetRoundsDefault; virtual;
    procedure SetRounds(Value: Integer); virtual;
    procedure SetModeControl(Value: Integer); virtual;
    // main processing
    procedure AddTreeLevel; virtual;
    procedure ProcessTreeNode(Level: Integer; const Data); virtual;
    procedure ProcessTreeNodeFinal(Level: Integer; PadBytes: TMemSize); virtual;
    // block hash processing
    procedure ProcessBlock(const Block); override;
    procedure ProcessFirst(const Block); override;
    procedure ProcessLast; override;
    // init/final
    procedure Initialize; override;
  public
    class Function MD6ToLE(MD6: TMD6): TMD6; virtual;
    class Function MD6ToBE(MD6: TMD6): TMD6; virtual;
    class Function MD6FromLE(MD6: TMD6): TMD6; virtual;
    class Function MD6FromBE(MD6: TMD6): TMD6; virtual;
    Function HashSize: TMemSize; reintroduce;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMD6); reintroduce; overload; virtual;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure SetKey(const Key; Size: TMemSize); overload; virtual;
  {
    The Key parameter is converted to UTF8 encoding and this new string is
    then used for the actual key. If it is longer than 64 bytes, it is
    truncated.
  }
    procedure SetKey(const Key: String); overload; virtual;
    property MD6: TMD6 read GetMD6;
    property HashBits: Integer read fHashBits write SetHashBits;
    property Key: TMD6Key read GetKey write PutKey;
    property Rounds: Integer read fRounds write SetRounds;
    property ModeControl: Integer read fModeControl write SetModeControl;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6DefHash                                  
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6DefHash - class declaration
===============================================================================}
type
  TMD6DefHash = class(TMD6Hash)
  protected
    procedure SetMD6(Value: TMD6); override;
    procedure SetHashBits(Value: Integer); override;
    procedure PutKey(Value: TMD6Key); override;
    procedure SetRounds(Value: Integer); override;
    procedure SetModeControl(Value: Integer); override;
  public
    procedure SetKey(const Key; Size: TMemSize); overload; override;
    procedure SetKey(const Key: String); overload; override;
    // the properties are made read-only
    property HashBits: Integer read fHashBits;
    property Key: TMD6Key read GetKey;
    property Rounds: Integer read fRounds;
    property ModeControl: Integer read fModeControl;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6_224Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6_224Hash - class declaration
===============================================================================}
type
  TMD6_224Hash = class(TMD6DefHash)
  protected
    Function GetMD6_224: TMD6_224; virtual;
    procedure Initialize; override;
  public
    class Function MD6_224ToLE(MD6: TMD6_224): TMD6_224; virtual;
    class Function MD6_224ToBE(MD6: TMD6_224): TMD6_224; virtual;
    class Function MD6_224FromLE(MD6: TMD6_224): TMD6_224; virtual;
    class Function MD6_224FromBE(MD6: TMD6_224): TMD6_224; virtual;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6_224); overload; virtual;
    procedure FromStringDef(const Str: String; const Default: TMD6_224); overload; virtual;
    property MD6_224: TMD6_224 read GetMD6_224;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6_256Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6_256Hash - class declaration
===============================================================================}
type
  TMD6_256Hash = class(TMD6DefHash)
  protected
    Function GetMD6_256: TMD6_256; virtual;
    procedure Initialize; override;
  public
    class Function MD6_256ToLE(MD6: TMD6_256): TMD6_256; virtual;
    class Function MD6_256ToBE(MD6: TMD6_256): TMD6_256; virtual;
    class Function MD6_256FromLE(MD6: TMD6_256): TMD6_256; virtual;
    class Function MD6_256FromBE(MD6: TMD6_256): TMD6_256; virtual;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6_256); overload; virtual;
    procedure FromStringDef(const Str: String; const Default: TMD6_256); overload; virtual;
    property MD6_256: TMD6_256 read GetMD6_256;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6_384Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6_384Hash - class declaration
===============================================================================}
type
  TMD6_384Hash = class(TMD6DefHash)
  protected
    Function GetMD6_384: TMD6_384; virtual;
    procedure Initialize; override;
  public
    class Function MD6_384ToLE(MD6: TMD6_384): TMD6_384; virtual;
    class Function MD6_384ToBE(MD6: TMD6_384): TMD6_384; virtual;
    class Function MD6_384FromLE(MD6: TMD6_384): TMD6_384; virtual;
    class Function MD6_384FromBE(MD6: TMD6_384): TMD6_384; virtual;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6_384); overload; virtual;
    procedure FromStringDef(const Str: String; const Default: TMD6_384); overload; virtual;
    property MD6_384: TMD6_384 read GetMD6_384;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6_512Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6_512Hash - class declaration
===============================================================================}
type
  TMD6_512Hash = class(TMD6DefHash)
  protected
    Function GetMD6_512: TMD6_512; virtual;
    procedure Initialize; override;
  public
    class Function MD6_512ToLE(MD6: TMD6_512): TMD6_512; virtual;
    class Function MD6_512ToBE(MD6: TMD6_512): TMD6_512; virtual;
    class Function MD6_512FromLE(MD6: TMD6_512): TMD6_512; virtual;
    class Function MD6_512FromBE(MD6: TMD6_512): TMD6_512; virtual;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6_512); overload; virtual;
    procedure FromStringDef(const Str: String; const Default: TMD6_512); overload; virtual;
    property MD6_512: TMD6_512 read GetMD6_512;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TMD6HashParallel
--------------------------------------------------------------------------------
===============================================================================}
type
  TMD6ThreadFunction = procedure(Param: Pointer);

  TMD6ThreadStartCallback = procedure(Sender: TObject; ThreadFunction: TMD6ThreadFunction; Param: Pointer);
  TMD6ThreadStartEvent = procedure(Sender: TObject; ThreadFunction: TMD6ThreadFunction; Param: Pointer) of object;

// internals
type
  TMD6ProcessingMode = (tmBuffer,tmFile);

  PMD6CommonProcessingNode = ^TMD6CommonProcessingNode;
  TMD6CommonProcessingNode = record
    IsLastNode:         Boolean;
    ParentNodePtr:      PMD6CommonProcessingNode;
    ParentNodePutIndex: Integer;
    Counter:            Integer;
    LevelIndex:         Integer;
    Index:              Int64;
    Bytes:              TMemSize;
    Block:              TMD6ProcessingBlock;
  end;

  TMD6CommonProcessingLevel = record
    Index:  Integer;
    Nodes:  array of TMD6CommonProcessingNode;
  end;

  TMD6CommonProcessingState = record
    Levels: array of TMD6CommonProcessingLevel;
  end;

  TMD6ThreadTask = record
    CurrentOffset:  Int64;
    State:          TMD6ProcessingState;
  end;

{===============================================================================
    TMD6HashParallel - class declaration
===============================================================================}
type
  TMD6HashParallel = class(TCustomObject)
  protected
    fMD6:                   TMD6;
  {
    Some of the following fields are accessed from (multiple) working threads.
    Data integrity protection mechanisms are as follows:

      R - read-only field, immutable during processing, does not need to be
          thread-protected
      I - accessed using only interlocked functions
      C - combined access
  }
    fHashSettings: record
  {R} HashBits:             Integer;
  {R} Key:                  TMD6Key;
  {R} Rounds:               Integer;
      RoundsDef:            Boolean;
  {R} ModeControl:          Integer;
      MaxThreads:           Integer;
      MaxSeqNodes:          Int64;
    end;
    fInputMessage: record
  {R} ProcessingMode:       TMD6ProcessingMode;
  {R} Buffer:               Pointer;
  {R} FileName:             String;
  {R} Size:                 UInt64;
    end;
    fProcessingSettings: record
  {R} Sequential:           Boolean;
  {R} ThreadCount:          Integer;
  {R} ThreadLevelCount:     Integer;
    end;
    fTasksSettings: record
  {R} ChunksPerTask:        UInt64;
  {R} BytesPerTask:         UInt64;
    end;
    fProcessingVariables: record
  {I} StopCounter:          Integer;
  {I} CurrentOffset:        UInt64;
  {I} Terminated:           Boolean;
    end;
    fCommonTree: record
  {C} State:                TMD6CommonProcessingState;
    end;
    fProgressTracking: record
  {I} CurrentIntProgress:   UInt64;
    end;
    fProgUpdInterval:       UInt32;
    fDoneEvent:             TEvent;
    fProcessing:            Boolean;
    fLastProgress:          Double;
    // events and callbacks
    fOnThreadStartCallback: TMD6ThreadStartCallback;
    fOnThreadStartEvent:    TMD6ThreadStartEvent;
    fOnProgressCallback:    TFloatCallback;
    fOnProgressEvent:       TFloatEvent;
    // getters setters
    Function GetMD6: TMD6; virtual;
    procedure SetHashBits(Value: Integer); virtual;
    Function GetKey: TMD6Key; virtual;
    procedure PutKey(Value: TMD6Key); virtual;
    procedure SetRoundsDefault; virtual;
    procedure SetRounds(Value: Integer); virtual;
    procedure SetModeControl(Value: Integer); virtual;
    procedure SetMaxThreads(Value: Integer); virtual;
    procedure SetMaxSeqNodes(Value: Int64); virtual;
    // parallel processing - all following methods are called from within the worker threads
    Function Thread_GetTask(var ThreadTask: TMD6ThreadTask): Boolean; virtual;
    procedure Thread_ProcessThreadNode(var ThreadTask: TMD6ThreadTask; Level: Integer; const Data); virtual;
    procedure Thread_ProcessThreadNodeFinal(var ThreadTask: TMD6ThreadTask; Level: Integer; PadBytes: TMemSize); virtual;
    procedure Thread_CommonNodesEntry(const ThreadTask: TMD6ThreadTask); virtual;
    procedure Thread_ProcessCommonNode(NodePtr: PMD6CommonProcessingNode; PutIndex: Integer; const Data); virtual;
    procedure Thread_Execute; virtual;
    // preparation and execution of parallel processing
    Function ParallelPossible(DataSize: UInt64): Boolean; virtual;
    procedure ParallelPrepare; virtual;
    Function ParallelExecute(Memory: Pointer; Size: TMemSize): Integer; overload; virtual;
    Function ParallelExecute(const FileName: String; Size: Int64): Integer; overload; virtual;
    Function ParallelExecute: Integer; overload; virtual;
    procedure DoThreadStart; virtual;
    // object init/final
    procedure Initialize; virtual;
    // others
    procedure ProgressHandler(Sender: TObject; Progress: Double); virtual;
    procedure DoProgress; virtual;
  public
    class Function ProcessorCount: Integer; virtual;
    constructor Create;
    procedure SetKey(const Key; Size: TMemSize); overload; virtual;
    procedure SetKey(const Key: String); overload; virtual;
    Function HashMemory(Memory: Pointer; Size: TMemSize): Integer; virtual;
    Function HashBuffer(const Buffer; Size: TMemSize): Integer; virtual;
    Function HashFile(const FileName: String): Integer; virtual;
    property MD6: TMD6 read GetMD6;
    property HashBits: Integer read fHashSettings.HashBits write SetHashBits;
    property Key: TMD6Key read GetKey write PutKey;
    property Rounds: Integer read fHashSettings.Rounds write SetRounds;
    property ModeControl: Integer read fHashSettings.ModeControl write SetModeControl;
    property MaxThreads: Integer read fHashSettings.MaxThreads write SetMaxThreads;
    property MaxSequentialNodes: Int64 read fHashSettings.MaxSeqNodes write SetMaxSeqNodes;
    property ProgressUpdateInterval: UInt32 read fProgUpdInterval write fProgUpdInterval;
    property OnThreadStartCallback: TMD6ThreadStartCallback read fOnThreadStartCallback write fOnThreadStartCallback;
    property OnThreadStartEvent: TMD6ThreadStartEvent read fOnThreadStartEvent write fOnThreadStartEvent;
    property OnThreadStart: TMD6ThreadStartEvent read fOnThreadStartEvent write fOnThreadStartEvent;
    property OnProgressCallback: TFloatCallback read fOnProgressCallback write fOnProgressCallback;
    property OnProgressEvent: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
    property OnProgress: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Note that there is, for the sake of simplicity, no function implemented for
  fixed-length hash types (eg. TMD6_224), only for variant-length type TMD6.
  If you want to pass a fixed type, or convert variant-length result to fixed
  type, use following conversion functions to do so.
}

Function MD6ToMD6_224(Hash: TMD6): TMD6_224;
Function MD6ToMD6_256(Hash: TMD6): TMD6_256;
Function MD6ToMD6_384(Hash: TMD6): TMD6_384;
Function MD6ToMD6_512(Hash: TMD6): TMD6_512;

Function MD6_224ToMD6(Hash: TMD6_224): TMD6;
Function MD6_256ToMD6(Hash: TMD6_256): TMD6;
Function MD6_384ToMD6(Hash: TMD6_384): TMD6;
Function MD6_512ToMD6(Hash: TMD6_512): TMD6;

Function IsCompatibleMD6_224(Hash: TMD6): Boolean;
Function IsCompatibleMD6_256(Hash: TMD6): Boolean;
Function IsCompatibleMD6_384(Hash: TMD6): Boolean;
Function IsCompatibleMD6_512(Hash: TMD6): Boolean;

//------------------------------------------------------------------------------

Function MD6ToStr(MD6: TMD6): String;
Function StrToMD6(Str: String): TMD6;
Function TryStrToMD6(const Str: String; out MD6: TMD6): Boolean;
Function StrToMD6Def(const Str: String; Default: TMD6): TMD6;

Function CompareMD6(A,B: TMD6): Integer;
Function SameMD6(A,B: TMD6): Boolean;

Function BinaryCorrectMD6(Hash: TMD6): TMD6;

//------------------------------------------------------------------------------
{
  For MD6, it is not enough to pass hash from previous step when doing
  continuous hashing (BufferMD6 > LastBufferMD6). TDM6State type is introduced
  for this purpose.
}
type
  TMD6State = type Pointer;

  TMD6Settings = record
    HashBits:     Integer;
    Rounds:       Integer;
    ModeControl:  Integer;
    Key:          TMD6Key;
  end;

Function MD6Settings(HashBits,Rounds,ModeControl: Integer; Key: TMD6Key): TMD6Settings; overload;
Function MD6Settings(HashBits,Rounds,ModeControl: Integer; const Key; KeySize: TMemSize): TMD6Settings; overload;
Function MD6Settings(HashBits,Rounds,ModeControl: Integer; const Key: String): TMD6Settings; overload;

Function InitialMD6(Settings: TMD6Settings): TMD6State; overload;
Function InitialMD6(HashBits: Integer = MD6_BITS_DEFAULT): TMD6State; overload;

procedure BufferMD6(State: TMD6State; const Buffer; Size: TMemSize); overload;
Function LastBufferMD6(var State: TMD6State; const Buffer; Size: TMemSize): TMD6;

//------------------------------------------------------------------------------

Function BufferMD6(const Buffer; Size: TMemSize; Settings: TMD6Settings): TMD6; overload;
Function BufferMD6(const Buffer; Size: TMemSize; HashBits: Integer = MD6_BITS_DEFAULT): TMD6; overload;

Function AnsiStringMD6(const Str: AnsiString; Settings: TMD6Settings): TMD6; overload;
Function AnsiStringMD6(const Str: AnsiString; HashBits: Integer = MD6_BITS_DEFAULT): TMD6; overload;
Function WideStringMD6(const Str: WideString; Settings: TMD6Settings): TMD6; overload;
Function WideStringMD6(const Str: WideString; HashBits: Integer = MD6_BITS_DEFAULT): TMD6; overload;
Function StringMD6(const Str: String; Settings: TMD6Settings): TMD6; overload;
Function StringMD6(const Str: String; HashBits: Integer = MD6_BITS_DEFAULT): TMD6; overload;

Function StreamMD6(Stream: TStream; Count: Int64; Settings: TMD6Settings): TMD6; overload;
Function StreamMD6(Stream: TStream; Count: Int64 = -1; HashBits: Integer = MD6_BITS_DEFAULT): TMD6; overload;
Function FileMD6(const FileName: String; Settings: TMD6Settings): TMD6; overload;
Function FileMD6(const FileName: String; HashBits: Integer = MD6_BITS_DEFAULT): TMD6; overload;

//------------------------------------------------------------------------------
type
  TMD6Context = type Pointer;

Function MD6_Init(Settings: TMD6Settings): TMD6Context; overload;
Function MD6_Init(HashBits: Integer = MD6_BITS_DEFAULT): TMD6Context; overload;
procedure MD6_Update(Context: TMD6Context; const Buffer; Size: TMemSize);
Function MD6_Final(var Context: TMD6Context; const Buffer; Size: TMemSize): TMD6; overload;
Function MD6_Final(var Context: TMD6Context): TMD6; overload;
Function MD6_Hash(const Buffer; Size: TMemSize; HashBits: Integer = MD6_BITS_DEFAULT): TMD6;

implementation

uses
  {$IFDEF Windows}Windows,{$ELSE}BaseUnix,{$ENDIF} Math,
  StrRect, BitOps, InterlockedOps, StaticMemoryStream;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    General utilities
===============================================================================}

{$IFDEF Windows}

procedure GetNativeSystemInfo(lpSystemInfo: PSystemInfo); stdcall; external kernel32;

{$ELSE}

const
  _SC_NPROCESSORS_ONLN = 84;

Function sysconf(name: cInt): cLong; cdecl; external;

{$ENDIF}

//------------------------------------------------------------------------------

{$IF not Declared(Ceil64)}
Function Ceil64(x: Extended): Int64;
begin
Result := Trunc(x);
If Frac(x) > 0 then
  Result := Result + 1;
end;
{$IFEND}

{===============================================================================
--------------------------------------------------------------------------------
                                    TMD6Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6Hash - Implementation constants
===============================================================================}
const
  MD6_BITS_MAX = 512;

  MD6_MODE_DEFAULT = 64;

  MD6_KEY_MAXLEN = 64;  // in bytes

  MD6_CHUNK_SIZE = 128; // bytes (16 words)
  MD6_CHUNK_LEN  = 16;  // words

  MD6_BLOCK_LEN       = 89;   // words
  MD6_BLOCK_KEYSTART  = 15;
  MD6_BLOCK_KEYEND    = 22;
  MD6_BLOCK_DATASTART = 25;
  MD6_BLOCK_CAPACITY  = 512;  // bytes (64 words)
  MD6_BLOCK_IDX_U     = 23;   // position of unique node ID word
  MD6_BLOCK_IDX_V     = 24;   // position of control word

  // fractional part of sqrt(6)
  MD6_VEC_Q: array[0..14] of TMD6Word = (
    TMD6Word($7311c2812425cfa0), TMD6Word($6432286434aac8e7),
    TMD6Word($b60450e9ef68b7c1), TMD6Word($e8fb23908d9f06f1),
    TMD6Word($dd2e76cba691e5bf), TMD6Word($0cd0d63b2c30bc41),
    TMD6Word($1f8ccf6823058f8a), TMD6Word($54e5ed5b88e3775d),
    TMD6Word($4ad12aae0a6d6031), TMD6Word($3e7f16bb88222e0d),
    TMD6Word($8af8671d3fb50c2c), TMD6Word($995ad1178bd25c31),
    TMD6Word($c878c1dd04c4b633), TMD6Word($3b72066c7a1552ac),
    TMD6Word($0d6f3522631effcb));

  MD6_S_0    = TMD6Word($0123456789abcdef);
  MD6_S_MASK = TMD6Word($7311c2812425cfa0);

(*
--------------------------------------------------------------------------------
  Following constants are not used anywhere in the code, they are instead
  directly expanded into numerals where needed or calculated on-the-fly.
--------------------------------------------------------------------------------
{
  Round constants MD6_S can be calculated like this:

  MD6_S[0] := MD6_S_0;
  For i := 1 to High(MD6_S) do
    MD6_S[i] := ROL(MD6_S[i - 1],1) xor (MD6_S[i - 1] and MD6_S_STAR)
}
  MD6_S: array[0..167] of TMD6Word = (
    TMD6Word($0123456789ABCDEF), TMD6Word($0347CACE1376567E),
    TMD6Word($058E571C26C8EADC), TMD6Word($0A1CEC3869911F38),
    TMD6Word($16291870F3233150), TMD6Word($3E5330E1C66763A0),
    TMD6Word($4EB7614288EB84E0), TMD6Word($DF7F828511F68D60),
    TMD6Word($EDEE878B23C997E1), TMD6Word($BADD8D976792A863),
    TMD6Word($47AA9BAFEB25D8E7), TMD6Word($CC55B5DEF66E796E),
    TMD6Word($D8BAEB3DC8F8BBFD), TMD6Word($E165147A91D1FC5B),
    TMD6Word($A3CB28F523A234B7), TMD6Word($6497516B67646DCF),
    TMD6Word($A93FE2D7EAEC961E), TMD6Word($736E072EF5FDAA3D),
    TMD6Word($95DC0C5DCFDEDE5A), TMD6Word($3AA818BA9BB972B5),
    TMD6Word($475031F53753A7CA), TMD6Word($CDB0636B4AA6C814),
    TMD6Word($DA7084D795695829), TMD6Word($E6F1892E2EF3F873),
    TMD6Word($AFF2925C79C638C7), TMD6Word($7CF5A6B8D388790F),
    TMD6Word($89FACFF1A710BB1E), TMD6Word($12E55D626A21FD3D),
    TMD6Word($37CBFAC4F462375A), TMD6Word($5C963709CCE469B4),
    TMD6Word($E93C6C129DEC9AC8), TMD6Word($B36898253FFDBF11),
    TMD6Word($55D1B04B5BDEF123), TMD6Word($FAB2E097B7B92366),
    TMD6Word($877501AE4B5345ED), TMD6Word($0DFB03DC96A7CE7B),
    TMD6Word($1AE70539296A52D6), TMD6Word($27CF0A7372F4E72C),
    TMD6Word($6C9F16E7C5CD0978), TMD6Word($B92F2F4E8F9F1BD0),
    TMD6Word($435F5C9D1B3B3C21), TMD6Word($C5AFF9BB36577462),
    TMD6Word($CA5E33F748ABACE5), TMD6Word($D6AC656F9176D56B),
    TMD6Word($FF588ADE22C96FF7), TMD6Word($8DA1973C6593904F),
    TMD6Word($1A42AC78EF26A09F), TMD6Word($2685D8F1FA69C1BE),
    TMD6Word($6F0A7162D4F242DC), TMD6Word($BD14A2C5ADC4C738),
    TMD6Word($4B39C70A7F8D4951), TMD6Word($D5624C14DB1FDBA2),
    TMD6Word($FBC4D829B63A7CE5), TMD6Word($848970524854B56B),
    TMD6Word($0913A0A490ADEFF7), TMD6Word($1336C1C9217E104E),
    TMD6Word($357D431362D8209C), TMD6Word($5BEBC427E5B041B8),
    TMD6Word($E4D6484EEF40C2D0), TMD6Word($A9BCD09DFA814721),
    TMD6Word($726961BAD503C963), TMD6Word($96D383F5AE065BE6),
    TMD6Word($3FB6856A7808FC6D), TMD6Word($4C7D8AD4D01134FA),
    TMD6Word($D8EA9729A0236D54), TMD6Word($E1D5AC52606797A9),
    TMD6Word($A2BAD8A4E0EAA8F3), TMD6Word($676571C9E1F5D947),
    TMD6Word($ADCBA312E3CE7B8E), TMD6Word($7A96C425E798BC9D),
    TMD6Word($873D484AEB31F5BA), TMD6Word($0D6BD095F6422ED5),
    TMD6Word($1BD661AAC884532A), TMD6Word($24BC83D5910CE574),
    TMD6Word($6969852A221D0FC8), TMD6Word($B3D28A54643F1010),
    TMD6Word($54B596A8EC5B2021), TMD6Word($F97AAFD1FCB74062),
    TMD6Word($83E5DD22DD4BC0E5), TMD6Word($04CA7A45BE96416B),
    TMD6Word($0994B68A5928C3F6), TMD6Word($1239EF94B271444C),
    TMD6Word($36621DA944C3CC98), TMD6Word($5EC43BD38D8655B0),
    TMD6Word($EF8875261F08EEC0), TMD6Word($BC10AA4C3A111301),
    TMD6Word($4831D69854232503), TMD6Word($D0726FB0AC674F06),
    TMD6Word($F0F49DE17CEBD10D), TMD6Word($91F9BB43DDF6631B),
    TMD6Word($32E2F486BFC88537), TMD6Word($57C5298D5B918F4E),
    TMD6Word($FC8B539BB722919C), TMD6Word($8917E5B64A65A2B9),
    TMD6Word($133E0BEC94EEC7D3), TMD6Word($356C15592DF94826),
    TMD6Word($5BD82AB37FD3D86C), TMD6Word($E4A057E7DBA678F8),
    TMD6Word($A940ED4EB768B951), TMD6Word($73811A9D4AF1FBA3),
    TMD6Word($940337BB95C23CE6), TMD6Word($38076DF62F84756D),
    TMD6Word($400F9B6C7B0CAFFA), TMD6Word($C01EB4D8D61DD054),
    TMD6Word($C02DE931A83E60A9), TMD6Word($C05A1262705881F3),
    TMD6Word($C0A426C4C0B18247), TMD6Word($C1484F098142868F),
    TMD6Word($C390DC1202858B9F), TMD6Word($C4317824050E9CBF),
    TMD6Word($C873B0480E19B5DF), TMD6Word($D0F6E0901832EE3F),
    TMD6Word($F1FD01A03045125F), TMD6Word($92EB03C0408F26BF),
    TMD6Word($37D70500811B4BDF), TMD6Word($5CBF0A010237DC3E),
    TMD6Word($E96F1603044A745C), TMD6Word($B3DF2E070C94ACB9),
    TMD6Word($54AF5E0F1D2DD5D3), TMD6Word($F95FFE1F3E7E6E26),
    TMD6Word($83AE3E3F58D8926D), TMD6Word($045C7E7FB1B1A6FB),
    TMD6Word($08A8BEFE4342CB56), TMD6Word($1151FF7C86855DAC),
    TMD6Word($33B23CF9090FF6F8), TMD6Word($54747973121A2B50),
    TMD6Word($F8F8B2E724345DA0), TMD6Word($81E1E74F6C4CF6E1),
    TMD6Word($02C20C9FFC9D2B63), TMD6Word($078419BEDD3F5DE6),
    TMD6Word($0C0833FDBE5BF66C), TMD6Word($1810657A58B62AF8),
    TMD6Word($20308AF4B1485F50), TMD6Word($607197694290F1A0),
    TMD6Word($A0F2ACD3852122E0), TMD6Word($61F5D9260E634761),
    TMD6Word($A2FA724C18E7C9E2), TMD6Word($67E4A69831EA5A65),
    TMD6Word($ACC9CFB043F4FEEA), TMD6Word($79925DE087CD3375),
    TMD6Word($8234FB410B9F65CA), TMD6Word($06793483173B8E15),
    TMD6Word($0EE369872A56922A), TMD6Word($1FC7938F74A9A674),
    TMD6Word($2C8EA59FCD72CAC8), TMD6Word($791DCBBE9EC55F10),
    TMD6Word($832A55FD398FF120), TMD6Word($0554EB7B531A2361),
    TMD6Word($0BB914F7A63445E2), TMD6Word($1463296E684CCE64),
    TMD6Word($38C752DCF09D52E8), TMD6Word($418FE739C13FE770),
    TMD6Word($C21E0C72825A09C0), TMD6Word($C62C18E504B41A01),
    TMD6Word($CE58314B0D4C3E03), TMD6Word($DEA062971E9C7207),
    TMD6Word($EF4087AF393CA60F), TMD6Word($BD818DDF525DCA1F),
    TMD6Word($4A029B3FA4BE5E3F), TMD6Word($D605B47E6D58F25E),
    TMD6Word($FE0AE8FCFEB126BD), TMD6Word($8E151179D9434BDB),
    TMD6Word($1E3B22F2B287DC37), TMD6Word($2E674765450A744E),
    TMD6Word($7ECFCCCB8E14AC9C), TMD6Word($8F9E5916182DD5B8),
    TMD6Word($1C2CF22C307E6ED1), TMD6Word($2859265840D89322));

  // tap positions
  MD6_TAP: array[0..4] of Integer = (17,18,21,31,67);

  // right shifts
  MD6_SHIFT_R: array[0..15] of Integer = (
    10, 5, 13, 10, 11, 12, 2, 7, 14, 15, 7, 13, 11, 7, 6, 12);

  // left shifts
  MD6_SHIFT_L: array[0..15] of Integer = (
    11, 24, 9, 16, 15, 9, 27, 15, 6, 2, 29, 8, 15, 5, 31, 9);
*)
{===============================================================================
    TMD6Hash - utility functions
===============================================================================}

Function GetControlWord(Rnds, ModeCtrl, PadBits, KeyLen, HashBits: Integer; Final: Boolean): TMD6Word;
begin
Result := 0 or
  (TMD6Word(Rnds and $FFF) shl 48) or
  (TMD6Word(ModeCtrl and $FF) shl 40) or
  (TMD6Word(Integer(IfThen(Final,1,0)) and $F) shl 36) or
  (TMD6Word(PadBits and $FFFF) shl 20) or
  (TMD6Word(KeyLen and $FF) shl 12) or
   TMD6Word(HashBits and $FFF);
end;

//------------------------------------------------------------------------------

Function GetUniqueNodeIDWord(LevelNumber: Integer; NodeIndex: Int64): TMD6Word;
begin
Result := (TMD6Word(LevelNumber and $FF) shl 56) or (NodeIndex and $00FFFFFFFFFFFFFF);
end;

//------------------------------------------------------------------------------

procedure InitializeBlock(out Block: TMD6ProcessingBlock; Rounds: Integer; Key: TMD6Key);
var
  i:  Integer;
begin
SetLength(Block,0);
SetLength(Block,MD6_BLOCK_LEN + (Rounds * MD6_CHUNK_LEN));
For i := Low(MD6_VEC_Q) to High(MD6_VEC_Q) do
  Block[i] := MD6_VEC_Q[i];
// prepare key
If Length(Key) > 0 then
  begin
    Move(Key[0],Block[MD6_BLOCK_KEYSTART],Length(Key));
  {$IFNDEF ENDIAN_BIG}
    For i := MD6_BLOCK_KEYSTART to MD6_BLOCK_KEYEND do
      EndianSwapValue(Block[i]);
  {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

procedure CompressBlock(var Block: TMD6ProcessingBlock);
var
  i:          Integer;
  RoundConst: TMD6Word;
  x:          TMD6Word;
begin
// block is assumed to be completely prepared, only do endianness corrections
{$IFNDEF ENDIAN_BIG}
For i := MD6_BLOCK_DATASTART to Pred(MD6_BLOCK_LEN) do
  EndianSwapValue(Block[i]);
{$ENDIF}
// main calculation...
RoundConst := MD6_S_0;
i := MD6_BLOCK_LEN;
while i <= (Length(Block) - MD6_CHUNK_LEN) do
  begin
    // unrolled round (16 steps)...
    
    // step 0
    x := RoundConst xor Block[i - 89] xor Block[i - 17];
    x := x xor (Block[i - 18] and Block[i - 21]);
    x := x xor (Block[i - 31] and Block[i - 67]);
    x := x xor (x shr 10);
    Block[i] := x xor (x shl 11);

    // step 1
    x := RoundConst xor Block[i - 88] xor Block[i - 16];
    x := x xor (Block[i - 17] and Block[i - 20]);
    x := x xor (Block[i - 30] and Block[i - 66]);
    x := x xor (x shr 5);
    Block[i + 1] := x xor (x shl 24);

    // step 2
    x := RoundConst xor Block[i - 87] xor Block[i - 15];
    x := x xor (Block[i - 16] and Block[i - 19]);
    x := x xor (Block[i - 29] and Block[i - 65]);
    x := x xor (x shr 13);
    Block[i + 2] := x xor (x shl 9);

    // step 3
    x := RoundConst xor Block[i - 86] xor Block[i - 14];
    x := x xor (Block[i - 15] and Block[i - 18]);
    x := x xor (Block[i - 28] and Block[i - 64]);
    x := x xor (x shr 10);
    Block[i + 3] := x xor (x shl 16);

    // step 4
    x := RoundConst xor Block[i - 85] xor Block[i - 13];
    x := x xor (Block[i - 14] and Block[i - 17]);
    x := x xor (Block[i - 27] and Block[i - 63]);
    x := x xor (x shr 11);
    Block[i + 4] := x xor (x shl 15);

    // step 5
    x := RoundConst xor Block[i - 84] xor Block[i - 12];
    x := x xor (Block[i - 13] and Block[i - 16]);
    x := x xor (Block[i - 26] and Block[i - 62]);
    x := x xor (x shr 12);
    Block[i + 5] := x xor (x shl 9);

    // step 6
    x := RoundConst xor Block[i - 83] xor Block[i - 11];
    x := x xor (Block[i - 12] and Block[i - 15]);
    x := x xor (Block[i - 25] and Block[i - 61]);
    x := x xor (x shr 2);
    Block[i + 6] := x xor (x shl 27);

    // step 7
    x := RoundConst xor Block[i - 82] xor Block[i - 10];
    x := x xor (Block[i - 11] and Block[i - 14]);
    x := x xor (Block[i - 24] and Block[i - 60]);
    x := x xor (x shr 7);
    Block[i + 7] := x xor (x shl 15);

    // step 8
    x := RoundConst xor Block[i - 81] xor Block[i - 9];
    x := x xor (Block[i - 10] and Block[i - 13]);
    x := x xor (Block[i - 23] and Block[i - 59]);
    x := x xor (x shr 14);
    Block[i + 8] := x xor (x shl 6);

    // step 9
    x := RoundConst xor Block[i - 80] xor Block[i - 8];
    x := x xor (Block[i - 9] and Block[i - 12]);
    x := x xor (Block[i - 22] and Block[i - 58]);
    x := x xor (x shr 15);
    Block[i + 9] := x xor (x shl 2);

    // step 10
    x := RoundConst xor Block[i - 79] xor Block[i - 7];
    x := x xor (Block[i - 8] and Block[i - 11]);
    x := x xor (Block[i - 21] and Block[i - 57]);
    x := x xor (x shr 7);
    Block[i + 10] := x xor (x shl 29);

    // step 11
    x := RoundConst xor Block[i - 78] xor Block[i - 6];
    x := x xor (Block[i - 7] and Block[i - 10]);
    x := x xor (Block[i - 20] and Block[i - 56]);
    x := x xor (x shr 13);
    Block[i + 11] := x xor (x shl 8);

    // step 12
    x := RoundConst xor Block[i - 77] xor Block[i - 5];
    x := x xor (Block[i - 6] and Block[i - 9]);
    x := x xor (Block[i - 19] and Block[i - 55]);
    x := x xor (x shr 11);
    Block[i + 12] := x xor (x shl 15);

    // step 13
    x := RoundConst xor Block[i - 76] xor Block[i - 4];
    x := x xor (Block[i - 5] and Block[i - 8]);
    x := x xor (Block[i - 18] and Block[i - 54]);
    x := x xor (x shr 7);
    Block[i + 13] := x xor (x shl 5);

    // step 14
    x := RoundConst xor Block[i - 75] xor Block[i - 3];
    x := x xor (Block[i - 4] and Block[i - 7]);
    x := x xor (Block[i - 17] and Block[i - 53]);
    x := x xor (x shr 6);
    Block[i + 14] := x xor (x shl 31);

    // step 15
    x := RoundConst xor Block[i - 74] xor Block[i - 2];
    x := x xor (Block[i - 3] and Block[i - 6]);
    x := x xor (Block[i - 16] and Block[i - 52]);
    x := x xor (x shr 12);
    Block[i + 15] := x xor (x shl 9);

    // recalculate round constant
    RoundConst := ROL(RoundConst,1) xor (RoundConst and MD6_S_MASK);
    // increment index by number of steps taken
    Inc(i,16);
  end;
// endianness corection for chaining variable (last 1024 bits, 16 words, one chunk)
{$IFNDEF ENDIAN_BIG}
For i := (Length(Block) - MD6_CHUNK_LEN) to High(Block) do
  EndianSwapValue(Block[i]);
{$ENDIF}
end;

{===============================================================================
--------------------------------------------------------------------------------
                                    TMD6Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6Hash - protected methods
-------------------------------------------------------------------------------}

Function TMD6Hash.GetMD6: TMD6;
begin
Result := Copy(fMD6);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetMD6(Value: TMD6);
begin
If not fProcessing then
  begin
    If (Length(Value) > 0) and (Length(Value) <= (MD6_BITS_MAX div 8)) then
      begin
        fMD6 := Copy(Value);
        fHashBits := Length(Value) * 8;
        SetRoundsDefault;
      end
    else raise EMD6InvalidHashLength.CreateFmt('TMD6Hash.SetMD6: Invalid hash length (%d).',[Length(Value)])
  end
else raise EMD6InvalidState.Create('TMD6Hash.SetMD6: Cannot change hash during processing.');
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetHashBits(Value: Integer);
begin
If not fProcessing then
  begin
    If (Value > 0) and (Value <= MD6_BITS_MAX) and ((Value and 7) = 0) then
      begin
        SetLength(fMD6,0);  // to prevent copying
        SetLength(fMD6,Value div 8);
        fHashBits := Value;
        SetRoundsDefault;
      end
    else raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetHashBits: Invalid hash bits value (%d).',[Value]);
  end
else raise EMD6InvalidState.Create('TMD6Hash.SetHashBits: Cannot change hash bits during processing.');
end;

//------------------------------------------------------------------------------

Function TMD6Hash.GetKey: TMD6Key;
begin
Result := Copy(fKey);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.PutKey(Value: TMD6Key);
begin
If not fProcessing then
  begin
    If Length(Value) <= MD6_KEY_MAXLEN then
      begin
        fKey := Copy(Value);
        SetRoundsDefault;
      end
    else raise EMD6InvalidValue.CreateFmt('TMD6Hash.PetKey: Invalid key length (%d).',[Length(Value)]);
  end
else raise EMD6InvalidState.Create('TMD6Hash.PutKey: Cannot change key during processing.');
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetRoundsDefault;
begin
// just a macro function
If fRoundsDef then
  begin
    If Length(fKey) > 0 then
      fRounds := Max(80,40 + (fHashBits div 4))
    else
      fRounds := 40 + (fHashBits div 4);
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetRounds(Value: Integer);
begin
If not fProcessing then
  begin
    If Value >= 0 then
      begin
        fRounds := Value;
        fRoundsDef := False;
      end
    else raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetRounds: Invalid number of rounds (%d).',[Value]);
  end
else raise EMD6InvalidState.Create('TMD6Hash.SetRounds: Cannot change number of rounds during processing.');
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetModeControl(Value: Integer);
begin
If not fProcessing then
  begin
    If (Value >= 0) and (Value <= 64) then
      fModeControl := Value
    else
      raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetModeControl: Invalid mode control (%d).',[Value]);
  end
else raise EMD6InvalidState.Create('TMD6Hash.SetModeControl: Cannot change mode control during processing.');
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.AddTreeLevel;
begin
SetLength(fState.Levels,Length(fState.Levels) + 1);
with fState.Levels[High(fState.Levels)] do
  begin
    Index := 0;
    Bytes := 0;
    InitializeBlock(Block,fRounds,fKey);
    // init vector for sequential processing
    If Length(fState.Levels) > fModeControl then
      Bytes := MD6_CHUNK_SIZE;  // empty chunk, SetLength intialized it to all zero
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ProcessTreeNode(Level: Integer; const Data);
begin
If Level <= High(fState.Levels) then  // sanity check
  begin
  {
    Cannot use "with fState.Levels[Level] do" - the array can be reallocated
    within the function (by TreeAddLevel), which invalidates the used pointer
    in subsequent code.
  }
    If fState.Levels[Level].Bytes >= MD6_BLOCK_CAPACITY then
      begin
      {
        Block at selected level is full, compress it and pass chaining variable
        up the tree for further processing.
      }
        with fState.Levels[Level] do
          begin
            Block[MD6_BLOCK_IDX_U] := GetUniqueNodeIDWord(Succ(Level),Index);
            Block[MD6_BLOCK_IDX_V] := GetControlWord(fRounds,fModeControl,0,Length(fKey),fHashBits,False);
            CompressBlock(Block);
            Inc(Index);
            Bytes := 0;
          end;
      {
        The succ is here because we start the level indices at 0, mode control
        expects them to start at 1.
      }
        If Succ(Level) <= fModeControl then
          begin
            // not at the maximum tree hight
            If Level >= High(fState.Levels) then
              AddTreeLevel; // there is no next level as of yet, add it
            // put chaining variable into the next level block
            with fState.Levels[Level] do
              ProcessTreeNode(Succ(Level),Block[Length(Block) - MD6_CHUNK_LEN]);
          end
        else with fState.Levels[Level] do
          begin
          {
            We are at the maximum tree hight, meaning current block is
            sequential, copy chaining variable back to it.
          }          
            Move(Block[Length(Block) - MD6_CHUNK_LEN],Block[MD6_BLOCK_DATASTART],MD6_CHUNK_SIZE);
            Inc(Bytes,MD6_CHUNK_SIZE);
           end;
      end;
    // copy data to state block
    with fState.Levels[Level] do
      begin
        Move(Data,PtrAdvance(Addr(Block[MD6_BLOCK_DATASTART]),PtrInt(Bytes))^,MD6_CHUNK_SIZE);
        Inc(Bytes,MD6_CHUNK_SIZE);
      end;
  end
else raise EMD6ProcessingError.CreateFmt('TMD6Hash.ProcessTreeNode: Cannot process non-existing level (%d).',[Level]);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ProcessTreeNodeFinal(Level: Integer; PadBytes: TMemSize);
begin
If Level <= High(fState.Levels) then
  begin
    with fState.Levels[Level] do
      begin
        If Bytes < MD6_BLOCK_CAPACITY then
          begin
            // block is not full, fill the rest wit zeroes
            FillChar(PtrAdvance(Addr(Block[MD6_BLOCK_DATASTART]),PtrInt(Bytes))^,MD6_BLOCK_CAPACITY - Bytes,0);
            Inc(PadBytes,MD6_BLOCK_CAPACITY - Bytes);
          end;
        Block[MD6_BLOCK_IDX_U] := GetUniqueNodeIDWord(Succ(Level),Index);
        Block[MD6_BLOCK_IDX_V] := GetControlWord(fRounds,fModeControl,PadBytes * 8,Length(fKey),fHashBits,Level >= High(fState.Levels));
        CompressBlock(Block);
      end;
    If Level < High(fState.Levels) then
      begin
        // we are not at the top-most node, pass result from current block to the next and process it
        with fState.Levels[Level] do
          ProcessTreeNode(Succ(Level),Block[Length(Block) - MD6_CHUNK_LEN]);
        ProcessTreeNodeFinal(Succ(Level),0);
      end
    else If Length(fMD6) > 0 then
      // top-most node, copy final result
      with fState.Levels[Level] do
        Move(PtrAdvance(Addr(Block[High(Block)]),PtrInt(SizeOf(TMD6Word) - Length(fMD6)))^,fMD6[0],Length(fMD6));
  end
else raise EMD6ProcessingError.CreateFmt('TMD6Hash.ProcessTreeNodeFinal: Cannot process non-existing level (%d).',[Level]);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ProcessBlock(const Block);
begin
ProcessTreeNode(0,Block);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ProcessFirst(const Block);
begin
fProcessing := True;
inherited;
AddTreeLevel;
ProcessTreeNode(0,Block);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ProcessLast;
var
  PadBytes: TMemSize;
begin
If fFirstBlock then
  AddTreeLevel;
If fTransCount > 0 then
  begin
    PadBytes := MD6_CHUNK_SIZE - fTransCount;
    FillChar(PtrAdvance(fTransBlock,PtrInt(fTransCount))^,PadBytes,0);
    ProcessTreeNode(0,fTransBlock^);
  end
else PadBytes := 0;
ProcessTreeNodeFinal(0,PadBytes);
fProcessing := False;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.Initialize;
begin
fBlockSize := MD6_CHUNK_SIZE;
inherited;
Setlength(fMD6,MD6_BITS_DEFAULT div 8);
fHashBits := MD6_BITS_DEFAULT;
SetLength(fKey,0);
fRounds := 168; // 40 + (512 / 4)
fRoundsDef := True;
fModeControl := MD6_MODE_DEFAULT;
SetLength(fState.Levels,0);
fProcessing := False;
end;

{-------------------------------------------------------------------------------
    TMD6Hash - public methods
-------------------------------------------------------------------------------}

class Function TMD6Hash.MD6ToLE(MD6: TMD6): TMD6;
begin
Result := Copy(MD6);
end;

//------------------------------------------------------------------------------

class Function TMD6Hash.MD6ToBE(MD6: TMD6): TMD6;
begin
Result := Copy(MD6);
end;

//------------------------------------------------------------------------------

class Function TMD6Hash.MD6FromLE(MD6: TMD6): TMD6;
begin
Result := Copy(MD6);
end;

//------------------------------------------------------------------------------

class Function TMD6Hash.MD6FromBE(MD6: TMD6): TMD6;
begin
Result := Copy(MD6);
end;

//------------------------------------------------------------------------------

Function TMD6Hash.HashSize: TMemSize;
begin
Result := fHashBits div 8;
end;

//------------------------------------------------------------------------------

class Function TMD6Hash.HashName: String;
begin
Result := 'MD6';
end;

//------------------------------------------------------------------------------

class Function TMD6Hash.HashEndianness: THashEndianness;
begin
Result := heBig;
end;

//------------------------------------------------------------------------------

class Function TMD6Hash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TMD6Hash.CreateAndInitFrom(Hash: THashBase);
var
  i:  Integer;
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMD6Hash then
  begin
    fMD6 := Copy(TMD6Hash(Hash).fMD6);
    fHashBits := TMD6Hash(Hash).fHashBits;
    fKey := Copy(TMD6Hash(Hash).Key);
    fRounds := TMD6Hash(Hash).fRounds;
    fRoundsDef := TMD6Hash(Hash).fRoundsDef;
    fModeControl := TMD6Hash(Hash).fModeControl;
    // state copy
    fState := TMD6Hash(Hash).fState;
    SetLength(fState.Levels,Length(fState.Levels));
    For i := Low(fState.Levels) to High(fState.Levels) do
      SetLength(fState.Levels[i].Block,Length(fState.Levels[i].Block));
    fProcessing := TMD6Hash(Hash).fProcessing;
  end
else raise EMD6IncompatibleClass.CreateFmt('TMD6Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TMD6Hash.CreateAndInitFrom(Hash: TMD6);
begin
CreateAndInit;
SetMD6(Hash); // checks for length
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.Init;
begin
inherited;
SetLength(fState.Levels,0);
end;

//------------------------------------------------------------------------------

Function TMD6Hash.Compare(Hash: THashBase): Integer;
var
  Temp: TMD6;
  i:    Integer;
begin
If Hash is Self.ClassType then
  begin
    Result := 0;
    Temp := TMD6Hash(Hash).fMD6;
    If Length(fMD6) = Length(Temp) then
      begin
        For i := Low(fMD6) to High(fMD6) do
          If fMD6[i] > Temp[i] then
            begin
              Result := +1;
              Break;
            end
          else If fMD6[i] < Temp[i] then
            begin
              Result := -1;
              Break;
            end;
      end
    else raise EMD6SizeMismatch.CreateFmt('TMD6Hash.Compare: Cannot compare hashes of differing lengths (%d,%d).',[Length(fMD6),Length(Temp)]);
  end
else raise EMD6IncompatibleClass.CreateFmt('TMD6Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMD6Hash.AsString: String;
const
  HEX_TAB: array[0..15] of Char =
    ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  i:  Integer;
begin
Result := StringOfChar('0',Length(fMD6) * 2);
For i := Low(fMD6) to High(fMD6) do
  begin
    Result[(i * 2) + 1] := HEX_TAB[fMD6[i] shr 4];
    Result[(i * 2) + 2] := HEX_TAB[fMD6[i] and 15];
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.FromString(const Str: String);
var
  Temp: TMD6;
  i:    Integer;
begin
If (Length(Str) >= 2) and (Length(Str) <= (MD6_BITS_MAX div 4)) then
  begin
    SetLength(Temp,Length(Str) div 2);
    For i := Low(Temp) to High(Temp) do
      Temp[i] := UInt8(StrToInt('$' + Copy(Str,(i * 2) + 1,2)));
    SetMD6(Temp);
  end
else raise EMD6InvalidValue.CreateFmt('TMD6Hash.FromString: Invalid string length (%d).',[Length(Str)]);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.FromStringDef(const Str: String; const Default: TMD6);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  SetMD6(Default);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMD6;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}MD6ToBE{$ELSE}MD6ToLE{$ENDIF}(fMD6);
  heLittle: Temp := MD6ToLE(fMD6);
  heBig:    Temp := MD6ToBE(fMD6);
else
 {heDefault}
  Temp := fMD6;
end;
If Length(Temp) > 0 then
  Stream.WriteBuffer(Addr(Temp[0])^,Length(Temp));
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMD6;
begin
SetLength(Temp,Length(fMD6));
If Length(Temp) > 0 then
  begin
    Stream.ReadBuffer(Addr(Temp[0])^,Length(Temp));
    case Endianness of
      heSystem: SetMD6({$IFDEF ENDIAN_BIG}MD6FromBE{$ELSE}MD6FromLE{$ENDIF}(Temp));
      heLittle: SetMD6(MD6FromLE(Temp));
      heBig:    SetMD6(MD6FromBE(Temp));
    else
     {heDefault}
      SetMD6(Temp);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetKey(const Key; Size: TMemSize);
var
  TempKey:  TMD6Key;
begin
If Size <= MD6_KEY_MAXLEN then
  SetLength(TempKey,Size)
else
  SetLength(TempKey,MD6_KEY_MAXLEN);
If Size > 0 then
  Move(Key,Addr(TempKey[0])^,Length(TempKey));
Self.Key := TempKey; // calls full setter
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TMD6Hash.SetKey(const Key: String);
var
  TempStr:  UTF8String;
begin
TempStr := StrToUTF8(Key);
SetKey(PUTF8Char(TempStr)^,Length(TempStr) * SizeOf(UTF8Char));
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6DefHash                                  
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6DefHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6DefHash - protected methods
-------------------------------------------------------------------------------}

procedure TMD6DefHash.SetMD6(Value: TMD6);
begin
If Length(Value) = Length(fMD6) then
  inherited SetMD6(fMD6)
else
  raise EMD6SizeMismatch.CreateFmt('TMD6DefHash.SetMD6: Size mismatch (%d,%d).',[Length(fMD6),Length(Value)]);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetHashBits(Value: Integer);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.SetHashBits: Changing hash bits not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.PutKey(Value: TMD6Key);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.PutKey: Key not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetRounds(Value: Integer);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.PuSetRoundstKey: Changing number of rounds not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetModeControl(Value: Integer);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.PuSetRoundstKey: Changing mode control not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{-------------------------------------------------------------------------------
    TMD6DefHash - public methods
-------------------------------------------------------------------------------}

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetKey(const Key; Size: TMemSize);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.SetKey: Key not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetKey(const Key: String);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.SetKey: Key not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6_224Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6_224Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6_224Hash - protected methods
-------------------------------------------------------------------------------}

Function TMD6_224Hash.GetMD6_224: TMD6_224;
begin
Move(fMD6[0],Addr(Result)^,SizeOf(Result))
end;

//------------------------------------------------------------------------------

procedure TMD6_224Hash.Initialize;
begin
inherited;
Setlength(fMD6,28);
fHashBits := 224;
fRounds := 96;  // 40 + (224 / 4)
end;

{-------------------------------------------------------------------------------
    TMD6_224Hash - public methods
-------------------------------------------------------------------------------}

class Function TMD6_224Hash.MD6_224ToLE(MD6: TMD6_224): TMD6_224;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_224Hash.MD6_224ToBE(MD6: TMD6_224): TMD6_224;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_224Hash.MD6_224FromLE(MD6: TMD6_224): TMD6_224;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_224Hash.MD6_224FromBE(MD6: TMD6_224): TMD6_224;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_224Hash.HashName: String;
begin
Result := 'MD6-224';
end;

//------------------------------------------------------------------------------

constructor TMD6_224Hash.CreateAndInitFrom(Hash: THashBase);
begin
If Hash is TMD6_224Hash then
  inherited CreateAndInitFrom(Hash)
else
  raise EMD6IncompatibleClass.CreateFmt('TMD6_224Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TMD6_224Hash.CreateAndInitFrom(Hash: TMD6);
begin
If Length(Hash) = SizeOf(TMD6_224) then
  inherited CreateAndInitFrom(Hash)
else
  raise EMD6SizeMismatch.CreateFmt('TMD6_224Hash.CreateAndInitFrom: Incompatible hash size (%d).',[Length(Hash)]);
end;

//------------------------------------------------------------------------------

constructor TMD6_224Hash.CreateAndInitFrom(Hash: TMD6_224);
begin
CreateAndInit;
Move(Hash,fMD6[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

procedure TMD6_224Hash.FromStringDef(const Str: String; const Default: TMD6_224);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  Move(Default,fMD6[0],SizeOf(Default));
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6_256Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6_256Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6_256Hash - protected methods
-------------------------------------------------------------------------------}

Function TMD6_256Hash.GetMD6_256: TMD6_256;
begin
Move(fMD6[0],Addr(Result)^,SizeOf(Result))
end;

//------------------------------------------------------------------------------

procedure TMD6_256Hash.Initialize;
begin
inherited;
Setlength(fMD6,32);
fHashBits := 256;
fRounds := 104; // 40 + (256 / 4)
end;

{-------------------------------------------------------------------------------
    TMD6_256Hash - public methods
-------------------------------------------------------------------------------}

class Function TMD6_256Hash.MD6_256ToLE(MD6: TMD6_256): TMD6_256;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_256Hash.MD6_256ToBE(MD6: TMD6_256): TMD6_256;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_256Hash.MD6_256FromLE(MD6: TMD6_256): TMD6_256;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_256Hash.MD6_256FromBE(MD6: TMD6_256): TMD6_256;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_256Hash.HashName: String;
begin
Result := 'MD6-256';
end;

//------------------------------------------------------------------------------

constructor TMD6_256Hash.CreateAndInitFrom(Hash: THashBase);
begin
If Hash is TMD6_256Hash then
  inherited CreateAndInitFrom(Hash)
else
  raise EMD6IncompatibleClass.CreateFmt('TMD6_256Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TMD6_256Hash.CreateAndInitFrom(Hash: TMD6);
begin
If Length(Hash) = SizeOf(TMD6_256) then
  inherited CreateAndInitFrom(Hash)
else
  raise EMD6SizeMismatch.CreateFmt('TMD6_256Hash.CreateAndInitFrom: Incompatible hash size (%d).',[Length(Hash)]);
end;

//------------------------------------------------------------------------------

constructor TMD6_256Hash.CreateAndInitFrom(Hash: TMD6_256);
begin
CreateAndInit;
Move(Hash,fMD6[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

procedure TMD6_256Hash.FromStringDef(const Str: String; const Default: TMD6_256);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  Move(Default,fMD6[0],SizeOf(Default));
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6_384Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6_384Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6_384Hash - protected methods
-------------------------------------------------------------------------------}

Function TMD6_384Hash.GetMD6_384: TMD6_384;
begin
Move(fMD6[0],Addr(Result)^,SizeOf(Result))
end;

//------------------------------------------------------------------------------

procedure TMD6_384Hash.Initialize;
begin
inherited;
Setlength(fMD6,48);
fHashBits := 384;
fRounds := 136; // 40 + (384 / 4)
end;

{-------------------------------------------------------------------------------
    TMD6_384Hash - public methods
-------------------------------------------------------------------------------}

class Function TMD6_384Hash.MD6_384ToLE(MD6: TMD6_384): TMD6_384;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_384Hash.MD6_384ToBE(MD6: TMD6_384): TMD6_384;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_384Hash.MD6_384FromLE(MD6: TMD6_384): TMD6_384;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_384Hash.MD6_384FromBE(MD6: TMD6_384): TMD6_384;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_384Hash.HashName: String;
begin
Result := 'MD6-384';
end;

//------------------------------------------------------------------------------

constructor TMD6_384Hash.CreateAndInitFrom(Hash: THashBase);
begin
If Hash is TMD6_384Hash then
  inherited CreateAndInitFrom(Hash)
else
  raise EMD6IncompatibleClass.CreateFmt('TMD6_384Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TMD6_384Hash.CreateAndInitFrom(Hash: TMD6);
begin
If Length(Hash) = SizeOf(TMD6_384) then
  inherited CreateAndInitFrom(Hash)
else
  raise EMD6SizeMismatch.CreateFmt('TMD6_384Hash.CreateAndInitFrom: Incompatible hash size (%d).',[Length(Hash)]);
end;

//------------------------------------------------------------------------------

constructor TMD6_384Hash.CreateAndInitFrom(Hash: TMD6_384);
begin
CreateAndInit;
Move(Hash,fMD6[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

procedure TMD6_384Hash.FromStringDef(const Str: String; const Default: TMD6_384);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  Move(Default,fMD6[0],SizeOf(Default));
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6_512Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6_512Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6_512Hash - protected methods
-------------------------------------------------------------------------------}

Function TMD6_512Hash.GetMD6_512: TMD6_512;
begin
Move(fMD6[0],Addr(Result)^,SizeOf(Result))
end;

//------------------------------------------------------------------------------

procedure TMD6_512Hash.Initialize;
begin
inherited;
Setlength(fMD6,64);
fHashBits := 512;
fRounds := 168; // 40 + (512 / 4)
end;

{-------------------------------------------------------------------------------
    TMD6_512Hash - public methods
-------------------------------------------------------------------------------}

class Function TMD6_512Hash.MD6_512ToLE(MD6: TMD6_512): TMD6_512;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_512Hash.MD6_512ToBE(MD6: TMD6_512): TMD6_512;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_512Hash.MD6_512FromLE(MD6: TMD6_512): TMD6_512;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_512Hash.MD6_512FromBE(MD6: TMD6_512): TMD6_512;
begin
Result := MD6;
end;

//------------------------------------------------------------------------------

class Function TMD6_512Hash.HashName: String;
begin
Result := 'MD6-512';
end;

//------------------------------------------------------------------------------

constructor TMD6_512Hash.CreateAndInitFrom(Hash: THashBase);
begin
If Hash is TMD6_512Hash then
  inherited CreateAndInitFrom(Hash)
else
  raise EMD6IncompatibleClass.CreateFmt('TMD6_512Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TMD6_512Hash.CreateAndInitFrom(Hash: TMD6);
begin
If Length(Hash) = SizeOf(TMD6_512) then
  inherited CreateAndInitFrom(Hash)
else
  raise EMD6SizeMismatch.CreateFmt('TMD6_512Hash.CreateAndInitFrom: Incompatible hash size (%d).',[Length(Hash)]);
end;

//------------------------------------------------------------------------------

constructor TMD6_512Hash.CreateAndInitFrom(Hash: TMD6_512);
begin
CreateAndInit;
Move(Hash,fMD6[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

procedure TMD6_512Hash.FromStringDef(const Str: String; const Default: TMD6_512);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  Move(Default,fMD6[0],SizeOf(Default));
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TMD6WorkerThread
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6WorkerThread - class declaration
===============================================================================}
type
  TMD6WorkerThread = class(TThread)
  protected
    fThreadFunction:  TMD6ThreadFunction;
    fParam:           Pointer;
    procedure Execute; override;
  public
    constructor Create(ThreadFunction: TMD6ThreadFunction; Param: Pointer);
  end;

{===============================================================================
    TMD6WorkerThread - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6WorkerThread - protected methods
-------------------------------------------------------------------------------}

procedure TMD6WorkerThread.Execute;
begin
fThreadFunction(fParam);
end;

{-------------------------------------------------------------------------------
    TMD6WorkerThread - public methods
-------------------------------------------------------------------------------}

constructor TMD6WorkerThread.Create(ThreadFunction: TMD6ThreadFunction; Param: Pointer);
begin
inherited Create(False);
FreeOnTerminate := True;
fThreadFunction := ThreadFunction;
fParam := Param;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TMD6HashParallel
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6HashParallel - thread processing
===============================================================================}

procedure ThreadFunction(ThreadsDataPtr: Pointer);
begin
TMD6HashParallel(ThreadsDataPtr).Thread_Execute;
end;

{===============================================================================
    TMD6HashParallel - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6HashParallel - protected methods
-------------------------------------------------------------------------------}

Function TMD6HashParallel.GetMD6: TMD6;
begin
Result := Copy(fMD6);
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.SetHashBits(Value: Integer);
begin
If not fProcessing then
  begin
    If (Value > 0) and (Value <= MD6_BITS_MAX) and ((Value and 7) = 0) then
      begin
        SetLength(fMD6,0);  // prevents copying
        SetLength(fMD6,Value div 8);
        fHashSettings.HashBits := Value;
        SetRoundsDefault;
      end
    else raise EMD6InvalidValue.CreateFmt('TMD6HashParallel.SetHashBits: Invalid hash bits value (%d).',[Value]);
  end
else raise EMD6InvalidState.Create('TMD6HashParallel.SetHashBits: Cannot change hash bits during processing.');
end;

//------------------------------------------------------------------------------

Function TMD6HashParallel.GetKey: TMD6Key;
begin
Result := Copy(fHashSettings.Key);
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.PutKey(Value: TMD6Key);
begin
If not fProcessing then
  begin
    If Length(Value) <= MD6_KEY_MAXLEN then
      begin
        fHashSettings.Key := Copy(Value);
        SetRoundsDefault;
      end
    else raise EMD6InvalidValue.CreateFmt('TMD6HashParallel.PetKey: Invalid key length (%d).',[Length(Value)]);
  end
else raise EMD6InvalidState.Create('TMD6HashParallel.PutKey: Cannot change key during processing.');
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.SetRoundsDefault;
begin
If fHashSettings.RoundsDef then
  begin
    If Length(fHashSettings.Key) > 0 then
      fHashSettings.Rounds := Max(80,40 + (fHashSettings.HashBits div 4))
    else
      fHashSettings.Rounds := 40 + (fHashSettings.HashBits div 4);
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.SetRounds(Value: Integer);
begin
If not fProcessing then
  begin
    If Value >= 0 then
      begin
        fHashSettings.Rounds := Value;
        fHashSettings.RoundsDef := False;
      end
    else raise EMD6InvalidValue.CreateFmt('TMD6HashParallel.SetRounds: Invalid number of rounds (%d).',[Value]);
  end
else raise EMD6InvalidState.Create('TMD6HashParallel.SetRounds: Cannot change number of rounds during processing.');
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.SetModeControl(Value: Integer);
begin
If not fProcessing then
  begin
    If (Value >= 0) and (Value <= 64) then
      fHashSettings.ModeControl := Value
    else
      raise EMD6InvalidValue.CreateFmt('TMD6HashParallel.SetModeControl: Invalid mode control (%d).',[Value]);
  end
else raise EMD6InvalidState.Create('TMD6HashParallel.SetModeControl: Cannot change mode control during processing.');
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.SetMaxThreads(Value: Integer);
begin
If not fProcessing then
  begin
    If Value > 0 then
      fHashSettings.MaxThreads := Value
    else
      raise EMD6InvalidValue.CreateFmt('TMD6HashParallel.SetMaxThreads: Invalid maximum number of threads (%d).',[Value]);
  end
else raise EMD6InvalidState.Create('TMD6HashParallel.SetMaxThreads: Cannot change maximum number of threads during processing.');
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.SetMaxSeqNodes(Value: Int64);
begin
If not fProcessing then
  begin
    If Value > 0 then
      fHashSettings.MaxSeqNodes := Value
    else
      raise EMD6InvalidValue.CreateFmt('TMD6HashParallel.SetMaxSeqNodes: Invalid maximum number of sequential nodes (%d).',[Value]);
  end
else raise EMD6InvalidState.Create('TMD6HashParallel.SetMaxSeqNodes: Cannot change maximum number of sequential nodes during processing.');
end;

//------------------------------------------------------------------------------

Function TMD6HashParallel.Thread_GetTask(var ThreadTask: TMD6ThreadTask): Boolean;
var
  i:  Integer;
begin
If not InterlockedLoad(fProcessingVariables.Terminated) then
  begin
    ThreadTask.CurrentOffset := InterlockedExchangeAdd(fProcessingVariables.CurrentOffset,fTasksSettings.BytesPerTask);
    // prepare processing state
    // +1 ... the last level is used only to hold the chaining variable to be passed to common levels
    SetLength(ThreadTask.State.Levels,fProcessingSettings.ThreadLevelCount + 1);
    For i := Low(ThreadTask.State.Levels) to High(ThreadTask.State.Levels) do
      begin
        If i <= Low(ThreadTask.State.Levels) then
          ThreadTask.State.Levels[i].Index := ThreadTask.CurrentOffset div MD6_BLOCK_CAPACITY
        else If i >= High(ThreadTask.State.Levels) then
          ThreadTask.State.Levels[i].Index := ThreadTask.State.Levels[Pred(i)].Index
        else
          ThreadTask.State.Levels[i].Index := ThreadTask.State.Levels[Pred(i)].Index div 4;
        ThreadTask.State.Levels[i].Bytes := 0;
        InitializeBlock(ThreadTask.State.Levels[i].Block,fHashSettings.Rounds,fHashSettings.Key);
      end;
    Result := ThreadTask.CurrentOffset < fInputMessage.Size;
  end
else raise EMD6ParallelInternal.Create('TMD6HashParallel.Thread_GetTask: Processing terminated.');
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.Thread_ProcessThreadNode(var ThreadTask: TMD6ThreadTask; Level: Integer; const Data);
begin
If Level <= High(ThreadTask.State.Levels) then
  begin
    If ThreadTask.State.Levels[Level].Bytes >= MD6_BLOCK_CAPACITY then
      begin
        with ThreadTask.State.Levels[Level] do
          begin
            Block[MD6_BLOCK_IDX_U] := GetUniqueNodeIDWord(Succ(Level),Index);
            Block[MD6_BLOCK_IDX_V] := GetControlWord(
              fHashSettings.Rounds,fHashSettings.ModeControl,0,
              Length(fHashSettings.Key),fHashSettings.HashBits,False);
            CompressBlock(Block);
            Inc(Index);
            Bytes := 0;
            Thread_ProcessThreadNode(ThreadTask,Succ(Level),Block[Length(Block) - MD6_CHUNK_LEN]);
          end; 
      end;
    with ThreadTask.State.Levels[Level] do
      begin
        Move(Data,PtrAdvance(Addr(Block[MD6_BLOCK_DATASTART]),PtrInt(Bytes))^,MD6_CHUNK_SIZE);
        Inc(Bytes,MD6_CHUNK_SIZE);
      end;      
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.Thread_ProcessThreadNodeFinal(var ThreadTask: TMD6ThreadTask; Level: Integer; PadBytes: TMemSize);
begin
// note that the last level is only used for storage
If Level < High(ThreadTask.State.Levels) then
  begin
    with ThreadTask.State.Levels[Level] do
      begin
        If Bytes < MD6_BLOCK_CAPACITY then
          begin
            FillChar(PtrAdvance(Addr(Block[MD6_BLOCK_DATASTART]),PtrInt(Bytes))^,MD6_BLOCK_CAPACITY - Bytes,0);
            Inc(PadBytes,MD6_BLOCK_CAPACITY - Bytes);
          end;
        Block[MD6_BLOCK_IDX_U] := GetUniqueNodeIDWord(Succ(Level),Index);
        Block[MD6_BLOCK_IDX_V] := GetControlWord(
          fHashSettings.Rounds,fHashSettings.ModeControl,PadBytes * 8,
          Length(fHashSettings.Key),fHashSettings.HashBits,False);
        CompressBlock(Block);
        Thread_ProcessThreadNode(ThreadTask,Succ(Level),Block[Length(Block) - MD6_CHUNK_LEN]);
        Thread_ProcessThreadNodeFinal(ThreadTask,Succ(Level),0);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.Thread_CommonNodesEntry(const ThreadTask: TMD6ThreadTask);
var
  BindingIndex:       Integer;
  BindingSequential:  Boolean;
begin
BindingIndex := Integer(ThreadTask.State.Levels[High(ThreadTask.State.Levels)].Index);
BindingSequential := (Length(fCommonTree.State.Levels) <= 1) and fProcessingSettings.Sequential;
// get connected node
with fCommonTree.State do
  Thread_ProcessCommonNode(
    Addr(Levels[Low(Levels)].Nodes[BindingIndex div IfThen(BindingSequential,3,4)]),
    IfThen(BindingSequential,1 + (BindingIndex mod 3),BindingIndex mod 4),
    ThreadTask.State.Levels[High(ThreadTask.State.Levels)].Block[MD6_BLOCK_DATASTART]);
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.Thread_ProcessCommonNode(NodePtr: PMD6CommonProcessingNode; PutIndex: Integer; const Data);
begin
If Assigned(NodePtr) then
  begin
    // copy data to put index, increment bytes, decrement counter
    Move(Data,NodePtr^.Block[MD6_BLOCK_DATASTART + (PutIndex * MD6_CHUNK_LEN)],MD6_CHUNK_SIZE);
    InterlockedAdd(NodePtr^.Bytes,MD6_CHUNK_SIZE);
    // if counter is at 0, we can process the block
    If InterlockedDecrement(NodePtr^.Counter) <= 0 then
      begin
        ReadWriteBarrier; // make sure the move is complete
        NodePtr^.Block[MD6_BLOCK_IDX_U] := GetUniqueNodeIDWord(NodePtr^.LevelIndex,NodePtr^.Index);
        NodePtr^.Block[MD6_BLOCK_IDX_V] := GetControlWord(
          fHashSettings.Rounds,fHashSettings.ModeControl,(MD6_BLOCK_CAPACITY - NodePtr^.Bytes) * 8,
          Length(fHashSettings.Key),fHashSettings.HashBits,NodePtr^.IsLastNode);
        CompressBlock(NodePtr^.Block);
        // next node...
        If Assigned(NodePtr^.ParentNodePtr) and (NodePtr^.ParentNodePutIndex >= 0) then
          Thread_ProcessCommonNode(NodePtr^.ParentNodePtr,NodePtr^.ParentNodePutIndex,
            NodePtr^.Block[Length(NodePtr^.Block) - MD6_CHUNK_LEN]);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.Thread_Execute;
var
  WorkStream: TStream;
  ThreadTask: TMD6ThreadTask;
  i:          Integer;
  Buffer:     array[0..Pred(MD6_CHUNK_SIZE)] of UInt8;
  BytesRead:  Integer;
  PadBytes:   Integer;
begin
try
  // open the input
  If fInputMessage.ProcessingMode = tmFile then
    WorkStream := TFileStream.Create(StrToRTL(fInputMessage.FileName),fmOpenRead or fmShareDenyWrite)
  else
    WorkStream := TStaticMemoryStream.Create(fInputMessage.Buffer,fInputMessage.Size);
  try
    ThreadTask.CurrentOffset := 0;
    SetLength(ThreadTask.State.Levels,0);
    while Thread_GetTask(ThreadTask) do
      begin
        WorkStream.Seek(ThreadTask.CurrentOffset,soBeginning);
        i := 0;
        PadBytes := 0;
        repeat
          BytesRead := WorkStream.Read(Addr(Buffer)^,SizeOf(Buffer));
          If BytesRead > 0 then
            begin
              // do padding if needed
              If BytesRead < SizeOf(Buffer) then
                begin
                  PadBytes := SizeOf(Buffer) - BytesRead;
                  FillChar(Buffer[BytesRead],PadBytes,0);
                end;
              // pass data for processing
              Thread_ProcessThreadNode(ThreadTask,0,Buffer);
              InterlockedAdd(fProgressTracking.CurrentIntProgress,BytesRead);
            end;
          Inc(i);
        until (i >= fTasksSettings.ChunksPerTask) or (BytesRead < SizeOf(Buffer));
        Thread_ProcessThreadNodeFinal(ThreadTask,0,PadBytes);
        // do sanity check before continuing to common-levels
        If ThreadTask.State.Levels[High(ThreadTask.State.Levels)].Bytes = MD6_CHUNK_SIZE then
          Thread_CommonNodesEntry(ThreadTask)
        else
          raise EMD6ParallelInternal.Create('TMD6HashParallel.Thread_Execute: processing failed at thread level.');
      end;
  finally
    FreeAndNil(WorkStream);
  end;
except
  // eat all exceptions
  InterlockedStore(fProcessingVariables.Terminated,True);
end;
If InterlockedDecrement(fProcessingVariables.StopCounter) <= 0 then
  fDoneEvent.Unlock;
end;

//------------------------------------------------------------------------------

Function TMD6HashParallel.ParallelPossible(DataSize: UInt64): Boolean;
begin
If (DataSize > MD6_BLOCK_CAPACITY) and (fHashSettings.ModeControl > 0) and (fHashSettings.MaxThreads > 1) then
  begin
    // is tree level count greater than mode control?
    If Succ(Ceil(LogN(4,Ceil64(DataSize / MD6_BLOCK_CAPACITY)))) > fHashSettings.ModeControl then
      // check required number of sequential nodes
      Result := fHashSettings.MaxSeqNodes >= Ceil64(Ceil64(DataSize / (UInt64(Trunc(
        IntPower(4,Pred(fHashSettings.ModeControl)))) * MD6_BLOCK_CAPACITY)) / 3)
    else
      Result := True;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.ParallelPrepare;
var
  TreeLevelCount:   Integer;
  CommonLevelCount: Integer;
  TaskCount:        UInt64;

  procedure PrepareTasksSettings;
  begin
    CommonLevelCount := TreeLevelCount - fProcessingSettings.ThreadLevelCount;
    // task settings
    fTasksSettings.ChunksPerTask := UInt64(Trunc(IntPower(4,Pred(fProcessingSettings.ThreadLevelCount)))) * 4;
    fTasksSettings.BytesPerTask := fTasksSettings.ChunksPerTask * MD6_CHUNK_SIZE;
    TaskCount := UInt64(Ceil64(fInputMessage.Size / fTasksSettings.BytesPerTask));
  end;

  procedure InitCommonTreeLevel(out Level: TMD6CommonProcessingLevel; TreeLevelIndex,SubNodeCount: Integer; Sequential: Boolean);
  var
    ii: Integer;
  begin
    SetLength(Level.Nodes,Ceil(SubNodeCount / IfThen(Sequential,3,4)));
    For ii := Low(Level.Nodes) to High(Level.Nodes) do
      with Level.Nodes[ii] do
        begin
          Sequential := Sequential;
          IsLastNode := False;
          ParentNodePtr := nil;
          ParentNodePutIndex := -1;
          If ii >= High(Level.Nodes) then
            Counter := SubNodeCount - IfThen(Sequential,(ii * 3) - IfThen(ii > Low(Level.Nodes),1,0),(ii * 4))
          else
            Counter := IfThen((ii <= Low(Level.Nodes)) and Sequential,3,4);
          LevelIndex := TreeLevelIndex;
          Index := ii;
          Bytes := IfThen((ii <= Low(Level.Nodes)) and Sequential,MD6_CHUNK_SIZE,0);
          InitializeBlock(Block,fHashSettings.Rounds,fHashSettings.Key);
        end;
  end;

var
  MessageBlocks:    Int64;
  SubNodeCount:     Integer;
  i,j:              Integer;
begin
MessageBlocks := Ceil64(fInputMessage.Size / MD6_BLOCK_CAPACITY);
// processing settings
fProcessingSettings.ThreadCount := Min(Int64(fHashSettings.MaxThreads),MessageBlocks);
// since MessageBlocks cannot be lower than 2, the fTreeLevelCount will always be 2 or more.
TreeLevelCount := Succ(Ceil(LogN(4,MessageBlocks)));
// will the tree degrade into sequential processing?
If TreeLevelCount > fHashSettings.ModeControl then
  begin
    fProcessingSettings.Sequential := True;
    TreeLevelCount := fHashSettings.ModeControl + 1;  // top-most node is sequential
  end
else fProcessingSettings.Sequential := False;
fProcessingSettings.ThreadLevelCount := Max(1,TreeLevelCount - Ceil(LogN(4,fProcessingSettings.ThreadCount)));
{
  following two assignments are here for FPC, so it stops throwing nonsensical
  warnings (nonsensical because both values are set in PrepareTasksSettings)
}
TaskCount := 0;
CommonLevelCount := 0;
PrepareTasksSettings;
// make sure there is "way" more tasks than threads if possible (load balancing and all that)
If (TaskCount < (fProcessingSettings.ThreadCount * 2)) and
   (fProcessingSettings.ThreadLevelCount > 1) then
  begin
    Dec(fProcessingSettings.ThreadLevelCount);
    PrepareTasksSettings;
  end;
// processing variables
fProcessingVariables.StopCounter := fProcessingSettings.ThreadCount;
fProcessingVariables.CurrentOffset := 0;
fProcessingVariables.Terminated := False;
// build the common tree
SetLength(fCommonTree.State.Levels,0);  // prevent copying
SetLength(fCommonTree.State.Levels,CommonLevelCount);
SubNodeCount := TaskCount;
For i := Low(fCommonTree.State.Levels) to High(fCommonTree.State.Levels) do
  begin
    fCommonTree.State.Levels[i].Index := Succ(fProcessingSettings.ThreadLevelCount + i);
    InitCommonTreeLevel(fCommonTree.State.Levels[i],fCommonTree.State.Levels[i].Index,
      SubNodeCount,(i >= High(fCommonTree.State.Levels)) and fProcessingSettings.Sequential);  
    SubNodeCount := Length(fCommonTree.State.Levels[i].Nodes);
  end;
// mark last node
with fCommonTree.State.Levels[High(fCommonTree.State.Levels)] do
  Nodes[High(Nodes)].IsLastNode := True;
// connect the nodes
For i := Low(fCommonTree.State.Levels) to High(fCommonTree.State.Levels) do
  with fCommonTree.State.Levels[i] do
    If (i = Pred(High(fCommonTree.State.Levels))) and fProcessingSettings.Sequential then
      begin
        For j := Low(Nodes) to High(Nodes) do
          begin
            Nodes[j].ParentNodePtr := Addr(fCommonTree.State.Levels[Succ(i)].Nodes[j div 3]);
            Nodes[j].ParentNodePutIndex := 1 + (j mod 3);
          end;      
      end
    else If i < High(fCommonTree.State.Levels) then
      begin
        For j := Low(Nodes) to High(Nodes) do
          begin
            Nodes[j].ParentNodePtr := Addr(fCommonTree.State.Levels[Succ(i)].Nodes[j div 4]);
            Nodes[j].ParentNodePutIndex := j mod 4;
          end;
      end
    else If fProcessingSettings.Sequential then
      begin
        For j := Low(Nodes) to Pred(High(Nodes)) do
          begin
            Nodes[j].ParentNodePtr := Addr(Nodes[Succ(j)]);
            Nodes[j].ParentNodePutIndex := 0;
          end
      end;
fProgressTracking.CurrentIntProgress := 0;
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

Function TMD6HashParallel.ParallelExecute(Memory: Pointer; Size: TMemSize): Integer;
begin
fInputMessage.ProcessingMode := tmBuffer;
fInputMessage.Buffer := Memory;
fInputMessage.FileName := '';
fInputMessage.Size := UInt64(Size);
Result := Self.ParallelExecute;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMD6HashParallel.ParallelExecute(const FileName: String; Size: Int64): Integer;
begin
fInputMessage.ProcessingMode := tmFile;
fInputMessage.Buffer := nil;
fInputMessage.FileName := FileName;
UniqueString(fInputMessage.FileName);
fInputMessage.Size := UInt64(Size);
Result := Self.ParallelExecute;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TMD6HashParallel.ParallelExecute: Integer;
var
  i:  Integer;
begin
ParallelPrepare;
Result := fProcessingSettings.ThreadCount;
fLastProgress := -1.0;
ProgressHandler(Self,0.0);
fDoneEvent := TEvent.Create(True,False);
try
  // run threads and wait for them to finish
  For i := 1 to fProcessingSettings.ThreadCount do
    DoThreadStart;
  If Assigned(fOnProgressEvent) or Assigned(fOnProgressCallback) then
    begin
      while True do
        If fDoneEvent.Wait(250) = wrTimeout then
          DoProgress
        else
          Break{while};
    end
  else fDoneEvent.Wait;
finally
  FreeAndNil(fDoneEvent);
end;
ReadWriteBarrier;
ProgressHandler(Self,1.0);
{
  All threads should be done by this point.

  If the processing ended without termination, pick up the result, otherwise
  signal problem by raising an exception.
}
If not fProcessingVariables.Terminated then
  begin
    // get result from the common tree top node
    If Length(fMD6) > 0 then
      with fCommonTree.State.Levels[High(fCommonTree.State.Levels)] do
        with Nodes[High(Nodes)] do
          Move(PtrAdvance(Addr(Block[High(Block)]),PtrInt(SizeOf(TMD6Word) - Length(fMD6)))^,fMD6[0],Length(fMD6));
  end
else raise EMD6ProcessingError.Create('TMD6HashParallel.ParallelExecute: Parallel processing failed.');
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.DoThreadStart;
begin
If Assigned(fOnThreadStartEvent) then
  fOnThreadStartEvent(Self,ThreadFunction,Pointer(Self))
else If Assigned(fOnThreadStartCallback) then
  fOnThreadStartCallback(Self,ThreadFunction,Pointer(Self))
else
  TMD6WorkerThread.Create(ThreadFunction,Pointer(Self));
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.Initialize;
begin
Setlength(fMD6,MD6_BITS_DEFAULT div 8);
fHashSettings.HashBits := MD6_BITS_DEFAULT;
SetLength(fHashSettings.Key,0);
fHashSettings.Rounds := 168;
fHashSettings.RoundsDef := True;
fHashSettings.ModeControl := MD6_MODE_DEFAULT;
fHashSettings.MaxThreads := ProcessorCount;
fHashSettings.MaxSeqNodes := 1024;
fProgUpdInterval := 250;
fProcessing := False;
fLastProgress := 0.0;
fOnThreadStartCallback := nil;
fOnThreadStartEvent := nil;
fOnProgressCallback := nil;
fOnProgressEvent := nil;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6HashParallel.ProgressHandler(Sender: TObject; Progress: Double);
begin
If Assigned(fOnProgressEvent) then
  fOnProgressEvent(Self,Progress)
else If Assigned(fOnProgressCallback) then
  fOnProgressCallback(Self,Progress)
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TMD6HashParallel.DoProgress;
var
  Progress: Double;
begin
If fInputMessage.Size > 0 then
  begin
    Progress := InterlockedLoad(fProgressTracking.CurrentIntProgress) / fInputMessage.Size;
    If not SameValue(Progress,fLastProgress,0.001) then
      ProgressHandler(Self,Progress);
  end
else ProgressHandler(Self,0.0);
end;

{-------------------------------------------------------------------------------
    TMD6HashParallel - public methods
-------------------------------------------------------------------------------}

class Function TMD6HashParallel.ProcessorCount: Integer;
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

constructor TMD6HashParallel.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

procedure TMD6HashParallel.SetKey(const Key; Size: TMemSize);
var
  TempKey:  TMD6Key;
begin
If Size <= MD6_KEY_MAXLEN then
  SetLength(TempKey,Size)
else
  SetLength(TempKey,MD6_KEY_MAXLEN);
If Size > 0 then
  Move(Key,Addr(TempKey[0])^,Length(TempKey));
Self.Key := TempKey;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

procedure TMD6HashParallel.SetKey(const Key: String);
var
  TempStr:  UTF8String;
begin
TempStr := StrToUTF8(Key);
SetKey(PUTF8Char(TempStr)^,Length(TempStr) * SizeOf(UTF8Char));
end;

//------------------------------------------------------------------------------

Function TMD6HashParallel.HashMemory(Memory: Pointer; Size: TMemSize): Integer;
var
  Hash: TMD6Hash;
begin
fProcessing := True;
try
  If not ParallelPossible(UInt64(Size)) then
    begin
      Hash := TMD6Hash.Create;
      try
        Hash.HashBits := fHashSettings.HashBits;
        Hash.Key := fHashSettings.Key;
        Hash.Rounds := fHashSettings.Rounds;
        Hash.ModeControl := fHashSettings.ModeControl;
        Hash.OnProgressEvent := ProgressHandler;
        Hash.HashMemory(Memory,Size);
        fMD6 := Hash.MD6;
        Result := 1;
      finally
        Hash.Free;
      end;
    end
  else Result := ParallelExecute(Memory,Size);
finally
  fProcessing := False;
end;
end;

//------------------------------------------------------------------------------

Function TMD6HashParallel.HashBuffer(const Buffer; Size: TMemSize): Integer;
begin
Result := HashMemory(@Buffer,Size);
end;

//------------------------------------------------------------------------------

Function TMD6HashParallel.HashFile(const FileName: String): Integer;
var
  Stream: TFileStream;
  Hash:   TMD6Hash;
begin
fProcessing := True;
try
  Stream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
  try
    If not ParallelPossible(UInt64(Stream.Size)) then
      begin
        Hash := TMD6Hash.Create;
        try
          Hash.HashBits := fHashSettings.HashBits;
          Hash.Key := fHashSettings.Key;
          Hash.Rounds := fHashSettings.Rounds;
          Hash.ModeControl := fHashSettings.ModeControl;
          Hash.OnProgressEvent := ProgressHandler;
          Hash.HashFile(FileName);
          fMD6 := Hash.MD6;
          Result := 1;
        finally
          Hash.Free;
        end;
      end
    else Result := ParallelExecute(FileName,Stream.Size);
  finally
    Stream.Free;
  end;
finally
  fProcessing := False;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}

Function MD6ToMD6_224(Hash: TMD6): TMD6_224;
begin
If Length(Hash) = SizeOf(Result) then
  Move(Hash[0],Addr(Result)^,SizeOf(Result))
else
  raise EMD6SizeMismatch.CreateFmt('MD6ToMD6_224: Incompatible hash size (%d).',[Length(Hash)]);
end;

//------------------------------------------------------------------------------

Function MD6ToMD6_256(Hash: TMD6): TMD6_256;
begin
If Length(Hash) = SizeOf(Result) then
  Move(Hash[0],Addr(Result)^,SizeOf(Result))
else
  raise EMD6SizeMismatch.CreateFmt('MD6ToMD6_256: Incompatible hash size (%d).',[Length(Hash)]);
end;

//------------------------------------------------------------------------------

Function MD6ToMD6_384(Hash: TMD6): TMD6_384;
begin
If Length(Hash) = SizeOf(Result) then
  Move(Hash[0],Addr(Result)^,SizeOf(Result))
else
  raise EMD6SizeMismatch.CreateFmt('MD6ToMD6_384: Incompatible hash size (%d).',[Length(Hash)]);
end;

//------------------------------------------------------------------------------

Function MD6ToMD6_512(Hash: TMD6): TMD6_512;
begin
If Length(Hash) = SizeOf(Result) then
  Move(Hash[0],Addr(Result)^,SizeOf(Result))
else
  raise EMD6SizeMismatch.CreateFmt('MD6ToMD6_512: Incompatible hash size (%d).',[Length(Hash)]);
end;

//------------------------------------------------------------------------------

Function MD6_224ToMD6(Hash: TMD6_224): TMD6;
begin
SetLength(Result,SizeOf(Hash));
Move(Hash,Result[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

Function MD6_256ToMD6(Hash: TMD6_256): TMD6;
begin
SetLength(Result,SizeOf(Hash));
Move(Hash,Result[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

Function MD6_384ToMD6(Hash: TMD6_384): TMD6;
begin
SetLength(Result,SizeOf(Hash));
Move(Hash,Result[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

Function MD6_512ToMD6(Hash: TMD6_512): TMD6;
begin
SetLength(Result,SizeOf(Hash));
Move(Hash,Result[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

Function IsCompatibleMD6_224(Hash: TMD6): Boolean;
begin
Result := Length(Hash) = SizeOf(TMD6_224);
end;

//------------------------------------------------------------------------------

Function IsCompatibleMD6_256(Hash: TMD6): Boolean;
begin
Result := Length(Hash) = SizeOf(TMD6_256);
end;

//------------------------------------------------------------------------------

Function IsCompatibleMD6_384(Hash: TMD6): Boolean;
begin
Result := Length(Hash) = SizeOf(TMD6_384);
end;

//------------------------------------------------------------------------------

Function IsCompatibleMD6_512(Hash: TMD6): Boolean;
begin
Result := Length(Hash) = SizeOf(TMD6_512);
end;

//==============================================================================

Function MD6ToStr(MD6: TMD6): String;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.CreateAndInitFrom(MD6);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToMD6(Str: String): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.CreateAndInitFromString(Str);
try  
  Result := Hash.MD6; // a copy is made internally
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function TryStrToMD6(const Str: String; out MD6: TMD6): Boolean;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  If Hash.TryFromString(Str) then
    begin
      MD6 := Hash.MD6;
      Result := True;
    end
  else Result := False;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToMD6Def(const Str: String; Default: TMD6): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.MD6;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function CompareMD6(A,B: TMD6): Integer;
var
  HashA:  TMD6Hash;
  HashB:  TMD6Hash;
begin
HashA := TMD6Hash.CreateAndInitFrom(A);
try
  HashB := TMD6Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Compare(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

//------------------------------------------------------------------------------

Function SameMD6(A,B: TMD6): Boolean;
var
  HashA:  TMD6Hash;
  HashB:  TMD6Hash;
begin
HashA := TMD6Hash.CreateAndInitFrom(A);
try
  HashB := TMD6Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Same(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

//------------------------------------------------------------------------------

Function BinaryCorrectMD6(Hash: TMD6): TMD6;
begin
Result := Copy(Hash);
end;

//==============================================================================

// internal function
Function MD6SettingsDef(HashBits: Integer): TMD6Settings;
begin
Result.HashBits := HashBits;
Result.Rounds := 40 + (HashBits div 4);
Result.ModeControl := MD6_MODE_DEFAULT;
Result.Key := nil;
end;

//------------------------------------------------------------------------------

Function MD6Settings(HashBits,Rounds,ModeControl: Integer; Key: TMD6Key): TMD6Settings;
begin
Result.HashBits := HashBits;
Result.Rounds := Rounds;
Result.ModeControl := ModeControl;
Result.Key := Copy(Key);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6Settings(HashBits,Rounds,ModeControl: Integer; const Key; KeySize: TMemSize): TMD6Settings;
begin
Result.HashBits := HashBits;
Result.Rounds := Rounds;
Result.ModeControl := ModeControl; 
If KeySize <= MD6_KEY_MAXLEN then
  SetLength(Result.Key,KeySize)
else
  SetLength(Result.Key,MD6_KEY_MAXLEN);
If KeySize > 0 then
  Move(Key,Addr(Result.Key[0])^,KeySize);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6Settings(HashBits,Rounds,ModeControl: Integer; const Key: String): TMD6Settings;
var
  TempStr:  UTF8String;
begin
Result.HashBits := HashBits;
Result.Rounds := Rounds;
Result.ModeControl := ModeControl;
TempStr := StrToUTF8(Key);
If Length(TempStr) > 0 then
  begin
    If Length(TempStr) * SizeOf(UTF8Char) <= MD6_KEY_MAXLEN then
      SetLength(Result.Key,Length(TempStr) * SizeOf(UTF8Char))
    else
      SetLength(Result.Key,MD6_KEY_MAXLEN);
    Move(PUTF8Char(TempStr)^,Result.Key[0],Length(Result.Key));
  end;
end;

//------------------------------------------------------------------------------

Function InitialMD6(Settings: TMD6Settings): TMD6State;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
Hash.HashBits := Settings.HashBits;
Hash.Rounds := Settings.Rounds;
Hash.ModeControl := Settings.ModeControl;
Hash.Key := Settings.Key; // no need to copy, setter does the copying
Hash.Init;
Result := TMD6State(Hash);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InitialMD6(HashBits: Integer = MD6_BITS_DEFAULT): TMD6State;
begin
Result := InitialMD6(MD6SettingsDef(HashBits));
end;

//------------------------------------------------------------------------------

procedure BufferMD6(State: TMD6State; const Buffer; Size: TMemSize);
begin
If Assigned(State) then
  TMD6Hash(State).Update(Buffer,Size)
else
  raise EMD6InvalidState.Create('BufferMD6: MD6 state not initialized.');
end;

//------------------------------------------------------------------------------

Function LastBufferMD6(var State: TMD6State; const Buffer; Size: TMemSize): TMD6;
begin
If Assigned(State) then
  begin
    TMD6Hash(State).Final(Buffer,Size);
    Result := TMD6Hash(State).MD6;
    TMD6Hash(State).Free;
    State := nil;
  end
else raise EMD6InvalidState.Create('LastBufferMD6: MD6 state not initialized.');
end;

//==============================================================================

Function BufferMD6(const Buffer; Size: TMemSize; Settings: TMD6Settings): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  Hash.HashBits := Settings.HashBits;
  Hash.Rounds := Settings.Rounds;
  Hash.ModeControl := Settings.ModeControl;
  Hash.Key := Settings.Key;
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.MD6;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferMD6(const Buffer; Size: TMemSize; HashBits: Integer = MD6_BITS_DEFAULT): TMD6;
begin
Result := BufferMD6(Buffer,Size,MD6SettingsDef(HashBits));
end;

//------------------------------------------------------------------------------

Function AnsiStringMD6(const Str: AnsiString; Settings: TMD6Settings): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  Hash.HashBits := Settings.HashBits;
  Hash.Rounds := Settings.Rounds;
  Hash.ModeControl := Settings.ModeControl;
  Hash.Key := Settings.Key;
  Hash.HashAnsiString(Str);
  Result := Hash.MD6;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function AnsiStringMD6(const Str: AnsiString; HashBits: Integer = MD6_BITS_DEFAULT): TMD6;
begin
Result := AnsiStringMD6(Str,MD6SettingsDef(HashBits));
end;

//------------------------------------------------------------------------------

Function WideStringMD6(const Str: WideString; Settings: TMD6Settings): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  Hash.HashBits := Settings.HashBits;
  Hash.Rounds := Settings.Rounds;
  Hash.ModeControl := Settings.ModeControl;
  Hash.Key := Settings.Key;
  Hash.HashWideString(Str);
  Result := Hash.MD6;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function WideStringMD6(const Str: WideString; HashBits: Integer = MD6_BITS_DEFAULT): TMD6;
begin
Result := WideStringMD6(Str,MD6SettingsDef(HashBits));
end;

//------------------------------------------------------------------------------

Function StringMD6(const Str: String; Settings: TMD6Settings): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  Hash.HashBits := Settings.HashBits;
  Hash.Rounds := Settings.Rounds;
  Hash.ModeControl := Settings.ModeControl;
  Hash.Key := Settings.Key;
  Hash.HashString(Str);
  Result := Hash.MD6;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StringMD6(const Str: String; HashBits: Integer = MD6_BITS_DEFAULT): TMD6;
begin
Result := StringMD6(Str,MD6SettingsDef(HashBits));
end;

//------------------------------------------------------------------------------

Function StreamMD6(Stream: TStream; Count: Int64; Settings: TMD6Settings): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  Hash.HashBits := Settings.HashBits;
  Hash.Rounds := Settings.Rounds;
  Hash.ModeControl := Settings.ModeControl;
  Hash.Key := Settings.Key;
  Hash.HashStream(Stream,Count);
  Result := Hash.MD6;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StreamMD6(Stream: TStream; Count: Int64 = -1; HashBits: Integer = MD6_BITS_DEFAULT): TMD6;
begin
Result := StreamMD6(Stream,Count,MD6SettingsDef(HashBits));
end;

//------------------------------------------------------------------------------

Function FileMD6(const FileName: String; Settings: TMD6Settings): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  Hash.HashBits := Settings.HashBits;
  Hash.Rounds := Settings.Rounds;
  Hash.ModeControl := Settings.ModeControl;
  Hash.Key := Settings.Key;
  Hash.HashFile(FileName);
  Result := Hash.MD6;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FileMD6(const FileName: String; HashBits: Integer = MD6_BITS_DEFAULT): TMD6;
begin
Result := FileMD6(FileName,MD6SettingsDef(HashBits));
end;

//==============================================================================

Function MD6_Init(Settings: TMD6Settings): TMD6Context;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
Hash.HashBits := Settings.HashBits;
Hash.Rounds := Settings.Rounds;
Hash.ModeControl := Settings.ModeControl;
Hash.Key := Settings.Key; 
Hash.Init;
Result := TMD6Context(Pointer(Hash));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6_Init(HashBits: Integer = MD6_BITS_DEFAULT): TMD6Context;
begin
Result := MD6_Init(MD6SettingsDef(HashBits));
end;

//------------------------------------------------------------------------------

procedure MD6_Update(Context: TMD6Context; const Buffer; Size: TMemSize);
begin
If Assigned(Context) then
  TMD6Hash(Context).Update(Buffer,Size)
else
  raise EMD6InvalidState.Create('MD6_Update: MD6 context not initialized.');
end;

//------------------------------------------------------------------------------

Function MD6_Final(var Context: TMD6Context; const Buffer; Size: TMemSize): TMD6;
begin
If Assigned(Context) then
  begin
    TMD6Hash(Context).Update(Buffer,Size);
    Result := MD6_Final(Context);
  end
else raise EMD6InvalidState.Create('MD6_Final: MD6 context not initialized.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6_Final(var Context: TMD6Context): TMD6;
begin
If Assigned(Context) then
  begin
    TMD6Hash(Context).Final;
    Result := TMD6Hash(Context).MD6;
    Context := nil;
  end
else raise EMD6InvalidState.Create('MD6_Final: MD6 context not initialized.');
end;

//------------------------------------------------------------------------------

Function MD6_Hash(const Buffer; Size: TMemSize; HashBits: Integer = MD6_BITS_DEFAULT): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  Hash.HashBits := HashBits;
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.MD6;
finally
  Hash.Free;
end;
end;

end.
