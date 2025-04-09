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

    Provided implementation (class TMD6Hash and its descendants) allows not
    only standard single-thread processing, but as the MD6 can be heavily
    parallelized, multi-thread processing mode was also implemented.

      WARNING - multi-thread in here means that the internal workings of the
                calculations are done in multiple execution paths, it does
                NOT mean that the TMD6Hash class is multi-thread capable (ie.
                calling its methods from multiple threads)!

      NOTE - although the MT processing was tested, I cannot guarantee it is
             bug-free and fully correct. Also, performance gain and scaling is
             uncertain, as I could not test the code on anything better than
             an ancient dual-core CPU (manufactured around year 2006).
             Linux build was tested only in virtual machine, so performance and
             stability there is also unknown.

  Version 2.0.1 (2025-04-09)

  Last change 2025-04-09

  ©2022-2025 František Milt

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
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxMath        - github.com/TheLazyTomcat/Lib.AuxMath
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps         - github.com/TheLazyTomcat/Lib.BitOps
    HashBase       - github.com/TheLazyTomcat/Lib.HashBase
    InterlockedOps - github.com/TheLazyTomcat/Lib.InterlockedOps
  * SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect

  Library SimpleCPUID is required only when compiling for x86 architecture (ie.
  32bit) and PurePascal symbol is not defined.

  Library SimpleCPUID might also be required as an indirect dependency.

  Indirect dependencies:
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit MD6;
{
  MD6_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and only this unit will be compiled in PurePascal mode.
}
{$IFDEF MD6_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

//------------------------------------------------------------------------------

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
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
  {$MODESWITCH DuplicateLocals+}
  {$MODESWITCH ClassicProcVars+}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------
// do not touch following
{$IF not Defined(PurePascal) and Defined(x86)}
  {$DEFINE VectorCompression}
{$ELSE}
  {$UNDEF VectorCompression}
{$IFEND}

interface

uses
  SysUtils, Classes, Contnrs,
  AuxTypes, AuxClasses, HashBase;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EMD6Exception = class(EHASHException);
  EMD6InvalidValue = class(EMD6Exception);
  EMD6InvalidState = class(EMD6Exception);

  EMD6SizeMismatch      = class(EMD6Exception);
  EMD6IncompatibleClass = class(EMD6Exception);

  EMD6OperationNotAllowed = class(EMD6Exception);

  EMD6ProcessingError = class(EMD6Exception);

  EMD6SemaphoreError = class(EMD6Exception);

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

  DefaultMD6Bits = 512;

{===============================================================================
--------------------------------------------------------------------------------
                                    TMD6Hash
--------------------------------------------------------------------------------
===============================================================================}
type
  // see description of OnThreadStart[*] properties of TMD6Hash class 
  TMD6ThreadFunction = procedure(Param: Pointer);

  TMD6ThreadStartCallback = procedure(Sender: TObject; ThreadFunction: TMD6ThreadFunction; Param: Pointer);
  TMD6ThreadStartEvent = procedure(Sender: TObject; ThreadFunction: TMD6ThreadFunction; Param: Pointer) of object;

{===============================================================================
    TMD6Hash - class declaration
===============================================================================}
type
  TMD6Hash = class(TBlockHash)
  protected
    fMD6:                   TMD6;
    // md6 parameters (inputs)
    fHashBits:              Integer;
    fKey:                   TMD6Key;
    fRounds:                Integer;
    fModeControl:           Integer;
    // processing settings
    fMaxThreads:            Integer;
    fParallelSizeLimit:     TMemSize;
    // processing variables
    fProcessor:             TObject;
    fProcessing:            Boolean;
    fWorkerThreads:         TObjectList;
    // events
    fOnThreadStartCallback: TMD6ThreadStartCallback;
    fOnThreadStartEvent:    TMD6ThreadStartEvent;
    // getters setters
    Function GetMD6: TMD6; virtual;
    procedure SetMD6(Value: TMD6); virtual; // not used as a setter in any property, called only directly
    procedure SetHashBits(Value: Integer); virtual;
    Function GetKey: TMD6Key; virtual;
    procedure SetKey(Value: TMD6Key); virtual;
    procedure SetRounds(Value: Integer); virtual;
    procedure SetModeControl(Value: Integer); virtual;
    procedure SetMaxThreads(Value: Integer); virtual;
    // block hash processing
    procedure ProcessBlock(const Block); override;
    procedure ProcessFirst(const Block); override;
    procedure ProcessLast; override;
    // multi-thread processing
    procedure ThreadStartHandler(Sender: TObject); virtual;
    procedure WaitAndFreeThreads; virtual;
    procedure ParallelProcessing(Address: Pointer; Size: TMemSize); virtual;
    // init/final
    procedure Initialize; override;
    procedure Finalize; override;
  public
  {
    ProcessorCount

    Returns number of processors available in the system. It can be used to
    properly set MaxThreads property when configuring multi-thread processing.
  }
    class Function ProcessorCount: Integer; virtual;
    class Function MD6ToLE(MD6: TMD6): TMD6; virtual;
    class Function MD6ToBE(MD6: TMD6): TMD6; virtual;
    class Function MD6FromLE(MD6: TMD6): TMD6; virtual;
    class Function MD6FromBE(MD6: TMD6): TMD6; virtual;
    Function HashSize: TMemSize; reintroduce;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
  {
    CreateAndInitFrom

      WARNING - first overload (accepting THashBase parameter) will fail,
                raising an exception, if the source object is currently
                processing in multi-thread or parallel mode.
  }
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMD6); overload; virtual;
    procedure Init; override;
    procedure Final; override;
    procedure HashBuffer(const Buffer; Size: TMemSize); override;
    procedure HashStream(Stream: TStream; Count: Int64 = -1); override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMD6); reintroduce; overload; virtual;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
  {
    SetupHashBits

    Sets HashBits to a passed value, zeroes and, if needed, resizes property
    MD6 and also changes property Rounds to a default value calculated using
    the following formula:

      If Length(Key) > 0 then
        Rounds := Max(80,40 + (HashBits / 4))
      else
        Rounds := 40 + (HashBits / 4)
  }
    procedure SetupHashBits(HashBits: Integer); virtual;
  {
    SetupKey

    Sets key to a selected value and changes property Rounds accordingly - see
    description of method SetupHashBits for details.

    If Key is shorter than 64 bytes, then it is back-padded with zero bytes.
    If it is longer, then it is truncated and only first 64 bytes are used.

    String key is converted to UTF8-encoded string and this new string is then
    used "as is" (ie. as an array of bytes) for the actual key.
  }
    procedure SetupKey(const Key; Size: TMemSize); overload; virtual;
    procedure SetupKey(const Key: array of Byte); overload; virtual;
    procedure SetupKey(const Key: String); overload; virtual;
  {
    BreakProcessing

    See class THashBase in library HashBase for explanation of this method.

    It is overriden here to allow for termination of worker threads in multi-
    thread or parallel processing.
  }
    Function BreakProcessing: Boolean; override;
    property MD6: TMD6 read GetMD6;
  {
    HashBits

    Length (size) of the resulting hash in bits. Only whole bytes - arbitrary
    bit lengths are not supported.

    Can be in range from 8 up to and including 512 (default is 512). Setting
    invalid or unsupported value will raise an exception.

    Assigning value to HashBits also clears (sets to all-zero) and resizes the
    MD6 property (always, not only when the value is changed) - no other input
    is changed (see method SetupHashBits).
  }
    property HashBits: Integer read fHashBits write SetHashBits;
  {
    Key

    Key used in hashing (integral part of MD6 specification) as an byte array.

    Allowed lengths are from 0 to 64 bytes. If you use longer key, then an
    exception is raised. Default value is an empty key (zero length, nil array).

      NOTE - when setting the key using this property, then no other MD6 input
             parameter is changed (see method SetupKey for more information).
  }
    property Key: TMD6Key read GetKey write SetKey;
  {
    Rounds

    Number of computing rounds performed on each processed block - internal
    thing, see MD6 specification for details. More rounds, more security,
    less paranoia :D.

    Can be any positive number, but avoid setting it to a very large value
    (eg. for default 512bit hash, it is 168). Using negative value will raise
    an exception. Default value depends on other parameters (hash bits and
    key), but at the object creation it is set to 168.
  }
    property Rounds: Integer read fRounds write SetRounds;
  {
    ModeControl

    This number controls how the hashing mechanism operates internally,
    therefore its value should not be important for the user (but changing it
    still changes the resulting hash). You should be only aware that it affects
    memory consumptions (bigger value means more memory is needed) and
    parallelization potential (again bigger means higher potential). So if you
    want, for example, to limit memory use, set it to a low value.

    Accepted range is from 0 to 64, default value is 64. Note that values from
    27 and up are all computationally equivalent (mathematical thing - see MD6
    specification for explanation).

    Mode control of zero completely rules out use of multi-thread or parallel
    processing.
  }
    property ModeControl: Integer read fModeControl write SetModeControl;
  {
    MaxThreads

    This number determines whether processing should be run synchronously in
    the context of calling thread (ie. single-thread processing), or using
    multiple execution paths (multi-thread or parallel processing - and yes,
    these are distinct modes, see further down).

    It can be set to any number above zero (default value is 1, meaning single
    thread processing). Using value of zero or lower raises an exception.

      NOTE - this number is capped at 1024. Assigning higher number will not
             raise an exception, but the value will be silently lowered to the
             limit.

      WARNING - it is not recommended to use value higher than is number of
                processors available in the system (see method ProcessorCount).
                Also, very high number may lead to an excessive memory use.

    Value of one (1) forces the execution to run in a sigle thread (the calling
    thread), value above one enables multi-thread or parallel execution and
    also sets limit on how many working threads can be spawned (use method
    ProcessorCount to get how many threads can run truly concurently).
    But note that it only enables multi-thread or parallel processing, not
    forces it. Whether it will be used depends also on mode control (must be
    above zero) and in case of parallel processing also on data size (see
    property ParallelSizeLimit for details on size limit).

    Multi-thread processing mode works the same as single-thread processing,
    ie. it runs under the standard init-update-final interface. Meaning no
    matter how you hash the data (uncluding macro functions), it can be used
    if enabled (including on zero-size data).

    Parallel processing mode is different - it can only be run on data that are
    in its entirety present in memory (so buffers, strings, streams descending
    from TCustomMemoryStream), therefore it is approached only from selected
    macro functions and run directly, without using standard i-u-f interface.

      NOTE - nature of parallel processing means the progress reporting might
             not work entirely as expected (it will be reported not based on
             amount of already processed data, but in specific time intervals).
  }
    property MaxThreads: Integer read fMaxThreads write SetMaxThreads;
  {
    ParallelSizeLimit

    Minimum size (in bytes) of a memory buffer that can be processed using
    parallel mode if that is enabled (by setting property MaxThreads to value
    above 1 and ModeControl above 0).

    If parallel processing is enabled, but the buffer is smaller than this
    limit, then it will be done using standard multi-thread processing instead
    of fully parallel one.

    Any value is allowed (setting it to zero effectively disables this limit),
    default is 4MiB (4194304 bytes).
  }
    property ParallelSizeLimit: TMemSize read fParallelSizeLimit write fParallelSizeLimit;
  {
    OnThreadStart[*]

    This event or callback (note event is called first, and if it IS assigned
    then callback is NOT called) is called when the hashing is running in
    multiple threads (multi-thread or parallel processing) and a new worker
    thread needs to be spawned.

    These are here to give user control over spawning and lifetime of worker
    (background) threads (eg. using effective thread pooling when performing
    many hashings in a row).

    Handler of this event will receive two arguments (well, three when counting
    Sender) - ThreadFunction and Param. You must, before the handler returns,
    pass both these arguments to a thread of your choice, This thread must then
    immediately execute ThreadFunction while passing given Param to it.

      While ThreadFunction is executing, do NOT pause or kill the thread - that
      would create number of errors and deadlocks. Manipulating thread priority
      or affinity is allowed - it should not affect the processing apart from
      possible performance effects.

      When the function ends (returns), you can do whatever you want with the
      thread within which it was executing (destroy it, pool it for further
      use, stare at it, ...), it will not be referenced or used by internal
      workings of this library in any capacity.

    How many times and when these events are called can be completely arbitrary,
    do not assume any such quantity!

      NOTE - the handlers are called in the context of thread that is managing
             the current instance (ie. thread that is calling method Update).    

    If neither of these events is assigned, then worker thread is spawned using
    internal means and its lifetime is also managed internally - this is ok for
    sporadic hashing, but if many hashes are calculated in rapid succession,
    then performance might suffer as each hash will spawn and then destroy its
    own set of worker threads.
  }
    property OnThreadStartCallback: TMD6ThreadStartCallback read fOnThreadStartCallback write fOnThreadStartCallback;
    property OnThreadStartEvent: TMD6ThreadStartEvent read fOnThreadStartEvent write fOnThreadStartEvent;
    property OnThreadStart: TMD6ThreadStartEvent read fOnThreadStartEvent write fOnThreadStartEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMD6DefHash                                  
--------------------------------------------------------------------------------
===============================================================================}
{
  This is a common ancestor for classes that provide predefined MD6 variants -
  that is variants with invariant hash length and other inputs set strictly to
  their default values.
}
{===============================================================================
    TMD6DefHash - class declaration
===============================================================================}
type
  TMD6DefHash = class(TMD6Hash)
  protected
    procedure SetMD6(Value: TMD6); override;
    procedure SetHashBits(Value: Integer); override;
    procedure SetKey(Value: TMD6Key); override;
    procedure SetRounds(Value: Integer); override;
    procedure SetModeControl(Value: Integer); override;
  public
    procedure SetupHashBits(HashBits: Integer); override;
    // it is enough to override the first overload of SetupKey as it is called by all others
    procedure SetupKey(const Key; Size: TMemSize); overload; override;
    // the input parameters are made read-only (only default values will be used)
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
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Note that there is, for the sake of simplicity, no processing function
  implemented for fixed-length hash types (eg. TMD6_224), only for variant-
  length type TMD6. If you want to pass a fixed type, or convert variant-length
  result to fixed type, use following conversion functions to do so.
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
type
  TMD6Settings = record
    HashBits:     Integer;
    Rounds:       Integer;
    ModeControl:  Integer;
    Key:          TMD6Key;
  end;

const
  DefaultMD6Settings: TMD6Settings = (
    HashBits:     DefaultMD6Bits;
    Rounds:       168;
    ModeControl:  64;
    Key:          nil);

// to get default value for any input, use constant DefaultMD6Settings and its fields
Function MD6Settings(HashBits,Rounds,ModeControl: Integer; Key: TMD6Key = nil): TMD6Settings; overload;
Function MD6Settings(HashBits,Rounds,ModeControl: Integer; const Key; KeySize: TMemSize): TMD6Settings; overload;
Function MD6Settings(HashBits,Rounds,ModeControl: Integer; const Key: String): TMD6Settings; overload;

// in all following overloads, Rounds is calculated from HashBits and Key
Function MD6Settings(HashBits,ModeControl: Integer; Key: TMD6Key = nil): TMD6Settings; overload;
Function MD6Settings(HashBits,ModeControl: Integer; const Key; KeySize: TMemSize): TMD6Settings; overload;
Function MD6Settings(HashBits,ModeControl: Integer; const Key: String): TMD6Settings; overload;

Function MD6Settings(HashBits: Integer; Key: TMD6Key = nil): TMD6Settings; overload;
Function MD6Settings(HashBits: Integer; const Key; KeySize: TMemSize): TMD6Settings; overload;
Function MD6Settings(HashBits: Integer; const Key: String): TMD6Settings; overload;

//------------------------------------------------------------------------------
{
  For MD6, it is not enough to pass hash from previous step when doing
  continuous hashing (BufferMD6 > LastBufferMD6). TDM6State type is introduced
  for this purpose.
}
type
  TMD6State = type Pointer;

Function InitialMD6(Settings: TMD6Settings): TMD6State; overload;
Function InitialMD6(HashBits: Integer = DefaultMD6Bits): TMD6State; overload;

procedure BufferMD6(State: TMD6State; const Buffer; Size: TMemSize); overload;
Function LastBufferMD6(var State: TMD6State; const Buffer; Size: TMemSize): TMD6;

//------------------------------------------------------------------------------

Function BufferMD6(const Buffer; Size: TMemSize; Settings: TMD6Settings): TMD6; overload;
Function BufferMD6(const Buffer; Size: TMemSize; HashBits: Integer = DefaultMD6Bits): TMD6; overload;

Function AnsiStringMD6(const Str: AnsiString; Settings: TMD6Settings): TMD6; overload;
Function AnsiStringMD6(const Str: AnsiString; HashBits: Integer = DefaultMD6Bits): TMD6; overload;
Function WideStringMD6(const Str: WideString; Settings: TMD6Settings): TMD6; overload;
Function WideStringMD6(const Str: WideString; HashBits: Integer = DefaultMD6Bits): TMD6; overload;
Function StringMD6(const Str: String; Settings: TMD6Settings): TMD6; overload;
Function StringMD6(const Str: String; HashBits: Integer = DefaultMD6Bits): TMD6; overload;

Function StreamMD6(Stream: TStream; Count: Int64; Settings: TMD6Settings): TMD6; overload;
Function StreamMD6(Stream: TStream; Count: Int64 = -1; HashBits: Integer = DefaultMD6Bits): TMD6; overload;
Function FileMD6(const FileName: String; Settings: TMD6Settings): TMD6; overload;
Function FileMD6(const FileName: String; HashBits: Integer = DefaultMD6Bits): TMD6; overload;

//------------------------------------------------------------------------------
type
  TMD6Context = type Pointer;

Function MD6_Init(Settings: TMD6Settings): TMD6Context; overload;
Function MD6_Init(HashBits: Integer = DefaultMD6Bits): TMD6Context; overload;
procedure MD6_Update(Context: TMD6Context; const Buffer; Size: TMemSize);
Function MD6_Final(var Context: TMD6Context; const Buffer; Size: TMemSize): TMD6; overload;
Function MD6_Final(var Context: TMD6Context): TMD6; overload;
Function MD6_Hash(const Buffer; Size: TMemSize; HashBits: Integer = DefaultMD6Bits): TMD6;

implementation

uses
  {$IFDEF Windows}Windows,{$ELSE}UnixType, BaseUnix,{$ENDIF} SyncObjs,
  {$IFDEF VectorCompression}SimpleCPUID,{$ENDIF}AuxMath, StrRect, BitOps,
  InterlockedOps;

{$IFNDEF Windows}
  {$LINKLIB C}
  {$LINKLIB PTHREAD}
{$ENDIF}

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    Imported (external/system) functions
===============================================================================}

{$IFDEF Windows}

procedure GetNativeSystemInfo(lpSystemInfo: PSystemInfo); stdcall; external kernel32;

Function SwitchToThread: BOOL; stdcall; external kernel32;

{$ELSE}

const
  _SC_NPROCESSORS_ONLN = 84;

Function sysconf(name: cInt): cLong; cdecl; external;

Function sched_yield: cint; cdecl; external;

Function errno_ptr: pcInt; cdecl; external name '__errno_location';

type
  psem_t = ^sem_t;

Function sem_init(sem: psem_t; pshared: cint; value: cunsigned): cint; cdecl; external;
Function sem_destroy(sem: psem_t): cint; cdecl; external;
Function sem_wait(sem: psem_t): cint; cdecl; external;
Function sem_post(sem: psem_t): cint; cdecl; external;

{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                TMD6ProcessorBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6ProcessorBase - implementation constants and types
===============================================================================}
const
  MD6_BITS_DEFAULT = DefaultMD6Bits{512};
  MD6_BITS_MAX     = 512;
  MD6_BYTES_MAX    = MD6_BITS_MAX div 8;

  MD6_MODE_DEFAULT = 64;

  MD6_KEY_MAXLEN  = 8;  // words
  MD6_KEY_MAXSIZE = 64; // in bytes

  MD6_CHUNK_LEN  = 16;  // words
  MD6_CHUNK_SIZE = 128; // bytes (16 words)
  MD6_CHUNK_BITS = 1024;

  MD6_BLOCK_CHUNKS = 4;
  MD6_BLOCK_SIZE   = 512; // bytes (64 words)

  MD6_BLOCKARRAY_MINLEN  = 89;  // words

  MD6_BLOCKARRAY_IDX_KEY  = 15;
  MD6_BLOCKARRAY_IDX_U    = 23; // position of unique node ID word
  MD6_BLOCKARRAY_IDX_V    = 24; // position of control word
  MD6_BLOCKARRAY_IDX_DATA = 25;

//------------------------------------------------------------------------------  
type
  TMD6Word = UInt64;

  TMD6BlockArray = array of TMD6Word;

  TMD6BlockChunks = array[0..Pred(MD6_BLOCK_CHUNKS)] of Pointer;

//------------------------------------------------------------------------------
const  
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
    MD6_S[i] := ROL(MD6_S[i - 1],1) xor (MD6_S[i - 1] and MD6_S_MASK)
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
    TMD6ProcessorBase - class declaration
===============================================================================}
type
  TMD6ProcessorBase = class(TCustomObject)
  protected
    // hash inputs
    fHashBits:      Integer;
    fKey:           TMD6Key;
    fProcessedKey:  array[0..Pred(MD6_KEY_MAXLEN)] of TMD6Word;
    fRounds:        Integer;
    fModeControl:   Integer;
    // hash outputs
    fMD6:           TMD6;
    // getters, setters
    procedure SetHashBits(Value: Integer); virtual;
    procedure SetKey(const Value: TMD6Key); virtual;
    Function GetMD6: TMD6; virtual;
    // object init/final
    procedure Initialize; overload; virtual;
    procedure Initialize(Source: TMD6ProcessorBase); overload; virtual;
    procedure Finalize; virtual;
    // hash processing
    Function BuildControlWord(PaddingBits: Integer; FinalBlock: Boolean): TMD6Word; virtual;
    Function BuildUniqueNodeIDWord(LevelNumber: Integer; BlockIndex: Int64): TMD6Word; virtual;
    procedure CalculateBlockChunks(BlockArray: TMD6BlockArray; out BlockChunks: TMD6BlockChunks); virtual;
    procedure InitializeBlockArray(out BlockArray: TMD6BlockArray; out BlockChunks: TMD6BlockChunks); virtual;
    procedure CompressBlockArray(var BlockArray: TMD6BlockArray); virtual;
  public
    constructor Create;
    constructor CreateAsCopy(Source: TMD6ProcessorBase);
    destructor Destroy; override;
    // main processing
    procedure ProcessInit; virtual; abstract;
    procedure ProcessUpdate(const Chunk{128-byte chunk}); virtual; abstract;
    procedure ProcessLast(ChunkPadBytes: TMemSize); virtual; abstract;
    procedure ProcessEnd; virtual; abstract;
    procedure ProcessTerminate; virtual; abstract;
    // properties
    property HashBits: Integer write SetHashBits;
    property Key: TMD6Key write SetKey;
    property Rounds: Integer write fRounds;
    property ModeControl: Integer write fModeControl;
    property MD6: TMD6 read GetMD6;
  end;

  TMD6ProcessorClass = class of TMD6ProcessorBase;

{===============================================================================
    TMD6ProcessorBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6ProcessorBase - protected methods
-------------------------------------------------------------------------------}

procedure TMD6ProcessorBase.SetHashBits(Value: Integer);
begin
fHashBits := Value;
// prepare resulting hash accordingly
SetLength(fMD6,fHashBits div 8);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorBase.SetKey(const Value: TMD6Key);
begin
fKey := Copy(Value);
// prepare processed key
FillChar(fProcessedKey,SizeOf(fProcessedKey),0);
If Length(fKey) > 0 then
  begin
    Move(fKey[0],fProcessedKey,Length(fKey));
  {$IFNDEF ENDIAN_BIG}
    EndianSwapItems64(fProcessedKey,Length(fProcessedKey));
  {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

Function TMD6ProcessorBase.GetMD6: TMD6;
begin
Result := Copy(fMD6);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorBase.Initialize;
begin
// nothing to do
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TMD6ProcessorBase.Initialize(Source: TMD6ProcessorBase);
begin
fHashBits := Source.fHashBits;
fKey := Copy(Source.fKey);
fProcessedKey := Source.fProcessedKey;  // static array, no need to do copy
fRounds := Source.fRounds;
fModeControl := Source.fModeControl;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorBase.Finalize;
begin
// nothing to do
end;

//------------------------------------------------------------------------------

Function TMD6ProcessorBase.BuildControlWord(PaddingBits: Integer; FinalBlock: Boolean): TMD6Word;
begin
Result := 0 or
  (TMD6Word(fRounds and $FFF) shl 48) or
  (TMD6Word(fModeControl and $FF) shl 40) or
  (TMD6Word(Integer(iIfThen(FinalBlock,1,0)) and $F) shl 36) or
  (TMD6Word(PaddingBits and $FFFF) shl 20) or
  (TMD6Word(Length(fKey) and $FF) shl 12) or
   TMD6Word(fHashBits and $FFF);
end;

//------------------------------------------------------------------------------

Function TMD6ProcessorBase.BuildUniqueNodeIDWord(LevelNumber: Integer; BlockIndex: Int64): TMD6Word;
begin
Result := (TMD6Word(LevelNumber and $FF) shl 56) or (BlockIndex and $00FFFFFFFFFFFFFF);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorBase.CalculateBlockChunks(BlockArray: TMD6BlockArray; out BlockChunks: TMD6BlockChunks);
var
  i:  Integer;
begin
For i := Low(BlockChunks) to High(BlockChunks) do
  BlockChunks[i] := Addr(BlockArray[MD6_BLOCKARRAY_IDX_DATA + (i * MD6_CHUNK_LEN)]);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorBase.InitializeBlockArray(out BlockArray: TMD6BlockArray; out BlockChunks: TMD6BlockChunks);
var
  i:  Integer;
begin
BlockArray := nil;
SetLength(BlockArray,MD6_BLOCKARRAY_MINLEN + (fRounds * MD6_CHUNK_LEN));
For i := Low(MD6_VEC_Q) to High(MD6_VEC_Q) do
  BlockArray[i] := MD6_VEC_Q[i];
// copy the prepared key (already byte-swapped if needed)
Move(fProcessedKey,BlockArray[MD6_BLOCKARRAY_IDX_KEY],MD6_KEY_MAXSIZE);
CalculateBlockChunks(BlockArray,BlockChunks);
end;

//------------------------------------------------------------------------------
{$IFDEF VectorCompression}
var
  VAR_SSE3Supported: Boolean = False;

procedure CompressionRoundSSE({EAX}RoundConst, {EDX}FirstWord: PInt64); register; assembler;
const
  Shifts: array[0..31] of Int64 = (
    10, 5, 13, 10, 11, 12, 2, 7, 14, 15, 7, 13, 11, 7, 6, 12, // right shifts
    11, 24, 9, 16, 15, 9, 27, 15, 6, 2, 29, 8, 15, 5, 31, 9); // left shifts
asm
{
  Compression is implemented in SSE only for 32bit code simply because it is
  not needed elsewhere.

  As the MD6 hash is based around 64bit words, 32bit processors cannot handle
  them directly and most things must be emulated in code - and that is slow.
  Therefore I have decided to implement processing in SSE, which can handle
  64bit integers, and as a bonus can process more than one at a time.
  I cannot say anything conclusive, but for me the SSE was more than 3x faster
  than best result produced from pascal code by any compiler.

  Code produced by the compiler (namely FPC) for 64bit CPUs seems to be well
  optimized and provides even better performance than this SSE implementation.

  AVX[2/512/10] could probably speed this even more thanks to 256 or 512bit
  registers, but my prehistoric computer does not offer those extensions, so
  I cannot develop for them.
}
    {$IFDEF FPC}
      MOVDDUP   XMM4, [EAX]
    {$ELSE}
      DB $F2, $0F, $12, $20
    {$ENDIF}

      LEA       EAX, [Shifts]
      MOV       ECX, 8

  @CycleStart:

      MOVDQA    XMM5, XMM4

      MOVDQU    XMM0, [EDX - 144]   // [i - 18]
      MOVDQU    XMM1, [EDX - 168]   // [i - 21]

      MOVDQU    XMM2, [EDX - 248]   // [i - 31]
      MOVDQU    XMM3, [EDX - 536]   // [i - 67]

      PAND      XMM0, XMM1
      PXOR      XMM5, XMM0

      MOVDQU    XMM0, [EDX - 712]   // [i - 89]
      MOVDQU    XMM1, [EDX - 136]   // [i - 17]

      PAND      XMM2, XMM3
      PXOR      XMM5, XMM2

      PXOR      XMM5, XMM0
      PXOR      XMM5, XMM1

      // right shift
      MOVQ      XMM0, [EAX]
      MOVQ      XMM1, [EAX + 8]
      MOVQ      XMM2, XMM5
      MOVHLPS   XMM3, XMM5
      PSRLQ     XMM2, XMM0
      PSRLQ     XMM3, XMM1
      MOVLHPS   XMM2, XMM3
      PXOR      XMM5, XMM2

      // left shift
      MOVQ      XMM0, [EAX + 128]
      MOVQ      XMM1, [EAX + 136]
      MOVQ      XMM2, XMM5
      MOVHLPS   XMM3, XMM5
      PSLLQ     XMM2, XMM0
      PSLLQ     XMM3, XMM1
      MOVLHPS   XMM2, XMM3
      PXOR      XMM5, XMM2
    {
      Is there any way of shifting each items in the vector register by
      different amount?
    }

      MOVDQU    [EDX], XMM5

      ADD       EDX, 16
      ADD       EAX, 16

      DEC       ECX
      JNZ       @CycleStart
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{$ENDIF}  

procedure TMD6ProcessorBase.CompressBlockArray(var BlockArray: TMD6BlockArray);
var
  i:          Integer;
  RoundConst: TMD6Word;
  x:          TMD6Word;
begin
// block is assumed to be completely prepared, only do endianness corrections
{$IFNDEF ENDIAN_BIG}
SwapEndianItems64(BlockArray[MD6_BLOCKARRAY_IDX_DATA],TMemSize(MD6_BLOCKARRAY_MINLEN - MD6_BLOCKARRAY_IDX_DATA));
{$ENDIF}
// main calculation...
RoundConst := MD6_S_0;
i := MD6_BLOCKARRAY_MINLEN;
// number of rounds is encoded in the length of Block
while i <= (Length(BlockArray) - MD6_CHUNK_LEN) do
  begin
    // unrolled round (16 steps)...
  {$IFDEF VectorCompression}
    If VAR_SSE3Supported then
      CompressionRoundSSE(@RoundConst,Addr(BlockArray[i]))
    else
  {$ENDIF}
      begin
        // step 0
        x := RoundConst xor BlockArray[i - 89{MD6_BLOCK_LEN - step}] xor BlockArray[i - 17{MD6_TAP[0] - step}];
        x := x xor (BlockArray[i - 18{MD6_TAP[1] - step}] and BlockArray[i - 21{MD6_TAP[2] - step}]);
        x := x xor (BlockArray[i - 31{MD6_TAP[3] - step}] and BlockArray[i - 67{MD6_TAP[4] - step}]);
        x := x xor (x shr 10{MD6_SHIFT_R[step]});
        BlockArray[i] := x xor (x shl 11{MD6_SHIFT_L[step]});

        // step 1
        x := RoundConst xor BlockArray[i - 88] xor BlockArray[i - 16];
        x := x xor (BlockArray[i - 17] and BlockArray[i - 20]);
        x := x xor (BlockArray[i - 30] and BlockArray[i - 66]);
        x := x xor (x shr 5);
        BlockArray[i + 1] := x xor (x shl 24);

        // step 2
        x := RoundConst xor BlockArray[i - 87] xor BlockArray[i - 15];
        x := x xor (BlockArray[i - 16] and BlockArray[i - 19]);
        x := x xor (BlockArray[i - 29] and BlockArray[i - 65]);
        x := x xor (x shr 13);
        BlockArray[i + 2] := x xor (x shl 9);

        // step 3
        x := RoundConst xor BlockArray[i - 86] xor BlockArray[i - 14];
        x := x xor (BlockArray[i - 15] and BlockArray[i - 18]);
        x := x xor (BlockArray[i - 28] and BlockArray[i - 64]);
        x := x xor (x shr 10);
        BlockArray[i + 3] := x xor (x shl 16);

        // step 4
        x := RoundConst xor BlockArray[i - 85] xor BlockArray[i - 13];
        x := x xor (BlockArray[i - 14] and BlockArray[i - 17]);
        x := x xor (BlockArray[i - 27] and BlockArray[i - 63]);
        x := x xor (x shr 11);
        BlockArray[i + 4] := x xor (x shl 15);

        // step 5
        x := RoundConst xor BlockArray[i - 84] xor BlockArray[i - 12];
        x := x xor (BlockArray[i - 13] and BlockArray[i - 16]);
        x := x xor (BlockArray[i - 26] and BlockArray[i - 62]);
        x := x xor (x shr 12);
        BlockArray[i + 5] := x xor (x shl 9);

        // step 6
        x := RoundConst xor BlockArray[i - 83] xor BlockArray[i - 11];
        x := x xor (BlockArray[i - 12] and BlockArray[i - 15]);
        x := x xor (BlockArray[i - 25] and BlockArray[i - 61]);
        x := x xor (x shr 2);
        BlockArray[i + 6] := x xor (x shl 27);

        // step 7
        x := RoundConst xor BlockArray[i - 82] xor BlockArray[i - 10];
        x := x xor (BlockArray[i - 11] and BlockArray[i - 14]);
        x := x xor (BlockArray[i - 24] and BlockArray[i - 60]);
        x := x xor (x shr 7);
        BlockArray[i + 7] := x xor (x shl 15);

        // step 8
        x := RoundConst xor BlockArray[i - 81] xor BlockArray[i - 9];
        x := x xor (BlockArray[i - 10] and BlockArray[i - 13]);
        x := x xor (BlockArray[i - 23] and BlockArray[i - 59]);
        x := x xor (x shr 14);
        BlockArray[i + 8] := x xor (x shl 6);

        // step 9
        x := RoundConst xor BlockArray[i - 80] xor BlockArray[i - 8];
        x := x xor (BlockArray[i - 9] and BlockArray[i - 12]);
        x := x xor (BlockArray[i - 22] and BlockArray[i - 58]);
        x := x xor (x shr 15);
        BlockArray[i + 9] := x xor (x shl 2);

        // step 10
        x := RoundConst xor BlockArray[i - 79] xor BlockArray[i - 7];
        x := x xor (BlockArray[i - 8] and BlockArray[i - 11]);
        x := x xor (BlockArray[i - 21] and BlockArray[i - 57]);
        x := x xor (x shr 7);
        BlockArray[i + 10] := x xor (x shl 29);

        // step 11
        x := RoundConst xor BlockArray[i - 78] xor BlockArray[i - 6];
        x := x xor (BlockArray[i - 7] and BlockArray[i - 10]);
        x := x xor (BlockArray[i - 20] and BlockArray[i - 56]);
        x := x xor (x shr 13);
        BlockArray[i + 11] := x xor (x shl 8);

        // step 12
        x := RoundConst xor BlockArray[i - 77] xor BlockArray[i - 5];
        x := x xor (BlockArray[i - 6] and BlockArray[i - 9]);
        x := x xor (BlockArray[i - 19] and BlockArray[i - 55]);
        x := x xor (x shr 11);
        BlockArray[i + 12] := x xor (x shl 15);

        // step 13
        x := RoundConst xor BlockArray[i - 76] xor BlockArray[i - 4];
        x := x xor (BlockArray[i - 5] and BlockArray[i - 8]);
        x := x xor (BlockArray[i - 18] and BlockArray[i - 54]);
        x := x xor (x shr 7);
        BlockArray[i + 13] := x xor (x shl 5);

        // step 14
        x := RoundConst xor BlockArray[i - 75] xor BlockArray[i - 3];
        x := x xor (BlockArray[i - 4] and BlockArray[i - 7]);
        x := x xor (BlockArray[i - 17] and BlockArray[i - 53]);
        x := x xor (x shr 6);
        BlockArray[i + 14] := x xor (x shl 31);

        // step 15
        x := RoundConst xor BlockArray[i - 74] xor BlockArray[i - 2];
        x := x xor (BlockArray[i - 3] and BlockArray[i - 6]);
        x := x xor (BlockArray[i - 16] and BlockArray[i - 52]);
        x := x xor (x shr 12);
        BlockArray[i + 15] := x xor (x shl 9);
      end;

    // recalculate round constant
    RoundConst := ROL(RoundConst,1) xor (RoundConst and MD6_S_MASK);
    // increment index by number of steps taken
    Inc(i,16);
  end;
// endianness corection for chaining variable (last 1024 bits, 16 words, one chunk)
{$IFNDEF ENDIAN_BIG}
SwapEndianItems64(BlockArray[Length(BlockArray) - MD6_CHUNK_LEN],MD6_CHUNK_LEN);
{$ENDIF}
end;

{-------------------------------------------------------------------------------
    TMD6ProcessorBase - public methods
-------------------------------------------------------------------------------}

constructor TMD6ProcessorBase.Create;
begin
inherited;
Initialize;
end;

//------------------------------------------------------------------------------

constructor TMD6ProcessorBase.CreateAsCopy(Source: TMD6ProcessorBase);
begin
inherited Create;
Initialize(Source);
end;

//------------------------------------------------------------------------------

destructor TMD6ProcessorBase.Destroy;
begin
Finalize;
inherited;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TMD6ProcessorSingle
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6ProcessorSingle - class declaration
===============================================================================}
type
  TMD6ProcessorSingle = class(TMD6ProcessorBase)
  protected
    fState: record
      Levels: array of record
        BlockArray:   TMD6BlockArray;
        BlockChunks:  TMD6BlockChunks;
        BlockIndex:   Int64;    // block index within current level
        Deposited:    Integer;  // number of chunks already copied into block array
      end;
    end;
    procedure Initialize; override;
    procedure Initialize(Source: TMD6ProcessorBase); override;
    procedure Finalize; override;
    // processing
    procedure AddStateLevel; virtual;
    procedure ProcessChunkInLevel(Level: Integer; const Chunk); virtual;
    procedure ProcessLastInLevel(Level: Integer; ChunkPadBytes: TMemSize = 0); virtual;
  public
    procedure ProcessInit; override;
    procedure ProcessUpdate(const Chunk); override;
    procedure ProcessLast(ChunkPadBytes: TMemSize); override;
    procedure ProcessEnd; override;
    procedure ProcessTerminate; override;
  end;

{===============================================================================
    TMD6ProcessorSingle - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6ProcessorSingle - protected methods
-------------------------------------------------------------------------------}

procedure TMD6ProcessorSingle.Initialize;
begin
inherited;
fState.Levels := nil;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TMD6ProcessorSingle.Initialize(Source: TMD6ProcessorBase);
var
  i:  Integer;
begin
inherited;
If Source is TMD6ProcessorSingle then
  begin
    fState.Levels := Copy(TMD6ProcessorSingle(Source).fState.Levels);
    // ensure unique copies
    For i := Low(fState.Levels) to High(fState.Levels) do
      begin
        SetLength(fState.Levels[i].BlockArray,Length(fState.Levels[i].BlockArray));
        CalculateBlockChunks(fState.Levels[i].BlockArray,fState.Levels[i].BlockChunks);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorSingle.Finalize;
begin
fState.Levels := nil;
inherited;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorSingle.AddStateLevel;
begin
SetLength(fState.Levels,Length(fState.Levels) + 1);
with fState.Levels[High(fState.Levels)] do
  begin
    InitializeBlockArray(BlockArray,BlockChunks);
    BlockIndex := 0;
    Deposited := 0;
    // init vector for sequential processing
    If Length(fState.Levels) > fModeControl then
      // empty chunk, InitializeBlock already intialized it to all zero
      Deposited := 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorSingle.ProcessChunkInLevel(Level: Integer; const Chunk);
begin
If Level <= High(fState.Levels) then  // sanity check
  begin
  {
    Cannot use "with fState.Levels[Level] do" - the array can be reallocated
    within the function (by AddStateLevel), which invalidates the used pointer
    in subsequent code.
  }
    If fState.Levels[Level].Deposited >= MD6_BLOCK_CHUNKS then
      begin
      {
        Block at selected level is full, compress it and pass chaining variable
        up the tree for further processing.
      }
        with fState.Levels[Level] do
          begin
            BlockArray[MD6_BLOCKARRAY_IDX_U] := BuildUniqueNodeIDWord(Succ(Level),BlockIndex);
            BlockArray[MD6_BLOCKARRAY_IDX_V] := BuildControlWord(0,False);
            CompressBlockArray(BlockArray);
            Inc(BlockIndex);
            Deposited := 0;
          end;
      {
        The succ() is here because we start the level indices at 0, mode
        control expects them to start at 1.
      }
        If Succ(Level) <= fModeControl then
          begin
            // not at the maximum tree hight
            If Level >= High(fState.Levels) then
              AddStateLevel;  // there is no next level as of yet, add it
            // put chaining variable into the next level block
            with fState.Levels[Level] do
              ProcessChunkInLevel(Succ(Level),BlockArray[Length(BlockArray) - MD6_CHUNK_LEN]);
          end
        else with fState.Levels[Level] do
          begin
          {
            We are at the maximum tree hight, meaning current block is
            sequential, copy chaining variable back to it because it will be
            reused as the next block in sequence at current level.
          }
            Move(BlockArray[Length(BlockArray) - MD6_CHUNK_LEN],BlockArray[MD6_BLOCKARRAY_IDX_DATA],MD6_CHUNK_SIZE);
            Deposited := 1;
           end;          
      end;
    // copy provided data to the block
    with fState.Levels[Level] do
      begin
        Move(Chunk,BlockChunks[Deposited]^,MD6_CHUNK_SIZE);
        Inc(Deposited);
      end;
  end
else raise EMD6ProcessingError.CreateFmt('TMD6ProcessorSingle.ProcessChunkInLevel: Cannot process non-existing level (%d).',[Level]);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorSingle.ProcessLastInLevel(Level: Integer; ChunkPadBytes: TMemSize = 0);
var
  Undeposited:  Integer;
begin
If Level <= High(fState.Levels) then
  begin
    with fState.Levels[Level] do
      begin
        If Deposited < MD6_BLOCK_CHUNKS then
          begin
            // block is not full, fill the rest with zeroes
            Undeposited := MD6_BLOCK_CHUNKS - Deposited;
            FillChar(BlockArray[MD6_BLOCKARRAY_IDX_DATA + (Deposited * MD6_CHUNK_LEN)],Undeposited * MD6_CHUNK_SIZE,0);
            Inc(ChunkPadBytes,TMemSize(Undeposited * MD6_CHUNK_SIZE));
          end;
        BlockArray[MD6_BLOCKARRAY_IDX_U] := BuildUniqueNodeIDWord(Succ(Level),BlockIndex);
        BlockArray[MD6_BLOCKARRAY_IDX_V] := BuildControlWord(ChunkPadBytes * 8,Level >= High(fState.Levels));
        CompressBlockArray(BlockArray);
      end;
    If Level < High(fState.Levels) then
      begin
        // we are not at the top-most node, pass result from current block to the next and process it
        with fState.Levels[Level] do
          ProcessChunkInLevel(Succ(Level),BlockArray[Length(BlockArray) - MD6_CHUNK_LEN]);
        ProcessLastInLevel(Succ(Level));
      end
    else If Length(fMD6) > 0 then
      // top-most node, copy final result
      begin
      with fState.Levels[Level] do
        Move(PtrAdvance(Addr(BlockArray[High(BlockArray)]),PtrInt(SizeOf(TMD6Word) - Length(fMD6)))^,fMD6[0],Length(fMD6));
      end;
  end
else raise EMD6ProcessingError.CreateFmt('TMD6ProcessorSingle.ProcessLastInLevel: Cannot process non-existing level (%d).',[Level]);
end;

{-------------------------------------------------------------------------------
    TMD6ProcessorSingle - public methods
-------------------------------------------------------------------------------}

procedure TMD6ProcessorSingle.ProcessInit;
begin
fState.Levels := nil;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorSingle.ProcessUpdate(const Chunk);
begin
If Length(fState.Levels) <= 0 then
  AddStateLevel;
ProcessChunkInLevel(0,Chunk);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorSingle.ProcessLast(ChunkPadBytes: TMemSize);
begin
If Length(fState.Levels) <= 0 then
  AddStateLevel;
ProcessLastInLevel(0,ChunkPadBytes);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorSingle.ProcessEnd;
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorSingle.ProcessTerminate;
begin
// nothing to do here
end;

{===============================================================================
--------------------------------------------------------------------------------
                             TMD6ProcessorMultiBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6ProcessorMultiBase - spinlock operations
===============================================================================}
type
  TMD6Spinlock = UInt32;

const
  MD6_SPINLOCK_UNLOCKED = 0;
  MD6_SPINLOCK_LOCKED   = 1;

//------------------------------------------------------------------------------

{$IFNDEF PurePascal}
procedure SpinlockDelay; assembler;
asm
{
  PAUSE instruction was introduced in Pentium 4, but Intel's documentation
  explicitly states that it is backwards compatible with all earlier IA-32
  processors where it it treated as NOP.
}
    PAUSE
end;
 {$ELSE}
procedure SpinlockDelay;
begin
// just do nothing, hope compiler will not optimize-out this function
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure SpinlockInit(out Spinlock: TMD6Spinlock);
begin
Spinlock := 0;
InterlockedStore(Spinlock,MD6_SPINLOCK_UNLOCKED);
end;

//------------------------------------------------------------------------------

procedure SpinlockFinal(var Spinlock: TMD6Spinlock);
begin
InterlockedStore(Spinlock,MD6_SPINLOCK_UNLOCKED);
end;

//------------------------------------------------------------------------------

procedure SpinlockAcquire(var Spinlock: TMD6Spinlock);
var
  Val:  UInt32;
  Cnt:  Integer;
begin
Cnt := 0;
If InterlockedExchange(Spinlock,MD6_SPINLOCK_LOCKED) <> MD6_SPINLOCK_UNLOCKED then
  repeat
    repeat
      Cnt := (Cnt + 1) mod 5;
    {
      If we have failed to acquire lock five times in a row, it may be time
      to try to yield execution to other threads...
    }
      If Cnt <= 0 then
      {$IFDEF Windows}
        SwitchToThread;
      {$ELSE}
        sched_yield;
      {$ENDIF}
      SpinlockDelay;        
      Val := SpinLock;
    until Val = MD6_SPINLOCK_UNLOCKED;
  until InterlockedCompareExchange(SpinLock,MD6_SPINLOCK_LOCKED,Val) = MD6_SPINLOCK_UNLOCKED;
end;

//------------------------------------------------------------------------------

procedure SpinlockRelease(var Spinlock: TMD6Spinlock);
begin
InterlockedStore(Spinlock,MD6_SPINLOCK_UNLOCKED);
end;

{===============================================================================
    TMD6ProcessorMultiBase - processed blocks provider (PBP)
===============================================================================}
type
  TMD6MTProcessedBlock = record
    BlockArray:     TMD6BlockArray;
    BlockChunks:    TMD6BlockChunks;
    BlockIndex:     Int64;
    BlockAuxiliary: Integer;
  end;
  PMD6MTProcessedBlock = ^TMD6MTProcessedBlock;

const
  MD6_MP_BLOCKAUX_MASK_DEPMAP = $F; // deposit map (bits 0..3)

  MD6_MP_BLOCKAUX_SHIFT_PAD = 4;    // padding chunks shift
  MD6_MP_BLOCKAUX_MASK_PAD = 3;     // padding chunks bits, shifted down

  MD6_MP_BLOCKAUX_FLAG_LAST = $100; // last (right-most) block in its level

//------------------------------------------------------------------------------
type
  TMD6MTBlockArrayInitFunc = procedure(out BlockArray: TMD6BlockArray; out BlockChunks: TMD6BlockChunks) of object;

const
  MD6_MT_PBP_GROWDELTA = 16;
  MD6_MT_PBP_PREALLOC  = 4; // must be larger than zero

{-------------------------------------------------------------------------------
    TMD6ProcessorMultiBase - PBP class declaration
-------------------------------------------------------------------------------}
type
  TMD6MTProcessedBlockProvider = class(TCustomObject)
  protected
    fBlockArrayInitFunc:  TMD6MTBlockArrayInitFunc;
    fLock:                TMD6Spinlock;
    fUnusedBlocks:        array of PMD6MTProcessedBlock;
    fUnusedBlockCount:    Integer;
    fAcquiredBlocks:      array of PMD6MTProcessedBlock;
    fAcquiredBlockCount:  Integer;
    procedure Initialize(BlockArrayInitFunc: TMD6MTBlockArrayInitFunc); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(BlockArrayInitFunc: TMD6MTBlockArrayInitFunc);
    destructor Destroy; override;
    Function AcquireBlock(BlockIndex: Int64): PMD6MTProcessedBlock; virtual;
    procedure ReleaseBlock(ProcessedBlock: PMD6MTProcessedBlock); virtual;
  end;

{-------------------------------------------------------------------------------
    TMD6ProcessorMulti - PBP class implementation - protected methods
-------------------------------------------------------------------------------}

procedure TMD6MTProcessedBlockProvider.Initialize(BlockArrayInitFunc: TMD6MTBlockArrayInitFunc);
var
  i:  Integer;
begin
fBlockArrayInitFunc := BlockArrayInitFunc;
SpinlockInit(fLock);
fUnusedBlocks := nil;
SetLength(fUnusedBlocks,MD6_MT_PBP_GROWDELTA * uDivCeil(TMemSize(MD6_MT_PBP_PREALLOC),MD6_MT_PBP_GROWDELTA));
fUnusedBlockCount := MD6_MT_PBP_PREALLOC;
For i := Low(fUnusedBlocks) to Pred(fUnusedBlockCount) do
  begin
    New(fUnusedBlocks[i]);
    FillChar(fUnusedBlocks[i]^,SizeOf(TMD6MTProcessedBlock),0);
    fBlockArrayInitFunc(fUnusedBlocks[i]^.BlockArray,fUnusedBlocks[i]^.BlockChunks);
  end;
fAcquiredBlocks := nil;
fAcquiredBlockCount := 0;
end;

//------------------------------------------------------------------------------

procedure TMD6MTProcessedBlockProvider.Finalize;
var
  i:  Integer;
begin
For i := Low(fUnusedBlocks) to Pred(fUnusedBlockCount) do
  Dispose(fUnusedBlocks[i]); // dispose will deallocate the dynamic array(s)
fUnusedBlocks := nil;
For i := Low(fAcquiredBlocks) to Pred(fAcquiredBlockCount) do
  Dispose(fAcquiredBlocks[i]);
fAcquiredBlocks := nil;
SpinlockFinal(fLock);
end;

{-------------------------------------------------------------------------------
    TMD6ProcessorMulti - PBP class implementation - public methods
-------------------------------------------------------------------------------}

constructor TMD6MTProcessedBlockProvider.Create(BlockArrayInitFunc: TMD6MTBlockArrayInitFunc);
begin
inherited Create;
Initialize(BlockArrayInitFunc);
end;

//------------------------------------------------------------------------------

destructor TMD6MTProcessedBlockProvider.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TMD6MTProcessedBlockProvider.AcquireBlock(BlockIndex: Int64): PMD6MTProcessedBlock;

  Function AcquireBlockInternal(out ProcessedBlock: PMD6MTProcessedBlock): Boolean;
  var
    i:  Integer;
  begin
    SpinlockAcquire(fLock);
    try
      // We are in a spinlock, so make it quick. Do we have the block?...
      For i := Low(fAcquiredBlocks) to Pred(fAcquiredBlockCount) do
        If fAcquiredBlocks[i]^.BlockIndex = BlockIndex then
          begin
            ProcessedBlock := fAcquiredBlocks[i];
            Result := fUnusedBlockCount > 0;
            Exit;
          end;
      // ...no, do we have any unused block?...
      If fUnusedBlockCount > 0 then
        begin
          // move the unused block to acquired blocks array
          ProcessedBlock := fUnusedBlocks[Pred(fUnusedBlockCount)];
          fUnusedBlocks[Pred(fUnusedBlockCount)] := nil;
          Dec(fUnusedBlockCount);
          // grow acquired blocks array if necessary
          If Length(fAcquiredBlocks) <= fAcquiredBlockCount then
            SetLength(fAcquiredBlocks,Length(fAcquiredBlocks) + MD6_MT_PBP_GROWDELTA);
          fAcquiredBlocks[fAcquiredBlockCount] := ProcessedBlock;
          Inc(fAcquiredBlockCount);
          // init the obtained block (do not touch field Block)
          ProcessedBlock^.BlockIndex := BlockIndex;
          ProcessedBlock^.BlockAuxiliary := 0;
          Result := fUnusedBlockCount > 0;
        end
      else
        begin
          // ...we have no unused block, so we must allocate one here and now
          New(ProcessedBlock);
          FillChar(ProcessedBlock^,SizeOf(TMD6MTProcessedBlock),0);
          // add the new block to acquired
          If Length(fAcquiredBlocks) <= fAcquiredBlockCount then
            SetLength(fAcquiredBlocks,Length(fAcquiredBlocks) + MD6_MT_PBP_GROWDELTA);
          fAcquiredBlocks[fAcquiredBlockCount] := ProcessedBlock;
          Inc(fAcquiredBlockCount);
          fBlockArrayInitFunc(ProcessedBlock^.BlockArray,ProcessedBlock^.BlockChunks);
          ProcessedBlock^.BlockIndex := BlockIndex;
          ProcessedBlock^.BlockAuxiliary := 0;
          Result := False;
        end;
    finally
      SpinlockRelease(fLock);
    end;
  end;

var
  NewBlocks:  array[0..Pred(MD6_MT_PBP_PREALLOC)] of PMD6MTProcessedBlock;
  i:          Integer;
begin
If not AcquireBlockInternal(Result) then
  begin
    // we have no unused blocks, so make some (can be done without locking)
    For i := Low(NewBlocks) to High(NewBlocks) do
      begin
        New(NewBlocks[i]);
        FillChar(NewBlocks[i]^,SizeOf(TMD6MTProcessedBlock),0);
        fBlockArrayInitFunc(NewBlocks[i]^.BlockArray,NewBlocks[i]^.BlockChunks);
      end;
    // now add those blocks inside a lock
    SpinlockAcquire(fLock);
    try
      while Length(fUnusedBlocks) <= (fUnusedBlockCount + Length(NewBlocks)) do
        SetLength(fUnusedBlocks,Length(fUnusedBlocks) + MD6_MT_PBP_GROWDELTA);
      For i := Low(NewBlocks) to High(NewBlocks) do
        fUnusedBlocks[fUnusedBlockCount + i] := NewBlocks[i];
      Inc(fUnusedBlockCount,Length(NewBlocks));
    finally
      SpinlockRelease(fLock);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6MTProcessedBlockProvider.ReleaseBlock(ProcessedBlock: PMD6MTProcessedBlock);
var
  i,j:  Integer;
begin
SpinlockAcquire(fLock);
try
  For i := Low(fAcquiredBlocks) to Pred(fAcquiredBlockCount) do
    If fAcquiredBlocks[i] = ProcessedBlock then
      begin                      
        // move it from acquired blocks to unused blocks
        For j := i to (fAcquiredBlockCount - 2) do
          fAcquiredBlocks[j] := fAcquiredBlocks[j + 1];
        fAcquiredBlocks[Pred(fAcquiredBlockCount)] := nil;
        Dec(fAcquiredBlockCount);        
        If Length(fUnusedBlocks) <= fUnusedBlockCount then
          SetLength(fUnusedBlocks,Length(fUnusedBlocks) + MD6_MT_PBP_GROWDELTA);
        fUnusedBlocks[fUnusedBlockCount] := ProcessedBlock;
        Inc(fUnusedBlockCount);
        Exit;
      end;
  // if here then the passed block was not found
  raise EMD6ProcessingError.Create('TMD6MTProcessedBlockProvider.ReleaseBlock: Block not found.');
finally
  SpinlockRelease(fLock);
end;
end;

{===============================================================================
    TMD6ProcessorMultiBase - implementation types and constants
===============================================================================}
const
  MD6_MT_STATUS_RUNNING     = 0;
  MD6_MT_STATUS_ENDING      = MD6_MT_STATUS_RUNNING + 1;
  MD6_MT_STATUS_TERMINATING = MD6_MT_STATUS_ENDING + 1;

{===============================================================================
    TMD6ProcessorMultiBase - class declaration
===============================================================================}
type
  TMD6ProcessorMultiBase = class(TMD6ProcessorBase)
  protected
    fMaxThreads:      Integer;
    fOnThreadStart:   TNotifyEvent;
    fProcessing:      record
      Status:           Integer;  // see constants MD6_MT_STATUS_*
      ThreadsSpawned:   Integer;  // number of spawned threads
    {
      EndingCounter

      Accessed only using interlocked functions. Incremented when a thread is
      spawned, decremented by ending thread - thread that decrements it to zero
      (or lower) sets the EndingEvent (to signaled).
    }
      EndingCounter:    Integer;
      EndingEvent:      TEvent;
      ExceptionObject:  Pointer;  // used to pass exception from worker thread to main thread
    end;
    fState:           record
      Levels:           array of TMD6MTProcessedBlockProvider;
    end;
    // methods called by worker threads
    procedure ThreadReleaseBlock(Level: Integer; Block: PMD6MTProcessedBlock); virtual;
    procedure ThreadProcessSequence(PrevBlock: PMD6MTProcessedBlock); virtual;
    procedure ThreadProcessLevelHir(Level: Integer; ChildBlock: PMD6MTProcessedBlock; LastBlock: Boolean); virtual;
    procedure ThreadProcessLevelSeq(Level: Integer; ChildBlock: PMD6MTProcessedBlock; LastBlock: Boolean); virtual;
    procedure ThreadProcessLevel(Level: Integer; ChildBlock: PMD6MTProcessedBlock; LastBlock: Boolean); virtual;
    procedure ThreadExecute; virtual; abstract;
    // other methods
    procedure Initialize; override;
    procedure Initialize(Source: TMD6ProcessorBase); override;
    procedure Finalize; override;
    procedure DoThreadStart; virtual;
    procedure WaitForThreads; virtual;
    procedure ProcessEnding; virtual;
  public
    procedure ProcessInit; override;
    procedure ProcessEnd; override;
    procedure ProcessTerminate; override;
    property MaxThreads: Integer write fMaxThreads;
    property OnThreadStart: TNotifyEvent read fOnThreadStart write fOnThreadStart;
  end;

{===============================================================================
    TMD6ProcessorMultiBase - helper routines
===============================================================================}

Function BlockAuxEncodePad(PaddingChunks: Integer): Integer;
begin
Result := Byte(PaddingChunks and MD6_MP_BLOCKAUX_MASK_PAD) shl MD6_MP_BLOCKAUX_SHIFT_PAD;
end;

//------------------------------------------------------------------------------

Function BlockAuxDecodePad(Mask: Integer): Integer;
begin
Result := Integer(Mask shr MD6_MP_BLOCKAUX_SHIFT_PAD) and MD6_MP_BLOCKAUX_MASK_PAD;
end;

//------------------------------------------------------------------------------

Function BlockAuxEncodeDep(ChunkIndex: Integer; AddPadding: Boolean): Integer;
begin
Result := (iIfThen(AddPadding,Integer(-1),Integer(1)) shl ChunkIndex) and MD6_MP_BLOCKAUX_MASK_DEPMAP
end;

{===============================================================================
    TMD6ProcessorMultiBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6ProcessorMultiBase - protected methods
-------------------------------------------------------------------------------}

procedure TMD6ProcessorMultiBase.ThreadReleaseBlock(Level: Integer; Block: PMD6MTProcessedBlock);
begin
If (Level >= Low(fState.Levels)) and (Level <= High(fState.Levels)) then
  fState.Levels[Level].ReleaseBlock(Block)
else
  raise EMD6ProcessingError.CreateFmt('TMD6ProcessorMultiBase.ThreadReleaseBlock: Invalid level (%d).',[Level]);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.ThreadProcessSequence(PrevBlock: PMD6MTProcessedBlock);
var
  Level:            Integer;
  BlockIndex:       Int64;
  CurrentBlock:     PMD6MTProcessedBlock;
  NewBlockAuxMask:  Integer;
  FinalBlock:       Boolean;
begin
{
  This mehtod is called when passing chaining variable within one level "from
  left to right" - ie. between blocks on the same level during sequential
  processing. This can happen only within the top level.
}
Level := High(fState.Levels);
BlockIndex := Succ(PrevBlock^.BlockIndex);
CurrentBlock := fState.Levels[Level].AcquireBlock(BlockIndex);
// move chaining variable to the first chunk
Move(PrevBlock^.BlockArray[Length(PrevBlock^.BlockArray) - MD6_CHUNK_LEN],
     CurrentBlock^.BlockChunks[0]^,MD6_CHUNK_SIZE);
ThreadReleaseBlock(Level,PrevBlock);
NewBlockAuxMask := InterlockedOr(CurrentBlock^.BlockAuxiliary,BlockAuxEncodeDep(0,False));
If (NewBlockAuxMask and MD6_MP_BLOCKAUX_MASK_DEPMAP) = MD6_MP_BLOCKAUX_MASK_DEPMAP then
  begin
  {
    We are at the top-most (last) level, so if this block is last within it, it
    must also be final.
  }
    FinalBlock := (NewBlockAuxMask and MD6_MP_BLOCKAUX_FLAG_LAST) <> 0;
    CurrentBlock^.BlockArray[MD6_BLOCKARRAY_IDX_U] := BuildUniqueNodeIDWord(Level + 2,BlockIndex);
    CurrentBlock^.BlockArray[MD6_BLOCKARRAY_IDX_V] := BuildControlWord(BlockAuxDecodePad(NewBlockAuxMask) * MD6_CHUNK_BITS,FinalBlock);
    CompressBlockArray(CurrentBlock^.BlockArray);
    If FinalBlock then
      begin
        with CurrentBlock^ do
          Move(PtrAdvance(Addr(BlockArray[High(BlockArray)]),PtrInt(SizeOf(TMD6Word) - Length(fMD6)))^,fMD6[0],Length(fMD6));
        fState.Levels[Level].ReleaseBlock(CurrentBlock);
      end
    // pass chaining variable to the block "right" of this one  
    else ThreadProcessSequence(CurrentBlock);
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.ThreadProcessLevelHir(Level: Integer; ChildBlock: PMD6MTProcessedBlock; LastBlock: Boolean);
var
  BlockIndex:       Int64;
  ChunkIndex:       Int64;
  CurrentBlock:     PMD6MTProcessedBlock;
  BlockAuxMask:     Integer;
  NewBlockAuxMask:  Integer;
  FinalBlock:       Boolean;
begin
iDivModPow2NC(ChildBlock^.BlockIndex,4,BlockIndex,ChunkIndex);
CurrentBlock := fState.Levels[Level].AcquireBlock(BlockIndex);
// copy chaining variable from child to proper chunk of current block
Move(ChildBlock^.BlockArray[Length(ChildBlock^.BlockArray) - MD6_CHUNK_LEN],
     CurrentBlock^.BlockChunks[ChunkIndex]^,MD6_CHUNK_SIZE);
// we are done with the child block, release it early
ThreadReleaseBlock(Pred(Level),ChildBlock);
// is this last block
If LastBlock then
  begin
    // processing last block, zero any missing chunks beyond the current one
    If ChunkIndex < 3 then
      FillChar(CurrentBlock^.BlockChunks[Succ(ChunkIndex)]^,(3 - ChunkIndex) * MD6_CHUNK_SIZE,0);
    BlockAuxMask := BlockAuxEncodeDep(Integer(ChunkIndex),True) or
      BlockAuxEncodePad(3 - ChunkIndex) or MD6_MP_BLOCKAUX_FLAG_LAST;
  end
// not a last block
else BlockAuxMask := BlockAuxEncodeDep(Integer(ChunkIndex),False);
// main synchro
NewBlockAuxMask := InterlockedOr(CurrentBlock^.BlockAuxiliary,BlockAuxMask);
// is the block full?
If (NewBlockAuxMask and MD6_MP_BLOCKAUX_MASK_DEPMAP) = MD6_MP_BLOCKAUX_MASK_DEPMAP then
  begin
  {
    Block is full (all chunks are deposited or padded).

    Note we need to reconstruct number of padding bytes/bits in case we are
    last to deposit but are not depositing the last (right-most) chunk.

    The same goes for discerning whether this is a final block - we look
    whether the block is marked as such in aux word and if so and if it is
    also the first block (index 0), then it must be final block.

    Level is +2 because level 1 was buffer block.

    Then compress the block and continue up the levels if not at final block.
  }
    FinalBlock := ((NewBlockAuxMask and MD6_MP_BLOCKAUX_FLAG_LAST) <> 0) and (BlockIndex <= 0);
    CurrentBlock^.BlockArray[MD6_BLOCKARRAY_IDX_U] := BuildUniqueNodeIDWord(Level + 2,BlockIndex);
    CurrentBlock^.BlockArray[MD6_BLOCKARRAY_IDX_V] := BuildControlWord(BlockAuxDecodePad(NewBlockAuxMask) * MD6_CHUNK_BITS,FinalBlock);
    CompressBlockArray(CurrentBlock^.BlockArray);
    If FinalBlock then
      begin
        // copy to resulting hash (field fMD6)
        with CurrentBlock^ do
          Move(PtrAdvance(Addr(BlockArray[High(BlockArray)]),PtrInt(SizeOf(TMD6Word) - Length(fMD6)))^,fMD6[0],Length(fMD6));
        // relese the block, it would otherwise be released in ThreadProcessLevel
        fState.Levels[Level].ReleaseBlock(CurrentBlock);
      end
    else ThreadProcessLevel(Level + 1,CurrentBlock,(NewBlockAuxMask and MD6_MP_BLOCKAUX_FLAG_LAST) <> 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.ThreadProcessLevelSeq(Level: Integer; ChildBlock: PMD6MTProcessedBlock; LastBlock: Boolean);
var
  BlockIndex:       Int64;
  ChunkIndex:       Int64;
  CurrentBlock:     PMD6MTProcessedBlock;
  BlockAuxMask:     Integer;
  NewBlockAuxMask:  Integer;
  FinalBlock:       Boolean;
begin
// sequential processing
iDivMod(ChildBlock^.BlockIndex,3,BlockIndex,ChunkIndex);
CurrentBlock := fState.Levels[Level].AcquireBlock(BlockIndex);
{
  Note first chunk is a chaning variable - ie. it is taken from block "to the
  left", not from the child, so leave it untouched.
}
Move(ChildBlock^.BlockArray[Length(ChildBlock^.BlockArray) - MD6_CHUNK_LEN],
     CurrentBlock^.BlockChunks[Succ(ChunkIndex)]^,MD6_CHUNK_SIZE);
ThreadReleaseBlock(Pred(Level),ChildBlock);
{
  If the block is first within its level (ie. has no "left" block), then we
  must initialize first chunk (chaining variable) to all-zero.
}
If (BlockIndex <= 0) and (ChunkIndex <= 0) then
  begin
    FillChar(CurrentBlock^.BlockChunks[0]^,MD6_CHUNK_SIZE,0);
    BlockAuxMask := BlockAuxEncodeDep(0,False);
  end
else BlockAuxMask := 0;
If LastBlock then
  begin
    If ChunkIndex < 2 then
      FillChar(CurrentBlock^.BlockChunks[ChunkIndex + 2]^,(2 - ChunkIndex) * MD6_CHUNK_SIZE,0);
    BlockAuxMask := BlockAuxMask or BlockAuxEncodeDep(Succ(Integer(ChunkIndex)),True) or
                    BlockAuxEncodePad(2 - ChunkIndex) or MD6_MP_BLOCKAUX_FLAG_LAST;
  end
else BlockAuxMask := BlockAuxMask or BlockAuxEncodeDep(Succ(Integer(ChunkIndex)),False);
NewBlockAuxMask := InterlockedOr(CurrentBlock^.BlockAuxiliary,BlockAuxMask);
If (NewBlockAuxMask and MD6_MP_BLOCKAUX_MASK_DEPMAP) = MD6_MP_BLOCKAUX_MASK_DEPMAP then
  begin
  {
    There is no next level, so if this is last block in this level, it means it
    is also a final block.
  }
    FinalBlock := (NewBlockAuxMask and MD6_MP_BLOCKAUX_FLAG_LAST) <> 0;
    CurrentBlock^.BlockArray[MD6_BLOCKARRAY_IDX_U] := BuildUniqueNodeIDWord(Level + 2,BlockIndex);
    CurrentBlock^.BlockArray[MD6_BLOCKARRAY_IDX_V] := BuildControlWord(BlockAuxDecodePad(NewBlockAuxMask) * MD6_CHUNK_BITS,FinalBlock);
    CompressBlockArray(CurrentBlock^.BlockArray);
    If FinalBlock then
      begin
        with CurrentBlock^ do
          Move(PtrAdvance(Addr(BlockArray[High(BlockArray)]),PtrInt(SizeOf(TMD6Word) - Length(fMD6)))^,fMD6[0],Length(fMD6));
        fState.Levels[Level].ReleaseBlock(CurrentBlock);
      end
    else ThreadProcessSequence(CurrentBlock);
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.ThreadProcessLevel(Level: Integer; ChildBlock: PMD6MTProcessedBlock; LastBlock: Boolean);
begin
If Level < High(fState.Levels) then
  // hierarchical processing
  ThreadProcessLevelHir(Level,ChildBlock,LastBlock)
else If Level = High(fState.Levels) then
  // sequential processing
  ThreadProcessLevelSeq(Level,ChildBlock,LastBlock)
else
  // oops
  raise EMD6ProcessingError.CreateFmt('TMD6ProcessorMultiBase.ThreadProcessLevel: Requested non-existing level (%d).',[Level]);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.Initialize;
begin
inherited;
fProcessing.EndingEvent := TEvent.Create(nil,True,False,'');
fState.Levels := nil;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.Initialize(Source: TMD6ProcessorBase);
begin
inherited;
{
  There is no safe and reliable way to do copy of internal structures when they
  are being operated upon by worker threads.

  I can think of some solutions, but they would be extremely convoluted (eg.
  there would be a need to pause all worker threads at predetermined location),
  probably doubling the size of this library - not really interested unless
  someone is paying real money for that :/
}
raise EMD6OperationNotAllowed.Create('TMD6ProcessorMultiBase.Initialize: Creating copy of multi-thread processing is not supported.');
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.Finalize;
var
  i:  Integer;
begin
For i := Low(fState.Levels) to High(fState.Levels) do
  FreeAndNil(fState.Levels[i]);
fState.Levels := nil;
FreeAndNil(fProcessing.EndingEvent);
inherited;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.DoThreadStart;
begin
If Assigned(fOnThreadStart) then
  begin
    Inc(fProcessing.ThreadsSpawned);  
    InterlockedIncrement(fProcessing.EndingCounter);
  {
    Following protects against rare possibility (only in parallel processing
    when working on very small data) that all previously existing threads ended
    before calling this function and therefore have set the event - it might
    allow main thread to exit processing and free ending event before thread
    spawned here ends itself and inadvertently tries to touch the event that
    is already freed (or in general the processor object, which might be also
    gone), causing bizzare and hard to debug AV.
  }
    fProcessing.EndingEvent.ResetEvent;
    fOnThreadStart(Self);
  end
else raise EMD6InvalidState.Create('TMD6ProcessorMultiBase.DoThreadStart: Request event not assigned.');
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.WaitForThreads;
begin
// now wait for all worker threads to end
fProcessing.EndingEvent.WaitFor(INFINITE);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.ProcessEnding;
var
  ExcObj: Pointer;
begin
WaitForThreads;
// by now all threads should have ended, re-raise exception if there is any
ExcObj := InterlockedExchange(fProcessing.ExceptionObject,nil);
If Assigned(ExcObj) then
  begin
    If InterlockedLoad(fProcessing.Status) >= MD6_MT_STATUS_TERMINATING then
      raise Exception(ExcObj)
    else
      FreeAndNil(Exception(ExcObj));
  end;
end;

{-------------------------------------------------------------------------------
    TMD6ProcessorMultiBase - public methods
-------------------------------------------------------------------------------}

procedure TMD6ProcessorMultiBase.ProcessInit;
var
  i:  Integer;
begin
// init processing variables
fProcessing.Status := MD6_MT_STATUS_RUNNING;
fProcessing.ThreadsSpawned := 0;
fProcessing.EndingCounter := 0;
// the event should be created non-signaled, but to be sure...
fProcessing.EndingEvent.ResetEvent;
fProcessing.ExceptionObject := nil;
// init state
fState.Levels := nil;
SetLength(fState.Levels,iMin(fModeControl,27{see MD6 specification for explanation}));
For i := High(fState.Levels) downto Low(fState.Levels) do
  fState.Levels[i] := TMD6MTProcessedBlockProvider.Create(Self.InitializeBlockArray);
{
  Since some fields otherwise accessed by interlocked functions are assigned
  by "normal" means here, we better isssue memory barrier to ensure memory
  coherency.
}
ReadWriteBarrier;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.ProcessEnd;
begin
InterlockedStore(fProcessing.Status,MD6_MT_STATUS_ENDING);
WaitForThreads;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMultiBase.ProcessTerminate;
begin
InterlockedStore(fProcessing.Status,MD6_MT_STATUS_TERMINATING);
WaitForThreads;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TMD6ProcessorMulti
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD6ProcessorMulti - semaphore operations
===============================================================================}
type
  TMD6SemaphoreHandle = {$IFDEF Windows}THANDLE{$ELSE}sem_t{$ENDIF};

//------------------------------------------------------------------------------

procedure SemaphoreInit(out Sem: TMD6SemaphoreHandle; InitialValue: UInt32);
begin
{$IFDEF Windows}
Sem := CreateSemaphore(nil,LongInt(InitialValue),MAXINT,nil);
If Sem = 0 then
  raise EMD6SemaphoreError.CreateFmt('SemaphoreInit: Failed to create semaphore (%u).',[GetLastError]);
{$ELSE}
If sem_init(@Sem,0,cunsigned(InitialValue)) = -1 then
  raise EMD6SemaphoreError.CreateFmt('SemaphoreInit: Failed to create semaphore (%d).',[errno_ptr^]);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure SemaphoreFinal(var Sem: TMD6SemaphoreHandle);
begin
// ignore errors
{$IFDEF Windows}
CloseHandle(Sem);
Sem := 0;
{$ELSE}
sem_destroy(@Sem);
{$ENDIF}
end;

//------------------------------------------------------------------------------
{
  Argument Sem must be "var" bevause of Linux, where it is directly used by the
  calls (ie. it is not a simple "handle").
}
procedure SemaphoreWait(var Sem: TMD6SemaphoreHandle);
{$IFDEF Windows}
var
  WaitResult: DWORD;
begin
WaitResult := WaitForSingleObject(Sem,INFINITE);
case WaitResult of
  WAIT_OBJECT_0:; // all is well
  WAIT_ABANDONED: raise EMD6SemaphoreError.Create('SemaphoreWait: Invalid wait result (abandoned).');
  WAIT_TIMEOUT:   raise EMD6SemaphoreError.Create('SemaphoreWait: Invalid wait result (timeout).');
  WAIT_FAILED:    raise EMD6SemaphoreError.CreateFmt('SemaphoreWait: Failed to wait semaphore (%u).',[GetLastError]);
else
  raise EMD6SemaphoreError.CreateFmt('SemaphoreWait: Unknown wait result (%u).',[WaitResult]);
end;
end;
{$ELSE}
var
  ExitWait:     Boolean;
  ErrorNumber:  cint;
begin
repeat
  ExitWait := True;
  If sem_wait(@Sem) = -1 then
    begin
    ErrorNumber := errno_ptr^;
    If ErrorNumber = ESysEINTR then
      ExitWait := False
    else
      raise EMD6SemaphoreError.CreateFmt('SemaphoreWait: Failed to wait on semaphore (%d).',[errno_ptr^]);
    end;
until ExitWait;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure SemaphorePost(var Sem: TMD6SemaphoreHandle; Count: Integer = 1);
{$IFDEF Windows}
begin
If not ReleaseSemaphore(Sem,Count,nil) then
  raise EMD6SemaphoreError.CreateFmt('SemaphoreFree: Failed to post semaphore (%u).',[GetLastError]);
end;
{$ELSE}
var
  i:  Integer;
begin
For i := 1 to Count do
  If sem_post(@Sem) = -1 then
    raise EMD6SemaphoreError.CreateFmt('SemaphorePost: Failed to post semaphore (%d).',[errno_ptr^]);
end;
{$ENDIF}

{===============================================================================
    TMD6ProcessorMulti - implementation types and constants
===============================================================================}
const
  // number of blocks in the buffer will be MaxThreads plus this number
  MD6_MT_BUFFER_RESERVE = 8;

  MD6_MT_UNPROC_LIMIT = MD6_MT_BUFFER_RESERVE;
  MD6_MT_UNPROC_SPAWN = 16;

{===============================================================================
    TMD6ProcessorMulti - class declaration
===============================================================================}
type
  TMD6ProcessorMulti = class(TMD6ProcessorMultiBase)
  protected
    fBuffer:          record
      Blocks:           array of record // only a storage (does not change during processing)
        Next:             Integer;      // buffer index of next block, used only in lists
        BlockData:        TMD6MTProcessedBlock;
        IsLast:           Boolean;
        PadBytes:         TMemSize;
      end;
    {
      WriteSemaphore

      Initialized to number of blocks in the buffer. Waited by main thread when
      picking empty block. Posted by worker threads when (after) full block is
      processed and returned to empty blocks.
    }
      WriteSemaphore:   TMD6SemaphoreHandle;
    {
      ReadSemaphore

      Initialized to zero. Waited by worker thread when picking full block for
      processing. Posted by main thread when streamed block is full and is
      passed for processing.
    }
      ReadSemaphore:    TMD6SemaphoreHandle;
      // singly linked lists (linked only via indices) of empty and full blocks
      EmptyBlocks:      record
        Lock:             TMD6Spinlock;
        First:            Integer;  // index (into Blocks array) of first block in this list
        Count:            Integer;  // number of blocks in this list, duh!
      end;
      FullBlocks:       record
        Lock:             TMD6Spinlock;
        First:            Integer;
        Count:            Integer;
      end;
      StreamedBlock:    record
        Index:            Integer;  // index in the Blocks array
        Chunks:           TMD6BlockChunks;
        Deposited:        Integer;  // number of chunks written (0..4)
      end;      
    {
      UnprocessedCnt

      How many times in a row there were MD6_MT_UNPROC_LIMIT or more unprocessed
      (full) blocks. If number of full blocks drop below the limit, then this
      field is reset to zero.
      If at or above MD6_MT_UNPROC_SPAWN and ThreadsSpawned < MaxThreads, then
      new worker thread is spawned and this field is reset to zero.

      Accessed only by caller thread, therefore not thread protected.
    }
      UnprocessedCnt:   Integer;
    end;
    // buffer block lists manipulation
    procedure BufferEmptyBlocksPush(Index: Integer); virtual;
    Function BufferEmptyBlocksPop(out Index: Integer; NoFailOnTerm: Boolean = False): Boolean; virtual;
    procedure BufferFullBlocksPush(Index: Integer; out PrevCount: Integer); overload; virtual;
    procedure BufferFullBlocksPush(Index: Integer); overload; virtual;
    Function BufferFullBlocksPop(out Index: Integer): Boolean; virtual;
    // methods called by worker threads
    procedure ThreadReleaseBlock(Level: Integer; Block: PMD6MTProcessedBlock); override;
    Function ThreadProcessBlock: Boolean; virtual;
    procedure ThreadExecute; override;
    // other methods
    procedure Initialize; override;
    procedure Finalize; override;
    procedure WaitForThreads; override;
  public
    procedure ProcessInit; override;
    procedure ProcessUpdate(const Chunk); override;
    procedure ProcessLast(ChunkPadBytes: TMemSize); override;
  end;

{===============================================================================
    TMD6ProcessorMulti - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6ProcessorMulti - protected methods
-------------------------------------------------------------------------------}

procedure TMD6ProcessorMulti.BufferEmptyBlocksPush(Index: Integer);
begin
SpinlockAcquire(fBuffer.EmptyBlocks.Lock);
try
  If fBuffer.EmptyBlocks.Count <= 0 then
    fBuffer.Blocks[Index].Next := -1
  else
    fBuffer.Blocks[Index].Next := fBuffer.EmptyBlocks.First;
  fBuffer.EmptyBlocks.First := Index;
  Inc(fBuffer.EmptyBlocks.Count);
finally
  SpinlockRelease(fBuffer.EmptyBlocks.Lock);
end;
end;

//------------------------------------------------------------------------------

Function TMD6ProcessorMulti.BufferEmptyBlocksPop(out Index: Integer; NoFailOnTerm: Boolean = False): Boolean;
begin
SpinlockAcquire(fBuffer.EmptyBlocks.Lock);
try
  If fBuffer.EmptyBlocks.Count > 0 then
    begin
      Index := fBuffer.EmptyBlocks.First;
      fBuffer.EmptyBlocks.First := fBuffer.Blocks[Index].Next;
      Dec(fBuffer.EmptyBlocks.Count);
      fBuffer.Blocks[Index].Next := -1;
    end
  else Index := -1;
finally
  SpinlockRelease(fBuffer.EmptyBlocks.Lock);
end;
// process situations where the list was empty
If Index < 0 then
  begin
    If InterlockedLoad(fProcessing.Status) = MD6_MT_STATUS_RUNNING then
      raise EMD6ProcessingError.Create('TMD6ProcessorMulti.BufferEmptyBlocksPop: Empty list.')
    else
      // ending or terminating, the thread will simply end
      Result := False;
  end
else If not NoFailOnTerm then
  Result := InterlockedLoad(fProcessing.Status) < MD6_MT_STATUS_TERMINATING
else
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMulti.BufferFullBlocksPush(Index: Integer; out PrevCount: Integer);
begin
SpinlockAcquire(fBuffer.FullBlocks.Lock);
try
  If fBuffer.FullBlocks.Count <= 0 then
    fBuffer.Blocks[Index].Next := -1
  else
    fBuffer.Blocks[Index].Next := fBuffer.FullBlocks.First;
  fBuffer.FullBlocks.First := Index;
  PrevCount := fBuffer.FullBlocks.Count;
  Inc(fBuffer.FullBlocks.Count);
finally
  SpinlockRelease(fBuffer.FullBlocks.Lock);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TMD6ProcessorMulti.BufferFullBlocksPush(Index: Integer);
var
  PrevCount:  Integer;
begin
BufferFullBlocksPush(Index,PrevCount);
end;

//------------------------------------------------------------------------------

Function TMD6ProcessorMulti.BufferFullBlocksPop(out Index: Integer): Boolean;
begin
SpinlockAcquire(fBuffer.FullBlocks.Lock);
try
  If fBuffer.FullBlocks.Count > 0 then
    begin
      Index := fBuffer.FullBlocks.First;
      fBuffer.FullBlocks.First := fBuffer.Blocks[Index].Next;
      Dec(fBuffer.FullBlocks.Count);
      fBuffer.Blocks[Index].Next := -1;
    end
  else Index := -1;
finally
  SpinlockRelease(fBuffer.FullBlocks.Lock);
end;
If Index < 0 then
  begin
    If InterlockedLoad(fProcessing.Status) = MD6_MT_STATUS_RUNNING then
      raise EMD6ProcessingError.Create('TMD6ProcessorMulti.BufferFullBlocksPop: Empty list.')
    else
      Result := False;
  end
else Result := InterlockedLoad(fProcessing.Status) < MD6_MT_STATUS_TERMINATING;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMulti.ThreadReleaseBlock(Level: Integer; Block: PMD6MTProcessedBlock);
begin
If Level < Low(fState.Levels) then
  begin
    // buffer block
    BufferEmptyBlocksPush(Block^.BlockAuxiliary);
    SemaphorePost(fBuffer.WriteSemaphore)
  end
// "normal" level block
else inherited ThreadReleaseBlock(Level,Block);
end;

//------------------------------------------------------------------------------

Function TMD6ProcessorMulti.ThreadProcessBlock: Boolean;
var
  Index:  Integer;
begin
Result := False;
SemaphoreWait(fBuffer.ReadSemaphore);
// BufferFullBlocksPop checks status, no need to do it explicitly
If BufferFullBlocksPop(Index) then
  begin
    // main processing - prepare and compress the buffered block
    with fBuffer.Blocks[Index] do
      begin
      {
        We are at level 1, level zero are the hashed data (chunks).

        Except for extremely small data (one block or less, ie. <= 512 bytes),
        this cannot be final processing - there must be at least one level
        above this one (because MT processing is activated only for ModeControl
        above 1, which implies two or more levels).
      }
        If (BlockData.BlockIndex <= 0) and IsLast then
          begin
            // small data
            BlockData.BlockArray[MD6_BLOCKARRAY_IDX_U] := BuildUniqueNodeIDWord(1,BlockData.BlockIndex);
            BlockData.BlockArray[MD6_BLOCKARRAY_IDX_V] := BuildControlWord(PadBytes * 8,True);
            CompressBlockArray(BlockData.BlockArray);
            // we are done, copy result
            Move(PtrAdvance(Addr(BlockData.BlockArray[High(BlockData.BlockArray)]),
                 PtrInt(SizeOf(TMD6Word) - Length(fMD6)))^,fMD6[0],Length(fMD6));
          end
        else
          begin
            BlockData.BlockArray[MD6_BLOCKARRAY_IDX_U] := BuildUniqueNodeIDWord(1,BlockData.BlockIndex);
            BlockData.BlockArray[MD6_BLOCKARRAY_IDX_V] := BuildControlWord(PadBytes * 8,False);
            CompressBlockArray(BlockData.BlockArray);
          {
            Pass processing to next level.

            Store buffer index of the block in its auxiliary word - it will be
            used within ThreadProcessLevel to return it to circulation (yes,
            that func. will also release the block).
          }
            BlockData.BlockAuxiliary := Index;
            ThreadProcessLevel(Low(fState.Levels),Addr(BlockData),IsLast);
          end;
      end;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMulti.ThreadExecute;
begin
try
  try
    while ThreadProcessBlock do {nothing, just keep it going until it returns false};
  except
  {
    Store exception object for re-raising in main thread, but only if there is
    no other exception object already stored.
  }
    If InterlockedCompareExchange(fProcessing.ExceptionObject,ExceptObject,nil) = nil then
      AcquireExceptionObject; // make sure the stored object will not be automatically freed
    InterlockedStore(fProcessing.Status,MD6_MT_STATUS_TERMINATING);
    // in case main thread is currently waiting on write semaphore...
    SemaphorePost(fBuffer.WriteSemaphore);
  end;
finally
  If InterlockedDecrement(fProcessing.EndingCounter) <= 0 then
    fProcessing.EndingEvent.SetEvent; // release the waiting main thread
end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMulti.Initialize;
begin
inherited;
SemaphoreInit(fBuffer.WriteSemaphore,0);
SemaphoreInit(fBuffer.ReadSemaphore,0);
SpinlockInit(fBuffer.EmptyBlocks.Lock);
SpinlockInit(fBuffer.FullBlocks.Lock);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMulti.Finalize;
begin
SpinlockFinal(fBuffer.FullBlocks.Lock);
SpinlockFinal(fBuffer.EmptyBlocks.Lock);
SemaphoreFinal(fBuffer.ReadSemaphore);
SemaphoreFinal(fBuffer.WriteSemaphore);
inherited;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMulti.WaitForThreads;
begin
// make sure no worker thread waits on read semaphore
SemaphorePost(fBuffer.ReadSemaphore,fMaxThreads);
inherited;
end;

{-------------------------------------------------------------------------------
    TMD6ProcessorMulti - public methods
-------------------------------------------------------------------------------}

procedure TMD6ProcessorMulti.ProcessInit;
var
  i:  Integer;
begin
try
  inherited;
  // init buffer
  SetLength(fBuffer.Blocks,fMaxThreads + MD6_MT_BUFFER_RESERVE);
  For i := Low(fBuffer.Blocks) to High(fBuffer.Blocks) do
    begin
      // leave first for streamed block
      If i > Succ(Low(fBuffer.Blocks)) then
        fBuffer.Blocks[i].Next := Pred(i)
      else
        fBuffer.Blocks[i].Next := -1;
      InitializeBlockArray(fBuffer.Blocks[i].BlockData.BlockArray,fBuffer.Blocks[i].BlockData.BlockChunks);
      fBuffer.Blocks[i].IsLast := False;
      fBuffer.Blocks[i].PadBytes := 0;
    end;
  // init semaphores
  SemaphorePost(fBuffer.WriteSemaphore,Pred(Length(fBuffer.Blocks)){-1 for streamed block});
  // init lists
  fBuffer.EmptyBlocks.First := High(fBuffer.Blocks);
  fBuffer.EmptyBlocks.Count := Pred(Length(fBuffer.Blocks));
  fBuffer.FullBlocks.First := -1;
  fBuffer.FullBlocks.Count := 0;
  // init streamed block
  fBuffer.StreamedBlock.Index := Low(fBuffer.Blocks);
  fBuffer.StreamedBlock.Chunks := fBuffer.Blocks[fBuffer.StreamedBlock.Index].BlockData.BlockChunks;
  fBuffer.StreamedBlock.Deposited := 0;
  fBuffer.Blocks[fBuffer.StreamedBlock.Index].Next := -1;
  // ensure proper index of the first block
  fBuffer.Blocks[fBuffer.StreamedBlock.Index].BlockData.BlockIndex := 0;
  // init other fields of buffer
  fBuffer.UnprocessedCnt := 0;
  // ensure memory coherency
  ReadWriteBarrier;
{
  Spawn first two threads (more may be spawned later if required - btw.
  spawning only one is somewhat pointless).

  Note both will immediately run into locked read semaphore.
}
  DoThreadStart;
  DoThreadStart;
except
  ProcessTerminate;
  raise;
end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMulti.ProcessUpdate(const Chunk);
var
  BlockIndex:     Int64;
  PrevFullBlocks: Integer;
begin
try
  If InterlockedLoad(fProcessing.Status) = MD6_MT_STATUS_RUNNING then
    begin
      // if streamed block is full, commit it for processing
      If fBuffer.StreamedBlock.Deposited >= 4 then
        begin
          BlockIndex := fBuffer.Blocks[fBuffer.StreamedBlock.Index].BlockData.BlockIndex;
        {
          Following assigns do not need to be interlocked - interlocked calls
          in BufferFullBlocksPush should provide necessary serialization.
        }
          fBuffer.Blocks[fBuffer.StreamedBlock.Index].BlockData.BlockAuxiliary := 0;
          fBuffer.Blocks[fBuffer.StreamedBlock.Index].IsLast := False;
          fBuffer.Blocks[fBuffer.StreamedBlock.Index].PadBytes := 0;
          // move streamed block to list of full blocks
          BufferFullBlocksPush(fBuffer.StreamedBlock.Index,PrevFullBlocks);
        {
          Spawn new threads if necessary and possible.

          Keep this order of checks - it is potentially faster when at thread
          limit.
        }
          If fProcessing.ThreadsSpawned < fMaxThreads then
            begin
              If PrevFullBlocks >= MD6_MT_UNPROC_LIMIT then
                begin
                  Inc(fBuffer.UnprocessedCnt);
                  If fBuffer.UnprocessedCnt >= MD6_MT_UNPROC_SPAWN then
                    begin
                      DoThreadStart;
                      fBuffer.UnprocessedCnt := 0;
                    end;
                end
              else fBuffer.UnprocessedCnt := 0;
            end;
          // release one worker thread
          SemaphorePost(fBuffer.ReadSemaphore);
          // get empty block for streaming
          SemaphoreWait(fBuffer.WriteSemaphore);
          // do status check in case of crash during waiting on semaphore...
          If InterlockedLoad(fProcessing.Status) = MD6_MT_STATUS_RUNNING then
            begin
              If not BufferEmptyBlocksPop(fBuffer.StreamedBlock.Index,True) then
                raise EMD6ProcessingError.Create('TMD6ProcessorMulti.ProcessUpdate: Failed to allocate block.');
              fBuffer.StreamedBlock.Chunks := fBuffer.Blocks[fBuffer.StreamedBlock.Index].BlockData.BlockChunks;
              fBuffer.StreamedBlock.Deposited := 0;
              fBuffer.Blocks[fBuffer.StreamedBlock.Index].BlockData.BlockIndex := BlockIndex + 1;
            end
          else ProcessEnding;
        end;
      // now the streamed block is not full, add current data and just exit
      Move(Chunk,fBuffer.StreamedBlock.Chunks[fBuffer.StreamedBlock.Deposited]^,MD6_CHUNK_SIZE);
      Inc(fBuffer.StreamedBlock.Deposited);
    end
  else ProcessEnding;
except
  ProcessTerminate;
  raise;
end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorMulti.ProcessLast(ChunkPadBytes: TMemSize);
var
  i:  Integer;
begin
try
  If InterlockedLoad(fProcessing.Status) = MD6_MT_STATUS_RUNNING then
    begin
      If fBuffer.StreamedBlock.Deposited < 4 then
        // fill missing bytes with zeroes
        For i := fBuffer.StreamedBlock.Deposited to High(fBuffer.StreamedBlock.Chunks) do
          begin
            FillChar(fBuffer.StreamedBlock.Chunks[i]^,MD6_CHUNK_SIZE,0);
            Inc(ChunkPadBytes,MD6_CHUNK_SIZE);
          end;
      fBuffer.Blocks[fBuffer.StreamedBlock.Index].BlockData.BlockAuxiliary := 0;
      fBuffer.Blocks[fBuffer.StreamedBlock.Index].IsLast := True;
      fBuffer.Blocks[fBuffer.StreamedBlock.Index].PadBytes := ChunkPadBytes;
      BufferFullBlocksPush(fBuffer.StreamedBlock.Index);
      SemaphorePost(fBuffer.ReadSemaphore);
      ProcessEnd;
    end
  else ProcessEnding;
except
  ProcessTerminate;
  raise;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              TMD6ProcessorParallel
--------------------------------------------------------------------------------
===============================================================================}
type
  TMD6ProgressHandler = procedure(Progess: Double) of object;

{===============================================================================
    TMD6ProcessorParallel - class declaration
===============================================================================}
type
  TMD6ProcessorParallel = class(TMD6ProcessorMultiBase)
  protected
    fParallelState: record
      ProcBuffer: record
        Address:    Pointer;
        Size:       TMemSize;
      end;
      LastBlock:  record
        BlockIndex: Int64;
        ValidBytes: TMemSize;
      end;
      BlockIndex: Int64;
    end;
    fProgressHandler: TMD6ProgressHandler;
    procedure ThreadReleaseBlock(Level: Integer; Block: PMD6MTProcessedBlock); override;
    procedure ThreadProcessing; virtual;
    procedure ThreadExecute; override;
    procedure ReportProgress(Progress: Double); virtual;
  public
    procedure ProcessUpdate(const Chunk); override;
    procedure ProcessLast(ChunkPadBytes: TMemSize); override;
    procedure ProcessParallel(Address: Pointer; Size: TMemSize); virtual;
    property ProgressHandler: TMD6ProgressHandler read fProgressHandler write fProgressHandler;
  end;

{===============================================================================
    TMD6ProcessorParallel - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD6ProcessorParallel - protected methods
-------------------------------------------------------------------------------}

procedure TMD6ProcessorParallel.ThreadReleaseBlock(Level: Integer; Block: PMD6MTProcessedBlock);
begin
// do not release blocks passed from ThreadProcessing
If Level >= Low(fState.Levels) then
  inherited ThreadReleaseBlock(Level,BLock);
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorParallel.ThreadProcessing;
var
  Block:    TMD6MTProcessedBlock;
  PadBytes: TMemSize;

  Function GetBlockAddress: Pointer;
  begin
    Result := PtrAdvance(fParallelState.ProcBuffer.Address,PtrInt(Block.BlockIndex * MD6_BLOCK_SIZE));
  end;

begin
InitializeBlockArray(Block.BlockArray,Block.BlockChunks);
while True do
  begin
    Block.BlockIndex := InterlockedExchangeAdd(fParallelState.BlockIndex,1);
    If Block.BlockIndex < fParallelState.LastBlock.BlockIndex then
      begin
        // "normal" block (ie. not a last one)
        Move(GetBlockAddress^,Block.BlockChunks[0]^,MD6_BLOCK_SIZE);
        // compress block and pass it to the next level
        Block.BlockArray[MD6_BLOCKARRAY_IDX_U] := BuildUniqueNodeIDWord(1,Block.BlockIndex);
        Block.BlockArray[MD6_BLOCKARRAY_IDX_V] := BuildControlWord(0,False);
        CompressBlockArray(Block.BlockArray);
        ThreadProcessLevel(Low(fState.Levels),@Block,False);
      end
    else If Block.BlockIndex = fParallelState.LastBlock.BlockIndex then
      begin
        // processing last block
        If fParallelState.LastBlock.ValidBytes > 0 then
          Move(GetBlockAddress^,Block.BlockChunks[0]^,fParallelState.LastBlock.ValidBytes);
        // padding
        PadBytes := MD6_BLOCK_SIZE - fParallelState.LastBlock.ValidBytes;
        FillChar(PtrAdvance(Block.BlockChunks[0],PtrInt(fParallelState.LastBlock.ValidBytes))^,PadBytes,0);
        // if this is also the first block (index 0) then it must be a final block
        Block.BlockArray[MD6_BLOCKARRAY_IDX_U] := BuildUniqueNodeIDWord(1,Block.BlockIndex);
        Block.BlockArray[MD6_BLOCKARRAY_IDX_V] := BuildControlWord(PadBytes * 8,Block.BlockIndex <= 0);
        CompressBlockArray(Block.BlockArray);
        If Block.BlockIndex <= 0 then
          // final block -> processing small or no data, just copy resulting hash
          Move(PtrAdvance(Addr(Block.BlockArray[High(Block.BlockArray)]),PtrInt(SizeOf(TMD6Word) - Length(fMD6)))^,fMD6[0],Length(fMD6))
        else
          ThreadProcessLevel(Low(fState.Levels),@Block,True);
      end
    else Break{while};  // all blocks have been consumed
  end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorParallel.ThreadExecute;
begin
try
  try
    ThreadProcessing; // all happens here
  except
    If InterlockedCompareExchange(fProcessing.ExceptionObject,ExceptObject,nil) = nil then
      AcquireExceptionObject;
    InterlockedStore(fProcessing.Status,MD6_MT_STATUS_TERMINATING);
  end;
finally
  If InterlockedDecrement(fProcessing.EndingCounter) <= 0 then
    fProcessing.EndingEvent.SetEvent;
end;
end;

//------------------------------------------------------------------------------

procedure TMD6ProcessorParallel.ReportProgress(Progress: Double);
begin
If Assigned(fProgressHandler) then
  fProgressHandler(Progress);
end;

{-------------------------------------------------------------------------------
    TMD6ProcessorParallel - public methods
-------------------------------------------------------------------------------}

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6ProcessorParallel.ProcessUpdate(const Chunk);
begin
raise EMD6OperationNotAllowed.Create('TMD6ProcessorParallel.ProcessUpdate: Operation not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6ProcessorParallel.ProcessLast(ChunkPadBytes: TMemSize);
begin
raise EMD6OperationNotAllowed.Create('TMD6ProcessorParallel.ProcessLast: Operation not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TMD6ProcessorParallel.ProcessParallel(Address: Pointer; Size: TMemSize);
var
  i:          Integer;
  WaitResult: TWaitResult;
begin
ProcessInit;
try
  // prepare parallel state
  fParallelState.ProcBuffer.Address := Address;
  fParallelState.ProcBuffer.Size := Size;
  If Size > 0 then
    begin
      fParallelState.LastBlock.BlockIndex := Pred(Int64(uDivCeil(Size,MD6_BLOCK_SIZE)));
      fParallelState.LastBlock.ValidBytes := Succ(Pred(Size) mod MD6_BLOCK_SIZE);
    end
  else
    begin
      fParallelState.LastBlock.BlockIndex := 0;
      fParallelState.LastBlock.ValidBytes := 0;
    end;
  InterlockedStore(fParallelState.BlockIndex,0);
  ReportProgress(0.0);
  // spawn as many threads as allowed, they will immediately start processing
  For i := 1 to fMaxThreads do
    DoThreadStart;
  // now wait until it all finishes (or crashes)
  while True do
    begin
      WaitResult := fProcessing.EndingEvent.WaitFor(100{ms});
      case WaitResult of
        wrSignaled: Break;                  // processing is done, exit cycle
        wrTimeout:  with fParallelState do  // report progress and continue the cycle
                      ReportProgress(Succ(InterlockedLoad(BlockIndex)) / Succ(LastBlock.BlockIndex));
      else
        raise EMD6ProcessingError.CreateFmt('TMD6ProcessorParallel.ProcessParallel: Waiting failed (%d).',[Ord(WaitResult)]);
      end;
    end;
  If InterlockedLoad(fProcessing.Status) = MD6_MT_STATUS_RUNNING then
    ProcessEnd
  else
    ProcessEnding;
  ReportProgress(1.0);  
except
  ProcessTerminate;
  raise
end;
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

procedure ThreadFunction(Param: Pointer);
begin
try
  TMD6ProcessorMultiBase(Param).ThreadExecute;
except
  // eat all exceptions
end;
end;

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
FreeOnTerminate := False;
fThreadFunction := ThreadFunction;
fParam := Param;
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
If fProcessing then
  raise EMD6InvalidState.Create('TMD6Hash.SetMD6: Cannot change hash during processing.');
If (Length(Value) <= 0) or (Length(Value) > MD6_BYTES_MAX) then
  raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetMD6: Invalid hash length (%d).',[Length(Value)]);
fMD6 := Copy(Value);
fHashBits := Length(Value) * 8;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetHashBits(Value: Integer);
begin
If fProcessing then
  raise EMD6InvalidState.Create('TMD6Hash.SetHashBits: Cannot change hash bits during processing.');
If (Value <= 0) or (Value > MD6_BITS_MAX) then
  raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetHashBits: Invalid hash bits value (%d).',[Value]);
If (Value and 7) <> 0 then
  raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetHashBits: Unsupported hash bits value (%d).',[Value]);
fMD6 := nil;  // to prevent copying and clear the variable
SetLength(fMD6,Value div 8);
fHashBits := Value;
end;

//------------------------------------------------------------------------------

Function TMD6Hash.GetKey: TMD6Key;
begin
Result := Copy(fKey);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetKey(Value: TMD6Key);
begin
If fProcessing then
  raise EMD6InvalidState.Create('TMD6Hash.SetKey: Cannot change key during processing.');
If Length(Value) > MD6_KEY_MAXSIZE then
  raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetKey: Invalid key length (%d).',[Length(Value)]);
fKey := Copy(Value);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetRounds(Value: Integer);
begin
If fProcessing then
  raise EMD6InvalidState.Create('TMD6Hash.SetRounds: Cannot change number of rounds during processing.');
If Value < 0 then
  raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetRounds: Invalid number of rounds (%d).',[Value]);
fRounds := Value;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetModeControl(Value: Integer);
begin
If fProcessing then
  raise EMD6InvalidState.Create('TMD6Hash.SetModeControl: Cannot change mode control during processing.');
If (Value < 0) or (Value > 64) then
  raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetModeControl: Invalid mode control (%d).',[Value]);
fModeControl := Value;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetMaxThreads(Value: Integer);
begin
If fProcessing then
  raise EMD6InvalidState.Create('TMD6Hash.SetMaxThreads: Cannot maximum number of threads during processing.');
If Value <= 0 then
  raise EMD6InvalidValue.CreateFmt('TMD6Hash.SetMaxThreads: Invalid maximum number of threads (%d).',[Value]);
fMaxThreads := iMin(1024,Value);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ProcessBlock(const Block{128-byte chunk});
begin
TMD6ProcessorBase(fProcessor).ProcessUpdate(Block);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ProcessFirst(const Block);
begin
fProcessing := True;
inherited;
TMD6ProcessorBase(fProcessor).ProcessUpdate(Block);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ProcessLast;
var
  PadBytes: TMemSize;
begin
If fTransCount > 0 then
  begin
    PadBytes := MD6_CHUNK_SIZE - fTransCount;
    FillChar(PtrAdvance(fTransBlock,PtrInt(fTransCount))^,PadBytes,0);
    TMD6ProcessorBase(fProcessor).ProcessUpdate(fTransBlock^);
  end
else PadBytes := 0;
TMD6ProcessorBase(fProcessor).ProcessLast(PadBytes);
fMD6 := TMD6ProcessorBase(fProcessor).MD6;
WaitAndFreeThreads;
fProcessing := False;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ThreadStartHandler(Sender: TObject);
begin
If Sender = fProcessor then // sanity check
  begin
    If Assigned(fOnThreadStartEvent) then
      fOnThreadStartEvent(Self,ThreadFunction,Pointer(fProcessor))
    else If Assigned(fOnThreadStartCallback) then
      fOnThreadStartCallback(Self,ThreadFunction,Pointer(fProcessor))
    else
      // not event nor callback is assigned, we have to spawn our own thread
      fWorkerThreads.Add(TMD6WorkerThread.Create(ThreadFunction,Pointer(fProcessor)));
  end
else raise EMD6InvalidValue.CreateFmt('TMD6Hash.ThreadStartHandler: Invalid sender: %s',[GetInstanceString(Sender)]);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.WaitAndFreeThreads;
var
  i:  Integer;
begin
// make sure all threads ended before freeing them
For i := 0 to Pred(fWorkerThreads.Count) do
  TThread(fWorkerThreads[i]).WaitFor;
fWorkerThreads.Clear; // frees all threads
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.ParallelProcessing(Address: Pointer; Size: TMemSize);
var
  Processor:  TMD6ProcessorParallel;
begin
Processor := TMD6ProcessorParallel.Create;
try
  FreeAndNil(fProcessor);
  fProcessor := Processor;
  // prepare the object
  Processor.HashBits := fHashBits;
  Processor.Key := fKey;
  Processor.Rounds := fRounds;
  Processor.ModeControl := fModeControl;
  Processor.MaxThreads := fMaxThreads;
  Processor.OnThreadStart := ThreadStartHandler;
  Processor.ProgressHandler := DoProgress;
  // do not call ProcessInit, it is called automatically
  Processor.ProcessParallel(Address,Size);
  fMD6 := Processor.MD6;
finally
  fProcessor := nil;
  Processor.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.Initialize;
begin
{
  fBlockSize must be set up here as it is used in inherited code to allocate
  block buffer.
}
fBlockSize := MD6_CHUNK_SIZE;
inherited;
fMD6 := nil;
Setlength(fMD6,MD6_BITS_DEFAULT div 8);
fHashBits := MD6_BITS_DEFAULT;
fKey := nil;
fRounds := 40 + (fHashBits div 4); // 168
fModeControl := MD6_MODE_DEFAULT;
fMaxThreads := 1;
fParallelSizeLimit := 4 * 1024 * 1024;  // 4MiB
fProcessor := nil;
fProcessing := False;
fWorkerThreads := TObjectList.Create(True);
fOnThreadStartCallback := nil;
fOnThreadStartEvent := nil;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.Finalize;
begin
If Assigned(fProcessor) then
  TMD6ProcessorBase(fProcessor).ProcessTerminate;
WaitAndFreeThreads;
FreeAndNil(fWorkerThreads);
FreeAndNil(fProcessor);
inherited;
end;

{-------------------------------------------------------------------------------
    TMD6Hash - public methods
-------------------------------------------------------------------------------}

class Function TMD6Hash.ProcessorCount: Integer;
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
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMD6Hash then
  begin
    fMD6 := Copy(TMD6Hash(Hash).fMD6);
    fHashBits := TMD6Hash(Hash).fHashBits;
    fKey := Copy(TMD6Hash(Hash).Key);
    fRounds := TMD6Hash(Hash).fRounds;
    fModeControl := TMD6Hash(Hash).fModeControl;
    FreeAndNil(fProcessor);
    If Assigned(TMD6Hash(Hash).fProcessor) then
      // note that following will fail for multi-thread processing
      fProcessor := TMD6ProcessorClass(TMD6Hash(Hash).fProcessor.ClassType).
        CreateAsCopy(TMD6ProcessorBase(TMD6Hash(Hash).fProcessor));
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
FreeAndNil(fProcessor);
If (fMaxThreads > 1) and (fModeControl > 0) then
  begin
    fProcessor := TMD6ProcessorMulti.Create;
    TMD6ProcessorMulti(fProcessor).MaxThreads := fMaxThreads;
    TMD6ProcessorMulti(fProcessor).OnThreadStart := ThreadStartHandler;
  end
else fProcessor := TMD6ProcessorSingle.Create;
TMD6ProcessorBase(fProcessor).HashBits := fHashBits;
TMD6ProcessorBase(fProcessor).Key := fKey;  // copy is called internally
TMD6ProcessorBase(fProcessor).Rounds := fRounds;
TMD6ProcessorBase(fProcessor).ModeControl := fModeControl;
TMD6ProcessorBase(fProcessor).ProcessInit;
inherited;
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.Final;
begin
inherited;
FreeAndNil(fProcessor);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.HashBuffer(const Buffer; Size: TMemSize);
begin
// can we do parallel processing?
If (fModeControl > 0) and (fMaxThreads > 1) and (Size >= fParallelSizeLimit) then
  ParallelProcessing(@Buffer,Size)
else
  inherited HashBuffer(Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.HashStream(Stream: TStream; Count: Int64 = -1);
var
  TrueCount:  TMemSize;
begin
If Stream is TCustomMemoryStream then
  begin
    // calculate how many bytes we will be really processing
    If Count = 0 then
      TrueCount := TMemSize(Stream.Size - Stream.Position)
    else If Count < 0 then
      TrueCount := TMemSize(Stream.Size)
    else
      TrueCount := TMemSize(iMin(Count,Stream.Size - Stream.Position));
    If (fModeControl > 0) and (fMaxThreads > 1) and (TrueCount >= fParallelSizeLimit) then
      begin
        If Count >= 0 then
          // process from current position
          ParallelProcessing(PtrAdvance(TCustomMemoryStream(Stream).Memory,PtrInt(Stream.Position)),TrueCount)
        else
          // process from the start
          ParallelProcessing(TCustomMemoryStream(Stream).Memory,TrueCount)
      end
    else inherited HashStream(Stream,Count);
  end
else inherited HashStream(Stream,Count);
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
    Temp := nil;
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
Temp := nil;
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

procedure TMD6Hash.SetupHashBits(HashBits: Integer);
begin
fHashBits := HashBits;
fMD6 := nil;
SetLength(fMD6,fHashBits div 8);  // fills fMD6 with zeroes
If Length(fKey) > 0 then
  fRounds := iMax(80,40 + (fHashBits div 4))
else
  fRounds := 40 + (fHashBits div 4);
end;

//------------------------------------------------------------------------------

procedure TMD6Hash.SetupKey(const Key; Size: TMemSize);
var
  TempKey:  TMD6Key;
begin
TempKey := nil;
If Size <= MD6_KEY_MAXSIZE then
  SetLength(TempKey,Size)
else
  SetLength(TempKey,MD6_KEY_MAXSIZE);
If Size > 0 then
  Move(Key,Addr(TempKey[0])^,Length(TempKey));
Self.Key := TempKey; // calls full setter
If Length(fKey) > 0 then
  fRounds := iMax(80,40 + (fHashBits div 4))
else
  fRounds := 40 + (fHashBits div 4);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TMD6Hash.SetupKey(const Key: array of Byte);
begin
If Length(Key) > 0 then
  SetupKey(Key[0],Length(Key))
else
  SetupKey(nil^,0); // clears the key
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TMD6Hash.SetupKey(const Key: String);
var
  TempStr:  UTF8String;
begin
TempStr := StrToUTF8(Key);
SetupKey(PUTF8Char(TempStr)^,Length(TempStr) * SizeOf(UTF8Char));
end;

//------------------------------------------------------------------------------

Function TMD6Hash.BreakProcessing: Boolean;
begin
Result := inherited BreakProcessing;
If Assigned(fProcessor) then
  TMD6ProcessorBase(fProcessor).ProcessTerminate;
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
raise EMD6OperationNotAllowed.Create('TMD6DefHash.SetHashBits: Changing hash bits is not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetKey(Value: TMD6Key);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.PutKey: Key is not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetRounds(Value: Integer);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.PuSetRoundstKey: Changing number of rounds is not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetModeControl(Value: Integer);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.PuSetRoundstKey: Changing mode control is not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{-------------------------------------------------------------------------------
    TMD6DefHash - public methods
-------------------------------------------------------------------------------}

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetupHashBits(HashBits: Integer);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.SetupHashBits: Changing hash bits is not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TMD6DefHash.SetupKey(const Key; Size: TMemSize);
begin
raise EMD6OperationNotAllowed.Create('TMD6DefHash.SetupKey: Key is not allowed.');
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
Result := nil;
SetLength(Result,SizeOf(Hash));
Move(Hash,Result[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

Function MD6_256ToMD6(Hash: TMD6_256): TMD6;
begin
Result := nil;
SetLength(Result,SizeOf(Hash));
Move(Hash,Result[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

Function MD6_384ToMD6(Hash: TMD6_384): TMD6;
begin
Result := nil;
SetLength(Result,SizeOf(Hash));
Move(Hash,Result[0],SizeOf(Hash));
end;

//------------------------------------------------------------------------------

Function MD6_512ToMD6(Hash: TMD6_512): TMD6;
begin
Result := nil;
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
If KeySize <= MD6_KEY_MAXSIZE then
  SetLength(Result.Key,KeySize)
else
  SetLength(Result.Key,MD6_KEY_MAXSIZE);
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
    If Length(TempStr) * SizeOf(UTF8Char) <= MD6_KEY_MAXSIZE then
      SetLength(Result.Key,Length(TempStr) * SizeOf(UTF8Char))
    else
      SetLength(Result.Key,MD6_KEY_MAXSIZE);
    Move(PUTF8Char(TempStr)^,Result.Key[0],Length(Result.Key));
  end
else Result.Key := nil;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6Settings(HashBits,ModeControl: Integer; Key: TMD6Key = nil): TMD6Settings;
begin
If Assigned(Key) then
  Result := MD6Settings(HashBits,iMax(80,40 + (HashBits div 4)),ModeControl,Key)
else
  Result := MD6Settings(HashBits,40 + (HashBits div 4),ModeControl,nil);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6Settings(HashBits,ModeControl: Integer; const Key; KeySize: TMemSize): TMD6Settings;
begin
If KeySize > 0 then
  Result := MD6Settings(HashBits,iMax(80,40 + (HashBits div 4)),ModeControl,Key,KeySize)
else
  Result := MD6Settings(HashBits,40 + (HashBits div 4),ModeControl,nil^,0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6Settings(HashBits,ModeControl: Integer; const Key: String): TMD6Settings;
begin
If Length(Key) > 0 then
  Result := MD6Settings(HashBits,iMax(80,40 + (HashBits div 4)),ModeControl,Key)
else
  Result := MD6Settings(HashBits,40 + (HashBits div 4),ModeControl,'');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6Settings(HashBits: Integer; Key: TMD6Key = nil): TMD6Settings;
begin
Result := MD6Settings(HashBits,MD6_MODE_DEFAULT,Key);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6Settings(HashBits: Integer; const Key; KeySize: TMemSize): TMD6Settings;
begin
Result := MD6Settings(HashBits,MD6_MODE_DEFAULT,Key,KeySize);
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MD6Settings(HashBits: Integer; const Key: String): TMD6Settings;
begin
Result := MD6Settings(HashBits,MD6_MODE_DEFAULT,Key);
end;

//------------------------------------------------------------------------------

Function InitialMD6(Settings: TMD6Settings): TMD6State;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
// do not use Setup*, all changes should have been made when building settings
Hash.HashBits := Settings.HashBits;
Hash.Rounds := Settings.Rounds;
Hash.ModeControl := Settings.ModeControl;
Hash.Key := Settings.Key;
Hash.Init;
Result := TMD6State(Hash);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InitialMD6(HashBits: Integer = DefaultMD6Bits): TMD6State;
begin
Result := InitialMD6(MD6Settings(HashBits));
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

Function BufferMD6(const Buffer; Size: TMemSize; HashBits: Integer = DefaultMD6Bits): TMD6;
begin
Result := BufferMD6(Buffer,Size,MD6Settings(HashBits));
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

Function AnsiStringMD6(const Str: AnsiString; HashBits: Integer = DefaultMD6Bits): TMD6;
begin
Result := AnsiStringMD6(Str,MD6Settings(HashBits));
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

Function WideStringMD6(const Str: WideString; HashBits: Integer = DefaultMD6Bits): TMD6;
begin
Result := WideStringMD6(Str,MD6Settings(HashBits));
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

Function StringMD6(const Str: String; HashBits: Integer = DefaultMD6Bits): TMD6;
begin
Result := StringMD6(Str,MD6Settings(HashBits));
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

Function StreamMD6(Stream: TStream; Count: Int64 = -1; HashBits: Integer = DefaultMD6Bits): TMD6;
begin
Result := StreamMD6(Stream,Count,MD6Settings(HashBits));
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

Function FileMD6(const FileName: String; HashBits: Integer = DefaultMD6Bits): TMD6;
begin
Result := FileMD6(FileName,MD6Settings(HashBits));
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

Function MD6_Init(HashBits: Integer = DefaultMD6Bits): TMD6Context;
begin
Result := MD6_Init(MD6Settings(HashBits));
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
    FreeAndNil(TMD6Hash(Context));
  end
else raise EMD6InvalidState.Create('MD6_Final: MD6 context not initialized.');
end;

//------------------------------------------------------------------------------

Function MD6_Hash(const Buffer; Size: TMemSize; HashBits: Integer = DefaultMD6Bits): TMD6;
var
  Hash: TMD6Hash;
begin
Hash := TMD6Hash.Create;
try
  Hash.SetupHashBits(HashBits);
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.MD6;
finally
  Hash.Free;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                               Unit initialization
--------------------------------------------------------------------------------
===============================================================================}

procedure Initialize;
begin
{$IFDEF VectorCompression}
with TSimpleCPUID.Create do
try
  VAR_SSE3Supported := Info.SupportedExtensions.SSE3;
finally
  Free;
end;
{$ENDIF}
end;

initialization
  Initialize;

end.
