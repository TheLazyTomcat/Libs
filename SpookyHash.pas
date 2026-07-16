{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Spooky Hash (by Robert John Jenkins)

    Pascal implementation of Spooky hash in both currently existing versions
    (V1 and V2). Variants producing 32bit, 64bit and 128bit hashes are all
    provided.

  Version 1.0 (2026-07-11)

  Last change 2026-07-11

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SpookyHash

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    HashBase    - github.com/TheLazyTomcat/Lib.HashBase
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    ListUtils          - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit SpookyHash;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
  {$DEFINE CanInline}
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

interface

uses
  Classes,
  Auxtypes, HashBase;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESHException = class(EHashException);

  ESHIncompatibleClass = class(ESHException);
  ESHProcessingError   = class(ESHException);

{===============================================================================
    Common types and constants
===============================================================================}
type
  TSHUInt128 = packed record
  {$IFDEF ENDIAN_BIG}
    Hi,Lo:  UInt64;
  {$ELSE}
    Lo,Hi:  UInt64;
  {$ENDIF}
  end;

//------------------------------------------------------------------------------
{
  Bytes in TSpooky[b] are, in memory, always ordered from least significant
  byte to most significant byte (little endian).

  Type TSpooky[b]Sys has no such guarantee and its endianness is system and
  implementation-dependent.

  To convert the hash in default ordering to a required specific ordering,
  use method Spooky[b]ToLE for little endian and Spooky[b]ToBE for big endian.
}
type
  TSpooky32 = packed array[0..3] of UInt8;
  PSpooky32 = ^TSpooky32;

  TSpooky32Sys = UInt32;
  PSpooky32Sys = ^TSpooky32Sys;

  TSpooky64 = packed array[0..7] of UInt8;
  PSpooky64 = ^TSpooky64;

  TSpooky64Sys = UInt64;
  PSpooky64Sys = ^TSpooky64Sys;

  TSpooky128 = packed array[0..15] of UInt8;
  PSpooky128 = ^TSpooky128;

  TSpooky128Sys = TSHUInt128;
  PSpooky128ys = ^TSpooky128Sys;

const
  InitialSpooky32:  TSpooky32  = ($00,$00,$00,$00);
  InitialSpooky64:  TSpooky64  = ($00,$00,$00,$00,$00,$00,$00,$00);
  InitialSpooky128: TSpooky128 = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

  ZeroSpooky32:  TSpooky32  = (0,0,0,0);
  ZeroSpooky64:  TSpooky64  = (0,0,0,0,0,0,0,0);
  ZeroSpooky128: TSpooky128 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                 TSpookyHashBase
--------------------------------------------------------------------------------
===============================================================================}
type
  TSpookyInternalState = array[0..11] of UInt64;
  
{===============================================================================
    TSpookyHashBase - class declaration
===============================================================================}
type
  TSpookyHashBase = class(TBlockHash)
  protected
    fSeed:        TSpooky128Sys;
    fState:       TSpookyInternalState;
    fSpookyValue: TSpooky128Sys;
    fFceShort:    procedure(const Buffer; Size: TMemSize; var Hash: TSHUInt128);
    fFceInit:     procedure(out State: TSpookyInternalState; const Seed: TSpooky128Sys);
    fFceUpdate:   procedure(var State: TSpookyInternalState; const Buffer);
    fFceFinal:    procedure(var State: TSpookyInternalState; const Buffer; Size: TMemSize);
    procedure VersionSelect(Version: Integer); virtual;
    procedure ProcessFirst(const Block); override;
    procedure ProcessBlock(const Block); override;
    procedure ProcessLast; override;
    procedure Initialize; override;
    class Function SpookyToSys(Hash: TSpooky128): TSpooky128Sys; virtual;
    class Function SpookyFromSys(Hash: TSpooky128Sys): TSpooky128; virtual;
  public
    class Function HashType: THashType; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
  {
    Only overload accepting THashBase descendant can be used for continuous
    hashing, other overloads do not get necessary state.
  }
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    procedure Init; override;
  {$IFDEF Debug}
    property State: TSpookyInternalState read fState write fState;
  {$ENDIF}
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TSpooky32HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky32HashBase - class declaration
===============================================================================}
type
  TSpooky32HashBase = class(TSpookyHashBase)
  protected
    Function GetSeed32: TSpooky32; virtual;
    procedure SetSeed32(const Value: TSpooky32); virtual;
    Function GetSeed32Sys: TSpooky32Sys; virtual;
    procedure SetSeed32Sys(const Value: TSpooky32Sys); virtual;
    Function GetSpooky32: TSpooky32; virtual;
    Function GetSpooky32Sys: TSpooky32Sys; virtual;
    procedure SetSpooky32Sys(const Value: TSpooky32Sys); virtual; // not a setter
  public
    class Function Spooky32ToSys(Hash: TSpooky32): TSpooky32Sys; virtual;
    class Function Spooky32FromSys(Hash: TSpooky32Sys): TSpooky32; virtual;
    class Function Spooky32ToLE(Hash: TSpooky32): TSpooky32; virtual;
    class Function Spooky32ToBE(Hash: TSpooky32): TSpooky32; virtual;
    class Function Spooky32FromLE(Hash: TSpooky32): TSpooky32; virtual;
    class Function Spooky32FromBE(Hash: TSpooky32): TSpooky32; virtual;
    class Function HashSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: TSpooky32); overload; virtual;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSpooky32); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Seed32: TSpooky32 read GetSeed32 write SetSeed32;
    property Seed32Sys: TSpooky32Sys read GetSeed32Sys write SetSeed32Sys;
    property Spooky32: TSpooky32 read GetSpooky32;
    property Spooky32Sys: TSpooky32Sys read GetSpooky32Sys;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TSpooky32V1Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky32V1Hash - class declaration
===============================================================================}
type
  TSpooky32V1Hash = class(TSpooky32HashBase)
  protected
    procedure Initialize; override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TSpooky32V2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky32V2Hash - class declaration
===============================================================================}
type
  TSpooky32V2Hash = class(TSpooky32HashBase)
  protected
    procedure Initialize; override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TSpooky64HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky64HashBase - class declaration
===============================================================================}
type
  TSpooky64HashBase = class(TSpookyHashBase)
  protected
    Function GetSeed64: TSpooky64; virtual;
    procedure SetSeed64(const Value: TSpooky64); virtual;
    Function GetSeed64Sys: TSpooky64Sys; virtual;
    procedure SetSeed64Sys(const Value: TSpooky64Sys); virtual;
    Function GetSpooky64: TSpooky64; virtual;
    Function GetSpooky64Sys: TSpooky64Sys; virtual;
    procedure SetSpooky64Sys(const Value: TSpooky64Sys); virtual;
  public
    class Function Spooky64ToSys(Hash: TSpooky64): TSpooky64Sys; virtual;
    class Function Spooky64FromSys(Hash: TSpooky64Sys): TSpooky64; virtual;
    class Function Spooky64ToLE(Hash: TSpooky64): TSpooky64; virtual;
    class Function Spooky64ToBE(Hash: TSpooky64): TSpooky64; virtual;
    class Function Spooky64FromLE(Hash: TSpooky64): TSpooky64; virtual;
    class Function Spooky64FromBE(Hash: TSpooky64): TSpooky64; virtual;
    class Function HashSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: TSpooky64); overload; virtual;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSpooky64); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Seed64: TSpooky64 read GetSeed64 write SetSeed64;
    property Seed64Sys: TSpooky64Sys read GetSeed64Sys write SetSeed64Sys;
    property Spooky64: TSpooky64 read GetSpooky64;
    property Spooky64Sys: TSpooky64Sys read GetSpooky64Sys;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TSpooky64V1Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky64V1Hash - class declaration
===============================================================================}
type
  TSpooky64V1Hash = class(TSpooky64HashBase)
  protected
    procedure Initialize; override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TSpooky64V2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky64V2Hash - class declaration
===============================================================================}
type
  TSpooky64V2Hash = class(TSpooky64HashBase)
  protected
    procedure Initialize; override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TSpooky128HashBase                                                               
--------------------------------------------------------------------------------
===============================================================================}
{
  Note that the reference C++ implementation usually shows resulting hash
  as two separate 64bit values, and in the order they are passed to processing
  function, meaning low-order qword first.
  Here, the 128bit hash is taken as single quantity, and textual representation
  corresponds to that - high order qword is to the left in the string. Be aware
  of that if you try to compare results produced by this library with other
  implementations.
}
{===============================================================================
    TSpooky128HashBase - class declaration
===============================================================================}
type
  TSpooky128HashBase = class(TSpookyHashBase)
  protected
    Function GetSeed128: TSpooky128; virtual;
    procedure SetSeed128(const Value: TSpooky128); virtual;
    Function GetSpooky128: TSpooky128; virtual;
  public
    class Function Spooky128ToSys(Hash: TSpooky128): TSpooky128Sys; virtual;
    class Function Spooky128FromSys(Hash: TSpooky128Sys): TSpooky128; virtual;
    class Function Spooky128ToLE(Hash: TSpooky128): TSpooky128; virtual;
    class Function Spooky128ToBE(Hash: TSpooky128): TSpooky128; virtual;
    class Function Spooky128FromLE(Hash: TSpooky128): TSpooky128; virtual;
    class Function Spooky128FromBE(Hash: TSpooky128): TSpooky128; virtual;
    class Function HashSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: TSpooky128); overload; virtual;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSpooky128); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Seed128: TSpooky128 read GetSeed128 write SetSeed128;
    property Seed128Sys: TSpooky128Sys read fSeed write fSeed;
    property Spooky128: TSpooky128 read GetSpooky128;
    property Spooky128Sys: TSpooky128Sys read fSpookyValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TSpooky128V1Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky128V1Hash - class declaration
===============================================================================}
type
  TSpooky128V1Hash = class(TSpooky128HashBase)
  protected
    procedure Initialize; override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TSpooky128V2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky128V2Hash - class declaration
===============================================================================}
type
  TSpooky128V2Hash = class(TSpooky128HashBase)
  protected
    procedure Initialize; override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{
  This state is used when doing continuous hashing using BufferSpooky* and
  LastBufferSpooky* functions as it is not enough to transfer only the hash
  from previous step.
  All values are stored with system endianness, so if you need to transfer
  them to a machine with differing endianness, make sure you do all necessary
  corrections.
}
type
  TSpookyState = record
    InternalState:  TSpookyInternalState;
    ProcessedBytes: TMemSize;
  end;

{===============================================================================
    Common 32bit procedural interface - declaration
===============================================================================}

Function Spooky32ToStr(const Hash: TSpooky32): String;
Function StrToSpooky32(const Str: String): TSpooky32;
Function TryStrToSpooky32(const Str: String; out Hash: TSpooky32): Boolean;
Function StrToSpooky32Def(const Str: String; const Default: TSpooky32): TSpooky32;

Function CompareSpooky32(const A,B: TSpooky32): Integer;
Function SameSpooky32(const A,B: TSpooky32): Boolean;

//------------------------------------------------------------------------------
type
  TSpooky32Context = type Pointer;

{===============================================================================
    Common 64bit procedural interface - declaration
===============================================================================}

Function Spooky64ToStr(const Hash: TSpooky64): String;
Function StrToSpooky64(const Str: String): TSpooky64;
Function TryStrToSpooky64(const Str: String; out Hash: TSpooky64): Boolean;
Function StrToSpooky64Def(const Str: String; const Default: TSpooky64): TSpooky64;

Function CompareSpooky64(const A,B: TSpooky64): Integer;
Function SameSpooky64(const A,B: TSpooky64): Boolean;

//------------------------------------------------------------------------------
type
  TSpooky64Context = type Pointer;

{===============================================================================
    Common 128bit procedural interface - declaration
===============================================================================}

Function Spooky128ToStr(const Hash: TSpooky128): String;
Function StrToSpooky128(const Str: String): TSpooky128;
Function TryStrToSpooky128(const Str: String; out Hash: TSpooky128): Boolean;
Function StrToSpooky128Def(const Str: String; const Default: TSpooky128): TSpooky128;

Function CompareSpooky128(const A,B: TSpooky128): Integer;
Function SameSpooky128(const A,B: TSpooky128): Boolean;

Function SeedSpooky128(SeedLo,SeedHi: UInt64): TSpooky128; overload;
Function SeedSpooky128(Seed: UInt64): TSpooky128; overload;

//------------------------------------------------------------------------------
type
  TSpooky128Context = type Pointer;

{===============================================================================
    SpookyV1 32bit procedural interface - declaration
===============================================================================}
{
  Following four functions are meant to be used when hashing continuous data
  in situation where the whole process cannot happen using the same processing
  object or context.

  At first, initialize the state using InitialSpooky32V1State, possibly with
  a seed if needed.

  Then repeatedly call BufferSpooky32V1 (overload accepting state) while
  providing it with the state and blocks of data.

    WARNING - size of the blocks must be always a multiple of value returned
              by BlockSizeSpooky32V1 (192), otherwise an ESHProcessingError
              will be raised. This limitation is here to remove a need for
              partial block transfer.

  Lastly, call LastBufferSpooky32V1 - it will finalize the processing and
  return a final hash. You can pass buffer of any size here, including size
  of zero.
}
Function BlockSizeSpooky32V1: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function InitialStateSpooky32V1(const Seed: TSpooky32): TSpookyState; overload;
Function InitialStateSpooky32V1: TSpookyState; overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure BufferSpooky32V1(var State: TSpookyState; const Buffer; Size: TMemSize); overload;
Function LastBufferSpooky32V1(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky32;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky32V1(const Seed: TSpooky32; const Buffer; Size: TMemSize): TSpooky32; overload;
Function BufferSpooky32V1(const Buffer; Size: TMemSize): TSpooky32; overload;

Function AnsiStringSpooky32V1(const Str: AnsiString): TSpooky32;
Function WideStringSpooky32V1(const Str: WideString): TSpooky32;
Function StringSpooky32V1(const Str: String): TSpooky32;

Function StreamSpooky32V1(Stream: TStream; Count: Int64 = -1): TSpooky32;
Function FileSpooky32V1(const FileName: String): TSpooky32;

//------------------------------------------------------------------------------

Function Spooky32V1_Init: TSpooky32Context; overload;
Function Spooky32V1_Init(const Seed: TSpooky32): TSpooky32Context; overload;
procedure Spooky32V1_Update(const Context: TSpooky32Context; const Buffer; Size: TMemSize);
Function Spooky32V1_Final(var Context: TSpooky32Context; const Buffer; Size: TMemSize): TSpooky32; overload;
Function Spooky32V1_Final(var Context: TSpooky32Context): TSpooky32; overload;
Function Spooky32V1_Hash(const Buffer; Size: TMemSize): TSpooky32;

{===============================================================================
    SpookyV2 32bit procedural interface - declaration
===============================================================================}

Function BlockSizeSpooky32V2: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function InitialStateSpooky32V2(const Seed: TSpooky32): TSpookyState; overload;
Function InitialStateSpooky32V2: TSpookyState; overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure BufferSpooky32V2(var State: TSpookyState; const Buffer; Size: TMemSize); overload;
Function LastBufferSpooky32V2(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky32;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky32V2(const Seed: TSpooky32; const Buffer; Size: TMemSize): TSpooky32; overload;
Function BufferSpooky32V2(const Buffer; Size: TMemSize): TSpooky32; overload;

Function AnsiStringSpooky32V2(const Str: AnsiString): TSpooky32;
Function WideStringSpooky32V2(const Str: WideString): TSpooky32;
Function StringSpooky32V2(const Str: String): TSpooky32;

Function StreamSpooky32V2(Stream: TStream; Count: Int64 = -1): TSpooky32;
Function FileSpooky32V2(const FileName: String): TSpooky32;

//------------------------------------------------------------------------------

Function Spooky32V2_Init: TSpooky32Context; overload;
Function Spooky32V2_Init(const Seed: TSpooky32): TSpooky32Context; overload;
procedure Spooky32V2_Update(const Context: TSpooky32Context; const Buffer; Size: TMemSize);
Function Spooky32V2_Final(var Context: TSpooky32Context; const Buffer; Size: TMemSize): TSpooky32; overload;
Function Spooky32V2_Final(var Context: TSpooky32Context): TSpooky32; overload;
Function Spooky32V2_Hash(const Buffer; Size: TMemSize): TSpooky32;

{===============================================================================
    SpookyV1 64bit procedural interface - declaration
===============================================================================}

Function BlockSizeSpooky64V1: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function InitialStateSpooky64V1(const Seed: TSpooky64): TSpookyState; overload;
Function InitialStateSpooky64V1: TSpookyState; overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure BufferSpooky64V1(var State: TSpookyState; const Buffer; Size: TMemSize); overload;
Function LastBufferSpooky64V1(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky64;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky64V1(const Seed: TSpooky64; const Buffer; Size: TMemSize): TSpooky64; overload;
Function BufferSpooky64V1(const Buffer; Size: TMemSize): TSpooky64; overload;

Function AnsiStringSpooky64V1(const Str: AnsiString): TSpooky64;
Function WideStringSpooky64V1(const Str: WideString): TSpooky64;
Function StringSpooky64V1(const Str: String): TSpooky64;

Function StreamSpooky64V1(Stream: TStream; Count: Int64 = -1): TSpooky64;
Function FileSpooky64V1(const FileName: String): TSpooky64;

//------------------------------------------------------------------------------

Function Spooky64V1_Init: TSpooky64Context; overload;
Function Spooky64V1_Init(const Seed: TSpooky64): TSpooky64Context; overload;
procedure Spooky64V1_Update(const Context: TSpooky64Context; const Buffer; Size: TMemSize);
Function Spooky64V1_Final(var Context: TSpooky64Context; const Buffer; Size: TMemSize): TSpooky64; overload;
Function Spooky64V1_Final(var Context: TSpooky64Context): TSpooky64; overload;
Function Spooky64V1_Hash(const Buffer; Size: TMemSize): TSpooky64;

{===============================================================================
    SpookyV2 64bit procedural interface - declaration
===============================================================================}

Function BlockSizeSpooky64V2: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function InitialStateSpooky64V2(const Seed: TSpooky64): TSpookyState; overload;
Function InitialStateSpooky64V2: TSpookyState; overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure BufferSpooky64V2(var State: TSpookyState; const Buffer; Size: TMemSize); overload;
Function LastBufferSpooky64V2(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky64;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky64V2(const Seed: TSpooky64; const Buffer; Size: TMemSize): TSpooky64; overload;
Function BufferSpooky64V2(const Buffer; Size: TMemSize): TSpooky64; overload;

Function AnsiStringSpooky64V2(const Str: AnsiString): TSpooky64;
Function WideStringSpooky64V2(const Str: WideString): TSpooky64;
Function StringSpooky64V2(const Str: String): TSpooky64;

Function StreamSpooky64V2(Stream: TStream; Count: Int64 = -1): TSpooky64;
Function FileSpooky64V2(const FileName: String): TSpooky64;

//------------------------------------------------------------------------------

Function Spooky64V2_Init: TSpooky64Context; overload;
Function Spooky64V2_Init(const Seed: TSpooky64): TSpooky64Context; overload;
procedure Spooky64V2_Update(const Context: TSpooky64Context; const Buffer; Size: TMemSize);
Function Spooky64V2_Final(var Context: TSpooky64Context; const Buffer; Size: TMemSize): TSpooky64; overload;
Function Spooky64V2_Final(var Context: TSpooky64Context): TSpooky64; overload;
Function Spooky64V2_Hash(const Buffer; Size: TMemSize): TSpooky64;

{===============================================================================
    SpookyV1 128bit procedural interface - declaration
===============================================================================}

Function BlockSizeSpooky128V1: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function InitialStateSpooky128V1(const Seed: TSpooky128): TSpookyState; overload;
Function InitialStateSpooky128V1: TSpookyState; overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure BufferSpooky128V1(var State: TSpookyState; const Buffer; Size: TMemSize); overload;
Function LastBufferSpooky128V1(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky128;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky128V1(const Seed: TSpooky128; const Buffer; Size: TMemSize): TSpooky128; overload;
Function BufferSpooky128V1(const Buffer; Size: TMemSize): TSpooky128; overload;

Function AnsiStringSpooky128V1(const Str: AnsiString): TSpooky128;
Function WideStringSpooky128V1(const Str: WideString): TSpooky128;
Function StringSpooky128V1(const Str: String): TSpooky128;

Function StreamSpooky128V1(Stream: TStream; Count: Int64 = -1): TSpooky128;
Function FileSpooky128V1(const FileName: String): TSpooky128;

//------------------------------------------------------------------------------

Function Spooky128V1_Init: TSpooky128Context; overload;
Function Spooky128V1_Init(const Seed: TSpooky128): TSpooky128Context; overload;
procedure Spooky128V1_Update(const Context: TSpooky128Context; const Buffer; Size: TMemSize);
Function Spooky128V1_Final(var Context: TSpooky128Context; const Buffer; Size: TMemSize): TSpooky128; overload;
Function Spooky128V1_Final(var Context: TSpooky128Context): TSpooky128; overload;
Function Spooky128V1_Hash(const Buffer; Size: TMemSize): TSpooky128;

{===============================================================================
    SpookyV2 128bit procedural interface - declaration
===============================================================================}

Function BlockSizeSpooky128V2: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function InitialStateSpooky128V2(const Seed: TSpooky128): TSpookyState; overload;
Function InitialStateSpooky128V2: TSpookyState; overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure BufferSpooky128V2(var State: TSpookyState; const Buffer; Size: TMemSize); overload;
Function LastBufferSpooky128V2(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky128;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky128V2(const Seed: TSpooky128; const Buffer; Size: TMemSize): TSpooky128; overload;
Function BufferSpooky128V2(const Buffer; Size: TMemSize): TSpooky128; overload;

Function AnsiStringSpooky128V2(const Str: AnsiString): TSpooky128;
Function WideStringSpooky128V2(const Str: WideString): TSpooky128;
Function StringSpooky128V2(const Str: String): TSpooky128;

Function StreamSpooky128V2(Stream: TStream; Count: Int64 = -1): TSpooky128;
Function FileSpooky128V2(const FileName: String): TSpooky128;

//------------------------------------------------------------------------------

Function Spooky128V2_Init: TSpooky128Context; overload;
Function Spooky128V2_Init(const Seed: TSpooky128): TSpooky128Context; overload;
procedure Spooky128V2_Update(const Context: TSpooky128Context; const Buffer; Size: TMemSize);
Function Spooky128V2_Final(var Context: TSpooky128Context; const Buffer; Size: TMemSize): TSpooky128; overload;
Function Spooky128V2_Final(var Context: TSpooky128Context): TSpooky128; overload;
Function Spooky128V2_Hash(const Buffer; Size: TMemSize): TSpooky128;

implementation

uses
  SysUtils,
  UInt64Utils;

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ELSE}
  {$UNDEF OverflowChecks}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE RangeChecks}
{$ELSE}
  {$UNDEF RangeChecks}
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                               Main implementation                              
--------------------------------------------------------------------------------
===============================================================================}
const
  SC_CONST   = UInt64($DEADBEEFDEADBEEF);
  SC_BLOCKSZ = 192;

type
  TShortReadBuffer = packed array[0..3] of UInt64;
  PShortReadBuffer = ^TShortReadBuffer;

  TLongReadBuffer = packed record case Integer of
    0: (Words: packed array[0..11] of UInt64);
    1: (Bytes: packed array[0..95] of UInt8);
  end;
  PLongReadBuffer = ^TLongReadBuffer;

//==============================================================================

Function SwapEndian(Hash: UInt32): UInt32; overload;
begin
Result := UInt32(((Hash and $000000FF) shl 24) or ((Hash and $0000FF00) shl 8) or
                 ((Hash and $00FF0000) shr 8) or ((Hash and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: UInt64): UInt64; overload;
begin
UInt64Rec(Result).Hi := SwapEndian(UInt64Rec(Hash).Lo);
UInt64Rec(Result).Lo := SwapEndian(UInt64Rec(Hash).Hi);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TSHUInt128): TSHUInt128; overload;
begin
Result.Hi := SwapEndian(Hash.Lo);
Result.Lo := SwapEndian(Hash.Hi);
end;

//==============================================================================
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function Rot64(const Value: UInt64; Shift: Integer): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := UInt64(Value shl Shift) or (Value shr (64 - Shift));
end;

//------------------------------------------------------------------------------

procedure SpookyV1ShortMix(var H0,H1,H2,H3: UInt64);
begin
H2 := Rot64(H2,50);   H2 := H2 + H3;    H0 := H0 xor H2;
H3 := Rot64(H3,52);   H3 := H3 + H0;    H1 := H1 xor H3;
H0 := Rot64(H0,30);   H0 := H0 + H1;    H2 := H2 xor H0;
H1 := Rot64(H1,41);   H1 := H1 + H2;    H3 := H3 xor H1;
H2 := Rot64(H2,54);   H2 := H2 + H3;    H0 := H0 xor H2;
H3 := Rot64(H3,48);   H3 := H3 + H0;    H1 := H1 xor H3;
H0 := Rot64(H0,38);   H0 := H0 + H1;    H2 := H2 xor H0;
H1 := Rot64(H1,37);   H1 := H1 + H2;    H3 := H3 xor H1;
H2 := Rot64(H2,62);   H2 := H2 + H3;    H0 := H0 xor H2; 
H3 := Rot64(H3,34);   H3 := H3 + H0;    H1 := H1 xor H3;
H0 := Rot64(H0, 5);   H0 := H0 + H1;    H2 := H2 xor H0;
H1 := Rot64(H1,36);   H1 := H1 + H2;    H3 := H3 xor H1;
end;

//------------------------------------------------------------------------------

procedure SpookyV1ShortEnd(var H0,H1,H2,H3: UInt64);
begin
H3 := H3 xor H2;    H2 := Rot64(H2,15);     H3 := H3 + H2;
H0 := H0 xor H3;    H3 := Rot64(H3,52);     H0 := H0 + H3;
H1 := H1 xor H0;    H0 := Rot64(H0,26);     H1 := H1 + H0;
H2 := H2 xor H1;    H1 := Rot64(H1,51);     H2 := H2 + H1;
H3 := H3 xor H2;    H2 := Rot64(H2,28);     H3 := H3 + H2;
H0 := H0 xor H3;    H3 := Rot64(H3, 9);     H0 := H0 + H3;
H1 := H1 xor H0;    H0 := Rot64(H0,47);     H1 := H1 + H0;
H2 := H2 xor H1;    H1 := Rot64(H1,54);     H2 := H2 + H1;
H3 := H3 xor H2;    H2 := Rot64(H2,32);     H3 := H3 + H2;
H0 := H0 xor H3;    H3 := Rot64(H3,25);     H0 := H0 + H3;
H1 := H1 xor H0;    H0 := Rot64(H0,63);     H1 := H1 + H0;
end;

//------------------------------------------------------------------------------

procedure SpookyV1Short(const Buffer; Size: TMemSize; var Hash: TSHUInt128);
var
  A,B,C,D:      UInt64;
  CurrentData:  PShortReadBuffer;
  Remaining:    TMemSize;
  ReadBuffer:   TShortReadBuffer;
begin
A := Hash.Lo;
B := Hash.Hi;
C := SC_CONST;
D := SC_CONST;
CurrentData := @Buffer;
Remaining := Size;
while Remaining >= 32 do begin
  C := C + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[0]);
  D := D + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[1]);
  SpookyV1ShortMix(A,B,C,D);
  A := A + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[2]);
  B := B + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[3]);
  Inc(CurrentData);
  Dec(Remaining,32);
end;
while Remaining >= 16 do begin
  C := C + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[0]);
  D := D + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[1]);
  SpookyV1ShortMix(A,B,C,D);
  Inc(PUInt64(CurrentData),2);
  Dec(Remaining,16);
end;
D := UInt64(Size) shl 56;
If Remaining > 0 then
  begin
    FillChar(Addr(ReadBuffer)^,SizeOf(TShortReadBuffer),0);
    Move(CurrentData^,ReadBuffer,Remaining);
    C := C + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer[0]);
    D := D + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer[1]);
  end
else
  begin
    C := C + SC_CONST;
    D := D + SC_CONST;
  end;
SpookyV1ShortEnd(A,B,C,D);
Hash.Lo := A;
Hash.Hi := B;
end;

//==============================================================================

procedure SpookyV1Mix(var S: TSpookyInternalState; Data: PLongReadBuffer);

  Function GetWord(const X: UInt64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
  begin
    Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(X);
  end;

begin
S[0] := S[0] + GetWord(Data^.Words[0]);     S[2] := S[2] xor S[10];   S[11] := S[11] xor S[0];  S[0] := Rot64(S[0],11);   S[11] := S[11] + S[1];
S[1] := S[1] + GetWord(Data^.Words[1]);     S[3] := S[3] xor S[11];   S[0] := S[0] xor S[1];    S[1] := Rot64(S[1],32);   S[0] := S[0] + S[2];
S[2] := S[2] + GetWord(Data^.Words[2]);     S[4] := S[4] xor S[0];    S[1] := S[1] xor S[2];    S[2] := Rot64(S[2],43);   S[1] := S[1] + S[3];
S[3] := S[3] + GetWord(Data^.Words[3]);     S[5] := S[5] xor S[1];    S[2] := S[2] xor S[3];    S[3] := Rot64(S[3],31);   S[2] := S[2] + S[4];
S[4] := S[4] + GetWord(Data^.Words[4]);     S[6] := S[6] xor S[2];    S[3] := S[3] xor S[4];    S[4] := Rot64(S[4],17);   S[3] := S[3] + S[5];
S[5] := S[5] + GetWord(Data^.Words[5]);     S[7] := S[7] xor S[3];    S[4] := S[4] xor S[5];    S[5] := Rot64(S[5],28);   S[4] := S[4] + S[6];
S[6] := S[6] + GetWord(Data^.Words[6]);     S[8] := S[8] xor S[4];    S[5] := S[5] xor S[6];    S[6] := Rot64(S[6],39);   S[5] := S[5] + S[7];
S[7] := S[7] + GetWord(Data^.Words[7]);     S[9] := S[9] xor S[5];    S[6] := S[6] xor S[7];    S[7] := Rot64(S[7],57);   S[6] := S[6] + S[8];
S[8] := S[8] + GetWord(Data^.Words[8]);     S[10] := S[10] xor S[6];  S[7] := S[7] xor S[8];    S[8] := Rot64(S[8],55);   S[7] := S[7] + S[9];
S[9] := S[9] + GetWord(Data^.Words[9]);     S[11] := S[11] xor S[7];  S[8] := S[8] xor S[9];    S[9] := Rot64(S[9],54);   S[8] := S[8] + S[10];
S[10] := S[10] + GetWord(Data^.Words[10]);  S[0] := S[0] xor S[8];    S[9] := S[9] xor S[10];   S[10] := Rot64(S[10],22); S[9] := S[9] + S[11];
S[11] := S[11] + GetWord(Data^.Words[11]);  S[1] := S[1] xor S[9];    S[10] := S[10] xor S[11]; S[11] := Rot64(S[11],46); S[10] := S[10] + S[0];
end;

//------------------------------------------------------------------------------

procedure SpookyV1End(var S: TSpookyInternalState);

  procedure EndPartial;
  begin
    S[11] := S[11] + S[1];    S[2] := S[2] xor S[11];   S[1] := Rot64(S[1],44);
    S[0] := S[0] + S[2];      S[3] := S[3] xor S[0];    S[2] := Rot64(S[2],15);
    S[1] := S[1] + S[3];      S[4] := S[4] xor S[1];    S[3] := Rot64(S[3],34);
    S[2] := S[2] + S[4];      S[5] := S[5] xor S[2];    S[4] := Rot64(S[4],21);
    S[3] := S[3] + S[5];      S[6] := S[6] xor S[3];    S[5] := Rot64(S[5],38);
    S[4] := S[4] + S[6];      S[7] := S[7] xor S[4];    S[6] := Rot64(S[6],33);
    S[5] := S[5] + S[7];      S[8] := S[8] xor S[5];    S[7] := Rot64(S[7],10);
    S[6] := S[6] + S[8];      S[9] := S[9] xor S[6];    S[8] := Rot64(S[8],13);
    S[7] := S[7] + S[9];      S[10] := S[10] xor S[7];  S[9] := Rot64(S[9],38); 
    S[8] := S[8] + S[10];     S[11] := S[11] xor S[8];  S[10] := Rot64(S[10],53);
    S[9] := S[9] + S[11];     S[0] := S[0] xor S[9];    S[11] := Rot64(S[11],42);
    S[10] := S[10] + S[0];    S[1] := S[1] xor S[10];   S[0] := Rot64(S[0],54);
  end;

begin
EndPartial;
EndPartial;
EndPartial;
end;

//------------------------------------------------------------------------------

procedure SpookyV1Init(out State: TSpookyInternalState; const Seed: TSpooky128Sys);
begin
State[0] := Seed.Lo;
State[1] := Seed.Hi;
State[2] := SC_CONST;
State[3] := State[0];
State[4] := State[1];
State[5] := SC_CONST;
State[6] := State[0];
State[7] := State[1];
State[8] := SC_CONST;
State[9] := State[0];
State[10] := State[1];
State[11] := SC_CONST;
end;

//------------------------------------------------------------------------------

procedure SpookyV1Update(var State: TSpookyInternalState; const Buffer);
var
  CurrentData:  PLongReadBuffer;
begin
// this always processes one block (192 bytes), ie. two mix buffers
CurrentData := @Buffer;
SpookyV1Mix(State,CurrentData);
Inc(CurrentData);
SpookyV1Mix(State,CurrentData);
end;

//------------------------------------------------------------------------------

procedure SpookyV1Final(var State: TSpookyInternalState; const Buffer; Size: TMemSize);
var
  CurrentData:  PLongReadBuffer;
  ReadBuffer:   TLongReadBuffer;
begin
CurrentData := @Buffer;
while Size >= SizeOf(TLongReadBuffer) do
  begin
    SpookyV1Mix(State,CurrentData);
    Inc(CurrentData);
    Dec(Size,SizeOf(TLongReadBuffer));
  end;
FillChar(Addr(ReadBuffer)^,SizeOf(TLongReadBuffer),0);
If Size > 0 then
  Move(CurrentData^,ReadBuffer,Size);
ReadBuffer.Bytes[High(ReadBuffer.Bytes)] := UInt8(Size);
SpookyV1Mix(State,@ReadBuffer);
SpookyV1End(State);
end;

//==============================================================================

procedure SpookyV2Short(const Buffer; Size: TMemSize; var Hash: TSHUInt128);
var
  A,B,C,D:      UInt64;
  CurrentData:  PShortReadBuffer;
  Remaining:    TMemSize;
  ReadBuffer:   TShortReadBuffer;
begin
A := Hash.Lo;
B := Hash.Hi;
C := SC_CONST;
D := SC_CONST;
CurrentData := @Buffer;
Remaining := Size;
while Remaining >= 32 do begin
  C := C + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[0]);
  D := D + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[1]);
  SpookyV1ShortMix(A,B,C,D);
  A := A + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[2]);
  B := B + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[3]);
  Inc(CurrentData);
  Dec(Remaining,32);
end;
while Remaining >= 16 do begin
  C := C + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[0]);
  D := D + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^[1]);
  SpookyV1ShortMix(A,B,C,D);
  Inc(PUInt64(CurrentData),2);
  Dec(Remaining,16);
end;
D := D + (UInt64(Size) shl 56);
If Remaining > 0 then
  begin
    FillChar(Addr(ReadBuffer)^,SizeOf(TShortReadBuffer),0);
    Move(CurrentData^,ReadBuffer,Remaining);
    C := C + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer[0]);
    D := D + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer[1]);
  end
else
  begin
    C := C + SC_CONST;
    D := D + SC_CONST;
  end;
SpookyV1ShortEnd(A,B,C,D);
Hash.Lo := A;
Hash.Hi := B;
end;

//==============================================================================

procedure SpookyV2End(var S: TSpookyInternalState; Data: PLongReadBuffer);

  procedure EndPartial;
  begin
    S[11] := S[11] + S[1];    S[2] := S[2] xor S[11];   S[1] := Rot64(S[1],44);
    S[0] := S[0] + S[2];      S[3] := S[3] xor S[0];    S[2] := Rot64(S[2],15);
    S[1] := S[1] + S[3];      S[4] := S[4] xor S[1];    S[3] := Rot64(S[3],34);
    S[2] := S[2] + S[4];      S[5] := S[5] xor S[2];    S[4] := Rot64(S[4],21);
    S[3] := S[3] + S[5];      S[6] := S[6] xor S[3];    S[5] := Rot64(S[5],38);
    S[4] := S[4] + S[6];      S[7] := S[7] xor S[4];    S[6] := Rot64(S[6],33);
    S[5] := S[5] + S[7];      S[8] := S[8] xor S[5];    S[7] := Rot64(S[7],10);
    S[6] := S[6] + S[8];      S[9] := S[9] xor S[6];    S[8] := Rot64(S[8],13);
    S[7] := S[7] + S[9];      S[10] := S[10] xor S[7];  S[9] := Rot64(S[9],38); 
    S[8] := S[8] + S[10];     S[11] := S[11] xor S[8];  S[10] := Rot64(S[10],53);
    S[9] := S[9] + S[11];     S[0] := S[0] xor S[9];    S[11] := Rot64(S[11],42);
    S[10] := S[10] + S[0];    S[1] := S[1] xor S[10];   S[0] := Rot64(S[0],54);
  end;

  Function GetWord(const X: UInt64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
  begin
    Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(X);
  end;

begin
S[0]  := S[0]  + GetWord(Data^.Words[0]);   S[1]  := S[1]  + GetWord(Data^.Words[1]);
S[2]  := S[2]  + GetWord(Data^.Words[2]);   S[3]  := S[3]  + GetWord(Data^.Words[3]);
S[4]  := S[4]  + GetWord(Data^.Words[4]);   S[5]  := S[5]  + GetWord(Data^.Words[5]);
S[6]  := S[6]  + GetWord(Data^.Words[6]);   S[7]  := S[7]  + GetWord(Data^.Words[7]);
S[8]  := S[8]  + GetWord(Data^.Words[8]);   S[9]  := S[9]  + GetWord(Data^.Words[9]);
S[10] := S[10] + GetWord(Data^.Words[10]);  S[11] := S[11] + GetWord(Data^.Words[11]);
EndPartial;
EndPartial;
EndPartial;
end;

//------------------------------------------------------------------------------

procedure SpookyV2Final(var State: TSpookyInternalState; const Buffer; Size: TMemSize);
var
  CurrentData:  PLongReadBuffer;
  ReadBuffer:   TLongReadBuffer;
begin
CurrentData := @Buffer;
while Size >= SizeOf(TLongReadBuffer) do
  begin
    SpookyV1Mix(State,CurrentData);
    Inc(CurrentData);
    Dec(Size,SizeOf(TLongReadBuffer));
  end;
FillChar(Addr(ReadBuffer)^,SizeOf(TLongReadBuffer),0);
If Size > 0 then
  Move(CurrentData^,ReadBuffer,Size);
ReadBuffer.Bytes[High(ReadBuffer.Bytes)] := UInt8(Size);
SpookyV2End(State,@ReadBuffer);
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
--------------------------------------------------------------------------------
                                 TSpookyHashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpookyHashBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpookyHashBase - protected methods
-------------------------------------------------------------------------------}

procedure TSpookyHashBase.VersionSelect(Version: Integer);
begin
case Version of
  1:  begin
        fFceShort := SpookyV1Short;
        fFceInit := SpookyV1Init;
        fFceUpdate := SpookyV1Update;
        fFceFinal := SpookyV1Final;
      end;
  2:  begin
        fFceShort := SpookyV2Short;
        fFceInit := SpookyV1Init;
        fFceUpdate := SpookyV1Update;
        fFceFinal := SpookyV2Final;
      end;
else
  fFceShort := nil;
  fFceInit := nil;
  fFceUpdate := nil;
  fFceFinal := nil;
end;
end;

//------------------------------------------------------------------------------

procedure TSpookyHashBase.ProcessFirst(const Block);
begin
inherited;
ProcessBlock(Block);
end;

//------------------------------------------------------------------------------

procedure TSpookyHashBase.ProcessBlock(const Block);
begin
fFceUpdate(fState,Block);
end;

//------------------------------------------------------------------------------

procedure TSpookyHashBase.ProcessLast;
begin
If fProcessedBytes < fBlockSize then
  begin
    // short processing (less than 192 bytes)
    fSpookyValue.Lo := fState[0];
    fSpookyValue.Hi := fState[1];
    fFceShort(fTransBlock^,fTransCount,fSpookyValue);
   end
else
  begin
    // normal finalization
    fFceFinal(fState,fTransBlock^,fTransCount);
    fSpookyValue.Lo := fState[0];
    fSpookyValue.Hi := fState[1];
  end;
end;

//------------------------------------------------------------------------------

procedure TSpookyHashBase.Initialize;
begin
fBlockSize := SC_BLOCKSZ; // 192 bytes
inherited;
fSeed := SpookyToSys(InitialSpooky128);
FillChar(fState,SizeOf(TSpookyInternalState),0);
fSpookyValue := SpookyToSys(ZeroSpooky128);
end;

//------------------------------------------------------------------------------

class Function TSpookyHashBase.SpookyToSys(Hash: TSpooky128): TSpooky128Sys;
begin
Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TSpooky128Sys(Hash));
end;

//------------------------------------------------------------------------------

class Function TSpookyHashBase.SpookyFromSys(Hash: TSpooky128Sys): TSpooky128;
begin
Result := TSpooky128({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

{-------------------------------------------------------------------------------
    TSpookyHashBase - public methods
-------------------------------------------------------------------------------}

class Function TSpookyHashBase.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TSpookyHashBase.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TSpookyHashBase.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TSpookyHashBase.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSpookyHashBase then
  begin
    fSeed := TSpookyHashBase(Hash).fSeed;
    fState := TSpookyHashBase(Hash).fState;
    fSpookyValue := TSpookyHashBase(Hash).fSpookyValue;
  end
else raise ESHIncompatibleClass.CreateFmt('TSpooky32HashBase.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

procedure TSpookyHashBase.Init;
begin
inherited;
fSpookyValue := fSeed;
fFceInit(fState,fSpookyValue);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSpooky32HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky32HashBase - auxiliary routines
===============================================================================}

Function SwapEndian(Hash: TSpooky32): TSpooky32; overload;
begin
Result := TSpooky32(SwapEndian(TSpooky32Sys(Hash)));
end;

//==============================================================================

Function Spooky32Compare(const A,B: TSpooky32Sys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function Spooky32Same(const A,B: TSpooky32Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function Spooky32AsString(const Spooky32: TSpooky32Sys): String;
begin
Result := IntToHex(Spooky32,8);
end;

//------------------------------------------------------------------------------

Function Spooky32FromString(const Str: String): TSpooky32Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TSpooky32Sys(StrToInt(Str))
    else
      Result := TSpooky32Sys(StrToInt('$' + Str));
  end
else Result := TSpooky32HashBase.Spooky32ToSys(ZeroSpooky32);
end;

{===============================================================================
    TSpooky32HashBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpooky32HashBase - protected methods
-------------------------------------------------------------------------------}

Function TSpooky32HashBase.GetSeed32: TSpooky32;
begin
Result := Spooky32FromSys(GetSeed32Sys);
end;

//------------------------------------------------------------------------------

procedure TSpooky32HashBase.SetSeed32(const Value: TSpooky32);
begin
SetSeed32Sys(Spooky32ToSys(Value));
end;

//------------------------------------------------------------------------------

Function TSpooky32HashBase.GetSeed32Sys: TSpooky32Sys;
begin
Result := TSpooky32Sys(fSeed.Lo);
end;

//------------------------------------------------------------------------------

procedure TSpooky32HashBase.SetSeed32Sys(const Value: TSpooky32Sys);
begin
fSeed.Lo := UInt64(Value);
fSeed.Hi := UInt64(Value);
end;

//------------------------------------------------------------------------------

Function TSpooky32HashBase.GetSpooky32: TSpooky32;
begin
Result := Spooky32FromSys(GetSpooky32Sys);
end;

//------------------------------------------------------------------------------

Function TSpooky32HashBase.GetSpooky32Sys: TSpooky32Sys;
begin
Result := TSpooky32Sys(fSpookyValue.Lo);
end;

//------------------------------------------------------------------------------

procedure TSpooky32HashBase.SetSpooky32Sys(const Value: TSpooky32Sys);
begin
fSpookyValue.Lo := UInt64(Value);
fSpookyValue.Hi := 0
end;

{-------------------------------------------------------------------------------
    TSpooky32HashBase - public methods
-------------------------------------------------------------------------------}

class Function TSpooky32HashBase.Spooky32ToSys(Hash: TSpooky32): TSpooky32Sys;
begin
Result := TSpooky32Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSpooky32HashBase.Spooky32FromSys(Hash: TSpooky32Sys): TSpooky32;
begin
Result := TSpooky32({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSpooky32HashBase.Spooky32ToLE(Hash: TSpooky32): TSpooky32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSpooky32HashBase.Spooky32ToBE(Hash: TSpooky32): TSpooky32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSpooky32HashBase.Spooky32FromLE(Hash: TSpooky32): TSpooky32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSpooky32HashBase.Spooky32FromBE(Hash: TSpooky32): TSpooky32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSpooky32HashBase.HashSize: TMemSize;
begin
Result := SizeOf(TSpooky32);
end;

//------------------------------------------------------------------------------

constructor TSpooky32HashBase.CreateAndInitFrom(Hash: TSpooky32);
begin
CreateAndInit;
SetSpooky32Sys(Spooky32ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function TSpooky32HashBase.Compare(Hash: THashBase): Integer;
begin
If Hash is TSpooky32HashBase then
  Result := Spooky32Compare(GetSpooky32Sys,TSpooky32HashBase(Hash).Spooky32Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSpooky32HashBase.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSpooky32HashBase.Same(Hash: THashBase): Boolean;
begin
If Hash is TSpooky32HashBase then
  Result := Spooky32Same(GetSpooky32Sys,TSpooky32HashBase(Hash).Spooky32Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSpooky32HashBase.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSpooky32HashBase.AsString: String;
begin
Result := Spooky32AsString(GetSpooky32Sys);
end;

//------------------------------------------------------------------------------

procedure TSpooky32HashBase.FromString(const Str: String);
begin
SetSpooky32Sys(Spooky32FromString(Str));
end;

//------------------------------------------------------------------------------

procedure TSpooky32HashBase.FromStringDef(const Str: String; const Default: TSpooky32);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  SetSpooky32Sys(Spooky32ToSys(Default));
end;

//------------------------------------------------------------------------------

procedure TSpooky32HashBase.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSpooky32;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}Spooky32ToBE{$ELSE}Spooky32ToLE{$ENDIF}(Spooky32FromSys(GetSpooky32Sys));
  heLittle: Temp := Spooky32ToLE(Spooky32FromSys(GetSpooky32Sys));
  heBig:    Temp := Spooky32ToBE(Spooky32FromSys(GetSpooky32Sys));
else
 {heDefault}
  Temp := Spooky32FromSys(GetSpooky32Sys);
end;
Stream.WriteBuffer(Temp,SizeOf(TSpooky32));
end;

//------------------------------------------------------------------------------

procedure TSpooky32HashBase.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSpooky32;
begin
Temp := ZeroSpooky32;
Stream.ReadBuffer(Temp,SizeOf(TSpooky32));
case Endianness of
  heSystem: SetSpooky32Sys(Spooky32ToSys({$IFDEF ENDIAN_BIG}Spooky32FromBE{$ELSE}Spooky32FromLE{$ENDIF}(Temp)));
  heLittle: SetSpooky32Sys(Spooky32ToSys(Spooky32FromLE(Temp)));
  heBig:    SetSpooky32Sys(Spooky32ToSys(Spooky32FromBE(Temp)));
else
 {heDefault}
  SetSpooky32Sys(Spooky32ToSys(Temp));
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TSpooky32V1Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky32V1Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpooky32V1Hash - protected methods
-------------------------------------------------------------------------------}

procedure TSpooky32V1Hash.Initialize;
begin
inherited;
VersionSelect(1);
end;

{-------------------------------------------------------------------------------
    TSpooky32V1Hash - public methods
-------------------------------------------------------------------------------}

class Function TSpooky32V1Hash.HashName: String;
begin
Result := 'SpookyV1(32)';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TSpooky32V2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky32V2Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpooky32V2Hash - protected methods
-------------------------------------------------------------------------------}

procedure TSpooky32V2Hash.Initialize;
begin
inherited;
VersionSelect(2);
end;

{-------------------------------------------------------------------------------
    TSpooky32V2Hash - public methods
-------------------------------------------------------------------------------}

class Function TSpooky32V2Hash.HashName: String;
begin
Result := 'SpookyV2(32)';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSpooky64HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky64HashBase - auxiliary routines
===============================================================================}

Function SwapEndian(Hash: TSpooky64): TSpooky64; overload;
begin
Result := TSpooky64(SwapEndian(TSpooky64Sys(Hash)));
end;

//==============================================================================

Function Spooky64Compare(const A,B: TSpooky64Sys): Integer;
begin
Result := CompareUInt64(A,B);
end;

//------------------------------------------------------------------------------

Function Spooky64Same(const A,B: TSpooky64Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function Spooky64AsString(const Spooky64: TSpooky64Sys): String;
begin
Result := IntToHex(Spooky64,16);
end;

//------------------------------------------------------------------------------

Function Spooky64FromString(const Str: String): TSpooky64Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TSpooky64Sys(StrToInt64(Str))
    else
      Result := TSpooky64Sys(StrToInt64('$' + Str));
  end
else Result := TSpooky64HashBase.Spooky64ToSys(ZeroSpooky64);
end;

{===============================================================================
    TSpooky64HashBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpooky64HashBase - protected methods
-------------------------------------------------------------------------------}

Function TSpooky64HashBase.GetSeed64: TSpooky64;
begin
Result := Spooky64FromSys(GetSeed64Sys);
end;

//------------------------------------------------------------------------------

procedure TSpooky64HashBase.SetSeed64(const Value: TSpooky64);
begin
SetSeed64Sys(Spooky64ToSys(Value));
end;

//------------------------------------------------------------------------------

Function TSpooky64HashBase.GetSeed64Sys: TSpooky64Sys;
begin
Result := TSpooky64Sys(fSeed.Lo);
end;

//------------------------------------------------------------------------------

procedure TSpooky64HashBase.SetSeed64Sys(const Value: TSpooky64Sys);
begin
fSeed.Hi := Value;
fSeed.Lo := Value;
end;

//------------------------------------------------------------------------------

Function TSpooky64HashBase.GetSpooky64: TSpooky64;
begin
Result := Spooky64FromSys(GetSpooky64Sys);
end;

//------------------------------------------------------------------------------

Function TSpooky64HashBase.GetSpooky64Sys: TSpooky64Sys;
begin
Result := TSpooky64Sys(fSpookyValue.Lo);
end;

//------------------------------------------------------------------------------

procedure TSpooky64HashBase.SetSpooky64Sys(const Value: TSpooky64Sys);
begin
fSpookyValue.Hi := 0;
fSpookyValue.Lo := Value;
end;

{-------------------------------------------------------------------------------
    TSpooky64HashBase - public methods
-------------------------------------------------------------------------------}

class Function TSpooky64HashBase.Spooky64ToSys(Hash: TSpooky64): TSpooky64Sys;
begin
Result := TSpooky64Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSpooky64HashBase.Spooky64FromSys(Hash: TSpooky64Sys): TSpooky64;
begin
Result := TSpooky64({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSpooky64HashBase.Spooky64ToLE(Hash: TSpooky64): TSpooky64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSpooky64HashBase.Spooky64ToBE(Hash: TSpooky64): TSpooky64;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSpooky64HashBase.Spooky64FromLE(Hash: TSpooky64): TSpooky64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSpooky64HashBase.Spooky64FromBE(Hash: TSpooky64): TSpooky64;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSpooky64HashBase.HashSize: TMemSize;
begin
Result := SizeOf(TSpooky64);
end;

//------------------------------------------------------------------------------

constructor TSpooky64HashBase.CreateAndInitFrom(Hash: TSpooky64);
begin
CreateAndInit;
SetSpooky64Sys(Spooky64ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function TSpooky64HashBase.Compare(Hash: THashBase): Integer;
begin
If Hash is TSpooky64HashBase then
  Result := Spooky64Compare(GetSpooky64Sys,TSpooky64HashBase(Hash).Spooky64Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSpooky64HashBase.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSpooky64HashBase.Same(Hash: THashBase): Boolean;
begin
If Hash is TSpooky64HashBase then
  Result := Spooky64Same(GetSpooky64Sys,TSpooky64HashBase(Hash).Spooky64Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSpooky64HashBase.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSpooky64HashBase.AsString: String;
begin
Result := Spooky64AsString(GetSpooky64Sys);
end;

//------------------------------------------------------------------------------

procedure TSpooky64HashBase.FromString(const Str: String);
begin
SetSpooky64Sys(Spooky64FromString(Str));
end;

//------------------------------------------------------------------------------

procedure TSpooky64HashBase.FromStringDef(const Str: String; const Default: TSpooky64);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  SetSpooky64Sys(Spooky64ToSys(Default));
end;

//------------------------------------------------------------------------------

procedure TSpooky64HashBase.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSpooky64;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}Spooky64ToBE{$ELSE}Spooky64ToLE{$ENDIF}(Spooky64FromSys(GetSpooky64Sys));
  heLittle: Temp := Spooky64ToLE(Spooky64FromSys(GetSpooky64Sys));
  heBig:    Temp := Spooky64ToBE(Spooky64FromSys(GetSpooky64Sys));
else
 {heDefault}
  Temp := Spooky64FromSys(GetSpooky64Sys);
end;
Stream.WriteBuffer(Temp,SizeOf(TSpooky64));
end;

//------------------------------------------------------------------------------

procedure TSpooky64HashBase.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSpooky64;
begin
Temp := ZeroSpooky64;
Stream.ReadBuffer(Temp,SizeOf(TSpooky64));
case Endianness of
  heSystem: SetSpooky64Sys(Spooky64ToSys({$IFDEF ENDIAN_BIG}Spooky64FromBE{$ELSE}Spooky64FromLE{$ENDIF}(Temp)));
  heLittle: SetSpooky64Sys(Spooky64ToSys(Spooky64FromLE(Temp)));
  heBig:    SetSpooky64Sys(Spooky64ToSys(Spooky64FromBE(Temp)));
else
 {heDefault}
  SetSpooky64Sys(Spooky64ToSys(Temp));
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TSpooky64V1Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky64V1Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpooky64V1Hash - protected methods
-------------------------------------------------------------------------------}

procedure TSpooky64V1Hash.Initialize;
begin
inherited;
VersionSelect(1);
end;

{-------------------------------------------------------------------------------
    TSpooky64V1Hash - public methods
-------------------------------------------------------------------------------}

class Function TSpooky64V1Hash.HashName: String;
begin
Result := 'SpookyV1(64)';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TSpooky64V2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky64V2Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpooky64V2Hash - protected methods
-------------------------------------------------------------------------------}

procedure TSpooky64V2Hash.Initialize;
begin
inherited;
VersionSelect(2);
end;

{-------------------------------------------------------------------------------
    TSpooky64V2Hash - public methods
-------------------------------------------------------------------------------}

class Function TSpooky64V2Hash.HashName: String;
begin
Result := 'SpookyV2(64)';
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TSpooky128HashBase                               
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky128HashBase - auxiliary routines
===============================================================================}

Function SwapEndian(Hash: TSpooky128): TSpooky128; overload;
begin
Result := TSpooky128(SwapEndian(TSpooky128Sys(Hash)));
end;

//==============================================================================

Function Spooky128Compare(const A,B: TSpooky128Sys): Integer;
begin
Result := CompareUInt64(A.Hi,B.Hi);
If Result = 0 then
  Result := CompareUInt64(A.Lo,B.Lo);
end;

//------------------------------------------------------------------------------

Function Spooky128Same(const A,B: TSpooky128Sys): Boolean;
begin
If A.Hi = B.Hi then
  Result := A.Lo = B.Lo
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function Spooky128AsString(const Spooky128: TSpooky128Sys): String;
begin
Result := IntToHex(Spooky128.Hi,16) + IntToHex(Spooky128.Lo,16);
end;

//------------------------------------------------------------------------------

Function Spooky128FromString(const Str: String): TSpooky128Sys;
var
  TempStr:  String;
  i:        Integer;
  TempRes:  TSpooky128 absolute Result;
begin
If Length(Str) < (SizeOf(TSpooky128) * 2) then
  TempStr := StringOfChar('0',(SizeOf(TSpooky128) * 2) - Length(Str)) + Str
else If Length(Str) > (SizeOf(TSpooky128) * 2) then
  TempStr := Copy(Str,Length(Str) - Pred(SizeOf(TSpooky128) * 2),SizeOf(TSpooky128) * 2)
else
  TempStr := Str;
For i := Low(TempRes) to High(TempRes) do
  TempRes[High(TempRes) - i] := UInt8(StrToInt('$' + Copy(TempStr,(i * 2) + 1,2)));
{$IFDEF ENDIAN_BIG}
Result := SwapEndian(Result);
{$ENDIF}
end;

{===============================================================================
    TSpooky128HashBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpooky128HashBase - protected methods
-------------------------------------------------------------------------------}

Function TSpooky128HashBase.GetSeed128: TSpooky128;
begin
Result := Spooky128FromSys(fSeed);
end;

//------------------------------------------------------------------------------

procedure TSpooky128HashBase.SetSeed128(const Value: TSpooky128);
begin
fSeed := Spooky128ToSys(Value);
end;

//------------------------------------------------------------------------------

Function TSpooky128HashBase.GetSpooky128: TSpooky128;
begin
Result := Spooky128FromSys(fSpookyValue);
end;

{-------------------------------------------------------------------------------
    TSpooky128HashBase - public methods
-------------------------------------------------------------------------------}

class Function TSpooky128HashBase.Spooky128ToSys(Hash: TSpooky128): TSpooky128Sys;
begin
Result := TSpooky128Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSpooky128HashBase.Spooky128FromSys(Hash: TSpooky128Sys): TSpooky128;
begin
Result := TSpooky128({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSpooky128HashBase.Spooky128ToLE(Hash: TSpooky128): TSpooky128;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSpooky128HashBase.Spooky128ToBE(Hash: TSpooky128): TSpooky128;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSpooky128HashBase.Spooky128FromLE(Hash: TSpooky128): TSpooky128;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSpooky128HashBase.Spooky128FromBE(Hash: TSpooky128): TSpooky128;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSpooky128HashBase.HashSize: TMemSize;
begin
Result := SizeOf(TSpooky128);
end;

//------------------------------------------------------------------------------

constructor TSpooky128HashBase.CreateAndInitFrom(Hash: TSpooky128);
begin
CreateAndInit;
fSpookyValue := Spooky128ToSys(Hash);
end;

//------------------------------------------------------------------------------

Function TSpooky128HashBase.Compare(Hash: THashBase): Integer;
begin
If Hash is TSpooky128HashBase then
  Result := Spooky128Compare(fSpookyValue,TSpooky128HashBase(Hash).Spooky128Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSpooky128HashBase.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSpooky128HashBase.Same(Hash: THashBase): Boolean;
begin
If Hash is TSpooky128HashBase then
  Result := Spooky128Same(fSpookyValue,TSpooky128HashBase(Hash).Spooky128Sys)
else
  raise ESHIncompatibleClass.CreateFmt('TSpooky128HashBase.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSpooky128HashBase.AsString: String;
begin
Result := Spooky128AsString(fSpookyValue);
end;

//------------------------------------------------------------------------------

procedure TSpooky128HashBase.FromString(const Str: String);
begin
fSpookyValue := Spooky128FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TSpooky128HashBase.FromStringDef(const Str: String; const Default: TSpooky128);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSpookyValue := Spooky128ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TSpooky128HashBase.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSpooky128;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}Spooky128ToBE{$ELSE}Spooky128ToLE{$ENDIF}(Spooky128FromSys(fSpookyValue));
  heLittle: Temp := Spooky128ToLE(Spooky128FromSys(fSpookyValue));
  heBig:    Temp := Spooky128ToBE(Spooky128FromSys(fSpookyValue));
else
 {heDefault}
  Temp := Spooky128FromSys(fSpookyValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TSpooky128));
end;

//------------------------------------------------------------------------------

procedure TSpooky128HashBase.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSpooky128;
begin
Temp := ZeroSpooky128;
Stream.ReadBuffer(Temp,SizeOf(TSpooky128));
case Endianness of
  heSystem: fSpookyValue := Spooky128ToSys({$IFDEF ENDIAN_BIG}Spooky128FromBE{$ELSE}Spooky128FromLE{$ENDIF}(Temp));
  heLittle: fSpookyValue := Spooky128ToSys(Spooky128FromLE(Temp));
  heBig:    fSpookyValue := Spooky128ToSys(Spooky128FromBE(Temp));
else
 {heDefault}
  fSpookyValue := Spooky128ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSpooky128V1Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky128V1Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpooky128V1Hash - protected methods
-------------------------------------------------------------------------------}

procedure TSpooky128V1Hash.Initialize;
begin
inherited;
VersionSelect(1);
end;

{-------------------------------------------------------------------------------
    TSpooky128V1Hash - public methods
-------------------------------------------------------------------------------}

class Function TSpooky128V1Hash.HashName: String;
begin
Result := 'SpookyV1(128)';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TSpooky128V2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSpooky128V2Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSpooky128V2Hash - protected methods
-------------------------------------------------------------------------------}

procedure TSpooky128V2Hash.Initialize;
begin
inherited;
VersionSelect(2);
end;

{-------------------------------------------------------------------------------
    TSpooky128V2Hash - public methods
-------------------------------------------------------------------------------}

class Function TSpooky128V2Hash.HashName: String;
begin
Result := 'SpookyV2(128)';
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - internals
===============================================================================}

Function InitialSpookyV1State(const Seed: TSpooky128Sys): TSpookyState;
begin
SpookyV1Init(Result.InternalState,Seed);
Result.ProcessedBytes := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InitialSpookyV2State(const Seed: TSpooky128Sys): TSpookyState;
begin
SpookyV1Init(Result.InternalState,Seed);
Result.ProcessedBytes := 0;
end;

//------------------------------------------------------------------------------

procedure BufferSpookyV1(var State: TSpookyState; const Buffer; Size: TMemSize);
var
  MovingBuffer: PByte;
begin
If Size > 0 then
  begin
    If (Size mod SC_BLOCKSZ) = 0 then
      begin
        MovingBuffer := @Buffer;
        while Size >= SC_BLOCKSZ do begin
          SpookyV1Update(State.InternalState,MovingBuffer^);
          Inc(State.ProcessedBytes,SC_BLOCKSZ);
          Inc(MovingBuffer,SC_BLOCKSZ);
          Dec(Size,SC_BLOCKSZ);
        end;
      end
    else raise ESHProcessingError.CreateFmt('BufferSpookyV1: Buffer size (%u) is not divisible by %d.',[Size,SC_BLOCKSZ]);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure BufferSpookyV2(var State: TSpookyState; const Buffer; Size: TMemSize);
var
  MovingBuffer: PByte;
begin
If Size > 0 then
  begin
    If (Size mod SC_BLOCKSZ) = 0 then
      begin
        MovingBuffer := @Buffer;
        while Size >= SC_BLOCKSZ do begin
          SpookyV1Update(State.InternalState,MovingBuffer^);
          Inc(State.ProcessedBytes,SC_BLOCKSZ);
          Inc(MovingBuffer,SC_BLOCKSZ);
          Dec(Size,SC_BLOCKSZ);          
        end;
      end
    else raise ESHProcessingError.CreateFmt('BufferSpookyV2: Buffer size (%u) is not divisible by %d.',[Size,SC_BLOCKSZ]);
  end;
end;

//------------------------------------------------------------------------------

Function LastBufferSpookyV1(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky128Sys;
begin
If (State.ProcessedBytes + Size) >= SC_BLOCKSZ then
  begin
  {
    Long processing - at this point Size can be 0 or 192+, nothing else is
    possible (unless someone explicitly alters it, which is a bad idea).

    Note that if the provided buffer is larger than block size, SpookyV1Final
    will manage that, as if it was correctly processed using SpookyV1Update.
  }
    SpookyV1Final(State.InternalState,Buffer,Size);
    Result.Lo := State.InternalState[0];
    Result.Hi := State.InternalState[1];
  end
else
  begin
    Result.Lo := State.InternalState[0];
    Result.Hi := State.InternalState[1];
    SpookyV1Short(Buffer,Size,Result);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSpookyV2(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky128Sys;
begin
If (State.ProcessedBytes + Size) >= SC_BLOCKSZ then
  begin
    SpookyV2Final(State.InternalState,Buffer,Size);
    Result.Lo := State.InternalState[0];
    Result.Hi := State.InternalState[1];
  end
else
  begin
    Result.Lo := State.InternalState[0];
    Result.Hi := State.InternalState[1];
    SpookyV2Short(Buffer,Size,Result);
  end;
end;


{===============================================================================
    Common 32bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Common 32bit procedural interface - utility functions
-------------------------------------------------------------------------------}

Function Spooky32ToStr(const Hash: TSpooky32): String;
begin
Result := Spooky32AsString(TSpooky32HashBase.Spooky32ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToSpooky32(const Str: String): TSpooky32;
begin
Result := TSpooky32HashBase.Spooky32FromSys(Spooky32FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToSpooky32(const Str: String; out Hash: TSpooky32): Boolean;
begin
try
  Hash := TSpooky32HashBase.Spooky32FromSys(Spooky32FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToSpooky32Def(const Str: String; const Default: TSpooky32): TSpooky32;
begin
If not TryStrToSpooky32(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareSpooky32(const A,B: TSpooky32): Integer;
begin
Result := Spooky32Compare(TSpooky32HashBase.Spooky32ToSys(A),TSpooky32HashBase.Spooky32ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameSpooky32(const A,B: TSpooky32): Boolean;
begin
Result := Spooky32Same(TSpooky32HashBase.Spooky32ToSys(A),TSpooky32HashBase.Spooky32ToSys(B));
end;

{===============================================================================
    Common 64bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Common 64bit procedural interface - utility functions
-------------------------------------------------------------------------------}

Function Spooky64ToStr(const Hash: TSpooky64): String;
begin
Result := Spooky64AsString(TSpooky64HashBase.Spooky64ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToSpooky64(const Str: String): TSpooky64;
begin
Result := TSpooky64HashBase.Spooky64FromSys(Spooky64FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToSpooky64(const Str: String; out Hash: TSpooky64): Boolean;
begin
try
  Hash := TSpooky64HashBase.Spooky64FromSys(Spooky64FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToSpooky64Def(const Str: String; const Default: TSpooky64): TSpooky64;
begin
If not TryStrToSpooky64(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareSpooky64(const A,B: TSpooky64): Integer;
begin
Result := Spooky64Compare(TSpooky64HashBase.Spooky64ToSys(A),TSpooky64HashBase.Spooky64ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameSpooky64(const A,B: TSpooky64): Boolean;
begin
Result := Spooky64Same(TSpooky64HashBase.Spooky64ToSys(A),TSpooky64HashBase.Spooky64ToSys(B));
end;

{===============================================================================
    Common 128bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Common 128bit procedural interface - utility functions
-------------------------------------------------------------------------------}

Function Spooky128ToStr(const Hash: TSpooky128): String;
begin
Result := Spooky128AsString(TSpooky128HashBase.Spooky128ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToSpooky128(const Str: String): TSpooky128;
begin
Result := TSpooky128HashBase.Spooky128FromSys(Spooky128FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToSpooky128(const Str: String; out Hash: TSpooky128): Boolean;
begin
try
  Hash := TSpooky128HashBase.Spooky128FromSys(Spooky128FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToSpooky128Def(const Str: String; const Default: TSpooky128): TSpooky128;
begin
If not TryStrToSpooky128(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareSpooky128(const A,B: TSpooky128): Integer;
begin
Result := Spooky128Compare(TSpooky128HashBase.Spooky128ToSys(A),TSpooky128HashBase.Spooky128ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameSpooky128(const A,B: TSpooky128): Boolean;
begin
Result := Spooky128Same(TSpooky128HashBase.Spooky128ToSys(A),TSpooky128HashBase.Spooky128ToSys(B));
end;

//------------------------------------------------------------------------------

Function SeedSpooky128(SeedLo,SeedHi: UInt64): TSpooky128;
var
  SeedSys:  TSpooky128Sys;
begin
SeedSys.Lo := SeedLo;
SeedSys.Hi := SeedHi;
Result := TSpooky128HashBase.Spooky128FromSys(SeedSys);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SeedSpooky128(Seed: UInt64): TSpooky128;
var
  SeedSys:  TSpooky128Sys;
begin
SeedSys.Lo := Seed;
SeedSys.Hi := Seed;
Result := TSpooky128HashBase.Spooky128FromSys(SeedSys);
end;

{===============================================================================
    SpookyV1 32bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    SpookyV1 32bit procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BlockSizeSpooky32V1: TMemSize;
begin
Result := SC_BLOCKSZ; // this is always the same
end;

//------------------------------------------------------------------------------

Function InitialStateSpooky32V1(const Seed: TSpooky32): TSpookyState;
var
  FullSeed: TSpooky128Sys;
begin
FullSeed.Lo := UInt64(TSpooky32HashBase.Spooky32ToSys(Seed));
FullSeed.Hi := UInt64(TSpooky32HashBase.Spooky32ToSys(Seed));
Result := InitialSpookyV1State(FullSeed);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InitialStateSpooky32V1: TSpookyState;
begin
Result := InitialStateSpooky32V1(InitialSpooky32);
end;

//------------------------------------------------------------------------------

procedure BufferSpooky32V1(var State: TSpookyState; const Buffer; Size: TMemSize);
begin
BufferSpookyV1(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function LastBufferSpooky32V1(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky32;
begin
Result := TSpooky32HashBase.Spooky32FromSys(TSpooky32Sys(LastBufferSpookyV1(State,Buffer,Size).Lo));
end;

//==============================================================================

Function BufferSpooky32V1(const Seed: TSpooky32; const Buffer; Size: TMemSize): TSpooky32;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky32V1(Seed);
Result := LastBufferSpooky32V1(State,Buffer,Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky32V1(const Buffer; Size: TMemSize): TSpooky32;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky32V1;
Result := LastBufferSpooky32V1(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringSpooky32V1(const Str: AnsiString): TSpooky32;
begin
Result := BufferSpooky32V1(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringSpooky32V1(const Str: WideString): TSpooky32;
begin
Result := BufferSpooky32V1(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringSpooky32V1(const Str: String): TSpooky32;
begin
Result := BufferSpooky32V1(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamSpooky32V1(Stream: TStream; Count: Int64 = -1): TSpooky32;
var
  Hasher: TSpooky32V1Hash;
begin
Hasher := TSpooky32V1Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Spooky32;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSpooky32V1(const FileName: String): TSpooky32;
var
  Hasher: TSpooky32V1Hash;
begin
Hasher := TSpooky32V1Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Spooky32;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    SpookyV1 32bit procedural interface - context functions
-------------------------------------------------------------------------------}

Function Spooky32V1_Init: TSpooky32Context;
begin
Result := TSpooky32Context(TSpooky32V1Hash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky32V1_Init(const Seed: TSpooky32): TSpooky32Context;
begin
Result := TSpooky32Context(TSpooky32V1Hash.CreateAndInit);
TSpooky32V1Hash(Result).Seed32 := Seed;
end;

//------------------------------------------------------------------------------

procedure Spooky32V1_Update(const Context: TSpooky32Context; const Buffer; Size: TMemSize);
begin
TSpooky32V1Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Spooky32V1_Final(var Context: TSpooky32Context; const Buffer; Size: TMemSize): TSpooky32;
begin
Spooky32V1_Update(Context,Buffer,Size);
Result := Spooky32V1_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky32V1_Final(var Context: TSpooky32Context): TSpooky32;
begin
TSpooky32V1Hash(Context).Final;
Result := TSpooky32V1Hash(Context).Spooky32;
FreeAndNil(TSpooky32V1Hash(Context));
end;

//------------------------------------------------------------------------------

Function Spooky32V1_Hash(const Buffer; Size: TMemSize): TSpooky32;
begin
Result := BufferSpooky32V1(Buffer,Size);
end;

{===============================================================================
    SpookyV@ 32bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    SpookyV@ 32bit procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BlockSizeSpooky32V2: TMemSize;
begin
Result := SC_BLOCKSZ;
end;

//------------------------------------------------------------------------------

Function InitialStateSpooky32V2(const Seed: TSpooky32): TSpookyState;
var
  FullSeed: TSpooky128Sys;
begin
FullSeed.Lo := UInt64(TSpooky32HashBase.Spooky32ToSys(Seed));
FullSeed.Hi := UInt64(TSpooky32HashBase.Spooky32ToSys(Seed));
Result := InitialSpookyV2State(FullSeed);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InitialStateSpooky32V2: TSpookyState;
begin
Result := InitialStateSpooky32V2(InitialSpooky32);
end;

//------------------------------------------------------------------------------

procedure BufferSpooky32V2(var State: TSpookyState; const Buffer; Size: TMemSize);
begin
BufferSpookyV2(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function LastBufferSpooky32V2(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky32;
begin
Result := TSpooky32HashBase.Spooky32FromSys(TSpooky32Sys(LastBufferSpookyV2(State,Buffer,Size).Lo));
end;

//==============================================================================

Function BufferSpooky32V2(const Seed: TSpooky32; const Buffer; Size: TMemSize): TSpooky32;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky32V2(Seed);
Result := LastBufferSpooky32V2(State,Buffer,Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky32V2(const Buffer; Size: TMemSize): TSpooky32;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky32V2;
Result := LastBufferSpooky32V2(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringSpooky32V2(const Str: AnsiString): TSpooky32;
begin
Result := BufferSpooky32V2(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringSpooky32V2(const Str: WideString): TSpooky32;
begin
Result := BufferSpooky32V2(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringSpooky32V2(const Str: String): TSpooky32;
begin
Result := BufferSpooky32V2(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamSpooky32V2(Stream: TStream; Count: Int64 = -1): TSpooky32;
var
  Hasher: TSpooky32V2Hash;
begin
Hasher := TSpooky32V2Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Spooky32;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSpooky32V2(const FileName: String): TSpooky32;
var
  Hasher: TSpooky32V2Hash;
begin
Hasher := TSpooky32V2Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Spooky32;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    SpookyV1 32bit procedural interface - context functions
-------------------------------------------------------------------------------}

Function Spooky32V2_Init: TSpooky32Context;
begin
Result := TSpooky32Context(TSpooky32V2Hash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky32V2_Init(const Seed: TSpooky32): TSpooky32Context;
begin
Result := TSpooky32Context(TSpooky32V2Hash.CreateAndInit);
TSpooky32V2Hash(Result).Seed32 := Seed;
end;

//------------------------------------------------------------------------------

procedure Spooky32V2_Update(const Context: TSpooky32Context; const Buffer; Size: TMemSize);
begin
TSpooky32V2Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Spooky32V2_Final(var Context: TSpooky32Context; const Buffer; Size: TMemSize): TSpooky32;
begin
Spooky32V2_Update(Context,Buffer,Size);
Result := Spooky32V2_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky32V2_Final(var Context: TSpooky32Context): TSpooky32;
begin
TSpooky32V2Hash(Context).Final;
Result := TSpooky32V2Hash(Context).Spooky32;
FreeAndNil(TSpooky32V2Hash(Context));
end;

//------------------------------------------------------------------------------

Function Spooky32V2_Hash(const Buffer; Size: TMemSize): TSpooky32;
begin
Result := BufferSpooky32V2(Buffer,Size);
end;

{===============================================================================
    SpookyV1 64bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    SpookyV1 64bit procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BlockSizeSpooky64V1: TMemSize;
begin
Result := SC_BLOCKSZ;
end;

//------------------------------------------------------------------------------

Function InitialStateSpooky64V1(const Seed: TSpooky64): TSpookyState;
var
  FullSeed: TSpooky128Sys;
begin
FullSeed.Lo := TSpooky64HashBase.Spooky64ToSys(Seed);
FullSeed.Hi := TSpooky64HashBase.Spooky64ToSys(Seed);
Result := InitialSpookyV1State(FullSeed);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InitialStateSpooky64V1: TSpookyState;
begin
Result := InitialStateSpooky64V1(InitialSpooky64);
end;

//------------------------------------------------------------------------------

procedure BufferSpooky64V1(var State: TSpookyState; const Buffer; Size: TMemSize);
begin
BufferSpookyV1(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function LastBufferSpooky64V1(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky64;
begin
Result := TSpooky64HashBase.Spooky64FromSys(LastBufferSpookyV1(State,Buffer,Size).Lo);
end;

//==============================================================================

Function BufferSpooky64V1(const Seed: TSpooky64; const Buffer; Size: TMemSize): TSpooky64;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky64V1(Seed);
Result := LastBufferSpooky64V1(State,Buffer,Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky64V1(const Buffer; Size: TMemSize): TSpooky64;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky64V1;
Result := LastBufferSpooky64V1(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringSpooky64V1(const Str: AnsiString): TSpooky64;
begin
Result := BufferSpooky64V1(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringSpooky64V1(const Str: WideString): TSpooky64;
begin
Result := BufferSpooky64V1(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringSpooky64V1(const Str: String): TSpooky64;
begin
Result := BufferSpooky64V1(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamSpooky64V1(Stream: TStream; Count: Int64 = -1): TSpooky64;
var
  Hasher: TSpooky64V1Hash;
begin
Hasher := TSpooky64V1Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Spooky64;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSpooky64V1(const FileName: String): TSpooky64;
var
  Hasher: TSpooky64V1Hash;
begin
Hasher := TSpooky64V1Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Spooky64;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    SpookyV1 64bit procedural interface - context functions
-------------------------------------------------------------------------------}

Function Spooky64V1_Init: TSpooky64Context;
begin
Result := TSpooky64Context(TSpooky64V1Hash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky64V1_Init(const Seed: TSpooky64): TSpooky64Context;
begin
Result := TSpooky64Context(TSpooky64V1Hash.CreateAndInit);
TSpooky64V1Hash(Result).Seed64 := Seed;
end;

//------------------------------------------------------------------------------

procedure Spooky64V1_Update(const Context: TSpooky64Context; const Buffer; Size: TMemSize);
begin
TSpooky64V1Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Spooky64V1_Final(var Context: TSpooky64Context; const Buffer; Size: TMemSize): TSpooky64;
begin
Spooky64V1_Update(Context,Buffer,Size);
Result := Spooky64V1_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky64V1_Final(var Context: TSpooky64Context): TSpooky64;
begin
TSpooky64V1Hash(Context).Final;
Result := TSpooky64V1Hash(Context).Spooky64;
FreeAndNil(TSpooky64V1Hash(Context));
end;

//------------------------------------------------------------------------------

Function Spooky64V1_Hash(const Buffer; Size: TMemSize): TSpooky64;
begin
Result := BufferSpooky64V1(Buffer,Size);
end;

{===============================================================================
    SpookyV2 64bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    SpookyV2 64bit procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BlockSizeSpooky64V2: TMemSize;
begin
Result := SC_BLOCKSZ;
end;

//------------------------------------------------------------------------------

Function InitialStateSpooky64V2(const Seed: TSpooky64): TSpookyState;
var
  FullSeed: TSpooky128Sys;
begin
FullSeed.Lo := TSpooky64HashBase.Spooky64ToSys(Seed);
FullSeed.Hi := TSpooky64HashBase.Spooky64ToSys(Seed);
Result := InitialSpookyV2State(FullSeed);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InitialStateSpooky64V2: TSpookyState;
begin
Result := InitialStateSpooky64V2(InitialSpooky64);
end;

//------------------------------------------------------------------------------

procedure BufferSpooky64V2(var State: TSpookyState; const Buffer; Size: TMemSize);
begin
BufferSpookyV2(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function LastBufferSpooky64V2(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky64;
begin
Result := TSpooky64HashBase.Spooky64FromSys(LastBufferSpookyV2(State,Buffer,Size).Lo);
end;

//==============================================================================

Function BufferSpooky64V2(const Seed: TSpooky64; const Buffer; Size: TMemSize): TSpooky64;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky64V2(Seed);
Result := LastBufferSpooky64V2(State,Buffer,Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky64V2(const Buffer; Size: TMemSize): TSpooky64;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky64V2;
Result := LastBufferSpooky64V2(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringSpooky64V2(const Str: AnsiString): TSpooky64;
begin
Result := BufferSpooky64V2(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringSpooky64V2(const Str: WideString): TSpooky64;
begin
Result := BufferSpooky64V2(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringSpooky64V2(const Str: String): TSpooky64;
begin
Result := BufferSpooky64V2(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamSpooky64V2(Stream: TStream; Count: Int64 = -1): TSpooky64;
var
  Hasher: TSpooky64V2Hash;
begin
Hasher := TSpooky64V2Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Spooky64;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSpooky64V2(const FileName: String): TSpooky64;
var
  Hasher: TSpooky64V2Hash;
begin
Hasher := TSpooky64V2Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Spooky64;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    SpookyV2 64bit procedural interface - context functions
-------------------------------------------------------------------------------}

Function Spooky64V2_Init: TSpooky64Context;
begin
Result := TSpooky64Context(TSpooky64V2Hash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky64V2_Init(const Seed: TSpooky64): TSpooky64Context;
begin
Result := TSpooky64Context(TSpooky64V2Hash.CreateAndInit);
TSpooky64V2Hash(Result).Seed64 := Seed;
end;

//------------------------------------------------------------------------------

procedure Spooky64V2_Update(const Context: TSpooky64Context; const Buffer; Size: TMemSize);
begin
TSpooky64V2Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Spooky64V2_Final(var Context: TSpooky64Context; const Buffer; Size: TMemSize): TSpooky64;
begin
Spooky64V2_Update(Context,Buffer,Size);
Result := Spooky64V2_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky64V2_Final(var Context: TSpooky64Context): TSpooky64;
begin
TSpooky64V2Hash(Context).Final;
Result := TSpooky64V2Hash(Context).Spooky64;
FreeAndNil(TSpooky64V2Hash(Context));
end;

//------------------------------------------------------------------------------

Function Spooky64V2_Hash(const Buffer; Size: TMemSize): TSpooky64;
begin
Result := BufferSpooky64V2(Buffer,Size);
end;

{===============================================================================
    SpookyV1 128bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    SpookyV1 128bit procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BlockSizeSpooky128V1: TMemSize;
begin
Result := SC_BLOCKSZ;
end;

//------------------------------------------------------------------------------

Function InitialStateSpooky128V1(const Seed: TSpooky128): TSpookyState;
begin
Result := InitialSpookyV1State(TSpooky128HashBase.Spooky128ToSys(Seed));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InitialStateSpooky128V1: TSpookyState;
begin
Result := InitialStateSpooky128V1(InitialSpooky128);
end;

//------------------------------------------------------------------------------

procedure BufferSpooky128V1(var State: TSpookyState; const Buffer; Size: TMemSize);
begin
BufferSpookyV1(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function LastBufferSpooky128V1(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky128;
begin
Result := TSpooky128HashBase.Spooky128FromSys(LastBufferSpookyV1(State,Buffer,Size));
end;

//==============================================================================

Function BufferSpooky128V1(const Seed: TSpooky128; const Buffer; Size: TMemSize): TSpooky128;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky128V1(Seed);
Result := LastBufferSpooky128V1(State,Buffer,Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky128V1(const Buffer; Size: TMemSize): TSpooky128;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky128V1;
Result := LastBufferSpooky128V1(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringSpooky128V1(const Str: AnsiString): TSpooky128;
begin
Result := BufferSpooky128V1(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringSpooky128V1(const Str: WideString): TSpooky128;
begin
Result := BufferSpooky128V1(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringSpooky128V1(const Str: String): TSpooky128;
begin
Result := BufferSpooky128V1(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamSpooky128V1(Stream: TStream; Count: Int64 = -1): TSpooky128;
var
  Hasher: TSpooky128V1Hash;
begin
Hasher := TSpooky128V1Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Spooky128;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSpooky128V1(const FileName: String): TSpooky128;
var
  Hasher: TSpooky128V1Hash;
begin
Hasher := TSpooky128V1Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Spooky128;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    SpookyV1 128bit procedural interface - context functions
-------------------------------------------------------------------------------}

Function Spooky128V1_Init: TSpooky128Context;
begin
Result := TSpooky128Context(TSpooky128V1Hash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky128V1_Init(const Seed: TSpooky128): TSpooky128Context;
begin
Result := TSpooky128Context(TSpooky128V1Hash.CreateAndInit);
TSpooky128V1Hash(Result).Seed128 := Seed;
end;

//------------------------------------------------------------------------------

procedure Spooky128V1_Update(const Context: TSpooky128Context; const Buffer; Size: TMemSize);
begin
TSpooky128V1Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Spooky128V1_Final(var Context: TSpooky128Context; const Buffer; Size: TMemSize): TSpooky128;
begin
Spooky128V1_Update(Context,Buffer,Size);
Result := Spooky128V1_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky128V1_Final(var Context: TSpooky128Context): TSpooky128;
begin
TSpooky128V1Hash(Context).Final;
Result := TSpooky128V1Hash(Context).Spooky128;
FreeAndNil(TSpooky128V1Hash(Context));
end;

//------------------------------------------------------------------------------

Function Spooky128V1_Hash(const Buffer; Size: TMemSize): TSpooky128;
begin
Result := BufferSpooky128V1(Buffer,Size);
end;

{===============================================================================
    SpookyV2 128bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    SpookyV2 128bit procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BlockSizeSpooky128V2: TMemSize;
begin
Result := SC_BLOCKSZ;
end;

//------------------------------------------------------------------------------

Function InitialStateSpooky128V2(const Seed: TSpooky128): TSpookyState;
begin
Result := InitialSpookyV2State(TSpooky128HashBase.Spooky128ToSys(Seed));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InitialStateSpooky128V2: TSpookyState;
begin
Result := InitialStateSpooky128V2(InitialSpooky128);
end;

//------------------------------------------------------------------------------

procedure BufferSpooky128V2(var State: TSpookyState; const Buffer; Size: TMemSize);
begin
BufferSpookyV2(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function LastBufferSpooky128V2(var State: TSpookyState; const Buffer; Size: TMemSize): TSpooky128;
begin
Result := TSpooky128HashBase.Spooky128FromSys(LastBufferSpookyV2(State,Buffer,Size));
end;

//==============================================================================

Function BufferSpooky128V2(const Seed: TSpooky128; const Buffer; Size: TMemSize): TSpooky128;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky128V2(Seed);
Result := LastBufferSpooky128V2(State,Buffer,Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSpooky128V2(const Buffer; Size: TMemSize): TSpooky128;
var
  State:  TSpookyState;
begin
State := InitialStateSpooky128V2;
Result := LastBufferSpooky128V2(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringSpooky128V2(const Str: AnsiString): TSpooky128;
begin
Result := BufferSpooky128V2(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringSpooky128V2(const Str: WideString): TSpooky128;
begin
Result := BufferSpooky128V2(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringSpooky128V2(const Str: String): TSpooky128;
begin
Result := BufferSpooky128V2(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamSpooky128V2(Stream: TStream; Count: Int64 = -1): TSpooky128;
var
  Hasher: TSpooky128V2Hash;
begin
Hasher := TSpooky128V2Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Spooky128;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSpooky128V2(const FileName: String): TSpooky128;
var
  Hasher: TSpooky128V2Hash;
begin
Hasher := TSpooky128V2Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Spooky128;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    SpookyV2 128bit procedural interface - context functions
-------------------------------------------------------------------------------}

Function Spooky128V2_Init: TSpooky128Context;
begin
Result := TSpooky128Context(TSpooky128V2Hash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky128V2_Init(const Seed: TSpooky128): TSpooky128Context;
begin
Result := TSpooky128Context(TSpooky128V2Hash.CreateAndInit);
TSpooky128V2Hash(Result).Seed128 := Seed;
end;

//------------------------------------------------------------------------------

procedure Spooky128V2_Update(const Context: TSpooky128Context; const Buffer; Size: TMemSize);
begin
TSpooky128V2Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Spooky128V2_Final(var Context: TSpooky128Context; const Buffer; Size: TMemSize): TSpooky128;
begin
Spooky128V2_Update(Context,Buffer,Size);
Result := Spooky128V2_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Spooky128V2_Final(var Context: TSpooky128Context): TSpooky128;
begin
TSpooky128V2Hash(Context).Final;
Result := TSpooky128V2Hash(Context).Spooky128;
FreeAndNil(TSpooky128V2Hash(Context));
end;

//------------------------------------------------------------------------------

Function Spooky128V2_Hash(const Buffer; Size: TMemSize): TSpooky128;
begin
Result := BufferSpooky128V2(Buffer,Size);
end; 

end.
