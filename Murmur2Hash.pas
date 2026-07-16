{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Murmur2 hash (also known as MurmurHash2)

    All main variants are provided, except for Aligned and Neutral - these
    should not be needed here (they are means for "unusual" platforms) and
    produce the same hashes as basic 32bit Murmur2.
    
    CMumur2A progressive/incremental hash is also fully implemented.

  Version 1.0 (2026-07-15)

  Last change 2026-07-15

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Murmur2Hash

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
unit Murmur2Hash;

{$IFDEF FPC}
  {$MODE ObjFPC}
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
  EMUR2Exception = class(EHashException);

  EMUR2IncompatibleClass = class(EMUR2Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TMurmur* are, in memory, always ordered from least significant
  byte to most significant byte (little endian).

  Types TMurmur*Sys have no such guarantee and their endianness is system-
  dependent.
}
type
  TMurmur32 = packed array[0..3] of UInt8;
  PMurmur32 = ^TMurmur32;

  TMurmur32Sys = UInt32;
  PMurmur32Sys = ^TMurmur32Sys;

  TMurmur64 = packed array[0..7] of UInt8;
  PMurmur64 = ^TMurmur64;

  TMurmur64Sys = UInt64;
  PMurmur64Sys = ^TMurmur64Sys;

const
  InitialMurmur32: TMurmur32 = ($00,$00,$00,$00);
  InitialMurmur64: TMurmur64 = ($00,$00,$00,$00,$00,$00,$00,$00);

  ZeroMurmur32:    TMurmur32 = (0,0,0,0);
  ZeroMurmur64:    TMurmur64 = (0,0,0,0,0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                TMurmur32HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur32HashBase - class declaration
===============================================================================}
type
  TMurmur32HashBase = class(TBufferHash)
  protected
    fSeed:        TMurmur32Sys;
    fMurmurValue: TMurmur32Sys;
    Function GetSeed: TMurmur32; virtual;
    procedure SetSeed(const Value: TMurmur32); virtual;
    Function GetMurmur32: TMurmur32; virtual;
    procedure Initialize; override;
  public
    class Function Murmur32ToSys(Hash: TMurmur32): TMurmur32Sys; virtual;
    class Function Murmur32FromSys(Hash: TMurmur32Sys): TMurmur32; virtual;
    class Function Murmur32ToLE(Hash: TMurmur32): TMurmur32; virtual;
    class Function Murmur32ToBE(Hash: TMurmur32): TMurmur32; virtual;
    class Function Murmur32FromLE(Hash: TMurmur32): TMurmur32; virtual;
    class Function Murmur32FromBE(Hash: TMurmur32): TMurmur32; virtual;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMurmur32); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMurmur32); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Seed: TMurmur32 read GetSeed write SetSeed;
    property SeedSys: TMurmur32Sys read fSeed write fSeed;
    property Murmur32: TMurmur32 read GetMurmur32;
    property Murmur32Sys: TMurmur32Sys read fMurmurValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TMurmur64HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur64HashBase - class declaration
===============================================================================}
type
  TMurmur64HashBase = class(TBufferHash)
  protected
    fSeed:        TMurmur64Sys;
    fMurmurValue: TMurmur64Sys;
    Function GetSeed: TMurmur64; virtual;
    procedure SetSeed(const Value: TMurmur64); virtual;
    Function GetMurmur64: TMurmur64; virtual;
    procedure Initialize; override;
  public
    class Function Murmur64ToSys(Hash: TMurmur64): TMurmur64Sys; virtual;
    class Function Murmur64FromSys(Hash: TMurmur64Sys): TMurmur64; virtual;
    class Function Murmur64ToLE(Hash: TMurmur64): TMurmur64; virtual;
    class Function Murmur64ToBE(Hash: TMurmur64): TMurmur64; virtual;
    class Function Murmur64FromLE(Hash: TMurmur64): TMurmur64; virtual;
    class Function Murmur64FromBE(Hash: TMurmur64): TMurmur64; virtual;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMurmur64); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMurmur64); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Seed: TMurmur64 read GetSeed write SetSeed;
    property SeedSys: TMurmur64Sys read fSeed write fSeed;
    property Murmur64: TMurmur64 read GetMurmur64;
    property Murmur64Sys: TMurmur64Sys read fMurmurValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur2Hash - class declaration
===============================================================================}
type
  TMurmur2Hash = class(TMurmur32HashBase)
  protected
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur2AHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur2AHash - class declaration
===============================================================================}
type
  TMurmur2AHash = class(TMurmur32HashBase)
  protected
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur64AHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur64AHash - class declaration
===============================================================================}
type
  TMurmur64AHash = class(TMurmur64HashBase)
  protected
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur64BHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur64BHash - class declaration
===============================================================================}
type
  TMurmur64BHash = class(TMurmur64HashBase)
  protected
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TCMurmur2AHash                                
--------------------------------------------------------------------------------
===============================================================================}
type
  TMUR2RemainderBuffer = packed array[0..3] of UInt8;

{===============================================================================
    TCMurmur2AHash - class declaration
===============================================================================}
type
  TCMurmur2AHash = class(TStreamHash)
  protected
    fMurmurValue:     TMurmur32Sys;
    fRemainder:       TMUR2RemainderBuffer;
    fRemainderBytes:  Integer;
    Function GetMurmur32: TMurmur32; virtual;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function Murmur32ToSys(Hash: TMurmur32): TMurmur32Sys; virtual;
    class Function Murmur32FromSys(Hash: TMurmur32Sys): TMurmur32; virtual;
    class Function Murmur32ToLE(Hash: TMurmur32): TMurmur32; virtual;
    class Function Murmur32ToBE(Hash: TMurmur32): TMurmur32; virtual;
    class Function Murmur32FromLE(Hash: TMurmur32): TMurmur32; virtual;
    class Function Murmur32FromBE(Hash: TMurmur32): TMurmur32; virtual;
    class Function HashName: String; override;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMurmur32); overload; virtual;
    procedure Init; override;
    procedure Final; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMurmur32); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Murmur32: TMurmur32 read GetMurmur32;
    property Murmur32Sys: TMurmur32Sys read fMurmurValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Common 32bit procedural interface - declaration
===============================================================================}

Function Murmur32ToStr(const Hash: TMurmur32): String;
Function StrToMurmur32(const Str: String): TMurmur32;
Function TryStrToMurmur32(const Str: String; out Hash: TMurmur32): Boolean;
Function StrToMurmur32Def(const Str: String; const Default: TMurmur32): TMurmur32;

Function CompareMurmur32(const A,B: TMurmur32): Integer;
Function SameMurmur32(const A,B: TMurmur32): Boolean;

{===============================================================================
    Common 64bit procedural interface - declaration
===============================================================================}

Function Murmur64ToStr(const Hash: TMurmur64): String;
Function StrToMurmur64(const Str: String): TMurmur64;
Function TryStrToMurmur64(const Str: String; out Hash: TMurmur64): Boolean;
Function StrToMurmur64Def(const Str: String; const Default: TMurmur64): TMurmur64;

Function CompareMurmur64(const A,B: TMurmur64): Integer;
Function SameMurmur64(const A,B: TMurmur64): Boolean;

{===============================================================================
    Murmur2 procedural interface - declaration
===============================================================================}

Function BufferMurmur2(const Seed: TMurmur32; const Buffer; Size: TMemSize): TMurmur32; overload;
Function BufferMurmur2(const Buffer; Size: TMemSize): TMurmur32; overload;

Function AnsiStringMurmur2(const Str: AnsiString): TMurmur32;
Function WideStringMurmur2(const Str: WideString): TMurmur32;
Function StringMurmur2(const Str: String): TMurmur32;

Function StreamMurmur2(Stream: TStream; Count: Int64 = -1): TMurmur32;
Function FileMurmur2(const FileName: String): TMurmur32;

//------------------------------------------------------------------------------
type
  TMurmur2Context = type Pointer;

Function Murmur2_Init: TMurmur2Context; overload;
Function Murmur2_Init(const Seed: TMurmur32): TMurmur2Context; overload;
procedure Murmur2_Update(const Context: TMurmur2Context; const Buffer; Size: TMemSize);
Function Murmur2_Final(var Context: TMurmur2Context; const Buffer; Size: TMemSize): TMurmur32; overload;
Function Murmur2_Final(var Context: TMurmur2Context): TMurmur32; overload;
Function Murmur2_Hash(const Buffer; Size: TMemSize): TMurmur32;

{===============================================================================
    Murmur2A procedural interface - declaration
===============================================================================}

Function BufferMurmur2A(const Seed: TMurmur32; const Buffer; Size: TMemSize): TMurmur32; overload;
Function BufferMurmur2A(const Buffer; Size: TMemSize): TMurmur32; overload;

Function AnsiStringMurmur2A(const Str: AnsiString): TMurmur32;
Function WideStringMurmur2A(const Str: WideString): TMurmur32;
Function StringMurmur2A(const Str: String): TMurmur32;

Function StreamMurmur2A(Stream: TStream; Count: Int64 = -1): TMurmur32;
Function FileMurmur2A(const FileName: String): TMurmur32;

//------------------------------------------------------------------------------
type
  TMurmur2AContext = type Pointer;

Function Murmur2A_Init: TMurmur2AContext; overload;
Function Murmur2A_Init(const Seed: TMurmur32): TMurmur2AContext; overload;
procedure Murmur2A_Update(const Context: TMurmur2AContext; const Buffer; Size: TMemSize);
Function Murmur2A_Final(var Context: TMurmur2AContext; const Buffer; Size: TMemSize): TMurmur32; overload;
Function Murmur2A_Final(var Context: TMurmur2AContext): TMurmur32; overload;
Function Murmur2A_Hash(const Buffer; Size: TMemSize): TMurmur32;

{===============================================================================
    Murmur64A procedural interface - declaration
===============================================================================}

Function BufferMurmur64A(const Seed: TMurmur64; const Buffer; Size: TMemSize): TMurmur64; overload;
Function BufferMurmur64A(const Buffer; Size: TMemSize): TMurmur64; overload;

Function AnsiStringMurmur64A(const Str: AnsiString): TMurmur64;
Function WideStringMurmur64A(const Str: WideString): TMurmur64;
Function StringMurmur64A(const Str: String): TMurmur64;

Function StreamMurmur64A(Stream: TStream; Count: Int64 = -1): TMurmur64;
Function FileMurmur64A(const FileName: String): TMurmur64;

//------------------------------------------------------------------------------
type
  TMurmur64AContext = type Pointer;

Function Murmur64A_Init: TMurmur64AContext; overload;
Function Murmur64A_Init(const Seed: TMurmur64): TMurmur64AContext; overload;
procedure Murmur64A_Update(const Context: TMurmur64AContext; const Buffer; Size: TMemSize);
Function Murmur64A_Final(var Context: TMurmur64AContext; const Buffer; Size: TMemSize): TMurmur64; overload;
Function Murmur64A_Final(var Context: TMurmur64AContext): TMurmur64; overload;
Function Murmur64A_Hash(const Buffer; Size: TMemSize): TMurmur64;

{===============================================================================
    Murmur64B procedural interface - declaration
===============================================================================}

Function BufferMurmur64B(const Seed: TMurmur64; const Buffer; Size: TMemSize): TMurmur64; overload;
Function BufferMurmur64B(const Buffer; Size: TMemSize): TMurmur64; overload;

Function AnsiStringMurmur64B(const Str: AnsiString): TMurmur64;
Function WideStringMurmur64B(const Str: WideString): TMurmur64;
Function StringMurmur64B(const Str: String): TMurmur64;

Function StreamMurmur64B(Stream: TStream; Count: Int64 = -1): TMurmur64;
Function FileMurmur64B(const FileName: String): TMurmur64;

//------------------------------------------------------------------------------
type
  TMurmur64BContext = type Pointer;

Function Murmur64B_Init: TMurmur64BContext; overload;
Function Murmur64B_Init(const Seed: TMurmur64): TMurmur64BContext; overload;
procedure Murmur64B_Update(const Context: TMurmur64BContext; const Buffer; Size: TMemSize);
Function Murmur64B_Final(var Context: TMurmur64BContext; const Buffer; Size: TMemSize): TMurmur64; overload;
Function Murmur64B_Final(var Context: TMurmur64BContext): TMurmur64; overload;
Function Murmur64B_Hash(const Buffer; Size: TMemSize): TMurmur64;

{===============================================================================
    CMurmur2A procedural interface - declaration
===============================================================================}
type
  TCMurmur2AState = record
    Hash:           TMurmur32Sys;
    Remainder:      TMUR2RemainderBuffer;
    RemainderBytes: Integer;
    TotalBytes:     TMemSize;
  end;

Function InitialStateCMurmur2A: TCMurmur2AState; overload;

procedure BufferCMurmur2A(var State: TCMurmur2AState; const Buffer; Size: TMemSize); overload;

Function LastBufferCMurmur2A(var State: TCMurmur2AState; const Buffer; Size: TMemSize): TMurmur32;

//------------------------------------------------------------------------------

Function BufferCMurmur2A(const Buffer; Size: TMemSize): TMurmur32; overload;

Function AnsiStringCMurmur2A(const Str: AnsiString): TMurmur32;
Function WideStringCMurmur2A(const Str: WideString): TMurmur32;
Function StringCMurmur2A(const Str: String): TMurmur32;

Function StreamCMurmur2A(Stream: TStream; Count: Int64 = -1): TMurmur32;
Function FileCMurmur2A(const FileName: String): TMurmur32;

//------------------------------------------------------------------------------
type
  TCMurmur2AContext = type Pointer;

Function CMurmur2A_Init: TCMurmur2AContext; 
procedure CMurmur2A_Update(const Context: TCMurmur2AContext; const Buffer; Size: TMemSize);
Function CMurmur2A_Final(var Context: TCMurmur2AContext; const Buffer; Size: TMemSize): TMurmur32; overload;
Function CMurmur2A_Final(var Context: TCMurmur2AContext): TMurmur32; overload;
Function CMurmur2A_Hash(const Buffer; Size: TMemSize): TMurmur32;

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
    Auxiliary functions
===============================================================================}

Function SwapEndian(Hash: TMurmur32Sys): TMurmur32Sys; overload;
begin
Result := TMurmur32Sys(((Hash and $000000FF) shl 24) or ((Hash and $0000FF00) shl 8) or
                       ((Hash and $00FF0000) shr 8) or ((Hash and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TMurmur32): TMurmur32; overload;
begin
Result := TMurmur32(SwapEndian(TMurmur32Sys(Hash)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TMurmur64Sys): TMurmur64Sys; overload;
begin
UInt64Rec(Result).Lo := SwapEndian(UInt64Rec(Hash).Hi);
UInt64Rec(Result).Hi := SwapEndian(UInt64Rec(Hash).Lo);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TMurmur64): TMurmur64; overload;
begin
Result := TMurmur64(SwapEndian(TMurmur64Sys(Hash)));
end;

//==============================================================================

Function Murmur32Compare(const A,B: TMurmur32Sys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function Murmur32Same(const A,B: TMurmur32Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function Murmur32AsString(const Murmur: TMurmur32Sys): String;
begin
Result := IntToHex(Murmur,8);
end;

//------------------------------------------------------------------------------

Function Murmur32FromString(const Str: String): TMurmur32Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TMurmur32Sys(StrToInt(Str))
    else
      Result := TMurmur32Sys(StrToInt('$' + Str));
  end
else Result := TMurmur32HashBase.Murmur32ToSys(ZeroMurmur32);
end;

//------------------------------------------------------------------------------

Function Murmur64Compare(const A,B: TMurmur64Sys): Integer;
begin
Result := CompareUInt64(A,B);
end;

//------------------------------------------------------------------------------

Function Murmur64Same(const A,B: TMurmur64Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function Murmur64AsString(const Murmur: TMurmur64Sys): String;
begin
Result := IntToHex(Murmur,16);
end;

//------------------------------------------------------------------------------

Function Murmur64FromString(const Str: String): TMurmur64Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TMurmur64Sys(StrToInt64(Str))
    else
      Result := TMurmur64Sys(StrToInt64('$' + Str));
  end
else Result := TMurmur64HashBase.Murmur64ToSys(ZeroMurmur64);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TMurmur32HashBase
--------------------------------------------------------------------------------
===============================================================================}  
{===============================================================================
    TMurmur32HashBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur32HashBase - protected methods
-------------------------------------------------------------------------------}

Function TMurmur32HashBase.GetSeed: TMurmur32;
begin
Result := Murmur32FromSys(fSeed);
end;

//------------------------------------------------------------------------------

procedure TMurmur32HashBase.SetSeed(const Value: TMurmur32);
begin
fSeed := Murmur32ToSys(Value);
end;

//------------------------------------------------------------------------------

Function TMurmur32HashBase.GetMurmur32: TMurmur32;
begin
Result := Murmur32FromSys(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur32HashBase.Initialize;
begin
inherited;
fSeed := Murmur32ToSys(ZeroMurmur32);
fMurmurValue := Murmur32ToSys(ZeroMurmur32);
end;

{-------------------------------------------------------------------------------
    TMurmur32HashBase - public methods
-------------------------------------------------------------------------------}

class Function TMurmur32HashBase.Murmur32ToSys(Hash: TMurmur32): TMurmur32Sys;
begin
Result := TMurmur32Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur32HashBase.Murmur32FromSys(Hash: TMurmur32Sys): TMurmur32;
begin
Result := TMurmur32({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur32HashBase.Murmur32ToLE(Hash: TMurmur32): TMurmur32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur32HashBase.Murmur32ToBE(Hash: TMurmur32): TMurmur32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur32HashBase.Murmur32FromLE(Hash: TMurmur32): TMurmur32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur32HashBase.Murmur32FromBE(Hash: TMurmur32): TMurmur32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur32HashBase.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TMurmur32HashBase.HashSize: TMemSize;
begin
Result := SizeOf(TMurmur32);
end;

//------------------------------------------------------------------------------

class Function TMurmur32HashBase.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TMurmur32HashBase.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TMurmur32HashBase.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMurmur32HashBase then
  fMurmurValue := TMurmur32HashBase(Hash).Murmur32Sys
else
  raise EMUR2IncompatibleClass.CreateFmt('TMurmur32HashBase.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMurmur32HashBase.CreateAndInitFrom(Hash: TMurmur32);
begin
CreateAndInit;
fMurmurValue := Murmur32ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TMurmur32HashBase.Init;
begin
inherited;
fMurmurValue := Murmur32ToSys(InitialMurmur32);
end;

//------------------------------------------------------------------------------

Function TMurmur32HashBase.Compare(Hash: THashBase): Integer;
begin
If Hash is TMurmur32HashBase then
  Result := Murmur32Compare(fMurmurValue,TMurmur32HashBase(Hash).Murmur32Sys)
else
  raise EMUR2IncompatibleClass.CreateFmt('TMurmur32HashBase.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur32HashBase.Same(Hash: THashBase): Boolean;
begin
If Hash is TMurmur32HashBase then
  Result := Murmur32Same(fMurmurValue,TMurmur32HashBase(Hash).Murmur32Sys)
else
  raise EMUR2IncompatibleClass.CreateFmt('TMurmur32HashBase.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur32HashBase.AsString: String;
begin
Result := Murmur32AsString(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur32HashBase.FromString(const Str: String);
begin
fMurmurValue := Murmur32FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TMurmur32HashBase.FromStringDef(const Str: String; const Default: TMurmur32);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMurmurValue := Murmur32ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TMurmur32HashBase.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur32;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}Murmur32ToBE{$ELSE}Murmur32ToLE{$ENDIF}(Murmur32FromSys(fMurmurValue));
  heLittle: Temp := Murmur32ToLE(Murmur32FromSys(fMurmurValue));
  heBig:    Temp := Murmur32ToBE(Murmur32FromSys(fMurmurValue));
else
 {heDefault}
  Temp := Murmur32FromSys(fMurmurValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TMurmur32));
end;

//------------------------------------------------------------------------------

procedure TMurmur32HashBase.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur32;
begin
Temp := ZeroMurmur32;
Stream.ReadBuffer(Temp,SizeOf(TMurmur32));
case Endianness of
  heSystem: fMurmurValue := Murmur32ToSys({$IFDEF ENDIAN_BIG}Murmur32FromBE{$ELSE}Murmur32FromLE{$ENDIF}(Temp));
  heLittle: fMurmurValue := Murmur32ToSys(Murmur32FromLE(Temp));
  heBig:    fMurmurValue := Murmur32ToSys(Murmur32FromBE(Temp));
else
 {heDefault}
  fMurmurValue := Murmur32ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TMurmur64HashBase
--------------------------------------------------------------------------------
===============================================================================}  
{===============================================================================
    TMurmur64HashBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur64HashBase - protected methods
-------------------------------------------------------------------------------}

Function TMurmur64HashBase.GetSeed: TMurmur64;
begin
Result := Murmur64FromSys(fSeed);
end;

//------------------------------------------------------------------------------

procedure TMurmur64HashBase.SetSeed(const Value: TMurmur64);
begin
fSeed := Murmur64ToSys(Value);
end;

//------------------------------------------------------------------------------

Function TMurmur64HashBase.GetMurmur64: TMurmur64;
begin
Result := Murmur64FromSys(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur64HashBase.Initialize;
begin
inherited;
fSeed := Murmur64ToSys(ZeroMurmur64);
fMurmurValue := Murmur64ToSys(ZeroMurmur64);
end;

{-------------------------------------------------------------------------------
    TMurmur64HashBase - public methods
-------------------------------------------------------------------------------}

class Function TMurmur64HashBase.Murmur64ToSys(Hash: TMurmur64): TMurmur64Sys;
begin
Result := TMurmur64Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur64HashBase.Murmur64FromSys(Hash: TMurmur64Sys): TMurmur64;
begin
Result := TMurmur64({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur64HashBase.Murmur64ToLE(Hash: TMurmur64): TMurmur64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur64HashBase.Murmur64ToBE(Hash: TMurmur64): TMurmur64;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur64HashBase.Murmur64FromLE(Hash: TMurmur64): TMurmur64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur64HashBase.Murmur64FromBE(Hash: TMurmur64): TMurmur64;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur64HashBase.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TMurmur64HashBase.HashSize: TMemSize;
begin
Result := SizeOf(TMurmur64);
end;

//------------------------------------------------------------------------------

class Function TMurmur64HashBase.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TMurmur64HashBase.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TMurmur64HashBase.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMurmur64HashBase then
  fMurmurValue := TMurmur64HashBase(Hash).Murmur64Sys
else
  raise EMUR2IncompatibleClass.CreateFmt('TMurmur64HashBase.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMurmur64HashBase.CreateAndInitFrom(Hash: TMurmur64);
begin
CreateAndInit;
fMurmurValue := Murmur64ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TMurmur64HashBase.Init;
begin
inherited;
fMurmurValue := Murmur64ToSys(InitialMurmur64);
end;

//------------------------------------------------------------------------------

Function TMurmur64HashBase.Compare(Hash: THashBase): Integer;
begin
If Hash is TMurmur64HashBase then
  Result := Murmur64Compare(fMurmurValue,TMurmur64HashBase(Hash).Murmur64Sys)
else
  raise EMUR2IncompatibleClass.CreateFmt('TMurmur64HashBase.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur64HashBase.Same(Hash: THashBase): Boolean;
begin
If Hash is TMurmur64HashBase then
  Result := Murmur64Same(fMurmurValue,TMurmur64HashBase(Hash).Murmur64Sys)
else
  raise EMUR2IncompatibleClass.CreateFmt('TMurmur64HashBase.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur64HashBase.AsString: String;
begin
Result := Murmur64AsString(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur64HashBase.FromString(const Str: String);
begin
fMurmurValue := Murmur64FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TMurmur64HashBase.FromStringDef(const Str: String; const Default: TMurmur64);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMurmurValue := Murmur64ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TMurmur64HashBase.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur64;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}Murmur64ToBE{$ELSE}Murmur64ToLE{$ENDIF}(Murmur64FromSys(fMurmurValue));
  heLittle: Temp := Murmur64ToLE(Murmur64FromSys(fMurmurValue));
  heBig:    Temp := Murmur64ToBE(Murmur64FromSys(fMurmurValue));
else
 {heDefault}
  Temp := Murmur64FromSys(fMurmurValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TMurmur64));
end;

//------------------------------------------------------------------------------

procedure TMurmur64HashBase.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur64;
begin
Temp := ZeroMurmur64;
Stream.ReadBuffer(Temp,SizeOf(TMurmur64));
case Endianness of
  heSystem: fMurmurValue := Murmur64ToSys({$IFDEF ENDIAN_BIG}Murmur64FromBE{$ELSE}Murmur64FromLE{$ENDIF}(Temp));
  heLittle: fMurmurValue := Murmur64ToSys(Murmur64FromLE(Temp));
  heBig:    fMurmurValue := Murmur64ToSys(Murmur64FromBE(Temp));
else
 {heDefault}
  fMurmurValue := Murmur64ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur2Hash - main processing
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}
const
  MulConst32 = UInt32($5BD1E995);
  RotConst32 = 24;

//------------------------------------------------------------------------------  

Function Murmur2Process(const Seed: TMurmur32Sys; const Buffer; Size: TMemSize): TMurmur32Sys;
var
  Hash:         UInt32;
  CurrentData:  PUInt32;
  Temp:         UInt32; // internal processing temporary
begin
Hash := Seed xor UInt32(Size);
CurrentData := @Buffer;
while Size >= 4 do
  begin
    Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^);
    Temp := Temp * MulConst32;
    Temp := Temp xor (Temp shr RotConst32);
    Temp := Temp * MulConst32;
    Hash := Hash * MulConst32;
    Hash := hash xor Temp;
    Inc(CurrentData);
    Dec(Size,4);
  end;
Temp := 0;
If Size > 0 then
  begin
    Move(CurrentData^,Temp,Size);
    Hash := Hash xor {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Temp);
    Hash := Hash * MulConst32;
  end;
Hash := Hash xor (Hash shr 13);
Hash := Hash * MulConst32;
Hash := Hash xor (Hash shr 15);
Result := Hash;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
    TMurmur2Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur2Hash - protected methods
-------------------------------------------------------------------------------}

procedure TMurmur2Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
fMurmurValue := Murmur2Process(fSeed,Memory^,Count);
end;

{-------------------------------------------------------------------------------
    TMurmur2Hash - public methods
-------------------------------------------------------------------------------}

class Function TMurmur2Hash.HashName: String;
begin
Result := 'Murmur2';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur2AHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur2AHash - main processing
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

procedure Murmur2AMix(var H,K: UInt32);
begin
K := K * MulConst32;
K := K xor (K shr RotConst32);
K := K * MulConst32;
H := H * MulConst32;
H := H xor K;
end;

//------------------------------------------------------------------------------

Function Murmur2AProcess(const Seed: TMurmur32Sys; const Buffer; Size: TMemSize): TMurmur32Sys;
var
  Hash:         UInt32;
  CurrentData:  PUInt32;
  Remaining:    TMemSize;
  Temp:         UInt32;
begin
Hash := Seed;
CurrentData := @Buffer;
Remaining := Size;
while Remaining >= 4 do
  begin
    Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^);
    Murmur2AMix(Hash,Temp);
    Inc(CurrentData);
    Dec(Remaining,4);
  end;
Temp := 0;
If Remaining > 0 then
  begin
    Move(CurrentData^,Temp,Remaining);
  {$IFDEF ENDIAN_BIG}
    Temp := SwapEndian(Temp);
  {$ENDIF}
  end;
Murmur2AMix(Hash,Temp);
Temp := UInt32(Size);
Murmur2AMix(Hash,Temp);
Hash := Hash xor (Hash shr 13);
Hash := Hash * MulConst32;
Hash := Hash xor (Hash shr 15);
Result := Hash;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
    TMurmur2AHash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur2AHash - protected methods
-------------------------------------------------------------------------------}

procedure TMurmur2AHash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
fMurmurValue := Murmur2AProcess(fSeed,Memory^,Count);
end;

{-------------------------------------------------------------------------------
    TMurmur2AHash - public methods
-------------------------------------------------------------------------------}

class Function TMurmur2AHash.HashName: String;
begin
Result := 'Murmur2A';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur64AHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur64AHash - main processing
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function Murmur64AProcess(const Seed: TMurmur64Sys; const Buffer; Size: TMemSize): TMurmur64Sys;
const
  _MulConst = UInt64($C6A4A7935BD1E995);
  RotConst  = 47;
var
  MulConst:     UInt64;
  Hash:         UInt64;
  CurrentData:  PUInt64;
  Temp:         UInt64;
begin
{
  Following utter bullshit is here for old FPC (2.6.x), which fails with
  internal exception 200706094 on multiplication with 64bit unsigned constant.
}
MulConst := _MulConst;
Hash := Seed xor (Size * MulConst);
CurrentData := @Buffer;
while Size >= 8 do
  begin
    Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^);
    Temp := Temp * MulConst;
    Temp := Temp xor (Temp shr RotConst);
    Temp := Temp * MulConst;
    Hash := Hash xor Temp;
    Hash := Hash * MulConst;
    Inc(CurrentData);
    Dec(Size,8);
  end;
Temp := 0;
If Size > 0 then
  begin
    Move(CurrentData^,Temp,Size);
    Hash := Hash xor {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Temp);
    Hash := Hash * MulConst;
  end;
Hash := Hash xor (Hash shr RotConst);
Hash := Hash * MulConst;
Hash := Hash xor (Hash shr RotConst);
Result := Hash;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
    TMurmur64AHash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur64AHash - protected methods
-------------------------------------------------------------------------------}

procedure TMurmur64AHash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
fMurmurValue := Murmur64AProcess(fSeed,Memory^,Count);
end;

{-------------------------------------------------------------------------------
    TMurmur64AHash - public methods
-------------------------------------------------------------------------------}

class Function TMurmur64AHash.HashName: String;
begin
Result := 'Murmur64A';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur64BHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur64BHash - main processing
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function Murmur64BProcess(const Seed: TMurmur64Sys; const Buffer; Size: TMemSize): TMurmur64Sys;
var
  HashLo:       UInt32;
  HashHi:       UInt32;
  CurrentData:  PUInt32;
  TempLo:       UInt32;
  TempHi:       UInt32;
begin
HashLo := UInt64Rec(Seed).Lo xor UInt32(Size);
HashHi := UInt64Rec(Seed).Hi;
CurrentData := @Buffer;
while Size >= 8 do
  begin
    TempLo := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^);
    TempLo := TempLo * MulConst32;
    TempLo := TempLo xor (TempLo shr RotConst32);
    TempLo := TempLo * MulConst32;
    HashLo := HashLo * MulConst32;
    HashLo := HashLo xor TempLo;
    Inc(CurrentData);
    TempHi := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^);
    TempHi := TempHi * MulConst32;
    TempHi := TempHi xor (TempHi shr RotConst32);
    TempHi := TempHi * MulConst32;
    HashHi := HashHi * MulConst32;
    HashHi := HashHi xor TempHi;
    Inc(CurrentData);
    Dec(Size,8);
  end;
while Size >= 4 do
  begin
    TempLo := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^);
    TempLo := TempLo * MulConst32;
    TempLo := TempLo xor (TempLo shr RotConst32);
    TempLo := TempLo * MulConst32;
    HashLo := HashLo * MulConst32;
    HashLo := HashLo xor TempLo;
    Inc(CurrentData);
    Dec(Size,4);    
  end;
TempHi := 0;
If Size > 0 then
  begin
    Move(CurrentData^,TempHi,Size);
    HashHi := HashHi xor {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TempHi);
    HashHi := HashHi * MulConst32;
  end;
HashLo := HashLo xor (HashHi shr 18);
HashLo := HashLo * MulConst32;
HashHi := HashHi xor (HashLo shr 22);
HashHi := HashHi * MulConst32;
HashLo := HashLo xor (HashHi shr 17);
HashLo := HashLo * MulConst32;
HashHi := HashHi xor (HashLo shr 19);
HashHi := HashHi * MulConst32;
// yes, it is this way l -> h / h -> l
UInt64Rec(Result).Hi := HashLo;
UInt64Rec(Result).Lo := HashHi;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
    TMurmur64BHash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur64BHash - protected methods
-------------------------------------------------------------------------------}

procedure TMurmur64BHash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
fMurmurValue := Murmur64BProcess(fSeed,Memory^,Count);
end;

{-------------------------------------------------------------------------------
    TMurmur64BHash - public methods
-------------------------------------------------------------------------------}

class Function TMurmur64BHash.HashName: String;
begin
Result := 'Murmur64B';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TCMurmur2AHash
--------------------------------------------------------------------------------
===============================================================================}
const
  EmptyRemainder: TMUR2RemainderBuffer = (0,0,0,0);

{===============================================================================
    TCMurmur2AHash - main processing
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function CMurmur2AUpdate(const Hash: TMurmur32Sys; var Remainder: TMUR2RemainderBuffer; var RemainderBytes: Integer; const Buffer; Size: TMemSize): TMurmur32Sys;
var
  CurrentData:  PUInt8;
  Temp:         UInt32;
begin
Result := Hash;
If Size > 0 then
  begin
    CurrentData := @Buffer;
    RemainderBytes := RemainderBytes and 3;
    If RemainderBytes > 0 then
      begin
        while (Size > 0) and (RemainderBytes < 4) do begin
          Remainder[RemainderBytes] := CurrentData^;
          Inc(RemainderBytes);
          Inc(CurrentData);
          Dec(Size);
        end;
        If RemainderBytes >= 4 then
          begin
            RemainderBytes := 0;
            // use recursion to deal with the full remainder
            Result := CMurmur2AUpdate(Result,Remainder,RemainderBytes{0},Remainder,4);
          end
        else Exit;  // we have only expanded remainder
      end;
    while Size >= 4 do
      begin
        Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(PUInt32(CurrentData)^);
        Murmur2AMix(Result,Temp);
        Inc(CurrentData,4);
        Dec(Size,4);
      end;
    // store remainder
    If Size <> 0 then
      begin
        RemainderBytes := Integer(Size and 3);
        Move(CurrentData^,Remainder,RemainderBytes);
      end;      
  end;
end;

//------------------------------------------------------------------------------

Function CMurmur2AFinal(const Hash: TMurmur32Sys; var Remainder: TMUR2RemainderBuffer; RemainderBytes: Integer; TotalBytes: TMemSize): TMurmur32Sys;
var
  Temp: UInt32;
begin
Result := Hash;
If RemainderBytes > 0 then
  begin
    FillChar(Remainder[RemainderBytes],4 - RemainderBytes,0);
    Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(UInt32(Remainder));
  end
else Temp := 0;
Murmur2AMix(Result,Temp);
Temp := UInt32(TotalBytes);
Murmur2AMix(Result,Temp);
Result := Result xor (Result shr 13);
Result := Result * MulConst32;
Result := Result xor (Result shr 15);
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
    TCMurmur2AHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCMurmur2AHash - protected methods
-------------------------------------------------------------------------------}

Function TCMurmur2AHash.GetMurmur32: TMurmur32;
begin
Result := Murmur32FromSys(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TCMurmur2AHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fMurmurValue := CMurmur2AUpdate(fMurmurValue,fRemainder,fRemainderBytes,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TCMurmur2AHash.Initialize;
begin
inherited;
fMurmurValue := Murmur32ToSys(ZeroMurmur32);
end;

{-------------------------------------------------------------------------------
    TCMurmur2AHash - public methods
-------------------------------------------------------------------------------}

class Function TCMurmur2AHash.Murmur32ToSys(Hash: TMurmur32): TMurmur32Sys;
begin
Result := TMurmur32Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.Murmur32FromSys(Hash: TMurmur32Sys): TMurmur32;
begin
Result := TMurmur32({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.Murmur32ToLE(Hash: TMurmur32): TMurmur32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.Murmur32ToBE(Hash: TMurmur32): TMurmur32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.Murmur32FromLE(Hash: TMurmur32): TMurmur32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.Murmur32FromBE(Hash: TMurmur32): TMurmur32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.HashName: String;
begin
Result := 'CMurmur2A';
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.HashSize: TMemSize;
begin
Result := SizeOf(TMurmur32);
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TCMurmur2AHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TCMurmur2AHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCMurmur2AHash then
  begin
    fMurmurValue := TCMurmur2AHash(Hash).Murmur32Sys;
    fRemainder := TCMurmur2AHash(Hash).fRemainder;
    fRemainderBytes := TCMurmur2AHash(Hash).fRemainderBytes;
  end
else raise EMUR2IncompatibleClass.CreateFmt('TCMurmur2AHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCMurmur2AHash.CreateAndInitFrom(Hash: TMurmur32);
begin
CreateAndInit;
fMurmurValue := Murmur32ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TCMurmur2AHash.Init;
begin
inherited;
fMurmurValue := Murmur32ToSys(InitialMurmur32);
fRemainder := EmptyRemainder;
fRemainderBytes := 0;
end;

//------------------------------------------------------------------------------

procedure TCMurmur2AHash.Final;
begin
fMurmurValue := CMurmur2AFinal(fMurmurValue,fRemainder,fRemainderBytes,fProcessedBytes);
inherited;
end;

//------------------------------------------------------------------------------

Function TCMurmur2AHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TCMurmur2AHash then
  Result := Murmur32Compare(fMurmurValue,TCMurmur2AHash(Hash).Murmur32Sys)
else
  raise EMUR2IncompatibleClass.CreateFmt('TCMurmur2AHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCMurmur2AHash.Same(Hash: THashBase): Boolean;
begin
If Hash is TCMurmur2AHash then
  Result := Murmur32Same(fMurmurValue,TCMurmur2AHash(Hash).Murmur32Sys)
else
  raise EMUR2IncompatibleClass.CreateFmt('TCMurmur2AHash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCMurmur2AHash.AsString: String;
begin
Result := Murmur32AsString(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TCMurmur2AHash.FromString(const Str: String);
begin
fMurmurValue := Murmur32FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TCMurmur2AHash.FromStringDef(const Str: String; const Default: TMurmur32);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMurmurValue := Murmur32ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TCMurmur2AHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur32;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}Murmur32ToBE{$ELSE}Murmur32ToLE{$ENDIF}(Murmur32FromSys(fMurmurValue));
  heLittle: Temp := Murmur32ToLE(Murmur32FromSys(fMurmurValue));
  heBig:    Temp := Murmur32ToBE(Murmur32FromSys(fMurmurValue));
else
 {heDefault}
  Temp := Murmur32FromSys(fMurmurValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TMurmur32));
end;

//------------------------------------------------------------------------------

procedure TCMurmur2AHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur32;
begin
Temp := ZeroMurmur32;
Stream.ReadBuffer(Temp,SizeOf(TMurmur32));
case Endianness of
  heSystem: fMurmurValue := Murmur32ToSys({$IFDEF ENDIAN_BIG}Murmur32FromBE{$ELSE}Murmur32FromLE{$ENDIF}(Temp));
  heLittle: fMurmurValue := Murmur32ToSys(Murmur32FromLE(Temp));
  heBig:    fMurmurValue := Murmur32ToSys(Murmur32FromBE(Temp));
else
 {heDefault}
  fMurmurValue := Murmur32ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Common 32bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Common 32bit procedural interface - utility functions
-------------------------------------------------------------------------------}

Function Murmur32ToStr(const Hash: TMurmur32): String;
begin
Result := Murmur32AsString(TMurmur32HashBase.Murmur32ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToMurmur32(const Str: String): TMurmur32;
begin
Result := TMurmur32HashBase.Murmur32FromSys(Murmur32FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToMurmur32(const Str: String; out Hash: TMurmur32): Boolean;
begin
try
  Hash := TMurmur32HashBase.Murmur32FromSys(Murmur32FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToMurmur32Def(const Str: String; const Default: TMurmur32): TMurmur32;
begin
If not TryStrToMurmur32(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareMurmur32(const A,B: TMurmur32): Integer;
begin
Result := Murmur32Compare(TMurmur32HashBase.Murmur32ToSys(A),TMurmur32HashBase.Murmur32ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameMurmur32(const A,B: TMurmur32): Boolean;
begin
Result := Murmur32Same(TMurmur32HashBase.Murmur32ToSys(A),TMurmur32HashBase.Murmur32ToSys(B));
end;

{===============================================================================
    Common 64bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Common 64bit procedural interface - utility functions
-------------------------------------------------------------------------------}

Function Murmur64ToStr(const Hash: TMurmur64): String;
begin
Result := Murmur64AsString(TMurmur64HashBase.Murmur64ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToMurmur64(const Str: String): TMurmur64;
begin
Result := TMurmur64HashBase.Murmur64FromSys(Murmur64FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToMurmur64(const Str: String; out Hash: TMurmur64): Boolean;
begin
try
  Hash := TMurmur64HashBase.Murmur64FromSys(Murmur64FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToMurmur64Def(const Str: String; const Default: TMurmur64): TMurmur64;
begin
If not TryStrToMurmur64(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareMurmur64(const A,B: TMurmur64): Integer;
begin
Result := Murmur64Compare(TMurmur64HashBase.Murmur64ToSys(A),TMurmur64HashBase.Murmur64ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameMurmur64(const A,B: TMurmur64): Boolean;
begin
Result := Murmur64Same(TMurmur64HashBase.Murmur64ToSys(A),TMurmur64HashBase.Murmur64ToSys(B));
end;

{===============================================================================
    Murmur2 procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Murmur2 procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferMurmur2(const Seed: TMurmur32; const Buffer; Size: TMemSize): TMurmur32;
begin
Result := TMurmur32HashBase.Murmur32FromSys(Murmur2Process(TMurmur32HashBase.Murmur32ToSys(Seed),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferMurmur2(const Buffer; Size: TMemSize): TMurmur32;
begin
Result := TMurmur32HashBase.Murmur32FromSys(Murmur2Process(TMurmur32HashBase.Murmur32ToSys(InitialMurmur32),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringMurmur2(const Str: AnsiString): TMurmur32;
begin
Result := BufferMurmur2(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringMurmur2(const Str: WideString): TMurmur32;
begin
Result := BufferMurmur2(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringMurmur2(const Str: String): TMurmur32;
begin
Result := BufferMurmur2(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamMurmur2(Stream: TStream; Count: Int64 = -1): TMurmur32;
var
  Hasher: TMurmur2Hash;
begin
Hasher := TMurmur2Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMurmur2(const FileName: String): TMurmur32;
var
  Hasher: TMurmur2Hash;
begin
Hasher := TMurmur2Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Murmur2 procedural interface - context functions
-------------------------------------------------------------------------------}

Function Murmur2_Init: TMurmur2Context;
begin
Result := TMurmur2Context(TMurmur2Hash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur2_Init(const Seed: TMurmur32): TMurmur2Context;
begin
Result := TMurmur2Context(TMurmur2Hash.Create);
TMurmur2Hash(Result).Seed := Seed;
TMurmur2Hash(Result).Init;
end;

//------------------------------------------------------------------------------

procedure Murmur2_Update(const Context: TMurmur2Context; const Buffer; Size: TMemSize);
begin
TMurmur2Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Murmur2_Final(var Context: TMurmur2Context; const Buffer; Size: TMemSize): TMurmur32;
begin
Murmur2_Update(Context,Buffer,Size);
Result := Murmur2_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur2_Final(var Context: TMurmur2Context): TMurmur32;
begin
TMurmur2Hash(Context).Final;
Result := TMurmur2Hash(Context).Murmur32;
FreeAndNil(TMurmur2Hash(Context));
end;

//------------------------------------------------------------------------------

Function Murmur2_Hash(const Buffer; Size: TMemSize): TMurmur32;
begin
Result := BufferMurmur2(Buffer,Size);
end;

{===============================================================================
    Murmur2A procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Murmur2A procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferMurmur2A(const Seed: TMurmur32; const Buffer; Size: TMemSize): TMurmur32;
begin
Result := TMurmur32HashBase.Murmur32FromSys(Murmur2AProcess(TMurmur32HashBase.Murmur32ToSys(Seed),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferMurmur2A(const Buffer; Size: TMemSize): TMurmur32;
begin
Result := TMurmur32HashBase.Murmur32FromSys(Murmur2AProcess(TMurmur32HashBase.Murmur32ToSys(InitialMurmur32),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringMurmur2A(const Str: AnsiString): TMurmur32;
begin
Result := BufferMurmur2A(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringMurmur2A(const Str: WideString): TMurmur32;
begin
Result := BufferMurmur2A(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringMurmur2A(const Str: String): TMurmur32;
begin
Result := BufferMurmur2A(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamMurmur2A(Stream: TStream; Count: Int64 = -1): TMurmur32;
var
  Hasher: TMurmur2AHash;
begin
Hasher := TMurmur2AHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMurmur2A(const FileName: String): TMurmur32;
var
  Hasher: TMurmur2AHash;
begin
Hasher := TMurmur2AHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Murmur2A procedural interface - context functions
-------------------------------------------------------------------------------}

Function Murmur2A_Init: TMurmur2AContext;
begin
Result := TMurmur2AContext(TMurmur2AHash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur2A_Init(const Seed: TMurmur32): TMurmur2AContext;
begin
Result := TMurmur2AContext(TMurmur2AHash.Create);
TMurmur2AHash(Result).Seed := Seed;
TMurmur2AHash(Result).Init;
end;

//------------------------------------------------------------------------------

procedure Murmur2A_Update(const Context: TMurmur2AContext; const Buffer; Size: TMemSize);
begin
TMurmur2AHash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Murmur2A_Final(var Context: TMurmur2AContext; const Buffer; Size: TMemSize): TMurmur32;
begin
Murmur2A_Update(Context,Buffer,Size);
Result := Murmur2A_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur2A_Final(var Context: TMurmur2AContext): TMurmur32;
begin
TMurmur2AHash(Context).Final;
Result := TMurmur2AHash(Context).Murmur32;
FreeAndNil(TMurmur2AHash(Context));
end;

//------------------------------------------------------------------------------

Function Murmur2A_Hash(const Buffer; Size: TMemSize): TMurmur32;
begin
Result := BufferMurmur2A(Buffer,Size);
end;

{===============================================================================
    Murmur64A procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Murmur64A procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferMurmur64A(const Seed: TMurmur64; const Buffer; Size: TMemSize): TMurmur64;
begin
Result := TMurmur64HashBase.Murmur64FromSys(Murmur64AProcess(TMurmur64HashBase.Murmur64ToSys(Seed),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferMurmur64A(const Buffer; Size: TMemSize): TMurmur64;
begin
Result := TMurmur64HashBase.Murmur64FromSys(Murmur64AProcess(TMurmur64HashBase.Murmur64ToSys(InitialMurmur64),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringMurmur64A(const Str: AnsiString): TMurmur64;
begin
Result := BufferMurmur64A(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringMurmur64A(const Str: WideString): TMurmur64;
begin
Result := BufferMurmur64A(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringMurmur64A(const Str: String): TMurmur64;
begin
Result := BufferMurmur64A(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamMurmur64A(Stream: TStream; Count: Int64 = -1): TMurmur64;
var
  Hasher: TMurmur64AHash;
begin
Hasher := TMurmur64AHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur64;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMurmur64A(const FileName: String): TMurmur64;
var
  Hasher: TMurmur64AHash;
begin
Hasher := TMurmur64AHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur64;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Murmur64A procedural interface - context functions
-------------------------------------------------------------------------------}

Function Murmur64A_Init: TMurmur64AContext;
begin
Result := TMurmur64AContext(TMurmur64AHash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur64A_Init(const Seed: TMurmur64): TMurmur64AContext;
begin
Result := TMurmur64AContext(TMurmur64AHash.Create);
TMurmur64AHash(Result).Seed := Seed;
TMurmur64AHash(Result).Init;
end;

//------------------------------------------------------------------------------

procedure Murmur64A_Update(const Context: TMurmur64AContext; const Buffer; Size: TMemSize);
begin
TMurmur64AHash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Murmur64A_Final(var Context: TMurmur64AContext; const Buffer; Size: TMemSize): TMurmur64;
begin
Murmur64A_Update(Context,Buffer,Size);
Result := Murmur64A_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur64A_Final(var Context: TMurmur64AContext): TMurmur64;
begin
TMurmur64AHash(Context).Final;
Result := TMurmur64AHash(Context).Murmur64;
FreeAndNil(TMurmur64AHash(Context));
end;

//------------------------------------------------------------------------------

Function Murmur64A_Hash(const Buffer; Size: TMemSize): TMurmur64;
begin
Result := BufferMurmur64A(Buffer,Size);
end;

{===============================================================================
    Murmur64B procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Murmur64B procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferMurmur64B(const Seed: TMurmur64; const Buffer; Size: TMemSize): TMurmur64;
begin
Result := TMurmur64HashBase.Murmur64FromSys(Murmur64BProcess(TMurmur64HashBase.Murmur64ToSys(Seed),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferMurmur64B(const Buffer; Size: TMemSize): TMurmur64;
begin
Result := TMurmur64HashBase.Murmur64FromSys(Murmur64BProcess(TMurmur64HashBase.Murmur64ToSys(InitialMurmur64),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringMurmur64B(const Str: AnsiString): TMurmur64;
begin
Result := BufferMurmur64B(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringMurmur64B(const Str: WideString): TMurmur64;
begin
Result := BufferMurmur64B(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringMurmur64B(const Str: String): TMurmur64;
begin
Result := BufferMurmur64B(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamMurmur64B(Stream: TStream; Count: Int64 = -1): TMurmur64;
var
  Hasher: TMurmur64BHash;
begin
Hasher := TMurmur64BHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur64;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMurmur64B(const FileName: String): TMurmur64;
var
  Hasher: TMurmur64BHash;
begin
Hasher := TMurmur64BHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur64;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Murmur64B procedural interface - context functions
-------------------------------------------------------------------------------}

Function Murmur64B_Init: TMurmur64BContext;
begin
Result := TMurmur64BContext(TMurmur64BHash.CreateAndInit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur64B_Init(const Seed: TMurmur64): TMurmur64BContext;
begin
Result := TMurmur64BContext(TMurmur64BHash.Create);
TMurmur64BHash(Result).Seed := Seed;
TMurmur64BHash(Result).Init;
end;

//------------------------------------------------------------------------------

procedure Murmur64B_Update(const Context: TMurmur64BContext; const Buffer; Size: TMemSize);
begin
TMurmur64BHash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Murmur64B_Final(var Context: TMurmur64BContext; const Buffer; Size: TMemSize): TMurmur64;
begin
Murmur64B_Update(Context,Buffer,Size);
Result := Murmur64B_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur64B_Final(var Context: TMurmur64BContext): TMurmur64;
begin
TMurmur64BHash(Context).Final;
Result := TMurmur64BHash(Context).Murmur64;
FreeAndNil(TMurmur64BHash(Context));
end;

//------------------------------------------------------------------------------

Function Murmur64B_Hash(const Buffer; Size: TMemSize): TMurmur64;
begin
Result := BufferMurmur64B(Buffer,Size);
end;

{===============================================================================
    CMurmur2A procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    CMurmur2A procedural interface - continuous hashing
-------------------------------------------------------------------------------}

Function InitialStateCMurmur2A: TCMurmur2AState;
begin
Result.Hash := TCMurmur2AHash.Murmur32ToSys(InitialMurmur32);
Result.Remainder := EmptyRemainder;
Result.RemainderBytes := 0;
Result.TotalBytes := 0;
end;

//------------------------------------------------------------------------------

procedure BufferCMurmur2A(var State: TCMurmur2AState; const Buffer; Size: TMemSize);
begin
State.Hash := CMurmur2AUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
State.TotalBytes := State.TotalBytes + Size;
end;

//------------------------------------------------------------------------------

Function LastBufferCMurmur2A(var State: TCMurmur2AState; const Buffer; Size: TMemSize): TMurmur32;
begin
If Size > 0 then
  begin
    State.Hash := CMurmur2AUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
    State.TotalBytes := State.TotalBytes + Size;
  end;
Result := TCMurmur2AHash.Murmur32FromSys(CMurmur2AFinal(State.Hash,State.Remainder,State.RemainderBytes,State.TotalBytes));
end;

{-------------------------------------------------------------------------------
    CMurmur2A procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferCMurmur2A(const Buffer; Size: TMemSize): TMurmur32;
var
  State:  TCMurmur2AState;
begin
State := InitialStateCMurmur2A;
Result := LastBufferCMurmur2A(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringCMurmur2A(const Str: AnsiString): TMurmur32;
begin
Result := BufferCMurmur2A(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringCMurmur2A(const Str: WideString): TMurmur32;
begin
Result := BufferCMurmur2A(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringCMurmur2A(const Str: String): TMurmur32;
begin
Result := BufferCMurmur2A(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamCMurmur2A(Stream: TStream; Count: Int64 = -1): TMurmur32;
var
  Hasher: TCMurmur2AHash;
begin
Hasher := TCMurmur2AHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileCMurmur2A(const FileName: String): TMurmur32;
var
  Hasher: TCMurmur2AHash;
begin
Hasher := TCMurmur2AHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    CMurmur2A procedural interface - context functions
-------------------------------------------------------------------------------}

Function CMurmur2A_Init: TCMurmur2AContext;
begin
Result := TCMurmur2AContext(TCMurmur2AHash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure CMurmur2A_Update(const Context: TCMurmur2AContext; const Buffer; Size: TMemSize);
begin
TCMurmur2AHash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function CMurmur2A_Final(var Context: TCMurmur2AContext; const Buffer; Size: TMemSize): TMurmur32;
begin
CMurmur2A_Update(Context,Buffer,Size);
Result := CMurmur2A_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CMurmur2A_Final(var Context: TCMurmur2AContext): TMurmur32;
begin
TCMurmur2AHash(Context).Final;
Result := TCMurmur2AHash(Context).Murmur32;
FreeAndNil(TCMurmur2AHash(Context));
end;

//------------------------------------------------------------------------------

Function CMurmur2A_Hash(const Buffer; Size: TMemSize): TMurmur32;
begin
Result := BufferCMurmur2A(Buffer,Size);
end;

end.

