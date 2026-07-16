{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Murmur3 hash (also known as MurmurHash3)

    All main variants are implemented, including PMurHash32 that is based on
    Murmur3A.

  Version 1.0 (2026-07-16)

  Last change 2026-07-16

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Murmur3Hash

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
unit Murmur3Hash;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
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
  Auxtypes, HashBase, UInt64Utils;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EMUR3Exception = class(EHashException);

  EMUR3IncompatibleClass = class(EMUR3Exception);

{===============================================================================
    Common types and constants
===============================================================================}
type
  TMUR3UInt128 = packed record case Integer of
  {$IFDEF ENDIAN_BIG}
    0: (Hi,Lo:  UInt64);
    1: (HiRec,
        LoRec:  UInt64Rec);
  {$ELSE}
    0: (Lo,Hi:  UInt64);
    1: (LoRec,
        HiRec:  UInt64Rec);
  {$ENDIF}
    2: (QWords: array[0..1] of UInt64);
    3: (DWords: array[0..3] of UInt32);
    4: (Words:  array[0..7] of UInt16);
    5: (Bytes:  array[0..15] of UInt8);
  end;

//------------------------------------------------------------------------------
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

  TMurmur128 = packed array[0..15] of UInt8;
  PMurmur128 = ^TMurmur128;

  TMurmur128Sys = TMUR3UInt128;
  PMurmur128Sys = ^TMurmur128Sys;

const
  InitialMurmur32:  TMurmur32  = ($00,$00,$00,$00);
  InitialMurmur128: TMurmur128 = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

  ZeroMurmur32:   TMurmur32  = (0,0,0,0);
  ZeroMurmur128:  TMurmur128 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur3AHash
--------------------------------------------------------------------------------
===============================================================================}
type
  TMUR32Remainder = packed array[0..3] of UInt8;

{===============================================================================
    TMurmur3AHash - class declaration
===============================================================================}
type
  TMurmur3AHash = class(TStreamHash)
  protected
    fMurmurValue:     TMurmur32Sys;
    fRemainder:       TMUR32Remainder;
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
                                   TPMurHash32
--------------------------------------------------------------------------------
===============================================================================}
{
  TPMurHash32

  This hash is identical to Murmur3A, but it is implemented for progressive
  processing. Since I have already implemented Murmur3A that way, let's just
  reuse it.
}
{===============================================================================
    TPMurHash32 - class declaration
===============================================================================}
type
  TPMurHash32Hash = class(TMurmur3AHash)
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TMurmur128HashBase
--------------------------------------------------------------------------------
===============================================================================}
type
  TMUR128Remainder = packed array[0..15] of UInt8;

{===============================================================================
    TMurmur128HashBase - class declaration
===============================================================================}
type
  TMurmur128HashBase = class(TStreamHash)
  protected
    fMurmurValue:     TMurmur128Sys;
    fRemainder:       TMUR128Remainder;
    fRemainderBytes:  Integer;
    Function GetMurmur128: TMurmur128; virtual;
    procedure Initialize; override;
  public
    class Function Murmur128ToSys(Hash: TMurmur128): TMurmur128Sys; virtual;
    class Function Murmur128FromSys(Hash: TMurmur128Sys): TMurmur128; virtual;
    class Function Murmur128ToLE(Hash: TMurmur128): TMurmur128; virtual;
    class Function Murmur128ToBE(Hash: TMurmur128): TMurmur128; virtual;
    class Function Murmur128FromLE(Hash: TMurmur128): TMurmur128; virtual;
    class Function Murmur128FromBE(Hash: TMurmur128): TMurmur128; virtual;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMurmur128); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMurmur128); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Murmur128: TMurmur128 read GetMurmur128;
    property Murmur128Sys: TMurmur128Sys read fMurmurValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur3CHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur3CHash - class declaration
===============================================================================}
type
  TMurmur3CHash = class(TMurmur128HashBase)
  protected
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
  public
    class Function HashName: String; override;
    procedure Final; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TMurmur3FHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur3FHash - class declaration
===============================================================================}
type
  TMurmur3FHash = class(TMurmur128HashBase)
  protected
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
  public
    class Function HashName: String; override;
    procedure Final; override;
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

//------------------------------------------------------------------------------
type
  TMurmur32State = record
    Hash:           TMurmur32Sys;
    Remainder:      TMUR32Remainder;
    RemainderBytes: Integer;
    TotalBytes:     TMemSize;
  end;

{===============================================================================
    Common 128bit procedural interface - declaration
===============================================================================}

Function Murmur128ToStr(const Hash: TMurmur128): String;
Function StrToMurmur128(const Str: String): TMurmur128;
Function TryStrToMurmur128(const Str: String; out Hash: TMurmur128): Boolean;
Function StrToMurmur128Def(const Str: String; const Default: TMurmur128): TMurmur128;

Function CompareMurmur128(const A,B: TMurmur128): Integer;
Function SameMurmur128(const A,B: TMurmur128): Boolean;

//------------------------------------------------------------------------------
type
  TMurmur128State = record
    Hash:           TMurmur128Sys;
    Remainder:      TMUR128Remainder;
    RemainderBytes: Integer;
    TotalBytes:     TMemSize;
  end;

{===============================================================================
    Murmur3A procedural interface - declaration
===============================================================================}
type
  TMurmur3AState = type TMurmur32State;

Function InitialStateMurmur3A: TMurmur3AState; overload;

procedure BufferMurmur3A(var State: TMurmur3AState; const Buffer; Size: TMemSize); overload;

Function LastBufferMurmur3A(var State: TMurmur3AState; const Buffer; Size: TMemSize): TMurmur32;

//------------------------------------------------------------------------------

Function BufferMurmur3A(const Buffer; Size: TMemSize): TMurmur32; overload;

Function AnsiStringMurmur3A(const Str: AnsiString): TMurmur32;
Function WideStringMurmur3A(const Str: WideString): TMurmur32;
Function StringMurmur3A(const Str: String): TMurmur32;

Function StreamMurmur3A(Stream: TStream; Count: Int64 = -1): TMurmur32;
Function FileMurmur3A(const FileName: String): TMurmur32;

//------------------------------------------------------------------------------
type
  TMurmur3AContext = type Pointer;

Function Murmur3A_Init: TMurmur3AContext;
procedure Murmur3A_Update(const Context: TMurmur3AContext; const Buffer; Size: TMemSize);
Function Murmur3A_Final(var Context: TMurmur3AContext; const Buffer; Size: TMemSize): TMurmur32; overload;
Function Murmur3A_Final(var Context: TMurmur3AContext): TMurmur32; overload;
Function Murmur3A_Hash(const Buffer; Size: TMemSize): TMurmur32;

{===============================================================================
    PMurHash32 procedural interface - declaration
===============================================================================}
type
  TPMurHash32State = type TMurmur32State;

Function InitialStatePMurHash32: TPMurHash32State; overload;

procedure BufferPMurHash32(var State: TPMurHash32State; const Buffer; Size: TMemSize); overload;

Function LastBufferPMurHash32(var State: TPMurHash32State; const Buffer; Size: TMemSize): TMurmur32;

//------------------------------------------------------------------------------

Function BufferPMurHash32(const Buffer; Size: TMemSize): TMurmur32; overload;

Function AnsiStringPMurHash32(const Str: AnsiString): TMurmur32;
Function WideStringPMurHash32(const Str: WideString): TMurmur32;
Function StringPMurHash32(const Str: String): TMurmur32;

Function StreamPMurHash32(Stream: TStream; Count: Int64 = -1): TMurmur32;
Function FilePMurHash32(const FileName: String): TMurmur32;

//------------------------------------------------------------------------------
type
  TPMurHash32Context = type Pointer;

Function PMurHash32_Init: TPMurHash32Context;
procedure PMurHash32_Update(const Context: TPMurHash32Context; const Buffer; Size: TMemSize);
Function PMurHash32_Final(var Context: TPMurHash32Context; const Buffer; Size: TMemSize): TMurmur32; overload;
Function PMurHash32_Final(var Context: TPMurHash32Context): TMurmur32; overload;
Function PMurHash32_Hash(const Buffer; Size: TMemSize): TMurmur32;

{===============================================================================
    Murmur3C procedural interface - declaration
===============================================================================}
type
  TMurmur3CState = type TMurmur128State;

Function InitialStateMurmur3C: TMurmur3CState; overload;

procedure BufferMurmur3C(var State: TMurmur3CState; const Buffer; Size: TMemSize); overload;

Function LastBufferMurmur3C(var State: TMurmur3CState; const Buffer; Size: TMemSize): TMurmur128;

//------------------------------------------------------------------------------

Function BufferMurmur3C(const Buffer; Size: TMemSize): TMurmur128; overload;

Function AnsiStringMurmur3C(const Str: AnsiString): TMurmur128;
Function WideStringMurmur3C(const Str: WideString): TMurmur128;
Function StringMurmur3C(const Str: String): TMurmur128;

Function StreamMurmur3C(Stream: TStream; Count: Int64 = -1): TMurmur128;
Function FileMurmur3C(const FileName: String): TMurmur128;

//------------------------------------------------------------------------------
type
  TMurmur3CContext = type Pointer;

Function Murmur3C_Init: TMurmur3CContext;
procedure Murmur3C_Update(const Context: TMurmur3CContext; const Buffer; Size: TMemSize);
Function Murmur3C_Final(var Context: TMurmur3CContext; const Buffer; Size: TMemSize): TMurmur128; overload;
Function Murmur3C_Final(var Context: TMurmur3CContext): TMurmur128; overload;
Function Murmur3C_Hash(const Buffer; Size: TMemSize): TMurmur128;

{===============================================================================
    Murmur3F procedural interface - declaration
===============================================================================}
type
  TMurmur3FState = type TMurmur128State;

Function InitialStateMurmur3F: TMurmur3FState; overload;

procedure BufferMurmur3F(var State: TMurmur3FState; const Buffer; Size: TMemSize); overload;

Function LastBufferMurmur3F(var State: TMurmur3FState; const Buffer; Size: TMemSize): TMurmur128;

//------------------------------------------------------------------------------

Function BufferMurmur3F(const Buffer; Size: TMemSize): TMurmur128; overload;

Function AnsiStringMurmur3F(const Str: AnsiString): TMurmur128;
Function WideStringMurmur3F(const Str: WideString): TMurmur128;
Function StringMurmur3F(const Str: String): TMurmur128;

Function StreamMurmur3F(Stream: TStream; Count: Int64 = -1): TMurmur128;
Function FileMurmur3F(const FileName: String): TMurmur128;

//------------------------------------------------------------------------------
type
  TMurmur3FContext = type Pointer;

Function Murmur3F_Init: TMurmur3FContext;
procedure Murmur3F_Update(const Context: TMurmur3FContext; const Buffer; Size: TMemSize);
Function Murmur3F_Final(var Context: TMurmur3FContext; const Buffer; Size: TMemSize): TMurmur128; overload;
Function Murmur3F_Final(var Context: TMurmur3FContext): TMurmur128; overload;
Function Murmur3F_Hash(const Buffer; Size: TMemSize): TMurmur128;

implementation

uses
  SysUtils;

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

{$IFNDEF FPC}
const
  FPC_VERSION = Integer(0);
{$ENDIF}  
{$UNDEF FPC_VersionPre3}
{$IFDEF FPC}
  {$IF FPC_VERSION < 3}
    {$DEFINE FPC_VersionPre3}
  {$IFEND}
{$ENDIF}

{===============================================================================
    Common auxiliary functions
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

Function SwapEndian(Hash: TMurmur128Sys): TMurmur128Sys; overload;
begin
Result.DWords[0] := SwapEndian(Hash.DWords[3]);
Result.DWords[1] := SwapEndian(Hash.DWords[2]);
Result.DWords[2] := SwapEndian(Hash.DWords[1]);
Result.DWords[3] := SwapEndian(Hash.DWords[0]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TMurmur128): TMurmur128; overload;
begin
Result := TMurmur128(SwapEndian(TMurmur128Sys(Hash)));
end;

//==============================================================================

Function RotL32(const Value: UInt32; Shift: Integer): UInt32;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := UInt32(Value shl Shift) or (Value shr (32 - Shift));
end;

//------------------------------------------------------------------------------

Function RotL64(const Value: UInt64; Shift: Integer): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := UInt64(Value shl Shift) or (Value shr (64 - Shift));
end;

//------------------------------------------------------------------------------
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function FinalMix32(const H: UInt32): UInt32;
begin
Result := H;
Result := Result xor (Result shr 16);
Result := Result * UInt32($85EBCA6B);
Result := Result xor (Result shr 13);
Result := Result * UInt32($C2B2AE35);
Result := Result xor (Result shr 16);
end;

//------------------------------------------------------------------------------

Function FinalMix64(const H: UInt64): UInt64;
{$IFDEF FPC_VersionPre3}
var
  MulConst: UInt64;
{$ENDIF}
begin
Result := H;
Result := Result xor (Result shr 33);
{$IFDEF FPC_VersionPre3}
MulConst := UInt64($FF51AFD7ED558CCD);
Result := Result * MulConst;
{$ELSE}
Result := Result * UInt64($FF51AFD7ED558CCD);
{$ENDIF}
Result := Result xor (Result shr 33);
{$IFDEF FPC_VersionPre3}
MulConst := UInt64($C4CEB9FE1A85EC53);
Result := Result * MulConst;
{$ELSE}
Result := Result * UInt64($C4CEB9FE1A85EC53);
{$ENDIF}
Result := Result xor (Result shr 33);
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur3AHash
--------------------------------------------------------------------------------
===============================================================================}
const
  EmptyRemainder32: TMUR32Remainder = (0,0,0,0);

{===============================================================================
    TMurmur3AHash - auxiliary functions
===============================================================================}

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
else Result := TMurmur3AHash.Murmur32ToSys(ZeroMurmur32);
end;

{===============================================================================
    TMurmur3AHash - main processing
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}
const
  Mul32Const1 = UInt32($CC9E2D51);
  Mul32Const2 = UInt32($1B873593);

//------------------------------------------------------------------------------

Function Murmur3AUpdate(const Hash: TMurmur32Sys; var Remainder: TMUR32Remainder; var RemainderBytes: Integer; const Buffer; Size: TMemSize): TMurmur32Sys;
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
            Result := Murmur3AUpdate(Result,Remainder,RemainderBytes{0},Remainder,4);
          end
        else Exit;  // we have only expanded remainder
      end;
    while Size >= 4 do
      begin
        Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(PUInt32(CurrentData)^);
        Temp := Temp * Mul32Const1;
        Temp := RotL32(Temp,15);
        Temp := Temp * Mul32Const2;
        Result := Result xor Temp;
        Result := RotL32(Result,13);
        Result := (Result * 5) + UInt32($E6546B64);
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

Function Murmur3AFinal(const Hash: TMurmur32Sys; var Remainder: TMUR32Remainder; RemainderBytes: Integer; TotalBytes: TMemSize): TMurmur32Sys;
var
  Temp: UInt32;
begin
Result := Hash; 
If RemainderBytes > 0 then
  begin
    FillChar(Remainder[RemainderBytes],4 - RemainderBytes,0);
    Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(UInt32(Remainder));
    Temp := Temp * Mul32Const1;
    Temp := RotL32(Temp,15);
    Temp := Temp * Mul32Const2;
    Result := Result xor Temp;
  end;
Result := Result xor UInt32(TotalBytes);
Result := FinalMix32(Result);
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
    TMurmur3AHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur3AHash - protected methods
-------------------------------------------------------------------------------}

Function TMurmur3AHash.GetMurmur32: TMurmur32;
begin
Result := Murmur32FromSys(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur3AHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fMurmurValue := Murmur3AUpdate(fMurmurValue,fRemainder,fRemainderBytes,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TMurmur3AHash.Initialize;
begin
inherited;
fMurmurValue := Murmur32ToSys(ZeroMurmur32);
fRemainder := EmptyRemainder32;
fRemainderBytes := 0;
end;

{-------------------------------------------------------------------------------
    TMurmur3AHash - public methods
-------------------------------------------------------------------------------}

class Function TMurmur3AHash.Murmur32ToSys(Hash: TMurmur32): TMurmur32Sys;
begin
Result := TMurmur32Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.Murmur32FromSys(Hash: TMurmur32Sys): TMurmur32;
begin
Result := TMurmur32({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.Murmur32ToLE(Hash: TMurmur32): TMurmur32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.Murmur32ToBE(Hash: TMurmur32): TMurmur32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.Murmur32FromLE(Hash: TMurmur32): TMurmur32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.Murmur32FromBE(Hash: TMurmur32): TMurmur32;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.HashName: String;
begin
Result := 'Murmur3A';
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.HashSize: TMemSize;
begin
Result := SizeOf(TMurmur32);
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TMurmur3AHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TMurmur3AHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMurmur3AHash then
  begin
    fMurmurValue := TMurmur3AHash(Hash).Murmur32Sys;
    fRemainder := TMurmur3AHash(Hash).fRemainder;
    fRemainderBytes := TMurmur3AHash(Hash).fRemainderBytes;
  end
else raise EMUR3IncompatibleClass.CreateFmt('TMurmur3AHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMurmur3AHash.CreateAndInitFrom(Hash: TMurmur32);
begin
CreateAndInit;
fMurmurValue := Murmur32ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TMurmur3AHash.Init;
begin
inherited;
fMurmurValue := Murmur32ToSys(InitialMurmur32);
fRemainder := EmptyRemainder32;
fRemainderBytes := 0;
end;

//------------------------------------------------------------------------------

procedure TMurmur3AHash.Final;
begin
fMurmurValue := Murmur3AFinal(fMurmurValue,fRemainder,fRemainderBytes,fProcessedBytes);
inherited;
end;

//------------------------------------------------------------------------------

Function TMurmur3AHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TMurmur3AHash then
  Result := Murmur32Compare(fMurmurValue,TMurmur3AHash(Hash).Murmur32Sys)
else
  raise EMUR3IncompatibleClass.CreateFmt('TMurmur3AHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur3AHash.Same(Hash: THashBase): Boolean;
begin
If Hash is TMurmur3AHash then
  Result := Murmur32Same(fMurmurValue,TMurmur3AHash(Hash).Murmur32Sys)
else
  raise EMUR3IncompatibleClass.CreateFmt('TMurmur3AHash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur3AHash.AsString: String;
begin
Result := Murmur32AsString(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur3AHash.FromString(const Str: String);
begin
fMurmurValue := Murmur32FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TMurmur3AHash.FromStringDef(const Str: String; const Default: TMurmur32);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMurmurValue := Murmur32ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TMurmur3AHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
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

procedure TMurmur3AHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
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
                                   TPMurHash32
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    TPMurHash32 - piblic methods
-------------------------------------------------------------------------------}

class Function TPMurHash32Hash.HashName: String;
begin
Result := 'PMurHash32';
end;


{===============================================================================
--------------------------------------------------------------------------------
                               TMurmur128HashBase
--------------------------------------------------------------------------------
===============================================================================}
const
  EmptyRemainder128: TMUR128Remainder = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

{===============================================================================
    TMurmur128HashBase - auxiliary functions
===============================================================================}

Function Murmur128Compare(const A,B: TMurmur128Sys): Integer;
begin
Result := CompareUInt64(A.Hi,B.Hi);
If Result = 0 then
  Result := CompareUInt64(A.Lo,B.Lo);
end;

//------------------------------------------------------------------------------

Function Murmur128Same(const A,B: TMurmur128Sys): Boolean;
begin
If A.Hi = B.Hi then
  Result := A.Lo = B.Lo
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function Murmur128AsString(const Murmur: TMurmur128Sys): String;
begin
Result := IntToHex(Murmur.Hi,16) + IntToHex(Murmur.Lo,16);
end;

//------------------------------------------------------------------------------

Function Murmur128FromString(const Str: String): TMurmur128Sys;
var
  TempStr:  String;
  i:        Integer;
  TempRes:  TMurmur128 absolute Result;
begin
If Length(Str) < (SizeOf(TMurmur128) * 2) then
  TempStr := StringOfChar('0',(SizeOf(TMurmur128) * 2) - Length(Str)) + Str
else If Length(Str) > (SizeOf(TMurmur128) * 2) then
  TempStr := Copy(Str,Length(Str) - Pred(SizeOf(TMurmur128) * 2),SizeOf(TMurmur128) * 2)
else
  TempStr := Str;
For i := Low(TempRes) to High(TempRes) do
  TempRes[High(TempRes) - i] := UInt8(StrToInt('$' + Copy(TempStr,(i * 2) + 1,2)));
{$IFDEF ENDIAN_BIG}
Result := SwapEndian(Result);
{$ENDIF}
end;

{===============================================================================
    TMurmur128HashBase class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur128HashBase - protected methods
-------------------------------------------------------------------------------}

Function TMurmur128HashBase.GetMurmur128: TMurmur128;
begin
Result := Murmur128FromSys(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur128HashBase.Initialize;
begin
inherited;
fMurmurValue := Murmur128ToSys(ZeroMurmur128);
fRemainder := EmptyRemainder128;
fRemainderBytes := 0;
end;

{-------------------------------------------------------------------------------
    TMurmur128HashBase - public methods
-------------------------------------------------------------------------------}

class Function TMurmur128HashBase.Murmur128ToSys(Hash: TMurmur128): TMurmur128Sys;
begin
Result := TMurmur128Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur128HashBase.Murmur128FromSys(Hash: TMurmur128Sys): TMurmur128;
begin
Result := TMurmur128({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur128HashBase.Murmur128ToLE(Hash: TMurmur128): TMurmur128;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur128HashBase.Murmur128ToBE(Hash: TMurmur128): TMurmur128;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur128HashBase.Murmur128FromLE(Hash: TMurmur128): TMurmur128;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur128HashBase.Murmur128FromBE(Hash: TMurmur128): TMurmur128;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur128HashBase.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TMurmur128HashBase.HashSize: TMemSize;
begin
Result := SizeOf(TMurmur128);
end;

//------------------------------------------------------------------------------

class Function TMurmur128HashBase.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TMurmur128HashBase.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TMurmur128HashBase.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMurmur128HashBase then
  begin
    fMurmurValue := TMurmur128HashBase(Hash).Murmur128Sys;
    fRemainder := TMurmur128HashBase(Hash).fRemainder;
    fRemainderBytes := TMurmur128HashBase(Hash).fRemainderBytes;
  end
else raise EMUR3IncompatibleClass.CreateFmt('TMurmur128HashBase.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMurmur128HashBase.CreateAndInitFrom(Hash: TMurmur128);
begin
CreateAndInit;
fMurmurValue := Murmur128ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TMurmur128HashBase.Init;
begin
inherited;
fMurmurValue := Murmur128ToSys(InitialMurmur128);
fRemainder := EmptyRemainder128;
fRemainderBytes := 0;
end;

//------------------------------------------------------------------------------

Function TMurmur128HashBase.Compare(Hash: THashBase): Integer;
begin
If Hash is TMurmur128HashBase then
  Result := Murmur128Compare(fMurmurValue,TMurmur128HashBase(Hash).Murmur128Sys)
else
  raise EMUR3IncompatibleClass.CreateFmt('TMurmur128HashBase.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur128HashBase.Same(Hash: THashBase): Boolean;
begin
If Hash is TMurmur128HashBase then
  Result := Murmur128Same(fMurmurValue,TMurmur128HashBase(Hash).Murmur128Sys)
else
  raise EMUR3IncompatibleClass.CreateFmt('TMurmur128HashBase.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur128HashBase.AsString: String;
begin
Result := Murmur128AsString(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur128HashBase.FromString(const Str: String);
begin
fMurmurValue := Murmur128FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TMurmur128HashBase.FromStringDef(const Str: String; const Default: TMurmur128);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMurmurValue := Murmur128ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TMurmur128HashBase.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur128;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}Murmur128ToBE{$ELSE}Murmur128ToLE{$ENDIF}(Murmur128FromSys(fMurmurValue));
  heLittle: Temp := Murmur128ToLE(Murmur128FromSys(fMurmurValue));
  heBig:    Temp := Murmur128ToBE(Murmur128FromSys(fMurmurValue));
else
 {heDefault}
  Temp := Murmur128FromSys(fMurmurValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TMurmur128));
end;

//------------------------------------------------------------------------------

procedure TMurmur128HashBase.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur128;
begin
Temp := ZeroMurmur128;
Stream.ReadBuffer(Temp,SizeOf(TMurmur128));
case Endianness of
  heSystem: fMurmurValue := Murmur128ToSys({$IFDEF ENDIAN_BIG}Murmur128FromBE{$ELSE}Murmur128FromLE{$ENDIF}(Temp));
  heLittle: fMurmurValue := Murmur128ToSys(Murmur128FromLE(Temp));
  heBig:    fMurmurValue := Murmur128ToSys(Murmur128FromBE(Temp));
else
 {heDefault}
  fMurmurValue := Murmur128ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur3CHash                                                                
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur3CHash - main processing
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}
const
  Mul128_32Const1 = UInt32($239B961B);
  Mul128_32Const2 = UInt32($AB0E9789);
  Mul128_32Const3 = UInt32($38B34AE5);
  Mul128_32Const4 = UInt32($A1E38B93);

//------------------------------------------------------------------------------

Function Murmur3CUpdate(const Hash: TMurmur128Sys; var Remainder: TMUR128Remainder; var RemainderBytes: Integer; const Buffer; Size: TMemSize): TMurmur128Sys;
var
  CurrentData:  PUInt8;
  Temp:         TMurmur128Sys;
begin
Result := Hash;
If Size > 0 then
  begin
    CurrentData := @Buffer;
    RemainderBytes := RemainderBytes and 15;
    If RemainderBytes > 0 then
      begin
        while (Size > 0) and (RemainderBytes < 16) do begin
          Remainder[RemainderBytes] := CurrentData^;
          Inc(RemainderBytes);
          Inc(CurrentData);
          Dec(Size);
        end;
        If RemainderBytes >= 16 then
          begin
            RemainderBytes := 0;
            // use recursion to deal with the full remainder
            Result := Murmur3CUpdate(Result,Remainder,RemainderBytes{0},Remainder,16);
          end
        else Exit;  // we have only expanded remainder
      end;
    while Size >= 16 do
      begin
        Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(PMurmur128Sys(CurrentData)^);

        Temp.LoRec.Lo := Temp.LoRec.Lo * Mul128_32Const1;
        Temp.LoRec.Lo := RotL32(Temp.LoRec.Lo,15);
        Temp.LoRec.Lo := Temp.LoRec.Lo * Mul128_32Const2;
        Result.LoRec.Lo := Result.LoRec.Lo xor Temp.LoRec.Lo;

        Result.LoRec.Lo := RotL32(Result.LoRec.Lo,19);
        Result.LoRec.Lo := Result.LoRec.Lo + Result.LoRec.Hi;
        Result.LoRec.Lo := (Result.LoRec.Lo * 5) + UInt32($561CCD1B);

        Temp.LoRec.Hi := Temp.LoRec.Hi * Mul128_32Const2;
        Temp.LoRec.Hi := RotL32(Temp.LoRec.Hi,16);
        Temp.LoRec.Hi := Temp.LoRec.Hi * Mul128_32Const3;
        Result.LoRec.Hi := Result.LoRec.Hi xor Temp.LoRec.Hi;

        Result.LoRec.Hi := RotL32(Result.LoRec.Hi,17);
        Result.LoRec.Hi := Result.LoRec.Hi + Result.HiRec.Lo;
        Result.LoRec.Hi := (Result.LoRec.Hi * 5) + UInt32($0BCAA747);

        Temp.HiRec.Lo := Temp.HiRec.Lo * Mul128_32Const3;
        Temp.HiRec.Lo := RotL32(Temp.HiRec.Lo,17);
        Temp.HiRec.Lo := Temp.HiRec.Lo * Mul128_32Const4;
        Result.HiRec.Lo := Result.HiRec.Lo xor Temp.HiRec.Lo;

        Result.HiRec.Lo := RotL32(Result.HiRec.Lo,15);
        Result.HiRec.Lo := Result.HiRec.Lo + Result.HiRec.Hi;
        Result.HiRec.Lo := (Result.HiRec.Lo * 5) + UInt32($96CD1C35);

        Temp.HiRec.Hi := Temp.HiRec.Hi * Mul128_32Const4;
        Temp.HiRec.Hi := RotL32(Temp.HiRec.Hi,18);
        Temp.HiRec.Hi := Temp.HiRec.Hi * Mul128_32Const1;
        Result.HiRec.Hi := Result.HiRec.Hi xor Temp.HiRec.Hi;

        Result.HiRec.Hi := RotL32(Result.HiRec.Hi,13);
        Result.HiRec.Hi := Result.HiRec.Hi + Result.LoRec.Lo;
        Result.HiRec.Hi := (Result.HiRec.Hi * 5) + UInt32($32AC3B17);

        Inc(CurrentData,16);
        Dec(Size,16);
      end;
    // store remainder
    If Size <> 0 then
      begin
        RemainderBytes := Integer(Size and 15);
        Move(CurrentData^,Remainder,RemainderBytes);
      end;      
  end;
end;

//------------------------------------------------------------------------------

Function Murmur3CFinal(const Hash: TMurmur128Sys; var Remainder: TMUR128Remainder; RemainderBytes: Integer; TotalBytes: TMemSize): TMurmur128Sys;
var
  Temp: TMurmur128Sys;
begin
Result := Hash;
If RemainderBytes > 0 then
  begin
    FillChar(Remainder[RemainderBytes],16 - RemainderBytes,0);
    Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TMurmur128Sys(Remainder));
    If RemainderBytes > 0 then
      begin
        Temp.LoRec.Lo := Temp.LoRec.Lo * Mul128_32Const1;
        Temp.LoRec.Lo := RotL32(Temp.LoRec.Lo,15);
        Temp.LoRec.Lo := Temp.LoRec.Lo * Mul128_32Const2;
        Result.LoRec.Lo := Result.LoRec.Lo xor Temp.LoRec.Lo;
      end;
    If RemainderBytes > 4 then
      begin
        Temp.LoRec.Hi := Temp.LoRec.Hi * Mul128_32Const2;
        Temp.LoRec.Hi := RotL32(Temp.LoRec.Hi,16);
        Temp.LoRec.Hi := Temp.LoRec.Hi * Mul128_32Const3;
        Result.LoRec.Hi := Result.LoRec.Hi xor Temp.LoRec.Hi;
      end;
    If RemainderBytes > 8 then
      begin
        Temp.HiRec.Lo := Temp.HiRec.Lo * Mul128_32Const3;
        Temp.HiRec.Lo := RotL32(Temp.HiRec.Lo,17);
        Temp.HiRec.Lo := Temp.HiRec.Lo * Mul128_32Const4;
        Result.HiRec.Lo := Result.HiRec.Lo xor Temp.HiRec.Lo;
      end;
    If RemainderBytes > 12 then
      begin
        Temp.HiRec.Hi := Temp.HiRec.Hi * Mul128_32Const4;
        Temp.HiRec.Hi := RotL32(Temp.HiRec.Hi,18);
        Temp.HiRec.Hi := Temp.HiRec.Hi * Mul128_32Const1;
        Result.HiRec.Hi := Result.HiRec.Hi xor Temp.HiRec.Hi;
      end;
  end;

Result.LoRec.Lo := Result.LoRec.Lo xor UInt32(TotalBytes);
Result.LoRec.Hi := Result.LoRec.Hi xor UInt32(TotalBytes);
Result.HiRec.Lo := Result.HiRec.Lo xor UInt32(TotalBytes);
Result.HiRec.Hi := Result.HiRec.Hi xor UInt32(TotalBytes);

Result.LoRec.Lo := Result.LoRec.Lo + Result.LoRec.Hi;
Result.LoRec.Lo := Result.LoRec.Lo + Result.HiRec.Lo;
Result.LoRec.Lo := Result.LoRec.Lo + Result.HiRec.Hi;
Result.LoRec.Hi := Result.LoRec.Hi + Result.LoRec.Lo;
Result.HiRec.Lo := Result.HiRec.Lo + Result.LoRec.Lo;
Result.HiRec.Hi := Result.HiRec.Hi + Result.LoRec.Lo;

Result.LoRec.Lo := FinalMix32(Result.LoRec.Lo);
Result.LoRec.Hi := FinalMix32(Result.LoRec.Hi);
Result.HiRec.Lo := FinalMix32(Result.HiRec.Lo);
Result.HiRec.Hi := FinalMix32(Result.HiRec.Hi);

Result.LoRec.Lo := Result.LoRec.Lo + Result.LoRec.Hi;
Result.LoRec.Lo := Result.LoRec.Lo + Result.HiRec.Lo;
Result.LoRec.Lo := Result.LoRec.Lo + Result.HiRec.Hi;
Result.LoRec.Hi := Result.LoRec.Hi + Result.LoRec.Lo;
Result.HiRec.Lo := Result.HiRec.Lo + Result.LoRec.Lo;
Result.HiRec.Hi := Result.HiRec.Hi + Result.LoRec.Lo;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
    TMurmur3CHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur3CHash - protected methods
-------------------------------------------------------------------------------}

procedure TMurmur3CHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fMurmurValue := Murmur3CUpdate(fMurmurValue,fRemainder,fRemainderBytes,Buffer,Size);
end;

{-------------------------------------------------------------------------------
    TMurmur3CHash - public methods
-------------------------------------------------------------------------------}

class Function TMurmur3CHash.HashName: String;
begin
Result := 'Murmur3C';
end;

//------------------------------------------------------------------------------

procedure TMurmur3CHash.Final;
begin
fMurmurValue := Murmur3CFinal(fMurmurValue,fRemainder,fRemainderBytes,fProcessedBytes);
inherited;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur3FHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur3FHash - main processing
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}
{$IFDEF FPC_VersionPre3}
const
  _Mul128_64Const1 = UInt64($87C37B91114253D5);
  _Mul128_64Const2 = UInt64($4CF5AD432745937F);

var
  Mul128_64Const1: UInt64 = _Mul128_64Const1;
  Mul128_64Const2: UInt64 = _Mul128_64Const2;
{$ELSE}
const
  Mul128_64Const1 = UInt64($87C37B91114253D5);
  Mul128_64Const2 = UInt64($4CF5AD432745937F);
{$ENDIF}

//------------------------------------------------------------------------------

Function Murmur3FUpdate(const Hash: TMurmur128Sys; var Remainder: TMUR128Remainder; var RemainderBytes: Integer; const Buffer; Size: TMemSize): TMurmur128Sys;
var
  CurrentData:  PUInt8;
  Temp:         TMurmur128Sys;
begin
Result := Hash;
If Size > 0 then
  begin
    CurrentData := @Buffer;
    RemainderBytes := RemainderBytes and 15;
    If RemainderBytes > 0 then
      begin
        while (Size > 0) and (RemainderBytes < 16) do begin
          Remainder[RemainderBytes] := CurrentData^;
          Inc(RemainderBytes);
          Inc(CurrentData);
          Dec(Size);
        end;
        If RemainderBytes >= 16 then
          begin
            RemainderBytes := 0;
            // use recursion to deal with the full remainder
            Result := Murmur3FUpdate(Result,Remainder,RemainderBytes{0},Remainder,16);
          end
        else Exit;  // we have only expanded remainder
      end;
    while Size >= 16 do
      begin
        Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(PMurmur128Sys(CurrentData)^);

        Temp.Lo := Temp.Lo * Mul128_64Const1;
        Temp.Lo := RotL64(Temp.Lo,31);
        Temp.Lo := Temp.Lo * Mul128_64Const2;
        Result.Lo := Result.Lo xor Temp.Lo;

        Result.Lo := RotL64(Result.Lo,27);
        Result.Lo := Result.Lo + Result.Hi;
        Result.Lo := (Result.Lo * 5) + UInt64($52DCE729);

        Temp.Hi := Temp.Hi * Mul128_64Const2;
        Temp.Hi := RotL64(Temp.Hi,33);
        Temp.Hi := Temp.Hi * Mul128_64Const1;
        Result.Hi := Result.Hi xor Temp.Hi;

        Result.Hi := RotL64(Result.Hi,31);
        Result.Hi := Result.Hi + Result.Lo;
        Result.Hi := (Result.Hi * 5) + UInt64($38495AB5);

        Inc(CurrentData,16);
        Dec(Size,16);
      end;
    // store remainder
    If Size <> 0 then
      begin
        RemainderBytes := Integer(Size and 15);
        Move(CurrentData^,Remainder,RemainderBytes);
      end;      
  end;
end;

//------------------------------------------------------------------------------

Function Murmur3FFinal(const Hash: TMurmur128Sys; var Remainder: TMUR128Remainder; RemainderBytes: Integer; TotalBytes: TMemSize): TMurmur128Sys;
var
  Temp: TMurmur128Sys;
begin
Result := Hash;
If RemainderBytes > 0 then
  begin
    FillChar(Remainder[RemainderBytes],16 - RemainderBytes,0);
    Temp := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TMurmur128Sys(Remainder));
    If RemainderBytes > 0 then
      begin
        Temp.Lo := Temp.Lo * Mul128_64Const1;
        Temp.Lo := RotL64(Temp.Lo,31);
        Temp.Lo := Temp.Lo * Mul128_64Const2;
        Result.Lo := Result.Lo xor Temp.Lo;
      end;
    If RemainderBytes > 8 then
      begin
        Temp.Hi := Temp.Hi * Mul128_64Const2;
        Temp.Hi := RotL64(Temp.Hi,33);
        Temp.Hi := Temp.Hi * Mul128_64Const1;
        Result.Hi := Result.Hi xor Temp.Hi;
      end;
  end;
  
Result.Lo := Result.Lo xor UInt64(TotalBytes);
Result.Hi := Result.Hi xor UInt64(TotalBytes);

Result.Lo := Result.Lo + Result.Hi;
Result.Hi := Result.Hi + Result.Lo;

Result.Lo := FinalMix64(Result.Lo);
Result.Hi := FinalMix64(Result.Hi);

Result.Lo := Result.Lo + Result.Hi;
Result.Hi := Result.Hi + Result.Lo;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
    TMurmur3FHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur3FHash - protected methods
-------------------------------------------------------------------------------}

procedure TMurmur3FHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fMurmurValue := Murmur3FUpdate(fMurmurValue,fRemainder,fRemainderBytes,Buffer,Size);
end;

{-------------------------------------------------------------------------------
    TMurmur3FHash - public methods
-------------------------------------------------------------------------------}

class Function TMurmur3FHash.HashName: String;
begin
Result := 'Murmur3F';
end;

//------------------------------------------------------------------------------

procedure TMurmur3FHash.Final;
begin
fMurmurValue := Murmur3FFinal(fMurmurValue,fRemainder,fRemainderBytes,fProcessedBytes);
inherited;
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
Result := Murmur32AsString(TMurmur3AHash.Murmur32ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToMurmur32(const Str: String): TMurmur32;
begin
Result := TMurmur3AHash.Murmur32FromSys(Murmur32FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToMurmur32(const Str: String; out Hash: TMurmur32): Boolean;
begin
try
  Hash := TMurmur3AHash.Murmur32FromSys(Murmur32FromString(Str));
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
Result := Murmur32Compare(TMurmur3AHash.Murmur32ToSys(A),TMurmur3AHash.Murmur32ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameMurmur32(const A,B: TMurmur32): Boolean;
begin
Result := Murmur32Same(TMurmur3AHash.Murmur32ToSys(A),TMurmur3AHash.Murmur32ToSys(B));
end;

{===============================================================================
    Common 128bit procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Common 128bit procedural interface - utility functions
-------------------------------------------------------------------------------}

Function Murmur128ToStr(const Hash: TMurmur128): String;
begin
Result := Murmur128AsString(TMurmur128HashBase.Murmur128ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToMurmur128(const Str: String): TMurmur128;
begin
Result := TMurmur128HashBase.Murmur128FromSys(Murmur128FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToMurmur128(const Str: String; out Hash: TMurmur128): Boolean;
begin
try
  Hash := TMurmur128HashBase.Murmur128FromSys(Murmur128FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToMurmur128Def(const Str: String; const Default: TMurmur128): TMurmur128;
begin
If not TryStrToMurmur128(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareMurmur128(const A,B: TMurmur128): Integer;
begin
Result := Murmur128Compare(TMurmur128HashBase.Murmur128ToSys(A),TMurmur128HashBase.Murmur128ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameMurmur128(const A,B: TMurmur128): Boolean;
begin
Result := Murmur128Same(TMurmur128HashBase.Murmur128ToSys(A),TMurmur128HashBase.Murmur128ToSys(B));
end;

{===============================================================================
    Murmur3A procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Murmur3A procedural interface - continuous hashing
-------------------------------------------------------------------------------}

Function InitialStateMurmur3A: TMurmur3AState;
begin
Result.Hash := TMurmur3AHash.Murmur32ToSys(InitialMurmur32);
Result.Remainder := EmptyRemainder32;
Result.RemainderBytes := 0;
Result.TotalBytes := 0;
end;

//------------------------------------------------------------------------------

procedure BufferMurmur3A(var State: TMurmur3AState; const Buffer; Size: TMemSize);
begin
State.Hash := Murmur3AUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
State.TotalBytes := State.TotalBytes + Size;
end;

//------------------------------------------------------------------------------

Function LastBufferMurmur3A(var State: TMurmur3AState; const Buffer; Size: TMemSize): TMurmur32;
begin
If Size > 0 then
  begin
    State.Hash := Murmur3AUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
    State.TotalBytes := State.TotalBytes + Size;
  end;
Result := TMurmur3AHash.Murmur32FromSys(Murmur3AFinal(State.Hash,State.Remainder,State.RemainderBytes,State.TotalBytes));
end;

{-------------------------------------------------------------------------------
    Murmur3A procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferMurmur3A(const Buffer; Size: TMemSize): TMurmur32;
var
  State:  TMurmur3AState;
begin
State := InitialStateMurmur3A;
Result := LastBufferMurmur3A(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringMurmur3A(const Str: AnsiString): TMurmur32;
begin
Result := BufferMurmur3A(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringMurmur3A(const Str: WideString): TMurmur32;
begin
Result := BufferMurmur3A(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringMurmur3A(const Str: String): TMurmur32;
begin
Result := BufferMurmur3A(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamMurmur3A(Stream: TStream; Count: Int64 = -1): TMurmur32;
var
  Hasher: TMurmur3AHash;
begin
Hasher := TMurmur3AHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMurmur3A(const FileName: String): TMurmur32;
var
  Hasher: TMurmur3AHash;
begin
Hasher := TMurmur3AHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Murmur3 procedural interface - context functions
-------------------------------------------------------------------------------}

Function Murmur3A_Init: TMurmur3AContext;
begin
Result := TMurmur3AContext(TMurmur3AHash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure Murmur3A_Update(const Context: TMurmur3AContext; const Buffer; Size: TMemSize);
begin
TMurmur3AHash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Murmur3A_Final(var Context: TMurmur3AContext; const Buffer; Size: TMemSize): TMurmur32;
begin
Murmur3A_Update(Context,Buffer,Size);
Result := Murmur3A_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur3A_Final(var Context: TMurmur3AContext): TMurmur32;
begin
TMurmur3AHash(Context).Final;
Result := TMurmur3AHash(Context).Murmur32;
FreeAndNil(TMurmur3AHash(Context));
end;

//------------------------------------------------------------------------------

Function Murmur3A_Hash(const Buffer; Size: TMemSize): TMurmur32;
begin
Result := BufferMurmur3A(Buffer,Size);
end;

{===============================================================================
    PMurHash32 procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    PMurHash32 procedural interface - continuous hashing
-------------------------------------------------------------------------------}

Function InitialStatePMurHash32: TPMurHash32State;
begin
Result.Hash := TPMurHash32Hash.Murmur32ToSys(InitialMurmur32);
Result.Remainder := EmptyRemainder32;
Result.RemainderBytes := 0;
Result.TotalBytes := 0;
end;

//------------------------------------------------------------------------------

procedure BufferPMurHash32(var State: TPMurHash32State; const Buffer; Size: TMemSize);
begin
State.Hash := Murmur3AUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
State.TotalBytes := State.TotalBytes + Size;
end;

//------------------------------------------------------------------------------

Function LastBufferPMurHash32(var State: TPMurHash32State; const Buffer; Size: TMemSize): TMurmur32;
begin
If Size > 0 then
  begin
    State.Hash := Murmur3AUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
    State.TotalBytes := State.TotalBytes + Size;
  end;
Result := TPMurHash32Hash.Murmur32FromSys(Murmur3AFinal(State.Hash,State.Remainder,State.RemainderBytes,State.TotalBytes));
end;

{-------------------------------------------------------------------------------
    PMurHash32 procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferPMurHash32(const Buffer; Size: TMemSize): TMurmur32;
var
  State:  TPMurHash32State;
begin
State := InitialStatePMurHash32;
Result := LastBufferPMurHash32(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringPMurHash32(const Str: AnsiString): TMurmur32;
begin
Result := BufferPMurHash32(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringPMurHash32(const Str: WideString): TMurmur32;
begin
Result := BufferPMurHash32(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringPMurHash32(const Str: String): TMurmur32;
begin
Result := BufferPMurHash32(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamPMurHash32(Stream: TStream; Count: Int64 = -1): TMurmur32;
var
  Hasher: TPMurHash32Hash;
begin
Hasher := TPMurHash32Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FilePMurHash32(const FileName: String): TMurmur32;
var
  Hasher: TPMurHash32Hash;
begin
Hasher := TPMurHash32Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur32;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Murmur3 procedural interface - context functions
-------------------------------------------------------------------------------}

Function PMurHash32_Init: TPMurHash32Context;
begin
Result := TPMurHash32Context(TPMurHash32Hash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure PMurHash32_Update(const Context: TPMurHash32Context; const Buffer; Size: TMemSize);
begin
TPMurHash32Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function PMurHash32_Final(var Context: TPMurHash32Context; const Buffer; Size: TMemSize): TMurmur32;
begin
PMurHash32_Update(Context,Buffer,Size);
Result := PMurHash32_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PMurHash32_Final(var Context: TPMurHash32Context): TMurmur32;
begin
TPMurHash32Hash(Context).Final;
Result := TPMurHash32Hash(Context).Murmur32;
FreeAndNil(TPMurHash32Hash(Context));
end;

//------------------------------------------------------------------------------

Function PMurHash32_Hash(const Buffer; Size: TMemSize): TMurmur32;
begin
Result := BufferPMurHash32(Buffer,Size);
end;

{===============================================================================
    Murmur3C procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Murmur3C procedural interface - continuous hashing
-------------------------------------------------------------------------------}

Function InitialStateMurmur3C: TMurmur3CState;
begin
Result.Hash := TMurmur128HashBase.Murmur128ToSys(InitialMurmur128);
Result.Remainder := EmptyRemainder128;
Result.RemainderBytes := 0;
Result.TotalBytes := 0;
end;

//------------------------------------------------------------------------------

procedure BufferMurmur3C(var State: TMurmur3CState; const Buffer; Size: TMemSize);
begin
State.Hash := Murmur3CUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
State.TotalBytes := State.TotalBytes + Size;
end;

//------------------------------------------------------------------------------

Function LastBufferMurmur3C(var State: TMurmur3CState; const Buffer; Size: TMemSize): TMurmur128;
begin
If Size > 0 then
  begin
    State.Hash := Murmur3CUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
    State.TotalBytes := State.TotalBytes + Size;
  end;
Result := TMurmur128HashBase.Murmur128FromSys(Murmur3CFinal(State.Hash,State.Remainder,State.RemainderBytes,State.TotalBytes));
end;

{-------------------------------------------------------------------------------
    Murmur3C procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferMurmur3C(const Buffer; Size: TMemSize): TMurmur128;
var
  State:  TMurmur3CState;
begin
State := InitialStateMurmur3C;
Result := LastBufferMurmur3C(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringMurmur3C(const Str: AnsiString): TMurmur128;
begin
Result := BufferMurmur3C(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringMurmur3C(const Str: WideString): TMurmur128;
begin
Result := BufferMurmur3C(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringMurmur3C(const Str: String): TMurmur128;
begin
Result := BufferMurmur3C(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamMurmur3C(Stream: TStream; Count: Int64 = -1): TMurmur128;
var
  Hasher: TMurmur3CHash;
begin
Hasher := TMurmur3CHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur128;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMurmur3C(const FileName: String): TMurmur128;
var
  Hasher: TMurmur3CHash;
begin
Hasher := TMurmur3CHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur128;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Murmur3 procedural interface - context functions
-------------------------------------------------------------------------------}

Function Murmur3C_Init: TMurmur3CContext;
begin
Result := TMurmur3CContext(TMurmur3CHash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure Murmur3C_Update(const Context: TMurmur3CContext; const Buffer; Size: TMemSize);
begin
TMurmur3CHash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Murmur3C_Final(var Context: TMurmur3CContext; const Buffer; Size: TMemSize): TMurmur128;
begin
Murmur3C_Update(Context,Buffer,Size);
Result := Murmur3C_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur3C_Final(var Context: TMurmur3CContext): TMurmur128;
begin
TMurmur3CHash(Context).Final;
Result := TMurmur3CHash(Context).Murmur128;
FreeAndNil(TMurmur3CHash(Context));
end;

//------------------------------------------------------------------------------

Function Murmur3C_Hash(const Buffer; Size: TMemSize): TMurmur128;
begin
Result := BufferMurmur3C(Buffer,Size);
end;

{===============================================================================
    Murmur3F procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Murmur3F procedural interface - continuous hashing
-------------------------------------------------------------------------------}

Function InitialStateMurmur3F: TMurmur3FState;
begin
Result.Hash := TMurmur128HashBase.Murmur128ToSys(InitialMurmur128);
Result.Remainder := EmptyRemainder128;
Result.RemainderBytes := 0;
Result.TotalBytes := 0;
end;

//------------------------------------------------------------------------------

procedure BufferMurmur3F(var State: TMurmur3FState; const Buffer; Size: TMemSize);
begin
State.Hash := Murmur3FUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
State.TotalBytes := State.TotalBytes + Size;
end;

//------------------------------------------------------------------------------

Function LastBufferMurmur3F(var State: TMurmur3FState; const Buffer; Size: TMemSize): TMurmur128;
begin
If Size > 0 then
  begin
    State.Hash := Murmur3FUpdate(State.Hash,State.Remainder,State.RemainderBytes,Buffer,Size);
    State.TotalBytes := State.TotalBytes + Size;
  end;
Result := TMurmur128HashBase.Murmur128FromSys(Murmur3FFinal(State.Hash,State.Remainder,State.RemainderBytes,State.TotalBytes));
end;

{-------------------------------------------------------------------------------
    Murmur3F procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferMurmur3F(const Buffer; Size: TMemSize): TMurmur128;
var
  State:  TMurmur3FState;
begin
State := InitialStateMurmur3F;
Result := LastBufferMurmur3F(State,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringMurmur3F(const Str: AnsiString): TMurmur128;
begin
Result := BufferMurmur3F(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringMurmur3F(const Str: WideString): TMurmur128;
begin
Result := BufferMurmur3F(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringMurmur3F(const Str: String): TMurmur128;
begin
Result := BufferMurmur3F(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamMurmur3F(Stream: TStream; Count: Int64 = -1): TMurmur128;
var
  Hasher: TMurmur3FHash;
begin
Hasher := TMurmur3FHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur128;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMurmur3F(const FileName: String): TMurmur128;
var
  Hasher: TMurmur3FHash;
begin
Hasher := TMurmur3FHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur128;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Murmur3 procedural interface - context functions
-------------------------------------------------------------------------------}

Function Murmur3F_Init: TMurmur3FContext;
begin
Result := TMurmur3FContext(TMurmur3FHash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure Murmur3F_Update(const Context: TMurmur3FContext; const Buffer; Size: TMemSize);
begin
TMurmur3FHash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Murmur3F_Final(var Context: TMurmur3FContext; const Buffer; Size: TMemSize): TMurmur128;
begin
Murmur3F_Update(Context,Buffer,Size);
Result := Murmur3F_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur3F_Final(var Context: TMurmur3FContext): TMurmur128;
begin
TMurmur3FHash(Context).Final;
Result := TMurmur3FHash(Context).Murmur128;
FreeAndNil(TMurmur3FHash(Context));
end;

//------------------------------------------------------------------------------

Function Murmur3F_Hash(const Buffer; Size: TMemSize): TMurmur128;
begin
Result := BufferMurmur3F(Buffer,Size);
end;

end.
