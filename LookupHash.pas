{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Lookup Hash (by Robert John Jenkins)

    Both Lookup2 and Lookup3 hashes are implemented, but note that Lookup2 was
    not tested for correctness (ie. whether it calculates correct values for
    any given data).
    Version of Lookup3 providing 64bit hash ia also implemented, here under
    the name Lookup64.

  Version 1.1 (2026-07-10)

  Last change 2026-07-10

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.LookupHash

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
unit LookupHash;

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
  ELHException = class(EHashException);

  ELHIncompatibleClass = class(ELHException);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TLookup(64) are, in memory, always ordered from least significant
  byte to most significant byte (little endian).

  Type TLookup(64)Sys has no such guarantee and its endianness is system-
  dependent.

  To convert the hash in default ordering to a required specific ordering,
  use method Lookup(64)ToLE for little endian and Lookup(64)ToBE for big
  endian.
}
type
  TLookup = packed array[0..3] of UInt8;
  PLookup = ^TLookup;

  TLookupSys = UInt32;
  PLookupSys = ^TLookupSys;

  TLookup64 = packed array[0..7] of UInt8;
  PLookup64 = ^TLookup64;

  TLookup64Sys = UInt64;
  PLookup64Sys = ^TLookup64Sys;

const
  InitialLookup:   TLookup   = ($00,$00,$00,$00);
  InitialLookup64: TLookup64 = ($00,$00,$00,$00,$00,$00,$00,$00);

  ZeroLookup:   TLookup   = (0,0,0,0);
  ZeroLookup64: TLookup64 = (0,0,0,0,0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                 TLookupHashBase
--------------------------------------------------------------------------------
===============================================================================}
{
  Technically, lookup2 could be implemented as block hash, but lookup3 cannot
  (see its declaration for why) - so, to make things more consistent, both
  hashes are implemented as buffered.
}
{===============================================================================
    TLookupHashBase - class declaration
===============================================================================}
type
  TLookupHashBase = class(TBufferHash)
  protected
    fLookupValue: TLookupSys;
    Function GetLookup: TLookup; virtual;
    procedure Initialize; override;
  public
    class Function LookupToSys(Hash: TLookup): TLookupSys; virtual;
    class Function LookupFromSys(Hash: TLookupSys): TLookup; virtual;
    class Function LookupToLE(Hash: TLookup): TLookup; virtual;
    class Function LookupToBE(Hash: TLookup): TLookup; virtual;
    class Function LookupFromLE(Hash: TLookup): TLookup; virtual;
    class Function LookupFromBE(Hash: TLookup): TLookup; virtual;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TLookup); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TLookup); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Lookup: TLookup read GetLookup;
    property LookupSys: TLookupSys read fLookupValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TLookup2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TLookup2Hash - class declaration
===============================================================================}
type
  TLookup2Hash = class(TLookupHashBase)
  protected
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TLookup3Hash
--------------------------------------------------------------------------------
===============================================================================}
{
  Lookup3 is using length of the hashed data to initialize the internal state,
  therefore it is not possible to do streaming and the implementation must use
  buffered approach.
}
{===============================================================================
    TLookup3Hash - class declaration
===============================================================================}
type
  TLookup3Hash = class(TLookupHashBase)
  protected
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TLookup64Hash
--------------------------------------------------------------------------------
===============================================================================}
{
  Lookup64 is based on Lookup3. In fact, it uses the same code and only
  extracts larger part of the internal state to build the result.
}
{===============================================================================
    TLookup64Hash - class declaration
===============================================================================}
type
  TLookup64Hash = class(TBufferHash)
  protected
    fLookup64Value: TLookup64Sys;
    Function GetLookup64: TLookup64; virtual;
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
    procedure Initialize; override;
  public
    class Function Lookup64ToSys(Hash: TLookup64): TLookup64Sys; virtual;
    class Function Lookup64FromSys(Hash: TLookup64Sys): TLookup64; virtual;
    class Function Lookup64ToLE(Hash: TLookup64): TLookup64; virtual;
    class Function Lookup64ToBE(Hash: TLookup64): TLookup64; virtual;
    class Function Lookup64FromLE(Hash: TLookup64): TLookup64; virtual;
    class Function Lookup64FromBE(Hash: TLookup64): TLookup64; virtual;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TLookup64); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TLookup64); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Lookup64: TLookup64 read GetLookup64;
    property Lookup64Sys: TLookup64Sys read fLookup64Value;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{
    WARNING - functions BufferLookup2, BufferLookup3 and BufferLookup64 cannot
              be used for continuous hashing. Overload accepting initial value
              is here only to allow hashing with seed.
}
{===============================================================================
    Common procedural interface - declaration
===============================================================================}

Function LookupToStr(const Hash: TLookup): String;
Function StrToLookup(const Str: String): TLookup;
Function TryStrToLookup(const Str: String; out Hash: TLookup): Boolean;
Function StrToLookupDef(const Str: String; const Default: TLookup): TLookup;

Function CompareLookup(const A,B: TLookup): Integer;
Function SameLookup(const A,B: TLookup): Boolean;

//------------------------------------------------------------------------------
type
  TLookupContext = type Pointer;

{===============================================================================
    Lookup2 procedural interface - declaration
===============================================================================}

Function BufferLookup2(const Hash: TLookup; const Buffer; Size: TMemSize): TLookup; overload;
Function BufferLookup2(const Buffer; Size: TMemSize): TLookup; overload;

Function AnsiStringLookup2(const Str: AnsiString): TLookup;
Function WideStringLookup2(const Str: WideString): TLookup;
Function StringLookup2(const Str: String): TLookup;

Function StreamLookup2(Stream: TStream; Count: Int64 = -1): TLookup;
Function FileLookup2(const FileName: String): TLookup;

//------------------------------------------------------------------------------

Function Lookup2_Init: TLookupContext;
procedure Lookup2_Update(const Context: TLookupContext; const Buffer; Size: TMemSize);
Function Lookup2_Final(var Context: TLookupContext; const Buffer; Size: TMemSize): TLookup; overload;
Function Lookup2_Final(var Context: TLookupContext): TLookup; overload;
Function Lookup2_Hash(const Buffer; Size: TMemSize): TLookup;

{===============================================================================
    Lookup3 procedural interface - declaration
===============================================================================}

Function BufferLookup3(const Hash: TLookup; const Buffer; Size: TMemSize): TLookup; overload;
Function BufferLookup3(const Buffer; Size: TMemSize): TLookup; overload;

Function AnsiStringLookup3(const Str: AnsiString): TLookup;
Function WideStringLookup3(const Str: WideString): TLookup;
Function StringLookup3(const Str: String): TLookup;

Function StreamLookup3(Stream: TStream; Count: Int64 = -1): TLookup;
Function FileLookup3(const FileName: String): TLookup;

//------------------------------------------------------------------------------

Function Lookup3_Init: TLookupContext;
procedure Lookup3_Update(const Context: TLookupContext; const Buffer; Size: TMemSize);
Function Lookup3_Final(var Context: TLookupContext; const Buffer; Size: TMemSize): TLookup; overload;
Function Lookup3_Final(var Context: TLookupContext): TLookup; overload;
Function Lookup3_Hash(const Buffer; Size: TMemSize): TLookup;

{===============================================================================
    Lookup64 procedural interface - declaration
===============================================================================}

Function Lookup64ToStr(const Hash: TLookup64): String;
Function StrToLookup64(const Str: String): TLookup64;
Function TryStrToLookup64(const Str: String; out Hash: TLookup64): Boolean;
Function StrToLookup64Def(const Str: String; const Default: TLookup64): TLookup64;

Function CompareLookup64(const A,B: TLookup64): Integer;
Function SameLookup64(const A,B: TLookup64): Boolean;

//------------------------------------------------------------------------------

Function BufferLookup64(const Hash: TLookup64; const Buffer; Size: TMemSize): TLookup64; overload;
Function BufferLookup64(const Buffer; Size: TMemSize): TLookup64; overload;

Function AnsiStringLookup64(const Str: AnsiString): TLookup64;
Function WideStringLookup64(const Str: WideString): TLookup64;
Function StringLookup64(const Str: String): TLookup64;

Function StreamLookup64(Stream: TStream; Count: Int64 = -1): TLookup64;
Function FileLookup64(const FileName: String): TLookup64;

//------------------------------------------------------------------------------
type
  TLookup64Context = type Pointer;

Function Lookup64_Init: TLookup64Context;
procedure Lookup64_Update(const Context: TLookup64Context; const Buffer; Size: TMemSize);
Function Lookup64_Final(var Context: TLookup64Context; const Buffer; Size: TMemSize): TLookup64; overload;
Function Lookup64_Final(var Context: TLookup64Context): TLookup64; overload;
Function Lookup64_Hash(const Buffer; Size: TMemSize): TLookup64;

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

Function SwapEndian(Hash: TLookupSys): TLookupSys; overload;
begin
Result := TLookupSys(((Hash and $000000FF) shl 24) or ((Hash and $0000FF00) shl 8) or
                     ((Hash and $00FF0000) shr 8) or ((Hash and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TLookup): TLookup; overload;
begin
Result := TLookup(SwapEndian(TLookupSys(Hash)));
end;

//==============================================================================

Function LookupCompare(const A,B: TLookupSys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function LookupSame(const A,B: TLookupSys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function LookupAsString(const Lookup: TLookupSys): String;
begin
Result := IntToHex(Lookup,8);
end;

//------------------------------------------------------------------------------

Function LookupFromString(const Str: String): TLookupSys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TLookupSys(StrToInt(Str))
    else
      Result := TLookupSys(StrToInt('$' + Str));
  end
else Result := TLookupHashBase.LookupToSys(ZeroLookup);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TLookupHashBase
--------------------------------------------------------------------------------
===============================================================================}  
{===============================================================================
    TLookupHashBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TLookupHashBase - protected methods
-------------------------------------------------------------------------------}

Function TLookupHashBase.GetLookup: TLookup;
begin
Result := LookupFromSys(fLookupValue);
end;

//------------------------------------------------------------------------------

procedure TLookupHashBase.Initialize;
begin
inherited;
fLookupValue := LookupToSys(ZeroLookup);
end;

{-------------------------------------------------------------------------------
    TLookupHashBase - public methods
-------------------------------------------------------------------------------}

class Function TLookupHashBase.LookupToSys(Hash: TLookup): TLookupSys;
begin
Result := TLookupSys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TLookupHashBase.LookupFromSys(Hash: TLookupSys): TLookup;
begin
Result := TLookup({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TLookupHashBase.LookupToLE(Hash: TLookup): TLookup;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TLookupHashBase.LookupToBE(Hash: TLookup): TLookup;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TLookupHashBase.LookupFromLE(Hash: TLookup): TLookup;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TLookupHashBase.LookupFromBE(Hash: TLookup): TLookup;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TLookupHashBase.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TLookupHashBase.HashSize: TMemSize;
begin
Result := SizeOf(TLookup);
end;

//------------------------------------------------------------------------------

class Function TLookupHashBase.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TLookupHashBase.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TLookupHashBase.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TLookupHashBase then
  fLookupValue := TLookupHashBase(Hash).LookupSys
else
  raise ELHIncompatibleClass.CreateFmt('TLookupHashBase.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLookupHashBase.CreateAndInitFrom(Hash: TLookup);
begin
CreateAndInit;
fLookupValue := LookupToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TLookupHashBase.Init;
begin
inherited;
fLookupValue := LookupToSys(InitialLookup);
end;

//------------------------------------------------------------------------------

Function TLookupHashBase.Compare(Hash: THashBase): Integer;
begin
If Hash is TLookupHashBase then
  Result := LookupCompare(fLookupValue,TLookupHashBase(Hash).LookupSys)
else
  raise ELHIncompatibleClass.CreateFmt('TLookupHashBase.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TLookupHashBase.Same(Hash: THashBase): Boolean;
begin
If Hash is TLookupHashBase then
  Result := LookupSame(fLookupValue,TLookupHashBase(Hash).LookupSys)
else
  raise ELHIncompatibleClass.CreateFmt('TLookupHashBase.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TLookupHashBase.AsString: String;
begin
Result := LookupAsString(fLookupValue);
end;

//------------------------------------------------------------------------------

procedure TLookupHashBase.FromString(const Str: String);
begin
fLookupValue := LookupFromString(Str);
end;

//------------------------------------------------------------------------------

procedure TLookupHashBase.FromStringDef(const Str: String; const Default: TLookup);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fLookupValue := LookupToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TLookupHashBase.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TLookup;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}LookupToBE{$ELSE}LookupToLE{$ENDIF}(LookupFromSys(fLookupValue));
  heLittle: Temp := LookupToLE(LookupFromSys(fLookupValue));
  heBig:    Temp := LookupToBE(LookupFromSys(fLookupValue));
else
 {heDefault}
  Temp := LookupFromSys(fLookupValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TLookup));
end;

//------------------------------------------------------------------------------

procedure TLookupHashBase.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TLookup;
begin
Temp := ZeroLookup;
Stream.ReadBuffer(Temp,SizeOf(TLookup));
case Endianness of
  heSystem: fLookupValue := LookupToSys({$IFDEF ENDIAN_BIG}LookupFromBE{$ELSE}LookupFromLE{$ENDIF}(Temp));
  heLittle: fLookupValue := LookupToSys(LookupFromLE(Temp));
  heBig:    fLookupValue := LookupToSys(LookupFromBE(Temp));
else
 {heDefault}
  fLookupValue := LookupToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TLookup2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TLookup2Hash - main processing
===============================================================================}
type
  TReadOverlay = record case Integer of
    0:(Words: packed array[0..2] of TLookupSys);
    1:(Bytes: packed array[0..11] of UInt8)
  end;
  PReadOverlay = ^TReadOverlay;

//------------------------------------------------------------------------------  
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function Lookup2Process(const InitVal: TLookupSys; const Buffer; Size: TMemSize): TLookupSys;

  procedure Lookup2Mix(var A,B,C: TLookupSys);
  begin
    A := A - B;   A := A - C;   A := A xor (C shr 13);
    B := B - C;   B := B - A;   B := B xor TLookupSys(A shl 8);
    C := C - A;   C := C - B;   C := C xor (B shr 13);
    A := A - B;   A := A - C;   A := A xor (C shr 12);
    B := B - C;   B := B - A;   B := B xor TLookupSys(A shl 16);
    C := C - A;   C := C - B;   C := C xor (B shr 5);
    A := A - B;   A := A - C;   A := A xor (C shr 3);
    B := B - C;   B := B - A;   B := B xor TLookupSys(A shl 10);
    C := C - A;   C := C - B;   C := C xor (B shr 15);
  end;
  
//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
var
  Remaining:    TMemSize;
  A,B,C:        TLookupSys;
  CurrentData:  PReadOverlay;
  ReadBuffer:   TReadOverlay;
begin
Remaining := Size;
A := $9E3779B9;
B := A;
C := InitVal;
CurrentData := @Buffer;
// process 12-byte blocks
while Remaining >= 12 do begin
  A := A + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^.Words[0]);
  B := B + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^.Words[1]);
  C := C + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^.Words[2]);
  Lookup2Mix(A,B,C);
  Dec(Remaining,12);
  Inc(CurrentData);
end;
// remaining bytes
C := C + TLookupSys(Size);
If Remaining > 0 then
  begin
    FillChar(Addr(ReadBuffer)^,SizeOf(TReadOverlay),0);
    Move(CurrentData^,ReadBuffer,Remaining);
    A := A + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer.Words[0]);
    B := B + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer.Words[1]);
    C := C + TLookupSys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer.Words[2]) shl 8);
  end;
Lookup2Mix(A,B,C);
Result := C;
end;

{$IFDEF RangeChecks}{$R+}{$ENDIF}
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

{===============================================================================
    TLookup2Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TLookup2Hash - protected methods
-------------------------------------------------------------------------------}

procedure TLookup2Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin 
fLookupValue := Lookup2Process(fLookupValue,Memory^,Count);
end;

{-------------------------------------------------------------------------------
    TLookup2Hash - public methods
-------------------------------------------------------------------------------}

class Function TLookup2Hash.HashName: String;
begin
Result := 'Lookup2';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TLookup3Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TLookup3Hash - main processing
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

procedure Lookup3Process(var ValHi,ValLo: TLookupSys; const Buffer; Size: TMemSize); overload;

  Function ROT(const Value: TLookupSys; Shift: Integer): TLookupSys;
  begin
    // left rotation
    Result := TLookupSys(Value shl Shift) or (Value shr (32 - Shift));
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  procedure Lookup3Mix(var A,B,C: TLookupSys);
  begin
    A := A - C;   A := A xor ROT(C, 4);   C := C + B;
    B := B - A;   B := B xor ROT(A, 6);   A := A + C;
    C := C - B;   C := C xor ROT(B, 8);   B := B + A;
    A := A - C;   A := A xor ROT(C,16);   C := C + B;
    B := B - A;   B := B xor ROT(A,19);   A := A + C;
    C := C - B;   C := C xor ROT(B, 4);   B := B + A;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  procedure Lookup3Final(var A,B,C: TLookupSys);
  begin
    C := C xor B;   C := C - ROT(B,14);
    A := A xor C;   A := A - ROT(C,11);
    B := B xor A;   B := B - ROT(A,25);
    C := C xor B;   C := C - ROT(B,16);
    A := A xor C;   A := A - ROT(C, 4);
    B := B xor A;   B := B - ROT(A,14);
    C := C xor B;   C := C - ROT(B,24);
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
var
  A,B,C:        TLookupSys;
  CurrentData:  PReadOverlay;
  ReadBuffer:   TReadOverlay;
begin
If Size > 0 then
  begin
    A := $DEADBEEF + TLookupSys(Size) + ValLo;
    B := A;
    C := A + ValHi;
    CurrentData := @Buffer;
  {
    Process blocks

    I have tried to implement following cycle entirely in assembly - there was
    significant performance gain (about 3x in 32bit, have not tried 64bit), but
    the code was messy. So, unless there is demand for speed-up, I will keep
    this pure-pascal.
  }
    while Size > 12 do begin
      A := A + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^.Words[0]);
      B := B + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^.Words[1]);
      C := C + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^.Words[2]);
      Lookup3Mix(A,B,C);
      Dec(Size,12);
      Inc(CurrentData);
    end;
    // process remaining data (note that Size cannot be 0 here)
    FillChar(Addr(ReadBuffer)^,SizeOf(TReadOverlay),0);
    Move(CurrentData^,ReadBuffer,Size);
    A := A + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer.Words[0]);
    B := B + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer.Words[1]);
    C := C + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer.Words[2]);
    // final touches
    Lookup3Final(A,B,C);
  end
else
  begin
    B := $DEADBEEF + ValLo;
    C := B + ValHi;
  end;
// return result(s)  
ValHi := B;
ValLo := C;
end;

{$IFDEF RangeChecks}{$R+}{$ENDIF}
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
//------------------------------------------------------------------------------

Function Lookup3Process(const InitVal: TLookupSys; const Buffer; Size: TMemSize): TLookupSys; overload;
var
  Dummy:  TLookupSys;
begin
Result := InitVal;
Dummy := 0;
Lookup3Process(Dummy,Result,Buffer,Size);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Lookup3Process(const InitVal: TLookup64Sys; const Buffer; Size: TMemSize): TLookup64Sys; overload;
begin
Result := InitVal;
Lookup3Process(Int64Rec(Result).Hi,Int64Rec(Result).Lo,Buffer,Size);
end;

{===============================================================================
    TLookup3Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TLookup3Hash - protected methods
-------------------------------------------------------------------------------}

procedure TLookup3Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
fLookupValue := Lookup3Process(fLookupValue,Memory^,Count);
end;

{-------------------------------------------------------------------------------
    TLookup3Hash - public methods
-------------------------------------------------------------------------------}

class Function TLookup3Hash.HashName: String;
begin
Result := 'Lookup3';
end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TLookup64Hash                                  
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TLookup64Hash - auxiliary functions
===============================================================================}

Function SwapEndian(Hash: TLookup64Sys): TLookup64Sys; overload;
begin
Int64Rec(Result).Hi := SwapEndian(Int64Rec(Result).Lo);
Int64Rec(Result).Lo := SwapEndian(Int64Rec(Result).Hi);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TLookup64): TLookup64; overload;
begin
Result := TLookup64(SwapEndian(TLookup64Sys(Hash)));
end;

//==============================================================================

Function Lookup64Compare(const A,B: TLookup64Sys): Integer;
begin
Result := UInt64Utils.CompareUInt64(A,B);
end;

//------------------------------------------------------------------------------

Function Lookup64Same(const A,B: TLookup64Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function Lookup64AsString(const Lookup: TLookup64Sys): String;
begin
Result := IntToHex(Lookup,16);
end;

//------------------------------------------------------------------------------

Function Lookup64FromString(const Str: String): TLookup64Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TLookup64Sys(StrToInt64(Str))
    else
      Result := TLookup64Sys(StrToInt64('$' + Str));
  end
else Result := TLookup64Hash.Lookup64ToSys(ZeroLookup64);
end;

{===============================================================================
    TLookup64Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TLookup64Hash - protected methods
-------------------------------------------------------------------------------}

Function TLookup64Hash.GetLookup64: TLookup64;
begin
Result := Lookup64FromSys(fLookup64Value);
end;

//------------------------------------------------------------------------------

procedure TLookup64Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
fLookup64Value := Lookup3Process(fLookup64Value,Memory^,Count);
end;

//------------------------------------------------------------------------------

procedure TLookup64Hash.Initialize;
begin
inherited;
fLookup64Value := Lookup64ToSys(ZeroLookup64);
end;

{-------------------------------------------------------------------------------
    TLookup64Hash - public methods
-------------------------------------------------------------------------------}

class Function TLookup64Hash.Lookup64ToSys(Hash: TLookup64): TLookup64Sys;
begin
Result := TLookup64Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.Lookup64FromSys(Hash: TLookup64Sys): TLookup64;
begin
Result := TLookup64({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.Lookup64ToLE(Hash: TLookup64): TLookup64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.Lookup64ToBE(Hash: TLookup64): TLookup64;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.Lookup64FromLE(Hash: TLookup64): TLookup64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.Lookup64FromBE(Hash: TLookup64): TLookup64;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.HashSize: TMemSize;
begin
Result := SizeOf(TLookup64);
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.HashName: String;
begin
Result := 'Lookup64';
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TLookup64Hash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TLookup64Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TLookup64Hash then
  fLookup64Value := TLookup64Hash(Hash).Lookup64Sys
else
  raise ELHIncompatibleClass.CreateFmt('TLookup64Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLookup64Hash.CreateAndInitFrom(Hash: TLookup64);
begin
CreateAndInit;
fLookup64Value := Lookup64ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TLookup64Hash.Init;
begin
inherited;
fLookup64Value := Lookup64ToSys(InitialLookup64);
end;

//------------------------------------------------------------------------------

Function TLookup64Hash.Compare(Hash: THashBase): Integer;
begin
If Hash is TLookup64Hash then
  Result := Lookup64Compare(fLookup64Value,TLookup64Hash(Hash).Lookup64Sys)
else
  raise ELHIncompatibleClass.CreateFmt('TLookup64Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TLookup64Hash.Same(Hash: THashBase): Boolean;
begin
If Hash is TLookup64Hash then
  Result := Lookup64Same(fLookup64Value,TLookup64Hash(Hash).Lookup64Sys)
else
  raise ELHIncompatibleClass.CreateFmt('TLookup64Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TLookup64Hash.AsString: String;
begin
Result := Lookup64AsString(fLookup64Value);
end;

//------------------------------------------------------------------------------

procedure TLookup64Hash.FromString(const Str: String);
begin
fLookup64Value := Lookup64FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TLookup64Hash.FromStringDef(const Str: String; const Default: TLookup64);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fLookup64Value := Lookup64ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TLookup64Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TLookup64;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}Lookup64ToBE{$ELSE}Lookup64ToLE{$ENDIF}(Lookup64FromSys(fLookup64Value));
  heLittle: Temp := Lookup64ToLE(Lookup64FromSys(fLookup64Value));
  heBig:    Temp := Lookup64ToBE(Lookup64FromSys(fLookup64Value));
else
 {heDefault}
  Temp := Lookup64FromSys(fLookup64Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TLookup64));
end;

//------------------------------------------------------------------------------

procedure TLookup64Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TLookup64;
begin
Temp := ZeroLookup64;
Stream.ReadBuffer(Temp,SizeOf(TLookup64));
case Endianness of
  heSystem: fLookup64Value := Lookup64ToSys({$IFDEF ENDIAN_BIG}Lookup64FromBE{$ELSE}Lookup64FromLE{$ENDIF}(Temp));
  heLittle: fLookup64Value := Lookup64ToSys(Lookup64FromLE(Temp));
  heBig:    fLookup64Value := Lookup64ToSys(Lookup64FromBE(Temp));
else
 {heDefault}
  fLookup64Value := Lookup64ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Common procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Common procedural interface - utility functions
-------------------------------------------------------------------------------}

Function LookupToStr(const Hash: TLookup): String;
begin
Result := LookupAsString(TLookupHashBase.LookupToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToLookup(const Str: String): TLookup;
begin
Result := TLookupHashBase.LookupFromSys(LookupFromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToLookup(const Str: String; out Hash: TLookup): Boolean;
begin
try
  Hash := TLookupHashBase.LookupFromSys(LookupFromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToLookupDef(const Str: String; const Default: TLookup): TLookup;
begin
If not TryStrToLookup(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareLookup(const A,B: TLookup): Integer;
begin
Result := LookupCompare(TLookupHashBase.LookupToSys(A),TLookupHashBase.LookupToSys(B));
end;

//------------------------------------------------------------------------------

Function SameLookup(const A,B: TLookup): Boolean;
begin
Result := LookupSame(TLookupHashBase.LookupToSys(A),TLookupHashBase.LookupToSys(B));
end;

{===============================================================================
    Lookup2 procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Lookup2 procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferLookup2(const Hash: TLookup; const Buffer; Size: TMemSize): TLookup;
begin
Result := TLookupHashBase.LookupFromSys(Lookup2Process(TLookupHashBase.LookupToSys(Hash),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferLookup2(const Buffer; Size: TMemSize): TLookup;
begin
Result := TLookupHashBase.LookupFromSys(Lookup2Process(TLookupHashBase.LookupToSys(InitialLookup),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringLookup2(const Str: AnsiString): TLookup;
begin
Result := BufferLookup2(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringLookup2(const Str: WideString): TLookup;
begin
Result := BufferLookup2(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringLookup2(const Str: String): TLookup;
begin
Result := BufferLookup2(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamLookup2(Stream: TStream; Count: Int64 = -1): TLookup;
var
  Hasher: TLookup2Hash;
begin
Hasher := TLookup2Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Lookup;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileLookup2(const FileName: String): TLookup;
var
  Hasher: TLookup2Hash;
begin
Hasher := TLookup2Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Lookup;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Lookup2 procedural interface - context functions
-------------------------------------------------------------------------------}

Function Lookup2_Init: TLookupContext;
begin
Result := TLookupContext(TLookup2Hash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure Lookup2_Update(const Context: TLookupContext; const Buffer; Size: TMemSize);
begin
TLookup2Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Lookup2_Final(var Context: TLookupContext; const Buffer; Size: TMemSize): TLookup;
begin
Lookup2_Update(Context,Buffer,Size);
Result := Lookup2_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Lookup2_Final(var Context: TLookupContext): TLookup;
begin
TLookup2Hash(Context).Final;
Result := TLookup2Hash(Context).Lookup;
FreeAndNil(TLookup2Hash(Context));
end;

//------------------------------------------------------------------------------

Function Lookup2_Hash(const Buffer; Size: TMemSize): TLookup;
begin
Result := BufferLookup2(Buffer,Size);
end;

{===============================================================================
    Lookup3 procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Lookup3 procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferLookup3(const Hash: TLookup; const Buffer; Size: TMemSize): TLookup;
begin
Result := TLookupHashBase.LookupFromSys(Lookup3Process(TLookupHashBase.LookupToSys(Hash),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferLookup3(const Buffer; Size: TMemSize): TLookup;
begin
Result := TLookupHashBase.LookupFromSys(Lookup3Process(TLookupHashBase.LookupToSys(InitialLookup),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringLookup3(const Str: AnsiString): TLookup;
begin
Result := BufferLookup3(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringLookup3(const Str: WideString): TLookup;
begin
Result := BufferLookup3(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringLookup3(const Str: String): TLookup;
begin
Result := BufferLookup3(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamLookup3(Stream: TStream; Count: Int64 = -1): TLookup;
var
  Hasher: TLookup3Hash;
begin
Hasher := TLookup3Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Lookup;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileLookup3(const FileName: String): TLookup;
var
  Hasher: TLookup3Hash;
begin
Hasher := TLookup3Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Lookup;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Lookup3 procedural interface - context functions
-------------------------------------------------------------------------------}

Function Lookup3_Init: TLookupContext;
begin
Result := TLookupContext(TLookup3Hash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure Lookup3_Update(const Context: TLookupContext; const Buffer; Size: TMemSize);
begin
TLookup3Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Lookup3_Final(var Context: TLookupContext; const Buffer; Size: TMemSize): TLookup;
begin
Lookup3_Update(Context,Buffer,Size);
Result := Lookup3_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Lookup3_Final(var Context: TLookupContext): TLookup;
begin
TLookup3Hash(Context).Final;
Result := TLookup3Hash(Context).Lookup;
FreeAndNil(TLookup3Hash(Context));
end;

//------------------------------------------------------------------------------

Function Lookup3_Hash(const Buffer; Size: TMemSize): TLookup;
begin
Result := BufferLookup3(Buffer,Size);
end;

{===============================================================================
    Lookup64 procedural interface - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Lookup64 procedural interface - utility functions
-------------------------------------------------------------------------------}

Function Lookup64ToStr(const Hash: TLookup64): String;
begin
Result := Lookup64AsString(TLookup64Hash.Lookup64ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToLookup64(const Str: String): TLookup64;
begin
Result := TLookup64Hash.Lookup64FromSys(Lookup64FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToLookup64(const Str: String; out Hash: TLookup64): Boolean;
begin
try
  Hash := TLookup64Hash.Lookup64FromSys(Lookup64FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToLookup64Def(const Str: String; const Default: TLookup64): TLookup64;
begin
If not TryStrToLookup64(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareLookup64(const A,B: TLookup64): Integer;
begin
Result := Lookup64Compare(TLookup64Hash.Lookup64ToSys(A),TLookup64Hash.Lookup64ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameLookup64(const A,B: TLookup64): Boolean;
begin
Result := Lookup64Same(TLookup64Hash.Lookup64ToSys(A),TLookup64Hash.Lookup64ToSys(B));
end;

{-------------------------------------------------------------------------------
    Lookup64 procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferLookup64(const Hash: TLookup64; const Buffer; Size: TMemSize): TLookup64;
begin
Result := TLookup64Hash.Lookup64FromSys(Lookup3Process(TLookup64Hash.Lookup64ToSys(Hash),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferLookup64(const Buffer; Size: TMemSize): TLookup64;
begin
Result := TLookup64Hash.Lookup64FromSys(Lookup3Process(TLookup64Hash.Lookup64ToSys(InitialLookup64),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringLookup64(const Str: AnsiString): TLookup64;
begin
Result := BufferLookup64(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringLookup64(const Str: WideString): TLookup64;
begin
Result := BufferLookup64(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringLookup64(const Str: String): TLookup64;
begin
Result := BufferLookup64(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamLookup64(Stream: TStream; Count: Int64 = -1): TLookup64;
var
  Hasher: TLookup64Hash;
begin
Hasher := TLookup64Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Lookup64;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileLookup64(const FileName: String): TLookup64;
var
  Hasher: TLookup64Hash;
begin
Hasher := TLookup64Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Lookup64;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Lookup64 procedural interface - context functions
-------------------------------------------------------------------------------}

Function Lookup64_Init: TLookup64Context;
begin
Result := TLookup64Context(TLookup64Hash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure Lookup64_Update(const Context: TLookup64Context; const Buffer; Size: TMemSize);
begin
TLookup64Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Lookup64_Final(var Context: TLookup64Context; const Buffer; Size: TMemSize): TLookup64;
begin
Lookup64_Update(Context,Buffer,Size);
Result := Lookup64_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Lookup64_Final(var Context: TLookup64Context): TLookup64;
begin
TLookup64Hash(Context).Final;
Result := TLookup64Hash(Context).Lookup64;
FreeAndNil(TLookup64Hash(Context));
end;

//------------------------------------------------------------------------------

Function Lookup64_Hash(const Buffer; Size: TMemSize): TLookup64;
begin
Result := BufferLookup64(Buffer,Size);
end;

end.
