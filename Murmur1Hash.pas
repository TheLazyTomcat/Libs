{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Murmur hash (first version, also known as Murmur1 or MurmurHash1)

    WARNING - implementation was not tested for correctness. It works, but
              whether it produces correct hashes I cannot guarantee.

  Version 1.0.1 (2026-07-14)

  Last change 2026-07-14

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Murmur1Hash

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes
    HashBase - github.com/TheLazyTomcat/Lib.HashBase

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    ListUtils          - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit Murmur1Hash;

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
  EMUR1Exception = class(EHashException);

  EMUR1IncompatibleClass = class(EMUR1Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TMurmur are, in memory, always ordered from least significant byte
  to most significant byte (little endian).

  Type TMurmurSys has no such guarantee and its endianness is system-dependent.
}
type
  TMurmur = packed array[0..3] of UInt8;
  PMurmur = ^TMurmur;

  TMurmurSys = UInt32;
  PMurmurSys = ^TMurmurSys;

const
  InitialMurmur: TMurmur = ($00,$00,$00,$00);
  ZeroMurmur:    TMurmur = (0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur1Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmur1Hash - class declaration
===============================================================================}
type
  TMurmur1Hash = class(TBufferHash)
  protected
    fSeed:        TMurmurSys;
    fMurmurValue: TMurmurSys;
    Function GetSeed: TMurmur; virtual;
    procedure SetSeed(const Value: TMurmur); virtual;
    Function GetMurmur: TMurmur; virtual;
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
    procedure Initialize; override;
  public
    class Function MurmurToSys(Hash: TMurmur): TMurmurSys; virtual;
    class Function MurmurFromSys(Hash: TMurmurSys): TMurmur; virtual;
    class Function MurmurToLE(Hash: TMurmur): TMurmur; virtual;
    class Function MurmurToBE(Hash: TMurmur): TMurmur; virtual;
    class Function MurmurFromLE(Hash: TMurmur): TMurmur; virtual;
    class Function MurmurFromBE(Hash: TMurmur): TMurmur; virtual;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMurmur); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMurmur); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Seed: TMurmur read GetSeed write SetSeed;
    property SeedSys: TMurmurSys read fSeed write fSeed;
    property Murmur: TMurmur read GetMurmur;
    property MurmurSys: TMurmurSys read fMurmurValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - declaration
===============================================================================}

Function MurmurToStr(const Hash: TMurmur): String;
Function StrToMurmur(const Str: String): TMurmur;
Function TryStrToMurmur(const Str: String; out Hash: TMurmur): Boolean;
Function StrToMurmurDef(const Str: String; const Default: TMurmur): TMurmur;

Function CompareMurmur(const A,B: TMurmur): Integer;
Function SameMurmur(const A,B: TMurmur): Boolean;

//------------------------------------------------------------------------------

Function BufferMurmur1(const Seed: TMurmur; const Buffer; Size: TMemSize): TMurmur; overload;
Function BufferMurmur1(const Buffer; Size: TMemSize): TMurmur; overload;

Function AnsiStringMurmur1(const Str: AnsiString): TMurmur;
Function WideStringMurmur1(const Str: WideString): TMurmur;
Function StringMurmur1(const Str: String): TMurmur;

Function StreamMurmur1(Stream: TStream; Count: Int64 = -1): TMurmur;
Function FileMurmur1(const FileName: String): TMurmur;

//------------------------------------------------------------------------------
type
  TMurmur1Context = type Pointer;

Function Murmur1_Init(const Seed: TMurmur): TMurmur1Context; overload;
Function Murmur1_Init: TMurmur1Context; overload;
procedure Murmur1_Update(const Context: TMurmur1Context; const Buffer; Size: TMemSize);
Function Murmur1_Final(var Context: TMurmur1Context; const Buffer; Size: TMemSize): TMurmur; overload;
Function Murmur1_Final(var Context: TMurmur1Context): TMurmur; overload;
Function Murmur1_Hash(const Buffer; Size: TMemSize): TMurmur;

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

{===============================================================================
    Auxiliary functions
===============================================================================}

Function SwapEndian(Hash: TMurmurSys): TMurmurSys; overload;
begin
Result := TMurmurSys(((Hash and $000000FF) shl 24) or ((Hash and $0000FF00) shl 8) or
                     ((Hash and $00FF0000) shr 8) or ((Hash and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TMurmur): TMurmur; overload;
begin
Result := TMurmur(SwapEndian(TMurmurSys(Hash)));
end;

//==============================================================================

Function MurmurCompare(const A,B: TMurmurSys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function MurmurSame(const A,B: TMurmurSys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function MurmurAsString(const Murmur: TMurmurSys): String;
begin
Result := IntToHex(Murmur,8);
end;

//------------------------------------------------------------------------------

Function MurmurFromString(const Str: String): TMurmurSys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TMurmurSys(StrToInt(Str))
    else
      Result := TMurmurSys(StrToInt('$' + Str));
  end
else Result := TMurmur1Hash.MurmurToSys(ZeroMurmur);
end;

{===============================================================================
    Main implementation
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function Murmur1Process(const Seed: TMurmurSys; const Buffer; Size: TMemSize): TMurmurSys;
const
  MulConst = UInt32($C6A4A793);
  RotConst = 16;
var
  Hash:         TMurmurSys;
  CurrentData:  PUInt32;
  ReadBuffer:   UInt32;
begin
Hash := Seed xor (Size * MulConst);
CurrentData := @Buffer;
while Size >= 4 do
  begin
    Hash := Hash + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CurrentData^);
    Hash := Hash * MulConst;
    Hash := Hash xor (Hash shr 16);
    Inc(CurrentData);
    Dec(Size,4);
  end;
ReadBuffer := 0;
If Size > 0 then
  begin
    Move(CurrentData^,ReadBuffer,Size);
    Hash := Hash + {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(ReadBuffer);
    Hash := Hash * MulConst;
    Hash := Hash xor (Hash shr RotConst);
  end;
Hash := Hash * MulConst;
Hash := Hash xor (Hash shr 10);
Hash := Hash * MulConst;
Hash := Hash xor (Hash shr 17);
Result := Hash;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
--------------------------------------------------------------------------------
                                  TMurmur1Hash
--------------------------------------------------------------------------------
===============================================================================}  
{===============================================================================
    TMurmur1Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmur1Hash - protected methods
-------------------------------------------------------------------------------}

Function TMurmur1Hash.GetSeed: TMurmur;
begin
Result := MurmurFromSys(fSeed);
end;

//------------------------------------------------------------------------------

procedure TMurmur1Hash.SetSeed(const Value: TMurmur);
begin
fSeed := MurmurToSys(Value);
end;

//------------------------------------------------------------------------------

Function TMurmur1Hash.GetMurmur: TMurmur;
begin
Result := MurmurFromSys(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur1Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
fMurmurValue := Murmur1Process(fSeed,Memory^,Count);
end;

//------------------------------------------------------------------------------

procedure TMurmur1Hash.Initialize;
begin
inherited;
fSeed := MurmurToSys(ZeroMurmur);
fMurmurValue := MurmurToSys(ZeroMurmur);
end;

{-------------------------------------------------------------------------------
    TMurmur1Hash - public methods
-------------------------------------------------------------------------------}

class Function TMurmur1Hash.MurmurToSys(Hash: TMurmur): TMurmurSys;
begin
Result := TMurmurSys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.MurmurFromSys(Hash: TMurmurSys): TMurmur;
begin
Result := TMurmur({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.MurmurToLE(Hash: TMurmur): TMurmur;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.MurmurToBE(Hash: TMurmur): TMurmur;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.MurmurFromLE(Hash: TMurmur): TMurmur;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.MurmurFromBE(Hash: TMurmur): TMurmur;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.HashSize: TMemSize;
begin
Result := SizeOf(TMurmur);
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.HashName: String;
begin
Result := 'Murmur1';
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TMurmur1Hash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TMurmur1Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMurmur1Hash then
  fMurmurValue := TMurmur1Hash(Hash).MurmurSys
else
  raise EMUR1IncompatibleClass.CreateFmt('TMurmur1Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMurmur1Hash.CreateAndInitFrom(Hash: TMurmur);
begin
CreateAndInit;
fMurmurValue := MurmurToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TMurmur1Hash.Init;
begin
inherited;
fMurmurValue := MurmurToSys(InitialMurmur);
end;

//------------------------------------------------------------------------------

Function TMurmur1Hash.Compare(Hash: THashBase): Integer;
begin
If Hash is TMurmur1Hash then
  Result := MurmurCompare(fMurmurValue,TMurmur1Hash(Hash).MurmurSys)
else
  raise EMUR1IncompatibleClass.CreateFmt('TMurmur1Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur1Hash.Same(Hash: THashBase): Boolean;
begin
If Hash is TMurmur1Hash then
  Result := MurmurSame(fMurmurValue,TMurmur1Hash(Hash).MurmurSys)
else
  raise EMUR1IncompatibleClass.CreateFmt('TMurmur1Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmur1Hash.AsString: String;
begin
Result := MurmurAsString(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmur1Hash.FromString(const Str: String);
begin
fMurmurValue := MurmurFromString(Str);
end;

//------------------------------------------------------------------------------

procedure TMurmur1Hash.FromStringDef(const Str: String; const Default: TMurmur);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMurmurValue := MurmurToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TMurmur1Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}MurmurToBE{$ELSE}MurmurToLE{$ENDIF}(MurmurFromSys(fMurmurValue));
  heLittle: Temp := MurmurToLE(MurmurFromSys(fMurmurValue));
  heBig:    Temp := MurmurToBE(MurmurFromSys(fMurmurValue));
else
 {heDefault}
  Temp := MurmurFromSys(fMurmurValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TMurmur));
end;

//------------------------------------------------------------------------------

procedure TMurmur1Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMurmur;
begin
Temp := ZeroMurmur;
Stream.ReadBuffer(Temp,SizeOf(TMurmur));
case Endianness of
  heSystem: fMurmurValue := MurmurToSys({$IFDEF ENDIAN_BIG}MurmurFromBE{$ELSE}MurmurFromLE{$ENDIF}(Temp));
  heLittle: fMurmurValue := MurmurToSys(MurmurFromLE(Temp));
  heBig:    fMurmurValue := MurmurToSys(MurmurFromBE(Temp));
else
 {heDefault}
  fMurmurValue := MurmurToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Procedural interface - utility functions
-------------------------------------------------------------------------------}

Function MurmurToStr(const Hash: TMurmur): String;
begin
Result := MurmurAsString(TMurmur1Hash.MurmurToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToMurmur(const Str: String): TMurmur;
begin
Result := TMurmur1Hash.MurmurFromSys(MurmurFromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToMurmur(const Str: String; out Hash: TMurmur): Boolean;
begin
try
  Hash := TMurmur1Hash.MurmurFromSys(MurmurFromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToMurmurDef(const Str: String; const Default: TMurmur): TMurmur;
begin
If not TryStrToMurmur(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareMurmur(const A,B: TMurmur): Integer;
begin
Result := MurmurCompare(TMurmur1Hash.MurmurToSys(A),TMurmur1Hash.MurmurToSys(B));
end;

//------------------------------------------------------------------------------

Function SameMurmur(const A,B: TMurmur): Boolean;
begin
Result := MurmurSame(TMurmur1Hash.MurmurToSys(A),TMurmur1Hash.MurmurToSys(B));
end;

{-------------------------------------------------------------------------------
    Procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferMurmur1(const Seed: TMurmur; const Buffer; Size: TMemSize): TMurmur;
begin
Result := TMurmur1Hash.MurmurFromSys(Murmur1Process(TMurmur1Hash.MurmurToSys(Seed),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferMurmur1(const Buffer; Size: TMemSize): TMurmur;
begin
Result := TMurmur1Hash.MurmurFromSys(Murmur1Process(TMurmur1Hash.MurmurToSys(InitialMurmur),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringMurmur1(const Str: AnsiString): TMurmur;
begin
Result := BufferMurmur1(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringMurmur1(const Str: WideString): TMurmur;
begin
Result := BufferMurmur1(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringMurmur1(const Str: String): TMurmur;
begin
Result := BufferMurmur1(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamMurmur1(Stream: TStream; Count: Int64 = -1): TMurmur;
var
  Hasher: TMurmur1Hash;
begin
Hasher := TMurmur1Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMurmur1(const FileName: String): TMurmur;
var
  Hasher: TMurmur1Hash;
begin
Hasher := TMurmur1Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Murmur;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - context functions
-------------------------------------------------------------------------------}

Function Murmur1_Init(const Seed: TMurmur): TMurmur1Context;
begin
Result := TMurmur1Context(TMurmur1Hash.Create);
TMurmur1Hash(Result).Seed := Seed;
TMurmur1Hash(Result).Init;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur1_Init: TMurmur1Context;
begin
Result := TMurmur1Context(TMurmur1Hash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure Murmur1_Update(const Context: TMurmur1Context; const Buffer; Size: TMemSize);
begin
TMurmur1Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Murmur1_Final(var Context: TMurmur1Context; const Buffer; Size: TMemSize): TMurmur;
begin
Murmur1_Update(Context,Buffer,Size);
Result := Murmur1_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Murmur1_Final(var Context: TMurmur1Context): TMurmur;
begin
TMurmur1Hash(Context).Final;
Result := TMurmur1Hash(Context).Murmur;
FreeAndNil(TMurmur1Hash(Context));
end;

//------------------------------------------------------------------------------

Function Murmur1_Hash(const Buffer; Size: TMemSize): TMurmur;
begin
Result := BufferMurmur1(Buffer,Size);
end;

end.
