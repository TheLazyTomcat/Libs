{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Murmur OAAT (one-at-a-time) hash

    This is simplistic hash based on Murmur mix, but, unlike Murmur, allowing
    for progressive hashing.

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

      github.com/TheLazyTomcat/Lib.MurmurOAATHash

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
unit MurmurOAATHash;

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
  EMURPException = class(EHashException);

  EMURPIncompatibleClass = class(EMURPException);

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
                                 TMurmurOAATHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMurmurOAATHash - class declaration
===============================================================================}
type
  TMurmurOAATHash = class(TStreamHash)
  protected
    fMurmurValue: TMurmurSys;
    Function GetMurmur: TMurmur; virtual;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
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

Function BufferMurmurOAAT(const Hash: TMurmur; const Buffer; Size: TMemSize): TMurmur; overload;

Function BufferMurmurOAAT(const Buffer; Size: TMemSize): TMurmur; overload;

Function AnsiStringMurmurOAAT(const Str: AnsiString): TMurmur;
Function WideStringMurmurOAAT(const Str: WideString): TMurmur;
Function StringMurmurOAAT(const Str: String): TMurmur;

Function StreamMurmurOAAT(Stream: TStream; Count: Int64 = -1): TMurmur;
Function FileMurmurOAAT(const FileName: String): TMurmur;

//------------------------------------------------------------------------------
type
  TMurmurOAATContext = type TMurmurSys;

Function MurmurOAAT_Init: TMurmurOAATContext;
procedure MurmurOAAT_Update(var Context: TMurmurOAATContext; const Buffer; Size: TMemSize);
Function MurmurOAAT_Final(var Context: TMurmurOAATContext; const Buffer; Size: TMemSize): TMurmur; overload;
Function MurmurOAAT_Final(var Context: TMurmurOAATContext): TMurmur; overload;
Function MurmurOAAT_Hash(const Buffer; Size: TMemSize): TMurmur;

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
else Result := TMurmurOAATHash.MurmurToSys(ZeroMurmur);
end;

{===============================================================================
    Main implementation
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function MurmurOAATProcess(const Hash: TMurmurSys; const Buffer; Size: TMemSize): TMurmurSys;
var
  CurrentData:  PUInt8;
  i:            TMemSize;
begin
Result := Hash;
If Size > 0 then
  begin
    CurrentData := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Result := Result xor TMurmurSys(CurrentData^);
        Result := Result * TMurmurSys($5BD1E995);
        Result := Result xor (Result shr 15);
        Inc(CurrentData);
      end;
  end;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}
{===============================================================================
--------------------------------------------------------------------------------
                                 TMurmurOAATHash
--------------------------------------------------------------------------------
===============================================================================}  
{===============================================================================
    TMurmurOAATHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMurmurOAATHash - protected methods
-------------------------------------------------------------------------------}

Function TMurmurOAATHash.GetMurmur: TMurmur;
begin
Result := MurmurFromSys(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmurOAATHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fMurmurValue := MurmurOAATProcess(fMurmurValue,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TMurmurOAATHash.Initialize;
begin
inherited;
fMurmurValue := MurmurToSys(ZeroMurmur);
end;

{-------------------------------------------------------------------------------
    TMurmurOAATHash - public methods
-------------------------------------------------------------------------------}

class Function TMurmurOAATHash.MurmurToSys(Hash: TMurmur): TMurmurSys;
begin
Result := TMurmurSys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.MurmurFromSys(Hash: TMurmurSys): TMurmur;
begin
Result := TMurmur({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.MurmurToLE(Hash: TMurmur): TMurmur;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.MurmurToBE(Hash: TMurmur): TMurmur;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.MurmurFromLE(Hash: TMurmur): TMurmur;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.MurmurFromBE(Hash: TMurmur): TMurmur;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.HashSize: TMemSize;
begin
Result := SizeOf(TMurmur);
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.HashName: String;
begin
Result := 'MurmurOAAT';
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TMurmurOAATHash.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

constructor TMurmurOAATHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMurmurOAATHash then
  fMurmurValue := TMurmurOAATHash(Hash).MurmurSys
else
  raise EMURPIncompatibleClass.CreateFmt('TMurmurOAATHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMurmurOAATHash.CreateAndInitFrom(Hash: TMurmur);
begin
CreateAndInit;
fMurmurValue := MurmurToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TMurmurOAATHash.Init;
begin
inherited;
fMurmurValue := MurmurToSys(InitialMurmur);
end;

//------------------------------------------------------------------------------

Function TMurmurOAATHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TMurmurOAATHash then
  Result := MurmurCompare(fMurmurValue,TMurmurOAATHash(Hash).MurmurSys)
else
  raise EMURPIncompatibleClass.CreateFmt('TMurmurOAATHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmurOAATHash.Same(Hash: THashBase): Boolean;
begin
If Hash is TMurmurOAATHash then
  Result := MurmurSame(fMurmurValue,TMurmurOAATHash(Hash).MurmurSys)
else
  raise EMURPIncompatibleClass.CreateFmt('TMurmurOAATHash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMurmurOAATHash.AsString: String;
begin
Result := MurmurAsString(fMurmurValue);
end;

//------------------------------------------------------------------------------

procedure TMurmurOAATHash.FromString(const Str: String);
begin
fMurmurValue := MurmurFromString(Str);
end;

//------------------------------------------------------------------------------

procedure TMurmurOAATHash.FromStringDef(const Str: String; const Default: TMurmur);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMurmurValue := MurmurToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TMurmurOAATHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
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

procedure TMurmurOAATHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
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
Result := MurmurAsString(TMurmurOAATHash.MurmurToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToMurmur(const Str: String): TMurmur;
begin
Result := TMurmurOAATHash.MurmurFromSys(MurmurFromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToMurmur(const Str: String; out Hash: TMurmur): Boolean;
begin
try
  Hash := TMurmurOAATHash.MurmurFromSys(MurmurFromString(Str));
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
Result := MurmurCompare(TMurmurOAATHash.MurmurToSys(A),TMurmurOAATHash.MurmurToSys(B));
end;

//------------------------------------------------------------------------------

Function SameMurmur(const A,B: TMurmur): Boolean;
begin
Result := MurmurSame(TMurmurOAATHash.MurmurToSys(A),TMurmurOAATHash.MurmurToSys(B));
end;

{-------------------------------------------------------------------------------
    Procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferMurmurOAAT(const Hash: TMurmur; const Buffer; Size: TMemSize): TMurmur;
begin
Result := TMurmurOAATHash.MurmurFromSys(MurmurOAATProcess(TMurmurOAATHash.MurmurToSys(Hash),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferMurmurOAAT(const Buffer; Size: TMemSize): TMurmur;
begin
Result := TMurmurOAATHash.MurmurFromSys(MurmurOAATProcess(TMurmurOAATHash.MurmurToSys(InitialMurmur),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringMurmurOAAT(const Str: AnsiString): TMurmur;
begin
Result := BufferMurmurOAAT(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringMurmurOAAT(const Str: WideString): TMurmur;
begin
Result := BufferMurmurOAAT(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringMurmurOAAT(const Str: String): TMurmur;
begin
Result := BufferMurmurOAAT(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamMurmurOAAT(Stream: TStream; Count: Int64 = -1): TMurmur;
var
  Hasher: TMurmurOAATHash;
begin
Hasher := TMurmurOAATHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Murmur;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMurmurOAAT(const FileName: String): TMurmur;
var
  Hasher: TMurmurOAATHash;
begin
Hasher := TMurmurOAATHash.Create;
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

Function MurmurOAAT_Init: TMurmurOAATContext;
begin
Result := TMurmurOAATContext(TMurmurOAATHash.MurmurToSys(InitialMurmur));
end;

//------------------------------------------------------------------------------

procedure MurmurOAAT_Update(var Context: TMurmurOAATContext; const Buffer; Size: TMemSize);
begin
TMurmurSys(Context) := MurmurOAATProcess(TMurmurSys(Context),Buffer,Size);
end;

//------------------------------------------------------------------------------

Function MurmurOAAT_Final(var Context: TMurmurOAATContext; const Buffer; Size: TMemSize): TMurmur;
begin
MurmurOAAT_Update(Context,Buffer,Size);
Result := MurmurOAAT_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function MurmurOAAT_Final(var Context: TMurmurOAATContext): TMurmur;
begin
Result := TMurmurOAATHash.MurmurFromSys(TMurmurSys(Context));
TMurmurSys(Context) := TMurmurOAATHash.MurmurToSys(ZeroMurmur);
end;

//------------------------------------------------------------------------------

Function MurmurOAAT_Hash(const Buffer; Size: TMemSize): TMurmur;
begin
Result := TMurmurOAATHash.MurmurFromSys(MurmurOAATProcess(TMurmurOAATHash.MurmurToSys(InitialMurmur),Buffer,Size));
end;

end.
