{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Adler-32 calculation

  Version 1.3 (2026-07-03)

  Last change 2026-07-08

  ©2018-2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Adler32

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
unit Adler32;

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
  AuxTypes, HashBase;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EADLER32Exception = class(EHASHException);

  EADLER32IncompatibleClass = class(EADLER32Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TAdler32 are always ordered from least significant byte to most
  significant byte (little endian).

  Type TAdler32Sys has no such guarantee and its endianness is system-dependent.

  To convert the checksum in default ordering to a required specific ordering,
  use methods Adler32ToLE for little endian and Adler32ToBE for big endian.
  Note that these methods are expecting the input value to be in default
  ordering, if it is not, the result will be wrong. Be carefull when using them.
}
type
  TAdler32 = packed array[0..3] of UInt8;
  PAdler32 = ^TAdler32;

  TAdler32Sys = UInt32;
  PAdler32Sys = ^TAdler32Sys;

const
  InitialAdler32: TAdler32 = ($01,$00,$00,$00);

  ZeroAdler32: TAdler32 = (0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                  TAdler32Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TAdler32Hash - class declaration
===============================================================================}
type
  TAdler32Hash = class(TStreamHash)
  protected
    fAdler32: TAdler32Sys;
    Function GetAdler32: TAdler32; virtual;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function Adler32ToSys(Adler32: TAdler32): TAdler32Sys; virtual;
    class Function Adler32FromSys(Adler32: TAdler32Sys): TAdler32; virtual;
    class Function Adler32ToLE(Adler32: TAdler32): TAdler32; virtual;
    class Function Adler32ToBE(Adler32: TAdler32): TAdler32; virtual;
    class Function Adler32FromLE(Adler32: TAdler32): TAdler32; virtual;
    class Function Adler32FromBE(Adler32: TAdler32): TAdler32; virtual;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TAdler32); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TAdler32); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Adler32: TAdler32 read GetAdler32;
    property Adler32Sys: TAdler32Sys read fAdler32;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}

Function Adler32ToStr(const Adler32: TAdler32): String;
Function StrToAdler32(const Str: String): TAdler32;
Function TryStrToAdler32(const Str: String; out Adler32: TAdler32): Boolean;
Function StrToAdler32Def(const Str: String; Default: TAdler32): TAdler32;
Function CompareAdler32(const A,B: TAdler32): Integer;
Function SameAdler32(const A,B: TAdler32): Boolean;

//------------------------------------------------------------------------------

Function BufferAdler32(const Adler32: TAdler32; const Buffer; Size: TMemSize): TAdler32; overload;

Function BufferAdler32(const Buffer; Size: TMemSize): TAdler32; overload;

Function AnsiStringAdler32(const Str: AnsiString): TAdler32;
Function WideStringAdler32(const Str: WideString): TAdler32;
Function StringAdler32(const Str: String): TAdler32;

Function StreamAdler32(Stream: TStream; Count: Int64 = -1): TAdler32;
Function FileAdler32(const FileName: String): TAdler32;

//------------------------------------------------------------------------------

type
  TAdler32Context = type TAdler32Sys;

Function Adler32_Init: TAdler32Context;
procedure Adler32_Update(var Context: TAdler32Context; const Buffer; Size: TMemSize);
Function Adler32_Final(var Context: TAdler32Context; const Buffer; Size: TMemSize): TAdler32; overload;
Function Adler32_Final(var Context: TAdler32Context): TAdler32; overload;
Function Adler32_Hash(const Buffer; Size: TMemSize): TAdler32;

implementation

uses
  SysUtils;

{===============================================================================
--------------------------------------------------------------------------------
                                  TAdler32Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TAdler32Hash - utility functions
===============================================================================}

Function SwapEndian(Value: TAdler32Sys): TAdler32Sys; overload;
begin
Result := TAdler32Sys(
  ((Value and $000000FF) shl 24) or
  ((Value and $0000FF00) shl 8) or
  ((Value and $00FF0000) shr 8) or
  ((Value and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TAdler32): TAdler32; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := TAdler32(SwapEndian(TAdler32Sys(Value)));
end;

{===============================================================================
    TAdler32Hash - main implementation
===============================================================================}
const
  Adler32Modulo      = 65521;
  Adler32NoModRounds = 5552; // number of rounds that can be done without calculating modulo

//------------------------------------------------------------------------------  

Function Adler32Process(const Adler32: TAdler32Sys; const Buffer; Size: TMemSize): TAdler32Sys;
var
  CurrentData:  PByte;
  SumA,SumB:    UInt32;
  i:            TMemSize;
begin
If Size > 0 then
  begin
    SumA := Adler32 and $FFFF;
    SumB := (Adler32 shr 16) and $FFFF;
    CurrentData := PByte(@Buffer);
    // rounds with deferred modulo operation
    while Size >= Adler32NoModRounds do
      begin
        For i := 0 to Pred(Adler32NoModRounds) do
          begin
            SumA := SumA + CurrentData^;
            SumB := SumB + SumA;
            Inc(CurrentData);
          end;
        SumA := SumA mod Adler32Modulo;
        SumB := SumB mod Adler32Modulo;
        Dec(Size,Adler32NoModRounds);
      end;
    // remaining bytes
    If Size > 0 then
      begin
        For i := 0 to Pred(Size) do
          begin
            SumA := SumA + CurrentData^;
            SumB := SumB + SumA;
            Inc(CurrentData);
          end;
        SumA := SumA mod Adler32Modulo;
        SumB := SumB mod Adler32Modulo;
      end;
    // construct result
    Result := (SumB shl 16) or (SumA and $FFFF);         
  end
else Result := Adler32;
end;

//------------------------------------------------------------------------------

Function Adler32Compare(const A,B: TAdler32Sys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function Adler32Same(const A,B: TAdler32Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function Adler32AsString(const Adler32: TAdler32Sys): String;
begin
Result := IntToHex(Adler32,8);
end;

//------------------------------------------------------------------------------

Function Adler32FromString(const Str: String): TAdler32Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TAdler32Sys(StrToInt(Str))
    else
      Result := TAdler32Sys(StrToInt('$' + Str));
  end
else Result := TAdler32Hash.Adler32ToSys(ZeroAdler32);
end;


{===============================================================================
    TAdler32Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TAdler32Hash - protected methods
-------------------------------------------------------------------------------}

Function TAdler32Hash.GetAdler32: TAdler32;
begin
Result := Adler32FromSys(fAdler32);
end;

//------------------------------------------------------------------------------

procedure TAdler32Hash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fAdler32 := Adler32Process(fAdler32,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TAdler32Hash.Initialize;
begin
inherited;
fAdler32 := 0;
end;

{-------------------------------------------------------------------------------
    TAdler32Hash - public methods
-------------------------------------------------------------------------------}

class Function TAdler32Hash.Adler32ToSys(Adler32: TAdler32): TAdler32Sys;
begin
Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TAdler32Sys(Adler32));
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.Adler32FromSys(Adler32: TAdler32Sys): TAdler32;
begin
Result := TAdler32({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Adler32));
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.Adler32ToLE(Adler32: TAdler32): TAdler32;
begin
Result := Adler32;
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.Adler32ToBE(Adler32: TAdler32): TAdler32;
begin
Result := SwapEndian(Adler32);
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.Adler32FromLE(Adler32: TAdler32): TAdler32;
begin
Result := Adler32;
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.Adler32FromBE(Adler32: TAdler32): TAdler32;
begin
Result := SwapEndian(Adler32);
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.HashType: THashType;
begin
Result := htChecksum;
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.HashSize: TMemSize;
begin
Result := SizeOf(TAdler32);
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.HashName: String;
begin
Result := 'Adler-32';
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TAdler32Hash.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

constructor TAdler32Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TAdler32Hash then
  fAdler32 := TAdler32Hash(Hash).Adler32Sys
else
  raise EADLER32IncompatibleClass.CreateFmt('TAdler32Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TAdler32Hash.CreateAndInitFrom(Hash: TAdler32);
begin
CreateAndInit;
fAdler32 := Adler32ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TAdler32Hash.Init;
begin
inherited;
fAdler32 := Adler32ToSys(InitialAdler32);
end;

//------------------------------------------------------------------------------

Function TAdler32Hash.Compare(Hash: THashBase): Integer;
begin
If Hash is TAdler32Hash then
  Result := Adler32Compare(fAdler32,TAdler32Hash(Hash).Adler32Sys)
else
  raise EADLER32IncompatibleClass.CreateFmt('TAdler32Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TAdler32Hash.Same(Hash: THashBase): Boolean;
begin
If Hash is TAdler32Hash then
  Result := Adler32Same(fAdler32,TAdler32Hash(Hash).Adler32Sys)
else
  raise EADLER32IncompatibleClass.CreateFmt('TAdler32Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TAdler32Hash.AsString: String;
begin
Result := Adler32AsString(fAdler32);
end;

//------------------------------------------------------------------------------

procedure TAdler32Hash.FromString(const Str: String);
begin
fAdler32 := Adler32FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TAdler32Hash.FromStringDef(const Str: String; const Default: TAdler32);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fAdler32 := Adler32ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TAdler32Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TAdler32;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}Adler32ToBE{$ELSE}Adler32ToLE{$ENDIF}(Adler32FromSys(fAdler32));
  heLittle: Temp := Adler32ToLE(Adler32FromSys(fAdler32));
  heBig:    Temp := Adler32ToBE(Adler32FromSys(fAdler32));
else
 {heDefault}
  Temp := Adler32FromSys(fAdler32);
end;
Stream.WriteBuffer(Temp,SizeOf(TAdler32));
end;

//------------------------------------------------------------------------------

procedure TAdler32Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TAdler32;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(TAdler32));
case Endianness of
  heSystem: fAdler32 := Adler32ToSys({$IFDEF ENDIAN_BIG}Adler32FromBE{$ELSE}Adler32FromLE{$ENDIF}(Temp));
  heLittle: fAdler32 := Adler32ToSys(Adler32FromLE(Temp));
  heBig:    fAdler32 := Adler32ToSys(Adler32FromBE(Temp));
else
 {heDefault}
  fAdler32 := Adler32ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Standalone functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Standalone functions - utility functions
-------------------------------------------------------------------------------}

Function Adler32ToStr(const Adler32: TAdler32): String;
begin
Result := Adler32AsString(TAdler32Hash.Adler32ToSys(Adler32));
end;

//------------------------------------------------------------------------------

Function StrToAdler32(const Str: String): TAdler32;
begin
Result := TAdler32Hash.Adler32FromSys(Adler32FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToAdler32(const Str: String; out Adler32: TAdler32): Boolean;
begin
try
  Adler32 := TAdler32Hash.Adler32FromSys(Adler32FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToAdler32Def(const Str: String; Default: TAdler32): TAdler32;
begin
If not TryStrToAdler32(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareAdler32(const A,B: TAdler32): Integer;
begin
Result := Adler32Compare(TAdler32Hash.Adler32ToSys(A),TAdler32Hash.Adler32ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameAdler32(const A,B: TAdler32): Boolean;
begin
Result := Adler32Same(TAdler32Hash.Adler32ToSys(A),TAdler32Hash.Adler32ToSys(B));
end;

{-------------------------------------------------------------------------------
    Standalone functions - processing functions
-------------------------------------------------------------------------------}

Function BufferAdler32(const Adler32: TAdler32; const Buffer; Size: TMemSize): TAdler32;
begin
Result := TAdler32Hash.Adler32FromSys(Adler32Process(TAdler32Hash.Adler32ToSys(Adler32),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferAdler32(const Buffer; Size: TMemSize): TAdler32;
begin
Result := TAdler32Hash.Adler32FromSys(Adler32Process(TAdler32Hash.Adler32ToSys(InitialAdler32),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringAdler32(const Str: AnsiString): TAdler32;
begin
Result := BufferAdler32(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringAdler32(const Str: WideString): TAdler32;
begin
Result := BufferAdler32(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringAdler32(const Str: String): TAdler32;
begin
Result := BufferAdler32(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamAdler32(Stream: TStream; Count: Int64 = -1): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileAdler32(const FileName: String): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Standalone functions - context functions
-------------------------------------------------------------------------------}

Function Adler32_Init: TAdler32Context;
begin
Result := TAdler32Context(TAdler32Hash.Adler32ToSys(InitialAdler32));
end;

//------------------------------------------------------------------------------

procedure Adler32_Update(var Context: TAdler32Context; const Buffer; Size: TMemSize);
begin
TAdler32Sys(Context) := Adler32Process(TAdler32Sys(Context),Buffer,Size);
end;

//------------------------------------------------------------------------------

Function Adler32_Final(var Context: TAdler32Context; const Buffer; Size: TMemSize): TAdler32;
begin
Adler32_Update(Context,Buffer,Size);
Result := Adler32_Final(Context);
end;

//------------------------------------------------------------------------------

Function Adler32_Final(var Context: TAdler32Context): TAdler32;
begin
Result := TAdler32Hash.Adler32FromSys(TAdler32Sys(Context));
Context := TAdler32Context(TAdler32Hash.Adler32ToSys(ZeroAdler32));
end;

//------------------------------------------------------------------------------

Function Adler32_Hash(const Buffer; Size: TMemSize): TAdler32;
begin
Result := BufferAdler32(Buffer,Size);
end;

end.
