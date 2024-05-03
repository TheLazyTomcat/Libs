{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Adler-32 calculation

  Version 1.2.3 (2020-07-13)

  Last change 2024-04-14

  ©2018-2024 František Milt

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
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
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

type
  EADLER32Exception = class(EHASHException);

  EADLER32IncompatibleClass = class(EADLER32Exception);

{-------------------------------------------------------------------------------
================================================================================
                                  TAdler32Hash
================================================================================
-------------------------------------------------------------------------------}
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
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TAdler32); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TAdler32); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Adler32: TAdler32 read GetAdler32;
    property Adler32Sys: TAdler32Sys read fAdler32;
  end;

{===============================================================================
    Backward compatibility functions
===============================================================================}

Function Adler32ToStr(Adler32: TAdler32): String;
Function StrToAdler32(const Str: String): TAdler32;
Function TryStrToAdler32(const Str: String; out Adler32: TAdler32): Boolean;
Function StrToAdler32Def(const Str: String; Default: TAdler32): TAdler32;
Function CompareAdler32(A,B: TAdler32): Integer;
Function SameAdler32(A,B: TAdler32): Boolean;

//------------------------------------------------------------------------------

Function BufferAdler32(Adler32: TAdler32; const Buffer; Size: TMemSize): TAdler32; overload;

Function BufferAdler32(const Buffer; Size: TMemSize): TAdler32; overload;

Function AnsiStringAdler32(const Str: AnsiString): TAdler32;
Function WideStringAdler32(const Str: WideString): TAdler32;
Function StringAdler32(const Str: String): TAdler32;

Function StreamAdler32(Stream: TStream; Count: Int64 = -1): TAdler32;
Function FileAdler32(const FileName: String): TAdler32;

//------------------------------------------------------------------------------

type
  TAdler32Context = type Pointer;

Function Adler32_Init: TAdler32Context;
procedure Adler32_Update(var Context: TAdler32Context; const Buffer; Size: TMemSize);
Function Adler32_Final(var Context: TAdler32Context; const Buffer; Size: TMemSize): TAdler32; overload;
Function Adler32_Final(var Context: TAdler32Context): TAdler32; overload;
Function Adler32_Hash(const Buffer; Size: TMemSize): TAdler32;

implementation

uses
  SysUtils;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5057:={$WARN 5057 OFF}} // Local variable "$1" does not seem to be initialized
{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                  TAdler32Hash
================================================================================
-------------------------------------------------------------------------------}

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
    TAdler32Hash - calculation constants
===============================================================================}

const
  Adler32Modulo   = 65521;
  Adler32NMRounds = 5552; // number of rounds that can be done without calculating modulo

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
var
  SumA: UInt32;
  SumB: UInt32;
  Buff: PByte;
  i:    TMemSize;
begin
If Size > 0 then
  begin
    SumA := fAdler32 and $FFFF;
    SumB := (fAdler32 shr 16) and $FFFF;
    Buff := PByte(@Buffer);
    // rounds with deferred modulo
    while Size >= Adler32NMRounds do
      begin
        For i := 0 to Pred(Adler32NMRounds) do
          begin
          {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
            SumA := SumA + PByte(PtrUInt(Buff) + PtrUInt(i))^;
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
            SumB := SumB + SumA;
          end;
        SumA := SumA mod Adler32Modulo;
        SumB := SumB mod Adler32Modulo;
        Inc(Buff,Adler32NMRounds);
        Dec(Size,Adler32NMRounds);
      end;
    // remaining bytes
    If Size > 0 then
      begin
        For i := 0 to Pred(Size) do
          begin
          {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
            SumA := (SumA + PByte(PtrUInt(Buff) + PtrUInt(i))^);
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
            SumB := (SumB + SumA);
          end;
        SumA := SumA mod Adler32Modulo;
        SumB := SumB mod Adler32Modulo;
      end;
    // construct result
    fAdler32 := (SumB shl 16) or (SumA and $FFFF);
  end;
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
  begin
    If fAdler32 > TAdler32Hash(Hash).Adler32Sys then
      Result := +1
    else If fAdler32 < TAdler32Hash(Hash).Adler32Sys then
      Result := -1
    else
      Result := 0;
  end
else raise EADLER32IncompatibleClass.CreateFmt('TAdler32Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TAdler32Hash.AsString: String;
begin
Result := IntToHex(fAdler32,8);
end;

//------------------------------------------------------------------------------

procedure TAdler32Hash.FromString(const Str: String);
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      fAdler32 := TAdler32Sys(StrToInt(Str))
    else
      fAdler32 := TAdler32Sys(StrToInt('$' + Str));
  end
else fAdler32 := Adler32ToSys(InitialAdler32);
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

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TAdler32Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TAdler32;
begin
Stream.ReadBuffer(Temp,SizeOf(TAdler32));
case Endianness of
  heSystem: fAdler32 := Adler32ToSys({$IFDEF ENDIAN_BIG}Adler32FromBE{$ELSE}Adler32FromLE{$ENDIF}(Temp));
  heLittle: fAdler32 := Adler32ToSys(Adler32FromLE(Temp));
  heBig:    fAdler32 := Adler32ToSys(Adler32FromBE(Temp));
else
 {heDefault}
  fAdler32 := Adler32ToSys(Temp);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{===============================================================================
    Backward compatibility functions
===============================================================================}
{-------------------------------------------------------------------------------
    Backward compatibility functions - utility functions
-------------------------------------------------------------------------------}

Function Adler32ToStr(Adler32: TAdler32): String;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.CreateAndInitFrom(Adler32);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToAdler32(const Str: String): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function TryStrToAdler32(const Str: String; out Adler32: TAdler32): Boolean;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    Adler32 := Hash.Adler32;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToAdler32Def(const Str: String; Default: TAdler32): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function CompareAdler32(A,B: TAdler32): Integer;
var
  HashA:  TAdler32Hash;
  HashB:  TAdler32Hash;
begin
HashA := TAdler32Hash.CreateAndInitFrom(A);
try
  HashB := TAdler32Hash.CreateAndInitFrom(B);
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

Function SameAdler32(A,B: TAdler32): Boolean;
var
  HashA:  TAdler32Hash;
  HashB:  TAdler32Hash;
begin
HashA := TAdler32Hash.CreateAndInitFrom(A);
try
  HashB := TAdler32Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Same(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - processing functions
-------------------------------------------------------------------------------}

Function BufferAdler32(Adler32: TAdler32; const Buffer; Size: TMemSize): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.CreateAndInitFrom(Adler32);
try
  Hash.Final(Buffer,Size);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferAdler32(const Buffer; Size: TMemSize): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringAdler32(const Str: AnsiString): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Hash.HashAnsiString(Str);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function WideStringAdler32(const Str: WideString): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Hash.HashWideString(Str);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StringAdler32(const Str: String): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Hash.HashString(Str);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
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
    Backward compatibility functions - context functions
-------------------------------------------------------------------------------}

Function Adler32_Init: TAdler32Context;
var
  Temp: TAdler32Hash;
begin
Temp := TAdler32Hash.Create;
Temp.Init;
Result := TAdler32Context(Temp);
end;

//------------------------------------------------------------------------------

procedure Adler32_Update(var Context: TAdler32Context; const Buffer; Size: TMemSize);
begin
TAdler32Hash(Context).Update(Buffer,Size);
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
TAdler32Hash(Context).Final;
Result := TAdler32Hash(Context).Adler32;
FreeAndNil(TAdler32Hash(Context));
end;

//------------------------------------------------------------------------------

Function Adler32_Hash(const Buffer; Size: TMemSize): TAdler32;
var
  Hash: TAdler32Hash;
begin
Hash := TAdler32Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.Adler32;
finally
  Hash.Free;
end;
end;

end.
