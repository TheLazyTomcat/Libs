{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  MD5 calculation

  Version 1.6.2 (2026-07-05)

  Last change 2026-07-05

  ©2015-2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.MD5

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps   - github.com/TheLazyTomcat/Lib.BitOps
    HashBase - github.com/TheLazyTomcat/Lib.HashBase

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    ListUtils          - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit MD5;

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

interface

uses
  Classes,
  AuxTypes, HashBase;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EMD5Exception = class(EHashException);

  EMD5IncompatibleClass = class(EMD5Exception);
  EMD5ProcessingError   = class(EMD5Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in type TMD5 are always ordered from most significant byte to least
  significant byte (big endian).

  Type TMD5Sys has no such guarantee and its internal structure depends on
  current implementation.
}
type
  TMD5 = packed array[0..15] of UInt8;
  PMD5 = ^TMD5;

  TMD5Sys = packed record
    PartA:  UInt32;
    PartB:  UInt32;
    PartC:  UInt32;
    PartD:  UInt32;
  end;
  PMD5Sys = ^TMD5Sys;

const
  InitialMD5: TMD5 = ($01,$23,$45,$67,$89,$AB,$CD,$EF,$FE,$DC,$BA,$98,$76,$54,$32,$10);
  ZeroMD5:    TMD5 = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

{===============================================================================
--------------------------------------------------------------------------------
                                    TMD5Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD5Hash - class declaration
===============================================================================}
type
  TMD5Hash = class(TBlockHash)
  protected
    fMD5: TMD5Sys;
    Function GetMD5: TMD5; virtual;
    procedure ProcessBlock(const Block); override;
    procedure ProcessFirst(const Block); override;
    procedure ProcessLast; override;
    procedure Initialize; override;
  public
    class Function MD5ToSys(MD5: TMD5): TMD5Sys; virtual;
    class Function MD5FromSys(MD5: TMD5Sys): TMD5; virtual;
    class Function MD5ToLE(MD5: TMD5): TMD5; virtual;
    class Function MD5ToBE(MD5: TMD5): TMD5; virtual;
    class Function MD5FromLE(MD5: TMD5): TMD5; virtual;
    class Function MD5FromBE(MD5: TMD5): TMD5; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMD5); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMD5); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property MD5: TMD5 read GetMD5;
    property MD5Sys: TMD5Sys read fMD5;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Standalone functions - declaration
===============================================================================}

Function MD5toStr(const MD5: TMD5): String;
Function StrToMD5(const Str: String): TMD5;
Function TryStrToMD5(const Str: String; out MD5: TMD5): Boolean;
Function StrToMD5Def(const Str: String; Default: TMD5): TMD5;

Function CompareMD5(const A,B: TMD5): Integer;
Function SameMD5(const A,B: TMD5): Boolean;

Function BinaryCorrectMD5(const MD5: TMD5): TMD5; deprecated;

//------------------------------------------------------------------------------

procedure BufferMD5(var MD5: TMD5; const Buffer; Size: TMemSize); overload;

Function LastBufferMD5(const MD5: TMD5; const Buffer; Size: TMemSize; MessageLength: UInt64): TMD5; overload;
Function LastBufferMD5(const MD5: TMD5; const Buffer; Size: TMemSize): TMD5; overload;

Function BufferMD5(const Buffer; Size: TMemSize): TMD5; overload;

Function AnsiStringMD5(const Str: AnsiString): TMD5;
Function WideStringMD5(const Str: WideString): TMD5;
Function StringMD5(const Str: String): TMD5;

Function StreamMD5(Stream: TStream; Count: Int64 = -1): TMD5;
Function FileMD5(const FileName: String): TMD5;

//------------------------------------------------------------------------------   
type
  TMD5Context = type Pointer;

Function MD5_Init: TMD5Context;
procedure MD5_Update(Context: TMD5Context; const Buffer; Size: TMemSize);
Function MD5_Final(var Context: TMD5Context; const Buffer; Size: TMemSize): TMD5; overload;
Function MD5_Final(var Context: TMD5Context): TMD5; overload;
Function MD5_Hash(const Buffer; Size: TMemSize): TMD5;

implementation

uses
  SysUtils,
  BitOps;

{===============================================================================
--------------------------------------------------------------------------------
                                    TMD5Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD5Hash - calculation constants
===============================================================================}
const
  MD5_COEF_SHIFT: array[0..63] of UInt8 = (
    7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
    5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
    4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
    6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21);

  MD5_COEF_SIN: array[0..63] of UInt32 = (
    $D76AA478,$E8C7B756,$242070DB,$C1BDCEEE,$F57C0FAF,$4787C62A,$A8304613,$FD469501,
    $698098D8,$8B44F7AF,$FFFF5BB1,$895CD7BE,$6B901122,$FD987193,$A679438E,$49B40821,
    $F61E2562,$C040B340,$265E5A51,$E9B6C7AA,$D62F105D,$02441453,$D8A1E681,$E7D3FBC8,
    $21E1CDE6,$C33707D6,$F4D50D87,$455A14ED,$A9E3E905,$FCEFA3F8,$676F02D9,$8D2A4C8A,
    $FFFA3942,$8771F681,$6D9D6122,$FDE5380C,$A4BEEA44,$4BDECFA9,$F6BB4B60,$BEBFBC70,
    $289B7EC6,$EAA127FA,$D4EF3085,$04881D05,$D9D4D039,$E6DB99E5,$1FA27CF8,$C4AC5665,
    $F4292244,$432AFF97,$AB9423A7,$FC93A039,$655B59C3,$8F0CCC92,$FFEFF47D,$85845DD1,
    $6FA87E4F,$FE2CE6E0,$A3014314,$4E0811A1,$F7537E82,$BD3AF235,$2AD7D2BB,$EB86D391);

  MD5_COEF_MOD: array[0..63] of UInt8 = (
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    1,  6, 11,  0,  5, 10, 15,  4,  9, 14,  3,  8, 13,  2,  7, 12,
    5,  8, 11, 14,  1,  4,  7, 10, 13,  0,  3,  6,  9, 12, 15,  2,
    0,  7, 14,  5, 12,  3, 10,  1,  8, 15,  6, 13,  4, 11,  2,  9);

{===============================================================================
    TMD5Hash - auxiliary functions
===============================================================================}

Function MD5Compare(const A,B: TMD5): Integer;
var
  i:  Integer;
begin
Result := 0;
For i := Low(A) to High(A) do
  If A[i] > B[i] then
    begin
      Result := +1;
      Break;
    end
  else If A[i] < B[i] then
    begin
      Result := -1;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function MD5Same(const A,B: TMD5): Boolean;
var
  i:  Integer;
begin
Result := True;
For i := Low(A) to High(A) do
  If A[i] <> B[i] then
    begin
      Result := False;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function MD5AsString(const MD5: TMD5): String;
var
  i:  Integer;
begin
Result := StringOfChar('0',SizeOf(TMD5) * 2);
For i := Low(MD5) to High(MD5) do
  begin
    Result[(i * 2) + 2] := IntToHex(MD5[i] and $0F,1)[1];
    Result[(i * 2) + 1] := IntToHex(MD5[i] shr 4,1)[1];
  end;
end;

//------------------------------------------------------------------------------

Function MD5FromString(const Str: String): TMD5;
var
  TempStr:  String;
  i:        Integer;
begin
If Length(Str) < (SizeOf(TMD5) * 2) then
  TempStr := StringOfChar('0',(SizeOf(TMD5) * 2) - Length(Str)) + Str
else If Length(Str) > (SizeOf(TMD5) * 2) then
  TempStr := Copy(Str,Length(Str) - Pred(SizeOf(TMD5) * 2),SizeOf(TMD5) * 2)
else
  TempStr := Str;
For i := Low(Result) to High(Result) do
  Result[i] := UInt8(StrToInt('$' + Copy(TempStr,(i * 2) + 1,2)));
end;

{===============================================================================
    TMD5Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD5Hash - protected methods
-------------------------------------------------------------------------------}

Function TMD5Hash.GetMD5: TMD5;
begin
Result := MD5FromSys(fMD5);
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
procedure TMD5Hash.ProcessBlock(const Block);
var
  Hash:       TMD5Sys;
  BlockWords: packed array[0..15] of UInt32 absolute Block;
  i:          Integer;
  FuncResult: UInt32;
  Temp:       UInt32;
begin
Hash := fMD5;
For i := 0 to 63 do // 64 cycles
  begin
    case i of
       0..15: FuncResult := (Hash.PartB and Hash.PartC) or ((not Hash.PartB) and Hash.PartD);
      16..31: FuncResult := (Hash.PartD and Hash.PartB) or (Hash.PartC and (not Hash.PartD));
      32..47: FuncResult := Hash.PartB xor Hash.PartC xor Hash.PartD;
    else
     {48..63:}FuncResult := Hash.PartC xor (Hash.PartB or (not Hash.PartD));
    end;
    Temp := Hash.PartD;
    Hash.PartD := Hash.PartC;
    Hash.PartC := Hash.PartB;
    Hash.PartB := UInt32(Hash.PartB + ROL(UInt32(Hash.PartA + FuncResult + MD5_COEF_SIN[i] +
      {$IFDEF ENDIAN_BIG}EndianSwap{$ENDIF}(BlockWords[MD5_COEF_MOD[i]])),MD5_COEF_SHIFT[i]));
    Hash.PartA := Temp;
  end;
fMD5.PartA := UInt32(fMD5.PartA + Hash.PartA);
fMD5.PartB := UInt32(fMD5.PartB + Hash.PartB);
fMD5.PartC := UInt32(fMD5.PartC + Hash.PartC);
fMD5.PartD := UInt32(fMD5.PartD + Hash.PartD);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure TMD5Hash.ProcessFirst(const Block);
begin
inherited;
ProcessBlock(Block);
end;

//------------------------------------------------------------------------------

procedure TMD5Hash.ProcessLast;
begin
If (fBlockSize - fTransCount) >= (SizeOf(UInt64) + 1) then
  begin
    // padding and length can fit
    FillChar(PtrAdvance(fTransBlock,TMemOff(fTransCount))^,fBlockSize - fTransCount,0);
    PUInt8(PtrAdvance(fTransBlock,TMemOff(fTransCount)))^ := $80;
    PUInt64(PtrAdvance(fTransBlock,TMemOff(fBlockSize) - SizeOf(UInt64)))^ :=
      {$IFDEF ENDIAN_BIG}EndianSwap{$ENDIF}(UInt64(fProcessedBytes) * 8);
    ProcessBlock(fTransBlock^);
  end
else
  begin
    // padding and length cannot fit  
    If fBlockSize > fTransCount then
      begin
        FillChar(PtrAdvance(fTransBlock,TMemOff(fTransCount))^,fBlockSize - fTransCount,0);
        PUInt8(PtrAdvance(fTransBlock,TMemOff(fTransCount)))^ := $80;
        ProcessBlock(fTransBlock^);
        FillChar(fTransBlock^,fBlockSize,0);
        PUInt64(PtrAdvance(fTransBlock,TMemOff(fBlockSize) - SizeOf(UInt64)))^ :=
          {$IFDEF ENDIAN_BIG}EndianSwap{$ENDIF}(UInt64(fProcessedBytes) * 8);
        ProcessBlock(fTransBlock^);        
      end
    else raise EMD5ProcessingError.CreateFmt('TMD5Hash.ProcessLast: Invalid data transfer (%d).',[fTransCount]);
  end;
end;

//------------------------------------------------------------------------------

procedure TMD5Hash.Initialize;
begin
fBlockSize := 64; // 512 bits
inherited;
fMD5 := MD5ToSys(ZeroMD5);
end;

{-------------------------------------------------------------------------------
    TMD5Hash - public methods
-------------------------------------------------------------------------------}

class Function TMD5Hash.MD5ToSys(MD5: TMD5): TMD5Sys;
var
  Temp: TMD5Sys absolute MD5;
begin
Result := Temp;
{$IFDEF ENDIAN_BIG}
EndianSwapValue(Result.PartA);
EndianSwapValue(Result.PartB);
EndianSwapValue(Result.PartC);
EndianSwapValue(Result.PartD);
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TMD5Hash.MD5FromSys(MD5: TMD5Sys): TMD5;
var
  Temp: TMD5Sys absolute Result;
begin
Temp := MD5;
{$IFDEF ENDIAN_BIG}
EndianSwapValue(Temp.PartA);
EndianSwapValue(Temp.PartB);
EndianSwapValue(Temp.PartC);
EndianSwapValue(Temp.PartD);
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TMD5Hash.MD5ToLE(MD5: TMD5): TMD5;
begin
Result := MD5;
SwapEndian(Result,SizeOf(TMD5));
end;

//------------------------------------------------------------------------------

class Function TMD5Hash.MD5ToBE(MD5: TMD5): TMD5;
begin
Result := MD5;
end;

//------------------------------------------------------------------------------

class Function TMD5Hash.MD5FromLE(MD5: TMD5): TMD5;
begin
Result := MD5;
SwapEndian(Result,SizeOf(TMD5));
end;

//------------------------------------------------------------------------------

class Function TMD5Hash.MD5FromBE(MD5: TMD5): TMD5;
begin
Result := MD5;
end;
 
//------------------------------------------------------------------------------

class Function TMD5Hash.HashSize: TMemSize;
begin
Result := SizeOf(TMD5);
end;

//------------------------------------------------------------------------------

class Function TMD5Hash.HashName: String;
begin
Result := 'MD5';
end;

//------------------------------------------------------------------------------

class Function TMD5Hash.HashEndianness: THashEndianness;
begin
// first byte is most significant
Result := heBig;
end;

//------------------------------------------------------------------------------

class Function TMD5Hash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TMD5Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMD5Hash then
  fMD5 := TMD5Hash(Hash).MD5Sys
else
  raise EMD5IncompatibleClass.CreateFmt('TMD5Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMD5Hash.CreateAndInitFrom(Hash: TMD5);
begin
CreateAndInit;
fMD5 := MD5ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TMD5Hash.Init;
begin
inherited;
fMD5 := MD5toSys(InitialMD5);
end;

//------------------------------------------------------------------------------

Function TMD5Hash.Compare(Hash: THashBase): Integer;
begin
If Hash is TMD5Hash then
  Result := MD5Compare(MD5FromSys(fMD5),TMD5Hash(Hash).MD5)
else
  raise EMD5IncompatibleClass.CreateFmt('TMD5Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMD5Hash.Same(Hash: THashBase): Boolean;
begin
If Hash is TMD5Hash then
  Result := MD5Same(MD5FromSys(fMD5),TMD5Hash(Hash).MD5)
else
  raise EMD5IncompatibleClass.CreateFmt('TMD5Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMD5Hash.AsString: String;
begin
Result := MD5AsString(MD5FromSys(fMD5));
end;

//------------------------------------------------------------------------------

procedure TMD5Hash.FromString(const Str: String);
begin
fMD5 := MD5ToSys(MD5FromString(Str));
end;

//------------------------------------------------------------------------------

procedure TMD5Hash.FromStringDef(const Str: String; const Default: TMD5);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMD5 := MD5ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TMD5Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMD5;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}MD5ToBE{$ELSE}MD5ToLE{$ENDIF}(MD5FromSys(fMD5));
  heLittle: Temp := MD5ToLE(MD5FromSys(fMD5));
  heBig:    Temp := MD5ToBE(MD5FromSys(fMD5));
else
 {heDefault}
  Temp := MD5FromSys(fMD5);
end;
Stream.WriteBuffer(Temp,SizeOf(TMD5));
end;

//------------------------------------------------------------------------------

procedure TMD5Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMD5;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(TMD5));
case Endianness of
  heSystem: fMD5 := MD5ToSys({$IFDEF ENDIAN_BIG}MD5FromBE{$ELSE}MD5FromLE{$ENDIF}(Temp));
  heLittle: fMD5 := MD5ToSys(MD5FromLE(Temp));
  heBig:    fMD5 := MD5ToSys(MD5FromBE(Temp));
else
 {heDefault}
  fMD5 := MD5ToSys(Temp);
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

Function MD5toStr(const MD5: TMD5): String;
begin
Result := MD5AsString(MD5);
end;

//------------------------------------------------------------------------------

Function StrToMD5(const Str: String): TMD5;
begin
Result := MD5FromString(Str);
end;

//------------------------------------------------------------------------------

Function TryStrToMD5(const Str: String; out MD5: TMD5): Boolean;
begin
try
  MD5 := MD5FromString(Str);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToMD5Def(const Str: String; Default: TMD5): TMD5;
begin
If not TryStrToMD5(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareMD5(const A,B: TMD5): Integer;
begin
Result := MD5Compare(A,B);
end;

//------------------------------------------------------------------------------

Function SameMD5(const A,B: TMD5): Boolean;
begin
Result := MD5Same(A,B);
end;

//------------------------------------------------------------------------------

Function BinaryCorrectMD5(const MD5: TMD5): TMD5;
begin
Result := MD5;
end;

{-------------------------------------------------------------------------------
    Standalone functions - processing functions
-------------------------------------------------------------------------------}

procedure BufferMD5(var MD5: TMD5; const Buffer; Size: TMemSize);
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.CreateAndInitFrom(MD5);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          MD5 := Hash.MD5;
        end
      else raise EMD5ProcessingError.CreateFmt('BufferMD5: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferMD5(const MD5: TMD5; const Buffer; Size: TMemSize; MessageLength: UInt64): TMD5;
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.CreateAndInitFrom(MD5);
try
  Hash.ProcessedBytes := (MessageLength shr 3) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.MD5;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferMD5(const MD5: TMD5; const Buffer; Size: TMemSize): TMD5;
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.CreateAndInitFrom(MD5);
try
  Hash.Final(Buffer,Size);
  Result := Hash.MD5;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function BufferMD5(const Buffer; Size: TMemSize): TMD5;
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.MD5;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringMD5(const Str: AnsiString): TMD5;
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.Create;
try
  Hash.HashAnsiString(Str);
  Result := Hash.MD5;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function WideStringMD5(const Str: WideString): TMD5;
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.Create;
try
  Hash.HashWideString(Str);
  Result := Hash.MD5;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StringMD5(const Str: String): TMD5;
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.Create;
try
  Hash.HashString(Str);
  Result := Hash.MD5;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function StreamMD5(Stream: TStream; Count: Int64 = -1): TMD5;
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.MD5;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMD5(const FileName: String): TMD5;
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.MD5;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Standalone functions - context functions
-------------------------------------------------------------------------------}

Function MD5_Init: TMD5Context;
var
  Temp: TMD5Hash;
begin
Temp := TMD5Hash.CreateAndInit;
Result := TMD5Context(Temp);
end;

//------------------------------------------------------------------------------

procedure MD5_Update(Context: TMD5Context; const Buffer; Size: TMemSize);
begin
TMD5Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function MD5_Final(var Context: TMD5Context; const Buffer; Size: TMemSize): TMD5;
begin
MD5_Update(Context,Buffer,Size);
Result := MD5_Final(Context);
end;

//------------------------------------------------------------------------------

Function MD5_Final(var Context: TMD5Context): TMD5;
begin
TMD5Hash(Context).Final;
Result := TMD5Hash(Context).MD5;
FreeAndNil(TMD5Hash(Context));
end;

//------------------------------------------------------------------------------

Function MD5_Hash(const Buffer; Size: TMemSize): TMD5;
var
  Hash: TMD5Hash;
begin
Hash := TMD5Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.MD5;
finally
  Hash.Free;
end;
end;

end.
