{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  MD4 calculation

    Please note that implementation of MD4 is more-or-less just a direct copy
    of MD5 codebase, only with changed block processing.

  Version 1.4.1 (2020-07-13)

  Last change 2023-05-01

  ©2015-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.MD4

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps   - github.com/TheLazyTomcat/Lib.BitOps
    HashBase - github.com/TheLazyTomcat/Lib.HashBase

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo  

===============================================================================}
unit MD4;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
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
    Common types and constants
===============================================================================}
{
  Bytes in type TMD4 are always ordered from most significant byte to least
  significant byte (big endian).

  Type TMD4Sys has no such guarantee and its internal structure depends on
  current implementation.

  MD4 does not differ in little and big endian form, as it is not a single
  quantity, therefore methods like MD4ToLE or MD4ToBE do nothing and are
  present only for the sake of completeness.
}
type
  TMD4 = packed array[0..15] of UInt8;
  PMD4 = ^TMD4;

  TMD4Sys = packed record
    PartA:  UInt32;
    PartB:  UInt32;
    PartC:  UInt32;
    PartD:  UInt32;
  end;
  PMD4Sys = ^TMD4Sys;

const
  InitialMD4: TMD4 = ($01,$23,$45,$67,$89,$AB,$CD,$EF,$FE,$DC,$BA,$98,$76,$54,$32,$10);
  ZeroMD4:    TMD4 = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

type
  EMD4Exception = class(EHashException);

  EMD4IncompatibleClass = class(EMD4Exception);
  EMD4ProcessingError   = class(EMD4Exception);

{-------------------------------------------------------------------------------
================================================================================
                                    TMD4Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TMD4Hash - class declaration
===============================================================================}
type
  TMD4Hash = class(TBlockHash)
  protected
    fMD4: TMD4Sys;
    Function GetMD4: TMD4; virtual;
    procedure ProcessBlock(const Block); override;
    procedure ProcessFirst(const Block); override;
    procedure ProcessLast; override;
    procedure Initialize; override;
  public
    class Function MD4ToSys(MD4: TMD4): TMD4Sys; virtual;
    class Function MD4FromSys(MD4: TMD4Sys): TMD4; virtual;
    class Function MD4ToLE(MD4: TMD4): TMD4; virtual;
    class Function MD4ToBE(MD4: TMD4): TMD4; virtual;
    class Function MD4FromLE(MD4: TMD4): TMD4; virtual;
    class Function MD4FromBE(MD4: TMD4): TMD4; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMD4); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMD4); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property MD4: TMD4 read GetMD4;
    property MD4Sys: TMD4Sys read fMD4;
  end;

{===============================================================================
    Backward compatibility functions
===============================================================================}

Function MD4toStr(MD4: TMD4): String;
Function StrToMD4(Str: String): TMD4;
Function TryStrToMD4(const Str: String; out MD4: TMD4): Boolean;
Function StrToMD4Def(const Str: String; Default: TMD4): TMD4;

Function CompareMD4(A,B: TMD4): Integer;
Function SameMD4(A,B: TMD4): Boolean;

Function BinaryCorrectMD4(MD4: TMD4): TMD4;

procedure BufferMD4(var MD4: TMD4; const Buffer; Size: TMemSize); overload;
Function LastBufferMD4(MD4: TMD4; const Buffer; Size: TMemSize; MessageLength: UInt64): TMD4; overload;
Function LastBufferMD4(MD4: TMD4; const Buffer; Size: TMemSize): TMD4; overload;

Function BufferMD4(const Buffer; Size: TMemSize): TMD4; overload;

Function AnsiStringMD4(const Str: AnsiString): TMD4;
Function WideStringMD4(const Str: WideString): TMD4;
Function StringMD4(const Str: String): TMD4;

Function StreamMD4(Stream: TStream; Count: Int64 = -1): TMD4;
Function FileMD4(const FileName: String): TMD4;

//------------------------------------------------------------------------------

type
  TMD4Context = type Pointer;

Function MD4_Init: TMD4Context;
procedure MD4_Update(Context: TMD4Context; const Buffer; Size: TMemSize);
Function MD4_Final(var Context: TMD4Context; const Buffer; Size: TMemSize): TMD4; overload;
Function MD4_Final(var Context: TMD4Context): TMD4; overload;
Function MD4_Hash(const Buffer; Size: TMemSize): TMD4;


implementation

uses
  SysUtils,
  BitOps;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W4056:={$WARN 4056 OFF}} // Conversion between ordinals and pointers is not portable  
  {$DEFINE W5057:={$WARN 5057 OFF}} // Local variable "$1" does not seem to be initialized
{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                    TMD4Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TMD4Hash - calculation constants
===============================================================================}
const
  MD4_ROUND_CONSTS: array[0..2] of UInt32 = ($00000000, $5A827999, $6ED9EBA1);

  MD4_INDEX_CONSTS: array[0..47] of UInt8 = (
    0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
    0,  4,  8, 12,  1,  5,  9, 13,  2,  6, 10, 14,  3,  7, 11, 15,
    0,  8,  4, 12,  2, 10,  6, 14,  1,  9,  5, 13,  3, 11,  7, 15);

  MD4_COEF_SHIFT: array[0..47] of UInt8 = (
    3,  7, 11, 19,  3,  7, 11, 19,  3,  7, 11, 19,  3,  7, 11, 19,
    3,  5,  9, 13,  3,  5,  9, 13,  3,  5,  9, 13,  3,  5,  9, 13,
    3,  9, 11, 15,  3,  9, 11, 15,  3,  9, 11, 15,  3,  9, 11, 15);

{===============================================================================
    TMD4Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD4Hash - protected methods
-------------------------------------------------------------------------------}

Function TMD4Hash.GetMD4: TMD4;
begin
Result := MD4FromSys(fMD4);
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
procedure TMD4Hash.ProcessBlock(const Block);
var
  Hash:           TMD4Sys;
  BlockWords:     packed array[0..15] of UInt32 absolute Block;
  i:              Integer;
  FuncResult:     UInt32;
  RoundConstant:  UInt32;
  Temp:           UInt32;
begin
Hash := fMD4;
For i := 0 to 47 do // 48 cycles
  begin
    case i of
       0..15: begin
                FuncResult := (Hash.PartB and Hash.PartC) or ((not Hash.PartB) and Hash.PartD);
                RoundConstant := MD4_ROUND_CONSTS[0];
              end;
      16..31: begin
                FuncResult := (Hash.PartB and Hash.PartC) or (Hash.PartB and Hash.PartD) or (Hash.PartC and Hash.PartD);
                RoundConstant := MD4_ROUND_CONSTS[1];
              end;
    else
      {32..47:} FuncResult := Hash.PartB xor Hash.PartC xor Hash.PartD;
                RoundConstant := MD4_ROUND_CONSTS[2];
    end;
    Temp := Hash.PartD;
    Hash.PartD := Hash.PartC;
    Hash.PartC := Hash.PartB;
    Hash.PartB := ROL(UInt32(Hash.PartA + FuncResult + RoundConstant +
      {$IFDEF ENDIAN_BIG}EndianSwap{$ENDIF}(BlockWords[MD4_INDEX_CONSTS[i]])),MD4_COEF_SHIFT[i]);
    Hash.PartA := Temp;
  end;
fMD4.PartA := UInt32(fMD4.PartA + Hash.PartA);
fMD4.PartB := UInt32(fMD4.PartB + Hash.PartB);
fMD4.PartC := UInt32(fMD4.PartC + Hash.PartC);
fMD4.PartD := UInt32(fMD4.PartD + Hash.PartD);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure TMD4Hash.ProcessFirst(const Block);
begin
inherited;
ProcessBlock(Block);
end;

//------------------------------------------------------------------------------

procedure TMD4Hash.ProcessLast;
begin
If (fBlockSize - fTransCount) >= (SizeOf(UInt64) + 1) then
  begin
    // padding and length can fit
  {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
    FillChar(Pointer(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^,fBlockSize - fTransCount,0);
    PUInt8(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^ := $80;
    PUInt64(PtrUInt(fTransBlock) + (PtrUInt(fBlockSize) - SizeOf(UInt64)))^ :=
      {$IFDEF ENDIAN_BIG}EndianSwap{$ENDIF}(UInt64(fProcessedBytes) * 8);
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    ProcessBlock(fTransBlock^);
  end
else
  begin
    // padding and length cannot fit  
    If fBlockSize > fTransCount then
      begin
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        FillChar(Pointer(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^,fBlockSize - fTransCount,0);
        PUInt8(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^ := $80;
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        ProcessBlock(fTransBlock^);
        FillChar(fTransBlock^,fBlockSize,0);
      {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
        PUInt64(PtrUInt(fTransBlock) + (PtrUInt(fBlockSize) - SizeOf(UInt64)))^ :=
          {$IFDEF ENDIAN_BIG}EndianSwap{$ENDIF}(UInt64(fProcessedBytes) * 8);
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        ProcessBlock(fTransBlock^);        
      end
    else raise EMD4ProcessingError.CreateFmt('TMD4Hash.ProcessLast: Invalid data transfer (%d).',[fTransCount]);
  end;
end;

//------------------------------------------------------------------------------

procedure TMD4Hash.Initialize;
begin
fBlockSize := 64; // 512 bits
inherited;
fMD4 := MD4ToSys(ZeroMD4);
end;

{-------------------------------------------------------------------------------
    TMD4Hash - public methods
-------------------------------------------------------------------------------}

class Function TMD4Hash.MD4ToSys(MD4: TMD4): TMD4Sys;
var
  Temp: TMD4Sys absolute MD4;
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

class Function TMD4Hash.MD4FromSys(MD4: TMD4Sys): TMD4;
var
  Temp: TMD4Sys absolute Result;
begin
Temp := MD4;
{$IFDEF ENDIAN_BIG}
EndianSwapValue(Temp.PartA);
EndianSwapValue(Temp.PartB);
EndianSwapValue(Temp.PartC);
EndianSwapValue(Temp.PartD);
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TMD4Hash.MD4ToLE(MD4: TMD4): TMD4;
begin
Result := MD4;
end;

//------------------------------------------------------------------------------

class Function TMD4Hash.MD4ToBE(MD4: TMD4): TMD4;
begin
Result := MD4;
end;

//------------------------------------------------------------------------------

class Function TMD4Hash.MD4FromLE(MD4: TMD4): TMD4;
begin
Result := MD4;
end;

//------------------------------------------------------------------------------

class Function TMD4Hash.MD4FromBE(MD4: TMD4): TMD4;
begin
Result := MD4;
end;
 
//------------------------------------------------------------------------------

class Function TMD4Hash.HashSize: TMemSize;
begin
Result := SizeOf(TMD4);
end;

//------------------------------------------------------------------------------

class Function TMD4Hash.HashName: String;
begin
Result := 'MD4';
end;

//------------------------------------------------------------------------------

class Function TMD4Hash.HashEndianness: THashEndianness;
begin
// first byte is most significant
Result := heBig;
end;

//------------------------------------------------------------------------------

class Function TMD4Hash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TMD4Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMD4Hash then
  fMD4 := TMD4Hash(Hash).MD4Sys
else
  raise EMD4IncompatibleClass.CreateFmt('TMD4Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMD4Hash.CreateAndInitFrom(Hash: TMD4);
begin
CreateAndInit;
fMD4 := MD4ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TMD4Hash.Init;
begin
inherited;
fMD4 := MD4toSys(InitialMD4);
end;

//------------------------------------------------------------------------------

Function TMD4Hash.Compare(Hash: THashBase): Integer;
var
  A,B:  TMD4;
  i:    Integer;
begin
If Hash is TMD4Hash then
  begin
    Result := 0;
    A := MD4FromSys(fMD4);
    B := TMD4Hash(Hash).MD4;
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
  end
else raise EMD4IncompatibleClass.CreateFmt('TMD4Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMD4Hash.AsString: String;
var
  Temp: TMD4;
  i:    Integer;
begin
Result := StringOfChar('0',HashSize * 2);
Temp := MD4FromSys(fMD4);
For i := Low(Temp) to High(Temp) do
  begin
    Result[(i * 2) + 2] := IntToHex(Temp[i] and $0F,1)[1];
    Result[(i * 2) + 1] := IntToHex(Temp[i] shr 4,1)[1];
  end;
end;

//------------------------------------------------------------------------------

procedure TMD4Hash.FromString(const Str: String);
var
  TempStr:  String;
  i:        Integer;
  TempMD4:  TMD4;
begin
If Length(Str) < Integer(HashSize * 2) then
  TempStr := StringOfChar('0',Integer(HashSize * 2) - Length(Str)) + Str
else If Length(Str) > Integer(HashSize * 2) then
  TempStr := Copy(Str,Length(Str) - Pred(Integer(HashSize * 2)),Integer(HashSize * 2))
else
  TempStr := Str;
For i := Low(TempMD4) to High(TempMD4) do
  TempMD4[i] := UInt8(StrToInt('$' + Copy(TempStr,(i * 2) + 1,2)));
fMD4 := MD4ToSys(TempMD4);
end;

//------------------------------------------------------------------------------

procedure TMD4Hash.FromStringDef(const Str: String; const Default: TMD4);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMD4 := MD4ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TMD4Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMD4;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}MD4ToBE{$ELSE}MD4ToLE{$ENDIF}(MD4FromSys(fMD4));
  heLittle: Temp := MD4ToLE(MD4FromSys(fMD4));
  heBig:    Temp := MD4ToBE(MD4FromSys(fMD4));
else
 {heDefault}
  Temp := MD4FromSys(fMD4);
end;
Stream.WriteBuffer(Temp,SizeOf(TMD4));
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TMD4Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMD4;
begin
Stream.ReadBuffer(Temp,SizeOf(TMD4));
case Endianness of
  heSystem: fMD4 := MD4ToSys({$IFDEF ENDIAN_BIG}MD4FromBE{$ELSE}MD4FromLE{$ENDIF}(Temp));
  heLittle: fMD4 := MD4ToSys(MD4FromLE(Temp));
  heBig:    fMD4 := MD4ToSys(MD4FromBE(Temp));
else
 {heDefault}
  fMD4 := MD4ToSys(Temp);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
    Backward compatibility functions
===============================================================================}
{-------------------------------------------------------------------------------
    Backward compatibility functions - utility functions
-------------------------------------------------------------------------------}

Function MD4toStr(MD4: TMD4): String;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.CreateAndInitFrom(MD4);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToMD4(Str: String): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function TryStrToMD4(const Str: String; out MD4: TMD4): Boolean;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    MD4 := Hash.MD4;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToMD4Def(const Str: String; Default: TMD4): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function CompareMD4(A,B: TMD4): Integer;
var
  HashA:  TMD4Hash;
  HashB:  TMD4Hash;
begin
HashA := TMD4Hash.CreateAndInitFrom(A);
try
  HashB := TMD4Hash.CreateAndInitFrom(B);
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

Function SameMD4(A,B: TMD4): Boolean;
var
  HashA:  TMD4Hash;
  HashB:  TMD4Hash;
begin
HashA := TMD4Hash.CreateAndInitFrom(A);
try
  HashB := TMD4Hash.CreateAndInitFrom(B);
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

Function BinaryCorrectMD4(MD4: TMD4): TMD4;
begin
Result := MD4;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - processing functions
-------------------------------------------------------------------------------}

procedure BufferMD4(var MD4: TMD4; const Buffer; Size: TMemSize);
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.CreateAndInitFrom(MD4);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          MD4 := Hash.MD4;
        end
      else raise EMD4ProcessingError.CreateFmt('BufferMD4: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferMD4(MD4: TMD4; const Buffer; Size: TMemSize; MessageLength: UInt64): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.CreateAndInitFrom(MD4);
try
  Hash.ProcessedBytes := (MessageLength shr 3) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferMD4(MD4: TMD4; const Buffer; Size: TMemSize): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.CreateAndInitFrom(MD4);
try
  Hash.Final(Buffer,Size);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function BufferMD4(const Buffer; Size: TMemSize): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringMD4(const Str: AnsiString): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Hash.HashAnsiString(Str);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function WideStringMD4(const Str: WideString): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Hash.HashWideString(Str);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StringMD4(const Str: String): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Hash.HashString(Str);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function StreamMD4(Stream: TStream; Count: Int64 = -1): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMD4(const FileName: String): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - context functions
-------------------------------------------------------------------------------}

Function MD4_Init: TMD4Context;
var
  Temp: TMD4Hash;
begin
Temp := TMD4Hash.CreateAndInit;
Result := TMD4Context(Temp);
end;

//------------------------------------------------------------------------------

procedure MD4_Update(Context: TMD4Context; const Buffer; Size: TMemSize);
begin
TMD4Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function MD4_Final(var Context: TMD4Context; const Buffer; Size: TMemSize): TMD4;
begin
MD4_Update(Context,Buffer,Size);
Result := MD4_Final(Context);
end;

//------------------------------------------------------------------------------

Function MD4_Final(var Context: TMD4Context): TMD4;
begin
TMD4Hash(Context).Final;
Result := TMD4Hash(Context).MD4;
FreeAndNil(TMD4Hash(Context));
end;

//------------------------------------------------------------------------------

Function MD4_Hash(const Buffer; Size: TMemSize): TMD4;
var
  Hash: TMD4Hash;
begin
Hash := TMD4Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.MD4;
finally
  Hash.Free;
end;
end;

end.
