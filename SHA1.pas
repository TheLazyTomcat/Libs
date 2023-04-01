{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SHA-1 calculation

  Version 1.2.1 (2020-07-13)

  Last change 2023-03-27

  ©2015-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SHA1

  Dependencies:
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID

  SimpleCPUID might not be needed, see BitOps library for details.

===============================================================================}
unit SHA1;

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
  Bytes in type TSHA1 are always ordered from most significant byte to least
  significant byte (big endian).
  
  Type TSHA1Sys has no such guarantee and its internal structure depends on
  current implementation.

  SHA-1 does not differ in little and big endian form, as it is not a single
  quantity, therefore methods like SHA1ToLE or SHA1ToBE do nothing and are
  present only for the sake of completeness.
}
type
  TSHA1 = packed array[0..19] of UInt8;
  PSHA1 = ^TSHA1;

  TSHA1Sys = packed record
    PartA:  UInt32;
    PartB:  UInt32;
    PartC:  UInt32;
    PartD:  UInt32;
    PartE:  UInt32;
  end;
  PSHA1Sys = ^TSHA1Sys;

const
  InitialSHA1: TSHA1 = ($67,$45,$23,$01,$EF,$CD,$AB,$89,$98,$BA,
                        $DC,$FE,$10,$32,$54,$76,$C3,$D2,$E1,$F0); 
  ZeroSHA1:    TSHA1 = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

type
  ESHA1Exception = class(EHashException);

  ESHA1IncompatibleClass = class(ESHA1Exception);
  ESHA1ProcessingError   = class(ESHA1Exception);

{-------------------------------------------------------------------------------
================================================================================
                                    TSHA1Hash                                    
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA1Hash - class declaration
===============================================================================}
type
  TSHA1Hash = class(TBlockHash)
  protected
    fSHA1: TSHA1Sys;
    Function GetSHA1: TSHA1; virtual;
    procedure ProcessBlock(const Block); override;
    procedure ProcessFirst(const Block); override;
    procedure ProcessLast; override;
    procedure Initialize; override;
  public
    class Function SHA1ToSys(SHA1: TSHA1): TSHA1Sys; virtual;
    class Function SHA1FromSys(SHA1: TSHA1Sys): TSHA1; virtual;
    class Function SHA1ToLE(SHA1: TSHA1): TSHA1; virtual;
    class Function SHA1ToBE(SHA1: TSHA1): TSHA1; virtual;
    class Function SHA1FromLE(SHA1: TSHA1): TSHA1; virtual;
    class Function SHA1FromBE(SHA1: TSHA1): TSHA1; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA1); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSHA1); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property SHA1: TSHA1 read GetSHA1;
    property SHA1Sys: TSHA1Sys read fSHA1;
  end;

{===============================================================================
    Backward compatibility functions
===============================================================================}


Function SHA1toStr(SHA1: TSHA1): String;
Function StrToSHA1(Str: String): TSHA1;
Function TryStrToSHA1(const Str: String; out SHA1: TSHA1): Boolean;
Function StrToSHA1Def(const Str: String; Default: TSHA1): TSHA1;

Function CompareSHA1(A,B: TSHA1): Integer;
Function SameSHA1(A,B: TSHA1): Boolean;

Function BinaryCorrectSHA1(SHA1: TSHA1): TSHA1;

//------------------------------------------------------------------------------

procedure BufferSHA1(var SHA1: TSHA1; const Buffer; Size: TMemSize); overload;
Function LastBufferSHA1(SHA1: TSHA1; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA1; overload;
Function LastBufferSHA1(SHA1: TSHA1; const Buffer; Size: TMemSize): TSHA1; overload;

Function BufferSHA1(const Buffer; Size: TMemSize): TSHA1; overload;

Function AnsiStringSHA1(const Str: AnsiString): TSHA1;
Function WideStringSHA1(const Str: WideString): TSHA1;
Function StringSHA1(const Str: String): TSHA1;

Function StreamSHA1(Stream: TStream; Count: Int64 = -1): TSHA1;
Function FileSHA1(const FileName: String): TSHA1;

//------------------------------------------------------------------------------

type
  TSHA1Context = type Pointer;

Function SHA1_Init: TSHA1Context;
procedure SHA1_Update(Context: TSHA1Context; const Buffer; Size: TMemSize);
Function SHA1_Final(var Context: TSHA1Context; const Buffer; Size: TMemSize): TSHA1; overload;
Function SHA1_Final(var Context: TSHA1Context): TSHA1; overload;
Function SHA1_Hash(const Buffer; Size: TMemSize): TSHA1;

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
                                    TSHA1Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA1Hash - calculation constants
===============================================================================}
const
  SHA1_ROUND_CONSTS: array[0..3] of UInt32 = ($5A827999, $6ED9EBA1, $8F1BBCDC, $CA62C1D6);

{===============================================================================
    TSHA1Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA1Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA1Hash.GetSHA1: TSHA1;
begin
Result := SHA1FromSys(fSHA1);
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
procedure TSHA1Hash.ProcessBlock(const Block);
var
  Hash:           TSHA1Sys;
  i:              Integer;
  BlockWords:     packed array[0..15] of UInt32 absolute Block;
  Schedule:       array[0..79] of UInt32;
  FuncResult:     UInt32;
  RoundConstant:  UInt32;
  Temp:           UInt32;
begin
Hash := fSHA1;
// prepare state
For i := 0 to 15 do
  Schedule[i] := {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(BlockWords[i]);
For i := 16 to 79 do
  Schedule[i] := ROL(Schedule[i - 3] xor Schedule[i - 8] xor Schedule[i - 14] xor Schedule[i - 16],1);
// hashing rounds
For i := 0 to 79 do
  begin
    case i of
       0..19: begin
                FuncResult := (Hash.PartB and Hash.PartC) or ((not Hash.PartB) and Hash.PartD);
                RoundConstant := SHA1_ROUND_CONSTS[0];
              end;
      20..39: begin
                FuncResult := Hash.PartB xor Hash.PartC xor Hash.PartD;
                RoundConstant := SHA1_ROUND_CONSTS[1];
              end;
      40..59: begin
                FuncResult := (Hash.PartB and Hash.PartC) or (Hash.PartB and Hash.PartD) or (Hash.PartC and Hash.PartD);
                RoundConstant := SHA1_ROUND_CONSTS[2];
              end;
    else
     {60..79:}  FuncResult := Hash.PartB xor Hash.PartC xor Hash.PartD;
                RoundConstant := SHA1_ROUND_CONSTS[3];
    end;
    Temp := UInt32(ROL(Hash.PartA,5) + FuncResult + Hash.PartE + RoundConstant + Schedule[i]);
    Hash.PartE := Hash.PartD;
    Hash.PartD := Hash.PartC;
    Hash.PartC := ROL(Hash.PartB,30);
    Hash.PartB := Hash.PartA;
    Hash.PartA := Temp;
  end;
// final calculation
fSHA1.PartA := UInt32(fSHA1.PartA + Hash.PartA);
fSHA1.PartB := UInt32(fSHA1.PartB + Hash.PartB);
fSHA1.PartC := UInt32(fSHA1.PartC + Hash.PartC);
fSHA1.PartD := UInt32(fSHA1.PartD + Hash.PartD);
fSHA1.PartE := UInt32(fSHA1.PartE + Hash.PartE);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSHA1Hash.ProcessFirst(const Block);
begin
inherited;
ProcessBlock(Block);
end;

//------------------------------------------------------------------------------

procedure TSHA1Hash.ProcessLast;
begin
If (fBlockSize - fTransCount) >= (SizeOf(UInt64) + 1) then
  begin
    // padding and length can fit
  {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
    FillChar(Pointer(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^,fBlockSize - fTransCount,0);
    PUInt8(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^ := $80;
    PUInt64(PtrUInt(fTransBlock) + (PtrUInt(fBlockSize) - SizeOf(UInt64)))^ :=
      {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(UInt64(fProcessedBytes) * 8);
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
          {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(UInt64(fProcessedBytes) * 8);
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        ProcessBlock(fTransBlock^);        
      end
    else raise ESHA1ProcessingError.CreateFmt('TSHA1Hash.ProcessLast: Invalid data transfer (%d).',[fTransCount]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSHA1Hash.Initialize;
begin
fBlockSize := 64; // 512 bits
inherited;
fSHA1 := SHA1ToSys(ZeroSHA1);
end;

{-------------------------------------------------------------------------------
    TSHA1Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA1Hash.SHA1ToSys(SHA1: TSHA1): TSHA1Sys;
var
  Temp: TSHA1Sys absolute SHA1;
begin
Result := Temp;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(Result.PartA);
EndianSwapValue(Result.PartB);
EndianSwapValue(Result.PartC);
EndianSwapValue(Result.PartD);
EndianSwapValue(Result.PartE);
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA1Hash.SHA1FromSys(SHA1: TSHA1Sys): TSHA1;
var
  Temp: TSHA1Sys absolute Result;
begin
Temp := SHA1;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(Temp.PartA);
EndianSwapValue(Temp.PartB);
EndianSwapValue(Temp.PartC);
EndianSwapValue(Temp.PartD);
EndianSwapValue(Temp.PartE);
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA1Hash.SHA1ToLE(SHA1: TSHA1): TSHA1;
begin
Result := SHA1;
end;

//------------------------------------------------------------------------------

class Function TSHA1Hash.SHA1ToBE(SHA1: TSHA1): TSHA1;
begin
Result := SHA1;
end;

//------------------------------------------------------------------------------

class Function TSHA1Hash.SHA1FromLE(SHA1: TSHA1): TSHA1;
begin
Result := SHA1;
end;

//------------------------------------------------------------------------------

class Function TSHA1Hash.SHA1FromBE(SHA1: TSHA1): TSHA1;
begin
Result := SHA1;
end;
 
//------------------------------------------------------------------------------

class Function TSHA1Hash.HashSize: TMemSize;
begin
Result := SizeOf(TSHA1);
end;

//------------------------------------------------------------------------------

class Function TSHA1Hash.HashName: String;
begin
Result := 'SHA-1';
end;

//------------------------------------------------------------------------------

class Function TSHA1Hash.HashEndianness: THashEndianness;
begin
// first byte is most significant
Result := heBig;
end;

//------------------------------------------------------------------------------

class Function TSHA1Hash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TSHA1Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA1Hash then
  fSHA1 := TSHA1Hash(Hash).SHA1Sys
else
  raise ESHA1IncompatibleClass.CreateFmt('TSHA1Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSHA1Hash.CreateAndInitFrom(Hash: TSHA1);
begin
CreateAndInit;
fSHA1 := SHA1ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TSHA1Hash.Init;
begin
inherited;
fSHA1 := SHA1toSys(InitialSHA1);
end;

//------------------------------------------------------------------------------

Function TSHA1Hash.Compare(Hash: THashBase): Integer;
var
  A,B:  TSHA1;
  i:    Integer;
begin
If Hash is TSHA1Hash then
  begin
    Result := 0;
    A := SHA1FromSys(fSHA1);
    B := TSHA1Hash(Hash).SHA1;
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
else raise ESHA1IncompatibleClass.CreateFmt('TSHA1Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSHA1Hash.AsString: String;
var
  Temp: TSHA1;
  i:    Integer;
begin
Result := StringOfChar('0',HashSize * 2);
Temp := SHA1FromSys(fSHA1);
For i := Low(Temp) to High(Temp) do
  begin
    Result[(i * 2) + 2] := IntToHex(Temp[i] and $0F,1)[1];
    Result[(i * 2) + 1] := IntToHex(Temp[i] shr 4,1)[1];
  end;
end;

//------------------------------------------------------------------------------

procedure TSHA1Hash.FromString(const Str: String);
var
  TempStr:  String;
  i:        Integer;
  TempSHA1: TSHA1;
begin
If Length(Str) < Integer(HashSize * 2) then
  TempStr := StringOfChar('0',Integer(HashSize * 2) - Length(Str)) + Str
else If Length(Str) > Integer(HashSize * 2) then
  TempStr := Copy(Str,Length(Str) - Pred(Integer(HashSize * 2)),Integer(HashSize * 2))
else
  TempStr := Str;
For i := Low(TempSHA1) to High(TempSHA1) do
  TempSHA1[i] := UInt8(StrToInt('$' + Copy(TempStr,(i * 2) + 1,2)));
fSHA1 := SHA1ToSys(TempSHA1);
end;

//------------------------------------------------------------------------------

procedure TSHA1Hash.FromStringDef(const Str: String; const Default: TSHA1);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSHA1 := SHA1ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TSHA1Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSHA1;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}SHA1ToBE{$ELSE}SHA1ToLE{$ENDIF}(SHA1FromSys(fSHA1));
  heLittle: Temp := SHA1ToLE(SHA1FromSys(fSHA1));
  heBig:    Temp := SHA1ToBE(SHA1FromSys(fSHA1));
else
 {heDefault}
  Temp := SHA1FromSys(fSHA1);
end;
Stream.WriteBuffer(Temp,SizeOf(TSHA1));
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA1Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSHA1;
begin
Stream.ReadBuffer(Temp,SizeOf(TSHA1));
case Endianness of
  heSystem: fSHA1 := SHA1ToSys({$IFDEF ENDIAN_BIG}SHA1FromBE{$ELSE}SHA1FromLE{$ENDIF}(Temp));
  heLittle: fSHA1 := SHA1ToSys(SHA1FromLE(Temp));
  heBig:    fSHA1 := SHA1ToSys(SHA1FromBE(Temp));
else
 {heDefault}
  fSHA1 := SHA1ToSys(Temp);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
    Backward compatibility functions
===============================================================================}
{-------------------------------------------------------------------------------
    Backward compatibility functions - utility functions
-------------------------------------------------------------------------------}

Function SHA1toStr(SHA1: TSHA1): String;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.CreateAndInitFrom(SHA1);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToSHA1(Str: String): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function TryStrToSHA1(const Str: String; out SHA1: TSHA1): Boolean;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    SHA1 := Hash.SHA1;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToSHA1Def(const Str: String; Default: TSHA1): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function CompareSHA1(A,B: TSHA1): Integer;
var
  HashA:  TSHA1Hash;
  HashB:  TSHA1Hash;
begin
HashA := TSHA1Hash.CreateAndInitFrom(A);
try
  HashB := TSHA1Hash.CreateAndInitFrom(B);
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

Function SameSHA1(A,B: TSHA1): Boolean;
var
  HashA:  TSHA1Hash;
  HashB:  TSHA1Hash;
begin
HashA := TSHA1Hash.CreateAndInitFrom(A);
try
  HashB := TSHA1Hash.CreateAndInitFrom(B);
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

Function BinaryCorrectSHA1(SHA1: TSHA1): TSHA1;
begin
Result := SHA1;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - processing functions
-------------------------------------------------------------------------------}

procedure BufferSHA1(var SHA1: TSHA1; const Buffer; Size: TMemSize);
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.CreateAndInitFrom(SHA1);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          SHA1 := Hash.SHA1;
        end
      else raise ESHA1ProcessingError.CreateFmt('BufferSHA1: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferSHA1(SHA1: TSHA1; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.CreateAndInitFrom(SHA1);
try
  Hash.ProcessedBytes := (MessageLength shr 3) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferSHA1(SHA1: TSHA1; const Buffer; Size: TMemSize): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.CreateAndInitFrom(SHA1);
try
  Hash.Final(Buffer,Size);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function BufferSHA1(const Buffer; Size: TMemSize): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringSHA1(const Str: AnsiString): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Hash.HashAnsiString(Str);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function WideStringSHA1(const Str: WideString): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Hash.HashWideString(Str);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StringSHA1(const Str: String): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Hash.HashString(Str);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function StreamSHA1(Stream: TStream; Count: Int64 = -1): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSHA1(const FileName: String): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - context functions
-------------------------------------------------------------------------------}

Function SHA1_Init: TSHA1Context;
var
  Temp: TSHA1Hash;
begin
Temp := TSHA1Hash.CreateAndInit;
Result := TSHA1Context(Temp);
end;

//------------------------------------------------------------------------------

procedure SHA1_Update(Context: TSHA1Context; const Buffer; Size: TMemSize);
begin
TSHA1Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SHA1_Final(var Context: TSHA1Context; const Buffer; Size: TMemSize): TSHA1;
begin
SHA1_Update(Context,Buffer,Size);
Result := SHA1_Final(Context);
end;

//------------------------------------------------------------------------------

Function SHA1_Final(var Context: TSHA1Context): TSHA1;
begin
TSHA1Hash(Context).Final;
Result := TSHA1Hash(Context).SHA1;
FreeAndNil(TSHA1Hash(Context));
end;

//------------------------------------------------------------------------------

Function SHA1_Hash(const Buffer; Size: TMemSize): TSHA1;
var
  Hash: TSHA1Hash;
begin
Hash := TSHA1Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.SHA1;
finally
  Hash.Free;
end;
end;

end.
