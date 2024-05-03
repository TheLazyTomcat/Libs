{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SHA-0 calculation

    Note that this unit is just a copy of SHA-1 implementation with slightly
    altered block calculation (SHA-1 differs from SHA-0 only in this small
    detail).

  Version 1.2.1 (2020-07-13)

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

      github.com/TheLazyTomcat/Lib.SHA0

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
unit SHA0;

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
  Bytes in type TSHA0 are always ordered from most significant byte to least
  significant byte (big endian).
  
  Type TSHA0Sys has no such guarantee and its internal structure depends on
  current implementation.

  SHA-0 does not differ in little and big endian form, as it is not a single
  quantity, therefore methods like SHA0ToLE or SHA0ToBE do nothing and are
  present only for the sake of completeness.
}
type
  TSHA0 = packed array[0..19] of UInt8;
  PSHA0 = ^TSHA0;

  TSHA0Sys = packed record
    PartA:  UInt32;
    PartB:  UInt32;
    PartC:  UInt32;
    PartD:  UInt32;
    PartE:  UInt32;
  end;
  PSHA0Sys = ^TSHA0Sys;

const
  InitialSHA0: TSHA0 = ($67,$45,$23,$01,$EF,$CD,$AB,$89,$98,$BA,
                        $DC,$FE,$10,$32,$54,$76,$C3,$D2,$E1,$F0); 
  ZeroSHA0:    TSHA0 = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
                        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

type
  ESHA0Exception = class(EHashException);

  ESHA0IncompatibleClass = class(ESHA0Exception);
  ESHA0ProcessingError   = class(ESHA0Exception);

{-------------------------------------------------------------------------------
================================================================================
                                    TSHA0Hash                                    
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA0Hash - class declaration
===============================================================================}
type
  TSHA0Hash = class(TBlockHash)
  protected
    fSHA0: TSHA0Sys;
    Function GetSHA0: TSHA0; virtual;
    procedure ProcessBlock(const Block); override;
    procedure ProcessFirst(const Block); override;
    procedure ProcessLast; override;
    procedure Initialize; override;
  public
    class Function SHA0ToSys(SHA0: TSHA0): TSHA0Sys; virtual;
    class Function SHA0FromSys(SHA0: TSHA0Sys): TSHA0; virtual;
    class Function SHA0ToLE(SHA0: TSHA0): TSHA0; virtual;
    class Function SHA0ToBE(SHA0: TSHA0): TSHA0; virtual;
    class Function SHA0FromLE(SHA0: TSHA0): TSHA0; virtual;
    class Function SHA0FromBE(SHA0: TSHA0): TSHA0; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA0); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSHA0); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property SHA0: TSHA0 read GetSHA0;
    property SHA0Sys: TSHA0Sys read fSHA0;
  end;

{===============================================================================
    Backward compatibility functions
===============================================================================}


Function SHA0toStr(SHA0: TSHA0): String;
Function StrToSHA0(Str: String): TSHA0;
Function TryStrToSHA0(const Str: String; out SHA0: TSHA0): Boolean;
Function StrToSHA0Def(const Str: String; Default: TSHA0): TSHA0;

Function CompareSHA0(A,B: TSHA0): Integer;
Function SameSHA0(A,B: TSHA0): Boolean;

Function BinaryCorrectSHA0(SHA0: TSHA0): TSHA0;

//------------------------------------------------------------------------------

procedure BufferSHA0(var SHA0: TSHA0; const Buffer; Size: TMemSize); overload;
Function LastBufferSHA0(SHA0: TSHA0; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA0; overload;
Function LastBufferSHA0(SHA0: TSHA0; const Buffer; Size: TMemSize): TSHA0; overload;

Function BufferSHA0(const Buffer; Size: TMemSize): TSHA0; overload;

Function AnsiStringSHA0(const Str: AnsiString): TSHA0;
Function WideStringSHA0(const Str: WideString): TSHA0;
Function StringSHA0(const Str: String): TSHA0;

Function StreamSHA0(Stream: TStream; Count: Int64 = -1): TSHA0;
Function FileSHA0(const FileName: String): TSHA0;

//------------------------------------------------------------------------------

type
  TSHA0Context = type Pointer;

Function SHA0_Init: TSHA0Context;
procedure SHA0_Update(Context: TSHA0Context; const Buffer; Size: TMemSize);
Function SHA0_Final(var Context: TSHA0Context; const Buffer; Size: TMemSize): TSHA0; overload;
Function SHA0_Final(var Context: TSHA0Context): TSHA0; overload;
Function SHA0_Hash(const Buffer; Size: TMemSize): TSHA0;

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
                                    TSHA0Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA0Hash - calculation constants
===============================================================================}
const
  SHA0_ROUND_CONSTS: array[0..3] of UInt32 = ($5A827999, $6ED9EBA1, $8F1BBCDC, $CA62C1D6);

{===============================================================================
    TSHA0Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA0Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA0Hash.GetSHA0: TSHA0;
begin
Result := SHA0FromSys(fSHA0);
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
procedure TSHA0Hash.ProcessBlock(const Block);
var
  Hash:           TSHA0Sys;
  i:              Integer;
  BlockWords:     packed array[0..15] of UInt32 absolute Block;
  Schedule:       array[0..79] of UInt32;
  FuncResult:     UInt32;
  RoundConstant:  UInt32;
  Temp:           UInt32;
begin
Hash := fSHA0;
// prepare state
For i := 0 to 15 do
  Schedule[i] := {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(BlockWords[i]);
For i := 16 to 79 do
  Schedule[i] := Schedule[i - 3] xor Schedule[i - 8] xor Schedule[i - 14] xor Schedule[i - 16];
// hashing rounds
For i := 0 to 79 do
  begin
    case i of
       0..19: begin
                FuncResult := (Hash.PartB and Hash.PartC) or ((not Hash.PartB) and Hash.PartD);
                RoundConstant := SHA0_ROUND_CONSTS[0];
              end;
      20..39: begin
                FuncResult := Hash.PartB xor Hash.PartC xor Hash.PartD;
                RoundConstant := SHA0_ROUND_CONSTS[1];
              end;
      40..59: begin
                FuncResult := (Hash.PartB and Hash.PartC) or (Hash.PartB and Hash.PartD) or (Hash.PartC and Hash.PartD);
                RoundConstant := SHA0_ROUND_CONSTS[2];
              end;
    else
     {60..79:}  FuncResult := Hash.PartB xor Hash.PartC xor Hash.PartD;
                RoundConstant := SHA0_ROUND_CONSTS[3];
    end;
    Temp := UInt32(ROL(Hash.PartA,5) + FuncResult + Hash.PartE + RoundConstant + Schedule[i]);
    Hash.PartE := Hash.PartD;
    Hash.PartD := Hash.PartC;
    Hash.PartC := ROL(Hash.PartB,30);
    Hash.PartB := Hash.PartA;
    Hash.PartA := Temp;
  end;
// final calculation
fSHA0.PartA := UInt32(fSHA0.PartA + Hash.PartA);
fSHA0.PartB := UInt32(fSHA0.PartB + Hash.PartB);
fSHA0.PartC := UInt32(fSHA0.PartC + Hash.PartC);
fSHA0.PartD := UInt32(fSHA0.PartD + Hash.PartD);
fSHA0.PartE := UInt32(fSHA0.PartE + Hash.PartE);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSHA0Hash.ProcessFirst(const Block);
begin
inherited;
ProcessBlock(Block);
end;

//------------------------------------------------------------------------------

procedure TSHA0Hash.ProcessLast;
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
    else raise ESHA0ProcessingError.CreateFmt('TSHA0Hash.ProcessLast: Invalid data transfer (%d).',[fTransCount]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSHA0Hash.Initialize;
begin
fBlockSize := 64; // 512 bits
inherited;
fSHA0 := SHA0ToSys(ZeroSHA0);
end;

{-------------------------------------------------------------------------------
    TSHA0Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA0Hash.SHA0ToSys(SHA0: TSHA0): TSHA0Sys;
var
  Temp: TSHA0Sys absolute SHA0;
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

class Function TSHA0Hash.SHA0FromSys(SHA0: TSHA0Sys): TSHA0;
var
  Temp: TSHA0Sys absolute Result;
begin
Temp := SHA0;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(Temp.PartA);
EndianSwapValue(Temp.PartB);
EndianSwapValue(Temp.PartC);
EndianSwapValue(Temp.PartD);
EndianSwapValue(Temp.PartE);
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA0Hash.SHA0ToLE(SHA0: TSHA0): TSHA0;
begin
Result := SHA0;
end;

//------------------------------------------------------------------------------

class Function TSHA0Hash.SHA0ToBE(SHA0: TSHA0): TSHA0;
begin
Result := SHA0;
end;

//------------------------------------------------------------------------------

class Function TSHA0Hash.SHA0FromLE(SHA0: TSHA0): TSHA0;
begin
Result := SHA0;
end;

//------------------------------------------------------------------------------

class Function TSHA0Hash.SHA0FromBE(SHA0: TSHA0): TSHA0;
begin
Result := SHA0;
end;
 
//------------------------------------------------------------------------------

class Function TSHA0Hash.HashSize: TMemSize;
begin
Result := SizeOf(TSHA0);
end;

//------------------------------------------------------------------------------

class Function TSHA0Hash.HashName: String;
begin
Result := 'SHA-0';
end;

//------------------------------------------------------------------------------

class Function TSHA0Hash.HashEndianness: THashEndianness;
begin
// first byte is most significant
Result := heBig;
end;

//------------------------------------------------------------------------------

class Function TSHA0Hash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TSHA0Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA0Hash then
  fSHA0 := TSHA0Hash(Hash).SHA0Sys
else
  raise ESHA0IncompatibleClass.CreateFmt('TSHA0Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSHA0Hash.CreateAndInitFrom(Hash: TSHA0);
begin
CreateAndInit;
fSHA0 := SHA0ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TSHA0Hash.Init;
begin
inherited;
fSHA0 := SHA0toSys(InitialSHA0);
end;

//------------------------------------------------------------------------------

Function TSHA0Hash.Compare(Hash: THashBase): Integer;
var
  A,B:  TSHA0;
  i:    Integer;
begin
If Hash is TSHA0Hash then
  begin
    Result := 0;
    A := SHA0FromSys(fSHA0);
    B := TSHA0Hash(Hash).SHA0;
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
else raise ESHA0IncompatibleClass.CreateFmt('TSHA0Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSHA0Hash.AsString: String;
var
  Temp: TSHA0;
  i:    Integer;
begin
Result := StringOfChar('0',HashSize * 2);
Temp := SHA0FromSys(fSHA0);
For i := Low(Temp) to High(Temp) do
  begin
    Result[(i * 2) + 2] := IntToHex(Temp[i] and $0F,1)[1];
    Result[(i * 2) + 1] := IntToHex(Temp[i] shr 4,1)[1];
  end;
end;

//------------------------------------------------------------------------------

procedure TSHA0Hash.FromString(const Str: String);
var
  TempStr:  String;
  i:        Integer;
  TempSHA0: TSHA0;
begin
If Length(Str) < Integer(HashSize * 2) then
  TempStr := StringOfChar('0',Integer(HashSize * 2) - Length(Str)) + Str
else If Length(Str) > Integer(HashSize * 2) then
  TempStr := Copy(Str,Length(Str) - Pred(Integer(HashSize * 2)),Integer(HashSize * 2))
else
  TempStr := Str;
For i := Low(TempSHA0) to High(TempSHA0) do
  TempSHA0[i] := UInt8(StrToInt('$' + Copy(TempStr,(i * 2) + 1,2)));
fSHA0 := SHA0ToSys(TempSHA0);
end;

//------------------------------------------------------------------------------

procedure TSHA0Hash.FromStringDef(const Str: String; const Default: TSHA0);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSHA0 := SHA0ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TSHA0Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSHA0;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}SHA0ToBE{$ELSE}SHA0ToLE{$ENDIF}(SHA0FromSys(fSHA0));
  heLittle: Temp := SHA0ToLE(SHA0FromSys(fSHA0));
  heBig:    Temp := SHA0ToBE(SHA0FromSys(fSHA0));
else
 {heDefault}
  Temp := SHA0FromSys(fSHA0);
end;
Stream.WriteBuffer(Temp,SizeOf(TSHA0));
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA0Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSHA0;
begin
Stream.ReadBuffer(Temp,SizeOf(TSHA0));
case Endianness of
  heSystem: fSHA0 := SHA0ToSys({$IFDEF ENDIAN_BIG}SHA0FromBE{$ELSE}SHA0FromLE{$ENDIF}(Temp));
  heLittle: fSHA0 := SHA0ToSys(SHA0FromLE(Temp));
  heBig:    fSHA0 := SHA0ToSys(SHA0FromBE(Temp));
else
 {heDefault}
  fSHA0 := SHA0ToSys(Temp);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
    Backward compatibility functions
===============================================================================}
{-------------------------------------------------------------------------------
    Backward compatibility functions - utility functions
-------------------------------------------------------------------------------}

Function SHA0toStr(SHA0: TSHA0): String;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.CreateAndInitFrom(SHA0);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToSHA0(Str: String): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function TryStrToSHA0(const Str: String; out SHA0: TSHA0): Boolean;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    SHA0 := Hash.SHA0;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToSHA0Def(const Str: String; Default: TSHA0): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function CompareSHA0(A,B: TSHA0): Integer;
var
  HashA:  TSHA0Hash;
  HashB:  TSHA0Hash;
begin
HashA := TSHA0Hash.CreateAndInitFrom(A);
try
  HashB := TSHA0Hash.CreateAndInitFrom(B);
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

Function SameSHA0(A,B: TSHA0): Boolean;
var
  HashA:  TSHA0Hash;
  HashB:  TSHA0Hash;
begin
HashA := TSHA0Hash.CreateAndInitFrom(A);
try
  HashB := TSHA0Hash.CreateAndInitFrom(B);
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

Function BinaryCorrectSHA0(SHA0: TSHA0): TSHA0;
begin
Result := SHA0;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - processing functions
-------------------------------------------------------------------------------}

procedure BufferSHA0(var SHA0: TSHA0; const Buffer; Size: TMemSize);
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.CreateAndInitFrom(SHA0);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          SHA0 := Hash.SHA0;
        end
      else raise ESHA0ProcessingError.CreateFmt('BufferSHA0: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferSHA0(SHA0: TSHA0; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.CreateAndInitFrom(SHA0);
try
  Hash.ProcessedBytes := (MessageLength shr 3) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferSHA0(SHA0: TSHA0; const Buffer; Size: TMemSize): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.CreateAndInitFrom(SHA0);
try
  Hash.Final(Buffer,Size);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function BufferSHA0(const Buffer; Size: TMemSize): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringSHA0(const Str: AnsiString): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Hash.HashAnsiString(Str);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function WideStringSHA0(const Str: WideString): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Hash.HashWideString(Str);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StringSHA0(const Str: String): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Hash.HashString(Str);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function StreamSHA0(Stream: TStream; Count: Int64 = -1): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSHA0(const FileName: String): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - context functions
-------------------------------------------------------------------------------}

Function SHA0_Init: TSHA0Context;
var
  Temp: TSHA0Hash;
begin
Temp := TSHA0Hash.CreateAndInit;
Result := TSHA0Context(Temp);
end;

//------------------------------------------------------------------------------

procedure SHA0_Update(Context: TSHA0Context; const Buffer; Size: TMemSize);
begin
TSHA0Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SHA0_Final(var Context: TSHA0Context; const Buffer; Size: TMemSize): TSHA0;
begin
SHA0_Update(Context,Buffer,Size);
Result := SHA0_Final(Context);
end;

//------------------------------------------------------------------------------

Function SHA0_Final(var Context: TSHA0Context): TSHA0;
begin
TSHA0Hash(Context).Final;
Result := TSHA0Hash(Context).SHA0;
FreeAndNil(TSHA0Hash(Context));
end;

//------------------------------------------------------------------------------

Function SHA0_Hash(const Buffer; Size: TMemSize): TSHA0;
var
  Hash: TSHA0Hash;
begin
Hash := TSHA0Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.SHA0;
finally
  Hash.Free;
end;
end;

end.
