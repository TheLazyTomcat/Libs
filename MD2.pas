{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  MD2 calculation

  Version 1.2.2 (2026-07-04)

  Last change 2026-07-04

  ©2015-2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.MD2

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
unit MD2;

{$IFDEF FPC}
  {$MODE ObjFPC}
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
  EMD2Exception = class(EHashException);

  EMD2IncompatibleClass = class(EMD2Exception);
  EMD2ProcessingError   = class(EMD2Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in type TMD2 are always ordered from most significant byte to least
  significant byte (big endian).

  Type TMD2Sys has no such guarantee and its internal structure depends on
  current implementation.
}
type
  TMD2 = packed array[0..15] of UInt8;
  PMD2 = ^TMD2;

  TMD2Sys = type TMD2;
  PMD2Sys = ^TMD2Sys;

const
  InitialMD2: TMD2 = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  ZeroMD2:    TMD2 = ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);

{===============================================================================
--------------------------------------------------------------------------------
                                    TMD2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMD2Hash - class declaration
===============================================================================}
type
  TMD2Hash = class(TBlockHash)
  protected
    fChecksum:  TMD2Sys;
    fMD2:       TMD2Sys;
    Function GetMD2: TMD2; virtual;
    procedure BlockChecksum(const Block); virtual;
    procedure BlockHash(const Block); virtual;
    procedure ProcessBlock(const Block); override;
    procedure ProcessFirst(const Block); override;
    procedure ProcessLast; override;
    procedure Initialize; override;
  public
    class Function MD2ToSys(MD2: TMD2): TMD2Sys; virtual;
    class Function MD2FromSys(MD2: TMD2Sys): TMD2; virtual;
    class Function MD2ToLE(MD2: TMD2): TMD2; virtual;
    class Function MD2ToBE(MD2: TMD2): TMD2; virtual;
    class Function MD2FromLE(MD2: TMD2): TMD2; virtual;
    class Function MD2FromBE(MD2: TMD2): TMD2; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TMD2); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TMD2); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property MD2: TMD2 read GetMD2;
    property MD2Sys: TMD2Sys read fMD2;
    property Checksum: TMD2Sys read fChecksum write fChecksum;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}
{
  For MD2, it is not enough to pass hash from previous step when doing
  continuous hashing (BufferMD2 > LastBufferMD2). TMD2State type is introduced
  for this purpose.
}
type
  TMD2State = record
    Checksum: TMD2Sys;
    Hash:     TMD2Sys;
  end;
  PMD2State = ^TMD2State;

const
  InitialMD2State: TMD2State = (
    Checksum: (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
    Hash:     (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0));

{===============================================================================
    Standalone functions - declaration
===============================================================================}

Function MD2toStr(const MD2: TMD2): String;
Function StrToMD2(const Str: String): TMD2;
Function TryStrToMD2(const Str: String; out MD2: TMD2): Boolean;
Function StrToMD2Def(const Str: String; Default: TMD2): TMD2;

Function CompareMD2(const A,B: TMD2): Integer;
Function SameMD2(const A,B: TMD2): Boolean;

Function BinaryCorrectMD2(const MD2: TMD2): TMD2; deprecated;

//------------------------------------------------------------------------------

procedure BufferMD2(var MD2State: TMD2State; const Buffer; Size: TMemSize); overload;
Function LastBufferMD2(const MD2State: TMD2State; const Buffer; Size: TMemSize): TMD2;

Function BufferMD2(const Buffer; Size: TMemSize): TMD2; overload;

Function AnsiStringMD2(const Str: AnsiString): TMD2;
Function WideStringMD2(const Str: WideString): TMD2;
Function StringMD2(const Str: String): TMD2;

Function StreamMD2(Stream: TStream; Count: Int64 = -1): TMD2;
Function FileMD2(const FileName: String): TMD2;

//------------------------------------------------------------------------------
type
  TMD2Context = type Pointer;

Function MD2_Init: TMD2Context;
procedure MD2_Update(Context: TMD2Context; const Buffer; Size: TMemSize);
Function MD2_Final(var Context: TMD2Context; const Buffer; Size: TMemSize): TMD2; overload;
Function MD2_Final(var Context: TMD2Context): TMD2; overload;
Function MD2_Hash(const Buffer; Size: TMemSize): TMD2;

implementation

uses
  SysUtils,
  BitOps;

{===============================================================================
    Auxiliary functions
===============================================================================}

Function MD2Compare(const A,B: TMD2Sys): Integer;
var
  i:  Integer;
begin
Result := 0;
{
  Both types TMD2 and TMD2Sys are big endian - lowest byte is the most
  significant, so start from it when comparing.
}
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

Function MD2Same(const A,B: TMD2Sys): Boolean;
var
  i:  Integer;
begin
Result := True;
// processing order does not matter here
For i := Low(A) to High(A) do
  If A[i] <> B[i] then
    begin
      Result := False;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function MD2AsString(const MD2: TMD2Sys): String;
var
  i:  Integer;
begin
Result := StringOfChar('0',SizeOf(TMD2) * 2);
For i := Low(MD2) to High(MD2) do
  begin
    Result[(i * 2) + 2] := IntToHex(MD2[i] and $0F,1)[1];
    Result[(i * 2) + 1] := IntToHex(MD2[i] shr 4,1)[1];
  end;
end;

//------------------------------------------------------------------------------

Function MD2FromString(const Str: String): TMD2Sys;
var
  TempStr:  String;
  i:        Integer;
begin
If Length(Str) < (SizeOf(TMD2) * 2) then
  TempStr := StringOfChar('0',(SizeOf(TMD2) * 2) - Length(Str)) + Str
else If Length(Str) > Integer(SizeOf(TMD2) * 2) then
  TempStr := Copy(Str,Length(Str) - Pred(SizeOf(TMD2) * 2),SizeOf(TMD2) * 2)
else
  TempStr := Str;
For i := Low(Result) to High(Result) do
  Result[i] := UInt8(StrToInt('$' + Copy(TempStr,(i * 2) + 1,2)));
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TMD2Hash
--------------------------------------------------------------------------------
===============================================================================}
const
  MD2_PI_TABLE: array[UInt8] of UInt8 =(
    $29, $2E, $43, $C9, $A2, $D8, $7C, $01, $3D, $36, $54, $A1, $EC, $F0, $06, $13,
    $62, $A7, $05, $F3, $C0, $C7, $73, $8C, $98, $93, $2B, $D9, $BC, $4C, $82, $CA,
    $1E, $9B, $57, $3C, $FD, $D4, $E0, $16, $67, $42, $6F, $18, $8A, $17, $E5, $12,
    $BE, $4E, $C4, $D6, $DA, $9E, $DE, $49, $A0, $FB, $F5, $8E, $BB, $2F, $EE, $7A,
    $A9, $68, $79, $91, $15, $B2, $07, $3F, $94, $C2, $10, $89, $0B, $22, $5F, $21,
    $80, $7F, $5D, $9A, $5A, $90, $32, $27, $35, $3E, $CC, $E7, $BF, $F7, $97, $03,
    $FF, $19, $30, $B3, $48, $A5, $B5, $D1, $D7, $5E, $92, $2A, $AC, $56, $AA, $C6,
    $4F, $B8, $38, $D2, $96, $A4, $7D, $B6, $76, $FC, $6B, $E2, $9C, $74, $04, $F1,
    $45, $9D, $70, $59, $64, $71, $87, $20, $86, $5B, $CF, $65, $E6, $2D, $A8, $02,
    $1B, $60, $25, $AD, $AE, $B0, $B9, $F6, $1C, $46, $61, $69, $34, $40, $7E, $0F,
    $55, $47, $A3, $23, $DD, $51, $AF, $3A, $C3, $5C, $F9, $CE, $BA, $C5, $EA, $26,
    $2C, $53, $0D, $6E, $85, $28, $84, $09, $D3, $DF, $CD, $F4, $41, $81, $4D, $52,
    $6A, $DC, $37, $C8, $6C, $C1, $AB, $FA, $24, $E1, $7B, $08, $0C, $BD, $B1, $4A,
    $78, $88, $95, $8B, $E3, $63, $E8, $6D, $E9, $CB, $D5, $FE, $3B, $00, $1D, $39,
    $F2, $EF, $B7, $0E, $66, $58, $D0, $E4, $A6, $77, $72, $F8, $EB, $75, $4B, $0A,
    $31, $44, $50, $B4, $8F, $ED, $1F, $1A, $DB, $99, $8D, $33, $9F, $11, $83, $14);

{===============================================================================
    TMD2Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMD2Hash - protected methods
-------------------------------------------------------------------------------}

Function TMD2Hash.GetMD2: TMD2;
begin
Result := MD2FromSys(fMD2);
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.BlockChecksum(const Block);
var
  Temp:     UInt8;
  i:        Integer;
  BlockArr: TMD2Sys absolute Block;
begin
Temp := fChecksum[High(fChecksum)];
For i := Low(fChecksum) to High(fChecksum) do
  begin
    fChecksum[i] := fChecksum[i] xor MD2_PI_TABLE[BlockArr[i] xor Temp];
    Temp := fChecksum[i];
  end;
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.BlockHash(const Block);
var
  i,j:      Integer;
  PiIndex:  UInt8;
  State:    array[0..47] of UInt8;  // 3 * Length(TMD2)
  BlockArr: TMD2Sys absolute Block;
begin
PMD2Sys(@State)^ := fMD2;
For i := Low(TMD2Sys) to High(TMD2Sys) do
  begin
    State[Length(BlockArr) + i] := BlockArr[i];
    State[(Length(BlockArr) * 2) + i] := BlockArr[i] xor State[i];
  end;
PiIndex := 0;
For i := 0 to 17 do
  begin
    For j := Low(State) to High(State) do
      begin
        State[j] := State[j] xor MD2_PI_TABLE[PiIndex];
        PiIndex := State[j];
      end;
    PiIndex := UInt8(Int32(PiIndex) + i);
  end;
fMD2 := PMD2Sys(@State)^;
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.ProcessBlock(const Block);
begin
BlockChecksum(Block);
BlockHash(Block);
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.ProcessFirst(const Block);
begin
inherited;
ProcessBlock(Block);
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.ProcessLast;
begin
FillChar(PtrAdvance(fTransBlock,TMemOff(fTransCount))^,fBlockSize - fTransCount,UInt8(fBlockSize - fTransCount));
ProcessBlock(fTransBlock^);
BlockHash(fChecksum);
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.Initialize;
begin
fBlockSize := 16; // 128 bits
inherited;
fCheckSum := MD2ToSys(ZeroMD2);
fMD2 := MD2ToSys(ZeroMD2);
end;

{-------------------------------------------------------------------------------
    TMD2Hash - public methods
-------------------------------------------------------------------------------}

class Function TMD2Hash.MD2ToSys(MD2: TMD2): TMD2Sys;
var
  Temp: TMD2Sys absolute MD2;
begin
Result := Temp;
end;

//------------------------------------------------------------------------------

class Function TMD2Hash.MD2FromSys(MD2: TMD2Sys): TMD2;
var
  Temp: TMD2Sys absolute Result;
begin
Temp := MD2;
end;

//------------------------------------------------------------------------------

class Function TMD2Hash.MD2ToLE(MD2: TMD2): TMD2;
begin
Result := MD2;
SwapEndian(Result,SizeOf(TMD2));
end;

//------------------------------------------------------------------------------

class Function TMD2Hash.MD2ToBE(MD2: TMD2): TMD2;
begin
Result := MD2;
end;

//------------------------------------------------------------------------------

class Function TMD2Hash.MD2FromLE(MD2: TMD2): TMD2;
begin
Result := MD2;
SwapEndian(Result,SizeOf(TMD2));
end;

//------------------------------------------------------------------------------

class Function TMD2Hash.MD2FromBE(MD2: TMD2): TMD2;
begin
Result := MD2;
end;
 
//------------------------------------------------------------------------------

class Function TMD2Hash.HashSize: TMemSize;
begin
Result := SizeOf(TMD2);
end;

//------------------------------------------------------------------------------

class Function TMD2Hash.HashName: String;
begin
Result := 'MD2';
end;

//------------------------------------------------------------------------------

class Function TMD2Hash.HashEndianness: THashEndianness;
begin
// first byte is most significant
Result := heBig;
end;

//------------------------------------------------------------------------------

class Function TMD2Hash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TMD2Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TMD2Hash then
  begin
    fMD2 := TMD2Hash(Hash).MD2Sys;
    fChecksum := TMD2Hash(Hash).Checksum;
  end
else raise EMD2IncompatibleClass.CreateFmt('TMD2Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TMD2Hash.CreateAndInitFrom(Hash: TMD2);
begin
CreateAndInit;
fMD2 := MD2ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.Init;
begin
inherited;
fCheckSum := MD2ToSys(ZeroMD2);
fMD2 := MD2toSys(InitialMD2);
end;

//------------------------------------------------------------------------------

Function TMD2Hash.Compare(Hash: THashBase): Integer;
begin
If Hash is TMD2Hash then
  Result := MD2Compare(fMD2,TMD2Hash(Hash).MD2Sys)
else
  raise EMD2IncompatibleClass.CreateFmt('TMD2Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMD2Hash.Same(Hash: THashBase): Boolean;
begin
If Hash is TMD2Hash then
  Result := MD2Same(fMD2,TMD2Hash(Hash).MD2Sys)
else
  raise EMD2IncompatibleClass.CreateFmt('TMD2Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TMD2Hash.AsString: String;
begin
Result := MD2AsString(fMD2);
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.FromString(const Str: String);
begin
fMD2 := MD2FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.FromStringDef(const Str: String; const Default: TMD2);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fMD2 := MD2ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMD2;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}MD2ToBE{$ELSE}MD2ToLE{$ENDIF}(MD2FromSys(fMD2));
  heLittle: Temp := MD2ToLE(MD2FromSys(fMD2));
  heBig:    Temp := MD2ToBE(MD2FromSys(fMD2));
else
 {heDefault}
  Temp := MD2FromSys(fMD2);
end;
Stream.WriteBuffer(Temp,SizeOf(TMD2));
end;

//------------------------------------------------------------------------------

procedure TMD2Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TMD2;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(TMD2));
case Endianness of
  heSystem: fMD2 := MD2ToSys({$IFDEF ENDIAN_BIG}MD2FromBE{$ELSE}MD2FromLE{$ENDIF}(Temp));
  heLittle: fMD2 := MD2ToSys(MD2FromLE(Temp));
  heBig:    fMD2 := MD2ToSys(MD2FromBE(Temp));
else
 {heDefault}
  fMD2 := MD2ToSys(Temp);
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

Function MD2toStr(const MD2: TMD2): String;
begin
Result := MD2AsString(TMD2Hash.MD2ToSys(MD2));
end;

//------------------------------------------------------------------------------

Function StrToMD2(const Str: String): TMD2;
begin
Result := TMD2Hash.MD2FromSys(MD2FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToMD2(const Str: String; out MD2: TMD2): Boolean;
begin
try
  MD2 := TMD2Hash.MD2FromSys(MD2FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToMD2Def(const Str: String; Default: TMD2): TMD2;
begin
If not TryStrToMD2(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareMD2(const A,B: TMD2): Integer;
begin
Result := MD2Compare(TMD2Hash.MD2ToSys(A),TMD2Hash.MD2ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameMD2(const A,B: TMD2): Boolean;
begin
Result := MD2Same(TMD2Hash.MD2ToSys(A),TMD2Hash.MD2ToSys(B));
end;

//------------------------------------------------------------------------------

Function BinaryCorrectMD2(const MD2: TMD2): TMD2;
begin
Result := MD2;
end;

{-------------------------------------------------------------------------------
    Standalone functions - processing functions
-------------------------------------------------------------------------------}

procedure BufferMD2(var MD2State: TMD2State; const Buffer; Size: TMemSize);
var
  Hash: TMD2Hash;
begin
Hash := TMD2Hash.CreateAndInitFrom(TMD2Hash.MD2FromSys(MD2State.Hash));
try
  Hash.Checksum := MD2State.Checksum;
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          MD2State.Checksum := Hash.Checksum;
          MD2State.Hash := Hash.MD2Sys;
        end
      else raise EMD2ProcessingError.CreateFmt('BufferMD2: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferMD2(const MD2State: TMD2State; const Buffer; Size: TMemSize): TMD2;
var
  Hash: TMD2Hash;
begin
Hash := TMD2Hash.CreateAndInitFrom(TMD2Hash.MD2FromSys(MD2State.Hash));
try
  Hash.Checksum := MD2State.Checksum;
  Hash.Final(Buffer,Size);
  Result := Hash.MD2;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function BufferMD2(const Buffer; Size: TMemSize): TMD2;
var
  Hash: TMD2Hash;
begin
Hash := TMD2Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.MD2;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringMD2(const Str: AnsiString): TMD2;
var
  Hash: TMD2Hash;
begin
Hash := TMD2Hash.Create;
try
  Hash.HashAnsiString(Str);
  Result := Hash.MD2;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function WideStringMD2(const Str: WideString): TMD2;
var
  Hash: TMD2Hash;
begin
Hash := TMD2Hash.Create;
try
  Hash.HashWideString(Str);
  Result := Hash.MD2;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StringMD2(const Str: String): TMD2;
var
  Hash: TMD2Hash;
begin
Hash := TMD2Hash.Create;
try
  Hash.HashString(Str);
  Result := Hash.MD2;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function StreamMD2(Stream: TStream; Count: Int64 = -1): TMD2;
var
  Hash: TMD2Hash;
begin
Hash := TMD2Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.MD2;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileMD2(const FileName: String): TMD2;
var
  Hash: TMD2Hash;
begin
Hash := TMD2Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.MD2;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Standalone functions - context functions
-------------------------------------------------------------------------------}

Function MD2_Init: TMD2Context;
var
  Temp: TMD2Hash;
begin
Temp := TMD2Hash.CreateAndInit;
Result := TMD2Context(Temp);
end;

//------------------------------------------------------------------------------

procedure MD2_Update(Context: TMD2Context; const Buffer; Size: TMemSize);
begin
TMD2Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function MD2_Final(var Context: TMD2Context; const Buffer; Size: TMemSize): TMD2;
begin
MD2_Update(Context,Buffer,Size);
Result := MD2_Final(Context);
end;

//------------------------------------------------------------------------------

Function MD2_Final(var Context: TMD2Context): TMD2;
begin
TMD2Hash(Context).Final;
Result := TMD2Hash(Context).MD2;
FreeAndNil(TMD2Hash(Context));
end;

//------------------------------------------------------------------------------

Function MD2_Hash(const Buffer; Size: TMemSize): TMD2;
var
  Hash: TMD2Hash;
begin
Hash := TMD2Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.MD2;
finally
  Hash.Free;
end;
end;

end.
