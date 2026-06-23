{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CRC-Lite

    This simple library is intended as a light-weight alternative to my other,
    full-blown, cyclic redundancy check (CRC) and checksum libraries.

    It provides only few selected versions of CRCs and one checksum (Adler-32)
    and only in simple procedural interface (to reduce overhead given by object
    allocation that is present in mentioned full libraries).

  Version 1.0.1 (2026-06-22)

  Last change 2026-06-22

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.CRC32Lite

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes

===============================================================================}
unit CRCLite;
{
  CRC32Lite_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$IFDEF CRC32Lite_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

//------------------------------------------------------------------------------

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
{$ENDIF}
{$H+}

interface

uses
  AuxTypes;

{
  All values returned as CRC or checksum using following types have system
  endianness (byte order).
  This is important to remember when working with types declared as arrays
  (currently only UInt24/TCRC24), as they are not a single quantity and
  their ordering must be explicitly observed (eg. when converting to string
  representation).
}
type
  UInt24 = packed array[0..2] of UInt8; // yeah...

  TAdler32 = UInt32;    PAdler32 = ^TAdler32;
  TCRC8    = UInt8;     PCRC8    = ^TCRC8;
  TCRC16   = UInt16;    PCRC16   = ^TCRC16;
  TCRC24   = UInt24;    PCRC24   = ^TCRC24;
  TCRC32   = UInt32;    PCRC32   = ^TCRC32;
  TCRC64   = UInt64;    PCRC64   = ^TCRC64;

{===============================================================================
--------------------------------------------------------------------------------
                                    Adler-32
--------------------------------------------------------------------------------
===============================================================================}
{
  Note that Adler-32 is a checksum, not CRC, but I am still putting it here in
  case someone needs it.
}
const
  InitialAdler32: TAdler32 = $00000001;
  ZeroAdler32:    TAdler32 = 0;

Function BufferAdler32(Adler32: TAdler32; const Buffer; Size: TMemSize): TAdler32; overload;
Function BufferAdler32(const Buffer; Size: TMemSize): TAdler32; overload;

Function SameAdler32(A,B: TAdler32): Boolean;

//------------------------------------------------------------------------------
type
  TAdler32Context = type TAdler32;

Function Adler32_Init: TAdler32Context;
procedure Adler32_Update(var Context: TAdler32Context; const Buffer; Size: TMemSize);
Function Adler32_Final(var Context: TAdler32Context; const Buffer; Size: TMemSize): TAdler32; overload;
Function Adler32_Final(var Context: TAdler32Context): TAdler32; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                      CRC-8
--------------------------------------------------------------------------------
===============================================================================}
{
                  polynomial        0x107
               initial value        0x00000000
             final xor value        0x00000000
       input bits reflection        False
      output bits reflection        False

  Given polynomial is in hexadecimal notation, full (includes highest 1 bit)
  and unreflected (most-significant bit to the left).

  Other values are always given in hexadecimal, most-significant byte first
  (to the left).

  Input bits reflection has no effect in the actual implementation - it affects
  only values in used look-up tables, which are precalculated.

  Output reflection affects order of bits within the returned quantity with
  respect to computation internals. This has no deeper meaning and changes
  only how the value is presented (eg. when converted to hex string).
}
//------------------------------------------------------------------------------
const
  InitialCRC8: TCRC8 = $00;
  ZeroCRC8:    TCRC8 = 0;

Function BufferCRC8(CRC8: TCRC8; const Buffer; Size: TMemSize): TCRC8; overload;
Function BufferCRC8(const Buffer; Size: TMemSize): TCRC8; overload;

Function SameCRC8(A,B: TCRC8): Boolean;

//------------------------------------------------------------------------------
type
  TCRC8Context = type TCRC8;

Function CRC8_Init: TCRC8Context;
procedure CRC8_Update(var Context: TCRC8Context; const Buffer; Size: TMemSize);
Function CRC8_Final(var Context: TCRC8Context; const Buffer; Size: TMemSize): TCRC8; overload;
Function CRC8_Final(var Context: TCRC8Context): TCRC8; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-16
--------------------------------------------------------------------------------
===============================================================================}
{
                  polynomial        0x18005
               initial value        0x00000000
             final xor value        0x00000000
       input bits reflection        True
      output bits reflection        True
}
//------------------------------------------------------------------------------
const
  InitialCRC16: TCRC16 = $0000;
  ZeroCRC16:    TCRC16 = 0;

Function BufferCRC16(CRC16: TCRC16; const Buffer; Size: TMemSize): TCRC16; overload;
Function BufferCRC16(const Buffer; Size: TMemSize): TCRC16; overload;

Function SameCRC16(A,B: TCRC16): Boolean;

//------------------------------------------------------------------------------
type
  TCRC16Context = type TCRC16;

Function CRC16_Init: TCRC16Context;
procedure CRC16_Update(var Context: TCRC16Context; const Buffer; Size: TMemSize);
Function CRC16_Final(var Context: TCRC16Context; const Buffer; Size: TMemSize): TCRC16; overload;
Function CRC16_Final(var Context: TCRC16Context): TCRC16; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-24
--------------------------------------------------------------------------------
===============================================================================}
{
                  polynomial        0x1864CFB
               initial value        0xB704CE
             final xor value        0x000000
       input bits reflection        False
      output bits reflection        False
}
//------------------------------------------------------------------------------
const
{
  Initial value is in reflected byte order - this is due to how the current
  implementation works.
}
  InitialCRC24: TCRC24 = ($CE,$04,$B7);
  ZeroCRC24:    TCRC24 = (0,0,0);

Function BufferCRC24(CRC24: TCRC24; const Buffer; Size: TMemSize): TCRC24; overload;
Function BufferCRC24(const Buffer; Size: TMemSize): TCRC24; overload;

Function SameCRC24(A,B: TCRC24): Boolean;

//------------------------------------------------------------------------------
type
  TCRC24Context = type TCRC24;

Function CRC24_Init: TCRC24Context;
procedure CRC24_Update(var Context: TCRC24Context; const Buffer; Size: TMemSize);
Function CRC24_Final(var Context: TCRC24Context; const Buffer; Size: TMemSize): TCRC24; overload;
Function CRC24_Final(var Context: TCRC24Context): TCRC24; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-32
--------------------------------------------------------------------------------
===============================================================================}
{
                  polynomial        0x104C11DB7
               initial value        0xFFFFFFFF
             final xor value        0xFFFFFFFF
       input bits reflection        True
      output bits reflection        True
}
//------------------------------------------------------------------------------
const
{
  Following is not a mistake, I know init is 0xFFFFFFFF in specification.
  Technically speaking, it is initial value xor-ed with final xor value
  (which is also 0xFFFFFFFF).
}
  InitialCRC32: TCRC32 = $00000000;
  ZeroCRC32:    TCRC32 = 0;

Function BufferCRC32(CRC32: TCRC32; const Buffer; Size: TMemSize): TCRC32; overload;
Function BufferCRC32(const Buffer; Size: TMemSize): TCRC32; overload;

Function SameCRC32(A,B: TCRC32): Boolean;

//------------------------------------------------------------------------------
type
  TCRC32Context = type TCRC32;

Function CRC32_Init: TCRC32Context;
procedure CRC32_Update(var Context: TCRC32Context; const Buffer; Size: TMemSize);
Function CRC32_Final(var Context: TCRC32Context; const Buffer; Size: TMemSize): TCRC32; overload;
Function CRC32_Final(var Context: TCRC32Context): TCRC32; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-32C
--------------------------------------------------------------------------------
===============================================================================}
{
                  polynomial        0x11EDC6F41
               initial value        0xFFFFFFFF
             final xor value        0xFFFFFFFF
       input bits reflection        True
      output bits reflection        True
}
//------------------------------------------------------------------------------

Function BufferCRC32C(CRC32: TCRC32; const Buffer; Size: TMemSize): TCRC32; overload;
Function BufferCRC32C(const Buffer; Size: TMemSize): TCRC32; overload;

//------------------------------------------------------------------------------

Function CRC32C_Init: TCRC32Context;
procedure CRC32C_Update(var Context: TCRC32Context; const Buffer; Size: TMemSize);
Function CRC32C_Final(var Context: TCRC32Context; const Buffer; Size: TMemSize): TCRC32; overload;
Function CRC32C_Final(var Context: TCRC32Context): TCRC32; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-64                                     
--------------------------------------------------------------------------------
===============================================================================}
{
                  polynomial        0x142F0E1EBA9EA3693
               initial value        0x0000000000000000
             final xor value        0x0000000000000000
       input bits reflection        False
      output bits reflection        False
}
//------------------------------------------------------------------------------
const
  InitialCRC64: TCRC64 = $0000000000000000;
  ZeroCRC64:    TCRC64 = 0;

Function BufferCRC64(CRC64: TCRC64; const Buffer; Size: TMemSize): TCRC64; overload;
Function BufferCRC64(const Buffer; Size: TMemSize): TCRC64; overload;

Function SameCRC64(A,B: TCRC64): Boolean;

//------------------------------------------------------------------------------
type
  TCRC64Context = type TCRC64;

Function CRC64_Init: TCRC64Context;
procedure CRC64_Update(var Context: TCRC64Context; const Buffer; Size: TMemSize);
Function CRC64_Final(var Context: TCRC64Context; const Buffer; Size: TMemSize): TCRC64; overload;
Function CRC64_Final(var Context: TCRC64Context): TCRC64; overload;

implementation

uses
  SysUtils;

{===============================================================================
    Internals - implementation
===============================================================================}

Function SwapEndian(const Value: UInt32): UInt32; overload;
begin
Result := UInt32(((Value and $000000FF) shl 24) or ((Value and $0000FF00) shl 8) or
                 ((Value and $00FF0000) shr 8) or ((Value and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(const Value: TCRC64): TCRC64; overload;
begin
Int64Rec(Result).Hi := SwapEndian(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := SwapEndian(Int64Rec(Value).Hi);
end;

//------------------------------------------------------------------------------

Function ExpandCRC24(const Value: TCRC24): UInt32;
begin
{
  TCRC24 always have system endianness, but used crc version is not reflected
  on output, meaning it is presented in reverse order than expected - it must
  therefore be byte-swapped (note bits within bytes are in correct order, this
  is ensured by used look-up table).
}
{$IFDEF ENDIAN_BIG}
Result := UInt32(Value[0]) or (UInt32(Value[1]) shl 8) or (UInt32(Value[2]) shl 16);
{$ELSE}
Result := UInt32(Value[2]) or (UInt32(Value[1]) shl 8) or (UInt32(Value[0]) shl 16);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function CollapseCRC24(const Value: UInt32): TCRC24;
begin
{$IFDEF ENDIAN_BIG}
Result[0] := Value and $FF;
Result[1] := (Value shr 8) and $FF;
Result[2] := (Value shr 16) and $FF;
{$ELSE}
Result[2] := Value and $FF;
Result[1] := (Value shr 8) and $FF;
Result[0] := (Value shr 16) and $FF;
{$ENDIF}
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    Adler-32
--------------------------------------------------------------------------------
===============================================================================}
const
  Adler32Modulo      = 65521;
  Adler32NoModRounds = 5552;

{===============================================================================
    Adler-32 - implementation
===============================================================================}

Function BufferAdler32(Adler32: TAdler32; const Buffer; Size: TMemSize): TAdler32;
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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferAdler32(const Buffer; Size: TMemSize): TAdler32;
begin
Result := BufferAdler32(InitialAdler32,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SameAdler32(A,B: TAdler32): Boolean;
begin
Result := A = B;
end;

//==============================================================================

Function Adler32_Init: TAdler32Context;
begin
Result := TAdler32Context(InitialAdler32);
end;

//------------------------------------------------------------------------------

procedure Adler32_Update(var Context: TAdler32Context; const Buffer; Size: TMemSize);
begin
Context := TAdler32Context(BufferAdler32(TAdler32(Context),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function Adler32_Final(var Context: TAdler32Context; const Buffer; Size: TMemSize): TAdler32;
begin
Adler32_Update(Context,Buffer,Size);
Result := Adler32_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Adler32_Final(var Context: TAdler32Context): TAdler32;
begin
Result := TAdler32(Context);
Context := TAdler32Context(ZeroAdler32);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                      CRC-8
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC8_TABLE: array[UInt8] of TCRC8 = (
    $00, $07, $0E, $09, $1C, $1B, $12, $15, $38, $3F, $36, $31, $24, $23, $2A, $2D,
    $70, $77, $7E, $79, $6C, $6B, $62, $65, $48, $4F, $46, $41, $54, $53, $5A, $5D,
    $E0, $E7, $EE, $E9, $FC, $FB, $F2, $F5, $D8, $DF, $D6, $D1, $C4, $C3, $CA, $CD,
    $90, $97, $9E, $99, $8C, $8B, $82, $85, $A8, $AF, $A6, $A1, $B4, $B3, $BA, $BD,
    $C7, $C0, $C9, $CE, $DB, $DC, $D5, $D2, $FF, $F8, $F1, $F6, $E3, $E4, $ED, $EA,
    $B7, $B0, $B9, $BE, $AB, $AC, $A5, $A2, $8F, $88, $81, $86, $93, $94, $9D, $9A,
    $27, $20, $29, $2E, $3B, $3C, $35, $32, $1F, $18, $11, $16, $03, $04, $0D, $0A,
    $57, $50, $59, $5E, $4B, $4C, $45, $42, $6F, $68, $61, $66, $73, $74, $7D, $7A,
    $89, $8E, $87, $80, $95, $92, $9B, $9C, $B1, $B6, $BF, $B8, $AD, $AA, $A3, $A4,
    $F9, $FE, $F7, $F0, $E5, $E2, $EB, $EC, $C1, $C6, $CF, $C8, $DD, $DA, $D3, $D4,
    $69, $6E, $67, $60, $75, $72, $7B, $7C, $51, $56, $5F, $58, $4D, $4A, $43, $44,
    $19, $1E, $17, $10, $05, $02, $0B, $0C, $21, $26, $2F, $28, $3D, $3A, $33, $34,
    $4E, $49, $40, $47, $52, $55, $5C, $5B, $76, $71, $78, $7F, $6A, $6D, $64, $63,
    $3E, $39, $30, $37, $22, $25, $2C, $2B, $06, $01, $08, $0F, $1A, $1D, $14, $13,
    $AE, $A9, $A0, $A7, $B2, $B5, $BC, $BB, $96, $91, $98, $9F, $8A, $8D, $84, $83,
    $DE, $D9, $D0, $D7, $C2, $C5, $CC, $CB, $E6, $E1, $E8, $EF, $FA, $FD, $F4, $F3);

{===============================================================================
    CRC-8 - implementation
===============================================================================}

Function BufferCRC8(CRC8: TCRC8; const Buffer; Size: TMemSize): TCRC8;
var
  CurrentData:  PUInt8;
  WorkCRC:      TCRC8;
  i:            TMemSize;
begin
CurrentData := @Buffer;
WorkCRC := CRC8;
For i := 1 to Size do
  begin
    WorkCRC := CRC8_TABLE[WorkCRC xor CurrentData^];
    Inc(CurrentData);
  end;
Result := WorkCRC;
end;

//------------------------------------------------------------------------------

Function BufferCRC8(const Buffer; Size: TMemSize): TCRC8;
begin
Result := BufferCRC8(InitialCRC8,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SameCRC8(A,B: TCRC8): Boolean;
begin
Result := A = B;
end;

//==============================================================================

Function CRC8_Init: TCRC8Context;
begin
Result := TCRC8Context(InitialCRC8);
end;

//------------------------------------------------------------------------------

procedure CRC8_Update(var Context: TCRC8Context; const Buffer; Size: TMemSize);
begin
Context := TCRC8Context(BufferCRC8(TCRC8(Context),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function CRC8_Final(var Context: TCRC8Context; const Buffer; Size: TMemSize): TCRC8;
begin
CRC8_Update(Context,Buffer,Size);
Result := CRC8_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CRC8_Final(var Context: TCRC8Context): TCRC8;
begin
Result := TCRC8(Context);
Context := TCRC8Context(ZeroCRC8);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-16
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC16_TABLE: array[UInt8] of TCRC16 = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241,
    $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
    $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40,
    $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
    $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
    $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
    $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641,
    $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
    $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240,
    $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
    $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
    $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41,
    $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
    $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
    $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
    $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240,
    $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
    $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41,
    $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
    $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41,
    $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640,
    $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
    $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
    $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
    $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40,
    $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
    $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40,
    $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
    $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641,
    $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040);

{===============================================================================
    CRC-16 - implementation
===============================================================================}

Function BufferCRC16(CRC16: TCRC16; const Buffer; Size: TMemSize): TCRC16;
var
  CurrentData:  PUInt8;
  WorkCRC:      TCRC16;
  i:            TMemSize;
begin
CurrentData := @Buffer;
WorkCRC := CRC16;
For i := 1 to Size do
  begin
    WorkCRC := CRC16_TABLE[UInt8(WorkCRC) xor CurrentData^] xor (WorkCRC shr 8);
    Inc(CurrentData);
  end;
Result := WorkCRC;
end;

//------------------------------------------------------------------------------

Function BufferCRC16(const Buffer; Size: TMemSize): TCRC16; overload;
begin
Result := BufferCRC16(InitialCRC32,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SameCRC16(A,B: TCRC16): Boolean;
begin
Result := A = B;
end;

//==============================================================================

Function CRC16_Init: TCRC16Context;
begin
Result := TCRC16Context(InitialCRC16);
end;

//------------------------------------------------------------------------------

procedure CRC16_Update(var Context: TCRC16Context; const Buffer; Size: TMemSize);
begin
Context := TCRC16Context(BufferCRC16(TCRC16(Context),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function CRC16_Final(var Context: TCRC16Context; const Buffer; Size: TMemSize): TCRC16;
begin
CRC16_Update(Context,Buffer,Size);
Result := CRC16_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CRC16_Final(var Context: TCRC16Context): TCRC16;
begin
Result := TCRC16(Context);
Context := TCRC16Context(ZeroCRC16);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-24
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC24_TABLE: array[UInt8] of UInt32 = (
    $00000000, $00FB4C86, $000DD58A, $00F6990C, $00E1E693, $001AAA15, $00EC3319, $00177F9F,
    $003981A1, $00C2CD27, $0034542B, $00CF18AD, $00D86732, $00232BB4, $00D5B2B8, $002EFE3E,
    $00894EC5, $00720243, $00849B4F, $007FD7C9, $0068A856, $0093E4D0, $00657DDC, $009E315A,
    $00B0CF64, $004B83E2, $00BD1AEE, $00465668, $005129F7, $00AA6571, $005CFC7D, $00A7B0FB,
    $00E9D10C, $00129D8A, $00E40486, $001F4800, $0008379F, $00F37B19, $0005E215, $00FEAE93,
    $00D050AD, $002B1C2B, $00DD8527, $0026C9A1, $0031B63E, $00CAFAB8, $003C63B4, $00C72F32,
    $00609FC9, $009BD34F, $006D4A43, $009606C5, $0081795A, $007A35DC, $008CACD0, $0077E056,
    $00591E68, $00A252EE, $0054CBE2, $00AF8764, $00B8F8FB, $0043B47D, $00B52D71, $004E61F7,
    $00D2A319, $0029EF9F, $00DF7693, $00243A15, $0033458A, $00C8090C, $003E9000, $00C5DC86,
    $00EB22B8, $00106E3E, $00E6F732, $001DBBB4, $000AC42B, $00F188AD, $000711A1, $00FC5D27,
    $005BEDDC, $00A0A15A, $00563856, $00AD74D0, $00BA0B4F, $004147C9, $00B7DEC5, $004C9243,
    $00626C7D, $009920FB, $006FB9F7, $0094F571, $00838AEE, $0078C668, $008E5F64, $007513E2,
    $003B7215, $00C03E93, $0036A79F, $00CDEB19, $00DA9486, $0021D800, $00D7410C, $002C0D8A,
    $0002F3B4, $00F9BF32, $000F263E, $00F46AB8, $00E31527, $001859A1, $00EEC0AD, $00158C2B,
    $00B23CD0, $00497056, $00BFE95A, $0044A5DC, $0053DA43, $00A896C5, $005E0FC9, $00A5434F,
    $008BBD71, $0070F1F7, $008668FB, $007D247D, $006A5BE2, $00911764, $00678E68, $009CC2EE,
    $00A44733, $005F0BB5, $00A992B9, $0052DE3F, $0045A1A0, $00BEED26, $0048742A, $00B338AC,
    $009DC692, $00668A14, $00901318, $006B5F9E, $007C2001, $00876C87, $0071F58B, $008AB90D,
    $002D09F6, $00D64570, $0020DC7C, $00DB90FA, $00CCEF65, $0037A3E3, $00C13AEF, $003A7669,
    $00148857, $00EFC4D1, $00195DDD, $00E2115B, $00F56EC4, $000E2242, $00F8BB4E, $0003F7C8,
    $004D963F, $00B6DAB9, $004043B5, $00BB0F33, $00AC70AC, $00573C2A, $00A1A526, $005AE9A0,
    $0074179E, $008F5B18, $0079C214, $00828E92, $0095F10D, $006EBD8B, $00982487, $00636801,
    $00C4D8FA, $003F947C, $00C90D70, $003241F6, $00253E69, $00DE72EF, $0028EBE3, $00D3A765,
    $00FD595B, $000615DD, $00F08CD1, $000BC057, $001CBFC8, $00E7F34E, $00116A42, $00EA26C4,
    $0076E42A, $008DA8AC, $007B31A0, $00807D26, $009702B9, $006C4E3F, $009AD733, $00619BB5,
    $004F658B, $00B4290D, $0042B001, $00B9FC87, $00AE8318, $0055CF9E, $00A35692, $00581A14,
    $00FFAAEF, $0004E669, $00F27F65, $000933E3, $001E4C7C, $00E500FA, $001399F6, $00E8D570,
    $00C62B4E, $003D67C8, $00CBFEC4, $0030B242, $0027CDDD, $00DC815B, $002A1857, $00D154D1,
    $009F3526, $006479A0, $0092E0AC, $0069AC2A, $007ED3B5, $00859F33, $0073063F, $00884AB9,
    $00A6B487, $005DF801, $00AB610D, $00502D8B, $00475214, $00BC1E92, $004A879E, $00B1CB18,
    $00167BE3, $00ED3765, $001BAE69, $00E0E2EF, $00F79D70, $000CD1F6, $00FA48FA, $0001047C,
    $002FFA42, $00D4B6C4, $00222FC8, $00D9634E, $00CE1CD1, $00355057, $00C3C95B, $003885DD);

{===============================================================================
    CRC-24 - implementation
===============================================================================}

Function BufferCRC24(CRC24: TCRC24; const Buffer; Size: TMemSize): TCRC24;
var
  CurrentData:  PUInt8;
  WorkCRC:      UInt32;
  i:            TMemSize;
begin
CurrentData := @Buffer;
WorkCRC := ExpandCRC24(CRC24);
For i := 1 to Size do
  begin
    WorkCRC := CRC24_TABLE[UInt8(WorkCRC) xor CurrentData^] xor (WorkCRC shr 8);
    Inc(CurrentData);
  end;
Result := CollapseCRC24(WorkCRC);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferCRC24(const Buffer; Size: TMemSize): TCRC24;
begin
Result := BufferCRC24(InitialCRC24,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SameCRC24(A,B: TCRC24): Boolean;
begin
Result := ExpandCRC24(A) = ExpandCRC24(B);
end;

//==============================================================================

Function CRC24_Init: TCRC24Context;
begin
Result := TCRC24Context(InitialCRC24);
end;

//------------------------------------------------------------------------------

procedure CRC24_Update(var Context: TCRC24Context; const Buffer; Size: TMemSize);
begin
Context := TCRC24Context(BufferCRC24(TCRC24(Context),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function CRC24_Final(var Context: TCRC24Context; const Buffer; Size: TMemSize): TCRC24;
begin
CRC24_Update(Context,Buffer,Size);
Result := CRC24_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CRC24_Final(var Context: TCRC24Context): TCRC24;
begin
Result := TCRC24(Context);
Context := TCRC24Context(ZeroCRC24);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-32
--------------------------------------------------------------------------------
===============================================================================}
type
  TCRC32Table = array[UInt8] of TCRC32;
  PCRC32Table = ^TCRC32Table;

const
  CRC32_TABLE: TCRC32Table = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
    $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

//------------------------------------------------------------------------------

Function BufferCRC32Common(CRC32: TCRC32; const Buffer; Size: TMemSize; Table: PCRC32Table): TCRC32;{$IFNDEF PurePascal} register; assembler;
asm
{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
                  win32 + lin32       win64           lin64
     CRC32             EAX             ECX             EDI
   @Buffer             EDX             RDX             RSI
      Size             ECX              R8             RDX
     Table          [EBP + 8]           R9             RCX
    Result             EAX             EAX             EAX
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}
{$IFDEF x64}
{$IFDEF Windows}
                MOV   EAX, ECX
                NOT   EAX
                TEST  R8, R8
                JZ    @RoutineEnd

  @MainLoop:    MOV   ECX, EAX
                AND   ECX, $000000FF
                XOR   CL,  byte ptr [RDX]
                MOV   ECX, dword ptr [R9 + RCX * 4]
                SHR   EAX, 8
                XOR   EAX, ECX

                INC   RDX
                DEC   R8
                JNZ   @MainLoop

  @RoutineEnd:  NOT   EAX
{$ELSE}
                MOV   EAX, EDI
                NOT   EAX
                TEST  RDX, RDX
                JZ    @RoutineEnd

  @MainLoop:    MOV   R8D, EAX
                AND   R8D, $000000FF
                XOR   R8B, byte ptr [RSI]
                MOV   R8D, dword ptr [RCX + R8 * 4]
                SHR   EAX, 8
                XOR   EAX, R8D

                INC   RSI
                DEC   RDX
                JNZ   @MainLoop

  @RoutineEnd:  NOT   EAX
{$ENDIF}
{$ELSE}
                PUSH  EBX
                PUSH  ESI

                MOV   ESI, Table
                NOT   EAX
                TEST  ECX, ECX
                JZ    @RoutineEnd

  @MainLoop:    MOV   EBX, EAX
                AND   EBX, $000000FF
                XOR   BL,  byte ptr [EDX]
                MOV   EBX, dword ptr [ESI + EBX * 4]
                SHR   EAX, 8
                XOR   EAX, EBX

                INC   EDX
                DEC   ECX
                JNZ   @MainLoop

  @RoutineEnd:  NOT   EAX
  
                POP   ESI
                POP   EBX
{$ENDIF}
end;
{$ELSE}
var
  CurrentData:  PUInt8;
  WorkCRC:      TCRC32;  
  i:            TMemSize;
begin
CurrentData := @Buffer;
WorkCRC := not CRC32;
For i := 1 to Size do
  begin
    WorkCRC := Table^[UInt8(WorkCRC) xor CurrentData^] xor (WorkCRC shr 8);
    Inc(CurrentData);
  end;
Result := not WorkCRC;
end;
{$ENDIF}

{===============================================================================
    CRC-32 - implementation
===============================================================================}

Function BufferCRC32(CRC32: TCRC32; const Buffer; Size: TMemSize): TCRC32;
begin
Result := BufferCRC32Common(CRC32,Buffer,Size,@CRC32_TABLE);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferCRC32(const Buffer; Size: TMemSize): TCRC32;
begin
Result := BufferCRC32Common(InitialCRC32,Buffer,Size,@CRC32_TABLE);
end;

//------------------------------------------------------------------------------

Function SameCRC32(A,B: TCRC32): Boolean;
begin
Result := A = B;
end;

//==============================================================================

Function CRC32_Init: TCRC32Context;
begin
Result := TCRC32Context(InitialCRC32);
end;

//------------------------------------------------------------------------------

procedure CRC32_Update(var Context: TCRC32Context; const Buffer; Size: TMemSize);
begin
Context := TCRC32Context(BufferCRC32Common(TCRC32(Context),Buffer,Size,@CRC32_TABLE));
end;

//------------------------------------------------------------------------------

Function CRC32_Final(var Context: TCRC32Context; const Buffer; Size: TMemSize): TCRC32;
begin
CRC32_Update(Context,Buffer,Size);
Result := CRC32_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CRC32_Final(var Context: TCRC32Context): TCRC32;
begin
Result := TCRC32(Context);
Context := TCRC32Context(ZeroCRC32);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-32C
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC32C_TABLE: TCRC32Table = (
    $00000000, $F26B8303, $E13B70F7, $1350F3F4, $C79A971F, $35F1141C, $26A1E7E8, $D4CA64EB,
    $8AD958CF, $78B2DBCC, $6BE22838, $9989AB3B, $4D43CFD0, $BF284CD3, $AC78BF27, $5E133C24,
    $105EC76F, $E235446C, $F165B798, $030E349B, $D7C45070, $25AFD373, $36FF2087, $C494A384,
    $9A879FA0, $68EC1CA3, $7BBCEF57, $89D76C54, $5D1D08BF, $AF768BBC, $BC267848, $4E4DFB4B,
    $20BD8EDE, $D2D60DDD, $C186FE29, $33ED7D2A, $E72719C1, $154C9AC2, $061C6936, $F477EA35,
    $AA64D611, $580F5512, $4B5FA6E6, $B93425E5, $6DFE410E, $9F95C20D, $8CC531F9, $7EAEB2FA,
    $30E349B1, $C288CAB2, $D1D83946, $23B3BA45, $F779DEAE, $05125DAD, $1642AE59, $E4292D5A,
    $BA3A117E, $4851927D, $5B016189, $A96AE28A, $7DA08661, $8FCB0562, $9C9BF696, $6EF07595,
    $417B1DBC, $B3109EBF, $A0406D4B, $522BEE48, $86E18AA3, $748A09A0, $67DAFA54, $95B17957,
    $CBA24573, $39C9C670, $2A993584, $D8F2B687, $0C38D26C, $FE53516F, $ED03A29B, $1F682198,
    $5125DAD3, $A34E59D0, $B01EAA24, $42752927, $96BF4DCC, $64D4CECF, $77843D3B, $85EFBE38,
    $DBFC821C, $2997011F, $3AC7F2EB, $C8AC71E8, $1C661503, $EE0D9600, $FD5D65F4, $0F36E6F7,
    $61C69362, $93AD1061, $80FDE395, $72966096, $A65C047D, $5437877E, $4767748A, $B50CF789,
    $EB1FCBAD, $197448AE, $0A24BB5A, $F84F3859, $2C855CB2, $DEEEDFB1, $CDBE2C45, $3FD5AF46,
    $7198540D, $83F3D70E, $90A324FA, $62C8A7F9, $B602C312, $44694011, $5739B3E5, $A55230E6,
    $FB410CC2, $092A8FC1, $1A7A7C35, $E811FF36, $3CDB9BDD, $CEB018DE, $DDE0EB2A, $2F8B6829,
    $82F63B78, $709DB87B, $63CD4B8F, $91A6C88C, $456CAC67, $B7072F64, $A457DC90, $563C5F93,
    $082F63B7, $FA44E0B4, $E9141340, $1B7F9043, $CFB5F4A8, $3DDE77AB, $2E8E845F, $DCE5075C,
    $92A8FC17, $60C37F14, $73938CE0, $81F80FE3, $55326B08, $A759E80B, $B4091BFF, $466298FC,
    $1871A4D8, $EA1A27DB, $F94AD42F, $0B21572C, $DFEB33C7, $2D80B0C4, $3ED04330, $CCBBC033,
    $A24BB5A6, $502036A5, $4370C551, $B11B4652, $65D122B9, $97BAA1BA, $84EA524E, $7681D14D,
    $2892ED69, $DAF96E6A, $C9A99D9E, $3BC21E9D, $EF087A76, $1D63F975, $0E330A81, $FC588982,
    $B21572C9, $407EF1CA, $532E023E, $A145813D, $758FE5D6, $87E466D5, $94B49521, $66DF1622,
    $38CC2A06, $CAA7A905, $D9F75AF1, $2B9CD9F2, $FF56BD19, $0D3D3E1A, $1E6DCDEE, $EC064EED,
    $C38D26C4, $31E6A5C7, $22B65633, $D0DDD530, $0417B1DB, $F67C32D8, $E52CC12C, $1747422F,
    $49547E0B, $BB3FFD08, $A86F0EFC, $5A048DFF, $8ECEE914, $7CA56A17, $6FF599E3, $9D9E1AE0,
    $D3D3E1AB, $21B862A8, $32E8915C, $C083125F, $144976B4, $E622F5B7, $F5720643, $07198540,
    $590AB964, $AB613A67, $B831C993, $4A5A4A90, $9E902E7B, $6CFBAD78, $7FAB5E8C, $8DC0DD8F,
    $E330A81A, $115B2B19, $020BD8ED, $F0605BEE, $24AA3F05, $D6C1BC06, $C5914FF2, $37FACCF1,
    $69E9F0D5, $9B8273D6, $88D28022, $7AB90321, $AE7367CA, $5C18E4C9, $4F48173D, $BD23943E,
    $F36E6F75, $0105EC76, $12551F82, $E03E9C81, $34F4F86A, $C69F7B69, $D5CF889D, $27A40B9E,
    $79B737BA, $8BDCB4B9, $988C474D, $6AE7C44E, $BE2DA0A5, $4C4623A6, $5F16D052, $AD7D5351);

{===============================================================================
    CRC-32C - implementation
===============================================================================}

Function BufferCRC32C(CRC32: TCRC32; const Buffer; Size: TMemSize): TCRC32;
begin
Result := BufferCRC32Common(CRC32,Buffer,Size,@CRC32C_TABLE);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferCRC32C(const Buffer; Size: TMemSize): TCRC32;
begin
Result := BufferCRC32Common(InitialCRC32,Buffer,Size,@CRC32C_TABLE);
end;

//==============================================================================

Function CRC32C_Init: TCRC32Context;
begin
Result := TCRC32Context(InitialCRC32);
end;

//------------------------------------------------------------------------------

procedure CRC32C_Update(var Context: TCRC32Context; const Buffer; Size: TMemSize);
begin
Context := TCRC32Context(BufferCRC32Common(TCRC32(Context),Buffer,Size,@CRC32C_TABLE));
end;

//------------------------------------------------------------------------------

Function CRC32C_Final(var Context: TCRC32Context; const Buffer; Size: TMemSize): TCRC32;
begin
CRC32C_Update(Context,Buffer,Size);
Result := CRC32C_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CRC32C_Final(var Context: TCRC32Context): TCRC32;
begin
Result := TCRC32(Context);
Context := TCRC32Context(ZeroCRC32);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                     CRC-64
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC64_TABLE: array[UInt8] of TCRC64 = (
    TCRC64($0000000000000000), TCRC64($9336EAA9EBE1F042), TCRC64($266DD453D7C3E185), TCRC64($B55B3EFA3C2211C7),
    TCRC64($DFEC420E45663349), TCRC64($4CDAA8A7AE87C30B), TCRC64($F981965D92A5D2CC), TCRC64($6AB77CF47944228E),
    TCRC64($BED9851C8ACC6692), TCRC64($2DEF6FB5612D96D0), TCRC64($98B4514F5D0F8717), TCRC64($0B82BBE6B6EE7755),
    TCRC64($6135C712CFAA55DB), TCRC64($F2032DBB244BA599), TCRC64($475813411869B45E), TCRC64($D46EF9E8F388441C),
    TCRC64($EF85E190FF783D66), TCRC64($7CB30B391499CD24), TCRC64($C9E835C328BBDCE3), TCRC64($5ADEDF6AC35A2CA1),
    TCRC64($3069A39EBA1E0E2F), TCRC64($A35F493751FFFE6D), TCRC64($160477CD6DDDEFAA), TCRC64($85329D64863C1FE8),
    TCRC64($515C648C75B45BF4), TCRC64($C26A8E259E55ABB6), TCRC64($7731B0DFA277BA71), TCRC64($E4075A7649964A33),
    TCRC64($8EB0268230D268BD), TCRC64($1D86CC2BDB3398FF), TCRC64($A8DDF2D1E7118938), TCRC64($3BEB18780CF0797A),
    TCRC64($DE0BC321FFF17ACC), TCRC64($4D3D298814108A8E), TCRC64($F866177228329B49), TCRC64($6B50FDDBC3D36B0B),
    TCRC64($01E7812FBA974985), TCRC64($92D16B865176B9C7), TCRC64($278A557C6D54A800), TCRC64($B4BCBFD586B55842),
    TCRC64($60D2463D753D1C5E), TCRC64($F3E4AC949EDCEC1C), TCRC64($46BF926EA2FEFDDB), TCRC64($D58978C7491F0D99),
    TCRC64($BF3E0433305B2F17), TCRC64($2C08EE9ADBBADF55), TCRC64($9953D060E798CE92), TCRC64($0A653AC90C793ED0),
    TCRC64($318E22B1008947AA), TCRC64($A2B8C818EB68B7E8), TCRC64($17E3F6E2D74AA62F), TCRC64($84D51C4B3CAB566D),
    TCRC64($EE6260BF45EF74E3), TCRC64($7D548A16AE0E84A1), TCRC64($C80FB4EC922C9566), TCRC64($5B395E4579CD6524),
    TCRC64($8F57A7AD8A452138), TCRC64($1C614D0461A4D17A), TCRC64($A93A73FE5D86C0BD), TCRC64($3A0C9957B66730FF),
    TCRC64($50BBE5A3CF231271), TCRC64($C38D0F0A24C2E233), TCRC64($76D631F018E0F3F4), TCRC64($E5E0DB59F30103B6),
    TCRC64($2F216CEA150205DA), TCRC64($BC178643FEE3F598), TCRC64($094CB8B9C2C1E45F), TCRC64($9A7A52102920141D),
    TCRC64($F0CD2EE450643693), TCRC64($63FBC44DBB85C6D1), TCRC64($D6A0FAB787A7D716), TCRC64($4596101E6C462754),
    TCRC64($91F8E9F69FCE6348), TCRC64($02CE035F742F930A), TCRC64($B7953DA5480D82CD), TCRC64($24A3D70CA3EC728F),
    TCRC64($4E14ABF8DAA85001), TCRC64($DD2241513149A043), TCRC64($68797FAB0D6BB184), TCRC64($FB4F9502E68A41C6),
    TCRC64($C0A48D7AEA7A38BC), TCRC64($539267D3019BC8FE), TCRC64($E6C959293DB9D939), TCRC64($75FFB380D658297B),
    TCRC64($1F48CF74AF1C0BF5), TCRC64($8C7E25DD44FDFBB7), TCRC64($39251B2778DFEA70), TCRC64($AA13F18E933E1A32),
    TCRC64($7E7D086660B65E2E), TCRC64($ED4BE2CF8B57AE6C), TCRC64($5810DC35B775BFAB), TCRC64($CB26369C5C944FE9),
    TCRC64($A1914A6825D06D67), TCRC64($32A7A0C1CE319D25), TCRC64($87FC9E3BF2138CE2), TCRC64($14CA749219F27CA0),
    TCRC64($F12AAFCBEAF37F16), TCRC64($621C456201128F54), TCRC64($D7477B983D309E93), TCRC64($44719131D6D16ED1),
    TCRC64($2EC6EDC5AF954C5F), TCRC64($BDF0076C4474BC1D), TCRC64($08AB39967856ADDA), TCRC64($9B9DD33F93B75D98),
    TCRC64($4FF32AD7603F1984), TCRC64($DCC5C07E8BDEE9C6), TCRC64($699EFE84B7FCF801), TCRC64($FAA8142D5C1D0843),
    TCRC64($901F68D925592ACD), TCRC64($03298270CEB8DA8F), TCRC64($B672BC8AF29ACB48), TCRC64($25445623197B3B0A),
    TCRC64($1EAF4E5B158B4270), TCRC64($8D99A4F2FE6AB232), TCRC64($38C29A08C248A3F5), TCRC64($ABF470A129A953B7),
    TCRC64($C1430C5550ED7139), TCRC64($5275E6FCBB0C817B), TCRC64($E72ED806872E90BC), TCRC64($741832AF6CCF60FE),
    TCRC64($A076CB479F4724E2), TCRC64($334021EE74A6D4A0), TCRC64($861B1F144884C567), TCRC64($152DF5BDA3653525),
    TCRC64($7F9A8949DA2117AB), TCRC64($ECAC63E031C0E7E9), TCRC64($59F75D1A0DE2F62E), TCRC64($CAC1B7B3E603066C),
    TCRC64($CD74327DC0E5FAF6), TCRC64($5E42D8D42B040AB4), TCRC64($EB19E62E17261B73), TCRC64($782F0C87FCC7EB31),
    TCRC64($129870738583C9BF), TCRC64($81AE9ADA6E6239FD), TCRC64($34F5A4205240283A), TCRC64($A7C34E89B9A1D878),
    TCRC64($73ADB7614A299C64), TCRC64($E09B5DC8A1C86C26), TCRC64($55C063329DEA7DE1), TCRC64($C6F6899B760B8DA3),
    TCRC64($AC41F56F0F4FAF2D), TCRC64($3F771FC6E4AE5F6F), TCRC64($8A2C213CD88C4EA8), TCRC64($191ACB95336DBEEA),
    TCRC64($22F1D3ED3F9DC790), TCRC64($B1C73944D47C37D2), TCRC64($049C07BEE85E2615), TCRC64($97AAED1703BFD657),
    TCRC64($FD1D91E37AFBF4D9), TCRC64($6E2B7B4A911A049B), TCRC64($DB7045B0AD38155C), TCRC64($4846AF1946D9E51E),
    TCRC64($9C2856F1B551A102), TCRC64($0F1EBC585EB05140), TCRC64($BA4582A262924087), TCRC64($2973680B8973B0C5),
    TCRC64($43C414FFF037924B), TCRC64($D0F2FE561BD66209), TCRC64($65A9C0AC27F473CE), TCRC64($F69F2A05CC15838C),
    TCRC64($137FF15C3F14803A), TCRC64($80491BF5D4F57078), TCRC64($3512250FE8D761BF), TCRC64($A624CFA6033691FD),
    TCRC64($CC93B3527A72B373), TCRC64($5FA559FB91934331), TCRC64($EAFE6701ADB152F6), TCRC64($79C88DA84650A2B4),
    TCRC64($ADA67440B5D8E6A8), TCRC64($3E909EE95E3916EA), TCRC64($8BCBA013621B072D), TCRC64($18FD4ABA89FAF76F),
    TCRC64($724A364EF0BED5E1), TCRC64($E17CDCE71B5F25A3), TCRC64($5427E21D277D3464), TCRC64($C71108B4CC9CC426),
    TCRC64($FCFA10CCC06CBD5C), TCRC64($6FCCFA652B8D4D1E), TCRC64($DA97C49F17AF5CD9), TCRC64($49A12E36FC4EAC9B),
    TCRC64($231652C2850A8E15), TCRC64($B020B86B6EEB7E57), TCRC64($057B869152C96F90), TCRC64($964D6C38B9289FD2),
    TCRC64($422395D04AA0DBCE), TCRC64($D1157F79A1412B8C), TCRC64($644E41839D633A4B), TCRC64($F778AB2A7682CA09),
    TCRC64($9DCFD7DE0FC6E887), TCRC64($0EF93D77E42718C5), TCRC64($BBA2038DD8050902), TCRC64($2894E92433E4F940),
    TCRC64($E2555E97D5E7FF2C), TCRC64($7163B43E3E060F6E), TCRC64($C4388AC402241EA9), TCRC64($570E606DE9C5EEEB),
    TCRC64($3DB91C999081CC65), TCRC64($AE8FF6307B603C27), TCRC64($1BD4C8CA47422DE0), TCRC64($88E22263ACA3DDA2),
    TCRC64($5C8CDB8B5F2B99BE), TCRC64($CFBA3122B4CA69FC), TCRC64($7AE10FD888E8783B), TCRC64($E9D7E57163098879),
    TCRC64($836099851A4DAAF7), TCRC64($1056732CF1AC5AB5), TCRC64($A50D4DD6CD8E4B72), TCRC64($363BA77F266FBB30),
    TCRC64($0DD0BF072A9FC24A), TCRC64($9EE655AEC17E3208), TCRC64($2BBD6B54FD5C23CF), TCRC64($B88B81FD16BDD38D),
    TCRC64($D23CFD096FF9F103), TCRC64($410A17A084180141), TCRC64($F451295AB83A1086), TCRC64($6767C3F353DBE0C4),
    TCRC64($B3093A1BA053A4D8), TCRC64($203FD0B24BB2549A), TCRC64($9564EE487790455D), TCRC64($065204E19C71B51F),
    TCRC64($6CE57815E5359791), TCRC64($FFD392BC0ED467D3), TCRC64($4A88AC4632F67614), TCRC64($D9BE46EFD9178656),
    TCRC64($3C5E9DB62A1685E0), TCRC64($AF68771FC1F775A2), TCRC64($1A3349E5FDD56465), TCRC64($8905A34C16349427),
    TCRC64($E3B2DFB86F70B6A9), TCRC64($70843511849146EB), TCRC64($C5DF0BEBB8B3572C), TCRC64($56E9E1425352A76E),
    TCRC64($828718AAA0DAE372), TCRC64($11B1F2034B3B1330), TCRC64($A4EACCF9771902F7), TCRC64($37DC26509CF8F2B5),
    TCRC64($5D6B5AA4E5BCD03B), TCRC64($CE5DB00D0E5D2079), TCRC64($7B068EF7327F31BE), TCRC64($E830645ED99EC1FC),
    TCRC64($D3DB7C26D56EB886), TCRC64($40ED968F3E8F48C4), TCRC64($F5B6A87502AD5903), TCRC64($668042DCE94CA941),
    TCRC64($0C373E2890088BCF), TCRC64($9F01D4817BE97B8D), TCRC64($2A5AEA7B47CB6A4A), TCRC64($B96C00D2AC2A9A08),
    TCRC64($6D02F93A5FA2DE14), TCRC64($FE341393B4432E56), TCRC64($4B6F2D6988613F91), TCRC64($D859C7C06380CFD3),
    TCRC64($B2EEBB341AC4ED5D), TCRC64($21D8519DF1251D1F), TCRC64($94836F67CD070CD8), TCRC64($07B585CE26E6FC9A));

{===============================================================================
    CRC-64 - implementation
===============================================================================}

Function BufferCRC64(CRC64: TCRC64; const Buffer; Size: TMemSize): TCRC64;
var
  CurrentData:  PUInt8;
  WorkCRC:      TCRC64;
  i:            TMemSize;
begin
CurrentData := @Buffer;
WorkCRC := SwapEndian(CRC64);
For i := 1 to Size do
  begin
    WorkCRC := CRC64_TABLE[UInt8(WorkCRC) xor CurrentData^] xor (WorkCRC shr 8);
    Inc(CurrentData);
  end;
Result := SwapEndian(WorkCRC);
end;

//------------------------------------------------------------------------------

Function BufferCRC64(const Buffer; Size: TMemSize): TCRC64;
begin
Result := BufferCRC64(InitialCRC64,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SameCRC64(A,B: TCRC64): Boolean;
begin
Result := A = B;
end;

//==============================================================================

Function CRC64_Init: TCRC64Context;
begin
Result := TCRC64Context(InitialCRC64);
end;

//------------------------------------------------------------------------------

procedure CRC64_Update(var Context: TCRC64Context; const Buffer; Size: TMemSize);
begin
Context := TCRC64Context(BufferCRC64(TCRC64(Context),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function CRC64_Final(var Context: TCRC64Context; const Buffer; Size: TMemSize): TCRC64;
begin
CRC64_Update(Context,Buffer,Size);
Result := CRC64_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CRC64_Final(var Context: TCRC64Context): TCRC64;
begin
Result := TCRC64(Context);
Context := TCRC64Context(ZeroCRC64);
end;

end.
