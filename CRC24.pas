{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CRC-24 calculation

    Provides means of calculating 24-bit cyclic redundancy check (CRC-24) for
    almost any provided data (eg. buffers, strings, streams or files).
    Data must consist of integral (whole) bytes, arbitrary bitstreams are not
    supported.

    It can be used either in form of objects, where you create an instance of
    approprite class (eg. TCRC24Hash) and use its methods to do the processing,
    or you can use provided procedural form (BufferCRC24, CRC24ToStr, ...).

    There is also class TCRC24CustomHash, which allows you to change parameters
    of CRC-24 algorithm (eg. polynomial or initial value), so you can calculate
    CRC for any desired specification.
    Provided constant array CRC24_KNOWN_PRESETS offers specifications for some
    of the known CRC-24 variants - you can use it to initialize an instance of
    TCRC24CustomHash, it will then calculate the selected crc version.

  Version 1.1 (2026-07-03)

  Last change 2026-07-03

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.CRC24

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
unit CRC24;

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
  ECRC24Exception = class(EHashException);

  ECRC24IncompatibleClass = class(ECRC24Exception);
  ECRC24IndexOutOfBounds  = class(ECRC24Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TCRC24 are always ordered from least significant byte to most
  significant byte (little endian).

  Type TCRC24Sys has no such guarantee and its endianness is system-dependent.

    WARNING - TCRC24Sys is of differing size than TCRC24, only lower 24 bits
              are used.

  To convert the checksum in default ordering to a required specific ordering,
  use methods CRC24ToLE for little endian and CRC24ToBE for big endian.
  Note that these methods are expecting the input value to be in default
  ordering, if it is not, the result will be wrong. Be carefull when using them.
}
type
  TCRC24 = packed array[0..2] of UInt8;
  PCRC24 = ^TCRC24;

  TCRC24Sys = UInt32;
  PCRC24Sys = ^TCRC24Sys;

  TCRC24Table = array[UInt8] of TCRC24Sys;
  PCRC24Table = ^TCRC24Table;

const
{
  Initial value of CRC-24. Use only in standalone functions, do not use it
  anywhere else.
}
  InitialCRC24: TCRC24 = ($CE,$04,$B7);

  ZeroCRC24: TCRC24 = (0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                 TCRC24BaseHash
-------------------------------------------------------------------------------                                 
===============================================================================}
{===============================================================================
    TCRC24BaseHash - class declaration
===============================================================================}
type
  TCRC24BaseHash = class(TStreamHash)
  protected
    fCRC24Value:  TCRC24Sys;
    fCRC24Table:  PCRC24Table;
    Function GetCRC24: TCRC24; virtual;
    Function GetCRC24Poly: TCRC24Sys; virtual;
    Function GetCRC24PolyRef: TCRC24Sys; virtual; abstract;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure InitializeTable; virtual; abstract;
    procedure FinalizeTable; virtual; abstract;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class Function CRC24ToSys(CRC24: TCRC24): TCRC24Sys; virtual;
    class Function CRC24FromSys(CRC24: TCRC24Sys): TCRC24; virtual;
    class Function CRC24ToLE(CRC24: TCRC24): TCRC24; virtual;
    class Function CRC24ToBE(CRC24: TCRC24): TCRC24; virtual;
    class Function CRC24FromLE(CRC24: TCRC24): TCRC24; virtual;
    class Function CRC24FromBE(CRC24: TCRC24): TCRC24; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TCRC24); overload; virtual;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TCRC24); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property CRC24: TCRC24 read GetCRC24;
    property CRC24Sys: TCRC24Sys read fCRC24Value;
    property CRC24Poly: TCRC24Sys read GetCRC24Poly;
    property CRC24PolyRef: TCRC24Sys read GetCRC24PolyRef;
    property CRC24Table: PCRC24Table read fCRC24Table;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TCRC24Hash                                   
--------------------------------------------------------------------------------
===============================================================================}
{
  Hardcoded implementation of CRC-24/OPENPGP, following parameters are used
  within the calculation:

                  polynomial        0x1864CFB
               initial value        0xB704CE
             final xor value        0x000000
       input bits reflection        False
      output bits reflection        False
}
{===============================================================================
    TCRC24Hash - class declaration
===============================================================================}
type
  TCRC24Hash = class(TCRC24BaseHash)
  protected
    Function GetCRC24PolyRef: TCRC24Sys; override;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure InitializeTable; override;
    procedure FinalizeTable; override;
  public
    class Function HashName: String; override;
    class Function HashFinalization: Boolean; override;
    procedure Init; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TCRC24CustomHash
--------------------------------------------------------------------------------
===============================================================================}
type
  TCRC24CustomPreset = record
    Name:           String;     // name assigned to this CRC-24 within this library
    Aliases:        String;     // name aliasses, separated by comma (,)
    Polynomial:     TCRC24Sys;  // polynomial with highest bit omitted, only lower 24 bits are part of the polynomial
    RefPolynomial:  TCRC24Sys;  // polynomial with reflected bit order and original highest bit omitted (only lower 24 bits)
    FullPolynomial: UInt32;     // full polynomial (only lower 25 bits are to be observed)
    InitialValue:   TCRC24;     // initial value of CRC register
    ReflectIn:      Boolean;    // order in which bits within input bytes are processed (true = LSB, false = MSB)
    ReflectOut:     Boolean;    // resulting CRC-24 is bit-swapped before presentation
    XOROutValue:    TCRC24;     // value XORed to the register after all processing is done
    Check:          TCRC24;     // CRC-24 of UTF-24 (really ASCII) encoded string "1234567249" (without quotes)
    Residue:        TCRC24;     // what is left in CRC-24 register (before final xor) after hashing of error-free data with appended CRC-24 value
    Codewords:      String;     // several comma-separated (,) datastream-crc pairs (binary, hexadecimal notation)
  end;

//------------------------------------------------------------------------------
// Source: reveng.sourceforge.net/crc-catalogue/17plus.htm
const
  CRC24_KNOWN_PRESETS: array[0..7] of TCRC24CustomPreset = (
   (Name:           'CRC-24/BLE';
    Aliases:        '';
    Polynomial:     $00065B;
    RefPolynomial:  $DA6000;
    FullPolynomial: $100065B;
    InitialValue:   ($55,$55,$55);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00,$00);
    Check:          ($56,$5A,$C2);
    Residue:        ($00,$00,$00);
    Codewords:      '0003424C45290ACE,' +
                    '4209A6A5A4A3A2C1010203ADB4EB,' +
                    '00119992B1EBD7900201050702031802180418A85DEF,' +
                    '030C5F96EA3018009992B1EBD790DF02EB,' +
                    '04129992B1EBD7900B094B6579666F6264656D6FD39F03,' +
                    '05225F96EA3018009992B1EBD7901B0A8560A77B22020F0050000000D007FFFFFFFF1FA948DA02,' +
                    '4021EE04A5DDA7F90B094C69616D73424C4576320319410302010607030D180F180A181C929E,' +
                    'C522C4A5F2BE7479EE04A5DDA7F9667265500B085B030500180000004800FFFFFFFF1FA9E478D0,' +
                    '00090DEF84B72D3C020105C2E2A4,' +
                    '0522AB2F853118000DEF84B72D3C782118C9567FA202270050000000D007FFFFFFFF1FAF0F1957'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-24/FLEXRAY-A';
    Aliases:        '';
    Polynomial:     $5D6DCB;
    RefPolynomial:  $D3B6BA;
    FullPolynomial: $15D6DCB;
    InitialValue:   ($BA,$DC,$FE);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00,$00);
    Check:          ($BD,$79,$79); 
    Residue:        ($00,$00,$00);
    Codewords:      '18020209880000F339C1,' +
                    '600A0248C80102646D70,' +
                    '205606C848102030405060474380,' +
                    '202E06C84810203040506096C9D1,' +
                    '201A06C848102030405060B072EB'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-24/FLEXRAY-B';
    Aliases:        '';
    Polynomial:     $5D6DCB ;
    RefPolynomial:  $D3B6BA;
    FullPolynomial: $15D6DCB;
    InitialValue:   ($EF,$CD,$AB);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00,$00);
    Check:          ($B8,$23,$1F); 
    Residue:        ($00,$00,$00);
    Codewords:      '18020209880000D5B910,' +
                    '600A0248C8010242EDA1,' +
                    '205606C848102030405060E6D9BE,' +
                    '202E06C8481020304050603753EF,' +
                    '201A06C84810203040506011E8D5'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-24/INTERLAKEN';
    Aliases:        '';
    Polynomial:     $328B63;
    RefPolynomial:  $C6D14C;
    FullPolynomial: $1328B63;
    InitialValue:   ($FF,$FF,$FF);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($FF,$FF,$FF);
    Check:          ($E6,$F3,$B4); 
    Residue:        ($63,$4E,$14);
    Codewords:      '520BB1047D585E00C2B4B401BBAF01000000FCB0B3A8468E1A0A01E1' +
                      'BA38A9DF00003677EEA56DDABEB48D4D93A88A1200001F9515F655DC' +
                      'C3857A641B260C51F10000000000000059E69D'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-24/LTE-A';
    Aliases:        '';
    Polynomial:     $864CFB;
    RefPolynomial:  $DF3261;
    FullPolynomial: $1864CFB;
    InitialValue:   ($00,$00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00,$00);
    Check:          ($03,$E7,$CD);
    Residue:        ($00,$00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-24/LTE-B';
    Aliases:        '';
    Polynomial:     $800063;
    RefPolynomial:  $C60001;
    FullPolynomial: $8000631;
    InitialValue:   ($00,$00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00,$00);
    Check:          ($52,$EF,$23); 
    Residue:        ($00,$00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-24/OPENPGP';
    Aliases:        'CRC-24';
    Polynomial:     $864CFB;
    RefPolynomial:  $DF3261;
    FullPolynomial: $1864CFB;
    InitialValue:   ($CE,$04,$B7);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00,$00);
    Check:          ($02,$CF,$21);
    Residue:        ($00,$00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-24/OS-9';
    Aliases:        '';
    Polynomial:     $800063;
    RefPolynomial:  $C60001;
    FullPolynomial: $1800063;
    InitialValue:   ($FF,$FF,$FF);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($FF,$FF,$FF);
    Check:          ($A5,$0F,$20);
    Residue:        ($E3,$0F,$80);
    Codewords:      '87CD00320021F181D600260029FF0000000F01040000010006010100' +
                      '1000100101445261ED015242C643626D4473EB0C4EDF,' +
                    '87CD0020000D118108001500FA556E4D6F756EF401113F21265F103F06782872,' +
                    '87CD003C002CF181D500300033030003051A00000100010100011808' +
                      '180D1B040117030508070000002C0000546572ED5343C643626D436FEE81D4AA,' +
                    '87CD00300021F181D400240027FF0000000F01000000010023010100' +
                      '100010010144B0015242C643626D4473EBA297DD,' +
                    '87CD00300021F181D400240027FF0000000F01010000010023010100' +
                      '100010010144B1015242C643626D4473EBC1CCD5,' +
                    '87CD00300021F181D400240027FF0000000F01020000010023010100' +
                      '100010010144B2015242C643626D4473EB6421CD,' +
                    '87CD00300021F181D400240027FF0000000F01030000010023010100' +
                      '100010010144B3015242C643626D4473EB077AC5'));
                      
const
  CRC24_DEFAULT_PRESET_IDX = 6; // CRC-24/OPENPGP

{===============================================================================
    TCRC24CustomHash - class declaration
===============================================================================}
type
  TCRC24CustomHash = class(TCRC24BaseHash)
  protected
    fName:          String;
    fCRC24Poly:     TCRC24Sys;  // in reflected bit order
    fInitialValue:  TCRC24;
    fReflectIn:     Boolean;
    fReflectOut:    Boolean;
    fXOROutValue:   TCRC24;
    procedure SetCRC24Poly(Value: TCRC24Sys); virtual;
    Function GetCRC24PolyRef: TCRC24Sys; override;
    procedure SetCRC24PolyRef(Value: TCRC24Sys); virtual;
    procedure SetInitialValue(Value: TCRC24); virtual;
    procedure SetReflectIn(Value: Boolean); virtual;
    procedure SetReflectOut(Value: Boolean); virtual;
    procedure SetXOROutValue(Value: TCRC24); virtual;
    procedure BuildTable; virtual;
    procedure InitializeTable; override;
    procedure FinalizeTable; override;
    procedure Initialize; override;
  public
    Function HashName: String; reintroduce; virtual;
    constructor CreateAndInitFrom(Hash: THashBase); override;
    constructor CreateAndLoadPreset(Preset: TCRC24CustomPreset); overload;
    constructor CreateAndLoadPreset(PresetIndex: Integer); overload;
    constructor CreateAndLoadPreset(const PresetName: String); overload;
    procedure LoadPreset(Preset: TCRC24CustomPreset); overload; virtual;
    procedure LoadPreset(PresetIndex: Integer); overload; virtual;
    procedure LoadPreset(const PresetName: String); overload; virtual;
    Function SelfTest(Preset: TCRC24CustomPreset): Boolean; virtual;
    procedure Init; override;
    procedure Final; override;
    property CRC24Poly: TCRC24Sys read GetCRC24Poly write SetCRC24Poly;
    property CRC24PolyRef: TCRC24Sys read GetCRC24PolyRef write SetCRC24PolyRef;
    property InitialValue: TCRC24 read fInitialValue write SetInitialValue;
    property ReflectIn: Boolean read fReflectIn write SetReflectIn;
    property ReflectOut: Boolean read fReflectOut write SetReflectOut;
    property XOROutValue: TCRC24 read fXOROutValue write SetXOROutValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Most of the following functions are calling direct implementation, only
  functions StreamCRC24 and FileCRC24 are using instance of TCRC16Hash class
  to do the processing.  
}
{===============================================================================
    Standalone functions - declaration
===============================================================================}

Function CRC24ToStr(const CRC24: TCRC24): String;
Function StrToCRC24(const Str: String): TCRC24;
Function TryStrToCRC24(const Str: String; out CRC24: TCRC24): Boolean;
Function StrToCRC24Def(const Str: String; Default: TCRC24): TCRC24;

Function CompareCRC24(const A,B: TCRC24): Integer;
Function SameCRC24(const A,B: TCRC24): Boolean;

//------------------------------------------------------------------------------

Function BufferCRC24(const CRC24: TCRC24; const Buffer; Size: TMemSize): TCRC24; overload;

Function BufferCRC24(const Buffer; Size: TMemSize): TCRC24; overload;

Function AnsiStringCRC24(const Str: AnsiString): TCRC24;
Function WideStringCRC24(const Str: WideString): TCRC24;
Function StringCRC24(const Str: String): TCRC24;

Function StreamCRC24(Stream: TStream; Count: Int64 = -1): TCRC24;
Function FileCRC24(const FileName: String): TCRC24;

//------------------------------------------------------------------------------

type
  TCRC24Context = type TCRC24Sys;

Function CRC24_Init: TCRC24Context;
procedure CRC24_Update(var Context: TCRC24Context; const Buffer; Size: TMemSize);
Function CRC24_Final(var Context: TCRC24Context; const Buffer; Size: TMemSize): TCRC24; overload;
Function CRC24_Final(var Context: TCRC24Context): TCRC24; overload;
Function CRC24_Hash(const Buffer; Size: TMemSize): TCRC24;

implementation

uses
  SysUtils;

{===============================================================================
    Internals - implementation
===============================================================================}

Function SwapEndian(Value: TCRC24Sys): TCRC24Sys; overload;
type
  TByteOverlay = packed array[0..3] of UInt8;
begin
TByteOverlay(Result)[0] := TByteOverlay(Value)[2];
TByteOverlay(Result)[1] := TByteOverlay(Value)[1];
TByteOverlay(Result)[2] := TByteOverlay(Value)[0];
TByteOverlay(Result)[3] := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TCRC24): TCRC24; overload;
begin
Result[0] := Value[2];
Result[1] := Value[1];
Result[2] := Value[0];
end;

//------------------------------------------------------------------------------

Function ReflectBits(Value: UInt8): UInt8; overload;
const
  RevBitsTable: array[UInt8] of UInt8 = (
    $00, $80, $40, $C0, $20, $A0, $60, $E0, $10, $90, $50, $D0, $30, $B0, $70, $F0,
    $08, $88, $48, $C8, $28, $A8, $68, $E8, $18, $98, $58, $D8, $38, $B8, $78, $F8,
    $04, $84, $44, $C4, $24, $A4, $64, $E4, $14, $94, $54, $D4, $34, $B4, $74, $F4,
    $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
    $02, $82, $42, $C2, $22, $A2, $62, $E2, $12, $92, $52, $D2, $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA, $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
    $06, $86, $46, $C6, $26, $A6, $66, $E6, $16, $96, $56, $D6, $36, $B6, $76, $F6,
    $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE, $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
    $01, $81, $41, $C1, $21, $A1, $61, $E1, $11, $91, $51, $D1, $31, $B1, $71, $F1,
    $09, $89, $49, $C9, $29, $A9, $69, $E9, $19, $99, $59, $D9, $39, $B9, $79, $F9,
    $05, $85, $45, $C5, $25, $A5, $65, $E5, $15, $95, $55, $D5, $35, $B5, $75, $F5,
    $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
    $03, $83, $43, $C3, $23, $A3, $63, $E3, $13, $93, $53, $D3, $33, $B3, $73, $F3,
    $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB, $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
    $07, $87, $47, $C7, $27, $A7, $67, $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF, $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF);
begin
Result := RevBitsTable[Value];
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ReflectBits(Value: TCRC24Sys): TCRC24Sys; overload;
type
  TByteOverlay = packed array[0..3] of UInt8;
begin
// reflect only lower 24 bits
TByteOverlay(Result)[2] := ReflectBits(TByteOverlay(Value)[0]);
TByteOverlay(Result)[1] := ReflectBits(TByteOverlay(Value)[1]);
TByteOverlay(Result)[0] := ReflectBits(TByteOverlay(Value)[2]);
TByteOverlay(Result)[3] := 0;
end;

//------------------------------------------------------------------------------

Function ReflectByteBits(Value: TCRC24Sys): TCRC24Sys; overload;
type
  TByteOverlay = packed array[0..3] of UInt8;
begin
TByteOverlay(Result)[0] := ReflectBits(TByteOverlay(Value)[0]);
TByteOverlay(Result)[1] := ReflectBits(TByteOverlay(Value)[1]);
TByteOverlay(Result)[2] := ReflectBits(TByteOverlay(Value)[2]);
TByteOverlay(Result)[3] := 0;
end;

//------------------------------------------------------------------------------

Function ExpandCRC24(const Value: TCRC24): TCRC24Sys;
begin
{
  TCRC24 is always in little endian form, meaning byte 0 contains lowest byte
  of the value. And since we are placing the bytes into integral number by
  shifting, not by copying them into memory, there is no difference whether
  we are in little or big endian system.
}
Result := UInt32(Value[0]) or (UInt32(Value[1]) shl 8) or (UInt32(Value[2]) shl 16);
end;

//------------------------------------------------------------------------------

Function CollapseCRC24(const Value: TCRC24Sys): TCRC24;
begin
Result[0] := Value and $FF;
Result[1] := (Value shr 8) and $FF;
Result[2] := (Value shr 16) and $FF;
end;

//------------------------------------------------------------------------------

procedure SplitString(const Str: String; Parts: TStrings);
var
  i:          Integer;
  PartStart:  Integer;
  PartLength: Integer;
begin
Parts.Clear;
If Length(Str) > 0 then
  begin
    PartStart := 1;
    PartLength := 0;
    For i := 1 to Length(Str) do
      If Str[i] = ',' then
        begin
          Parts.Add(Trim(Copy(Str,PartStart,PartLength)));
          PartStart := Succ(i);
          PartLength := 0;
        end
      else Inc(PartLength);
    If PartLength > 0 then
      Parts.Add(Trim(Copy(Str,PartStart,PartLength)));
  end;
end;

{===============================================================================
    Main implementation
===============================================================================}

Function CRC24Process(const CRC24: TCRC24Sys; const Buffer; Size: TMemSize; CRC24TablePtr: PCRC24Table): TCRC24Sys; overload;
var
  i:    TMemSize;
  Buff: PByte;
begin
Result := CRC24;
Buff := @Buffer;
For i := 1 to Size do
  begin
    Result := CRC24TablePtr^[Byte(Result) xor Buff^] xor (Result shr 8);
    Inc(Buff);
  end;
end;

//------------------------------------------------------------------------------

Function CRC24Compare(const A,B: TCRC24Sys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function CRC24Same(const A,B: TCRC24Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function CRC24AsString(const CRC24: TCRC24Sys): String;
begin
Result := IntToHex(CRC24,6);
end;

//------------------------------------------------------------------------------

Function CRC24FromString(const Str: String): TCRC24Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TCRC24Sys(StrToInt(Str))
    else
      Result := TCRC24Sys(StrToInt('$' + Str));
  end
else Result := TCRC24Hash.CRC24ToSys(ZeroCRC24);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TCRC24BaseHash
-------------------------------------------------------------------------------                                 
===============================================================================}
{===============================================================================
    TCRC24BaseHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC24BaseHash - protected methods
-------------------------------------------------------------------------------}

Function TCRC24BaseHash.GetCRC24: TCRC24;
begin
Result := CRC24FromSys(fCRC24Value);
end;

//------------------------------------------------------------------------------

Function TCRC24BaseHash.GetCRC24Poly: TCRC24Sys;
begin
Result := ReflectBits(GetCRC24PolyRef);
end;

//------------------------------------------------------------------------------

procedure TCRC24BaseHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fCRC24Value := CRC24Process(fCRC24Value,Buffer,Size,fCRC24Table);
end;

//------------------------------------------------------------------------------

procedure TCRC24BaseHash.Initialize;
begin
inherited;
fCRC24Value := 0;
InitializeTable;
end;

//------------------------------------------------------------------------------

procedure TCRC24BaseHash.Finalize;
begin
FinalizeTable;
inherited;
end;

{-------------------------------------------------------------------------------
    TCRC24BaseHash - public methods
-------------------------------------------------------------------------------}

class Function TCRC24BaseHash.CRC24ToSys(CRC24: TCRC24): TCRC24Sys;
begin
Result := ExpandCRC24(CRC24);
end;

//------------------------------------------------------------------------------

class Function TCRC24BaseHash.CRC24FromSys(CRC24: TCRC24Sys): TCRC24;
begin
Result := CollapseCRC24(CRC24);
end;

//------------------------------------------------------------------------------

class Function TCRC24BaseHash.CRC24ToLE(CRC24: TCRC24): TCRC24;
begin
Result := CRC24;
end;

//------------------------------------------------------------------------------

class Function TCRC24BaseHash.CRC24ToBE(CRC24: TCRC24): TCRC24;
begin
Result := SwapEndian(CRC24);
end;

//------------------------------------------------------------------------------

class Function TCRC24BaseHash.CRC24FromLE(CRC24: TCRC24): TCRC24;
begin
Result := CRC24;
end;

//------------------------------------------------------------------------------

class Function TCRC24BaseHash.CRC24FromBE(CRC24: TCRC24): TCRC24;
begin
Result := SwapEndian(CRC24);
end;

//------------------------------------------------------------------------------

class Function TCRC24BaseHash.HashSize: TMemSize;
begin
Result := SizeOf(TCRC24);
end;

//------------------------------------------------------------------------------

class Function TCRC24BaseHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TCRC24BaseHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TCRC24BaseHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCRC24BaseHash then
  fCRC24Value := TCRC24BaseHash(Hash).CRC24Sys
else
  raise ECRC24IncompatibleClass.CreateFmt('TCRC24BaseHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC24BaseHash.CreateAndInitFrom(Hash: TCRC24);
begin
CreateAndInit;
fCRC24Value := CRC24ToSys(Hash);
end;

//------------------------------------------------------------------------------

Function TCRC24BaseHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TCRC24BaseHash then
  Result := CRC24Compare(fCRC24Value,TCRC24BaseHash(Hash).CRC24Sys)
else
  raise ECRC24IncompatibleClass.CreateFmt('TCRC24BaseHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCRC24BaseHash.Same(Hash: THashBase): Boolean;
begin
If Hash is TCRC24BaseHash then
  Result := CRC24Same(fCRC24Value,TCRC24BaseHash(Hash).CRC24Sys)
else
  raise ECRC24IncompatibleClass.CreateFmt('TCRC24BaseHash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCRC24BaseHash.AsString: String;
begin
Result := CRC24AsString(fCRC24Value);
end;

//------------------------------------------------------------------------------

procedure TCRC24BaseHash.FromString(const Str: String);
begin
fCRC24Value := CRC24FRomString(Str);
end;

//------------------------------------------------------------------------------

procedure TCRC24BaseHash.FromStringDef(const Str: String; const Default: TCRC24);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fCRC24Value := CRC24ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TCRC24BaseHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRC24;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}CRC24ToBE{$ELSE}CRC24ToLE{$ENDIF}(CRC24FromSys(fCRC24Value));
  heLittle: Temp := CRC24ToLE(CRC24FromSys(fCRC24Value));
  heBig:    Temp := CRC24ToBE(CRC24FromSys(fCRC24Value));
else
 {heDefault}
  Temp := CRC24FromSys(fCRC24Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TCRC24));
end;

//------------------------------------------------------------------------------

procedure TCRC24BaseHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRC24;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(TCRC24));
case Endianness of
  heSystem: fCRC24Value := CRC24ToSys({$IFDEF ENDIAN_BIG}CRC24FromBE{$ELSE}CRC24FromLE{$ENDIF}(Temp));
  heLittle: fCRC24Value := CRC24ToSys(CRC24FromLE(Temp));
  heBig:    fCRC24Value := CRC24ToSys(CRC24FromBE(Temp));
else
 {heDefault}
  fCRC24Value := CRC24ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TCRC24Hash                                   
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC24_POLYREF: TCRC24Sys = $00DF3261;

  CRC24_TABLE: TCRC24Table = (
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

//------------------------------------------------------------------------------

Function CRC24Process(const CRC24: TCRC24Sys; const Buffer; Size: TMemSize): TCRC24Sys; overload;
begin
Result := SwapEndian(CRC24Process(SwapEndian(CRC24),Buffer,Size,@CRC24_TABLE));
end;

{===============================================================================
    TCRC24Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC24Hash - protected methods
-------------------------------------------------------------------------------}

Function TCRC24Hash.GetCRC24PolyRef: TCRC24Sys;
begin
Result := CRC24_POLYREF;
end;

//------------------------------------------------------------------------------

procedure TCRC24Hash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fCRC24Value := SwapEndian(fCRC24Value);
inherited ProcessBuffer(Buffer,Size);
fCRC24Value := SwapEndian(fCRC24Value);
end;

//------------------------------------------------------------------------------

procedure TCRC24Hash.InitializeTable;
begin
fCRC24Table := @CRC24_TABLE;
end;

//------------------------------------------------------------------------------

procedure TCRC24Hash.FinalizeTable;
begin
fCRC24Table := nil;
end;

{-------------------------------------------------------------------------------
    TCRC24Hash - public methods
-------------------------------------------------------------------------------}

class Function TCRC24Hash.HashName: String;
begin
Result := 'CRC-24';
end;

//------------------------------------------------------------------------------

class Function TCRC24Hash.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

procedure TCRC24Hash.Init;
begin
inherited;
fCRC24Value := CRC24ToSys(InitialCRC24);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TCRC24CustomHash
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC24_CUSTOM_DEFNAME = 'CRC-24(custom)';

{===============================================================================
    TCRC24CustomHash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC24CustomHash - protected methods
-------------------------------------------------------------------------------}

procedure TCRC24CustomHash.SetCRC24Poly(Value: TCRC24Sys);
begin
SetCRC24PolyRef(ReflectBits(Value));
end;

//------------------------------------------------------------------------------

Function TCRC24CustomHash.GetCRC24PolyRef: TCRC24Sys;
begin
Result := fCRC24Poly;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.SetCRC24PolyRef(Value: TCRC24Sys);
begin
If fCRC24Poly <> Value then
  begin
    fName := CRC24_CUSTOM_DEFNAME;
    fCRC24Poly := Value;
    BuildTable;
    // invalidate running computations
    If fInitialized and not fFinalized then
      fInitialized := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.SetInitialValue(Value: TCRC24);
begin
If CRC24ToSys(Value) <> CRC24ToSys(fInitialValue) then
  begin
    fName := CRC24_CUSTOM_DEFNAME;
    fInitialValue := Value;
    If fInitialized and not fFinalized then
      fInitialized := False;    
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.SetReflectIn(Value: Boolean);
begin
If Value <> fReflectIn then
  begin
    fName := CRC24_CUSTOM_DEFNAME;
    fReflectIn := Value;
    BuildTable;
    If fInitialized and not fFinalized then
      fInitialized := False;    
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.SetReflectOut(Value: Boolean);
begin
If Value <> fReflectOut then
  begin
    fName := CRC24_CUSTOM_DEFNAME;
    fReflectOut := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.SetXOROutValue(Value: TCRC24);
begin
If CRC24ToSys(Value) <> CRC24ToSys(fXOROutValue) then
  begin
    fName := CRC24_CUSTOM_DEFNAME;
    fXOROutValue := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.BuildTable;
var
  i,j:  Integer;
  Temp: TCRC24Sys;
begin
For i := Low(TCRC24Table) to High(TCRC24Table) do
  begin
    If fReflectIn then
      Temp := TCRC24Sys(i) shl 1
    else
      Temp := TCRC24Sys(ReflectBits(UInt8(i))) shl 1;
    For j := 8 downto 0 do
      begin
        If (Temp and 1) <> 0 then
          Temp := (Temp shr 1) xor fCRC24Poly
        else
          Temp := Temp shr 1;
      end;
    If fReflectIn then
      fCRC24Table^[UInt8(i)] := Temp and $00FFFFFF
    else
      fCRC24Table^[UInt8(i)] := ReflectByteBits(Temp) and $00FFFFFF;;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.InitializeTable;
begin
New(fCRC24Table);
BuildTable;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.FinalizeTable;
begin
Dispose(fCRC24Table);
fCRC24Table := nil;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.Initialize;
begin
fName := CRC24_KNOWN_PRESETS[CRC24_DEFAULT_PRESET_IDX].Name;
fCRC24Poly := CRC24_KNOWN_PRESETS[CRC24_DEFAULT_PRESET_IDX].RefPolynomial;
fInitialValue := CRC24_KNOWN_PRESETS[CRC24_DEFAULT_PRESET_IDX].InitialValue;
fReflectIn := CRC24_KNOWN_PRESETS[CRC24_DEFAULT_PRESET_IDX].ReflectIn;
fReflectOut := CRC24_KNOWN_PRESETS[CRC24_DEFAULT_PRESET_IDX].ReflectOut;
fXOROutValue := CRC24_KNOWN_PRESETS[CRC24_DEFAULT_PRESET_IDX].XOROutValue;
inherited;
end;

{-------------------------------------------------------------------------------
    TCRC24CustomHash - public methods
-------------------------------------------------------------------------------}

Function TCRC24CustomHash.HashName: String;
begin
Result := fName;
end;

//------------------------------------------------------------------------------

constructor TCRC24CustomHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCRC24CustomHash then
  begin
    fName := TCRC24CustomHash(Hash).HashName;
    fCRC24Poly := TCRC24CustomHash(Hash).CRC24PolyRef;
    fInitialValue := TCRC24CustomHash(Hash).InitialValue;
    fReflectIn := TCRC24CustomHash(Hash).ReflectIn;
    fReflectOut := TCRC24CustomHash(Hash).ReflectOut;
    fXOROutValue := TCRC24CustomHash(Hash).XOROutValue;
    BuildTable;
  end
else raise ECRC24IncompatibleClass.CreateFmt('TCRC24CustomHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TCRC24CustomHash.CreateAndLoadPreset(Preset: TCRC24CustomPreset);
begin
Create;
LoadPreset(Preset);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC24CustomHash.CreateAndLoadPreset(PresetIndex: Integer);
begin
Create;
LoadPreset(PresetIndex);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC24CustomHash.CreateAndLoadPreset(const PresetName: String);
begin
Create;
LoadPreset(PresetName);
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.LoadPreset(Preset: TCRC24CustomPreset);
begin
fName := Preset.Name;
fCRC24Poly := Preset.RefPolynomial;
fInitialValue := Preset.InitialValue;
fReflectIn := Preset.ReflectIn;
fReflectOut := Preset.ReflectOut;
fXOROutValue := Preset.XOROutValue;
BuildTable;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRC24CustomHash.LoadPreset(PresetIndex: Integer);
begin
If (PresetIndex >= Low(CRC24_KNOWN_PRESETS)) and (PresetIndex <= High(CRC24_KNOWN_PRESETS)) then
  LoadPreset(CRC24_KNOWN_PRESETS[PresetIndex])
else
  raise ECRC24IndexOutOfBounds.CreateFmt('TCRC24CustomHash.LoadPreset: Index (%d) out of bounds.',[PresetIndex]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRC24CustomHash.LoadPreset(const PresetName: String);
var
  Aliases:  TStringList;
  i:        Integer;
  Index:    Integer;
begin
Aliases := TStringList.Create;
try
  Aliases.CaseSensitive := False;
  For i := Low(CRC24_KNOWN_PRESETS) to High(CRC24_KNOWN_PRESETS) do
    begin
      If not AnsiSameText(CRC24_KNOWN_PRESETS[i].Name,PresetName) then
        begin
          SplitString(CRC24_KNOWN_PRESETS[i].Aliases,Aliases);
          Index := Aliases.IndexOf(PresetName);
        end
      else Index := i;
      If Index >= 0 then
        begin
          Index := i;
          Break{For i};
        end;
    end;
  If Index >= 0 then
    LoadPreset(CRC24_KNOWN_PRESETS[Index]);
finally
  Aliases.Free;
end;
end;

//------------------------------------------------------------------------------

Function TCRC24CustomHash.SelfTest(Preset: TCRC24CustomPreset): Boolean;
var
  CodewordData: array of Byte;
  CodewordCRC:  TCRC24;

  Function DecodeCodeword(const Str: String): Boolean;
  var
    i:  TStrOff;
  begin
    Result := False;
    If Length(Str) >= (SizeOf(TCRC24) * 2) then
      begin
        CodewordData := nil;
        SetLength(CodewordData,Length(Str) div 2);
        For i := Low(CodewordData) to High(CodewordData) do
          CodewordData[i] := StrToInt('$' + Copy(Str,(i * 2) + 1,2));
        CodewordCRC := PCRC24(Addr(CodewordData[High(CodewordData) - Pred(SizeOf(TCRC24))]))^;
        If not fReflectOut then
          CodewordCRC := SwapEndian(CodewordCRC);
        Result := True;
      end;
  end;

var
  TempCRC:    TCRC24;
  TempStr:    AnsiString;
  Codewords:  TStringList;
  i:          Integer;
begin
Result := False;
LoadPreset(Preset);
HashAnsiString(AnsiString('123456789'));
If CRC24ToSys(Preset.Check) = fCRC24Value then
  begin
    // prepare check string with appended check CRC
    If fReflectOut then
      TempCRC := Preset.Check
    else
      TempCRC := SwapEndian(Preset.Check);
    TempStr := AnsiString('123456789') + AnsiString(StringOfChar('0',SizeOf(TCRC24)));
    Move(TempCRC,Addr(TempStr[10])^,SizeOf(TCRC24));
    HashAnsiString(TempStr);
    If CRC24ToSys(Preset.Residue) = (fCRC24Value xor CRC24ToSys(fXOROutValue)) then
      begin
        Codewords := TStringList.Create;
        try
          SplitString(Preset.Codewords,Codewords);
          // check codewords for crc
          For i := 0 to Pred(Codewords.Count) do
            If DecodeCodeword(Codewords[i]) then
              begin
                // check crc
                HashBuffer(CodewordData[0],Length(CodewordData) - SizeOf(TCRC24));
                If CRC24ToSys(CodewordCRC) <> fCRC24Value then
                  Exit;
                // check residue
                HashMemory(Addr(CodewordData[0]),Length(CodewordData));
                If CRC24ToSys(Preset.Residue) <> (fCRC24Value xor CRC24ToSys(fXOROutValue)) then
                  Exit;
              end;
          Result := True;
        finally
          Codewords.Free;
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.Init;
begin
inherited;
If fReflectIn then
  fCRC24Value := ReflectBits(CRC24ToSys(fInitialValue))
else
  fCRC24Value := SwapEndian(CRC24ToSys(fInitialValue));
end;

//------------------------------------------------------------------------------

procedure TCRC24CustomHash.Final;
begin
inherited;
If fReflectIn then
  begin
    If fReflectOut then
      fCRC24Value := fCRC24Value xor CRC24ToSys(fXOROutValue)
    else
      fCRC24Value := ReflectBits(fCRC24Value) xor CRC24ToSys(fXOROutValue);
  end
else
  begin
    If fReflectOut then
      fCRC24Value := ReflectByteBits(fCRC24Value) xor CRC24ToSys(fXOROutValue)
    else
      fCRC24Value := SwapEndian(fCRC24Value) xor CRC24ToSys(fXOROutValue);
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

Function CRC24ToStr(const CRC24: TCRC24): String;
begin
Result := CRC24AsString(TCRC24Hash.CRC24ToSys(CRC24));
end;

//------------------------------------------------------------------------------

Function StrToCRC24(const Str: String): TCRC24;
begin
Result := TCRC24Hash.CRC24FromSys(CRC24FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToCRC24(const Str: String; out CRC24: TCRC24): Boolean;
begin
try
  CRC24 := TCRC24Hash.CRC24FromSys(CRC24FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToCRC24Def(const Str: String; Default: TCRC24): TCRC24;
begin
If not TryStrToCRC24(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareCRC24(const A,B: TCRC24): Integer;
begin
Result := CRC24Compare(TCRC24Hash.CRC24ToSys(A),TCRC24Hash.CRC24ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameCRC24(const A,B: TCRC24): Boolean;
begin
Result := CRC24Same(TCRC24Hash.CRC24ToSys(A),TCRC24Hash.CRC24ToSys(B));
end;

{-------------------------------------------------------------------------------
    Standalone functions - processing functions
-------------------------------------------------------------------------------}

Function BufferCRC24(const CRC24: TCRC24; const Buffer; Size: TMemSize): TCRC24;
begin
Result := TCRC24Hash.CRC24FromSys(CRC24Process(TCRC24Hash.CRC24ToSys(CRC24),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function BufferCRC24(const Buffer; Size: TMemSize): TCRC24;
begin
Result := TCRC24Hash.CRC24FromSys(CRC24Process(TCRC24Hash.CRC24ToSys(InitialCRC24),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringCRC24(const Str: AnsiString): TCRC24;
begin
Result := BufferCRC24(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringCRC24(const Str: WideString): TCRC24;
begin
Result := BufferCRC24(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringCRC24(const Str: String): TCRC24;
begin
Result := BufferCRC24(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamCRC24(Stream: TStream; Count: Int64 = -1): TCRC24;
var
  Hash: TCRC24Hash;
begin
Hash := TCRC24Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.CRC24;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileCRC24(const FileName: String): TCRC24;
var
  Hash: TCRC24Hash;
begin
Hash := TCRC24Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.CRC24;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Standalone functions - context functions
-------------------------------------------------------------------------------}

Function CRC24_Init: TCRC24Context;
begin
Result := TCRC24Context(TCRC24Hash.CRC24ToSys(InitialCRC24));
end;


//------------------------------------------------------------------------------

procedure CRC24_Update(var Context: TCRC24Context; const Buffer; Size: TMemSize);
begin
TCRC24Sys(Context) := CRC24Process(TCRC24Sys(Context),Buffer,Size);
end;

//------------------------------------------------------------------------------

Function CRC24_Final(var Context: TCRC24Context; const Buffer; Size: TMemSize): TCRC24;
begin
CRC24_Update(Context,Buffer,Size);
Result := CRC24_Final(Context);
end;

//------------------------------------------------------------------------------

Function CRC24_Final(var Context: TCRC24Context): TCRC24;
begin
Result := TCRC24Hash.CRC24FromSys(TCRC24Sys(Context));
Context := TCRC24Context(TCRC24Hash.CRC24ToSys(ZeroCRC24));
end;

//------------------------------------------------------------------------------

Function CRC24_Hash(const Buffer; Size: TMemSize): TCRC24;
begin
Result := BufferCRC24(Buffer,Size);
end;

end.
