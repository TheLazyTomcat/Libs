{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CRC-8 calculation

    Small library providing means of calculating 8-bit cyclic redundancy check
    (CRC-8) for almost any data (buffers, strings, streams, files, ...).

    It can be used either in form of objects (eg. class TCRC8Hash) or in pure
    procedural form.

    If the imlemented CRC does not suit your needs, you can create an instance
    of class TCRC8CustomHash, provide it with your own parameters (polynomial,
    initial value, ...) and use its methods for computations.
    There is also an array containing a selection of 20 known CRC-8 variants.
    These can be used to initialize the custom hash object with respective
    settings.

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

      github.com/TheLazyTomcat/Lib.CRC8

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
unit CRC8;

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
  ECRC8Exception = class(EHashException);

  ECRC8IncompatibleClass = class(ECRC8Exception);
  ECRC8IndexOutOfBounds  = class(ECRC8Exception);

{===============================================================================
    Common types and constants
===============================================================================}
type
{
  No need to declare separate TCRC8Sys as single-byte quentity is not affected
  by system byte order.
}
  TCRC8 = UInt8;
  PCRC8 = ^TCRC8;

  TCRC8Table = array[UInt8] of TCRC8;
  PCRC8Table = ^TCRC8Table;

const
{
  Initial value of CRC-8 for use in standalone functions, do not use it
  anywhere else.
}
  InitialCRC8 = $00;
  ZeroCRC8    = 0;  

{===============================================================================
--------------------------------------------------------------------------------
                                  TCRC8BaseHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCRC8BaseHash - class declaration
===============================================================================}
type
  TCRC8BaseHash = class(TStreamHash)
  protected
    fCRC8Value: TCRC8;
    fCRC8Table: PCRC8Table;
    Function GetCRC8Poly: TCRC8; virtual;
    Function GetCRC8PolyRef: TCRC8; virtual; abstract;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure InitializeTable; virtual; abstract;
    procedure FinalizeTable; virtual; abstract;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class Function HashSize: TMemSize; override;
    // endianness is meaningless for CRC-8, nevertheless heLittle is returned
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TCRC8); overload; virtual;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TCRC8); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property CRC8: TCRC8 read fCRC8Value;
    property CRC8Poly: TCRC8 read GetCRC8Poly;        // polynomial
    property CRC8PolyRef: TCRC8 read GetCRC8PolyRef;  // polynomial with reflected bit order
    property CRC8Table: PCRC8Table read fCRC8Table;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                    TCRC8Hash                                    
--------------------------------------------------------------------------------
===============================================================================}
{
  Hardcoded implementation of CRC-8/SMBUS, meaning the algorithm is run with
  following parameters:

                  polynomial        0x107
               initial value        0x00000000
             final xor value        0x00000000
       input bits reflection        False
      output bits reflection        False
}
{===============================================================================
    TCRC8Hash - class declaration
===============================================================================}
type
  TCRC8Hash = class(TCRC8BaseHash)
  protected
    Function GetCRC8PolyRef: TCRC8; override;
    procedure InitializeTable; override;
    procedure FinalizeTable; override;
  public
    class Function HashName: String; override;
    class Function HashFinalization: Boolean; override;
    procedure Init; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TCRC8CustomHash
--------------------------------------------------------------------------------
===============================================================================}
type
  TCRC8CustomPreset = record
    Name:           String;     // name assigned to this CRC-8 within this library
    Aliases:        String;     // name aliasses, separated by comma (,)
    Polynomial:     TCRC8;      // polynomial with highest bit omitted
    RefPolynomial:  TCRC8;      // polynomial with reflected bit order and original highest bit omitted
    FullPolynomial: UInt16;     // full polynomial (only lower 9bits are to be observed)
    InitialValue:   TCRC8;      // initial value of CRC register
    ReflectIn:      Boolean;    // order in which bits within input bytes are processed (true = LSB, false = MSB)
    ReflectOut:     Boolean;    // resulting CRC-8 is bit-swapped before presentation
    XOROutValue:    TCRC8;      // value XORed to the register after all processing is done
    Check:          TCRC8;      // CRC-8 of UTF-8 (really ASCII) encoded string "123456789" (without quotes)
    Residue:        TCRC8;      // what is left in CRC-8 register (before final xor) after hashing of error-free data with appended CRC-8 value
    Codewords:      String;     // several comma-separated (,) datastream-crc pairs (binary, hexadecimal notation)
  end;

//------------------------------------------------------------------------------  
const
  CRC8_KNOWN_PRESETS: array[0..19] of TCRC8CustomPreset = (
   (Name:           'CRC-8/AUTOSAR';
    Aliases:        '';
    Polynomial:     $2F;
    RefPolynomial:  $F4;
    FullPolynomial: $12F;
    InitialValue:   $FF;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $FF;
    Check:          $DF;
    Residue:        $42;
    Codewords:      '0000000012,' +
                    'F20183C2,' +
                    '0FAA0055C6,' +
                    '00FF551177,' +
                    '332255AABBCCDDEEFF11,' +
                    '926B5533,' +
                    'FFFFFFFF6C'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/BLUETOOTH';
    Aliases:        '';
    Polynomial:     $A7;
    RefPolynomial:  $E5;
    FullPolynomial: $1A7;
    InitialValue:   $00;
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    $00;
    Check:          $26;
    Residue:        $00;
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/CDMA2000';
    Aliases:        '';
    Polynomial:     $9B;
    RefPolynomial:  $D9;
    FullPolynomial: $19B;
    InitialValue:   $FF;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $DA;
    Residue:        $00;
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/DARC';
    Aliases:        '';
    Polynomial:     $39;
    RefPolynomial:  $9C;
    FullPolynomial: $139;
    InitialValue:   $00;
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    $00;
    Check:          $15;
    Residue:        $00;
    Codewords:      '80C0EB'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/DVB-S2';
    Aliases:        '';
    Polynomial:     $D5;
    RefPolynomial:  $AB;
    FullPolynomial: $1D5;
    InitialValue:   $00;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $BC;
    Residue:        $00;
    Codewords:      '22C812563011223344556677884F'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/GSM-A';
    Aliases:        '';
    Polynomial:     $1D;
    RefPolynomial:  $B8;
    FullPolynomial: $11D;
    InitialValue:   $00;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $37;
    Residue:        $00;
    Codewords:      '§0D6'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/GSM-B';
    Aliases:        '';
    Polynomial:     $49;
    RefPolynomial:  $92;
    FullPolynomial: $149;
    InitialValue:   $00;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $FF;
    Check:          $94;
    Residue:        $53;
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/HITAG';
    Aliases:        '';
    Polynomial:     $1D;
    RefPolynomial:  $B8;
    FullPolynomial: $11D;
    InitialValue:   $FF;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $B4;
    Residue:        $00;
    Codewords:      'CA9340FFC6'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/I-432-1';
    Aliases:        'CRC-8/ITU';
    Polynomial:     $07;
    RefPolynomial:  $E0;
    FullPolynomial: $107;
    InitialValue:   $00;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $55;
    Check:          $A1;
    Residue:        $AC;
    Codewords:      '0000000055,' +
                    '0000000152'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/I-CODE';
    Aliases:        '';
    Polynomial:     $1D;
    RefPolynomial:  $B8;
    FullPolynomial: $11D;
    InitialValue:   $FD;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $7E;
    Residue:        $00;
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/LTE';
    Aliases:        '';
    Polynomial:     $9B;
    RefPolynomial:  $D9;
    FullPolynomial: $19B;
    InitialValue:   $00;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $EA;
    Residue:        $00;
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/MAXIM-DOW';
    Aliases:        'CRC-8/MAXIM,DOW-CRC';
    Polynomial:     $31;
    RefPolynomial:  $8C;
    FullPolynomial: $131;
    InitialValue:   $00;
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    $00;
    Check:          $A1;
    Residue:        $00;
    Codewords:      '242BC5FB00000040,' +
                    '102BC5FB000000A0,' +
                    '212BC5FB00000089,' +
                    '142BC5FB00000054,' +
                    '14B3D8FB000000D4,' +
                    '232BC5FB000000F3,' +
                    '23B3D8FB00000073,' +
                    '092BC5FB00000097,' +
                    '09B3D8FB00000017,' +
                    '0B2BC5FB000000ED,' +
                    '0BB3D8FB0000006D,' +
                    '0F2BC5FB00000019,' +
                    '0FB3D8FB00000099,' +
                    '0A2BC5FB000000D0,' +
                    '0C2BC5FB0000005E'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/MIFARE-MAD';
    Aliases:        '';
    Polynomial:     $1D;
    RefPolynomial:  $B8;
    FullPolynomial: $11D;
    InitialValue:   $C7;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $99;
    Residue:        $00;
    Codewords:      '0101080108010800000000000004000310031002100210000000000000113089,' +
                    '0103E103E103E103E103E10000000000000000000000000000000000000000C4,' +
                    '0103E103E103E103E103E103E1000000000000000000000000000000000000AB,' +
                    '00563D563C563C563C563C563C00000000000000000000000000000000000030'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/NRSC-5';
    Aliases:        '';
    Polynomial:     $31;
    RefPolynomial:  $8C;
    FullPolynomial: $131;
    InitialValue:   $FF;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $F7;
    Residue:        $00;
    Codewords:      '5822EF639D240114F25D007CDA4208A1CBBDE9DFD73273A7D180003038EB,' +
                    'B002F6E01BF5E00BE3F6D05450D23B70273990D90ED77AEDF6EB803C1AD30A685E0A009A,' +
                      '412C565998C4938214C58005800B09CABACB98B656DE98FB199A18923DE06D592D2D399A' +
                      '5CA72CFAD96BC10735E4774FA06AFEAB1726EF0F471ABED704DA798DEDABB16D1A4F0D7B' +
                      '7C49F65C1EBE2D5DE6EB16D8BF2DB0199F76D6C8455C1C70251B4290CA669C00AC529FC0' +
                      '014002174006E6860558EEF5CBE4DEF994D1F642156E48FA68072E78A7F1C88A80DF0251' +
                      '6B5672FFF281780B6200A3,' +
                    '896C608852750D2DC92490555A448BA655686F62F35A2F7522C62EB13161881468600683310689,' +
                    '412C570068CB2362DCCB2400004416B42A5F9B2C469A721D106258A76D77C2F105C315DA' +
                      '1257CAD1232A47282475FB250171BCC094DE3C5711A0B68892F7A5685CAB97881BFA8020' +
                      '56600688A08815EAD3C069B68A952C00058BD0071DFD5F975D1CFD976FAE0A82A1AF1636' +
                      'E4687EE472C62F22A96E3197B88943852B128D0489E911B85B84023122176D0670130348' +
                      'DDD554416BB8852A854364,' +
                    '0D0640C6D800124B,' +
                    '89866AD812A05E8034A3BBBAAAB56AD4FAD9894B0030000000007752'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/OPENSAFETY';
    Aliases:        '';
    Polynomial:     $2F;
    RefPolynomial:  $F4;
    FullPolynomial: $12F;
    InitialValue:   $00;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $3E;
    Residue:        $00;
    Codewords:      '23C8083411223344556677883C' +
                    '02A806000060650006A11C,' +
                    '03A80001000060650006A131,' +
                    '03A806000060650007211D,' +
                    '02A80001000060650007211D,' +
                    '04A8060000606500021393,' +
                    '05A800010000606500021350,' +
                    '01AC0600006065000721AF,' +
                    '00AC000300006065000721B5'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/ROHC';
    Aliases:        '';
    Polynomial:     $07;
    RefPolynomial:  $E0;
    FullPolynomial: $107;
    InitialValue:   $FF;
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    $00;
    Check:          $D0;
    Residue:        $00;
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/SAE-J1850';
    Aliases:        '';
    Polynomial:     $1D;
    RefPolynomial:  $B8;
    FullPolynomial: $11D;
    InitialValue:   $FF;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $FF;
    Check:          $4B;
    Residue:        $C4;
    Codewords:      '0000000059,' +
                    'F2018337,' +
                    '0FAA005579,' +
                    '00FF5511B8,' +
                    '332255AABBCCDDEEFFCB,' +
                    '926B558C,' +
                    'FFFFFFFF74,' +
                    '55FF0000ECFF601F,' +
                    '55FF0000F0FFA038,' +
                    '660BEAFFBFFFC0CA,' +
                    '5E18EAFFB7FF60BD,' +
                    'F6301600FCFE1081'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/SMBUS';
    Aliases:        'CRC-8';
    Polynomial:     $07;
    RefPolynomial:  $E0;
    FullPolynomial: $107;
    InitialValue:   $00;
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    $00;
    Check:          $F4;
    Residue:        $00;
    Codewords:      '§0E3'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-8/TECH-3250';
    Aliases:        'CRC-8/AES,CRC-8/EBU';
    Polynomial:     $1D;
    RefPolynomial:  $B8;
    FullPolynomial: $11D;
    InitialValue:   $FF;
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    $00;
    Check:          $97;
    Residue:        $00;
    Codewords:      '3D020000020000000000000000000000000000000000009B,' +
                    '010000000000000000000000000000000000000000000032'),
    // - - - - - - - - - - - - - - - -    
   (Name:           'CRC-8/WCDMA';
    Aliases:        '';
    Polynomial:     $9B;
    RefPolynomial:  $D9;
    FullPolynomial: $19B;
    InitialValue:   $00;
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    $00;
    Check:          $25;
    Residue:        $00;
    Codewords:      '00001148,' +
                    '00012077,' +
                    '80800012,' +
                    '00001021010022000A002D000000000000460A20,' +
                    '00001021010123000A0028000000000000460A54,' +
                    '00001021010125000A0029000000000000460A4B,' +
                    '400010200B01001A000A000100000000008009001F01F393,' +
                    '400010200B01001F000A000100000000000009001F01F3F7,' +
                    '400010200B010020000A000100000000000009001F01F331,' +
                    '40001107083829B41903,' +
                    '40001107084826682180,' +
                    '40001107085C293417A8'));

const
  CRC8_DEFAULT_PRESET_IDX = 17; // CRC-8/SMBUS

{===============================================================================
    TCRC8CustomHash - class declaration
===============================================================================}
type
  TCRC8CustomHash = class(TCRC8BaseHash)
  protected
    fName:          String;
    fCRC8Poly:      TCRC8;  // in reflected bit order
    fInitialValue:  TCRC8;
    fReflectIn:     Boolean;
    fReflectOut:    Boolean;
    fXOROutValue:   TCRC8;
    procedure SetCRC8Poly(Value: TCRC8); virtual;
    Function GetCRC8PolyRef: TCRC8; override;
    procedure SetCRC8PolyRef(Value: TCRC8); virtual;
    procedure SetInitialValue(Value: TCRC8); virtual;
    procedure SetReflectIn(Value: Boolean); virtual;
    procedure SetReflectOut(Value: Boolean); virtual;
    procedure SetXOROutValue(Value: TCRC8); virtual;
    procedure BuildTable; virtual;
    procedure InitializeTable; override;
    procedure FinalizeTable; override;
    procedure Initialize; override;
  public
    Function HashName: String; reintroduce; virtual;
    constructor CreateAndInitFrom(Hash: THashBase); override;
    constructor CreateAndLoadPreset(Preset: TCRC8CustomPreset); overload;
    constructor CreateAndLoadPreset(PresetIndex: Integer); overload;
    constructor CreateAndLoadPreset(const PresetName: String); overload;
    procedure LoadPreset(Preset: TCRC8CustomPreset); overload; virtual;
    procedure LoadPreset(PresetIndex: Integer); overload; virtual;
    procedure LoadPreset(const PresetName: String); overload; virtual;
    Function SelfTest(Preset: TCRC8CustomPreset): Boolean; virtual;
    procedure Init; override;
    procedure Final; override;
    property CRC8Poly: TCRC8 read GetCRC8Poly write SetCRC8Poly;
    property CRC8PolyRef: TCRC8 read GetCRC8PolyRef write SetCRC8PolyRef;
    property InitialValue: TCRC8 read fInitialValue write SetInitialValue;
    property ReflectIn: Boolean read fReflectIn write SetReflectIn;
    property ReflectOut: Boolean read fReflectOut write SetReflectOut;
    property XOROutValue: TCRC8 read fXOROutValue write SetXOROutValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Most of the following functions are calling direct implementation, only
  functions StreamCRC8 and FileCRC8 are using instance of TCRC8Hash class
  to do the work.
}
{===============================================================================
    Standalone functions - declaration
===============================================================================}

Function CRC8ToStr(const CRC8: TCRC8): String;
Function StrToCRC8(const Str: String): TCRC8;
Function TryStrToCRC8(const Str: String; out CRC8: TCRC8): Boolean;
Function StrToCRC8Def(const Str: String; Default: TCRC8): TCRC8;

Function CompareCRC8(const A,B: TCRC8): Integer;
Function SameCRC8(const A,B: TCRC8): Boolean;

//------------------------------------------------------------------------------

Function BufferCRC8(const CRC8: TCRC8; const Buffer; Size: TMemSize): TCRC8; overload;

Function BufferCRC8(const Buffer; Size: TMemSize): TCRC8; overload;

Function AnsiStringCRC8(const Str: AnsiString): TCRC8;
Function WideStringCRC8(const Str: WideString): TCRC8;
Function StringCRC8(const Str: String): TCRC8;

Function StreamCRC8(Stream: TStream; Count: Int64 = -1): TCRC8;
Function FileCRC8(const FileName: String): TCRC8;

//------------------------------------------------------------------------------

type
  TCRC8Context = type TCRC8;

Function CRC8_Init: TCRC8Context;
procedure CRC8_Update(var Context: TCRC8Context; const Buffer; Size: TMemSize);
Function CRC8_Final(var Context: TCRC8Context; const Buffer; Size: TMemSize): TCRC8; overload;
Function CRC8_Final(var Context: TCRC8Context): TCRC8; overload;
Function CRC8_Hash(const Buffer; Size: TMemSize): TCRC8;

implementation

uses
  SysUtils;

{===============================================================================
    Internals implementation
===============================================================================}

Function ConsumeArgs(const Args: array of const): Integer;
begin
Result := Length(Args);
end;

//------------------------------------------------------------------------------

Function ReflectBits(Value: UInt8): UInt8;
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

Function CRC8Process(const CRC8: TCRC8; const Buffer; Size: TMemSize; CRC8TablePtr: PCRC8Table): TCRC8;
var
  CurrentData:  PUInt8;
  i:            TMemSize;
begin
Result := CRC8;
CurrentData := @Buffer;
For i := 1 to Size do
  begin
    Result := CRC8TablePtr^[Result xor CurrentData^];
    Inc(CurrentData);
  end;
end;

//------------------------------------------------------------------------------

Function CRC8Compare(const A,B: TCRC8): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function CRC8Same(const A,B: TCRC8): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function CRC8AsString(const CRC8: TCRC8): String;
begin
Result := IntToHex(CRC8,2);
end;

//------------------------------------------------------------------------------

Function CRC8FromString(const Str: String): TCRC8;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TCRC8(StrToInt(Str))
    else
      Result := TCRC8(StrToInt('$' + Str));
  end
else Result := ZeroCRC8;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TCRC8BaseHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCRC8BaseHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC8BaseHash - protected methods
-------------------------------------------------------------------------------}

Function TCRC8BaseHash.GetCRC8Poly: TCRC8;
begin
Result := TCRC8(ReflectBits(UInt8(GetCRC8PolyRef)));
end;

//------------------------------------------------------------------------------

procedure TCRC8BaseHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fCRC8Value := CRC8Process(fCRC8Value,Buffer,Size,fCRC8Table);
end;

//------------------------------------------------------------------------------

procedure TCRC8BaseHash.Initialize;
begin
inherited;
fCRC8Value := 0;
InitializeTable;
end;

//------------------------------------------------------------------------------

procedure TCRC8BaseHash.Finalize;
begin
FinalizeTable;
inherited;
end;

{-------------------------------------------------------------------------------
    TCRC8BaseHash - public methods
-------------------------------------------------------------------------------}

class Function TCRC8BaseHash.HashSize: TMemSize;
begin
Result := SizeOf(TCRC8);
end;

//------------------------------------------------------------------------------

class Function TCRC8BaseHash.HashEndianness: THashEndianness;
begin
{
  Endianness is meaningless for single-byte values, but we have to provide
  something here.
}
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TCRC8BaseHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TCRC8BaseHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCRC8BaseHash then
  fCRC8Value := TCRC8BaseHash(Hash).CRC8
else
  raise ECRC8IncompatibleClass.CreateFmt('TCRC8BaseHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TCRC8BaseHash.CreateAndInitFrom(Hash: TCRC8);
begin
CreateAndInit;
fCRC8Value := Hash;
end;

//------------------------------------------------------------------------------

Function TCRC8BaseHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TCRC8BaseHash then
  Result := CRC8Compare(fCRC8Value,TCRC8BaseHash(Hash).CRC8)
else
  raise ECRC8IncompatibleClass.CreateFmt('TCRC8BaseHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCRC8BaseHash.Same(Hash: THashBase): Boolean;
begin
If Hash is TCRC8BaseHash then
  Result := CRC8Same(fCRC8Value,TCRC8BaseHash(Hash).CRC8)
else
  raise ECRC8IncompatibleClass.CreateFmt('TCRC8BaseHash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCRC8BaseHash.AsString: String;
begin
Result := CRC8AsString(fCRC8Value);
end;

//------------------------------------------------------------------------------

procedure TCRC8BaseHash.FromString(const Str: String);
begin
fCRC8Value := CRC8FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TCRC8BaseHash.FromStringDef(const Str: String; const Default: TCRC8);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fCRC8Value := Default;
end;

//------------------------------------------------------------------------------

procedure TCRC8BaseHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRC8;
begin
ConsumeArgs([Ord(Endianness)]);
Temp := fCRC8Value;
Stream.WriteBuffer(Temp,SizeOf(TCRC8));
end;

//------------------------------------------------------------------------------

procedure TCRC8BaseHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRC8;
begin
ConsumeArgs([Ord(Endianness)]);
Stream.ReadBuffer(Addr(Temp)^,SizeOf(TCRC8));
fCRC8Value := Temp;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TCRC8Hash                                    
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC8_POLYREF: TCRC8 = $E0;

  CRC8_TABLE: TCRC8Table = (
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
    TCRC8Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC8Hash - protected methods
-------------------------------------------------------------------------------}

Function TCRC8Hash.GetCRC8PolyRef: TCRC8;
begin
Result := CRC8_POLYREF;
end;

//------------------------------------------------------------------------------

procedure TCRC8Hash.InitializeTable;
begin
fCRC8Table := @CRC8_TABLE;
end;

//------------------------------------------------------------------------------

procedure TCRC8Hash.FinalizeTable;
begin
fCRC8Table := nil;
end;

{-------------------------------------------------------------------------------
    TCRC8Hash - public methods
-------------------------------------------------------------------------------}

class Function TCRC8Hash.HashName: String;
begin
Result := 'CRC-8';
end;

//------------------------------------------------------------------------------

class Function TCRC8Hash.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

procedure TCRC8Hash.Init;
begin
inherited;
fCRC8Value := InitialCRC8;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TCRC8CustomHash
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC8_CUSTOM_DEFNAME = 'CRC-8(custom)';

{===============================================================================
    TCRC8CustomHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC8CustomHash - protected methods
-------------------------------------------------------------------------------}

procedure TCRC8CustomHash.SetCRC8Poly(Value: TCRC8);
begin
SetCRC8PolyRef(ReflectBits(Value));
end;

//------------------------------------------------------------------------------

Function TCRC8CustomHash.GetCRC8PolyRef: TCRC8;
begin
Result := fCRC8Poly;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.SetCRC8PolyRef(Value: TCRC8);
begin
If fCRC8Poly <> Value then
  begin
    fName := CRC8_CUSTOM_DEFNAME;
    fCRC8Poly := Value;
    BuildTable;
    // invalidate running computations
    If fInitialized and not fFinalized then
      fInitialized := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.SetInitialValue(Value: TCRC8);
begin
If Value <> fInitialValue then
  begin
    fName := CRC8_CUSTOM_DEFNAME;
    fInitialValue := Value;
    If fInitialized and not fFinalized then
      fInitialized := False;    
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.SetReflectIn(Value: Boolean);
begin
If Value <> fReflectIn then
  begin
    fName := CRC8_CUSTOM_DEFNAME;
    fReflectIn := Value;
    BuildTable;
    If fInitialized and not fFinalized then
      fInitialized := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.SetReflectOut(Value: Boolean);
begin
If Value <> fReflectOut then
  begin
    // no need to rebuild table or cancel processing
    fName := CRC8_CUSTOM_DEFNAME;
    fReflectOut := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.SetXOROutValue(Value: TCRC8);
begin
If Value <> fXOROutValue then
  begin
    fName := CRC8_CUSTOM_DEFNAME;
    fXOROutValue := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.BuildTable;
var
  i,j:  Integer;
  Temp: UInt16;
begin
For i := Low(TCRC8Table) to High(TCRC8Table) do
  begin
    If fReflectIn then
      Temp := UInt16(i) shl 1
    else
      Temp := UInt16(ReflectBits(UInt8(i))) shl 1;
    For j := 8 downto 0 do
      begin
        If (Temp and 1) <> 0 then
          Temp := (Temp shr 1) xor UInt16(fCRC8Poly)
        else
          Temp := Temp shr 1;
      end;
    If fReflectIn then
      fCRC8Table^[UInt8(i)] := TCRC8(Temp)
    else
      fCRC8Table^[UInt8(i)] := TCRC8(ReflectBits(UInt8(Temp)));
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.InitializeTable;
begin
New(fCRC8Table);
BuildTable;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.FinalizeTable;
begin
Dispose(fCRC8Table);
fCRC8Table := nil;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.Initialize;
begin
fName := CRC8_KNOWN_PRESETS[CRC8_DEFAULT_PRESET_IDX].Name;
fCRC8Poly := CRC8_KNOWN_PRESETS[CRC8_DEFAULT_PRESET_IDX].RefPolynomial;
fInitialValue := CRC8_KNOWN_PRESETS[CRC8_DEFAULT_PRESET_IDX].InitialValue;
fReflectIn := CRC8_KNOWN_PRESETS[CRC8_DEFAULT_PRESET_IDX].ReflectIn;
fReflectOut := CRC8_KNOWN_PRESETS[CRC8_DEFAULT_PRESET_IDX].ReflectOut;
fXOROutValue := CRC8_KNOWN_PRESETS[CRC8_DEFAULT_PRESET_IDX].XOROutValue;
inherited;
end;

{-------------------------------------------------------------------------------
    TCRC8CustomHash - public methods
-------------------------------------------------------------------------------}

Function TCRC8CustomHash.HashName: String;
begin
Result := fName;
end;

//------------------------------------------------------------------------------

constructor TCRC8CustomHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCRC8CustomHash then
  begin
    fName := TCRC8CustomHash(Hash).HashName;
    fCRC8Poly := TCRC8CustomHash(Hash).CRC8PolyRef;
    fInitialValue := TCRC8CustomHash(Hash).InitialValue;
    fReflectIn := TCRC8CustomHash(Hash).ReflectIn;
    fReflectOut := TCRC8CustomHash(Hash).ReflectOut;
    fXOROutValue := TCRC8CustomHash(Hash).XOROutValue;
    BuildTable;
  end
else raise ECRC8IncompatibleClass.CreateFmt('TCRC8CustomHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TCRC8CustomHash.CreateAndLoadPreset(Preset: TCRC8CustomPreset);
begin
Create;
LoadPreset(Preset);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC8CustomHash.CreateAndLoadPreset(PresetIndex: Integer);
begin
Create;
LoadPreset(PresetIndex);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC8CustomHash.CreateAndLoadPreset(const PresetName: String);
begin
Create;
LoadPreset(PresetName);
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.LoadPreset(Preset: TCRC8CustomPreset);
begin
fName := Preset.Name;
fCRC8Poly := Preset.RefPolynomial;
fInitialValue := Preset.InitialValue;
fReflectIn := Preset.ReflectIn;
fReflectOut := Preset.ReflectOut;
fXOROutValue := Preset.XOROutValue;
BuildTable;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRC8CustomHash.LoadPreset(PresetIndex: Integer);
begin
If (PresetIndex >= Low(CRC8_KNOWN_PRESETS)) and (PresetIndex <= High(CRC8_KNOWN_PRESETS)) then
  LoadPreset(CRC8_KNOWN_PRESETS[PresetIndex])
else
  raise ECRC8IndexOutOfBounds.CreateFmt('TCRC8CustomHash.LoadPreset: Index (%d) out of bounds.',[PresetIndex]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRC8CustomHash.LoadPreset(const PresetName: String);
var
  Aliases:  TStringList;
  i:        Integer;
  Index:    Integer;
begin
Aliases := TStringList.Create;
try
  Aliases.CaseSensitive := False;
  For i := Low(CRC8_KNOWN_PRESETS) to High(CRC8_KNOWN_PRESETS) do
    begin
      If not AnsiSameText(CRC8_KNOWN_PRESETS[i].Name,PresetName) then
        begin
          SplitString(CRC8_KNOWN_PRESETS[i].Aliases,Aliases);
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
    LoadPreset(CRC8_KNOWN_PRESETS[Index]);
finally
  Aliases.Free;
end;
end;

//------------------------------------------------------------------------------

Function TCRC8CustomHash.SelfTest(Preset: TCRC8CustomPreset): Boolean;
var
  CodewordData: array of Byte;
  CodewordCRC:  TCRC8;

  Function DecodeCodeword(const Str: String): Boolean;
  var
    i:  TStrOff;
  begin
    If Length(Str) >= 2 then
      begin
        CodewordData := nil;
        If (Length(Str) = 4) and AnsiSameStr(Copy(Str,1,2),'§0') then
          begin
            SetLength(CodewordData,2000);
            For i := Low(CodewordData) to Pred(High(CodewordData)) do
              CodewordData[i] := Byte(i + i * i);
            CodewordCRC := StrToInt('$' + Copy(Str,Length(Str) - 1,2));
            CodewordData[High(CodewordData)] := CodewordCRC;
          end
        else
          begin
            SetLength(CodewordData,Length(Str) div 2);
            For i := Low(CodewordData) to High(CodewordData) do
              CodewordData[i] := StrToInt('$' + Copy(Str,(i * 2) + 1,2));
            CodewordCRC := TCRC8(CodewordData[High(CodewordData)]);
          end;
        Result := True;
      end
    else Result := False;
  end;

var
  Codewords:  TStringList;
  i:          Integer;
begin
Result := False;
LoadPreset(Preset);
HashAnsiString(AnsiString('123456789'));
If Preset.Check = fCRC8Value then
  begin
    HashAnsiString(AnsiString('123456789') + AnsiChar(Preset.Check));
    If Preset.Residue = (fCRC8Value xor fXOROutValue) then
      begin
        Codewords := TStringList.Create;
        try
          SplitString(Preset.Codewords,Codewords);
          // check codewords for crc
          For i := 0 to Pred(Codewords.Count) do
            If DecodeCodeword(Codewords[i]) then
              begin
                // check crc
                HashBuffer(CodewordData[0],Length(CodewordData) - 1);
                If CodewordCRC <> fCRC8Value then
                   Exit;
                // check residue
                HashMemory(Addr(CodewordData[0]),Length(CodewordData));
                If Preset.Residue <> (fCRC8Value xor fXOROutValue) then
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

procedure TCRC8CustomHash.Init;
begin
inherited;
If fReflectIn then
  fCRC8Value := ReflectBits(fInitialValue)
else
  fCRC8Value := fInitialValue;
end;

//------------------------------------------------------------------------------

procedure TCRC8CustomHash.Final;
begin
inherited;
If fReflectIn xor fReflectOut then
  fCRC8Value := TCRC8(ReflectBits(UInt8(fCRC8Value))) xor fXOROutValue
else
  fCRC8Value := fCRC8Value xor fXOROutValue;
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

Function CRC8ToStr(const CRC8: TCRC8): String;
begin
Result := CRC8AsString(CRC8);
end;

//------------------------------------------------------------------------------

Function StrToCRC8(const Str: String): TCRC8;
begin
Result := CRC8FromString(Str);
end;

//------------------------------------------------------------------------------

Function TryStrToCRC8(const Str: String; out CRC8: TCRC8): Boolean;
begin
try
  CRC8 := CRC8FromString(Str);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToCRC8Def(const Str: String; Default: TCRC8): TCRC8;
begin
If not TryStrToCRC8(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareCRC8(const A,B: TCRC8): Integer;
begin
Result := CRC8Compare(A,B);
end;

//------------------------------------------------------------------------------

Function SameCRC8(const A,B: TCRC8): Boolean;
begin
Result := CRC8Same(A,B);
end;

{-------------------------------------------------------------------------------
    Standalone functions - processing functions
-------------------------------------------------------------------------------}

Function BufferCRC8(const CRC8: TCRC8; const Buffer; Size: TMemSize): TCRC8;
begin
Result := CRC8Process(CRC8,Buffer,Size,@CRC8_TABLE);
end;

//------------------------------------------------------------------------------

Function BufferCRC8(const Buffer; Size: TMemSize): TCRC8;
begin
Result := CRC8Process(InitialCRC8,Buffer,Size,@CRC8_TABLE);
end;

//------------------------------------------------------------------------------

Function AnsiStringCRC8(const Str: AnsiString): TCRC8;
begin
Result := BufferCRC8(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringCRC8(const Str: WideString): TCRC8;
begin
Result := BufferCRC8(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringCRC8(const Str: String): TCRC8;
begin
Result := BufferCRC8(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamCRC8(Stream: TStream; Count: Int64 = -1): TCRC8;
var
  Hash: TCRC8Hash;
begin
Hash := TCRC8Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.CRC8;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileCRC8(const FileName: String): TCRC8;
var
  Hash: TCRC8Hash;
begin
Hash := TCRC8Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.CRC8;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Standalone functions - context functions
-------------------------------------------------------------------------------}

Function CRC8_Init: TCRC8Context;
begin
Result := TCRC8Context(InitialCRC8);
end;

//------------------------------------------------------------------------------

procedure CRC8_Update(var Context: TCRC8Context; const Buffer; Size: TMemSize);
begin
TCRC8(Context) := CRC8Process(TCRC8(Context),Buffer,Size,@CRC8_TABLE);
end;

//------------------------------------------------------------------------------

Function CRC8_Final(var Context: TCRC8Context; const Buffer; Size: TMemSize): TCRC8;
begin
CRC8_Update(Context,Buffer,Size);
Result := CRC8_Final(Context);
end;

//------------------------------------------------------------------------------

Function CRC8_Final(var Context: TCRC8Context): TCRC8;
begin
Result := TCRC8(Context);
Context := TCRC8Context(ZeroCRC8)
end;

//------------------------------------------------------------------------------

Function CRC8_Hash(const Buffer; Size: TMemSize): TCRC8;
begin
Result := BufferCRC8(Buffer,Size);
end;

end.
