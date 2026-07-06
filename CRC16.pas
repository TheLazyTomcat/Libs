{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CRC-16 calculation

    Provides means of calculating 16-bit cyclic redundancy check (CRC-16) for
    any provided data (as long as they are in form of integral/whole bytes,
    arbitrary bit-streams are not supported), eg. buffers, strings, streams
    or files.

    It can be used either in form of objects, where you create an instance of
    approprite class (eg. TCRC16Hash) and use its methods to do the processing,
    or you can use provided procedural form (BufferCRC16, CRC16ToStr, ...).

    There is also class TCRC16CustomHash, which allows you to change parameters
    of CRC-16 algorithm (eg. polynomial or initial value), so you can calculate
    CRC for any desired specification.
    Provided constant array CRC16_KNOWN_PRESETS provides specifications for
    about 30 known CRC-16 variants - you can use it to initialize an instance
    of TCRC16CustomHash, it will then calculate the selected crc version.

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

      github.com/TheLazyTomcat/Lib.CRC16

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
unit CRC16;

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
  ECRC16Exception = class(EHashException);

  ECRC16IncompatibleClass = class(ECRC16Exception);
  ECRC16IndexOutOfBounds  = class(ECRC16Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TCRC16 are always ordered from least significant byte to most
  significant byte (little endian).

  Type TCRC16Sys has no such guarantee and its endianness is system-dependent.

  To convert the checksum in default ordering to a required specific ordering,
  use methods CRC16ToLE for little endian and CRC16ToBE for big endian.
  Note that these methods are expecting the input value to be in default
  ordering, if it is not, the result will be wrong. Be carefull when using them.
}
type
  TCRC16 = packed array[0..1] of UInt8;
  PCRC16 = ^TCRC16;

  TCRC16Sys = UInt16;
  PCRC16Sys = ^TCRC16Sys;

  TCRC16Table = array[UInt8] of TCRC16Sys;
  PCRC16Table = ^TCRC16Table;

const
{
  Initial value of CRC-16. Only to be used in standalone funstions, do not use
  it anywhere else.
}
  InitialCRC16: TCRC16 = ($00,$00);

  ZeroCRC16: TCRC16 = (0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                 TCRC16BaseHash
-------------------------------------------------------------------------------                                 
===============================================================================}
{===============================================================================
    TCRC16BaseHash - class declaration
===============================================================================}
type
  TCRC16BaseHash = class(TStreamHash)
  protected
    fCRC16Value:    TCRC16Sys;
    fCRC16Table:    PCRC16Table;
    Function GetCRC16: TCRC16; virtual;
    Function GetCRC16Poly: TCRC16Sys; virtual;
    Function GetCRC16PolyRef: TCRC16Sys; virtual; abstract;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure InitializeTable; virtual; abstract;
    procedure FinalizeTable; virtual; abstract;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class Function CRC16ToSys(CRC16: TCRC16): TCRC16Sys; virtual;
    class Function CRC16FromSys(CRC16: TCRC16Sys): TCRC16; virtual;
    class Function CRC16ToLE(CRC16: TCRC16): TCRC16; virtual;
    class Function CRC16ToBE(CRC16: TCRC16): TCRC16; virtual;
    class Function CRC16FromLE(CRC16: TCRC16): TCRC16; virtual;
    class Function CRC16FromBE(CRC16: TCRC16): TCRC16; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TCRC16); overload; virtual;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TCRC16); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property CRC16: TCRC16 read GetCRC16;
    property CRC16Sys: TCRC16Sys read fCRC16Value;
    property CRC16Poly: TCRC16Sys read GetCRC16Poly;        // polynomial
    property CRC16PolyRef: TCRC16Sys read GetCRC16PolyRef;  // polynomial with reflected bit order
    property CRC16Table: PCRC16Table read fCRC16Table;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TCRC16Hash                                   
--------------------------------------------------------------------------------
===============================================================================}
{
  Hardcoded implementation of CRC-16/ARC, following parameters are used within
  the calculation:

                  polynomial        0x18005
               initial value        0x00000000
             final xor value        0x00000000
       input bits reflection        True
      output bits reflection        True
}
{===============================================================================
    TCRC16Hash - class declaration
===============================================================================}
type
  TCRC16Hash = class(TCRC16BaseHash)
  protected
    Function GetCRC16PolyRef: TCRC16Sys; override;
    procedure InitializeTable; override;
    procedure FinalizeTable; override;
  public
    class Function HashName: String; override;
    class Function HashFinalization: Boolean; override;
    procedure Init; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TCRC16CustomHash
--------------------------------------------------------------------------------
===============================================================================}
type
  TCRC16CustomPreset = record
    Name:           String;     // name assigned to this CRC-16 within this library
    Aliases:        String;     // name aliasses, separated by comma (,)
    Polynomial:     TCRC16Sys;  // polynomial with highest bit omitted
    RefPolynomial:  TCRC16Sys;  // polynomial with reflected bit order and original highest bit omitted
    FullPolynomial: UInt32;     // full polynomial (only lower 17 bits are to be observed)
    InitialValue:   TCRC16;     // initial value of CRC register
    ReflectIn:      Boolean;    // order in which bits within input bytes are processed (true = LSB, false = MSB)
    ReflectOut:     Boolean;    // resulting CRC-16 is bit-swapped before presentation
    XOROutValue:    TCRC16;     // value XORed to the register after all processing is done
    Check:          TCRC16;     // CRC-16 of UTF-16 (really ASCII) encoded string "1234567169" (without quotes)
    Residue:        TCRC16;     // what is left in CRC-16 register (before final xor) after hashing of error-free data with appended CRC-16 value
    Codewords:      String;     // several comma-separated (,) datastream-crc pairs (binary, hexadecimal notation)
  end;

//------------------------------------------------------------------------------
// Source: reveng.sourceforge.net/crc-catalogue/16.htm
const
  CRC16_KNOWN_PRESETS: array[0..30] of TCRC16CustomPreset = (
   (Name:           'CRC-16/ARC';
    Aliases:        'ARC, CRC-16, CRC-16/LHA, CRC-IBM';
    Polynomial:     $8005;
    RefPolynomial:  $A001;
    FullPolynomial: $18005;
    InitialValue:   ($00,$00);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00);
    Check:          ($3D,$BB);
    Residue:        ($00,$00);
    Codewords:      '000000000000,' +
                    'F20183E1C2,' +
                    '0FAA0055E30B,' +
                    '00FF5511CF6C,' +
                    '332255AABBCCDDEEFF98AE,' +
                    '926B554EE2,' +
                    'FFFFFFFF0194'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/CDMA2000';
    Aliases:        '';
    Polynomial:     $C867;
    RefPolynomial:  $E613;
    FullPolynomial: $1C867;
    InitialValue:   ($FF,$FF);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($06,$4C);
    Residue:        ($00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/CMS';
    Aliases:        '';
    Polynomial:     $8005;
    RefPolynomial:  $A001;
    FullPolynomial: $18005;
    InitialValue:   ($FF,$FF);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($E7,$AE);
    Residue:        ($00,$00);
    Codewords:      '0200080078110000F00F0000F7E0,' +
                    '020008005B110000F00F00004725,' +
                    '0200080050110000F00F0000F71F,' +
                    '0200080024110000F00F00003636,' +
                    '02000800A6100000F00F0000141A,' +
                    '0200080034100000B0090000B725,' +
                    '0200080029100000B0090000E76E,' +
                    '0200080002100000B0090000D79B,' +
                    '0200080005100000B0090000A789,' +
                    '0200080078100000B0090000F696,' +
                    '00050900000000003030303000D55E,' +
                    '01000400051000003793,' +
                    '0200080034100000F00F00003740,' +
                    '0200080029100000F00F0000670B,' +
                    '0200080002100000F00F000057FE,' +
                    '0200080005100000F00F000027EC,' +
                    '0200080078100000F00F000076F3,' +
                    '02000C00301000009A01000000000000837B,' +
                    '02000C00301000009A0100009A0100004B55,' +
                    '02000C00301000009A01000034030000934E,' +
                    '02000C00301000009A010000CE0400005B09,' +
                    '02000C00301000009A01000068060000A311'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/DDS-110';
    Aliases:        '';
    Polynomial:     $8005;
    RefPolynomial:  $A001;
    FullPolynomial: $80051;
    InitialValue:   ($0D,$80);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($CF,$9E);
    Residue:        ($00,$00);
    Codewords:      '0200108200731082F7FE,' +
                    '02001082007800ED96,' +
                    '020010820078016D93,' +
                    '020005006600057A586435'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/DECT-R';
    Aliases:        'R-CRC-16';
    Polynomial:     $0589;
    RefPolynomial:  $91A0;
    FullPolynomial: $10589;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($01,$00);
    Check:          ($7E,$00);
    Residue:        ($89,$05);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/DECT-X';
    Aliases:        'X-CRC-16';
    Polynomial:     $0589;
    RefPolynomial:  $91A0;
    FullPolynomial: $10589;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($7F,$00);
    Residue:        ($00,$00);
    Codewords:      'A37029'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/DNP';
    Aliases:        '';
    Polynomial:     $3D65;
    RefPolynomial:  $A6BC;
    FullPolynomial: $13d65;
    InitialValue:   ($00,$00);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($FF,$FF);
    Check:          ($82,$EA);
    Residue:        ($C5,$66);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/EN-13757';
    Aliases:        '';
    Polynomial:     $3D65;
    RefPolynomial:  $A6BC;
    FullPolynomial: $13D65;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($FF,$FF);
    Check:          ($B7,$C2);
    Residue:        ($66,$A3);
    Codewords:      'EE449ACE010000802307AABC,' +
                    '7A4700E0A535E79CFAA94D07B173BC101496,' +
                    '021CCB0127DCF516FF4EA8BC0A4851744D3D,' +
                    '438A680ECFC8EAA7F5D6476CAD938EBF88AF,' +
                    '259794C336C6FE9601D99F81B5EBDD9F36B5,' +
                    '67FC406AA1107F380CA349EB2F1AD57AD22A,' +
                    '7D8067CD9D1076,' +
                    'D0F1E6C2C3962E,' +
                    '384BABA953BCE5,' +
                    '70414545C59920,' +
                    'F0649A4E111111110000E9CA,' +
                    'A12F2F2F2F2F2F2F2F2F2F2F2F2F2F2F4A08,' +
                    '2F2F2F2F2F2F2F2F2F2F2F2F2F2F2F2F63D7,' +
                    '7A8D00E0A503B1922864C36F29B37AF0646A,' +
                    'D1391E979A44C456786CBB99CD6ECAACB373,' +
                    '780B134365871E6D,' +
                    '07400001AAAA0000363E'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/GENIBUS';
    Aliases:        'CRC-16/DARC, CRC-16/EPC, CRC-16/EPC-C1G2, CRC-16/I-CODE';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($FF,$FF);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($FF,$FF);
    Check:          ($4E,$D6);
    Residue:        ($0F,$1D);
    Codewords:      '0000E2F0,' +
                    '08001111CCAE,' +
                    '100011112222968F,' +
                    '180011112222333378F6,' +
                    '20001111222233334444C241,' +
                    '2800111122223333444455552A91,' +
                    '30001111222233334444555566661835,' +
                    '402141424387F5,' +
                    '3000319F60A356276E28F14FDC9C5DB4,' +
                    '100120020C823E003982150064820900FA910A,' +
                    '07200102C302101A901C,' +
                    'C05000062C680811F27B45C0C1'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/GSM';
    Aliases:        '';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($FF,$FF);
    Check:          ($3C,$CE);
    Residue:        ($0F,$1D);
    Codewords:      '010101000000000002005A28,' +
                    '035D24E3580200010034D0F2'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/IBM-3740';
    Aliases:        'CRC-16/AUTOSAR, CRC-16/CCITT-FALSE';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($FF,$FF);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($B1,$29);
    Residue:        ($00,$00);
    Codewords:      '0000000084C0,' +
                    'F20183D374,' +
                    '0FAA00552023,' +
                    '00FF5511B8F9,' +
                    '332255AABBCCDDEEFFF53F,' +
                    '926B550745,' +
                    'FFFFFFFF1D0F,' +
                    'FE00000001F1D3,' +
                    '§1FBE5A40C'),  // FB + (E5 x 256) + A40C
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/IBM-SDLC';
    Aliases:        'CRC-16/ISO-HDLC, CRC-16/ISO-IEC-14443-3-B, CRC-16/X-25, CRC-B, X-25';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($FF,$FF);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($FF,$FF);
    Check:          ($6E,$90);
    Residue:        ($B8,$F0);
    Codewords:      '033F5BEC,' +
                    '01738357,' +
                    '013FEBDF,' +
                    '03733364,' +
                    '54D9E4,' +
                    '4361744D6F757365393837363534333231910A,' +
                    '21005100004944330300000000004A544954320000000D000000416E616C6F67' +
                      '20426C756573545045310000000D0000004A2E20512E205075626C696354414C' +
                      '4200000012000000546865204C6F73742053657373696F6E73F527,' +
                    '000000CCC6,' +
                    '0FAAFFFCD1,' +
                    '0A1234562CF6'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/ISO-IEC-14443-3-A';
    Aliases:        'CRC-A';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($C6,$C6);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00);
    Check:          ($05,$BF);
    Residue:        ($00,$00);
    Codewords:      '0000A01E,' +
                    '123426CF,' +
                    'AABBCCDDEE0011227B09,' +
                    '9370C2A82DF4B3BAA3,' +
                    '08B6DD,' +
                    '6030764A,' +
                    '500057CD,' +
                    '937000000000009CD9,' +
                    '6000F57B,' +
                    '93701DFBE03335D355'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/KERMIT';
    Aliases:        'CRC-16/BLUETOOTH, CRC-16/CCITT, CRC-16/CCITT-TRUE, CRC-16/V-41-LSB, CRC-CCITT, KERMIT';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($00,$00);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00);
    Check:          ($89,$21);
    Residue:        ($00,$00);
    Codewords:      '43AED6C8ADD651431551B03102D332B9C1D651313732B583F303,' +
                    '6DAEB9CDADCD524F15C1C154022FCD454C43C1D9C1AEC15431AEB9CDADCD524F' +
                      '32B0B93446C2C13443B0B3B9B946834861,' +
                    'CDAEB9CDADCD524F54DF7F3802D33231C1CDC8B03134388361A7,' +
                    '6D8080808080808015D3518002B0B058D3838236,' +
                    'CDAEB9CDADCD524F15C2C1B9024CB032C1CDC8B03134382FCD454C43C1D9C1AE' +
                      'C15431AEB9CDADCD524FC1B0B9B531323443B3C4B9B0B3C2C1B53131C2B9B0B0' +
                      'C143B53437313443B0C437C134B6B54334C1C432B6B9B5B5C14343433445B9C1' +
                      '3831B543383132343446C131B332B0B934B9B0B3B3C1B0454534C438374643B3834483,' +
                    '6DAEB9CDADCD524FB9DF7FD58391C6,' +
                    '43AED6C8ADDA5843D50DD0C1524954D920BF8397AC,' +
                    '43AED6C8ADDA584315C831BC0DD0C1524954D920BF8302D5,' +
                    '32AED6C8ADDA58C21538313702CD31B3C15146B034B3B90D8ACDD6C10D8A5146C1' +
                      'B034B3B92F31B0AED6C8DA58C2AECD454C200D8AC1C1B0B53137202020202083751B,' +
                    '6DAEB9D6ADD3CD57B5DF7F4A839BC6,' +
                    '54A114,' +
                    '4361744D6F7573653938373635343332318DC2,' +
                    '01000000C0F90080442E,' +
                    '71236340C0F900803E27'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/LJ1200';
    Aliases:        '';
    Polynomial:     $6F63;
    RefPolynomial:  $C6F6;
    FullPolynomial: $16F63;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($F4,$BD);
    Residue:        ($00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/M17';
    Aliases:        '';
    Polynomial:     $5935;
    RefPolynomial:  $AC9A;
    FullPolynomial: $15935;
    InitialValue:   ($FF,$FF);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($2B,$77);
    Residue:        ($00,$00);
    Codewords:      'FFFF,' +
                    '41206E,' +
                    '§21C31'),  // (i) x 256 + 1C31
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/MAXIM-DOW';
    Aliases:        'CRC-16/MAXIM';
    Polynomial:     $8005;
    RefPolynomial:  $A001;
    FullPolynomial: $18005;
    InitialValue:   ($00,$00);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($FF,$FF);
    Check:          ($C2,$44);
    Residue:        ($01,$B0);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/MCRF4XX';
    Aliases:        '';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($FF,$FF);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00);
    Check:          ($91,$6F);
    Residue:        ($00,$00);
    Codewords:      '54261B,' +
                    '4361744D6F7573653938373635343332316EF5,' +
                    '3A7164D9,' +
                    '3A916A3E'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/MODBUS';
    Aliases:        'MODBUS';
    Polynomial:     $8005;
    RefPolynomial:  $A001;
    FullPolynomial: $18005;
    InitialValue:   ($FF,$FF);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00);
    Check:          ($37,$4B);
    Residue:        ($00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/NRSC-5';
    Aliases:        '';
    Polynomial:     $080B;
    RefPolynomial:  $D010;
    FullPolynomial: $1080B;
    InitialValue:   ($FF,$FF);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00);
    Check:          ($66,$A0);
    Residue:        ($00,$00);
    Codewords:      '000048BBABA0329A0A0A2671,' +
                    '0030C8D3D37379D91E0A7B40,' +
                    '0010C097A920F31B240A2D1D,' +
                    '00200882E861D03C3C0AEB71,' +
                    '00100000DC69FABC4E0AB6CD,' +
                    '000015004000809E54456AC7,' +
                    '00200008FA7E849E54454B82,' +
                    '001000744280849E54457C5C'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/OPENSAFETY-A';
    Aliases:        '';
    Polynomial:     $5935;
    RefPolynomial:  $AC9A;
    FullPolynomial: $15935;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($38,$5D);
    Residue:        ($00,$00);
    Codewords:      '23C8083411223344556677880374'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/OPENSAFETY-B';
    Aliases:        '';
    Polynomial:     $755B;
    RefPolynomial:  $DAAE;
    FullPolynomial: $1755B;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($FE,$20);
    Residue:        ($00,$00);
    Codewords:      '22C812563011223344556677887031'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/PROFIBUS';
    Aliases:        'CRC-16/IEC-61158-2';
    Polynomial:     $1DCF;
    RefPolynomial:  $F3B8;
    FullPolynomial: $1;
    InitialValue:   ($FF,$Ff);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($FF,$FF);
    Check:          ($19,$A8);
    Residue:        ($94,$E3);
    Codewords:      '34AF21,' +
                    '011057E0,' +
                    '321000E37E82,' +
                    '331000E32A0F,' +
                    '2611010000080A109739'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/RIELLO';
    Aliases:        '';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($AA,$B2);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00);
    Check:          ($D0,$63);
    Residue:        ($00,$00);
    Codewords:      '2022525330308790'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/SPI-FUJITSU';
    Aliases:        'CRC-16/AUG-CCITT';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($0F,$1D);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($CC,$E5);
    Residue:        ($00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/T10-DIF';
    Aliases:        '';
    Polynomial:     $8BB7;
    RefPolynomial:  $EDD1;
    FullPolynomial: $18BB7;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($DB,$D0);
    Residue:        ($00,$00);
    Codewords:      '00000000000000000000000000000000000000000000000000000000000000000000,' +
                    'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA293,' +
                    '000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F0224,' +
                    'FFFF00000000000000000000000000000000000000000000000000000000000021B8,' +
                    'FFFEFDFCFBFAF9F8F7F6F5F4F3F2F1F0EFEEEDECEBEAE9E8E7E6E5E4E3E2E1E0A0B7'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/TELEDISK';
    Aliases:        '';
    Polynomial:     $A097;
    RefPolynomial:  $E905;
    FullPolynomial: $1A097;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($B3,$0F);
    Residue:        ($00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/TMS37157';
    Aliases:        '';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($EC,$89);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00);
    Check:          ($B1,$26);
    Residue:        ($00,$00);
    Codewords:      '0225A6,' +
                    '024000000000A060E7,' +
                    '024100000000A46FA5,' +
                    '01AAAAAAAAAADC25AB'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/UMTS';
    Aliases:        'CRC-16/BUYPASS, CRC-16/VERIFONE';
    Polynomial:     $8005;
    RefPolynomial:  $A001;
    FullPolynomial: $18005;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($E8,$FE);
    Residue:        ($00,$00);
    Codewords:      '§01FBB,' + // (i+i*i) x 1999 + 1FBB
                    '0384901B56,' +
                    '03848400001230314131333030323031333030311C39303062BF'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/USB';
    Aliases:        '';
    Polynomial:     $8005;
    RefPolynomial:  $A001;
    FullPolynomial: $18005;
    InitialValue:   ($FF,$FF);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($FF,$FF);
    Check:          ($C8,$B4);
    Residue:        ($01,$B0);
    Codewords:      '1BED534B3E6D7F7CD7CC,' +
                    '55534243A864AB870800000080000A25000000000000000000000000000000C30E,' +
                    '8006000100004000DD94,' +
                    '00010203EF7A,' +
                    '234567890E1C'),
    // - - - - - - - - - - - - - - - -
   (Name:           'CRC-16/XMODEM';
    Aliases:        'CRC-16/ACORN, CRC-16/LTE, CRC-16/V-41-MSB, XMODEM, ZMODEM';
    Polynomial:     $1021;
    RefPolynomial:  $8408;
    FullPolynomial: $11021;
    InitialValue:   ($00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00);
    Check:          ($C3,$31);
    Residue:        ($00,$00);
    Codewords:      '541A71,' +
                    '4361744D6F757365393837363534333231E556'));
    
const
  CRC16_DEFAULT_PRESET_IDX = 0; // CRC-16/ARC

{===============================================================================
    TCRC16CustomHash - class declaration
===============================================================================}
type
  TCRC16CustomHash = class(TCRC16BaseHash)
  protected
    fName:          String;
    fCRC16Poly:     TCRC16Sys;  // in reflected bit order
    fInitialValue:  TCRC16;
    fReflectIn:     Boolean;
    fReflectOut:    Boolean;
    fXOROutValue:   TCRC16;
    procedure SetCRC16Poly(Value: TCRC16Sys); virtual;
    Function GetCRC16PolyRef: TCRC16Sys; override;
    procedure SetCRC16PolyRef(Value: TCRC16Sys); virtual;
    procedure SetInitialValue(Value: TCRC16); virtual;
    procedure SetReflectIn(Value: Boolean); virtual;
    procedure SetReflectOut(Value: Boolean); virtual;
    procedure SetXOROutValue(Value: TCRC16); virtual;
    procedure BuildTable; virtual;
    procedure InitializeTable; override;
    procedure FinalizeTable; override;
    procedure Initialize; override;
  public
    Function HashName: String; reintroduce; virtual;
    constructor CreateAndInitFrom(Hash: THashBase); override;
    constructor CreateAndLoadPreset(Preset: TCRC16CustomPreset); overload;
    constructor CreateAndLoadPreset(PresetIndex: Integer); overload;
    constructor CreateAndLoadPreset(const PresetName: String); overload;
    procedure LoadPreset(Preset: TCRC16CustomPreset); overload; virtual;
    procedure LoadPreset(PresetIndex: Integer); overload; virtual;
    procedure LoadPreset(const PresetName: String); overload; virtual;
    Function SelfTest(Preset: TCRC16CustomPreset): Boolean; virtual;
    procedure Init; override;
    procedure Final; override;
    property CRC16Poly: TCRC16Sys read GetCRC16Poly write SetCRC16Poly;
    property CRC16PolyRef: TCRC16Sys read GetCRC16PolyRef write SetCRC16PolyRef;
    property InitialValue: TCRC16 read fInitialValue write SetInitialValue;
    property ReflectIn: Boolean read fReflectIn write SetReflectIn;
    property ReflectOut: Boolean read fReflectOut write SetReflectOut;
    property XOROutValue: TCRC16 read fXOROutValue write SetXOROutValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}
{
  Most of the following functions are calling direct implementation, only
  functions StreamCRC16 and FileCRC16 are using instance of TCRC16Hash class
  to do the processing.  
}
{===============================================================================
    Standalone functions - declaration
===============================================================================}

Function CRC16ToStr(const CRC16: TCRC16): String;
Function StrToCRC16(const Str: String): TCRC16;
Function TryStrToCRC16(const Str: String; out CRC16: TCRC16): Boolean;
Function StrToCRC16Def(const Str: String; Default: TCRC16): TCRC16;

Function CompareCRC16(const A,B: TCRC16): Integer;
Function SameCRC16(const A,B: TCRC16): Boolean;

//------------------------------------------------------------------------------

Function BufferCRC16(const CRC16: TCRC16; const Buffer; Size: TMemSize): TCRC16; overload;

Function BufferCRC16(const Buffer; Size: TMemSize): TCRC16; overload;

Function AnsiStringCRC16(const Str: AnsiString): TCRC16;
Function WideStringCRC16(const Str: WideString): TCRC16;
Function StringCRC16(const Str: String): TCRC16;

Function StreamCRC16(Stream: TStream; Count: Int64 = -1): TCRC16;
Function FileCRC16(const FileName: String): TCRC16;

//------------------------------------------------------------------------------

type
  TCRC16Context = type TCRC16Sys;

Function CRC16_Init: TCRC16Context;
procedure CRC16_Update(var Context: TCRC16Context; const Buffer; Size: TMemSize);
Function CRC16_Final(var Context: TCRC16Context; const Buffer; Size: TMemSize): TCRC16; overload;
Function CRC16_Final(var Context: TCRC16Context): TCRC16; overload;
Function CRC16_Hash(const Buffer; Size: TMemSize): TCRC16;

implementation

uses
  SysUtils;

{===============================================================================
    Internals implementation
===============================================================================}

Function SwapEndian(Value: TCRC16Sys): TCRC16Sys; overload;
begin
Result := TCRC16Sys(((Value and $00FF) shl 8) or ((Value and $FF00) shr 8));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TCRC16): TCRC16; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := TCRC16(SwapEndian(TCRC16Sys(Value)));
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

Function ReflectBits(Value: TCRC16Sys): TCRC16Sys; overload;
type
  TByteOverlay = packed array[0..1] of UInt8;
begin
TByteOverlay(Result)[1] := ReflectBits(TByteOverlay(Value)[0]);
TByteOverlay(Result)[0] := ReflectBits(TByteOverlay(Value)[1]);
end;

//------------------------------------------------------------------------------

Function ReflectByteBits(Value: TCRC16Sys): TCRC16Sys; overload;
type
  TByteOverlay = packed array[0..1] of UInt8;
begin
TByteOverlay(Result)[0] := ReflectBits(TByteOverlay(Value)[0]);
TByteOverlay(Result)[1] := ReflectBits(TByteOverlay(Value)[1]);
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

Function CRC16Process(const CRC16: TCRC16Sys; const Buffer; Size: TMemSize; CRC16TablePtr: PCRC16Table): TCRC16Sys;
var
  i:    TMemSize;
  Buff: PByte;
begin
Result := CRC16;
Buff := @Buffer;
For i := 1 to Size do
  begin
    Result := CRC16TablePtr^[Byte(Result) xor Buff^] xor (Result shr 8);
    Inc(Buff);
  end;
end;

//------------------------------------------------------------------------------

Function CRC16Compare(const A,B: TCRC16Sys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function CRC16Same(const A,B: TCRC16Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function CRC16AsString(const CRC16: TCRC16Sys): String;
begin
Result := IntToHex(CRC16,4);
end;

//------------------------------------------------------------------------------

Function CRC16FromString(const Str: String): TCRC16Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TCRC16Sys(StrToInt(Str))
    else
      Result := TCRC16Sys(StrToInt('$' + Str));
  end
else Result := TCRC16Hash.CRC16ToSys(ZeroCRC16);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TCRC16BaseHash
-------------------------------------------------------------------------------                                 
===============================================================================}
{===============================================================================
    TCRC16BaseHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC16BaseHash - protected methods
-------------------------------------------------------------------------------}

Function TCRC16BaseHash.GetCRC16: TCRC16;
begin
Result := CRC16FromSys(fCRC16Value);
end;

//------------------------------------------------------------------------------

Function TCRC16BaseHash.GetCRC16Poly: TCRC16Sys;
begin
Result := ReflectBits(GetCRC16PolyRef);
end;

//------------------------------------------------------------------------------

procedure TCRC16BaseHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fCRC16Value := CRC16Process(fCRC16Value,Buffer,Size,fCRC16Table);
end;

//------------------------------------------------------------------------------

procedure TCRC16BaseHash.Initialize;
begin
inherited;
fCRC16Value := 0;
InitializeTable;
end;

//------------------------------------------------------------------------------

procedure TCRC16BaseHash.Finalize;
begin
FinalizeTable;
inherited;
end;

{-------------------------------------------------------------------------------
    TCRC16BaseHash - public methods
-------------------------------------------------------------------------------}

class Function TCRC16BaseHash.CRC16ToSys(CRC16: TCRC16): TCRC16Sys;
begin
Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TCRC16Sys(CRC16));
end;

//------------------------------------------------------------------------------

class Function TCRC16BaseHash.CRC16FromSys(CRC16: TCRC16Sys): TCRC16;
begin
Result := TCRC16({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CRC16));
end;

//------------------------------------------------------------------------------

class Function TCRC16BaseHash.CRC16ToLE(CRC16: TCRC16): TCRC16;
begin
Result := CRC16;
end;

//------------------------------------------------------------------------------

class Function TCRC16BaseHash.CRC16ToBE(CRC16: TCRC16): TCRC16;
begin
Result := SwapEndian(CRC16);
end;

//------------------------------------------------------------------------------

class Function TCRC16BaseHash.CRC16FromLE(CRC16: TCRC16): TCRC16;
begin
Result := CRC16;
end;

//------------------------------------------------------------------------------

class Function TCRC16BaseHash.CRC16FromBE(CRC16: TCRC16): TCRC16;
begin
Result := SwapEndian(CRC16);
end;

//------------------------------------------------------------------------------

class Function TCRC16BaseHash.HashSize: TMemSize;
begin
Result := SizeOf(TCRC16);
end;

//------------------------------------------------------------------------------

class Function TCRC16BaseHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TCRC16BaseHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TCRC16BaseHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCRC16BaseHash then
  fCRC16Value := TCRC16BaseHash(Hash).CRC16Sys
else
  raise ECRC16IncompatibleClass.CreateFmt('TCRC16BaseHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC16BaseHash.CreateAndInitFrom(Hash: TCRC16);
begin
CreateAndInit;
fCRC16Value := CRC16ToSys(Hash);
end;

//------------------------------------------------------------------------------

Function TCRC16BaseHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TCRC16BaseHash then
  Result := CRC16Compare(fCRC16Value,TCRC16BaseHash(Hash).CRC16Sys)
else
  raise ECRC16IncompatibleClass.CreateFmt('TCRC16BaseHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCRC16BaseHash.Same(Hash: THashBase): Boolean;
begin
If Hash is TCRC16BaseHash then
  Result := CRC16Same(fCRC16Value,TCRC16BaseHash(Hash).CRC16Sys)
else
  raise ECRC16IncompatibleClass.CreateFmt('TCRC16BaseHash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCRC16BaseHash.AsString: String;
begin
Result := CRC16AsString(fCRC16Value);
end;

//------------------------------------------------------------------------------

procedure TCRC16BaseHash.FromString(const Str: String);
begin
fCRC16Value := CRC16FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TCRC16BaseHash.FromStringDef(const Str: String; const Default: TCRC16);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fCRC16Value := CRC16ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TCRC16BaseHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRC16;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}CRC16ToBE{$ELSE}CRC16ToLE{$ENDIF}(CRC16FromSys(fCRC16Value));
  heLittle: Temp := CRC16ToLE(CRC16FromSys(fCRC16Value));
  heBig:    Temp := CRC16ToBE(CRC16FromSys(fCRC16Value));
else
 {heDefault}
  Temp := CRC16FromSys(fCRC16Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TCRC16));
end;

//------------------------------------------------------------------------------

procedure TCRC16BaseHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRC16;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(TCRC16));
case Endianness of
  heSystem: fCRC16Value := CRC16ToSys({$IFDEF ENDIAN_BIG}CRC16FromBE{$ELSE}CRC16FromLE{$ENDIF}(Temp));
  heLittle: fCRC16Value := CRC16ToSys(CRC16FromLE(Temp));
  heBig:    fCRC16Value := CRC16ToSys(CRC16FromBE(Temp));
else
 {heDefault}
  fCRC16Value := CRC16ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TCRC16Hash                                   
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC16_POLYREF: TCRC16Sys = $A001;

  CRC16_TABLE: TCRC16Table = (
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
    TCRC16Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC16Hash - protected methods
-------------------------------------------------------------------------------}

Function TCRC16Hash.GetCRC16PolyRef: TCRC16Sys;
begin
Result := CRC16_POLYREF;
end;

//------------------------------------------------------------------------------

procedure TCRC16Hash.InitializeTable;
begin
fCRC16Table := @CRC16_TABLE;
end;

//------------------------------------------------------------------------------

procedure TCRC16Hash.FinalizeTable;
begin
fCRC16Table := nil;
end;

{-------------------------------------------------------------------------------
    TCRC16Hash - public methods
-------------------------------------------------------------------------------}

class Function TCRC16Hash.HashName: String;
begin
Result := 'CRC-16';
end;

//------------------------------------------------------------------------------

class Function TCRC16Hash.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

procedure TCRC16Hash.Init;
begin
inherited;
fCRC16Value := CRC16ToSys(InitialCRC16);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TCRC16CustomHash
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC16_CUSTOM_DEFNAME = 'CRC-16(custom)';

{===============================================================================
    TCRC16CustomHash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC16CustomHash - protected methods
-------------------------------------------------------------------------------}

procedure TCRC16CustomHash.SetCRC16Poly(Value: TCRC16Sys);
begin
SetCRC16PolyRef(ReflectBits(Value));
end;

//------------------------------------------------------------------------------

Function TCRC16CustomHash.GetCRC16PolyRef: TCRC16Sys;
begin
Result := fCRC16Poly;
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.SetCRC16PolyRef(Value: TCRC16Sys);
begin
If fCRC16Poly <> Value then
  begin
    fName := CRC16_CUSTOM_DEFNAME;
    fCRC16Poly := Value;
    BuildTable;
    // invalidate running computations
    If fInitialized and not fFinalized then
      fInitialized := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.SetInitialValue(Value: TCRC16);
begin
If CRC16ToSys(Value) <> CRC16ToSys(fInitialValue) then
  begin
    fName := CRC16_CUSTOM_DEFNAME;
    fInitialValue := Value;
    If fInitialized and not fFinalized then
      fInitialized := False;    
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.SetReflectIn(Value: Boolean);
begin
If Value <> fReflectIn then
  begin
    fName := CRC16_CUSTOM_DEFNAME;
    fReflectIn := Value;
    BuildTable;
    If fInitialized and not fFinalized then
      fInitialized := False;    
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.SetReflectOut(Value: Boolean);
begin
If Value <> fReflectOut then
  begin
    fName := CRC16_CUSTOM_DEFNAME;
    fReflectOut := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.SetXOROutValue(Value: TCRC16);
begin
If CRC16ToSys(Value) <> CRC16ToSys(fXOROutValue) then
  begin
    fName := CRC16_CUSTOM_DEFNAME;
    fXOROutValue := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.BuildTable;
var
  i,j:  Integer;
  Temp: TCRC16Sys;
begin
For i := Low(TCRC16Table) to High(TCRC16Table) do
  begin
    If fReflectIn then
      Temp := TCRC16Sys(i) shl 1
    else
      Temp := TCRC16Sys(ReflectBits(UInt8(i))) shl 1;
    For j := 8 downto 0 do
      begin
        If (Temp and 1) <> 0 then
          Temp := (Temp shr 1) xor fCRC16Poly
        else
          Temp := Temp shr 1;
      end;
    If fReflectIn then
      fCRC16Table^[UInt8(i)] := Temp
    else
      fCRC16Table^[UInt8(i)] := ReflectByteBits(Temp);
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.InitializeTable;
begin
New(fCRC16Table);
BuildTable;
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.FinalizeTable;
begin
Dispose(fCRC16Table);
fCRC16Table := nil;
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.Initialize;
begin
fName := CRC16_KNOWN_PRESETS[CRC16_DEFAULT_PRESET_IDX].Name;
fCRC16Poly := CRC16_KNOWN_PRESETS[CRC16_DEFAULT_PRESET_IDX].RefPolynomial;
fInitialValue := CRC16_KNOWN_PRESETS[CRC16_DEFAULT_PRESET_IDX].InitialValue;
fReflectIn := CRC16_KNOWN_PRESETS[CRC16_DEFAULT_PRESET_IDX].ReflectIn;
fReflectOut := CRC16_KNOWN_PRESETS[CRC16_DEFAULT_PRESET_IDX].ReflectOut;
fXOROutValue := CRC16_KNOWN_PRESETS[CRC16_DEFAULT_PRESET_IDX].XOROutValue;
inherited;
end;

{-------------------------------------------------------------------------------
    TCRC16CustomHash - public methods
-------------------------------------------------------------------------------}

Function TCRC16CustomHash.HashName: String;
begin
Result := fName;
end;

//------------------------------------------------------------------------------

constructor TCRC16CustomHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCRC16CustomHash then
  begin
    fName := TCRC16CustomHash(Hash).HashName;
    fCRC16Poly := TCRC16CustomHash(Hash).CRC16PolyRef;
    fInitialValue := TCRC16CustomHash(Hash).InitialValue;
    fReflectIn := TCRC16CustomHash(Hash).ReflectIn;
    fReflectOut := TCRC16CustomHash(Hash).ReflectOut;
    fXOROutValue := TCRC16CustomHash(Hash).XOROutValue;
    BuildTable;
  end
else raise ECRC16IncompatibleClass.CreateFmt('TCRC16CustomHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TCRC16CustomHash.CreateAndLoadPreset(Preset: TCRC16CustomPreset);
begin
Create;
LoadPreset(Preset);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC16CustomHash.CreateAndLoadPreset(PresetIndex: Integer);
begin
Create;
LoadPreset(PresetIndex);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC16CustomHash.CreateAndLoadPreset(const PresetName: String);
begin
Create;
LoadPreset(PresetName);
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.LoadPreset(Preset: TCRC16CustomPreset);
begin
fName := Preset.Name;
fCRC16Poly := Preset.RefPolynomial;
fInitialValue := Preset.InitialValue;
fReflectIn := Preset.ReflectIn;
fReflectOut := Preset.ReflectOut;
fXOROutValue := Preset.XOROutValue;
BuildTable;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRC16CustomHash.LoadPreset(PresetIndex: Integer);
begin
If (PresetIndex >= Low(CRC16_KNOWN_PRESETS)) and (PresetIndex <= High(CRC16_KNOWN_PRESETS)) then
  LoadPreset(CRC16_KNOWN_PRESETS[PresetIndex])
else
  raise ECRC16IndexOutOfBounds.CreateFmt('TCRC16CustomHash.LoadPreset: Index (%d) out of bounds.',[PresetIndex]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRC16CustomHash.LoadPreset(const PresetName: String);
var
  Aliases:  TStringList;
  i:        Integer;
  Index:    Integer;
begin
Aliases := TStringList.Create;
try
  Aliases.CaseSensitive := False;
  For i := Low(CRC16_KNOWN_PRESETS) to High(CRC16_KNOWN_PRESETS) do
    begin
      If not AnsiSameText(CRC16_KNOWN_PRESETS[i].Name,PresetName) then
        begin
          SplitString(CRC16_KNOWN_PRESETS[i].Aliases,Aliases);
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
    LoadPreset(CRC16_KNOWN_PRESETS[Index]);
finally
  Aliases.Free;
end;
end;

//------------------------------------------------------------------------------

Function TCRC16CustomHash.SelfTest(Preset: TCRC16CustomPreset): Boolean;
var
  CodewordData: array of Byte;
  CodewordCRC:  TCRC16;

  Function DecodeCodeword(const Str: String): Boolean;
  var
    i:  TStrOff;
  begin
    Result := False;
    If Length(Str) >= (SizeOf(TCRC16) * 2) then
      begin
        CodewordData := nil;
        If (Length(Str) = 10) and AnsiSameStr(Copy(Str,1,2),'§1') then
          begin
            SetLength(CodewordData,259);
            CodewordData[0] := Byte(StrToInt('$' + Copy(Str,3,2)));
            CodewordData[1] := Byte(StrToInt('$' + Copy(Str,5,2)));
            For i := (Low(CodewordData) + 2) to High(CodewordData) do
              CodewordData[i] := CodewordData[1];
            CodewordData[Pred(High(CodewordData))] := Byte(StrToInt('$' + Copy(Str,7,2)));
            CodewordData[High(CodewordData)] := Byte(StrToInt('$' + Copy(Str,9,2)));
          end
        else If (Length(Str) = 6) and AnsiSameStr(Copy(Str,1,2),'§2') then
          begin
            SetLength(CodewordData,258);
            For i := Low(CodewordData) to High(CodewordData) do
              CodewordData[i] := Byte(i);
            CodewordData[Pred(High(CodewordData))] := Byte(StrToInt('$' + Copy(Str,3,2)));
            CodewordData[High(CodewordData)] := Byte(StrToInt('$' + Copy(Str,5,2)));
          end
        else If (Length(Str) = 6) and AnsiSameStr(Copy(Str,1,2),'§0') then
          begin
            SetLength(CodewordData,2001);
            For i := Low(CodewordData) to (High(CodewordData) - 2) do
              CodewordData[i] := Byte(i + i * i);
            CodewordData[Pred(High(CodewordData))] := Byte(StrToInt('$' + Copy(Str,3,2)));
            CodewordData[High(CodewordData)] := Byte(StrToInt('$' + Copy(Str,5,2)));           
          end
        else
          begin
            SetLength(CodewordData,Length(Str) div 2);
            For i := Low(CodewordData) to High(CodewordData) do
              CodewordData[i] := StrToInt('$' + Copy(Str,(i * 2) + 1,2));
          end;
        CodewordCRC := PCRC16(Addr(CodewordData[High(CodewordData) - Pred(SizeOf(TCRC16))]))^;
        If not fReflectOut then
          CodewordCRC := SwapEndian(CodewordCRC);
        Result := True;
      end;
  end;

var
  TempCRC:    TCRC16;
  TempStr:    AnsiString;
  Codewords:  TStringList;
  i:          Integer;
begin
Result := False;
LoadPreset(Preset);
HashAnsiString(AnsiString('123456789'));
If CRC16ToSys(Preset.Check) = fCRC16Value then
  begin
    // prepare check string with appended check CRC
    If fReflectOut then
      TempCRC := Preset.Check
    else
      TempCRC := SwapEndian(Preset.Check);
    TempStr := AnsiString('123456789') + AnsiString(StringOfChar('0',SizeOf(TCRC16)));
    Move(TempCRC,Addr(TempStr[10])^,SizeOf(TCRC16));
    HashAnsiString(TempStr);
    If CRC16ToSys(Preset.Residue) = (fCRC16Value xor CRC16ToSys(fXOROutValue)) then
      begin
        Codewords := TStringList.Create;
        try
          SplitString(Preset.Codewords,Codewords);
          // check codewords for crc
          For i := 0 to Pred(Codewords.Count) do
            If DecodeCodeword(Codewords[i]) then
              begin
                // check crc
                HashBuffer(CodewordData[0],Length(CodewordData) - SizeOf(TCRC16));
                If CRC16ToSys(CodewordCRC) <> fCRC16Value then
                  Exit;
                // check residue
                HashMemory(Addr(CodewordData[0]),Length(CodewordData));
                If CRC16ToSys(Preset.Residue) <> (fCRC16Value xor CRC16ToSys(fXOROutValue)) then
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

procedure TCRC16CustomHash.Init;
begin
inherited;
If fReflectIn then
  fCRC16Value := ReflectBits(CRC16ToSys(fInitialValue))
else
  fCRC16Value := SwapEndian(CRC16ToSys(fInitialValue));
end;

//------------------------------------------------------------------------------

procedure TCRC16CustomHash.Final;
begin
inherited;
If fReflectIn then
  begin
    If fReflectOut then
      fCRC16Value := fCRC16Value xor CRC16ToSys(fXOROutValue)
    else
      fCRC16Value := ReflectBits(fCRC16Value) xor CRC16ToSys(fXOROutValue);
  end
else
  begin
    If fReflectOut then
      fCRC16Value := ReflectByteBits(fCRC16Value) xor CRC16ToSys(fXOROutValue)
    else
      fCRC16Value := SwapEndian(fCRC16Value) xor CRC16ToSys(fXOROutValue);
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

Function CRC16ToStr(const CRC16: TCRC16): String;
begin
Result := CRC16AsString(TCRC16Hash.CRC16ToSys(CRC16));
end;

//------------------------------------------------------------------------------

Function StrToCRC16(const Str: String): TCRC16;
begin
Result := TCRC16Hash.CRC16FromSys(CRC16FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToCRC16(const Str: String; out CRC16: TCRC16): Boolean;
begin
try
  CRC16 := TCRC16Hash.CRC16FromSys(CRC16FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToCRC16Def(const Str: String; Default: TCRC16): TCRC16;
begin
If not TryStrToCRC16(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareCRC16(const A,B: TCRC16): Integer;
begin
Result := CRC16Compare(TCRC16Hash.CRC16ToSys(A),TCRC16Hash.CRC16ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameCRC16(const A,B: TCRC16): Boolean;
begin
Result := CRC16Same(TCRC16Hash.CRC16ToSys(A),TCRC16Hash.CRC16ToSys(B));
end;

{-------------------------------------------------------------------------------
    Standalone functions - processing functions
-------------------------------------------------------------------------------}

Function BufferCRC16(const CRC16: TCRC16; const Buffer; Size: TMemSize): TCRC16;
begin
Result := TCRC16Hash.CRC16FromSys(CRC16Process(TCRC16Hash.CRC16ToSys(CRC16),Buffer,Size,@CRC16_TABLE));
end;

//------------------------------------------------------------------------------

Function BufferCRC16(const Buffer; Size: TMemSize): TCRC16;
begin
Result := TCRC16Hash.CRC16FromSys(CRC16Process(TCRC16Hash.CRC16ToSys(InitialCRC16),Buffer,Size,@CRC16_TABLE));
end;

//------------------------------------------------------------------------------

Function AnsiStringCRC16(const Str: AnsiString): TCRC16;
begin
Result := BufferCRC16(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringCRC16(const Str: WideString): TCRC16;
begin
Result := BufferCRC16(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringCRC16(const Str: String): TCRC16;
begin
Result := BufferCRC16(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamCRC16(Stream: TStream; Count: Int64 = -1): TCRC16;
var
  Hash: TCRC16Hash;
begin
Hash := TCRC16Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.CRC16;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileCRC16(const FileName: String): TCRC16;
var
  Hash: TCRC16Hash;
begin
Hash := TCRC16Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.CRC16;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Standalone functions - context functions
-------------------------------------------------------------------------------}

Function CRC16_Init: TCRC16Context;
begin
Result := TCRC16Context(TCRC16Hash.CRC16ToSys(InitialCRC16));
end;

//------------------------------------------------------------------------------

procedure CRC16_Update(var Context: TCRC16Context; const Buffer; Size: TMemSize);
begin
TCRC16Sys(Context) := CRC16Process(TCRC16Sys(Context),Buffer,Size,@CRC16_TABLE);
end;

//------------------------------------------------------------------------------

Function CRC16_Final(var Context: TCRC16Context; const Buffer; Size: TMemSize): TCRC16;
begin
CRC16_Update(Context,Buffer,Size);
Result := CRC16_Final(Context);
end;

//------------------------------------------------------------------------------

Function CRC16_Final(var Context: TCRC16Context): TCRC16;
begin
Result := TCRC16Hash.CRC16FromSys(TCRC16Sys(Context));
Context := TCRC16Context(TCRC16Hash.CRC16ToSys(ZeroCRC16));
end;

//------------------------------------------------------------------------------

Function CRC16_Hash(const Buffer; Size: TMemSize): TCRC16;
begin
Result := BufferCRC16(Buffer,Size);
end;

end.
