{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CRC-64 calculation

    Provides means of calculating 64-bit cyclic redundancy check (CRC-64) for
    almost any provided data (eg. buffers, strings, streams or files).
    Data must consist of integral (whole) bytes, arbitrary bitstreams are not
    supported.

    It can be used either in form of objects, where you create an instance of
    approprite class (eg. TCRC64Hash) and use its methods to do the processing,
    or you can use provided procedural form (BufferCRC64, CRC64ToStr, ...).
    But note that the procedural interface is implemented only as a wrapper
    around class TCRC64Hash.

    There is also class TCRC64CustomHash, which allows you to change parameters
    of CRC-64 algorithm (eg. polynomial or initial value), so you can calculate
    CRC for any desired specification.
    Provided constant array CRC64_KNOWN_PRESETS offers specifications for some
    of the known CRC-64 variants - you can use it to initialize an instance of
    TCRC64CustomHash, it will then calculate the selected crc version.

  Version 1.0 (2026-06-23)

  Last change 2026-06-24

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.CRC64

  Dependencies:
  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    HashBase    - github.com/TheLazyTomcat/Lib.HashBase
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    ListUtils          - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit CRC64;

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
  ECRC64Exception = class(EHashException);

  ECRC64IncompatibleClass = class(ECRC64Exception);
  ECRC64IndexOutOfBounds  = class(ECRC64Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TCRC64 are always ordered from least significant byte to most
  significant byte (little endian).

  Type TCRC64Sys has no such guarantee and its endianness is system-dependent.

  To convert the checksum in default ordering to a required specific ordering,
  use methods CRC64ToLE for little endian and CRC64ToBE for big endian.
  Note that these methods are expecting the input value to be in default
  ordering, if it is not, the result will be wrong. Be carefull when using them.
}
type
  TCRC64 = packed array[0..7] of UInt8;
  PCRC64 = ^TCRC64;

  TCRC64Sys = UInt64;
  PCRC64Sys = ^TCRC64Sys;

  TCRC64Table = array[UInt8] of TCRC64Sys;
  PCRC64Table = ^TCRC64Table;

const
{
  Initial value of CRC-64. Only to be used in standalone functions, do not use
  it anywhere else.
}
  InitialCRC64: TCRC64 = ($00,$00,$00,$00,$00,$00,$00,$00);

  ZeroCRC64: TCRC64 = (0,0,0,0,0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                 TCRC64BaseHash
-------------------------------------------------------------------------------                                 
===============================================================================}
{===============================================================================
    TCRC64BaseHash - class declaration
===============================================================================}
type
  TCRC64BaseHash = class(TStreamHash)
  protected
    fCRC64Value:    TCRC64Sys;
    fCRC64Table:    PCRC64Table;
    Function GetCRC64: TCRC64; virtual;
    Function GetCRC64Poly: TCRC64Sys; virtual;
    Function GetCRC64PolyRef: TCRC64Sys; virtual; abstract;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure InitializeTable; virtual; abstract;
    procedure FinalizeTable; virtual; abstract;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class Function CRC64ToSys(CRC64: TCRC64): TCRC64Sys; virtual;
    class Function CRC64FromSys(CRC64: TCRC64Sys): TCRC64; virtual;
    class Function CRC64ToLE(CRC64: TCRC64): TCRC64; virtual;
    class Function CRC64ToBE(CRC64: TCRC64): TCRC64; virtual;
    class Function CRC64FromLE(CRC64: TCRC64): TCRC64; virtual;
    class Function CRC64FromBE(CRC64: TCRC64): TCRC64; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TCRC64); overload; virtual;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TCRC64); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property CRC64: TCRC64 read GetCRC64;
    property CRC64Sys: TCRC64Sys read fCRC64Value;
    property CRC64Poly: TCRC64Sys read GetCRC64Poly;        // polynomial
    property CRC64PolyRef: TCRC64Sys read GetCRC64PolyRef;  // polynomial with reflected bit order
    property CRC64Table: PCRC64Table read fCRC64Table;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TCRC64Hash                                   
--------------------------------------------------------------------------------
===============================================================================}
{
  Hardcoded implementation of CRC-64/ECMA-182, following parameters are used
  within the calculation:

                  polynomial        0x142F0E1EBA9EA3693
               initial value        0x0000000000000000
             final xor value        0x0000000000000000
       input bits reflection        False
      output bits reflection        False
}
{===============================================================================
    TCRC64Hash - class declaration
===============================================================================}
type
  TCRC64Hash = class(TCRC64BaseHash)
  protected
    Function GetCRC64PolyRef: TCRC64Sys; override;
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
                                TCRC64CustomHash
--------------------------------------------------------------------------------
===============================================================================}
type
  TCRC64CustomPreset = record
    Name:           String;       // name assigned to this CRC-64 within this library
    Aliases:        String;       // name aliasses, separated by comma (,)
    Polynomial:     TCRC64Sys;    // polynomial with highest bit omitted, only lower 64 bits are part of the polynomial
    RefPolynomial:  TCRC64Sys;    // polynomial with reflected bit order and original highest bit omitted (only lower 64 bits)
    FullPolynomial: packed record // full polynomial (only lower 65 bits (lowest bit of Hi) are to be observed)
    {$IFDEF ENDIAN_BIG}
      HiBits: UInt8;  LoBits: UInt64;
    {$ELSE}
      LoBits: UInt64; HiBits: UInt8;
    {$ENDIF}
    end;
    InitialValue:   TCRC64;       // initial value of CRC register
    ReflectIn:      Boolean;      // order in which bits within input bytes are processed (true = LSB, false = MSB)
    ReflectOut:     Boolean;      // resulting CRC-64 is bit-swapped before presentation
    XOROutValue:    TCRC64;       // value XORed to the register after all processing is done
    Check:          TCRC64;       // CRC-64 of UTF-64 (really ASCII) encoded string "1234567649" (without quotes)
    Residue:        TCRC64;       // what is left in CRC-64 register (before final xor) after hashing of error-free data with appended CRC-64 value
    Codewords:      String;       // several comma-separated (,) datastream-crc pairs (binary, hexadecimal notation)
  end;

//------------------------------------------------------------------------------
// Source: reveng.sourceforge.net/crc-catalogue/17plus.htm
const
  CRC64_KNOWN_PRESETS: array[0..6] of TCRC64CustomPreset = (
   (Name:           'CRC-64/ECMA-182';
    Aliases:        'CRC-64';
    Polynomial:     TCRC64Sys($42F0E1EBA9EA3693);
    RefPolynomial:  TCRC64Sys($C96C5795D7870F42);
    // delphi requires the fields to be in the same order as in declaration
{$IFDEF ENDIAN_BIG}
    FullPolynomial: (HiBits: $01; LoBits: UInt64($42F0E1EBA9EA3693));
{$ELSE}
    FullPolynomial: (LoBits: UInt64($42F0E1EBA9EA3693); HiBits: $01);
{$ENDIF}
    InitialValue:   ($00,$00,$00,$00,$00,$00,$00,$00);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($00,$00,$00,$00,$00,$00,$00,$00);
    Check:          ($47,$73,$49,$0B,$5F,$DF,$40,$6C);
    Residue:        ($00,$00,$00,$00,$00,$00,$00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - - - - - - - - - - - -
   (Name:           'CRC-64/GO-ISO';
    Aliases:        '';
    Polynomial:     TCRC64Sys($000000000000001B);
    RefPolynomial:  TCRC64Sys($D800000000000000);
{$IFDEF ENDIAN_BIG}
    FullPolynomial: (HiBits: $01; LoBits: UInt64($000000000000001B));
{$ELSE}
    FullPolynomial: (LoBits: UInt64($000000000000001B); HiBits: $01);
{$ENDIF}
    InitialValue:   ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
    Check:          ($01,$10,$A4,$75,$C7,$56,$09,$B9);
    Residue:        ($00,$00,$00,$00,$00,$00,$00,$53);
    Codewords:      '0000000000000000,' +
                    '610000000000002034,' +
                    '6162000000000020C436,' +
                    '6162630000000020C47637,' +
                    '6162636400000020C4766733,' +
                    '6162636465000020C47667D332,' +
                    '6162636465660020C47667D30230,' +
                    '6162636465666720C47667D302B031,' +
                    '6162636465666768C47667D302B0210E,' +
                    '6162636465666768697667D302B0216E8B,' +
                    '6162636465666768696A67D302B0216E5B7F,' +
                    '48652077686F2068617320612073686164792070617374206B6E6F77' +
                      '732074686174206E69636520677579732066696E697368206C617374' +
                      '2EB45ABEE25917DBC7,' +
                    '46726565212046726565212F4120747269702F746F204D6172732F66' +
                      '6F72203930302F656D707479206A6172732F4275726D612053686176' +
                      '65EFA0DAA61142FCEA,' +
                    '4E6570616C207072656D69657220776F6E27742072657369676E2EA628EFD66A865552,' +
                    '486973206D6F6E6579206973207477696365207461696E7465643A20' +
                      '277461696E7420796F75727320616E6420277461696E74206D696E65' +
                      '2E16499D643A967888,' +
                    '73697A653A2020612E6F75743A2020626164206D61676963D2ADCDDA653C55F3,' +
                    '546865206D616A6F722070726F626C656D2069732077697468207365' +
                      '6E646D61696C2E20202D4D61726B20486F72746F6EB976A68740035E9D,' +
                    '5468697320697320612074657374206F662074686520656D65726765' +
                      '6E63792062726F6164636173742073797374656D2E613B506B00F1FCE7'),
    // - - - - - - - - - - - - - - - - - - - - - - - - - -
   (Name:           'CRC-64/MS';
    Aliases:        '';
    Polynomial:     TCRC64Sys($259C84CBA6426349);
    RefPolynomial:  TCRC64Sys($92C64265D32139A4);
{$IFDEF ENDIAN_BIG}
    FullPolynomial: (HiBits: $01; LoBits: UInt64($259C84CBA6426349));
{$ELSE}
    FullPolynomial: (LoBits: UInt64($259C84CBA6426349); HiBits: $01);
{$ENDIF}
    InitialValue:   ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00,$00,$00,$00,$00,$00,$00);
    Check:          ($EA,$CE,$4E,$02,$4F,$B7,$D4,$75);
    Residue:        ($00,$00,$00,$00,$00,$00,$00,$00);
    Codewords:      'EBDBF499B234C9018A000000000000000000000002000000D8AE24AF' +
                      'CF9C941F0100000008000000360000002E0000004200750073006900' +
                      '6E0065007300730049006D0070006100630074000000480042004900' +
                      '000007000000080000001C0000001800000050004900490000003100' +
                      '00005365C6807317DACE,' +
                    '4D004900430052004F0053004F00460054002E0049004E0054004500' +
                      '52004E00450054004500580050004C004F005200450052002E004400' +
                      '45004600410055004C005400A149B5EA6DB8C828,' +
                    '4D004900430052004F0053004F00460054002E00570049004E004400' +
                      '4F00570053002E0053005400490043004B0059004E004F0054004500' +
                      '530058C773F29AD57E33,' +
                    '7B00310041004300310034004500370037002D003000320045003700' +
                      '2D0034004500350044002D0042003700340034002D00320045004200' +
                      '3100410045003500310039003800420037007D005C004E004F005400' +
                      '45005000410044002E004500580045002B4EC2C169DC9C9B,' +
                    '7B00440036003500320033003100420030002D004200320046003100' +
                      '2D0034003800350037002D0041003400430045002D00410038004500' +
                      '3700430036004500410037004400320037007D005C004E004F005400' +
                      '45005000410044002E00450058004500237ED143CB0E8E91,' +
                    '7B00390030003500450036003300420036002D004300310042004600' +
                      '2D0034003900340045002D0042003200390043002D00360035004200' +
                      '3700330032004400330044003200310041007D005C00490045005800' +
                      '50004C004F00520045002E0045005800450006B6151674EBA0C3,' +
                    '7B00360044003800300039003300370037002D003600410046003000' +
                      '2D0034003400340042002D0038003900350037002D00410033003700' +
                      '3700330046003000320032003000300045007D005C00490045005800' +
                      '50004C004F00520045002E00450058004500CE6D913EB0E370E2,' +
                    '7B00370043003500410034003000450046002D004100300046004200' +
                      '2D0034004200460043002D0038003700340041002D00430030004600' +
                      '3200450030004200390046004100380045007D005C00490045005800' +
                      '50004C004F00520045002E00450058004500A5A793B7B47F220E'),
    // - - - - - - - - - - - - - - - - - - - - - - - - - -
   (Name:           'CRC-64/NVME';
    Aliases:        '';
    Polynomial:     TCRC64Sys($AD93D23594C93659);
    RefPolynomial:  TCRC64Sys($9A6C9329AC4BC9B5);
{$IFDEF ENDIAN_BIG}
    FullPolynomial: (HiBits: $01; LoBits: UInt64($AD93D23594C93659));
{$ELSE}
    FullPolynomial: (LoBits: UInt64($AD93D23594C93659); HiBits: $01);
{$ENDIF}
    InitialValue:   ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
    Check:          ($88,$98,$79,$0A,$86,$14,$8B,$AE);
    Residue:        ($42,$6E,$6F,$2B,$3B,$30,$10,$F3);
    Codewords:      '§0004EB622EB67D38264,' +     // 00 x 4096 + 4EB622EB67D38264
                    '§0FFACA3EC0273BADDC0,' +     // FF x 4096 + ACA3EC0273BADDC0
                    '§19C4450675F9F723E,' +       // (i) x 4096 + 9C4450675F9F723E
                    '§27E519E8E4BF62D9A'),        // (4095 - i) x 4096 + 7E519E8E4BF62D9A
    // - - - - - - - - - - - - - - - - - - - - - - - - - -
   (Name:           'CRC-64/REDIS';
    Aliases:        '';
    Polynomial:     TCRC64Sys($AD93D23594C935A9);
    RefPolynomial:  TCRC64Sys($95AC9329AC4BC9B5);
{$IFDEF ENDIAN_BIG}
    FullPolynomial: (HiBits: $01; LoBits: UInt64($AD93D23594C935A9));
{$ELSE}
    FullPolynomial: (LoBits: UInt64($AD93D23594C935A9); HiBits: $01);
{$ENDIF}
    InitialValue:   ($00,$00,$00,$00,$00,$00,$00,$00);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($00,$00,$00,$00,$00,$00,$00,$00);
    Check:          ($CA,$D9,$B8,$C4,$14,$D9,$C6,$E9);
    Residue:        ($00,$00,$00,$00,$00,$00,$00,$00);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - - - - - - - - - - - -
   (Name:           'CRC-64/WE';
    Aliases:        '';
    Polynomial:     TCRC64Sys($42F0E1EBA9EA3693);
    RefPolynomial:  TCRC64Sys($C96C5795D7870F42);
{$IFDEF ENDIAN_BIG}
    FullPolynomial: (HiBits: $01; LoBits: UInt64($42F0E1EBA9EA3693));
{$ELSE}
    FullPolynomial: (LoBits: UInt64($42F0E1EBA9EA3693); HiBits: $01);
{$ENDIF}
    InitialValue:   ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
    ReflectIn:      False;
    ReflectOut:     False;
    XOROutValue:    ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
    Check:          ($0A,$F0,$A4,$F1,$E3,$59,$EC,$62);
    Residue:        ($92,$A9,$31,$59,$BD,$BE,$AC,$FC);
    Codewords:      ''),
    // - - - - - - - - - - - - - - - - - - - - - - - - - -
   (Name:           'CRC-64/XZ';         
    Aliases:        'CRC-64/GO-ECMA';
    Polynomial:     TCRC64Sys($42F0E1EBA9EA3693);
    RefPolynomial:  TCRC64Sys($C96C5795D7870F42);
{$IFDEF ENDIAN_BIG}
    FullPolynomial: (HiBits: $01; LoBits: UInt64($42F0E1EBA9EA3693));
{$ELSE}
    FullPolynomial: (LoBits: UInt64($42F0E1EBA9EA3693); HiBits: $01);
{$ENDIF}
    InitialValue:   ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
    ReflectIn:      True;
    ReflectOut:     True;
    XOROutValue:    ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF);
    Check:          ($FA,$39,$19,$DF,$BB,$C9,$5D,$99);
    Residue:        ($3F,$35,$7D,$BD,$9A,$8C,$95,$49);
    Codewords:      '000000004B9F1B1E3586A5F4,' +
                    'F20183C6F1648166279C31,' +
                    '0FAA005575157C66F7D0C554,' +
                    '00FF5511E604077EBE2238A6,' +
                    '332255AABBCCDDEEFFD5E5A819B2CE1E70,' +
                    '926B554E3E9FB5A996AA5F,' +
                    'FFFFFFFF00000000FFFFFFFF,' +
                    '0000000000000000,' +
                    '61052B652E77840233,' +
                    '616246B0840E207365BC,' +
                    '6162632776271A4A09D82C,' +
                    '61626364BA60596E59289D3C,' +
                    '6162636465F29508FB58DF0B04,' +
                    '616263646566F400A745859F8ED0,' +
                    '61626364656667660E71CCA8A320EC,' +
                    '6162636465666768590C7A640AF3B467,' +
                    '6162636465666768698EEF569DC8F66699,' +
                    '6162636465666768696AF47357CD2E3A0932,' +
                    '48652077686F2068617320612073686164792070617374206B6E6F77' +
                      '732074686174206E69636520677579732066696E697368206C617374' +
                      '2E0DA0B92AACC06285,' +
                    '46726565212046726565212F4120747269702F746F204D6172732F66' +
                      '6F72203930302F656D707479206A6172732F4275726D612053686176' +
                      '658A513E353038601F,' +
                    '4E6570616C207072656D69657220776F6E27742072657369676E2E415A746AB1F20E79,' +
                    '486973206D6F6E6579206973207477696365207461696E7465643A20' +
                      '277461696E7420796F75727320616E6420277461696E74206D696E65' +
                      '2E6D100B66B2419E04,' +
                    '73697A653A2020612E6F75743A2020626164206D616769634D3AD6D86CE4B5E3,' +
                    '546865206D616A6F722070726F626C656D2069732077697468207365' +
                      '6E646D61696C2E20202D4D61726B20486F72746F6E51A0F2946BAF5A86,' +
                    '5468697320697320612074657374206F662074686520656D65726765' +
                    '6E63792062726F6164636173742073797374656D2E72BC5BC17F18DB27'));

const
  CRC64_DEFAULT_PRESET_IDX = 0; // CRC-64/ECMA-182

{===============================================================================
    TCRC64CustomHash - class declaration
===============================================================================}
type
  TCRC64CustomHash = class(TCRC64BaseHash)
  protected
    fName:          String;
    fCRC64Poly:     TCRC64Sys;  // in reflected bit order
    fInitialValue:  TCRC64;
    fReflectIn:     Boolean;
    fReflectOut:    Boolean;
    fXOROutValue:   TCRC64;
    procedure SetCRC64Poly(Value: TCRC64Sys); virtual;
    Function GetCRC64PolyRef: TCRC64Sys; override;
    procedure SetCRC64PolyRef(Value: TCRC64Sys); virtual;
    procedure SetInitialValue(Value: TCRC64); virtual;
    procedure SetReflectIn(Value: Boolean); virtual;
    procedure SetReflectOut(Value: Boolean); virtual;
    procedure SetXOROutValue(Value: TCRC64); virtual;
    procedure BuildTable; virtual;
    procedure InitializeTable; override;
    procedure FinalizeTable; override;
    procedure Initialize; override;
  public
    Function HashName: String; reintroduce; virtual;
    constructor CreateAndInitFrom(Hash: THashBase); override;
    constructor CreateAndLoadPreset(Preset: TCRC64CustomPreset); overload;
    constructor CreateAndLoadPreset(PresetIndex: Integer); overload;
    constructor CreateAndLoadPreset(const PresetName: String); overload;
    procedure LoadPreset(Preset: TCRC64CustomPreset); overload; virtual;
    procedure LoadPreset(PresetIndex: Integer); overload; virtual;
    procedure LoadPreset(const PresetName: String); overload; virtual;
    Function SelfTest(Preset: TCRC64CustomPreset): Boolean; virtual;
    procedure Init; override;
    procedure Final; override;
    property CRC64Poly: TCRC64Sys read GetCRC64Poly write SetCRC64Poly;
    property CRC64PolyRef: TCRC64Sys read GetCRC64PolyRef write SetCRC64PolyRef;
    property InitialValue: TCRC64 read fInitialValue write SetInitialValue;
    property ReflectIn: Boolean read fReflectIn write SetReflectIn;
    property ReflectOut: Boolean read fReflectOut write SetReflectOut;
    property XOROutValue: TCRC64 read fXOROutValue write SetXOROutValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Standalone functions
--------------------------------------------------------------------------------
===============================================================================}
{
  All following functions are implemented as wrappers around TCRC64Hash class
  and its methods.
}
{===============================================================================
    Standalone functions - declaration
===============================================================================}

Function CRC64ToStr(CRC64: TCRC64): String;
Function StrToCRC64(const Str: String): TCRC64;
Function TryStrToCRC64(const Str: String; out CRC64: TCRC64): Boolean;
Function StrToCRC64Def(const Str: String; Default: TCRC64): TCRC64;

Function CompareCRC64(A,B: TCRC64): Integer;
Function SameCRC64(A,B: TCRC64): Boolean;

//------------------------------------------------------------------------------

Function BufferCRC64(CRC64: TCRC64; const Buffer; Size: TMemSize): TCRC64; overload;

Function BufferCRC64(const Buffer; Size: TMemSize): TCRC64; overload;

Function AnsiStringCRC64(const Str: AnsiString): TCRC64;
Function WideStringCRC64(const Str: WideString): TCRC64;
Function StringCRC64(const Str: String): TCRC64;

Function StreamCRC64(Stream: TStream; Count: Int64 = -1): TCRC64;
Function FileCRC64(const FileName: String): TCRC64;

//------------------------------------------------------------------------------

type
  TCRC64Context = type Pointer;

Function CRC64_Init: TCRC64Context;
procedure CRC64_Update(Context: TCRC64Context; const Buffer; Size: TMemSize);
Function CRC64_Final(var Context: TCRC64Context; const Buffer; Size: TMemSize): TCRC64; overload;
Function CRC64_Final(var Context: TCRC64Context): TCRC64; overload;
Function CRC64_Hash(const Buffer; Size: TMemSize): TCRC64;

implementation

uses
  SysUtils,
  UInt64Utils;

{===============================================================================
    Internals implementation
===============================================================================}

Function SwapEndian(Value: TCRC64Sys): TCRC64Sys; overload;
begin
UInt64Rec(Result).Bytes[0] := UInt64Rec(Value).Bytes[7];
UInt64Rec(Result).Bytes[1] := UInt64Rec(Value).Bytes[6];
UInt64Rec(Result).Bytes[2] := UInt64Rec(Value).Bytes[5];
UInt64Rec(Result).Bytes[3] := UInt64Rec(Value).Bytes[4];
UInt64Rec(Result).Bytes[4] := UInt64Rec(Value).Bytes[3];
UInt64Rec(Result).Bytes[5] := UInt64Rec(Value).Bytes[2];
UInt64Rec(Result).Bytes[6] := UInt64Rec(Value).Bytes[1];
UInt64Rec(Result).Bytes[7] := UInt64Rec(Value).Bytes[0];
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TCRC64): TCRC64; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := TCRC64(SwapEndian(TCRC64Sys(Value)));
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

Function ReflectBits(Value: TCRC64Sys): TCRC64Sys; overload;
begin
UInt64Rec(Result).Bytes[0] := ReflectBits(UInt64Rec(Value).Bytes[7]);
UInt64Rec(Result).Bytes[1] := ReflectBits(UInt64Rec(Value).Bytes[6]);
UInt64Rec(Result).Bytes[2] := ReflectBits(UInt64Rec(Value).Bytes[5]);
UInt64Rec(Result).Bytes[3] := ReflectBits(UInt64Rec(Value).Bytes[4]);
UInt64Rec(Result).Bytes[4] := ReflectBits(UInt64Rec(Value).Bytes[3]);
UInt64Rec(Result).Bytes[5] := ReflectBits(UInt64Rec(Value).Bytes[2]);
UInt64Rec(Result).Bytes[6] := ReflectBits(UInt64Rec(Value).Bytes[1]);
UInt64Rec(Result).Bytes[7] := ReflectBits(UInt64Rec(Value).Bytes[0]);
end;

//------------------------------------------------------------------------------

Function ReflectByteBits(Value: TCRC64Sys): TCRC64Sys;
begin
UInt64Rec(Result).Bytes[0] := ReflectBits(UInt64Rec(Value).Bytes[0]);
UInt64Rec(Result).Bytes[1] := ReflectBits(UInt64Rec(Value).Bytes[1]);
UInt64Rec(Result).Bytes[2] := ReflectBits(UInt64Rec(Value).Bytes[2]);
UInt64Rec(Result).Bytes[3] := ReflectBits(UInt64Rec(Value).Bytes[3]);
UInt64Rec(Result).Bytes[4] := ReflectBits(UInt64Rec(Value).Bytes[4]);
UInt64Rec(Result).Bytes[5] := ReflectBits(UInt64Rec(Value).Bytes[5]);
UInt64Rec(Result).Bytes[6] := ReflectBits(UInt64Rec(Value).Bytes[6]);
UInt64Rec(Result).Bytes[7] := ReflectBits(UInt64Rec(Value).Bytes[7]);
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
--------------------------------------------------------------------------------
                                 TCRC64BaseHash
-------------------------------------------------------------------------------                                 
===============================================================================}
{===============================================================================
    TCRC64BaseHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC64BaseHash - protected methods
-------------------------------------------------------------------------------}

Function TCRC64BaseHash.GetCRC64: TCRC64;
begin
Result := CRC64FromSys(fCRC64Value);
end;

//------------------------------------------------------------------------------

Function TCRC64BaseHash.GetCRC64Poly: TCRC64Sys;
begin
Result := ReflectBits(GetCRC64PolyRef);
end;

//------------------------------------------------------------------------------

procedure TCRC64BaseHash.ProcessBuffer(const Buffer; Size: TMemSize);
var
  WorkCRC:  TCRC64Sys;
  i:        TMemSize;
  Buff:     PByte;
begin
WorkCRC := fCRC64Value;
Buff := @Buffer;
For i := 1 to Size do
  begin
    WorkCRC := fCRC64Table^[Byte(WorkCRC) xor Buff^] xor (WorkCRC shr 8);
    Inc(Buff);
  end;
fCRC64Value := WorkCRC;
end;

//------------------------------------------------------------------------------

procedure TCRC64BaseHash.Initialize;
begin
inherited;
fCRC64Value := 0;
InitializeTable;
end;

//------------------------------------------------------------------------------

procedure TCRC64BaseHash.Finalize;
begin
FinalizeTable;
inherited;
end;

{-------------------------------------------------------------------------------
    TCRC64BaseHash - public methods
-------------------------------------------------------------------------------}

class Function TCRC64BaseHash.CRC64ToSys(CRC64: TCRC64): TCRC64Sys;
begin
Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TCRC64Sys(CRC64));
end;

//------------------------------------------------------------------------------

class Function TCRC64BaseHash.CRC64FromSys(CRC64: TCRC64Sys): TCRC64;
begin
Result := TCRC64({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(CRC64));
end;

//------------------------------------------------------------------------------

class Function TCRC64BaseHash.CRC64ToLE(CRC64: TCRC64): TCRC64;
begin
Result := CRC64;
end;

//------------------------------------------------------------------------------

class Function TCRC64BaseHash.CRC64ToBE(CRC64: TCRC64): TCRC64;
begin
Result := SwapEndian(CRC64);
end;

//------------------------------------------------------------------------------

class Function TCRC64BaseHash.CRC64FromLE(CRC64: TCRC64): TCRC64;
begin
Result := CRC64;
end;

//------------------------------------------------------------------------------

class Function TCRC64BaseHash.CRC64FromBE(CRC64: TCRC64): TCRC64;
begin
Result := SwapEndian(CRC64);
end;

//------------------------------------------------------------------------------

class Function TCRC64BaseHash.HashSize: TMemSize;
begin
Result := SizeOf(TCRC64);
end;

//------------------------------------------------------------------------------

class Function TCRC64BaseHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TCRC64BaseHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TCRC64BaseHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCRC64BaseHash then
  fCRC64Value := TCRC64BaseHash(Hash).CRC64Sys
else
  raise ECRC64IncompatibleClass.CreateFmt('TCRC64BaseHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC64BaseHash.CreateAndInitFrom(Hash: TCRC64);
begin
CreateAndInit;
fCRC64Value := CRC64ToSys(Hash);
end;

//------------------------------------------------------------------------------

Function TCRC64BaseHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TCRC64BaseHash then
  Result := CompareUInt64(fCRC64Value,TCRC64BaseHash(Hash).CRC64Sys)
else
  raise ECRC64IncompatibleClass.CreateFmt('TCRC64BaseHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCRC64BaseHash.AsString: String;
begin
Result := IntToHex(fCRC64Value,16);
end;

//------------------------------------------------------------------------------

procedure TCRC64BaseHash.FromString(const Str: String);
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      fCRC64Value := TCRC64Sys(StrToInt64(Str))
    else
      fCRC64Value := TCRC64Sys(StrToInt64('$' + Str));
  end
else fCRC64Value := CRC64ToSys(ZeroCRC64);
end;

//------------------------------------------------------------------------------

procedure TCRC64BaseHash.FromStringDef(const Str: String; const Default: TCRC64);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fCRC64Value := CRC64ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TCRC64BaseHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRC64;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}CRC64ToBE{$ELSE}CRC64ToLE{$ENDIF}(CRC64FromSys(fCRC64Value));
  heLittle: Temp := CRC64ToLE(CRC64FromSys(fCRC64Value));
  heBig:    Temp := CRC64ToBE(CRC64FromSys(fCRC64Value));
else
 {heDefault}
  Temp := CRC64FromSys(fCRC64Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TCRC64));
end;

//------------------------------------------------------------------------------

procedure TCRC64BaseHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRC64;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(TCRC64));
case Endianness of
  heSystem: fCRC64Value := CRC64ToSys({$IFDEF ENDIAN_BIG}CRC64FromBE{$ELSE}CRC64FromLE{$ENDIF}(Temp));
  heLittle: fCRC64Value := CRC64ToSys(CRC64FromLE(Temp));
  heBig:    fCRC64Value := CRC64ToSys(CRC64FromBE(Temp));
else
 {heDefault}
  fCRC64Value := CRC64ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TCRC64Hash
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC64_POLYREF: TCRC64Sys = TCRC64Sys($C96C5795D7870F42);

  CRC64_TABLE: TCRC64Table = (
    TCRC64Sys($0000000000000000), TCRC64Sys($9336EAA9EBE1F042),
    TCRC64Sys($266DD453D7C3E185), TCRC64Sys($B55B3EFA3C2211C7),
    TCRC64Sys($DFEC420E45663349), TCRC64Sys($4CDAA8A7AE87C30B),
    TCRC64Sys($F981965D92A5D2CC), TCRC64Sys($6AB77CF47944228E),
    TCRC64Sys($BED9851C8ACC6692), TCRC64Sys($2DEF6FB5612D96D0),
    TCRC64Sys($98B4514F5D0F8717), TCRC64Sys($0B82BBE6B6EE7755),
    TCRC64Sys($6135C712CFAA55DB), TCRC64Sys($F2032DBB244BA599),
    TCRC64Sys($475813411869B45E), TCRC64Sys($D46EF9E8F388441C),
    TCRC64Sys($EF85E190FF783D66), TCRC64Sys($7CB30B391499CD24),
    TCRC64Sys($C9E835C328BBDCE3), TCRC64Sys($5ADEDF6AC35A2CA1),
    TCRC64Sys($3069A39EBA1E0E2F), TCRC64Sys($A35F493751FFFE6D),
    TCRC64Sys($160477CD6DDDEFAA), TCRC64Sys($85329D64863C1FE8),
    TCRC64Sys($515C648C75B45BF4), TCRC64Sys($C26A8E259E55ABB6),
    TCRC64Sys($7731B0DFA277BA71), TCRC64Sys($E4075A7649964A33),
    TCRC64Sys($8EB0268230D268BD), TCRC64Sys($1D86CC2BDB3398FF),
    TCRC64Sys($A8DDF2D1E7118938), TCRC64Sys($3BEB18780CF0797A),
    TCRC64Sys($DE0BC321FFF17ACC), TCRC64Sys($4D3D298814108A8E),
    TCRC64Sys($F866177228329B49), TCRC64Sys($6B50FDDBC3D36B0B),
    TCRC64Sys($01E7812FBA974985), TCRC64Sys($92D16B865176B9C7),
    TCRC64Sys($278A557C6D54A800), TCRC64Sys($B4BCBFD586B55842),
    TCRC64Sys($60D2463D753D1C5E), TCRC64Sys($F3E4AC949EDCEC1C),
    TCRC64Sys($46BF926EA2FEFDDB), TCRC64Sys($D58978C7491F0D99),
    TCRC64Sys($BF3E0433305B2F17), TCRC64Sys($2C08EE9ADBBADF55),
    TCRC64Sys($9953D060E798CE92), TCRC64Sys($0A653AC90C793ED0),
    TCRC64Sys($318E22B1008947AA), TCRC64Sys($A2B8C818EB68B7E8),
    TCRC64Sys($17E3F6E2D74AA62F), TCRC64Sys($84D51C4B3CAB566D),
    TCRC64Sys($EE6260BF45EF74E3), TCRC64Sys($7D548A16AE0E84A1),
    TCRC64Sys($C80FB4EC922C9566), TCRC64Sys($5B395E4579CD6524),
    TCRC64Sys($8F57A7AD8A452138), TCRC64Sys($1C614D0461A4D17A),
    TCRC64Sys($A93A73FE5D86C0BD), TCRC64Sys($3A0C9957B66730FF),
    TCRC64Sys($50BBE5A3CF231271), TCRC64Sys($C38D0F0A24C2E233),
    TCRC64Sys($76D631F018E0F3F4), TCRC64Sys($E5E0DB59F30103B6),
    TCRC64Sys($2F216CEA150205DA), TCRC64Sys($BC178643FEE3F598),
    TCRC64Sys($094CB8B9C2C1E45F), TCRC64Sys($9A7A52102920141D),
    TCRC64Sys($F0CD2EE450643693), TCRC64Sys($63FBC44DBB85C6D1),
    TCRC64Sys($D6A0FAB787A7D716), TCRC64Sys($4596101E6C462754),
    TCRC64Sys($91F8E9F69FCE6348), TCRC64Sys($02CE035F742F930A),
    TCRC64Sys($B7953DA5480D82CD), TCRC64Sys($24A3D70CA3EC728F),
    TCRC64Sys($4E14ABF8DAA85001), TCRC64Sys($DD2241513149A043),
    TCRC64Sys($68797FAB0D6BB184), TCRC64Sys($FB4F9502E68A41C6),
    TCRC64Sys($C0A48D7AEA7A38BC), TCRC64Sys($539267D3019BC8FE),
    TCRC64Sys($E6C959293DB9D939), TCRC64Sys($75FFB380D658297B),
    TCRC64Sys($1F48CF74AF1C0BF5), TCRC64Sys($8C7E25DD44FDFBB7),
    TCRC64Sys($39251B2778DFEA70), TCRC64Sys($AA13F18E933E1A32),
    TCRC64Sys($7E7D086660B65E2E), TCRC64Sys($ED4BE2CF8B57AE6C),
    TCRC64Sys($5810DC35B775BFAB), TCRC64Sys($CB26369C5C944FE9),
    TCRC64Sys($A1914A6825D06D67), TCRC64Sys($32A7A0C1CE319D25),
    TCRC64Sys($87FC9E3BF2138CE2), TCRC64Sys($14CA749219F27CA0),
    TCRC64Sys($F12AAFCBEAF37F16), TCRC64Sys($621C456201128F54),
    TCRC64Sys($D7477B983D309E93), TCRC64Sys($44719131D6D16ED1),
    TCRC64Sys($2EC6EDC5AF954C5F), TCRC64Sys($BDF0076C4474BC1D),
    TCRC64Sys($08AB39967856ADDA), TCRC64Sys($9B9DD33F93B75D98),
    TCRC64Sys($4FF32AD7603F1984), TCRC64Sys($DCC5C07E8BDEE9C6),
    TCRC64Sys($699EFE84B7FCF801), TCRC64Sys($FAA8142D5C1D0843),
    TCRC64Sys($901F68D925592ACD), TCRC64Sys($03298270CEB8DA8F),
    TCRC64Sys($B672BC8AF29ACB48), TCRC64Sys($25445623197B3B0A),
    TCRC64Sys($1EAF4E5B158B4270), TCRC64Sys($8D99A4F2FE6AB232),
    TCRC64Sys($38C29A08C248A3F5), TCRC64Sys($ABF470A129A953B7),
    TCRC64Sys($C1430C5550ED7139), TCRC64Sys($5275E6FCBB0C817B),
    TCRC64Sys($E72ED806872E90BC), TCRC64Sys($741832AF6CCF60FE),
    TCRC64Sys($A076CB479F4724E2), TCRC64Sys($334021EE74A6D4A0),
    TCRC64Sys($861B1F144884C567), TCRC64Sys($152DF5BDA3653525),
    TCRC64Sys($7F9A8949DA2117AB), TCRC64Sys($ECAC63E031C0E7E9),
    TCRC64Sys($59F75D1A0DE2F62E), TCRC64Sys($CAC1B7B3E603066C),
    TCRC64Sys($CD74327DC0E5FAF6), TCRC64Sys($5E42D8D42B040AB4),
    TCRC64Sys($EB19E62E17261B73), TCRC64Sys($782F0C87FCC7EB31),
    TCRC64Sys($129870738583C9BF), TCRC64Sys($81AE9ADA6E6239FD),
    TCRC64Sys($34F5A4205240283A), TCRC64Sys($A7C34E89B9A1D878),
    TCRC64Sys($73ADB7614A299C64), TCRC64Sys($E09B5DC8A1C86C26),
    TCRC64Sys($55C063329DEA7DE1), TCRC64Sys($C6F6899B760B8DA3),
    TCRC64Sys($AC41F56F0F4FAF2D), TCRC64Sys($3F771FC6E4AE5F6F),
    TCRC64Sys($8A2C213CD88C4EA8), TCRC64Sys($191ACB95336DBEEA),
    TCRC64Sys($22F1D3ED3F9DC790), TCRC64Sys($B1C73944D47C37D2),
    TCRC64Sys($049C07BEE85E2615), TCRC64Sys($97AAED1703BFD657),
    TCRC64Sys($FD1D91E37AFBF4D9), TCRC64Sys($6E2B7B4A911A049B),
    TCRC64Sys($DB7045B0AD38155C), TCRC64Sys($4846AF1946D9E51E),
    TCRC64Sys($9C2856F1B551A102), TCRC64Sys($0F1EBC585EB05140),
    TCRC64Sys($BA4582A262924087), TCRC64Sys($2973680B8973B0C5),
    TCRC64Sys($43C414FFF037924B), TCRC64Sys($D0F2FE561BD66209),
    TCRC64Sys($65A9C0AC27F473CE), TCRC64Sys($F69F2A05CC15838C),
    TCRC64Sys($137FF15C3F14803A), TCRC64Sys($80491BF5D4F57078),
    TCRC64Sys($3512250FE8D761BF), TCRC64Sys($A624CFA6033691FD),
    TCRC64Sys($CC93B3527A72B373), TCRC64Sys($5FA559FB91934331),
    TCRC64Sys($EAFE6701ADB152F6), TCRC64Sys($79C88DA84650A2B4),
    TCRC64Sys($ADA67440B5D8E6A8), TCRC64Sys($3E909EE95E3916EA),
    TCRC64Sys($8BCBA013621B072D), TCRC64Sys($18FD4ABA89FAF76F),
    TCRC64Sys($724A364EF0BED5E1), TCRC64Sys($E17CDCE71B5F25A3),
    TCRC64Sys($5427E21D277D3464), TCRC64Sys($C71108B4CC9CC426),
    TCRC64Sys($FCFA10CCC06CBD5C), TCRC64Sys($6FCCFA652B8D4D1E),
    TCRC64Sys($DA97C49F17AF5CD9), TCRC64Sys($49A12E36FC4EAC9B),
    TCRC64Sys($231652C2850A8E15), TCRC64Sys($B020B86B6EEB7E57),
    TCRC64Sys($057B869152C96F90), TCRC64Sys($964D6C38B9289FD2),
    TCRC64Sys($422395D04AA0DBCE), TCRC64Sys($D1157F79A1412B8C),
    TCRC64Sys($644E41839D633A4B), TCRC64Sys($F778AB2A7682CA09),
    TCRC64Sys($9DCFD7DE0FC6E887), TCRC64Sys($0EF93D77E42718C5),
    TCRC64Sys($BBA2038DD8050902), TCRC64Sys($2894E92433E4F940),
    TCRC64Sys($E2555E97D5E7FF2C), TCRC64Sys($7163B43E3E060F6E),
    TCRC64Sys($C4388AC402241EA9), TCRC64Sys($570E606DE9C5EEEB),
    TCRC64Sys($3DB91C999081CC65), TCRC64Sys($AE8FF6307B603C27),
    TCRC64Sys($1BD4C8CA47422DE0), TCRC64Sys($88E22263ACA3DDA2),
    TCRC64Sys($5C8CDB8B5F2B99BE), TCRC64Sys($CFBA3122B4CA69FC),
    TCRC64Sys($7AE10FD888E8783B), TCRC64Sys($E9D7E57163098879),
    TCRC64Sys($836099851A4DAAF7), TCRC64Sys($1056732CF1AC5AB5),
    TCRC64Sys($A50D4DD6CD8E4B72), TCRC64Sys($363BA77F266FBB30),
    TCRC64Sys($0DD0BF072A9FC24A), TCRC64Sys($9EE655AEC17E3208),
    TCRC64Sys($2BBD6B54FD5C23CF), TCRC64Sys($B88B81FD16BDD38D),
    TCRC64Sys($D23CFD096FF9F103), TCRC64Sys($410A17A084180141),
    TCRC64Sys($F451295AB83A1086), TCRC64Sys($6767C3F353DBE0C4),
    TCRC64Sys($B3093A1BA053A4D8), TCRC64Sys($203FD0B24BB2549A),
    TCRC64Sys($9564EE487790455D), TCRC64Sys($065204E19C71B51F),
    TCRC64Sys($6CE57815E5359791), TCRC64Sys($FFD392BC0ED467D3),
    TCRC64Sys($4A88AC4632F67614), TCRC64Sys($D9BE46EFD9178656),
    TCRC64Sys($3C5E9DB62A1685E0), TCRC64Sys($AF68771FC1F775A2),
    TCRC64Sys($1A3349E5FDD56465), TCRC64Sys($8905A34C16349427),
    TCRC64Sys($E3B2DFB86F70B6A9), TCRC64Sys($70843511849146EB),
    TCRC64Sys($C5DF0BEBB8B3572C), TCRC64Sys($56E9E1425352A76E),
    TCRC64Sys($828718AAA0DAE372), TCRC64Sys($11B1F2034B3B1330),
    TCRC64Sys($A4EACCF9771902F7), TCRC64Sys($37DC26509CF8F2B5),
    TCRC64Sys($5D6B5AA4E5BCD03B), TCRC64Sys($CE5DB00D0E5D2079),
    TCRC64Sys($7B068EF7327F31BE), TCRC64Sys($E830645ED99EC1FC),
    TCRC64Sys($D3DB7C26D56EB886), TCRC64Sys($40ED968F3E8F48C4),
    TCRC64Sys($F5B6A87502AD5903), TCRC64Sys($668042DCE94CA941),
    TCRC64Sys($0C373E2890088BCF), TCRC64Sys($9F01D4817BE97B8D),
    TCRC64Sys($2A5AEA7B47CB6A4A), TCRC64Sys($B96C00D2AC2A9A08),
    TCRC64Sys($6D02F93A5FA2DE14), TCRC64Sys($FE341393B4432E56),
    TCRC64Sys($4B6F2D6988613F91), TCRC64Sys($D859C7C06380CFD3),
    TCRC64Sys($B2EEBB341AC4ED5D), TCRC64Sys($21D8519DF1251D1F),
    TCRC64Sys($94836F67CD070CD8), TCRC64Sys($07B585CE26E6FC9A));

{===============================================================================
    TCRC64Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC64Hash - protected methods
-------------------------------------------------------------------------------}

Function TCRC64Hash.GetCRC64PolyRef: TCRC64Sys;
begin
Result := CRC64_POLYREF;
end;

//------------------------------------------------------------------------------

procedure TCRC64Hash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fCRC64Value := SwapEndian(fCRC64Value);
inherited ProcessBuffer(Buffer,Size);
fCRC64Value := SwapEndian(fCRC64Value);
end;

//------------------------------------------------------------------------------

procedure TCRC64Hash.InitializeTable;
begin
fCRC64Table := @CRC64_TABLE;
end;

//------------------------------------------------------------------------------

procedure TCRC64Hash.FinalizeTable;
begin
fCRC64Table := nil;
end;

{-------------------------------------------------------------------------------
    TCRC64Hash - public methods
-------------------------------------------------------------------------------}

class Function TCRC64Hash.HashName: String;
begin
Result := 'CRC-64';
end;

//------------------------------------------------------------------------------

class Function TCRC64Hash.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

procedure TCRC64Hash.Init;
begin
inherited;
fCRC64Value := CRC64ToSys(InitialCRC64);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TCRC64CustomHash
--------------------------------------------------------------------------------
===============================================================================}
const
  CRC64_CUSTOM_DEFNAME = 'CRC-64(custom)';

{===============================================================================
    TCRC64CustomHash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TCRC64CustomHash - protected methods
-------------------------------------------------------------------------------}

procedure TCRC64CustomHash.SetCRC64Poly(Value: TCRC64Sys);
begin
SetCRC64PolyRef(ReflectBits(Value));
end;

//------------------------------------------------------------------------------

Function TCRC64CustomHash.GetCRC64PolyRef: TCRC64Sys;
begin
Result := fCRC64Poly;
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.SetCRC64PolyRef(Value: TCRC64Sys);
begin
If fCRC64Poly <> Value then
  begin
    fName := CRC64_CUSTOM_DEFNAME;
    fCRC64Poly := Value;
    BuildTable;
    // invalidate running computations
    If fInitialized and not fFinalized then
      fInitialized := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.SetInitialValue(Value: TCRC64);
begin
If CRC64ToSys(Value) <> CRC64ToSys(fInitialValue) then
  begin
    fName := CRC64_CUSTOM_DEFNAME;
    fInitialValue := Value;
    If fInitialized and not fFinalized then
      fInitialized := False;    
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.SetReflectIn(Value: Boolean);
begin
If Value <> fReflectIn then
  begin
    fName := CRC64_CUSTOM_DEFNAME;
    fReflectIn := Value;
    BuildTable;
    If fInitialized and not fFinalized then
      fInitialized := False;    
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.SetReflectOut(Value: Boolean);
begin
If Value <> fReflectOut then
  begin
    fName := CRC64_CUSTOM_DEFNAME;
    fReflectOut := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.SetXOROutValue(Value: TCRC64);
begin
If CRC64ToSys(Value) <> CRC64ToSys(fXOROutValue) then
  begin
    fName := CRC64_CUSTOM_DEFNAME;
    fXOROutValue := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.BuildTable;
var
  i,j:  Integer;
  Temp: TCRC64Sys;
begin
For i := Low(TCRC64Table) to High(TCRC64Table) do
  begin
    If fReflectIn then
      Temp := TCRC64Sys(i) shl 1
    else
      Temp := TCRC64Sys(ReflectBits(UInt8(i))) shl 1;
    For j := 8 downto 0 do
      begin
        If (Temp and 1) <> 0 then
          Temp := (Temp shr 1) xor fCRC64Poly
        else
          Temp := Temp shr 1;
      end;
    If fReflectIn then
      fCRC64Table^[UInt8(i)] := Temp
    else
      fCRC64Table^[UInt8(i)] := ReflectByteBits(Temp);
  end;
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.InitializeTable;
begin
New(fCRC64Table);
BuildTable;
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.FinalizeTable;
begin
Dispose(fCRC64Table);
fCRC64Table := nil;
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.Initialize;
begin
fName := CRC64_KNOWN_PRESETS[CRC64_DEFAULT_PRESET_IDX].Name;
fCRC64Poly := CRC64_KNOWN_PRESETS[CRC64_DEFAULT_PRESET_IDX].RefPolynomial;
fInitialValue := CRC64_KNOWN_PRESETS[CRC64_DEFAULT_PRESET_IDX].InitialValue;
fReflectIn := CRC64_KNOWN_PRESETS[CRC64_DEFAULT_PRESET_IDX].ReflectIn;
fReflectOut := CRC64_KNOWN_PRESETS[CRC64_DEFAULT_PRESET_IDX].ReflectOut;
fXOROutValue := CRC64_KNOWN_PRESETS[CRC64_DEFAULT_PRESET_IDX].XOROutValue;
inherited;
end;

{-------------------------------------------------------------------------------
    TCRC64CustomHash - public methods
-------------------------------------------------------------------------------}

Function TCRC64CustomHash.HashName: String;
begin
Result := fName;
end;

//------------------------------------------------------------------------------

constructor TCRC64CustomHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCRC64CustomHash then
  begin
    fName := TCRC64CustomHash(Hash).HashName;
    fCRC64Poly := TCRC64CustomHash(Hash).CRC64PolyRef;
    fInitialValue := TCRC64CustomHash(Hash).InitialValue;
    fReflectIn := TCRC64CustomHash(Hash).ReflectIn;
    fReflectOut := TCRC64CustomHash(Hash).ReflectOut;
    fXOROutValue := TCRC64CustomHash(Hash).XOROutValue;
    BuildTable;
  end
else raise ECRC64IncompatibleClass.CreateFmt('TCRC64CustomHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TCRC64CustomHash.CreateAndLoadPreset(Preset: TCRC64CustomPreset);
begin
Create;
LoadPreset(Preset);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC64CustomHash.CreateAndLoadPreset(PresetIndex: Integer);
begin
Create;
LoadPreset(PresetIndex);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRC64CustomHash.CreateAndLoadPreset(const PresetName: String);
begin
Create;
LoadPreset(PresetName);
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.LoadPreset(Preset: TCRC64CustomPreset);
begin
fName := Preset.Name;
fCRC64Poly := Preset.RefPolynomial;
fInitialValue := Preset.InitialValue;
fReflectIn := Preset.ReflectIn;
fReflectOut := Preset.ReflectOut;
fXOROutValue := Preset.XOROutValue;
BuildTable;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRC64CustomHash.LoadPreset(PresetIndex: Integer);
begin
If (PresetIndex >= Low(CRC64_KNOWN_PRESETS)) and (PresetIndex <= High(CRC64_KNOWN_PRESETS)) then
  LoadPreset(CRC64_KNOWN_PRESETS[PresetIndex])
else
  raise ECRC64IndexOutOfBounds.CreateFmt('TCRC64CustomHash.LoadPreset: Index (%d) out of bounds.',[PresetIndex]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRC64CustomHash.LoadPreset(const PresetName: String);
var
  Aliases:  TStringList;
  i:        Integer;
  Index:    Integer;
begin
Aliases := TStringList.Create;
try
  Aliases.CaseSensitive := False;
  For i := Low(CRC64_KNOWN_PRESETS) to High(CRC64_KNOWN_PRESETS) do
    begin
      If not AnsiSameText(CRC64_KNOWN_PRESETS[i].Name,PresetName) then
        begin
          SplitString(CRC64_KNOWN_PRESETS[i].Aliases,Aliases);
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
    LoadPreset(CRC64_KNOWN_PRESETS[Index]);
finally
  Aliases.Free;
end;
end;

//------------------------------------------------------------------------------

Function TCRC64CustomHash.SelfTest(Preset: TCRC64CustomPreset): Boolean;
var
  CodewordData: array of Byte;
  CodewordCRC:  TCRC64;

  Function DecodeCodeword(const Str: String): Boolean;
  var
    i:  TStrOff;
  begin
    Result := False;
    If Length(Str) >= (SizeOf(TCRC64) * 2) then
      begin
        CodewordData := nil;
        If (Length(Str) = 20) and AnsiSameStr(Copy(Str,1,2),'§0') then
          begin
            SetLength(CodewordData,4096 + SizeOf(TCRC64));
            CodewordData[0] := Byte(StrToInt('$' + Copy(Str,3,2)));
            For i := Succ(Low(CodewordData)) to High(CodewordData) do
              CodewordData[i] := CodewordData[0];
            For i := 0 to 7 do
              CodewordData[High(CodewordData) + i - 7] :=
                Byte(StrToInt('$' + Copy(Str,5 + (i * 2),2)));
          end
        else If (Length(Str) = 18) and AnsiSameStr(Copy(Str,1,2),'§1') then
          begin
            SetLength(CodewordData,4096 + SizeOf(TCRC64));
            For i := Low(CodewordData) to High(CodewordData) do
              CodewordData[i] := Byte(i);
            For i := 0 to 7 do
              CodewordData[High(CodewordData) + i - 7] :=
                Byte(StrToInt('$' + Copy(Str,3 + (i * 2),2)));
          end
        else If (Length(Str) = 18) and AnsiSameStr(Copy(Str,1,2),'§2') then
          begin
            SetLength(CodewordData,4096 + SizeOf(TCRC64));
            For i := Low(CodewordData) to High(CodewordData) do
              CodewordData[i] := Byte(4095 - i);
            For i := 0 to 7 do
              CodewordData[High(CodewordData) + i - 7] :=
                Byte(StrToInt('$' + Copy(Str,3 + (i * 2),2)));
          end
        else
          begin
            SetLength(CodewordData,Length(Str) div 2);
            For i := Low(CodewordData) to High(CodewordData) do
              CodewordData[i] := StrToInt('$' + Copy(Str,(i * 2) + 1,2));
          end;
        CodewordCRC := PCRC64(Addr(CodewordData[High(CodewordData) - Pred(SizeOf(TCRC64))]))^;
        If not fReflectOut then
          CodewordCRC := SwapEndian(CodewordCRC);
        Result := True;
      end;
  end;

var
  TempCRC:    TCRC64;
  TempStr:    AnsiString;
  Codewords:  TStringList;
  i:          Integer;
begin
Result := False;
LoadPreset(Preset);
HashAnsiString(AnsiString('123456789'));
If CRC64ToSys(Preset.Check) = fCRC64Value then
  begin
    // prepare check string with appended check CRC
    If fReflectOut then
      TempCRC := Preset.Check
    else
      TempCRC := SwapEndian(Preset.Check);
    TempStr := AnsiString('123456789') + AnsiString(StringOfChar('0',SizeOf(TCRC64)));
    Move(TempCRC,Addr(TempStr[10])^,SizeOf(TCRC64));
    HashAnsiString(TempStr);
    If CRC64ToSys(Preset.Residue) = (fCRC64Value xor CRC64ToSys(fXOROutValue)) then
      begin
        Codewords := TStringList.Create;
        try
          SplitString(Preset.Codewords,Codewords);
          // check codewords for crc
          For i := 0 to Pred(Codewords.Count) do
            If DecodeCodeword(Codewords[i]) then
              begin
                // check crc
                HashBuffer(CodewordData[0],Length(CodewordData) - SizeOf(TCRC64));
                If CRC64ToSys(CodewordCRC) <> fCRC64Value then
                  Exit;
                // check residue
                HashMemory(Addr(CodewordData[0]),Length(CodewordData));
                If CRC64ToSys(Preset.Residue) <> (fCRC64Value xor CRC64ToSys(fXOROutValue)) then
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

procedure TCRC64CustomHash.Init;
begin
inherited;
If fReflectIn then
  fCRC64Value := ReflectBits(CRC64ToSys(fInitialValue))
else
  fCRC64Value := SwapEndian(CRC64ToSys(fInitialValue));
end;

//------------------------------------------------------------------------------

procedure TCRC64CustomHash.Final;
begin
inherited;
If fReflectIn then
  begin
    If fReflectOut then
      fCRC64Value := fCRC64Value xor CRC64ToSys(fXOROutValue)
    else
      fCRC64Value := ReflectBits(fCRC64Value) xor CRC64ToSys(fXOROutValue);
  end
else
  begin
    If fReflectOut then
      fCRC64Value := ReflectByteBits(fCRC64Value) xor CRC64ToSys(fXOROutValue)
    else
      fCRC64Value := SwapEndian(fCRC64Value) xor CRC64ToSys(fXOROutValue);
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

Function CRC64ToStr(CRC64: TCRC64): String;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.CreateAndInitFrom(CRC64);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToCRC64(const Str: String): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function TryStrToCRC64(const Str: String; out CRC64: TCRC64): Boolean;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    CRC64 := Hash.CRC64;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToCRC64Def(const Str: String; Default: TCRC64): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function CompareCRC64(A,B: TCRC64): Integer;
var
  HashA:  TCRC64Hash;
  HashB:  TCRC64Hash;
begin
HashA := TCRC64Hash.CreateAndInitFrom(A);
try
  HashB := TCRC64Hash.CreateAndInitFrom(B);
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

Function SameCRC64(A,B: TCRC64): Boolean;
var
  HashA:  TCRC64Hash;
  HashB:  TCRC64Hash;
begin
HashA := TCRC64Hash.CreateAndInitFrom(A);
try
  HashB := TCRC64Hash.CreateAndInitFrom(B);
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
    Standalone functions - processing functions
-------------------------------------------------------------------------------}

Function BufferCRC64(CRC64: TCRC64; const Buffer; Size: TMemSize): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.CreateAndInitFrom(CRC64);
try
  Hash.Final(Buffer,Size);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function BufferCRC64(const Buffer; Size: TMemSize): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringCRC64(const Str: AnsiString): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Hash.HashAnsiString(Str);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function WideStringCRC64(const Str: WideString): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Hash.HashWideString(Str);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StringCRC64(const Str: String): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Hash.HashString(Str);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StreamCRC64(Stream: TStream; Count: Int64 = -1): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Hash.HashStream(Stream,Count);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileCRC64(const FileName: String): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Hash.HashFile(FileName);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Standalone functions - context functions
-------------------------------------------------------------------------------}

Function CRC64_Init: TCRC64Context;
var
  Temp: TCRC64Hash;
begin
Temp := TCRC64Hash.CreateAndInit;
Result := TCRC64Context(Temp);
end;

//------------------------------------------------------------------------------

procedure CRC64_Update(Context: TCRC64Context; const Buffer; Size: TMemSize);
begin
TCRC64Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function CRC64_Final(var Context: TCRC64Context; const Buffer; Size: TMemSize): TCRC64;
begin
CRC64_Update(Context,Buffer,Size);
Result := CRC64_Final(Context);
end;

//------------------------------------------------------------------------------

Function CRC64_Final(var Context: TCRC64Context): TCRC64;
begin
TCRC64Hash(Context).Final;
Result := TCRC64Hash(Context).CRC64;
FreeAndNil(TCRC64Hash(Context));
end;

//------------------------------------------------------------------------------

Function CRC64_Hash(const Buffer; Size: TMemSize): TCRC64;
var
  Hash: TCRC64Hash;
begin
Hash := TCRC64Hash.Create;
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.CRC64;
finally
  Hash.Free;
end;
end;

end.
