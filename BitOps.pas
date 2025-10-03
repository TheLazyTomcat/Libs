{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  BitOps

    Set of functions providing some of the not-so-common bit-manipulating
    operations and other binary utilities.

  Version 1.25.2 (2025-08-20)

  Last change 2025-10-03

  ©2014-2025 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.BitOps

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    BasicUIM      - github.com/TheLazyTomcat/Lib.BasicUIM
  * SimpleCPUID   - github.com/TheLazyTomcat/Lib.SimpleCPUID

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol BitOps_UseAuxExceptions for details).

  SimpleCPUID is required only when PurePascal symbol is not defined.

  Libraries AuxExceptions and SimpleCPUID might also be required as an indirect
  dependencies.

  Indirect dependencies:
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit BitOps;
{
  BitOps_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and only this unit will be compiled in PurePascal mode.
}
{$IFDEF BitOps_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{
  BitOps_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  BitOps_UseAuxExceptions to achieve this.
}
{$IF Defined(BitOps_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17} // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

//------------------------------------------------------------------------------

{
  UseLookupTable

  When defined, PopCount functions will, in their PurePascal version, use
  lookup table instead of testing each bit in a passed value.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol BitOps_UseLookupTable_Off.
}
{$DEFINE UseLookupTable}
{$IFDEF BitOps_UseLookupTable_Off}
  {$UNDEF UseLookupTable}
{$ENDIF}

{
  AllowASMExtensions

  Allows use of x86(-64) instruction extensions in ASM.
  When defined, availability of each extension is tested at unit initialization.
  The instructions are used only when proper extension is supported. When it is
  not, pascal form of the function is called instead.

  Currently used extensions:

    CMOV      - ParallelBitsDeposit(CMOVcc)[32b,p64]
    POPCNT    - PopCount, ParallelBitsExtract[32b,p64], ParallelBitsDeposit[32b,p64]
    LZCNT     - LZCount
    BMI1      - TZCount(TZCNT), ExtractBits(BEXTR)
    BMI2      - ParallelBitsExtract(PEXT), ParallelBitsDeposit(PDEP)

      [32b] = 32bit ASM variant of the function
      [p64] = function accepting 64bit values
      (ins) = ins is a specific used instruction from the extension set

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol BitOps_AllowASMExtensions_Off.
}
{$DEFINE AllowASMExtensions}
{$IFDEF BitOps_AllowASMExtensions_Off}
  {$UNDEF AllowASMExtensions}
{$ENDIF}

//------------------------------------------------------------------------------

// do not touch following...
{$IF not Defined(PurePascal) and Defined(AllowASMExtensions)}
  {$DEFINE ASM_Extensions}
{$ELSE}
  {$UNDEF ASM_Extensions}
{$IFEND}

interface

uses
  SysUtils,
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EBOException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EBOUnknownFunction     = class(EBOException);
  EBONoImplementation    = class(EBOException);
  EBOInvalidValue        = class(EBOException);
  EBOUnsupportedPlatform = class(EBOException);

  EBOConversionError  = class(EBOException);
  EBOInvalidCharacter = class(EBOConversionError);
  EBOBufferTooSmall   = class(EBOConversionError);
  EBOSizeMismatch     = class(EBOConversionError);

{===============================================================================
--------------------------------------------------------------------------------

                       Binary data <-> string conversions

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                   Integer number <-> Bit string conversions
================================================================================
-------------------------------------------------------------------------------}
{
  Following functions are converting numbers (unsigned integers) to their bit
  string representations and back. Bit string here means a string, where each
  character represents one particular bit from the number.

  The resulting string is ordered from the most significant (highest) bit (on
  the left side) to the least significant (lowest) bit (on the right side).

  The string can be split into groups of bits when a better readability is
  required, and both bit characters and splitting character can be user defined.
}
type
  TBitStringSplit = (bssNone,bss4bits,bss8bits,bss16bits,bss32bits);

  TBitStringFormat = record
    Split:        TBitStringSplit;
    SetBitChar:   Char;
    ZeroBitChar:  Char;
    SplitChar:    Char;
  end;

const
  DefBitStringFormat: TBitStringFormat = (
    Split:        bssNone;
    SetBitChar:   '1';
    ZeroBitChar:  '0';
    SplitChar:    ' ');

//------------------------------------------------------------------------------

Function NumberToBitStr(Number: UInt8; BitStringFormat: TBitStringFormat): String; overload;
Function NumberToBitStr(Number: UInt16; BitStringFormat: TBitStringFormat): String; overload;
Function NumberToBitStr(Number: UInt32; BitStringFormat: TBitStringFormat): String; overload;
Function NumberToBitStr(Number: UInt64; BitStringFormat: TBitStringFormat): String; overload;

Function NumberToBitStr(Number: UInt8; Split: TBitStringSplit): String; overload;
Function NumberToBitStr(Number: UInt16; Split: TBitStringSplit): String; overload;
Function NumberToBitStr(Number: UInt32; Split: TBitStringSplit): String; overload;
Function NumberToBitStr(Number: UInt64; Split: TBitStringSplit): String; overload;

Function NumberToBitStr(Number: UInt8): String; overload;
Function NumberToBitStr(Number: UInt16): String; overload;
Function NumberToBitStr(Number: UInt32): String; overload;
Function NumberToBitStr(Number: UInt64): String; overload;

//------------------------------------------------------------------------------

Function BitStrToNumber(const BitString: String; BitStringFormat: TBitStringFormat): UInt64; overload;
Function BitStrToNumber(const BitString: String; Split: TBitStringSplit): UInt64; overload;
Function BitStrToNumber(const BitString: String): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function TryBitStrToNumber(const BitString: String; out Value: UInt8; BitStringFormat: TBitStringFormat): Boolean; overload;
Function TryBitStrToNumber(const BitString: String; out Value: UInt16; BitStringFormat: TBitStringFormat): Boolean; overload;
Function TryBitStrToNumber(const BitString: String; out Value: UInt32; BitStringFormat: TBitStringFormat): Boolean; overload;
Function TryBitStrToNumber(const BitString: String; out Value: UInt64; BitStringFormat: TBitStringFormat): Boolean; overload;

Function TryBitStrToNumber(const BitString: String; out Value: UInt8; Split: TBitStringSplit): Boolean; overload;
Function TryBitStrToNumber(const BitString: String; out Value: UInt16; Split: TBitStringSplit): Boolean; overload;
Function TryBitStrToNumber(const BitString: String; out Value: UInt32; Split: TBitStringSplit): Boolean; overload;
Function TryBitStrToNumber(const BitString: String; out Value: UInt64; Split: TBitStringSplit): Boolean; overload;

Function TryBitStrToNumber(const BitString: String; out Value: UInt8): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function TryBitStrToNumber(const BitString: String; out Value: UInt16): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function TryBitStrToNumber(const BitString: String; out Value: UInt32): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function TryBitStrToNumber(const BitString: String; out Value: UInt64): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function BitStrToNumberDef(const BitString: String; Default: UInt64; BitStringFormat: TBitStringFormat): UInt64; overload;
Function BitStrToNumberDef(const BitString: String; Default: UInt64; Split: TBitStringSplit): UInt64; overload;
Function BitStrToNumberDef(const BitString: String; Default: UInt64): UInt64; overload;

{-------------------------------------------------------------------------------
================================================================================
                   Integer number <-> Octal string conversions
================================================================================
-------------------------------------------------------------------------------}
{
  Following functions are converting unsigned numbers to their octal (base-8)
  representation and back.
}

Function NumberToOctStr(Number: UInt8): String; overload;
Function NumberToOctStr(Number: UInt16): String; overload;
Function NumberToOctStr(Number: UInt32): String; overload;
Function NumberToOctStr(Number: UInt64): String; overload;

//------------------------------------------------------------------------------

Function OctStrToNumber(const OctString: String): UInt64;

//------------------------------------------------------------------------------

Function TryOctStrToNumber(const OctString: String; out Value: UInt8): Boolean; overload;
Function TryOctStrToNumber(const OctString: String; out Value: UInt16): Boolean; overload;
Function TryOctStrToNumber(const OctString: String; out Value: UInt32): Boolean; overload;
Function TryOctStrToNumber(const OctString: String; out Value: UInt64): Boolean; overload;

//------------------------------------------------------------------------------

Function OctStrToNumberDef(const OctString: String; Default: UInt64): UInt64;

{-------------------------------------------------------------------------------
================================================================================
                     General data <-> Hex string conversions
================================================================================
-------------------------------------------------------------------------------}
{
  Functions converting general data (buffer or array of bytes) to hexadecimal
  representation and back.

  The resulting string contains pair of hexadecimal numbers (0..9,a..f), where
  each pair represents one byte. These pairs appear in the order they have in
  the source data (first byte is to the left in the string, last byte is to
  the right). But note that the pairs themselwes are ordered from from most
  significant nibble to the least significant (ie. left character represents
  higher four bits of the byte, right character represents lower four bits).

  The string can be split for better readability, you can select splitting
  character and whether the hexadecimal chars will be of lower or upper case.
}

type
  THexStringSplit = (hssNone,hssNibble,hssByte,hssWord,hss24bits,hssLong,
                     hssQuad,hss80bits,hssOcta);

  THexStringFormat = record
    Split:      THexStringSplit;
    SplitChar:  Char;
    UpperCase:  Boolean;
  end;

const
  DefHexStringFormat: THexStringFormat = (
    Split:      hssNone;
    SplitChar:  ' ';
    UpperCase:  True);

type
  TArrayOfBytes = packed array of UInt8;

//------------------------------------------------------------------------------

Function DataToHexStr(const Buffer; Size: TMemSize; HexStringFormat: THexStringFormat): String; overload;
Function DataToHexStr(Arr: array of UInt8; HexStringFormat: THexStringFormat): String; overload;

Function DataToHexStr(const Buffer; Size: TMemSize; Split: THexStringSplit): String; overload;
Function DataToHexStr(Arr: array of UInt8; Split: THexStringSplit): String; overload;

Function DataToHexStr(const Buffer; Size: TMemSize): String; overload;
Function DataToHexStr(Arr: array of UInt8): String; overload;

//------------------------------------------------------------------------------

Function HexStrToData(const Str: String; out Buffer; Size: TMemSize; HexStringFormat: THexStringFormat): TMemSize; overload;
Function HexStrToData(const Str: String; HexStringFormat: THexStringFormat): TArrayOfBytes; overload;

Function HexStrToData(const Str: String; out Buffer; Size: TMemSize; SplitChar: Char): TMemSize; overload;
Function HexStrToData(const Str: String; SplitChar: Char): TArrayOfBytes; overload;

Function HexStrToData(const Str: String; out Buffer; Size: TMemSize): TMemSize; overload;
Function HexStrToData(const Str: String): TArrayOfBytes; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function TryHexStrToData(const Str: String; out Buffer; var Size: TMemSize; HexStringFormat: THexStringFormat): Boolean; overload;
Function TryHexStrToData(const Str: String; out Arr: TArrayOfBytes; HexStringFormat: THexStringFormat): Boolean; overload;

Function TryHexStrToData(const Str: String; out Buffer; var Size: TMemSize; SplitChar: Char): Boolean; overload;
Function TryHexStrToData(const Str: String; out Arr: TArrayOfBytes; SplitChar: Char): Boolean; overload;

Function TryHexStrToData(const Str: String; out Buffer; var Size: TMemSize): Boolean; overload;
Function TryHexStrToData(const Str: String; out Arr: TArrayOfBytes): Boolean; overload;{$IFDEF CanInline} inline; {$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                     General data <-> Bit string conversions
================================================================================
-------------------------------------------------------------------------------}
{
  Conversion of general data to its bit string representation and back.

  The resulting string can be split for better readability and the splitting
  character can be selected.

  It is possible to define order in which the bytes appear in the bit string
  as well as order of bits within the individual bytes (groups of eight
  chracters). Let's have an example:

    Data(hex): 82 A3

      bytes L2R, bits L2R ... 01000001 11000101
      bytes L2R, bits R2L ... 10000010 10100011
      bytes R2L, bits L2R ... 11000101 01000001
      bytes R2L, bits R2L ... 10100011 10000010
}

type
  TBitStringOrder = (bsoLeftToRight,bsoRightToLeft);

  TDataBitStringFormat = record
    Split:            TBitStringSplit;
    SplitChar:        Char;
    BytesOrder:       TBitStringOrder;
    BitsInByteOrder:  TBitStringOrder;
  end;

const
  DefDataBitStringFormat: TDataBitStringFormat = (
    Split:            bssNone;
    SplitChar:        ' ';
    BytesOrder:       bsoLeftToRight;
    BitsInByteOrder:  bsoLeftToRight);

//------------------------------------------------------------------------------

Function DataToBitStr(const Buffer; Size: TMemSize; BitStringFormat: TDataBitStringFormat): String; overload;
Function DataToBitStr(Arr: array of UInt8; BitStringFormat: TDataBitStringFormat): String; overload;

Function DataToBitStr(const Buffer; Size: TMemSize; Split: TBitStringSplit): String; overload;
Function DataToBitStr(Arr: array of UInt8; Split: TBitStringSplit): String; overload;

Function DataToBitStr(const Buffer; Size: TMemSize): String; overload;
Function DataToBitStr(Arr: array of UInt8): String; overload;

//------------------------------------------------------------------------------

Function BitStrToData(const Str: String; out Buffer; Size: TMemSize; BitStringFormat: TDataBitStringFormat): TMemSize; overload;
Function BitStrToData(const Str: String; BitStringFormat: TDataBitStringFormat): TArrayOfBytes; overload;

Function BitStrToData(const Str: String; out Buffer; Size: TMemSize; SplitChar: Char): TMemSize; overload;
Function BitStrToData(const Str: String; SplitChar: Char): TArrayOfBytes; overload;

Function BitStrToData(const Str: String; out Buffer; Size: TMemSize): TMemSize; overload;
Function BitStrToData(const Str: String): TArrayOfBytes; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function TryBitStrToData(const Str: String; out Buffer; var Size: TMemSize; BitStringFormat: TDataBitStringFormat): Boolean; overload;
Function TryBitStrToData(const Str: String; out Arr: TArrayOfBytes; BitStringFormat: TDataBitStringFormat): Boolean; overload;

Function TryBitStrToData(const Str: String; out Buffer; var Size: TMemSize; SplitChar: Char): Boolean; overload;
Function TryBitStrToData(const Str: String; out Arr: TArrayOfBytes; SplitChar: Char): Boolean; overload;

Function TryBitStrToData(const Str: String; out Buffer; var Size: TMemSize): Boolean; overload;
Function TryBitStrToData(const Str: String; out Arr: TArrayOfBytes): Boolean; overload;{$IFDEF CanInline} inline; {$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------

                             Bit-level manipulations

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                                Rotate left (ROL)
================================================================================
-------------------------------------------------------------------------------}
{
  Rotates the number left - that is, the number is shifted left (towards higher
  places) by a selected amount of bits, while the shifted-out bits are inserted
  to the right (lower places).

  Assembly implementation uses instruction ROL.

  Only lower 3, 4, 5 or 6 bits of the shift, depending on the argument size,
  are used (the ASM version masks 5 or 6 bits, but the result is the same).
}

Function ROL(Value: UInt8; Shift: Integer): UInt8; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function ROL(Value: UInt16; Shift: Integer): UInt16; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function ROL(Value: UInt32; Shift: Integer): UInt32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function ROL(Value: UInt64; Shift: Integer): UInt64; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

procedure ROLValue(var Value: UInt8; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure ROLValue(var Value: UInt16; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure ROLValue(var Value: UInt32; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure ROLValue(var Value: UInt64; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                               Rotate right (ROR)
================================================================================
-------------------------------------------------------------------------------}
{
  Rotates the number right - the number is shifted right (towards lower places)
  by a selected amount of bits, while the shifted-out bits are inserted to the
  left (higher places).

  Assembly implementation uses instruction ROR.

  Only lower 3, 4, 5 or 6 bits of the shift, depending on the argument size,
  are used (the ASM version masks 5 or 6 bits, but the result is the same).
}

Function ROR(Value: UInt8; Shift: Integer): UInt8; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function ROR(Value: UInt16; Shift: Integer): UInt16; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function ROR(Value: UInt32; Shift: Integer): UInt32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function ROR(Value: UInt64; Shift: Integer): UInt64; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

procedure RORValue(var Value: UInt8; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RORValue(var Value: UInt16; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RORValue(var Value: UInt32; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RORValue(var Value: UInt64; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                          Rotate left with carry (RCL)
================================================================================
-------------------------------------------------------------------------------}
{
  Rotates the number left with carry - works the same as ROL, but the number
  is being rotated trough carry flag, effectively making it n + 1 bits wide.
  When the number is shifted, the bit shifted in is taken from carry flag and
  bit shifted out is stored back in the carry flag.
  First shifted-in bit is taken from the parameter CF, and the last shifted-out
  bit is returned again in the CF.

    For example...

      original number:    01001100 (0x4C)
       original carry:    1

      rotating left by 3 places...

      resulting number:   01100101 (0x65)
       resulting carry:   0

  Assembly implementation uses instruction RCL.

  Only lower 5 bits (for 8bit, 16bit and 32bit numbers) or 6 bits (for 64bit
  numbers) of the shift are used.
}

Function RCLCarry(Value: UInt8; Shift: Integer; var CF: Boolean): UInt8; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function RCLCarry(Value: UInt16; Shift: Integer; var CF: Boolean): UInt16; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function RCLCarry(Value: UInt32; Shift: Integer; var CF: Boolean): UInt32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function RCLCarry(Value: UInt64; Shift: Integer; var CF: Boolean): UInt64; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

Function RCL(Value: UInt8; Shift: Integer; CF: Boolean = False): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function RCL(Value: UInt16; Shift: Integer; CF: Boolean = False): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function RCL(Value: UInt32; Shift: Integer; CF: Boolean = False): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function RCL(Value: UInt64; Shift: Integer; CF: Boolean = False): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure RCLValueCarry(var Value: UInt8; Shift: Integer; var CF: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCLValueCarry(var Value: UInt16; Shift: Integer; var CF: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCLValueCarry(var Value: UInt32; Shift: Integer; var CF: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCLValueCarry(var Value: UInt64; Shift: Integer; var CF: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure RCLValue(var Value: UInt8; Shift: Integer; CF: Boolean = False); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCLValue(var Value: UInt16; Shift: Integer; CF: Boolean = False); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCLValue(var Value: UInt32; Shift: Integer; CF: Boolean = False); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCLValue(var Value: UInt64; Shift: Integer; CF: Boolean = False); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                         Rotate right with carry (RCR)
================================================================================
-------------------------------------------------------------------------------}
{
  Rotates the number right with carry - works the same as ROR, but the number
  is being shifted trough carry flag. See description of RCL for more info.

  Assembly implementation uses instruction RCR.

  Only lower 5 bits (for 8bit, 16bit and 32bit numbers) or 6 bits (for 64bit
  numbers) of the shift are used.
}

Function RCRCarry(Value: UInt8; Shift: Integer; var CF: Boolean): UInt8; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function RCRCarry(Value: UInt16; Shift: Integer; var CF: Boolean): UInt16; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function RCRCarry(Value: UInt32; Shift: Integer; var CF: Boolean): UInt32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function RCRCarry(Value: UInt64; Shift: Integer; var CF: Boolean): UInt64; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

Function RCR(Value: UInt8; Shift: Integer; CF: Boolean = False): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function RCR(Value: UInt16; Shift: Integer; CF: Boolean = False): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function RCR(Value: UInt32; Shift: Integer; CF: Boolean = False): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function RCR(Value: UInt64; Shift: Integer; CF: Boolean = False): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure RCRValueCarry(var Value: UInt8; Shift: Integer; var CF: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCRValueCarry(var Value: UInt16; Shift: Integer; var CF: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCRValueCarry(var Value: UInt32; Shift: Integer; var CF: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCRValueCarry(var Value: UInt64; Shift: Integer; var CF: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure RCRValue(var Value: UInt8; Shift: Integer; CF: Boolean = False); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCRValue(var Value: UInt16; Shift: Integer; CF: Boolean = False); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCRValue(var Value: UInt32; Shift: Integer; CF: Boolean = False); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure RCRValue(var Value: UInt64; Shift: Integer; CF: Boolean = False); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                          Arithmetic left shift (SAL)
================================================================================
-------------------------------------------------------------------------------}
{
  This operation is identical to logical left shift (SHL).

  Assembly implementation uses instruction SAL.

  Only lower 5 bits (for 8bit, 16bit and 32bit numbers) or 6 bits (for 64bit
  numbers) of the shift are used.
}

Function SAL(Value: UInt8; Shift: Integer): UInt8; overload;{$IFDEF PurePascal}{$IFDEF CanInline} inline;{$ENDIF}{$ELSE} register; assembler;{$ENDIF}
Function SAL(Value: UInt16; Shift: Integer): UInt16; overload;{$IFDEF PurePascal}{$IFDEF CanInline} inline;{$ENDIF}{$ELSE} register; assembler;{$ENDIF}
Function SAL(Value: UInt32; Shift: Integer): UInt32; overload;{$IFDEF PurePascal}{$IFDEF CanInline} inline;{$ENDIF}{$ELSE} register; assembler;{$ENDIF}
Function SAL(Value: UInt64; Shift: Integer): UInt64; overload;{$IFDEF PurePascal}{$IFDEF CanInline} inline;{$ENDIF}{$ELSE} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

procedure SALValue(var Value: UInt8; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SALValue(var Value: UInt16; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SALValue(var Value: UInt32; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SALValue(var Value: UInt64; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                          Arithmetic right shift (SAR)
================================================================================
-------------------------------------------------------------------------------}
{
  Shifts the number to the right (towards lower bits), while preserving the
  highest bit. For example...

    number    10010100

    SAR by 3...

    result    11110010

  Assembly implementation uses instruction SAR.

  Only lower 5 bits (for 8bit, 16bit and 32bit numbers) or 6 bits (for 64bit
  numbers) of the shift are used.
}

Function SAR(Value: UInt8; Shift: Integer): UInt8; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function SAR(Value: UInt16; Shift: Integer): UInt16; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function SAR(Value: UInt32; Shift: Integer): UInt32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function SAR(Value: UInt64; Shift: Integer): UInt64; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------

procedure SARValue(var Value: UInt8; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SARValue(var Value: UInt16; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SARValue(var Value: UInt32; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SARValue(var Value: UInt64; Shift: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                 Endianity swap
================================================================================
-------------------------------------------------------------------------------}
{
  Reverses order of bytes within the number or general data buffer.

  Assembly implementation uses instruction BSWAP.
}

Function EndianSwap(Value: UInt16): UInt16; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function EndianSwap(Value: UInt32): UInt32; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function EndianSwap(Value: UInt64): UInt64; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

Function SwapEndian(Value: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function SwapEndian(Value: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function SwapEndian(Value: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure EndianSwapValue(var Value: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure EndianSwapValue(var Value: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure EndianSwapValue(var Value: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure SwapEndianValue(var Value: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapEndianValue(var Value: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapEndianValue(var Value: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure EndianSwap(var Buffer; Size: TMemSize); overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
procedure SwapEndian(var Buffer; Size: TMemSize); overload;

//------------------------------------------------------------------------------
{
  Reverses byte order of individial items in a given array or vector.

  Number in the name denotes size of items in bits. Function without number
  accepts an explicitly given item size (in bytes).

  Argument Arr should point to the first element/item in the array.

  Count gives number of items in the array (NOT its size in bytes).

  Stride is a linear distance betveen two items (NOT a space between them,
  distance from one item to another). Usually it equals to item size - it is
  here for situations when manipulating an array of records and only one
  field of each record needs to be swapped.
}
procedure EndianSwapItems16(var Arr; Count,Stride: TMemSize);{$IFNDEF PurePascal} register; assembler;{$ENDIF} overload;
procedure EndianSwapItems32(var Arr; Count,Stride: TMemSize);{$IFNDEF PurePascal} register; assembler;{$ENDIF} overload;
procedure EndianSwapItems64(var Arr; Count,Stride: TMemSize);{$IFNDEF PurePascal} register; assembler;{$ENDIF} overload;

procedure EndianSwapItems(var Arr; ItemSize,Count,Stride: TMemSize); overload;

procedure EndianSwapItems16(var Arr; Count: TMemSize); overload;
procedure EndianSwapItems32(var Arr; Count: TMemSize); overload;
procedure EndianSwapItems64(var Arr; Count: TMemSize); overload;

procedure EndianSwapItems(var Arr; ItemSize,Count: TMemSize); overload;

//------------------------------------------------------------------------------

procedure SwapEndianItems16(var Arr; Count,Stride: TMemSize); overload;
procedure SwapEndianItems32(var Arr; Count,Stride: TMemSize); overload;
procedure SwapEndianItems64(var Arr; Count,Stride: TMemSize); overload;

procedure SwapEndianItems(var Arr; ItemSize,Count,Stride: TMemSize); overload;

procedure SwapEndianItems16(var Arr; Count: TMemSize); overload;
procedure SwapEndianItems32(var Arr; Count: TMemSize); overload;
procedure SwapEndianItems64(var Arr; Count: TMemSize); overload;

procedure SwapEndianItems(var Arr; ItemSize,Count: TMemSize); overload;

{-------------------------------------------------------------------------------
================================================================================
                                  Bit test (BT)
================================================================================
-------------------------------------------------------------------------------}
{
  Returns true when selected bit in the Value is set, false when it is clear.

  Assembly implementation uses instruction BT.

  The bit number/index is taken modulo 8, 16, 32 or 64, depending on argument
  width.
}

Function BT(Value: UInt8; Bit: Integer): Boolean; overload;{$IFDEF PurePascal}{$IFDEF CanInline} inline;{$ENDIF}{$ELSE} register; assembler;{$ENDIF}
Function BT(Value: UInt16; Bit: Integer): Boolean; overload;{$IFDEF PurePascal}{$IFDEF CanInline} inline;{$ENDIF}{$ELSE} register; assembler;{$ENDIF}
Function BT(Value: UInt32; Bit: Integer): Boolean; overload;{$IFDEF PurePascal}{$IFDEF CanInline} inline;{$ENDIF}{$ELSE} register; assembler;{$ENDIF}
Function BT(Value: UInt64; Bit: Integer): Boolean; overload;{$IFDEF PurePascal}{$IFDEF CanInline} inline;{$ENDIF}{$ELSE} register; assembler;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                             Bit test and set (BTS)
================================================================================
-------------------------------------------------------------------------------}
{
  Sets the selected bit in the Value (to 1) and returns true when the bit was
  previously set, false when it was clear.

  Assembly implementation uses instruction BTS.

  The bit number/index is taken modulo 8, 16, 32 or 64, depending on argument
  width.
}

Function BTS(var Value: UInt8; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BTS(var Value: UInt16; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BTS(var Value: UInt32; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BTS(var Value: UInt64; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                            Bit test and reset (BTR)
================================================================================
-------------------------------------------------------------------------------}
{
  Resets the selected bit in the Value (to 0) and returns true when the bit was
  previously set, false when it was clear.

  Assembly implementation uses instruction BTR.

  The bit number/index is taken modulo 8, 16, 32 or 64, depending on argument
  width.
}

Function BTR(var Value: UInt8; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BTR(var Value: UInt16; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BTR(var Value: UInt32; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BTR(var Value: UInt64; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                          Bit test and complement (BTC)
================================================================================
-------------------------------------------------------------------------------}
{
  Complements the selected bit in the Value (swaps its state, 0 <-> 1) and
  returns true when the bit was previously set, false when it was clear.

  Assembly implementation uses instruction BTC.

  The bit number/index is taken modulo 8, 16, 32 or 64, depending on argument
  width.
}

Function BTC(var Value: UInt8; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BTC(var Value: UInt16; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BTC(var Value: UInt32; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BTC(var Value: UInt64; Bit: Integer): Boolean; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                       Bit test and set to a given value
================================================================================
-------------------------------------------------------------------------------}
{
  Sets value of the selected bit in the number given in Value to a NewValue and
  returns its previous state.

  The bit number/index is taken modulo 8, 16, 32 or 64, depending on argument
  width.  
}

Function BitSetTo(var Value: UInt8; Bit: Integer; NewValue: Boolean): Boolean; overload;
Function BitSetTo(var Value: UInt16; Bit: Integer; NewValue: Boolean): Boolean; overload;
Function BitSetTo(var Value: UInt32; Bit: Integer; NewValue: Boolean): Boolean; overload;
Function BitSetTo(var Value: UInt64; Bit: Integer; NewValue: Boolean): Boolean; overload;

{-------------------------------------------------------------------------------
================================================================================
                             Bit scan forward (BSF)
================================================================================
-------------------------------------------------------------------------------}
{
  Returns index of lowest set (1) bit in the passed number. If no bit is set,
  then -1 is returned.

  Assembly implementation uses instruction BSF.
}

Function BSF(Value: UInt8): Integer; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BSF(Value: UInt16): Integer; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BSF(Value: UInt32): Integer; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BSF(Value: UInt64): Integer; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                             Bit scan reversed (BSR)
================================================================================
-------------------------------------------------------------------------------}
{
  Returns index of highest set bit in the passed number. If no bit is set, then
  -1 is returned.

  Assembly implementation uses instruction BSR.
}

Function BSR(Value: UInt8): Integer; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BSR(Value: UInt16): Integer; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BSR(Value: UInt32): Integer; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}
Function BSR(Value: UInt64): Integer; overload;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                Population count
================================================================================
-------------------------------------------------------------------------------}
{
  Returns number of set (1) bits in the passed number. When no bit is set, it
  will return 0.

  Assembly implementation uses instruction POPCNT.
}

Function PopCount(Value: UInt8): Integer; overload;
Function PopCount(Value: UInt16): Integer; overload;
Function PopCount(Value: UInt32): Integer; overload;
Function PopCount(Value: UInt64): Integer; overload;

{-------------------------------------------------------------------------------
================================================================================
                               Nibble manipulation
================================================================================
-------------------------------------------------------------------------------}
{
  Following funtions are meant for obtaining or changing lower or higher
  nibbles (half bytes, 4bit halfs) within one byte (an octet).
}

Function GetHighNibble(Value: UInt8): TNibble;{$IFDEF CanInline} inline;{$ENDIF}
Function GetLowNibble(Value: UInt8): TNibble;{$IFDEF CanInline} inline;{$ENDIF}

Function SetHighNibble(Value: UInt8; SetTo: TNibble): UInt8;{$IFDEF CanInline} inline;{$ENDIF}
Function SetLowNibble(Value: UInt8; SetTo: TNibble): UInt8;{$IFDEF CanInline} inline;{$ENDIF}

procedure SetHighNibbleValue(var Value: UInt8; SetTo: TNibble);{$IFDEF CanInline} inline;{$ENDIF}
procedure SetLowNibbleValue(var Value: UInt8; SetTo: TNibble);{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                 Get flag state
================================================================================
-------------------------------------------------------------------------------}
{
  When exact match is false, the following function will return true when at
  least one bit set in FlagBitmask is also set in the Value, false otherwise.

  When exact match is true, then true is returned only when all bits set in the
  FlagBitmask are also set in Value, false otherwise.
}

Function GetFlagState(Value,FlagBitmask: UInt8; ExactMatch: Boolean = False): Boolean; overload;
Function GetFlagState(Value,FlagBitmask: UInt16; ExactMatch: Boolean = False): Boolean; overload;
Function GetFlagState(Value,FlagBitmask: UInt32; ExactMatch: Boolean = False): Boolean; overload;
Function GetFlagState(Value,FlagBitmask: UInt64; ExactMatch: Boolean = False): Boolean; overload;

{-------------------------------------------------------------------------------
================================================================================
                                    Set flag
================================================================================
-------------------------------------------------------------------------------}
{
  Bits set in the FlagBitmask are also set (to 1) in the passed number and this
  resulting number is then returned.

  Functions accepting array of flags will set all bits within all the passed
  flag bistmasks.

  Functions with bits noted in name (*_8, *_16, ...) are there mainly for older
  versions of Delphi (up to Delphi 2007), because they are not able to
  distinguish what overloaded function to call (some problem with open array
  parameter parsing).
}

Function SetFlag(Value,FlagBitmask: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function SetFlag(Value,FlagBitmask: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function SetFlag(Value,FlagBitmask: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function SetFlag(Value,FlagBitmask: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure SetFlagValue(var Value: UInt8; FlagBitmask: UInt8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetFlagValue(var Value: UInt16; FlagBitmask: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetFlagValue(var Value: UInt32; FlagBitmask: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetFlagValue(var Value: UInt64; FlagBitmask: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function SetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
Function SetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
Function SetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
Function SetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;

//------------------------------------------------------------------------------

Function SetFlags(Value: UInt8; Flags: array of UInt8): UInt8; overload;
Function SetFlags(Value: UInt16; Flags: array of UInt16): UInt16; overload;
Function SetFlags(Value: UInt32; Flags: array of UInt32): UInt32; overload;
Function SetFlags(Value: UInt64; Flags: array of UInt64): UInt64; overload;

//------------------------------------------------------------------------------

procedure SetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
procedure SetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
procedure SetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
procedure SetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);

//------------------------------------------------------------------------------

procedure SetFlagsValue(var Value: UInt8; Flags: array of UInt8); overload;
procedure SetFlagsValue(var Value: UInt16; Flags: array of UInt16); overload;
procedure SetFlagsValue(var Value: UInt32; Flags: array of UInt32); overload;
procedure SetFlagsValue(var Value: UInt64; Flags: array of UInt64); overload;

{-------------------------------------------------------------------------------
================================================================================
                                   Reset flag
================================================================================
-------------------------------------------------------------------------------}
{
  Bits set in the FlagBitmask are reset (to 0) in the passed number and this
  resulting number is then returned.

  Functions accepting array of flags will reset all bits within all the passed
  flag bistmasks.

  Functions with bits noted in name (*_8, *_16, ...) are there mainly for older
  versions of Delphi (up to Delphi 2007), because they are not able to
  distinguish what overloaded function to call (some problem with open array
  parameter parsing).
}

Function ResetFlag(Value,FlagBitmask: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function ResetFlag(Value,FlagBitmask: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function ResetFlag(Value,FlagBitmask: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function ResetFlag(Value,FlagBitmask: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure ResetFlagValue(var Value: UInt8; FlagBitmask: UInt8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure ResetFlagValue(var Value: UInt16; FlagBitmask: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure ResetFlagValue(var Value: UInt32; FlagBitmask: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure ResetFlagValue(var Value: UInt64; FlagBitmask: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function ResetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
Function ResetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
Function ResetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
Function ResetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;

//------------------------------------------------------------------------------

Function ResetFlags(Value: UInt8; Flags: array of UInt8): UInt8; overload;
Function ResetFlags(Value: UInt16; Flags: array of UInt16): UInt16; overload;
Function ResetFlags(Value: UInt32; Flags: array of UInt32): UInt32; overload;
Function ResetFlags(Value: UInt64; Flags: array of UInt64): UInt64; overload;

//------------------------------------------------------------------------------

procedure ResetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
procedure ResetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
procedure ResetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
procedure ResetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);

//------------------------------------------------------------------------------

procedure ResetFlagsValue(var Value: UInt8; Flags: array of UInt8); overload;
procedure ResetFlagsValue(var Value: UInt16; Flags: array of UInt16); overload;
procedure ResetFlagsValue(var Value: UInt32; Flags: array of UInt32); overload;
procedure ResetFlagsValue(var Value: UInt64; Flags: array of UInt64); overload;

{-------------------------------------------------------------------------------
================================================================================
                                 Set flag state
================================================================================
-------------------------------------------------------------------------------}
{
  Sets or resets (depending on NewState; false = reset, true = set) all bits
  selected (set to 1) in FlagBitmask in the passed number Value and returns the
  result.
}

Function SetFlagState(Value,FlagBitmask: UInt8; NewState: Boolean): UInt8; overload;
Function SetFlagState(Value,FlagBitmask: UInt16; NewState: Boolean): UInt16; overload;
Function SetFlagState(Value,FlagBitmask: UInt32; NewState: Boolean): UInt32; overload;
Function SetFlagState(Value,FlagBitmask: UInt64; NewState: Boolean): UInt64; overload;

//------------------------------------------------------------------------------

procedure SetFlagStateValue(var Value: UInt8; FlagBitmask: UInt8; NewState: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetFlagStateValue(var Value: UInt16; FlagBitmask: UInt16; NewState: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetFlagStateValue(var Value: UInt32; FlagBitmask: UInt32; NewState: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetFlagStateValue(var Value: UInt64; FlagBitmask: UInt64; NewState: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                    Get bits
================================================================================
-------------------------------------------------------------------------------}
{
  Returns contiguous segment of bits from passed Value, selected by a bit range.

  When ShiftDown is true, the extracted bits are shifted down so that the first
  extracted bit is at lowest position in the result, otherwise the bit segment
  is left at its original position.

  Bit indices are taken module 8, 16, 32 and 64, depending on the argument
  width.
}

Function GetBits(Value: UInt8; FromBit,ToBit: Integer; ShiftDown: Boolean = True): UInt8; overload;
Function GetBits(Value: UInt16; FromBit,ToBit: Integer; ShiftDown: Boolean = True): UInt16; overload;
Function GetBits(Value: UInt32; FromBit,ToBit: Integer; ShiftDown: Boolean = True): UInt32; overload;
Function GetBits(Value: UInt64; FromBit,ToBit: Integer; ShiftDown: Boolean = True): UInt64; overload;

{-------------------------------------------------------------------------------
================================================================================
                                    Set bits
================================================================================
-------------------------------------------------------------------------------}
{
  Replaces contiguous segment of bits selected by range in Value by
  corresponding bits from the NewBits.

  Parameter ShiftUP is counterpart to ShiftDown in GetBits. When it is true,
  the deposited bits (NewBits parameter) are shifted up so that the first bit
  (bit 0) becomes the first deposited bit. When false, the NewBits are taken
  without change, meaning the deposited bits are taken from corresponding
  location in the NewBits, not from the low-order bits.

  Bit indices are taken module 8, 16, 32 and 64, depending on the argument
  width.
}

Function SetBits(Value,NewBits: UInt8; FromBit,ToBit: Integer; ShiftUp: Boolean = True): UInt8; overload;
Function SetBits(Value,NewBits: UInt16; FromBit,ToBit: Integer; ShiftUp: Boolean = True): UInt16; overload;
Function SetBits(Value,NewBits: UInt32; FromBit,ToBit: Integer; ShiftUp: Boolean = True): UInt32; overload;
Function SetBits(Value,NewBits: UInt64; FromBit,ToBit: Integer; ShiftUp: Boolean = True): UInt64; overload;

//------------------------------------------------------------------------------

procedure SetBitsValue(var Value: UInt8; NewBits: UInt8; FromBit,ToBit: Integer; ShiftUp: Boolean = True); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetBitsValue(var Value: UInt16; NewBits: UInt16; FromBit,ToBit: Integer; ShiftUp: Boolean = True); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetBitsValue(var Value: UInt32; NewBits: UInt32; FromBit,ToBit: Integer; ShiftUp: Boolean = True); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SetBitsValue(var Value: UInt64; NewBits: UInt64; FromBit,ToBit: Integer; ShiftUp: Boolean = True); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                  Reverse bits
================================================================================
-------------------------------------------------------------------------------}
{
  Reverses all bits within a given number.
}

Function ReverseBits(Value: UInt8): UInt8; overload;
Function ReverseBits(Value: UInt16): UInt16; overload;
Function ReverseBits(Value: UInt32): UInt32; overload;
Function ReverseBits(Value: UInt64): UInt64; overload;

//------------------------------------------------------------------------------

procedure ReverseBitsValue(var Value: UInt8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure ReverseBitsValue(var Value: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure ReverseBitsValue(var Value: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure ReverseBitsValue(var Value: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                               Leading zero count
================================================================================
-------------------------------------------------------------------------------}
{
  Returns number of leading most significant zero bits (number of zero bits
  above the highest set bit). If the number is zero, it will return size of
  the number (in bits).

  Assembly implementation uses instruction LZCNT.
}

Function LZCount(Value: UInt8): Integer; overload;
Function LZCount(Value: UInt16): Integer; overload;
Function LZCount(Value: UInt32): Integer; overload;
Function LZCount(Value: UInt64): Integer; overload;

{-------------------------------------------------------------------------------
================================================================================
                              Trailing zero count
================================================================================
-------------------------------------------------------------------------------}
{
  Returns number of trailing least significant zero bits (number of zero bits
  below the lowest set bit). If the number is zero, it will return size of the
  number (in bits).

  Assembly implementation uses instruction TZCNT.
}

Function TZCount(Value: UInt8): Integer; overload;
Function TZCount(Value: UInt16): Integer; overload;
Function TZCount(Value: UInt32): Integer; overload;
Function TZCount(Value: UInt64): Integer; overload;

{-------------------------------------------------------------------------------
================================================================================
                                  Extract bits
================================================================================
-------------------------------------------------------------------------------}
{
  Extracts contiguous segment of bits from given value, selected by a starting
  bit index and length of the segment.
  The bits are written to the result from lowest bit up. Bits above length are
  zeroed in the result.

  Assembly implementation uses instruction BEXTR.

  Both Start and Length are masked so that only lower 8 bits are observed,
  irrespective of argument width.
  If observed value of start index is out of alloved range (0..width-1), then
  no bit is extracted.
  Only bits up to width-1 can be extracted, even if length is greater than that.
}

Function ExtractBits(Value: UInt8; Start,Length: Integer): UInt8; overload;
Function ExtractBits(Value: UInt16; Start,Length: Integer): UInt16; overload;
Function ExtractBits(Value: UInt32; Start,Length: Integer): UInt32; overload;
Function ExtractBits(Value: UInt64; Start,Length: Integer): UInt64; overload;

{-------------------------------------------------------------------------------
================================================================================
                                  Deposit bits
================================================================================
-------------------------------------------------------------------------------}
{
  Deposits contiguous segment of bits from parameter NewBits to a selected
  position (Start) in Value and returns it. Bits are taken from low-order
  positions (starting from bit 0).

  Both Start and Length are masked so that only lower 8 bits are observed,
  irrespective of argument width.

  If observed value of start index is out of alloved range (0..width-1), then
  no bit is deposited.
}

Function DepositBits(Value,NewBits: UInt8; Start,Length: Integer): UInt8; overload;
Function DepositBits(Value,NewBits: UInt16; Start,Length: Integer): UInt16; overload;
Function DepositBits(Value,NewBits: UInt32; Start,Length: Integer): UInt32; overload;
Function DepositBits(Value,NewBits: UInt64; Start,Length: Integer): UInt64; overload;

//------------------------------------------------------------------------------

procedure DepositBitsValue(var Value: UInt8; NewBits: UInt8; Start,Length: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DepositBitsValue(var Value: UInt16; NewBits: UInt16; Start,Length: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DepositBitsValue(var Value: UInt32; NewBits: UInt32; Start,Length: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure DepositBitsValue(var Value: UInt64; NewBits: UInt64; Start,Length: Integer); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                              Parallel bits extract
================================================================================
-------------------------------------------------------------------------------}
{
  Extracts bits from value using a mask - bits that are set (to 1) in the mask
  are extracted, other bits are ignored. The extracted bits are written into
  result from the lowest bit (bit 0) up. Unused bit in the result are zeroed.

  Assembly implementation uses instruction PEXT.
}

Function ParallelBitsExtract(Value,Mask: UInt8): UInt8; overload;
Function ParallelBitsExtract(Value,Mask: UInt16): UInt16; overload;
Function ParallelBitsExtract(Value,Mask: UInt32): UInt32; overload;
Function ParallelBitsExtract(Value,Mask: UInt64): UInt64; overload;

{-------------------------------------------------------------------------------
================================================================================
                              Parallel bits deposit
================================================================================
-------------------------------------------------------------------------------}
{
  Deposits bits from value to result using a mask - it takes low order bits
  from value and puts them at locations in result corresponding to set bits
  in the mask, in the order they appear (so first bit from value is put into
  result at location corresponding to first set bit in mask and so on). All
  unused bits in the result are zeroed.

  Assembly implementation uses instruction PDEP.
}

Function ParallelBitsDeposit(Value,Mask: UInt8): UInt8; overload;
Function ParallelBitsDeposit(Value,Mask: UInt16): UInt16; overload;
Function ParallelBitsDeposit(Value,Mask: UInt32): UInt32; overload;
Function ParallelBitsDeposit(Value,Mask: UInt64): UInt64; overload;

{-------------------------------------------------------------------------------
================================================================================
                                   Bit parity
================================================================================
-------------------------------------------------------------------------------}
{
  Bit parity returns true when the number contains an even number or zero set
  bits, false otherwise.
}
Function BitParity(Value: UInt8): Boolean; overload;
Function BitParity(Value: UInt16): Boolean; overload;
Function BitParity(Value: UInt32): Boolean; overload;
Function BitParity(Value: UInt64): Boolean; overload;


{===============================================================================
--------------------------------------------------------------------------------

                               Pointer operations

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                           Pointer arithmetic helpers
================================================================================
-------------------------------------------------------------------------------}
{
  Following functions are to be used in situations where a pointer needs to be
  incremented or decremented by an arbitrary offset and doing it in-situ is not
  desirable or possible.

  Overload accepting Count and Stride is for arrays/vectors. Here, the pointer
  is incremented by Count * Stride.
}

Function PtrAdvance(Ptr: Pointer; Offset: TMemOffset): Pointer; overload;
Function PtrAdvance(Ptr: Pointer; Count: Integer; Stride: TMemSize): Pointer; overload;

procedure PtrAdvanceVar(var Ptr: Pointer; Offset: TMemOffset); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure PtrAdvanceVar(var Ptr: Pointer; Count: Integer; Stride: TMemSize); overload;{$IFDEF CanInline} inline;{$ENDIF}

// some aliases
Function AdvancePtr(Ptr: Pointer; Offset: TMemOffset): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function AdvancePtr(Ptr: Pointer; Count: Integer; Stride: TMemSize): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure AdvancePtrVar(var Ptr: Pointer; Offset: TMemOffset); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure AdvancePtrVar(var Ptr: Pointer; Count: Integer; Stride: TMemSize); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                               Address comparison
================================================================================
-------------------------------------------------------------------------------}
{
  Compares addresses of two given pointers.

  If pointer A is smaller (the address is lower) than B, then a negative value
  is returned. If A is larger than B, then a positive value is returned. When
  the two pointers point to the same address, then zero is returned.
}
Function PtrCompare(A,B: Pointer): Integer;

{-------------------------------------------------------------------------------
================================================================================
                            Memory address alignment
================================================================================
-------------------------------------------------------------------------------}
type
{
  More alignments can be added later anywhere into the following enumeration,
  so do not assume anything about the numerical value or position of any enum
  value.
}
  TMemoryAlignment = (maNone,
    ma8bit,ma16bit,ma32bit,ma64bit,ma128bit,ma256bit,ma512bit,ma1024bit,ma2048bit,
    ma1byte,ma2byte,ma4byte,ma8byte,ma16byte,ma32byte,ma64byte,ma128byte,ma256byte);

//------------------------------------------------------------------------------

{
  AlignmentBytes returns number of bytes corresponding to requested alignment.
}
Function AlignmentBytes(Alignment: TMemoryAlignment): TMemSize;

//------------------------------------------------------------------------------

{
  CheckAlignment returns true when the provided memory address is aligned
  as indicated by Alignment parameter, false otherwise.
}
Function CheckAlignment(Address: Pointer; Alignment: TMemoryAlignment): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

{
  Misalignment returns distance, in bytes, from the closest properly aligned
  (defined by parameter Alignment) address that is not larger than the passed
  address.
  If the address is aligned, it will return zero.
}
Function Misalignment(Address: Pointer; Alignment: TMemoryAlignment): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

{
  Returns number of bytes that needs to be added to Address to obtain a
  properly aligned memory address, effectively calculating:

      AlignedMemory(Address,Alignment) - Address
}
Function AlignmentOffset(Address: Pointer; Alignment: TMemoryAlignment): TMemSize;

{
  Returns highest alignment the passed address conforms to.
  Note that the actual alignment of the address migh be even higher, this
  function just returns highest value from TMemoryAlignment enum.

  By default the function returns bit alignments (eg. ma64bit), if you want
  it to return byte alignments (eg ma8byte), set argument ByteAlignments to
  true.

  If the address is not aligned, then maNone is returned (which should not ever
  happen, ma1byte or ma8bit should be the worst case).
}
Function ResolveAlignment(Address: Pointer; ByteAlignments: Boolean = False): TMemoryAlignment;

//------------------------------------------------------------------------------
{
  AlignedMemory checks provided memory address for requested alignment. When
  the address is properly aligned, it is returned and nothing more is done.
  When is is not properly aligned, then this functions will return closest
  properly aligned memory address that is not smaller than the provided address.

    WARNING - this function does NOT do any (re)allocation, it merely returns
              an aligned pointer closest to a given one.
}
Function AlignedMemory(Address: Pointer; Alignment: TMemoryAlignment): Pointer;{$IFDEF CanInline} inline;{$ENDIF}

{
  AlignMemory works the same as AlignedMemory, it just operates directly on a
  passed variable.
}
procedure AlignMemory(var Address: Pointer; Alignment: TMemoryAlignment);{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------

                             Binary data operations

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                             Binary data comparison
================================================================================
-------------------------------------------------------------------------------}
type
  TCompareMethod = (cmSizeData,cmDataSize,cmEqSizeData,cmDataPadFront,
                    cmDataPadBack);

//------------------------------------------------------------------------------
{
  Following function are comparing data presented in two buffers or two arrays
  of byte.

  Behavior of these functions depends on how the parameter CompareMethod is set:

    cmSizeData

      The function first compares size of the buffers (length of arrays). When
      they do not match, the function will return negative value when the first
      size (length) is smaller than the second, or positive value when the first
      size (length) is larger than the second.
      When sizes (lengths) match, the buffers are scanned byte-by-byte (in case
      of arrays item-by-item). If all bytes within both buffers (items in
      arrays) match, then 0 (zero) is returned. When a differing bytes (items)
      on corresponding locations are found, they are compared and the result
      is set according to this comparison (negative value when byte/item in the
      first buffer/array is smaller than in the second, positive value when
      byte/item in the first buffer/array is larger than in the second).

    cmDataSize

      The function first compares bytes/items on corresponding places common to
      both buffers/arrays. When a differing byte/item is found, the function
      exits and returns a negative value when byte/item in first buffer/array
      is smaller than in the second, or positive value when byte/item in the
      first buffer/array is larger than in the second.
      When no differing byte is found, then the function compares sizes/lengths
      and returns value according to this comparison. Negative value when first
      size/length is smaller than second, positive value when first size/length
      is larger than the second. When the sizes/lengths are matching, the
      function will return 0.

    cmEqSizeData

      If both sizes/length are the same, then bytes on corresponding locations
      are compared. When all bytes/items are matching, a zero is returned. If
      any differing bytes/items are found, they are compared and result is set
      according to this comparison. Negative value is returned when byte/item
      in first buffer/array is smaller than in the second, positive value when
      byte/item in the first buffer/array is larger than in the second.
      If the sizes/lengths do not match, the function will raise an
      EBOSizeMismatch exception.

    cmDataPadFront

      If both buffers/arrays have the same size/length, then it behaves the
      same as previous modes (bytes/items at corresponding positions are
      compared). If the sizes/lengths differs, then the shorter buffer/array
      if virtually padded at the front by a number of zero bytes so that the
      size/length with padding is the same as for the larger/longer input.
      This padded buffer/array is then compared with the larger/longer one
      as if they were the same size/length.

    cmDataPadBack

      Works the same as cmDataPadFront. The only difference is, that the shorter
      buffer/array is back-padded (zero bytes added at the end).
}
Function CompareData(const A; SizeA: TMemSize; const B; SizeB: TMemSize; CompareMethod: TCompareMethod): Integer; overload;
Function CompareData(A,B: array of UInt8; CompareMethod: TCompareMethod): Integer; overload;

{
  Following overloads are here only for the sake of backward compatibility.
  They call main implementation with a parameter CompareMethod set to cmSizeData
  when AllowSizeDiff is true, or cmEqSizeData when AllowSizeDiff is false);
}
Function CompareData(const A; SizeA: TMemSize; const B; SizeB: TMemSize; AllowSizeDiff: Boolean = True): Integer; overload;
Function CompareData(A,B: array of UInt8; AllowSizeDiff: Boolean = True): Integer; overload;

{-------------------------------------------------------------------------------
================================================================================
                              Binary data equality
================================================================================
-------------------------------------------------------------------------------}
{
  If the two data samples differ in size, SameData will return false,
  irrespective of actual content.
  If both data have zero size, it will return true.
  In other cases it will compare the buffer/array byte-by-byte and return true
  only when all corresponding bytes within them are equal, false otherwise.
}
Function SameData(const A; SizeA: TMemSize; const B; SizeB: TMemSize): Boolean; overload;
Function SameData(A,B: array of UInt8): Boolean; overload;

{-------------------------------------------------------------------------------
================================================================================
                               Buffer shift down
================================================================================
-------------------------------------------------------------------------------}
{
  Takes bytes at address Buffer + Shift and moves them down so the first moved
  byte is placed at the start of the buffer. Number of bytes shifted is equal
  to BufferSize - Shift (so that the entire rest of the buffer beyond Shift
  offset is moved).
  This function is intended for situations where buffered data are only
  partially consumed and what is left must be shifted down to the beginning of
  the buffer for further processing.
}
procedure BufferShiftDown(var Buffer; BufferSize: TMemSize; Shift: TMemSize);


{-------------------------------------------------------------------------------
================================================================================
                                  Bits copying
================================================================================
-------------------------------------------------------------------------------}
{
  Takes BitCount number of bits from the Source memory location and copies them
  into Destination memory location. This should not be used as a replacement
  for standard functions copying integral (whole) bytes, only when copying an
  arbitrary bit strings.
  Also note that the bit count is strictly observed, meaning only the truly
  copied bits are replacing bits in the destination (ie. when copying one bit,
  then only that bit is put into destination, other bits in destination parent
  byte are unaffected).

  Second overload alows for more precise control of which bits are copied.
  The SrcBitShift parameter prescribes from which bit, counting from the Source
  bit 0, to start copying.
  DstBitShift prescribes at which bit, counting from the Destination bit 0, to
  start putting the copied bits.
}
procedure CopyBits(Source,Destination: Pointer; BitCount: TMemSize); overload;
procedure CopyBits(Source,Destination: Pointer; SrcBitOffset,DstBitOffset,BitCount: TMemSize); overload;

{-------------------------------------------------------------------------------
================================================================================
                              Memory space filling
================================================================================
-------------------------------------------------------------------------------}
{
  FillByte

  Fills given untyped destination buffer Dst with Count-number of Value bytes.
  This is equivalent to RTL function FillChar.

  64bit ASM code requires SSE2 instruction set extension. Presence and support
  of this extension is asserted at the unit initialization - when not supported,
  then an exception of class EBOUnsupportedPlatform is raised there.
}
procedure FillByte(var Dst; Count: TMemSize; Value: UInt8);{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{
  FillWord

  Fills given untyped destination buffer Dst with Count-number of Value words.

  64bit ASM code requires SSE2 instruction set extension (see FillByte for more
  details).

    WARNING - count gives number of words (2-byte entities) to fill, not bytes!
}
procedure FillWord(var Dst; Count: TMemSize; Value: UInt16);{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{
  FillLong

  Fills given untyped destination buffer Dst with Count-number of Value long
  words.

  64bit ASM code requires SSE2 instruction set extension (see FillByte for more
  details).

    WARNING - count gives number of long words (4-byte entities) to fill,
              not bytes!
}
procedure FillLong(var Dst; Count: TMemSize; Value: UInt32);{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{
  FillLong

  Fills given untyped destination buffer Dst with Count-number of Value quad
  words.

  64bit ASM code requires SSE2 instruction set extension (see FillByte for more
  details).

    WARNING - count gives number of quad words (8-byte entities) to fill,
              not bytes!
}
procedure FillQuad(var Dst; Count: TMemSize; Value: UInt64);{$IFNDEF PurePascal} register; assembler;{$ENDIF}

//------------------------------------------------------------------------------
{
  FillMemory

  Fills given memory space of Size bytes with Value bytes.

  It is equivalent to function FillByte except that it accepts pointer instead
  of untyped variable. In fact it internally calls the FillByte function.
}
procedure FillMemory(Mem: Pointer; Size: TMemSize; Value: UInt8);{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  ZeroMemory

  Fills given memory space of Size bytes with zero (0) bytes.

  If is equivalent to FillMemory with Value argument set to zero (internally
  calls the FillByte function).
}
procedure ZeroMemory(Mem: Pointer; Size: TMemSize);{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                              Memory space copying
================================================================================
-------------------------------------------------------------------------------}
{
  CopyMemory
  MoveMemory

  Copies Size-number of bytes from Src memory location to Dst memory location.

  Unlike in Windows-defined implementation, where CopyMemory does not allow
  for overlapped buffers, here it is completely equivalent to MoveMemory.

    NOTE - these functions are only wrappers for procedure System.Move, which
           is usually heavilly optimized, so there is no need to reimplement it
           here. Size larger than High(PtrInt) (a limit in System.Move) is
           compensated for and is therefore supported.

    WARNING - arguments for source and destination are reversed when compared
              to System.Move.
}
procedure CopyMemory(Dst,Src: Pointer; Size: TMemSize);{$IFDEF CanInline} inline;{$ENDIF}

procedure MoveMemory(Dst,Src: Pointer; Size: TMemSize);

{-------------------------------------------------------------------------------
================================================================================
                                  Memory search
================================================================================
-------------------------------------------------------------------------------}
{
  Following functions are searching provided buffer or memory location for a
  given data (byte sequence or integral value).

  When the data are found, their position within the buffer is returned in
  output parameter Position. It is a zero-based position of start of the
  sequence/value in relation to the start of provided buffer (that is, a
  distance of the data from start of the buffer). Whether the data were found
  or not (and whether in full or partially, see options) is indicated by the
  result (see description of type TBOSearchResult).

    WARNING - both arguments Size and Count (where applicable) must be lower
              or equal to High(TMemOffset), otherwise an EBOInvalidValue
              exception is raised.

    NOTE - the integers are assumed to be stored with system endianness, and
           are therefore searched that way. If you want to search for values
           stored with different endianness, just use SwapEndian on the Value
           parameter.

  If no option is included, then the sequence/value is searched for full match,
  that is, it must be present in its entirety, which means only positions from
  zero up to (Size - Count) or (Size - SizeOf(Value)) are returned.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TBOSearchResult

    This type is used for return values and indicate whether and how was the
    requested data found.

      srNotFound              - The requested data were not found in the
                                provided buffer. Value of output parameter
                                Position is undefined.

      srFound                 - The data were found in their entirety within
                                the provided buffer. Position contains a
                                positive distance of start of the data from
                                start of the provided buffer.

      srFoundLeadPartial      - A partial match with the requested data was
                                found at the start of the buffer (see option
                                soLeadPartialMatch for more details). Position
                                contains a negative distance from the start of
                                buffer to imagined start of the data.

      srFoundTrailPartial     - Partial match was found at the end of the
                                provided buffer (see option soTrailPartialMatch
                                for more details). Position contains positive
                                distance from buffer start to start of the
                                patially matching byte sequence.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -                                

  TBOSearchOptions

    Each search function accepts parameter Options of this type. It is a set
    where if any of the enumerated values is present, its corresponding option
    is activated, when not present, the option is deactivated.

      soLeadPartialMatch

        If this option is activated, then the function will check leading bytes
        of the provided buffer for a partial match with the given sequence or
        value (as if the data were stored at address below start of the buffer).
        If a match is found, then lowest negative position corresponding to how
        far below the given buffer the data would be stored to produce the
        found partial match is returned.

        Positions from -Pred(SizeOf(Value)) or -Pred(Count) to -1 can be
        returned for leading partial match.

          NOTE - this check is done before the memory is scanned for full
                 occurences. If partial leading match is found, the function
                 exits, meaning full or trailing partial matches are not even
                 attempted.

        For example, let's have UInt32 value $11223344 (little endian) and a
        memory buffer starting with byte sequence $22 $11 $AA $5B $00 ... .
        This will return position -2.

      soTrailPartialMatch

        This is similar to soLeadPartialMatch, except it tries to match
        trailing bytes of the buffer. If match is found, then it returns a
        positive position which will be above normally returned values, that
        is, larger than (Size - SizeOf(Value)) or (Size - Count), corresponding
        to where the partially matched value would start in the buffer.

        Positions from (Size - Pred(SizeOf(Value))) or (Size - Pred(Count)) to
        (Size - 1) can be returned for trailing partial match.

          NOTE - check for trailing partial match is done at the end, meaning
                 if full or leading partial match is found, then check for
                 trailing partial match is not attempted.

      soPartialMatch

        Including this option is equivalent to including both soLeadPartialMatch
        and soTrailPartialMatch.

      soAbsolutePosition

        This has effect only in overloads accepting parameter Offset.
        Normally the returned position is relative to the start of search,
        which is affected by the value of Offset, this option forces the
        function to calculate the position in relation to the start of searched
        buffer, as if Offset was 0.

    Leading partial match and trailing partial match are here for situations
    where you are scanning some non-memory data (eg. file) - there, one would
    read and scan smaller buffers. This might create problems if a multi-byte
    value lies across the buffers boundary - you can use these settings to
    search for such occurences.
}
type
  TBOSearchResult = (srNotFound,srFound,srFoundLeadPartial,srFoundTrailPartial);

  TBOSearchOptions = set of (soLeadPartialMatch,soTrailPartialMatch,soPartialMatch,
                             soAbsolutePosition);

//------------------------------------------------------------------------------
{
  Count denotes number of bytes in Bytes (byte sequence searched for), whereas
  argument Size gives size of the provided buffer (argument Buffer) that is to
  be searched/scanned.
}
Function FindBytes(const Bytes; Count: TMemSize; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;

Function FindByte(Value: UInt8; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
Function FindWord(Value: UInt16; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
Function FindLong(Value: UInt32; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
Function FindQuad(Value: UInt64; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;

//------------------------------------------------------------------------------
{
  Following overloads are wrappers that are here to ease repeated search within
  the same buffer without a need to use moving pointer. They accept argument
  Offset, which is used to move start of the searched buffer - the searching
  starts at address (Addr(Buffer) + Offset).

    NOTE - if used for repeated search, do not directly use position of previous
           occurence for parameter Offset - first, the position can be negative
           and Offset accepts only positive numbers since it is an unsigned
           integer, second, the searching would catch the same occurence - use
           value that is by at least one larger (SizeOf(Value) or Count if
           overlap is forbidden), and third, unless soAbsolutePosition is used,
           the returned position is relative to start of search, not to the
           start of the buffer (so it might not be a valid value for Offset).

  The returned position is normally relative to the start of searching (ie.
  address shifted by offset). If you want the position to be in relation to the
  start of the passed buffer, activate option soAbsolutePosition.

    WARNING - this can be deceiving in case of partial leading match as the
              returned postion might not actually point to the searched data
              if they were only partially matched at the position shifted by
              offset.

  The functions will not touch memory that is bellow (Addr(Buffer) + Offset)
  even if it is fully within the buffer and accessible.
}
Function FindBytes(const Bytes; Count: TMemSize; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;

Function FindByte(Value: UInt8; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
Function FindWord(Value: UInt16; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
Function FindLong(Value: UInt32; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
Function FindQuad(Value: UInt64; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;

//------------------------------------------------------------------------------
{
  Following overloads are provided only for backward compatibility with
  previous implementation. They are internally calling current implementation
  (previous overloads).

  Paramenters LeadingPartialMatch and TrailingPartialMatch are turned into
  corresponding options.

  Returned value is the same as in output parameter Position in previous
  overloads when the value is found. If not found, then -SizeOf(Value) or
  -Count is returned.
}
Function FindBytes(const Bytes; Count: TMemSize; Memory: Pointer; Size: TMemSize; LeadingPartialMatch: Boolean = False; TrailingPartialMatch: Boolean = False): TMemOffset; overload;

Function FindByte(Value: UInt8; Memory: Pointer; Size: TMemSize): TMemOffset; overload;
Function FindWord(Value: UInt16; Memory: Pointer; Size: TMemSize; LeadingPartialMatch: Boolean = False; TrailingPartialMatch: Boolean = False): TMemOffset; overload;
Function FindLong(Value: UInt32; Memory: Pointer; Size: TMemSize; LeadingPartialMatch: Boolean = False; TrailingPartialMatch: Boolean = False): TMemOffset; overload;
Function FindQuad(Value: UInt64; Memory: Pointer; Size: TMemSize; LeadingPartialMatch: Boolean = False; TrailingPartialMatch: Boolean = False): TMemOffset; overload;


{===============================================================================
--------------------------------------------------------------------------------

                                Other operations

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                                Low-level compare
================================================================================
-------------------------------------------------------------------------------}
{
  For meaning of individual flags, please refer to x86(-64) CPU documentation
  (eg. Intel® 64 and IA-32 Architectures Software Developer’s Manual, section
  3.4.3.1; or AMD64 Architecture Programmer’s Manual, section 3.1.4).
}
const
  BO_FLAG_CARRY    = $0001;
  BO_FLAG_PARITY   = $0004;
  BO_FLAG_AUXCARRY = $0010;
  BO_FLAG_ZERO     = $0040;
  BO_FLAG_SIGN     = $0080;
  BO_FLAG_OVERFLOW = $0800;

type
  TBOStatusFlag = (flCarry,flParity,flAuxCarry,flZero,flSign,flOverflow);
  TBOStatusFlags = set of TBOStatusFlag;

{
  LLDecodeFlags

  Converts flags word (as returned by LLCompareRaw or LLTestRaw) to a
  TBOStatusFlags set.
}
Function LLDecodeFlags(Flags: UInt16): TBOStatusFlags;

//------------------------------------------------------------------------------
{$IFNDEF PurePascal}
{
  LLCompareRaw

  Performs instruction CMP (compare, which is equivalent to SUB, in other words
  calculating A minus B) on the two given values and returns resulting status
  flags from (E/R)FLAGS register.

  Only the status flags are returned, other bits from the register are masked
  and not returned.

  Use constants BO_FLAG_* (see above) to test for individual flags.

    NOTE - These functions are available only when PurePascal symbol is not
           defined. Function accepting 64bit values is accessible only in 64bit
           programs.

  For more technical details, refer to x86(-64) CPU documentation.
}
Function LLCompareRaw(A,B: UInt8): UInt16; overload; register; assembler;
Function LLCompareRaw(A,B: UInt16): UInt16; overload; register; assembler;
Function LLCompareRaw(A,B: UInt32): UInt16; overload; register; assembler;
{$IFDEF x64}
Function LLCompareRaw(A,B: UInt64): UInt16; overload; register; assembler;
{$ENDIF}

//------------------------------------------------------------------------------
{
  LLCompare

  Works the same as LLCompareRaw (see there for more details), but parses the
  result and returns the flags as a set of TBOStatusFlag enums. If an enum is
  present in the returned set, then the corresponding flag was set(1). When not
  present, then it was clear(0).
}
Function LLCompare(A,B: UInt8): TBOStatusFlags; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function LLCompare(A,B: UInt16): TBOStatusFlags; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function LLCompare(A,B: UInt32): TBOStatusFlags; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFDEF x64}
Function LLCompare(A,B: UInt64): TBOStatusFlags; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                 Low-level test
================================================================================
-------------------------------------------------------------------------------}

{$IFNDEF PurePascal}
{
  LLTestRaw

  Performs instruction TEST (which is equivalent to bit-wise logical AND) on
  the two given values and returns resulting status flags from (E/R)FLAGS
  register.

  Only status flags for parity, zero and sign are returned, other bits from
  the register are masked and not returned (they are cleared to zero).

  Use constants BO_FLAG_* (see above) to test for individual flags.

    NOTE - These functions are available only when PurePascal symbol is not
           defined. Function accepting 64bit values is accessible only in 64bit
           programs.

  For more technical details, refer to x86(-64) CPU documentation.
}
Function LLTestRaw(A,B: UInt8): UInt16; overload; register; assembler;
Function LLTestRaw(A,B: UInt16): UInt16; overload; register; assembler;
Function LLTestRaw(A,B: UInt32): UInt16; overload; register; assembler;
{$IFDEF x64}
Function LLTestRaw(A,B: UInt64): UInt16; overload; register; assembler;
{$ENDIF}

//------------------------------------------------------------------------------
{
  LLTest

  Works the same as LLTestRaw (see there for more details), but parses the
  result and returns the flags as a set of TBOStatusFlag enums. If an enum is
  present in the returned set, then the corresponding flag was set(1). When not
  present, then it was clear(0).
}
Function LLTest(A,B: UInt8): TBOStatusFlags; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function LLTest(A,B: UInt16): TBOStatusFlags; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function LLTest(A,B: UInt32): TBOStatusFlags; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFDEF x64}
Function LLTest(A,B: UInt64): TBOStatusFlags; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------

                                      UIM

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                         Unit implementation management
================================================================================
-------------------------------------------------------------------------------}
{
  Some of the functions provided by this library can have multiple various
  implementations, typically one in pascal and one in assembly.

  In most cases, which implementation will be used is decided during compilation
  according to active symbols - in most cases asm is used whenever symbol
  PurePascal is not defined.

  But in some cases, when PurePascal is not defined, it cannot be decided
  whether the asm implementation can be used or not in advance - usually because
  the asm version uses some CPU instruction extension that is not guaranteed to
  be present on the executing machine.
  These functions have both implementations (pascal and assembly) compiled, and
  which will be used is selected at unit initialization, after checking CPU for
  required extensions.
  Before unit initialization, all the functions are routed to default
  implementation (pascal).

  When such function is called, the selected implementation is called via an
  internal global variable which holds pointer to the implementation. Within
  this library, this process is called "routing".

  Following types and functions are here to obtain information about which
  implementation is currently selected and to provide a mean of changing it.

  WARNING - be wery careful when changing the selected implementation, as there
            is absolutely no thread-safety protection (the variables are global,
            initialized only once and not expected to be ever changed)
}
type
  TUIM_BitOps_Function = (
    fnPopCount8,fnPopCount16,fnPopCount32,fnPopCount64,                                             // PopCount()
    fnLZCount8,fnLZCount16,fnLZCount32,fnLZCount64,                                                 // LZCount()
    fnTZCount8,fnTZCount16,fnTZCount32,fnTZCount64,                                                 // TZCount()
    fnExtractBits8,fnExtractBits16,fnExtractBits32,fnExtractBits64,                                 // ExtractBits()
    fnParallelBitsExtract8,fnParallelBitsExtract16,fnParallelBitsExtract32,fnParallelBitsExtract64, // ParallelBitsExtract()
    fnParallelBitsDeposit8,fnParallelBitsDeposit16,fnParallelBitsDeposit32,fnParallelBitsDeposit64  // ParallelBitsDeposit()
  );

  TUIM_BitOps_Implementation = (imNone,imPascal,imAssembly);

  TUIM_BitOps_Implementations = set of TUIM_BitOps_Implementation;

//------------------------------------------------------------------------------

{
  Returns which implementations are available for the selected function.
}
Function UIM_BitOps_AvailableFuncImpl(Func: TUIM_BitOps_Function): TUIM_BitOps_Implementations;

{
  Returns which implementations are supported and can be safely selected for
  a given function.
}
Function UIM_BitOps_SupportedFuncImpl(Func: TUIM_BitOps_Function): TUIM_BitOps_Implementations;

{
  Returns value indicating what implementation of the selected function is
  executed when calling the function.
}
Function UIM_BitOps_GetFuncImpl(Func: TUIM_BitOps_Function): TUIM_BitOps_Implementation;

{
  Routes selected function to a selected implementation.

  Returned value is the previous routing.

  NOTE - when asm implementation cannot be used, and you still select it,
         the function will be routed to pascal version

  WARNING - when selecting imNone as an implementation for some function, the
            routing is set to nil, and because the routing mechanism, for the
            sake of speed, does not check validity, it will result in an
            exception when calling this function

  WANRING - when selecting unsupported implementation, calling the function will
            almost certainly result in an system exception (invalid
            instruction).
}
Function UIM_BitOps_SetFuncImpl(Func: TUIM_BitOps_Function; NewImpl: TUIM_BitOps_Implementation): TUIM_BitOps_Implementation;

implementation

uses
  BasicUIM{$IFNDEF PurePascal}, SimpleCPUID{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{$IFNDEF FPC}
const
  FPC_VERSION = 0;
{$ENDIF}

{$IF (not Defined(FPC) and not Defined(x64)) or (FPC_VERSION < 3)}
  {
    ASM_MachineCode

    When defined, some ASM instructions are inserted into byte stream directly
    as a machine code. It is there because not all compilers supports, and
    therefore can compile, such instructions.

    As I am not able to tell which 32bit delphi compilers do support them,
    I am assuming none of them do. I am also assuming that all 64bit delphi
    compilers and current FPCs are supporting the instructions.

    Has effect only in assembly code.
  }
  {$DEFINE ASM_MachineCode}
{$ELSE}
  {$UNDEF ASM_MachineCode}
{$IFEND}

{$IF SizeOf(Integer) <> 4}
  {$MESSAGE FATAL 'Unsupported implementation detail.'}
{$IFEND}

{-------------------------------------------------------------------------------
================================================================================
                               Auxiliary functions
================================================================================
-------------------------------------------------------------------------------}

Function CharInSet(C: Char; S: TSysCharSet): Boolean;
begin
{$IF SizeOf(Char) <> 1}
If Ord(C) > 255 then
  Result := False
else
{$IFEND}
  Result := AnsiChar(C) in S;
end;

//------------------------------------------------------------------------------

Function MemSizeMin(A,B: TMemSize): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
begin
If A < B then
  Result := A
else
  Result := B;
end;

//------------------------------------------------------------------------------

Function MemSizeMax(A,B: TMemSize): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
begin
If A > B then
  Result := A
else
  Result := B;
end;


{===============================================================================
--------------------------------------------------------------------------------

                       Binary data <-> string conversions

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                   Integer number <-> Bit string conversions
================================================================================
-------------------------------------------------------------------------------}

Function NumberToBitString(Number: UInt64; Bits: UInt8; BitStringFormat: TBitStringFormat): String;
var
  i,SplitCnt: Integer;
begin
case BitStringFormat.Split of
  bss4bits:   SplitCnt := 4;
  bss8bits:   SplitCnt := 8;
  bss16bits:  SplitCnt := 16;
  bss32bits:  SplitCnt := 32;
else
  SplitCnt := Bits;
end;
If SplitCnt > Bits then SplitCnt := Bits;
Result := StringOfChar(BitStringFormat.ZeroBitChar,Bits + (Pred(Bits) div SplitCnt));
For i := Bits downto 1 do
  begin
    If (Number and 1) <> 0 then
      Result[i + (Pred(i) div SplitCnt)] := BitStringFormat.SetBitChar;
    Number := Number shr 1;
  end;
For i := 1 to Pred(Bits div SplitCnt) do
  Result[(i * SplitCnt) + i] := BitStringFormat.SplitChar;
end;

//------------------------------------------------------------------------------

Function NumberToBitStr(Number: UInt8; BitStringFormat: TBitStringFormat): String;
begin
Result := NumberToBitString(Number,8,BitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToBitStr(Number: UInt16; BitStringFormat: TBitStringFormat): String;
begin
Result := NumberToBitString(Number,16,BitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToBitStr(Number: UInt32; BitStringFormat: TBitStringFormat): String;
begin
Result := NumberToBitString(Number,32,BitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToBitStr(Number: UInt64; BitStringFormat: TBitStringFormat): String;
begin
Result := NumberToBitString(Number,64,BitStringFormat);
end;

//------------------------------------------------------------------------------

Function NumberToBitStr(Number: UInt8; Split: TBitStringSplit): String;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
Result := NumberToBitStr(Number,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToBitStr(Number: UInt16; Split: TBitStringSplit): String;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
Result := NumberToBitStr(Number,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToBitStr(Number: UInt32; Split: TBitStringSplit): String;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
Result := NumberToBitStr(Number,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToBitStr(Number: UInt64; Split: TBitStringSplit): String;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
Result := NumberToBitStr(Number,Format);
end;

//------------------------------------------------------------------------------

Function NumberToBitStr(Number: UInt8): String;
begin
Result := NumberToBitString(Number,8,DefBitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToBitStr(Number: UInt16): String;
begin
Result := NumberToBitString(Number,16,DefBitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToBitStr(Number: UInt32): String;
begin
Result := NumberToBitString(Number,32,DefBitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToBitStr(Number: UInt64): String;
begin
Result := NumberToBitString(Number,64,DefBitStringFormat);
end;

//==============================================================================

Function BitStrToNumber(const BitString: String; BitStringFormat: TBitStringFormat): UInt64;
var
  i:  Integer;
begin
Result := 0;
For i := 1 to Length(BitString) do
  begin
    If BitString[i] <> BitStringFormat.SplitChar then
      begin
        Result := Result shl 1;
        If BitString[i] = BitStringFormat.SetBitChar then
          Result := Result or 1
        else If BitString[i] <> BitStringFormat.ZeroBitChar then
          raise EBOInvalidCharacter.CreateFmt('BitStrToNumber: Unknown character (#%d) in bitstring.',[Ord(BitString[i])]);
      end
    else Continue{For i};
  end;
end;

//------------------------------------------------------------------------------

Function BitStrToNumber(const BitString: String; Split: TBitStringSplit): UInt64;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
Result := BitStrToNumber(BitString,Format);
end;

//------------------------------------------------------------------------------

Function BitStrToNumber(const BitString: String): UInt64;
begin
Result := BitStrToNumber(BitString,DefBitStringFormat);
end;

//------------------------------------------------------------------------------

Function TryBitStrToNumber(const BitString: String; out Value: UInt8; BitStringFormat: TBitStringFormat): Boolean;
begin
try
  Value := UInt8(BitStrToNumber(BitString,BitStringFormat));
  Result := True;
except
  Result := False;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToNumber(const BitString: String; out Value: UInt16; BitStringFormat: TBitStringFormat): Boolean;
begin
try
  Value := UInt16(BitStrToNumber(BitString,BitStringFormat));
  Result := True;
except
  Result := False;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToNumber(const BitString: String; out Value: UInt32; BitStringFormat: TBitStringFormat): Boolean;
begin
try
  Value := UInt32(BitStrToNumber(BitString,BitStringFormat));
  Result := True;
except
  Result := False;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToNumber(const BitString: String; out Value: UInt64; BitStringFormat: TBitStringFormat): Boolean;
begin
try
  Value := BitStrToNumber(BitString,BitStringFormat);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TryBitStrToNumber(const BitString: String; out Value: UInt8; Split: TBitStringSplit): Boolean;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
Result := TryBitStrToNumber(BitString,Value,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToNumber(const BitString: String; out Value: UInt16; Split: TBitStringSplit): Boolean;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
Result := TryBitStrToNumber(BitString,Value,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToNumber(const BitString: String; out Value: UInt32; Split: TBitStringSplit): Boolean;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
Result := TryBitStrToNumber(BitString,Value,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToNumber(const BitString: String; out Value: UInt64; Split: TBitStringSplit): Boolean;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
Result := TryBitStrToNumber(BitString,Value,Format);
end;


//------------------------------------------------------------------------------

Function TryBitStrToNumber(const BitString: String; out Value: UInt8): Boolean;
begin
Result := TryBitStrToNumber(BitString,Value,DefBitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToNumber(const BitString: String; out Value: UInt16): Boolean;
begin
Result := TryBitStrToNumber(BitString,Value,DefBitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToNumber(const BitString: String; out Value: UInt32): Boolean;
begin
Result := TryBitStrToNumber(BitString,Value,DefBitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToNumber(const BitString: String; out Value: UInt64): Boolean;
begin
Result := TryBitStrToNumber(BitString,Value,DefBitStringFormat);
end;

//------------------------------------------------------------------------------

Function BitStrToNumberDef(const BitString: String; Default: UInt64; BitStringFormat: TBitStringFormat): UInt64;
begin
If not TryBitStrToNumber(BitString,Result,BitStringFormat) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function BitStrToNumberDef(const BitString: String; Default: UInt64; Split: TBitStringSplit): UInt64;
var
  Format: TBitStringFormat;
begin
Format := DefBitStringFormat;
Format.Split := Split;
If not TryBitStrToNumber(BitString,Result,Format) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function BitStrToNumberDef(const BitString: String; Default: UInt64): UInt64;
begin
If not TryBitStrToNumber(BitString,Result,DefBitStringFormat) then
  Result := Default;
end;

{-------------------------------------------------------------------------------
================================================================================
                   Integer number <-> Octal string conversions
================================================================================
-------------------------------------------------------------------------------}

Function NumberToOctString(Number: UInt64): String;
var
  Len:  TStrOff;
begin
Result := StringOfChar('0',22);
Len := 0;
while Number <> 0 do
  begin
    Result[Length(Result) - Len] := Chr(Ord('0') + (Number and 7));
    Number := Number shr 3;
    Inc(Len);
  end;
// remove leading zeroes
If len > 0 then
  Result := Copy(Result,Succ(Length(Result) - Len),Len)
else
  Result := '0';
end;

//------------------------------------------------------------------------------

Function NumberToOctStr(Number: UInt8): String;
begin
Result := NumberToOctString(UInt64(Number));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToOctStr(Number: UInt16): String;
begin
Result := NumberToOctString(UInt64(Number));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToOctStr(Number: UInt32): String;
begin
Result := NumberToOctString(UInt64(Number));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function NumberToOctStr(Number: UInt64): String;
begin
Result := NumberToOctString(UInt64(Number));
end;

//------------------------------------------------------------------------------

Function OctStrToNumber(const OctString: String): UInt64;
var
  i:  TStrOff;
begin
// highest possible octal value is 1 777 777 777 777 777 777 777 (length 22)
Result := 0;
If (Length(OctString) > 0) and (Length(OctString) <= 22) then
  begin
    For i := 1 to Length(OctString) do
      If Ord(OctString[i]) in [Ord('0')..Ord('7')] then
        begin
          If (Result and $E000000000000000) = 0 then
            Result := Result shl 3
          else
            raise EBOConversionError.CreateFmt('OctStrToNumber: "%s" is not a valid octal number.',[OctString]);
          Result := Result or (Ord(OctString[i]) - Ord('0'));
        end
      else raise EBOInvalidCharacter.CreateFmt('OctStrToNumber: Unknown character (#%d) in octstring.',[Ord(OctString[i])]);
  end
else raise EBOConversionError.CreateFmt('OctStrToNumber: "%s" is not a valid octal number.',[OctString]);
end;

//------------------------------------------------------------------------------

Function TryOctStrToNumber(const OctString: String; out Value: UInt8): Boolean;
begin
try
  Value := UInt8(OctStrToNumber(OctString));
  Result := True;
except
  Result := False;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryOctStrToNumber(const OctString: String; out Value: UInt16): Boolean;
begin
try
  Value := UInt16(OctStrToNumber(OctString));
  Result := True;
except
  Result := False;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryOctStrToNumber(const OctString: String; out Value: UInt32): Boolean;
begin
try
  Value := UInt32(OctStrToNumber(OctString));
  Result := True;
except
  Result := False;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryOctStrToNumber(const OctString: String; out Value: UInt64): Boolean;
begin
try
  Value := UInt64(OctStrToNumber(OctString));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function OctStrToNumberDef(const OctString: String; Default: UInt64): UInt64;
begin
If not TryOctStrToNumber(OctString,Result) then
  Result := Default;
end;

{-------------------------------------------------------------------------------
================================================================================
                     General data <-> Hex string conversions
================================================================================
-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------
    General data <-> Hex string conversions - auxiliary functions
-------------------------------------------------------------------------------}

Function DataToHexStr_SplitChars(Split: THexStringSplit): Integer;
begin
case Split of
  hssNibble:  Result := 1;
  hssByte:    Result := 2;
  hssWord:    Result := 4;
  hss24bits:  Result := 6;
  hssLong:    Result := 8;
  hssQuad:    Result := 16;
  hss80bits:  Result := 20;
  hssOcta:    Result := 32;
else
 {hssNone}
  Result := 0;
end;
end;

{-------------------------------------------------------------------------------
    General data <-> Hex string conversions - main implementation
-------------------------------------------------------------------------------}

Function DataToHexStr(const Buffer; Size: TMemSize; HexStringFormat: THexStringFormat): String;
var
  SplitCnt:   Integer;
  i:          TMemSize;
  CharResPos: Integer;
  DataResPos: TMemSize;
  BuffPtr:    PByte;
  TempStr:    String;

  procedure PutChar(NewChar: Char);
  begin
    If (SplitCnt > 0) and (DataResPos > 0) and ((DataResPos mod TMemSize(SplitCnt)) = 0) then
      Inc(CharResPos);
    Result[CharResPos] := NewChar;
    Inc(CharResPos);
    Inc(DataResPos)
  end;

begin
If Size <> 0 then
  begin
    // preallocate resulting string and then just fill it
    SplitCnt := DataToHexStr_SplitChars(HexStringFormat.Split);
    If SplitCnt > 0 then
      Result := StringOfChar(HexStringFormat.SplitChar,(UInt64(Size) * 2) +
                             (Pred(UInt64(Size) * 2) div UInt64(SplitCnt)))
    else
      SetLength(Result,Size * 2);
    CharResPos := 1;
    DataResPos := 0;
    BuffPtr := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        If HexStringFormat.UpperCase then
          TempStr := AnsiUpperCase(IntToHex(BuffPtr^,2))
        else
          TempStr := AnsiLowerCase(IntToHex(BuffPtr^,2));
        If Length(TempStr) = 2 then
          begin
            PutChar(TempStr[1]);
            PutChar(TempStr[2]);
          end
        else raise EBOConversionError.CreateFmt('DataToHexStr: Invalid string length (%d).',[Length(TempStr)]);
        Inc(BuffPtr);
      end;
  end
else Result := '';
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DataToHexStr(Arr: array of UInt8; HexStringFormat: THexStringFormat): String;
var
  SplitCnt:   Integer;
  i:          Integer;
  CharResPos: Integer;
  DataResPos: TMemSize;
  TempStr:    String;

  procedure PutChar(NewChar: Char);
  begin
    If (SplitCnt > 0) and (DataResPos > 0) and ((DataResPos mod TMemSize(SplitCnt)) = 0) then
      Inc(CharResPos);
    Result[CharResPos] := NewChar;
    Inc(CharResPos);
    Inc(DataResPos)
  end;

begin
If Length(Arr) <> 0 then
  begin
    SplitCnt := DataToHexStr_SplitChars(HexStringFormat.Split);
    If SplitCnt > 0 then
      Result := StringOfChar(HexStringFormat.SplitChar,(Length(Arr) * 2) +
                             (Pred(Length(Arr) * 2) div SplitCnt))
    else
      SetLength(Result,Length(Arr) * 2);
    CharResPos := 1;
    DataResPos := 0;
    For i := Low(Arr) to High(Arr) do
      begin
        If HexStringFormat.UpperCase then
          TempStr := AnsiUpperCase(IntToHex(Arr[i],2))
        else
          TempStr := AnsiLowerCase(IntToHex(Arr[i],2));
        If Length(TempStr) = 2 then
          begin
            PutChar(TempStr[1]);
            PutChar(TempStr[2]);
          end
        else raise EBOConversionError.CreateFmt('DataToHexStr: Invalid string length (%d).',[Length(TempStr)]);
      end;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function DataToHexStr(const Buffer; Size: TMemSize; Split: THexStringSplit): String;
var
  Format: THexStringFormat;
begin
Format := DefHexStringFormat;
Format.Split := Split;
Result := DataToHexStr(Buffer,Size,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DataToHexStr(Arr: array of UInt8; Split: THexStringSplit): String;
var
  Format: THexStringFormat;
begin
Format := DefHexStringFormat;
Format.Split := Split;
Result := DataToHexStr(Arr,Format);
end;

//------------------------------------------------------------------------------

Function DataToHexStr(const Buffer; Size: TMemSize): String;
begin
Result := DataToHexStr(Buffer,Size,DefHexStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DataToHexStr(Arr: array of UInt8): String; overload;
begin
Result := DataToHexStr(Arr,DefHexStringFormat);
end;

//==============================================================================

Function HexStrToData(const Str: String; out Buffer; Size: TMemSize; HexStringFormat: THexStringFormat): TMemSize;
var
  i,Cntr:   Integer;
  BuffPtr:  PByte;
  StrBuff:  String;
begin
If Length(Str) > 0 then
  begin
    If Size <> 0 then
      begin
        Cntr := 2;        // position in StrBuff just behind $
        BuffPtr := @Buffer;
        StrBuff := '$  '; // two spaces
        Result := 0;
        For i := 1 to Length(Str) do
          begin
            If Str[i] = HexStringFormat.SplitChar then
              Continue
            else If CharInSet(Str[i],['0'..'9','a'..'f','A'..'F']) then
              begin
                StrBuff[Cntr] := Str[i];
                Inc(Cntr);
                If Cntr > 3 then
                  begin
                    If Result < Size then
                      BuffPtr^ := UInt8(StrToInt(StrBuff))
                    else
                      raise EBOBufferTooSmall.CreateFmt('HexStrToData: Buffer too small (%d).',[Size]);
                    Cntr := 2;
                    Inc(BuffPtr);
                    Inc(Result);
                  end;
              end
            else raise EBOInvalidCharacter.CreateFmt('HexStrToData: Invalid character (#%d).',[Ord(Str[i])]);
          end;
      end
    else
      begin
        // only return required size, do not convert
        Cntr := Length(Str);
        For i := 1 to Length(Str) do
          If Str[i] = HexStringFormat.SplitChar then
            Dec(Cntr)
          else If not CharInSet(Str[i],['0'..'9','a'..'f','A'..'F']) then
            raise EBOInvalidCharacter.CreateFmt('HexStrToData: Invalid character (#%d).',[Ord(Str[i])]);
        Result := TMemSize(Cntr div 2);
      end;
  end
else Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function HexStrToData(const Str: String; HexStringFormat: THexStringFormat): TArrayOfBytes;
var
  ByteCnt:  Integer;
begin
ByteCnt := HexStrToData(Str,nil^,0,HexStringFormat);
Result := nil;
SetLength(Result,ByteCnt);
If ByteCnt > 0 then
  begin
    ByteCnt := HexStrToData(Str,Result[0],Length(Result),HexStringFormat);
    If ByteCnt <> Length(Result) then
      SetLength(Result,ByteCnt);
  end;
end;

//------------------------------------------------------------------------------

Function HexStrToData(const Str: String; out Buffer; Size: TMemSize; SplitChar: Char): TMemSize;
var
  Format: THexStringFormat;
begin
Format := DefHexStringFormat;
Format.SplitChar := SplitChar;
Result := HexStrToData(Str,Buffer,Size,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function HexStrToData(const Str: String; SplitChar: Char): TArrayOfBytes;
var
  Format: THexStringFormat;
begin
Format := DefHexStringFormat;
Format.SplitChar := SplitChar;
Result := HexStrToData(Str,Format);
end;

//------------------------------------------------------------------------------

Function HexStrToData(const Str: String; out Buffer; Size: TMemSize): TMemSize;
begin
Result := HexStrToData(Str,Buffer,Size,DefHexStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function HexStrToData(const Str: String): TArrayOfBytes;
begin
Result := HexStrToData(Str,DefHexStringFormat);
end;

//==============================================================================

Function TryHexStrToData(const Str: String; out Buffer; var Size: TMemSize; HexStringFormat: THexStringFormat): Boolean;
begin
try
  Size := HexStrToData(Str,Buffer,Size,HexStringFormat);
  Result := True;
except
  Result := False;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryHexStrToData(const Str: String; out Arr: TArrayOfBytes; HexStringFormat: THexStringFormat): Boolean;
begin
try
  Arr := HexStrToData(Str,HexStringFormat);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TryHexStrToData(const Str: String; out Buffer; var Size: TMemSize; SplitChar: Char): Boolean;
var
  Format: THexStringFormat;
begin
Format := DefHexStringFormat;
Format.SplitChar := SplitChar;
Result := TryHexStrToData(Str,Buffer,Size,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryHexStrToData(const Str: String; out Arr: TArrayOfBytes; SplitChar: Char): Boolean;
var
  Format: THexStringFormat;
begin
Format := DefHexStringFormat;
Format.SplitChar := SplitChar;
Result := TryHexStrToData(Str,Arr,Format);
end;

//------------------------------------------------------------------------------

Function TryHexStrToData(const Str: String; out Buffer; var Size: TMemSize): Boolean;
begin
Result := TryHexStrToData(Str,Buffer,Size,DefHexStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryHexStrToData(const Str: String; out Arr: TArrayOfBytes): Boolean;
begin
Result := TryHexStrToData(Str,Arr,DefHexStringFormat);
end;

{-------------------------------------------------------------------------------
================================================================================
                     General data <-> Bit string conversions
================================================================================
-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------
    General data <-> Bit string conversions - auxiliary functions
-------------------------------------------------------------------------------}

Function DataToBitStr_SplitChars(Split: TBitStringSplit): Integer;
begin
case Split of
  bss4bits:   Result := 4;
  bss8bits:   Result := 8;
  bss16bits:  Result := 16;
  bss32bits:  Result := 32;
else
 {hssNone}
  Result := 0;
end;
end;

{-------------------------------------------------------------------------------
    General data <-> Bit string conversions - main implementation
-------------------------------------------------------------------------------}

Function DataToBitStr(const Buffer; Size: TMemSize; BitStringFormat: TDataBitStringFormat): String;
var
  SplitCnt:   Integer;
  i,j:        TMemSize;
  CharResPos: Integer;
  DataResPos: TMemSize;
  BuffPtr:    PByte;
  TempByte:   UInt8;

  procedure PutChar(NewChar: Char);
  begin
    If (SplitCnt > 0) and (DataResPos > 0) and ((DataResPos mod TMemSize(SplitCnt)) = 0) then
      Inc(CharResPos);
    Result[CharResPos] := NewChar;
    Inc(CharResPos);
    Inc(DataResPos)
  end;

begin
If Size <> 0 then
  begin
    SplitCnt := DataToBitStr_SplitChars(BitStringFormat.Split);
    If SplitCnt > 0 then
      Result := StringOfChar(BitStringFormat.SplitChar,(UInt64(Size) * 8) +
                             (Pred(UInt64(Size) * 8) div UInt64(SplitCnt)))
    else
      SetLength(Result,Size * 8);
    CharResPos := 1;
    DataResPos := 0;
    If BitStringFormat.BytesOrder = bsoLeftToRight then
      BuffPtr := @Buffer
    else
      BuffPtr := PtrAdvance(@Buffer,Size - 1);
    For i := 0 to Pred(Size) do
      begin
        If BitStringFormat.BitsInByteOrder = bsoLeftToRight then
          TempByte := UInt8(BuffPtr^)
        else
          TempByte := ReverseBits(UInt8(BuffPtr^));
        For j := 1 to 8 do
          begin
            If TempByte and 1 <> 0 then
              PutChar('1')
            else
              PutChar('0');
            TempByte := TempByte shr 1;
          end;
        If BitStringFormat.BytesOrder = bsoLeftToRight then
          Inc(BuffPtr)
        else
          Dec(BuffPtr);
      end;
  end
else Result := '';
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DataToBitStr(Arr: array of UInt8; BitStringFormat: TDataBitStringFormat): String;
var
  SplitCnt:   Integer;
  i,j:        TMemSize;
  CharResPos: Integer;
  DataResPos: TMemSize;
  ArrPos:     Integer;
  TempByte:   UInt8;

  procedure PutChar(NewChar: Char);
  begin
    If (SplitCnt > 0) and (DataResPos > 0) and ((DataResPos mod TMemSize(SplitCnt)) = 0) then
      Inc(CharResPos);
    Result[CharResPos] := NewChar;
    Inc(CharResPos);
    Inc(DataResPos)
  end;

begin
If Length(Arr) <> 0 then
  begin
    SplitCnt := DataToBitStr_SplitChars(BitStringFormat.Split);
    If SplitCnt > 0 then
      Result := StringOfChar(BitStringFormat.SplitChar,(UInt64(Length(Arr)) * 8) +
                             (Pred(UInt64(Length(Arr)) * 8) div UInt64(SplitCnt)))
    else
      SetLength(Result,Length(Arr) * 8);
    CharResPos := 1;
    DataResPos := 0;
    If BitStringFormat.BytesOrder = bsoLeftToRight then
      ArrPos := Low(Arr)
    else
      ArrPos := High(Arr);
    For i := 0 to Pred(Length(Arr)) do
      begin
        If BitStringFormat.BitsInByteOrder = bsoLeftToRight then
          TempByte := Arr[ArrPos]
        else
          TempByte := ReverseBits(Arr[ArrPos]);
        For j := 1 to 8 do
          begin
            If TempByte and 1 <> 0 then
              PutChar('1')
            else
              PutChar('0');
            TempByte := TempByte shr 1;
          end;
        If BitStringFormat.BytesOrder = bsoLeftToRight then
          Inc(ArrPos)
        else
          Dec(ArrPos);
      end;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function DataToBitStr(const Buffer; Size: TMemSize; Split: TBitStringSplit): String;
var
  Format: TDataBitStringFormat;
begin
Format := DefDataBitStringFormat;
Format.Split := Split;
Result := DataToBitStr(Buffer,Size,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DataToBitStr(Arr: array of UInt8; Split: TBitStringSplit): String;
var
  Format: TDataBitStringFormat;
begin
Format := DefDataBitStringFormat;
Format.Split := Split;
Result := DataToBitStr(Arr,Format);
end;

//------------------------------------------------------------------------------

Function DataToBitStr(const Buffer; Size: TMemSize): String;
begin
Result := DataToBitStr(Buffer,Size,DefDataBitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DataToBitStr(Arr: array of UInt8): String;
begin
Result := DataToBitStr(Arr,DefDataBitStringFormat);
end;

//==============================================================================

Function BitStrToData(const Str: String; out Buffer; Size: TMemSize; BitStringFormat: TDataBitStringFormat): TMemSize;
var
  i:        Integer;
  BuffPtr:  PByte;
  TempByte: UInt8;
  Cntr:     Integer;
begin
If Length(Str) > 0 then
  begin
    If Size <> 0 then
      begin
        Result := 0;
        If BitStringFormat.BytesOrder = bsoLeftToRight then
          BuffPtr := @Buffer
        else
          BuffPtr := PtrAdvance(@Buffer,Size - 1);
        TempByte := 0;
        Cntr := 0;
        For i := 1 to Length(Str) do
          begin
            If Str[i] = BitStringFormat.SplitChar then
              Continue
            else If CharInSet(Str[i],['0','1']) then
              begin
                If Str[i] <> '0' then
                  TempByte := (TempByte shl 1) or 1
                else
                  TempByte := TempByte shl 1;
                Inc(Cntr);
              end;
            If Cntr >= 8 then
              begin
                If BitStringFormat.BitsInByteOrder = bsoLeftToRight then
                  BuffPtr^ := Byte(ReverseBits(TempByte))
                else
                  BuffPtr^ := Byte(TempByte);
                If BitStringFormat.BytesOrder = bsoLeftToRight then
                  Inc(BuffPtr)
                else
                  Dec(BuffPtr);
                TempByte := 0;
                Cntr := 0;
                Inc(Result);
              end;
          end;
      end
    else
      begin
        // only return required size, do not convert
        Cntr := Length(Str);
        For i := 1 to Length(Str) do
          If Str[i] = BitStringFormat.SplitChar then
            Dec(Cntr)
          else If not CharInSet(Str[i],['0','1']) then
            raise EBOInvalidCharacter.CreateFmt('BitStrToData: Invalid character (#%d).',[Ord(Str[i])]);
        Result := TMemSize(Cntr div 8);
      end;
  end
else Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BitStrToData(const Str: String; BitStringFormat: TDataBitStringFormat): TArrayOfBytes;
var
  ByteCnt:  Integer;
begin
ByteCnt := BitStrToData(Str,nil^,0,BitStringFormat);
Result := nil;
SetLength(Result,ByteCnt);
If ByteCnt > 0 then
  begin
    ByteCnt := BitStrToData(Str,Result[0],Length(Result),BitStringFormat);
    If ByteCnt <> Length(Result) then
      SetLength(Result,ByteCnt);
  end;
end;

//------------------------------------------------------------------------------

Function BitStrToData(const Str: String; out Buffer; Size: TMemSize; SplitChar: Char): TMemSize;
var
  Format: TDataBitStringFormat;
begin
Format := DefDataBitStringFormat;
Format.SplitChar := SplitChar;
Result := BitStrToData(Str,Buffer,Size,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BitStrToData(const Str: String; SplitChar: Char): TArrayOfBytes;
var
  Format: TDataBitStringFormat;
begin
Format := DefDataBitStringFormat;
Format.SplitChar := SplitChar;
Result := BitStrToData(Str,Format);
end;

//------------------------------------------------------------------------------

Function BitStrToData(const Str: String; out Buffer; Size: TMemSize): TMemSize;
begin
Result := BitStrToData(Str,Buffer,Size,DefDataBitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BitStrToData(const Str: String): TArrayOfBytes;
begin
Result := BitStrToData(Str,DefDataBitStringFormat);
end;

//==============================================================================

Function TryBitStrToData(const Str: String; out Buffer; var Size: TMemSize; BitStringFormat: TDataBitStringFormat): Boolean;
begin
try
  Size := BitStrToData(Str,Buffer,Size,BitStringFormat);
  Result := True;
except
  Result := False;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToData(const Str: String; out Arr: TArrayOfBytes; BitStringFormat: TDataBitStringFormat): Boolean;
begin
try
  Arr := BitStrToData(Str,BitStringFormat);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function TryBitStrToData(const Str: String; out Buffer; var Size: TMemSize; SplitChar: Char): Boolean;
var
  Format: TDataBitStringFormat;
begin
Format := DefDataBitStringFormat;
Format.SplitChar := SplitChar;
Result := TryBitStrToData(Str,Buffer,Size,Format);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToData(const Str: String; out Arr: TArrayOfBytes; SplitChar: Char): Boolean;
var
  Format: TDataBitStringFormat;
begin
Format := DefDataBitStringFormat;
Format.SplitChar := SplitChar;
Result := TryBitStrToData(Str,Arr,Format);
end;

//------------------------------------------------------------------------------

Function TryBitStrToData(const Str: String; out Buffer; var Size: TMemSize): Boolean;
begin
Result := TryBitStrToData(Str,Buffer,Size,DefDataBitStringFormat);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryBitStrToData(const Str: String; out Arr: TArrayOfBytes): Boolean;
begin
Result := TryBitStrToData(Str,Arr,DefDataBitStringFormat);
end;


{===============================================================================
--------------------------------------------------------------------------------

                             Bit-level manipulations

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                               Rotate left (ROL)
================================================================================
-------------------------------------------------------------------------------}

Function ROL(Value: UInt8; Shift: Integer): UInt8;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AL, CL
    MOV   RCX, RDX
  {$ELSE}
    MOV   AL, DIL
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    ROL   AL, CL
end;
{$ELSE}
begin
Shift := Shift and 7;
Result := UInt8((Value shl Shift) or (Value shr (8 - Shift)));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ROL(Value: UInt16; Shift: Integer): UInt16;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AX, CX
    MOV   RCX, RDX
  {$ELSE}
    MOV   AX, DI
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    ROL   AX, CL
end;
{$ELSE}
begin
Shift := Shift and 15;
Result := UInt16((Value shl Shift) or (Value shr (16 - Shift)));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ROL(Value: UInt32; Shift: Integer): UInt32;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   EAX, ECX
    MOV   RCX, RDX
  {$ELSE}
    MOV   EAX, EDI
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    ROL   EAX, CL
end;
{$ELSE}
begin
Shift := Shift and 31;
Result := UInt32((Value shl Shift) or (Value shr (32 - Shift)));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ROL(Value: UInt64; Shift: Integer): UInt64;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   RAX, RCX
    MOV   RCX, RDX
  {$ELSE}
    MOV   RAX, RDI
    MOV   RCX, RSI
  {$ENDIF}
    ROL   RAX, CL
{$ELSE}
    MOV   ECX, EAX
    AND   ECX, 63
    CMP   ECX, 32

    JAE   @Above31

  @Below32:
    MOV   EAX, dword ptr [Value]
    MOV   EDX, dword ptr [Value + 4]
    CMP   ECX, 0
    JE    @FuncEnd

    MOV   dword ptr [Value], EDX
    JMP   @Rotate

  @Above31:
    MOV   EDX, dword ptr [Value]
    MOV   EAX, dword ptr [Value + 4]
    JE    @FuncEnd

    AND   ECX, 31

  @Rotate:
    SHLD  EDX, EAX, CL
    SHL   EAX, CL
    PUSH  EAX
    MOV   EAX, dword ptr [Value]
    XOR   CL, 31
    INC   CL
    SHR   EAX, CL
    POP   ECX
    OR    EAX, ECX

  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Shift := Shift and 63;
Result := UInt64((Value shl Shift) or (Value shr (64 - Shift)));
end;
{$ENDIF}

//==============================================================================

procedure ROLValue(var Value: UInt8; Shift: Integer);
begin
Value := ROL(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ROLValue(var Value: UInt16; Shift: Integer);
begin
Value := ROL(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ROLValue(var Value: UInt32; Shift: Integer);
begin
Value := ROL(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ROLValue(var Value: UInt64; Shift: Integer);
begin
Value := ROL(Value,Shift);
end;

{-------------------------------------------------------------------------------
================================================================================
                               Rotate right (ROR)
================================================================================
-------------------------------------------------------------------------------}

Function ROR(Value: UInt8; Shift: Integer): UInt8;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AL, CL
    MOV   RCX, RDX
  {$ELSE}
    MOV   AL, DIL
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    ROR   AL, CL
end;
{$ELSE}
begin
Shift := Shift and 7;
Result := UInt8((Value shr Shift) or (Value shl (8 - Shift)));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ROR(Value: UInt16; Shift: Integer): UInt16;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AX, CX
    MOV   RCX, RDX
  {$ELSE}
    MOV   AX, DI
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    ROR   AX, CL
end;
{$ELSE}
begin
Shift := Shift and 15;
Result := UInt16((Value shr Shift) or (Value shl (16 - Shift)));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ROR(Value: UInt32; Shift: Integer): UInt32;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   EAX, ECX
    MOV   RCX, RDX
  {$ELSE}
    MOV   EAX, EDI
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    ROR   EAX, CL
end;
{$ELSE}
begin
Shift := Shift and 31;
Result := UInt32((Value shr Shift) or (Value shl (32 - Shift)));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ROR(Value: UInt64; Shift: Integer): UInt64;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   RAX, RCX
    MOV   RCX, RDX
  {$ELSE}
    MOV   RAX, RDI
    MOV   RCX, RSI
  {$ENDIF}
    ROR   RAX, CL
{$ELSE}
    MOV   ECX, EAX
    AND   ECX, 63
    CMP   ECX, 32

    JAE   @Above31

  @Below32:
    MOV   EAX, dword ptr [Value]
    MOV   EDX, dword ptr [Value + 4]
    CMP   ECX, 0
    JE    @FuncEnd

    MOV   dword ptr [Value], EDX
    JMP   @Rotate

  @Above31:
    MOV   EDX, dword ptr [Value]
    MOV   EAX, dword ptr [Value + 4]
    JE    @FuncEnd

    AND   ECX, 31

  @Rotate:
    SHRD  EDX, EAX, CL
    SHR   EAX, CL
    PUSH  EAX
    MOV   EAX, dword ptr [Value]
    XOR   CL, 31
    INC   CL
    SHL   EAX, CL
    POP   ECX
    OR    EAX, ECX

  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Shift := Shift and 63;
Result := UInt64((Value shr Shift) or (Value shl (64 - Shift)));
end;
{$ENDIF}

//==============================================================================

procedure RORValue(var Value: UInt8; Shift: Integer);
begin
Value := ROR(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RORValue(var Value: UInt16; Shift: Integer);
begin
Value := ROR(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RORValue(var Value: UInt32; Shift: Integer);
begin
Value := ROR(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RORValue(var Value: UInt64; Shift: Integer);
begin
Value := ROR(Value,Shift);
end;

{-------------------------------------------------------------------------------
================================================================================
                          Rotate left with carry (RCL)
================================================================================
-------------------------------------------------------------------------------}

Function RCLCarry(Value: UInt8; Shift: Integer; var CF: Boolean): UInt8;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AL, CL
    MOV   RCX, RDX
    ADD   byte ptr [R8], $FF
    RCL   AL, CL
    SETC  byte ptr [R8]
  {$ELSE}
    MOV   AL, DIL
    MOV   RCX, RSI
    ADD   byte ptr [RDX], $FF
    RCL   AL, CL
    SETC  byte ptr [RDX]
  {$ENDIF}
{$ELSE}
    XCHG  EDX, ECX
    ADD   byte ptr [EDX], $FF
    RCL   AL, CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and 31;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and UInt8($80)) <> 0;
    If Carry then
      Result := UInt8((Result shl 1) or UInt8(1))
    else
      Result := UInt8(Result shl 1);
    Carry := CF;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCLCarry(Value: UInt16; Shift: Integer; var CF: Boolean): UInt16;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AX, CX
    MOV   RCX, RDX
    ADD   byte ptr [R8], $FF
    RCL   AX, CL
    SETC  byte ptr [R8]
  {$ELSE}
    MOV   AX, DI
    MOV   RCX, RSI
    ADD   byte ptr [RDX], $FF
    RCL   AX, CL
    SETC  byte ptr [RDX]
  {$ENDIF}
{$ELSE}
    XCHG  EDX, ECX
    ADD   byte ptr [EDX], $FF
    RCL   AX, CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and 31;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and UInt16($8000)) <> 0;
    If Carry then
      Result := UInt16((Result shl 1) or UInt16(1))
    else
      Result := UInt16(Result shl 1);
    Carry := CF;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCLCarry(Value: UInt32; Shift: Integer; var CF: Boolean): UInt32;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   EAX, ECX
    MOV   RCX, RDX
    ADD   byte ptr [R8], $FF
    RCL   EAX, CL
    SETC  byte ptr [R8]
  {$ELSE}
    MOV   EAX, EDI
    MOV   RCX, RSI
    ADD   byte ptr [RDX], $FF
    RCL   EAX, CL
    SETC  byte ptr [RDX]
  {$ENDIF}
{$ELSE}
    XCHG  EDX, ECX
    ADD   byte ptr [EDX], $FF
    RCL   EAX, CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and 31;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and UInt32($80000000)) <> 0;
    If Carry then
      Result := UInt32((Result shl 1) or UInt32(1))
    else
      Result := UInt32(Result shl 1);
    Carry := CF;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCLCarry(Value: UInt64; Shift: Integer; var CF: Boolean): UInt64;
{$IFNDEF PurePascal}
{$IFDEF x64}
asm
  {$IFDEF Windows}
    MOV   RAX, RCX
    MOV   RCX, RDX
    ADD   byte ptr [R8], $FF
    RCL   RAX, CL
    SETC  byte ptr [R8]
  {$ELSE}
    MOV   RAX, RDI
    MOV   RCX, RSI
    ADD   byte ptr [RDX], $FF
    RCL   RAX, CL
    SETC  byte ptr [RDX]
  {$ENDIF}
end;
{$ELSE}
var
  TempShift:  UInt32;
asm
    PUSH  EBX

    AND   EAX, 63
    MOV   dword ptr [TempShift], EAX
    MOV   ECX, EAX
    MOV   EBX, EDX

    MOV   EAX, dword ptr [Value]
    MOV   EDX, dword ptr [Value + 4]
    CMP   ECX, 32

    JE    @Exactly32
    JA    @Above32

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    TEST  ECX, ECX
    JZ    @FuncEnd

    SHLD  EDX, EAX, CL
    JMP   @Shift

{- Shift is above 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above32:
    AND   ECX, 31

    DEC   CL
    SHLD  EDX, EAX, CL
    INC   CL

{- Main shifting  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Shift:
    SHL   EAX, CL
    PUSH  ECX
    PUSH  EAX
    MOV   EAX, dword ptr [Value + 4]
    SHR   EAX, 2
    XOR   CL, 31
    SHR   EAX, CL
    POP   ECX
    OR    EAX, ECX
    POP   ECX
    JMP   @SetCarry

{- Shift is equal to 32, no shifting required, only swap Hi and Lo dwords - - -}
  @Exactly32:
    SHR   EDX, 1
    XCHG  EAX, EDX

{- Write passed carry bit to the result - - - - - - - - - - - - - - - - - - - -}
  @SetCarry:
    DEC   ECX
    CMP   byte ptr [EBX], 0
    JE    @ResetBit

    BTS   EAX, ECX
    JMP   @Swap

  @ResetBit:
    BTR   EAX, ECX

{- Swap Hi and Lo dwords for shift > 32 - - - - - - - - - - - - - - - - - - - -}
  @Swap:
    CMP   byte ptr [TempShift], 32
    JBE   @GetCarry
    XCHG  EAX, EDX

{- Get carry bit that will be output in CF parameter  - - - - - - - - - - - - -}
  @GetCarry:
    MOV   CL, byte ptr [TempShift]
    AND   ECX, 63
    CMP   CL, 32
    JBE   @FromHigh

    AND   CL, 31
    DEC   CL
    XOR   CL, 31
    BT    dword ptr [Value], ECX
    JMP   @StoreCarry

  @FromHigh:
    DEC   CL
    XOR   CL, 31
    BT    dword ptr [Value + 4], ECX

  @StoreCarry:
    SETC  CL
    MOV   byte ptr [EBX], CL

{- Restore EBX register - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
    POP   EBX
end;
{$ENDIF}
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and 63;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and UInt64($8000000000000000)) <> 0;
    If Carry then
      Result := UInt64((Result shl 1) or UInt64(1))
    else
      Result := UInt64(Result shl 1);
    Carry := CF;
  end;
end;
{$ENDIF}


//==============================================================================

Function RCL(Value: UInt8; Shift: Integer; CF: Boolean = False): UInt8;
var
  TempCF: Boolean;
begin
TempCF := CF;
Result := RCLCarry(Value,Shift,TempCF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCL(Value: UInt16; Shift: Integer; CF: Boolean = False): UInt16;
var
  TempCF: Boolean;
begin
TempCF := CF;
Result := RCLCarry(Value,Shift,TempCF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCL(Value: UInt32; Shift: Integer; CF: Boolean = False): UInt32;
var
  TempCF: Boolean;
begin
TempCF := CF;
Result := RCLCarry(Value,Shift,TempCF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCL(Value: UInt64; Shift: Integer; CF: Boolean = False): UInt64;
var
  TempCF: Boolean;
begin
TempCF := CF;
Result := RCLCarry(Value,Shift,TempCF);
end;

//==============================================================================

procedure RCLValueCarry(var Value: UInt8; Shift: Integer; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCLValueCarry(var Value: UInt16; Shift: Integer; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCLValueCarry(var Value: UInt32; Shift: Integer; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCLValueCarry(var Value: UInt64; Shift: Integer; var CF: Boolean);
begin
Value := RCLCarry(Value,Shift,CF);
end;

//==============================================================================

procedure RCLValue(var Value: UInt8; Shift: Integer; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCLValue(var Value: UInt16; Shift: Integer; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCLValue(var Value: UInt32; Shift: Integer; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCLValue(var Value: UInt64; Shift: Integer; CF: Boolean = False);
begin
Value := RCL(Value,Shift,CF);
end;

{-------------------------------------------------------------------------------
================================================================================
                         Rotate right with carry (RCR)
================================================================================
-------------------------------------------------------------------------------}

Function RCRCarry(Value: UInt8; Shift: Integer; var CF: Boolean): UInt8;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AL, CL
    MOV   RCX, RDX
    ADD   byte ptr [R8], $FF
    RCR   AL, CL
    SETC  byte ptr [R8]
  {$ELSE}
    MOV   AL, DIL
    MOV   RCX, RSI
    ADD   byte ptr [RDX], $FF
    RCR   AL, CL
    SETC  byte ptr [RDX]
  {$ENDIF}
{$ELSE}
    XCHG  EDX, ECX
    ADD   byte ptr [EDX], $FF
    RCR   AL,  CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and 31;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    If Carry then
      Result := UInt8((Result shr 1) or UInt8($80))
    else
      Result := UInt8(Result shr 1);
    Carry := CF;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCRCarry(Value: UInt16; Shift: Integer; var CF: Boolean): UInt16;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AX, CX
    MOV   RCX, RDX
    ADD   byte ptr [R8], $FF
    RCR   AX, CL
    SETC  byte ptr [R8]
  {$ELSE}
    MOV   AX, DI
    MOV   RCX, RSI
    ADD   byte ptr [RDX], $FF
    RCR   AX, CL
    SETC  byte ptr [RDX]
  {$ENDIF}
{$ELSE}
    XCHG  EDX, ECX
    ADD   byte ptr [EDX], $FF
    RCR   AX, CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and 31;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    If Carry then
      Result := UInt16((Result shr 1) or UInt16($8000))
    else
      Result := UInt16(Result shr 1);
    Carry := CF;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCRCarry(Value: UInt32; Shift: Integer; var CF: Boolean): UInt32;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   EAX, ECX
    MOV   RCX, RDX
    ADD   byte ptr [R8], $FF
    RCR   EAX, CL
    SETC  byte ptr [R8]
  {$ELSE}
    MOV   EAX, EDI
    MOV   RCX, RSI
    ADD   byte ptr [RDX], $FF
    RCR   EAX, CL
    SETC  byte ptr [RDX]
  {$ENDIF}
{$ELSE}
    XCHG  EDX, ECX
    ADD   byte ptr [EDX], $FF
    RCR   EAX, CL
    SETC  byte ptr [EDX]
{$ENDIF}
end;
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and 31;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    If Carry then
      Result := UInt32((Result shr 1) or UInt32($80000000))
    else
      Result := UInt32(Result shr 1);
    Carry := CF;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCRCarry(Value: UInt64; Shift: Integer; var CF: Boolean): UInt64;
{$IFNDEF PurePascal}
{$IFDEF x64}
asm
  {$IFDEF Windows}
    MOV   RAX, RCX
    MOV   RCX, RDX
    ADD   byte ptr [R8], $FF
    RCR   RAX, CL
    SETC  byte ptr [R8]
  {$ELSE}
    MOV   RAX, RDI
    MOV   RCX, RSI
    ADD   byte ptr [RDX], $FF
    RCR   RAX, CL
    SETC  byte ptr [RDX]
  {$ENDIF}
end;
{$ELSE}
var
  TempShift:  UInt32;
asm
    PUSH  EBX

    AND   EAX, 63
    MOV   dword ptr [TempShift], EAX
    MOV   ECX, EAX
    MOV   EBX, EDX

    MOV   EAX, dword ptr [Value]
    MOV   EDX, dword ptr [Value + 4]
    CMP   ECX, 32

    JE    @Exactly32
    JA    @Above32

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    TEST  ECX, ECX
    JZ    @FuncEnd

    SHRD  EAX, EDX, CL
    JMP   @Shift

{- Shift is above 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above32:
    AND   ECX, 31

    DEC   CL
    SHRD  EAX, EDX, CL
    INC   CL

{- Main shifting  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Shift:
    SHR   EDX, CL
    PUSH  ECX
    PUSH  EDX
    MOV   EDX, dword ptr [Value]
    SHL   EDX, 2
    XOR   CL, 31
    SHL   EDX, CL
    POP   ECX
    OR    EDX, ECX
    POP   ECX
    JMP   @SetCarry

{- Shift is equal to 32, no shifting required, only swap Hi and Lo dwords - - -}
  @Exactly32:
    SHL   EAX, 1
    XCHG  EAX, EDX

{- Write passed carry bit to the result - - - - - - - - - - - - - - - - - - - -}
  @SetCarry:
    DEC   ECX
    XOR   ECX, 31
    CMP   byte ptr [EBX], 0
    JE    @ResetBit

    BTS   EDX, ECX
    JMP   @Swap

  @ResetBit:
    BTR   EDX, ECX

{- Swap Hi and Lo dwords for shift > 32 - - - - - - - - - - - - - - - - - - - -}
  @Swap:
    CMP   byte ptr [TempShift], 32
    JBE   @GetCarry
    XCHG  EAX, EDX

{- Get carry bit that will be output in CF parameter  - - - - - - - - - - - - -}
  @GetCarry:
    MOV   CL,  byte ptr [TempShift]
    AND   ECX, 63
    CMP   CL, 32
    JA    @FromHigh

    DEC   CL
    BT    dword ptr [Value], ECX
    JMP   @StoreCarry

  @FromHigh:
    AND   CL, 31
    DEC   CL
    BT    dword ptr [Value + 4], ECX

  @StoreCarry:
    SETC  CL
    MOV   byte ptr [EBX], CL

{- Restore EBX register - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
    POP   EBX
end;
{$ENDIF}
{$ELSE}
var
  i:      Integer;
  Carry:  Boolean;
begin
Shift := Shift and 63;
Carry := CF;
Result := Value;
For i := 1 to Shift do
  begin
    CF := (Result and 1) <> 0;
    If Carry then
      Result := UInt64((Result shr 1) or UInt64($8000000000000000))
    else
      Result := UInt64(Result shr 1);
    Carry := CF;
  end;
end;
{$ENDIF}

//==============================================================================

Function RCR(Value: UInt8; Shift: Integer; CF: Boolean = False): UInt8;
var
  TempCF: Boolean;
begin
TempCF := CF;
Result := RCRCarry(Value,Shift,TempCF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCR(Value: UInt16; Shift: Integer; CF: Boolean = False): UInt16;
var
  TempCF: Boolean;
begin
TempCF := CF;
Result := RCRCarry(Value,Shift,TempCF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCR(Value: UInt32; Shift: Integer; CF: Boolean = False): UInt32;
var
  TempCF: Boolean;
begin
TempCF := CF;
Result := RCRCarry(Value,Shift,TempCF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function RCR(Value: UInt64; Shift: Integer; CF: Boolean = False): UInt64;
var
  TempCF: Boolean;
begin
TempCF := CF;
Result := RCRCarry(Value,Shift,TempCF);
end;

//==============================================================================

procedure RCRValueCarry(var Value: UInt8; Shift: Integer; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCRValueCarry(var Value: UInt16; Shift: Integer; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCRValueCarry(var Value: UInt32; Shift: Integer; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCRValueCarry(var Value: UInt64; Shift: Integer; var CF: Boolean);
begin
Value := RCRCarry(Value,Shift,CF);
end;

//==============================================================================

procedure RCRValue(var Value: UInt8; Shift: Integer; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCRValue(var Value: UInt16; Shift: Integer; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCRValue(var Value: UInt32; Shift: Integer; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RCRValue(var Value: UInt64; Shift: Integer; CF: Boolean = False);
begin
Value := RCR(Value,Shift,CF);
end;

{-------------------------------------------------------------------------------
================================================================================
                          Arithmetic left shift (SAL)
================================================================================
-------------------------------------------------------------------------------}

Function SAL(Value: UInt8; Shift: Integer): UInt8;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AL, CL
    MOV   RCX, RDX
  {$ELSE}
    MOV   AL, DIL
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    SAL   AL, CL
end;
{$ELSE}
begin
{
  Typecasting of Shift to 8bit is here due to problems in FPC (PurePascal,
  inlining active, O3 optimization). The error reported during compilation is...

    Error: (8007) Asm: [shl mem16,reg16] invalid combination of opcode and operands

  ...and is shown for second (16bit) overload of function SALValue on its only
  implementation line.

  It seems to manifests only for 16bit integers, but I have put it everywhere
  as a precaution.
}
Result := UInt8(Value shl UInt8(Shift));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SAL(Value: UInt16; Shift: Integer): UInt16;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AX, CX
    MOV   RCX, RDX
  {$ELSE}
    MOV   AX, DI
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    SAL   AX, CL
end;
{$ELSE}
begin
Result := UInt16(Value shl UInt8(Shift));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SAL(Value: UInt32; Shift: Integer): UInt32;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   EAX, ECX
    MOV   RCX, RDX
  {$ELSE}
    MOV   EAX, EDI
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    SAL   EAX, CL
end;
{$ELSE}
begin
Result := UInt32(Value shl UInt8(Shift));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SAL(Value: UInt64; Shift: Integer): UInt64;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   RAX, RCX
    MOV   RCX, RDX
  {$ELSE}
    MOV   RAX, RDI
    MOV   RCX, RSI
  {$ENDIF}
    SAL   RAX, CL
{$ELSE}
    MOV   ECX, EAX
    AND   ECX, 63

    CMP   ECX, 31
    JA    @Above31

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    MOV   EAX, dword ptr [Value]
    MOV   EDX, dword ptr [Value + 4]

    TEST  ECX, ECX
    JZ    @FuncEnd

    SHLD  EDX, EAX, CL
    SHL   EAX, CL
    JMP   @FuncEnd

{- Shift is above 31  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above31:
    XOR   EAX, EAX
    MOV   EDX, dword ptr [Value]
    AND   ECX, 31
    SHL   EDX, CL

{- End of the function  - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
{
  We need to explicitly mask the Shift, because Delphi (at least D7) 32bit
  software implementation just nulls the result for large shifts (64+), which
  does not conform to Intel's documentation of SHL/SAL instruction.
}
Result := UInt64(Value shl UInt8(Shift and 63));
end;
{$ENDIF}

//==============================================================================

procedure SALValue(var Value: UInt8; Shift: Integer);
begin
Value := SAL(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SALValue(var Value: UInt16; Shift: Integer);
begin
Value := SAL(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SALValue(var Value: UInt32; Shift: Integer);
begin
Value := SAL(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SALValue(var Value: UInt64; Shift: Integer);
begin
Value := SAL(Value,Shift);
end;

{-------------------------------------------------------------------------------
================================================================================
                          Arithmetic right shift (SAR)
================================================================================
-------------------------------------------------------------------------------}

Function SAR(Value: UInt8; Shift: Integer): UInt8;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AL, CL
    MOV   RCX, RDX
  {$ELSE}
    MOV   AL, DIL
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    SAR   AL, CL
end;
{$ELSE}
begin
Shift := Shift and 31;
If Shift < 8 then
  begin
    If (Value and UInt8($80)) <> 0 then
      Result := UInt8((Value shr Shift) or (UInt8($FF) shl (8 - Shift)))
    else
      Result := UInt8(Value shr Shift);
  end
else
  begin
    If (Value and UInt8($80)) <> 0 then
      Result := UInt8($FF)
    else
      Result := 0;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SAR(Value: UInt16; Shift: Integer): UInt16;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AX, CX
    MOV   RCX, RDX
  {$ELSE}
    MOV   AX, DI
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    SAR   AX, CL
end;
{$ELSE}
begin
Shift := Shift and 31;
If Shift < 16 then
  begin
    If (Value and UInt16($8000)) <> 0 then
      Result := UInt16((Value shr Shift) or (UInt16($FFFF) shl (16 - Shift)))
    else
      Result := UInt16(Value shr Shift);
  end
else
  begin
    If (Value and UInt16($8000)) <> 0 then
      Result := UInt16($FFFF)
    else
      Result := 0;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SAR(Value: UInt32; Shift: Integer): UInt32;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   EAX, ECX
    MOV   RCX, RDX
  {$ELSE}
    MOV   EAX, EDI
    MOV   RCX, RSI
  {$ENDIF}
{$ELSE}
    MOV   ECX, EDX
{$ENDIF}
    SAR   EAX, CL
end;
{$ELSE}
begin
Shift := Shift and 31;
If Shift <> 0 then
  begin
    If (Value and UInt32($80000000)) <> 0 then
      Result := UInt32((Value shr Shift) or (UInt32($FFFFFFFF) shl (32 - Shift)))
    else
      Result := UInt32(Value shr Shift);
  end
else Result := Value;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SAR(Value: UInt64; Shift: Integer): UInt64;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   RAX, RCX
    MOV   RCX, RDX
  {$ELSE}
    MOV   RAX, RDI
    MOV   RCX, RSI
  {$ENDIF}
    SAR   RAX, CL
{$ELSE}
    MOV   ECX, EAX
    AND   ECX, 63

    CMP   ECX, 31
    JA    @Above31

{- Shift is below 32  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
    MOV   EAX, dword ptr [Value]
    MOV   EDX, dword ptr [Value + 4]

    TEST  ECX, ECX
    JZ    @FuncEnd

    SHRD  EAX, EDX, CL
    SAR   EDX, CL
    JMP   @FuncEnd

{- Shift is above 31  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @Above31:
    MOV   EAX, dword ptr [Value + 4]
    BT    EAX, 31
    JC    @BitSet

    XOR   EDX, EDX
    JMP   @DoShift

  @BitSet:
    MOV   EDX, $FFFFFFFF

  @DoShift:
    AND   ECX, 31
    SAR   EAX, CL

{- End of the function  - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  @FuncEnd:
{$ENDIF}
end;
{$ELSE}
begin
Shift := Shift and 63;
If Shift <> 0 then
  begin
    If (Value and UInt64($8000000000000000)) <> 0 then
      Result := UInt64((Value shr Shift) or (UInt64($FFFFFFFFFFFFFFFF) shl (64 - Shift)))
    else
      Result := UInt64(Value shr Shift);
  end
else Result := Value;
end;
{$ENDIF}

//==============================================================================

procedure SARValue(var Value: UInt8; Shift: Integer);
begin
Value := SAR(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SARValue(var Value: UInt16; Shift: Integer);
begin
Value := SAR(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SARValue(var Value: UInt32; Shift: Integer);
begin
Value := SAR(Value,Shift);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SARValue(var Value: UInt64; Shift: Integer);
begin
Value := SAR(Value,Shift);
end;

{-------------------------------------------------------------------------------
================================================================================
                                 Endianity swap
================================================================================
-------------------------------------------------------------------------------}

Function EndianSwap(Value: UInt16): UInt16;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   AX, CX
  {$ELSE}
    MOV   AX, DI
  {$ENDIF}
{$ENDIF}
    XCHG  AL, AH
end;
{$ELSE}
begin
Result := UInt16((Value shl 8) or (Value shr 8));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function EndianSwap(Value: UInt32): UInt32;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   EAX, ECX
  {$ELSE}
    MOV   EAX, EDI
  {$ENDIF}
{$ENDIF}
    BSWAP EAX
end;
{$ELSE}
begin
Result := UInt32(((Value and $000000FF) shl 24) or ((Value and $0000FF00) shl 8) or
                 ((Value and $00FF0000) shr 8) or ((Value and $FF000000) shr 24));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function EndianSwap(Value: UInt64): UInt64;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV   RAX, RCX
  {$ELSE}
    MOV   RAX, RDI
  {$ENDIF}
    BSWAP RAX
{$ELSE}
    MOV   EAX, dword ptr [Value + 4]
    MOV   EDX, dword ptr [Value]
    BSWAP EAX
    BSWAP EDX
{$ENDIF}
end;
{$ELSE}
begin
Int64Rec(Result).Hi := EndianSwap(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := EndianSwap(Int64Rec(Value).Hi);
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function SwapEndian(Value: UInt16): UInt16;
begin
Result := EndianSwap(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: UInt32): UInt32;
begin
Result := EndianSwap(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: UInt64): UInt64;
begin
Result := EndianSwap(Value);
end;

//==============================================================================

procedure EndianSwapValue(var Value: UInt16);
begin
Value := EndianSwap(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure EndianSwapValue(var Value: UInt32);
begin
Value := EndianSwap(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure EndianSwapValue(var Value: UInt64);
begin
Value := EndianSwap(Value);
end;

//------------------------------------------------------------------------------

procedure SwapEndianValue(var Value: UInt16);
begin
Value := EndianSwap(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapEndianValue(var Value: UInt32);
begin
Value := EndianSwap(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapEndianValue(var Value: UInt64);
begin
Value := EndianSwap(Value);
end;

//==============================================================================

procedure EndianSwap(var Buffer; Size: TMemSize);
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}

    PUSH  RSI
    PUSH  RDI

    CMP   RDX, 1
    JNA   @RoutineEnd

    MOV   RSI, RCX
    LEA   RDI, [RCX + RDX - 1]

    PUSH  RDX
    SHR   RDX, 4    // div 16 (2 * 8)
    JZ    @RemainingBytes
    SUB   RDI, 7

  @LongLoop:

    MOV   RAX, qword ptr [RSI]
    MOV   RCX, qword ptr [RDI]
    BSWAP RAX
    BSWAP RCX
    MOV   qword ptr [RSI], RCX
    MOV   qword ptr [RDI], RAX

    ADD   RSI, 8
    SUB   RDI, 8

    DEC   RDX
    JNZ   @LongLoop

    ADD   RDI, 7

  @RemainingBytes:

    POP   RDX
    SHR   RDX, 1
    AND   RDX, 7
    JZ    @RoutineEnd

  @ByteLoop:

    MOV   AL, byte ptr [RSI]
    MOV   CL, byte ptr [RDI]
    MOV   byte ptr [RSI], CL
    MOV   byte ptr [RDI], AL

    INC   RSI
    DEC   RDI

    DEC   RDX
    JNZ   @ByteLoop

  @RoutineEnd:

    POP   RDI
    POP   RSI

  {$ELSE}

    CMP   RSI, 1
    JNA   @RoutineEnd

    MOV   RCX, RSI
    MOV   RSI, RDI
    LEA   RDI, [RSI + RCX - 1]

    PUSH  RCX
    SHR   RCX, 4    // div 16 (2 * 8)
    JZ    @RemainingBytes
    SUB   RDI, 7

  @LongLoop:

    MOV   RAX, qword ptr [RSI]
    MOV   RDX, qword ptr [RDI]
    BSWAP RAX
    BSWAP RDX
    MOV   qword ptr [RSI], RDX
    MOV   qword ptr [RDI], RAX

    ADD   RSI, 8
    SUB   RDI, 8

    DEC   RCX
    JNZ   @LongLoop

    ADD   RDI, 7

  @RemainingBytes:

    POP   RCX
    SHR   RCX, 1
    AND   RCX, 7
    JZ    @RoutineEnd

  @ByteLoop:

    MOV   AL, byte ptr [RSI]
    MOV   DL, byte ptr [RDI]
    MOV   byte ptr [RSI], DL
    MOV   byte ptr [RDI], AL

    INC   RSI
    DEC   RDI

    DEC   RCX
    JNZ   @ByteLoop

  @RoutineEnd:

  {$ENDIF}
{$ELSE}

    PUSH  ESI
    PUSH  EDI

    CMP   EDX, 1
    JNA   @RoutineEnd

    MOV   ESI, EAX
    LEA   EDI, [EAX + EDX - 1]

    PUSH  EDX
    SHR   EDX, 3    // div 8 (2 * 4)
    JZ    @RemainingBytes
    SUB   EDI, 3

  @LongLoop:

    MOV   EAX, dword ptr [ESI]
    MOV   ECX, dword ptr [EDI]
    BSWAP EAX
    BSWAP ECX
    MOV   dword ptr [ESI], ECX
    MOV   dword ptr [EDI], EAX

    ADD   ESI, 4
    SUB   EDI, 4

    DEC   EDX
    JNZ   @LongLoop

    ADD   EDI, 3    // rectify high address

  @RemainingBytes:

    POP   EDX
    SHR   EDX, 1    // div 2
    AND   EDX, 3
    JZ    @RoutineEnd

  @ByteLoop:

    MOV   AL, byte ptr [ESI]
    MOV   CL, byte ptr [EDI]
    MOV   byte ptr [ESI], CL
    MOV   byte ptr [EDI], AL

    INC   ESI
    DEC   EDI

    DEC   EDX
    JNZ   @ByteLoop

  @RoutineEnd:

    POP   EDI
    POP   ESI

{$ENDIF}
end;
{$ELSE}
var
  i:        TMemSize;
  SrcPtr:   PByte;
  DstPtr:   PByte;
  ByteTemp: Byte;
begin
case Size of
  Low(Size)..1: Exit;
             2: EndianSwapValue(UInt16(Buffer));
             4: EndianSwapValue(UInt32(Buffer));
             8: EndianSwapValue(UInt64(Buffer));
else
{
  Do not implement for long/quadwords, such optimization would not work due to
  overhead from calls to SwapEndian for each words.
}
  SrcPtr := Addr(Buffer);
  DstPtr := PtrAdvance(Addr(Buffer),Size - 1);
  For i := 0 to Pred(Size div 2) do
    begin
      ByteTemp := SrcPtr^;
      SrcPtr^ := DstPtr^;
      DstPtr^ := ByteTemp;
      Inc(SrcPtr);
      Dec(DstPtr);
    end;
end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapEndian(var Buffer; Size: TMemSize);
begin
EndianSwap(Buffer,Size);
end;

//==============================================================================

procedure EndianSwapItems16(var Arr; Count,Stride: TMemSize);
{$IFNDEF PurePascal}
asm
{
                    win32 & lin32         win64             lin64
       @Arr              EAX               RCX               RDI
      Count              EDX               RDX               RSI
     Stride              ECX               R8                RDX
}
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   RDX, 0
    JNA   @RoutineEnd

@MainLoop:

    MOV   AX, word ptr [RCX]
    XCHG  AL, AH
    MOV   word ptr [RCX], AX

    ADD   RCX, R8
    DEC   RDX
    JNZ   @MainLoop

  {$ELSE}

    CMP   RSI, 0
    JNA   @RoutineEnd

@MainLoop:

    MOV   AX, word ptr [RDI]
    XCHG  AL, AH
    MOV   word ptr [RDI], AX

    ADD   RDI, RDX
    DEC   RSI
    JNZ   @MainLoop

  {$ENDIF}
{$ELSE}

    CMP   EDX, 0
    JNA   @RoutineEnd

    PUSH  EBX

@MainLoop:
  {
    Using string instructions (LODS, STOS) does not seem to provide any
    performance benefit here.
  }
    MOV   BX, word ptr [EAX]
    XCHG  BL, BH
    MOV   word ptr [EAX], BX

    ADD   EAX, ECX 
    DEC   EDX
    JNZ   @MainLoop

    POP   EBX

{$ENDIF}

@RoutineEnd:
end;
{$ELSE}
var
  BuffPtr:  PUInt16;
  i:        TMemSize;
begin
If Count > 0 then
  begin
    BuffPtr := @Arr;
    For i := 0 to Pred(Count) do
      begin
        EndianSwapValue(BuffPtr^);
        Inc(PByte(BuffPtr),Stride);
      end;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure EndianSwapItems32(var Arr; Count,Stride: TMemSize);
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   RDX, 0
    JNA   @RoutineEnd

@MainLoop:

    MOV   EAX, dword ptr [RCX]
    BSWAP EAX
    MOV   dword ptr [RCX], EAX

    ADD   RCX, R8
    DEC   RDX
    JNZ   @MainLoop

  {$ELSE}

    CMP   RSI, 0
    JNA   @RoutineEnd

@MainLoop:

    MOV   EAX, dword ptr [RDI]
    BSWAP EAX
    MOV   dword ptr [RDI], EAX

    ADD   RDI, RDX
    DEC   RSI
    JNZ   @MainLoop

  {$ENDIF}
{$ELSE}

    CMP   EDX, 0
    JNA   @RoutineEnd

    PUSH  EBX

@MainLoop:
  {
    I know about MOVBE instruction, but it is a relatively recent (~2013)
    addition to the instruction set, therefore I am using more backward
    compatible solution of load-bswap-save.
  }
    MOV   EBX, dword ptr [EAX]
    BSWAP EBX
    MOV   dword ptr [EAX], EBX

    ADD   EAX, ECX
    DEC   EDX
    JNZ   @MainLoop

    POP   EBX

{$ENDIF}

@RoutineEnd:
end;
{$ELSE}
var
  BuffPtr:  PUInt32;
  i:        TMemSize;
begin
If Count > 0 then
  begin
    BuffPtr := @Arr;
    For i := 0 to Pred(Count) do
      begin
        EndianSwapValue(BuffPtr^);
        Inc(PByte(BuffPtr),Stride);
      end;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure EndianSwapItems64(var Arr; Count,Stride: TMemSize);
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}

    CMP   RDX, 0
    JNA   @RoutineEnd

@MainLoop:

    MOV   RAX, qword ptr [RCX]
    BSWAP RAX
    MOV   qword ptr [RCX], RAX

    ADD   RCX, R8
    DEC   RDX
    JNZ   @MainLoop

  {$ELSE}

    CMP   RSI, 0
    JNA   @RoutineEnd

@MainLoop:

    MOV   RAX, qword ptr [RDI]
    BSWAP RAX
    MOV   qword ptr [RDI], RAX

    ADD   RDI, RDX
    DEC   RSI
    JNZ   @MainLoop

  {$ENDIF}
{$ELSE}

    CMP   EDX, 0
    JNA   @RoutineEnd

    PUSH  ESI
    PUSH  EDI

@MainLoop:

    MOV   ESI, dword ptr [EAX]
    MOV   EDI, dword ptr [EAX + 4]
    BSWAP ESI
    BSWAP EDI
    MOV   dword ptr [EAX], EDI
    MOV   dword ptr [EAX + 4], ESI

    ADD   EAX, ECX
    DEC   EDX
    JNZ   @MainLoop

    POP   EDI
    POP   ESI

{$ENDIF}

@RoutineEnd:
end;
{$ELSE}
var
  BuffPtr:  PUInt64;
  i:        TMemSize;
begin
If Count > 0 then
  begin
    BuffPtr := @Arr;
    For i := 0 to Pred(Count) do
      begin
        EndianSwapValue(BuffPtr^);
        Inc(PByte(BuffPtr),Stride);
      end;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure EndianSwapItems(var Arr; ItemSize,Count,Stride: TMemSize);
var
  BuffPtr:  Pointer;
  i:        TMemSize;
begin
If (ItemSize > 1) and (Count > 0) then
  begin
    case ItemSize of
      2:  EndianSwapItems16(Arr,Count,Stride);
      4:  EndianSwapItems32(Arr,Count,Stride);
      8:  EndianSwapItems64(Arr,Count,Stride);
    else
      BuffPtr := @Arr;
      For i := 0 to Pred(Count) do
        begin
          EndianSwap(BuffPtr^,ItemSize);
          // PtrAdvanceVar cannot be inlined here, because it is not yet
          // implemented at this point of compilation
          BuffPtr := PtrAdvance(BuffPtr,Stride);
        end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure EndianSwapItems16(var Arr; Count: TMemSize);
begin
EndianSwapItems16(Arr,Count,2);
end;

//------------------------------------------------------------------------------

procedure EndianSwapItems32(var Arr; Count: TMemSize);
begin
EndianSwapItems32(Arr,Count,4);
end;

//------------------------------------------------------------------------------

procedure EndianSwapItems64(var Arr; Count: TMemSize);
begin
EndianSwapItems64(Arr,Count,8);
end;

//------------------------------------------------------------------------------

procedure EndianSwapItems(var Arr; ItemSize,Count: TMemSize);
begin
EndianSwapItems(Arr,ItemSize,Count,ItemSize);
end;

//==============================================================================

procedure SwapEndianItems16(var Arr; Count,Stride: TMemSize);
begin
EndianSwapItems16(Arr,Count,Stride);
end;

//------------------------------------------------------------------------------

procedure SwapEndianItems32(var Arr; Count,Stride: TMemSize);
begin
EndianSwapItems32(Arr,Count,Stride);
end;

//------------------------------------------------------------------------------

procedure SwapEndianItems64(var Arr; Count,Stride: TMemSize);
begin
EndianSwapItems64(Arr,Count,Stride);
end;

//------------------------------------------------------------------------------

procedure SwapEndianItems(var Arr; ItemSize,Count,Stride: TMemSize);
begin
EndianSwapItems(Arr,ItemSize,Count,Stride);
end;

//------------------------------------------------------------------------------

procedure SwapEndianItems16(var Arr; Count: TMemSize);
begin
EndianSwapItems16(Arr,Count,2);
end;

//------------------------------------------------------------------------------

procedure SwapEndianItems32(var Arr; Count: TMemSize);
begin
EndianSwapItems32(Arr,Count,4);
end;

//------------------------------------------------------------------------------

procedure SwapEndianItems64(var Arr; Count: TMemSize);
begin
EndianSwapItems64(Arr,Count,8);
end;

//------------------------------------------------------------------------------

procedure SwapEndianItems(var Arr; ItemSize,Count: TMemSize);
begin
EndianSwapItems(Arr,ItemSize,Count,ItemSize);
end;

{-------------------------------------------------------------------------------
================================================================================
                                  Bit test (BT)
================================================================================
-------------------------------------------------------------------------------}

Function BT(Value: UInt8; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   DX, 7
    BT    CX, DX
  {$ELSE}
    AND   SI, 7
    BT    DI, SI
  {$ENDIF}
{$ELSE}
    AND   DX, 7
    BT    AX, DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr (Bit and 7)) and 1) <> 0;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BT(Value: UInt16; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BT    CX, DX
  {$ELSE}
    BT    DI, SI
  {$ENDIF}
{$ELSE}
    BT    AX, DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr (Bit and 15)) and 1) <> 0;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BT(Value: UInt32; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BT    ECX, EDX
  {$ELSE}
    BT    EDI, ESI
  {$ENDIF}
{$ELSE}
    BT    EAX, EDX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr (Bit and 31)) and 1) <> 0;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BT(Value: UInt64; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BT    RCX, RDX
  {$ELSE}
    BT    RDI, RSI
  {$ENDIF}
{$ELSE}
    AND   EAX, 63
    CMP   EAX, 32
    JAE   @TestHigh

    BT    dword ptr [Value], EAX
    JMP   @SetResult

  @TestHigh:
    AND   EAX, 31
    BT    dword ptr [Value + 4], EAX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Result := ((Value shr (Bit and 63)) and 1) <> 0;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                             Bit test and set (BTS)
================================================================================
-------------------------------------------------------------------------------}

Function BTS(var Value: UInt8; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND     DX, 7
    MOVZX   RAX, byte ptr [RCX]
    BTS     AX, DX
    MOV     byte ptr [RCX], AL
  {$ELSE}
    AND     SI, 7
    MOVZX   RAX, byte ptr [RDI]
    BTS     AX, SI
    MOV     byte ptr [RDI], AL
  {$ENDIF}
{$ELSE}
    AND     DX, 7
    MOVZX   ECX, byte ptr [EAX]
    BTS     CX, DX
    MOV     byte ptr [EAX], CL
{$ENDIF}
    SETC    AL
end;
{$ELSE}
begin
Bit := Bit and 7;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt8(Value or (UInt8(1) shl Bit));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BTS(var Value: UInt16; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   DX, 15
    BTS   word ptr [RCX], DX
  {$ELSE}
    AND   SI, 15
    BTS   word ptr [RDI], SI
  {$ENDIF}
{$ELSE}
    AND   DX, 15
    BTS   word ptr [EAX], DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Bit := Bit and 15;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt16(Value or (UInt16(1) shl Bit));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BTS(var Value: UInt32; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   EDX, 31
    BTS   dword ptr [RCX], EDX
  {$ELSE}
    AND   ESI, 31
    BTS   dword ptr [RDI], ESI
  {$ENDIF}
{$ELSE}
    AND   EDX, 31
    BTS   dword ptr [EAX], EDX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Bit := Bit and 31;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt32(Value or (UInt32(1) shl Bit));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BTS(var Value: UInt64; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RDX, 63
    BTS   qword ptr [RCX], RDX
  {$ELSE}
    AND   RSI, 63
    BTS   qword ptr [RDI], RSI
  {$ENDIF}
{$ELSE}
    AND   EDX, 63
    CMP   EDX, 32
    JAE   @TestHigh

    BTS   dword ptr [Value], EDX
    JMP   @SetResult

  @TestHigh:
    AND   EDX, 31
    BTS   dword ptr [EAX + 4], EDX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Bit := Bit and 63;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt64(Value or (UInt64(1) shl Bit));
end;
{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                            Bit test and reset (BTR)
================================================================================
-------------------------------------------------------------------------------}

Function BTR(var Value: UInt8; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND     DX, 7
    MOVZX   RAX, byte ptr [RCX]
    BTR     AX, DX
    MOV     byte ptr [RCX], AL
  {$ELSE}
    AND     SI, 7
    MOVZX   RAX, byte ptr [RDI]
    BTR     AX, SI
    MOV     byte ptr [RDI], AL
  {$ENDIF}
{$ELSE}
    AND     DX, 7
    MOVZX   ECX, byte ptr [EAX]
    BTR     CX, DX
    MOV     byte ptr [EAX], CL
{$ENDIF}
    SETC    AL
end;
{$ELSE}
begin
Bit := Bit and 7;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt8(Value and not(UInt8(1) shl Bit));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BTR(var Value: UInt16; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   DX, 15
    BTR   word ptr [RCX], DX
  {$ELSE}
    AND   SI, 15
    BTR   word ptr [RDI], SI
  {$ENDIF}
{$ELSE}
    AND   DX, 15
    BTR   word ptr [EAX], DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Bit := Bit and 15;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt16(Value and not(UInt16(1) shl Bit));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BTR(var Value: UInt32; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   EDX, 31
    BTR   dword ptr [RCX], EDX
  {$ELSE}
    AND   ESI, 31
    BTR   dword ptr [RDI], ESI
  {$ENDIF}
{$ELSE}
    AND   EDX, 31
    BTR   dword ptr [EAX], EDX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Bit := Bit and 31;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt32(Value and not(UInt32(1) shl Bit));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BTR(var Value: UInt64; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RDX, 63
    BTR   qword ptr [RCX], RDX
  {$ELSE}
    AND   RSI, 63
    BTR   qword ptr [RDI], RSI
  {$ENDIF}
{$ELSE}
    AND   EDX, 63
    CMP   EDX, 32
    JAE   @TestHigh

    BTR   dword ptr [Value], EDX
    JMP   @SetResult

  @TestHigh:
    AND   EDX, 31
    BTR   dword ptr [EAX + 4], EDX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Bit := Bit and 63;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt64(Value and not(UInt64(1) shl Bit));
end;
{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                          Bit test and complement (BTC)
================================================================================
-------------------------------------------------------------------------------}

Function BTC(var Value: UInt8; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND     DX, 7
    MOVZX   RAX, byte ptr [RCX]
    BTC     AX, DX
    MOV     byte ptr [RCX], AL
  {$ELSE}
    AND     SI, 7
    MOVZX   RAX, byte ptr [RDI]
    BTC     AX, SI
    MOV     byte ptr [RDI], AL
  {$ENDIF}
{$ELSE}
    AND     DX, 7
    MOVZX   ECX, byte ptr [EAX]
    BTC     CX, DX
    MOV     byte ptr [EAX], CL
{$ENDIF}
    SETC    AL
end;
{$ELSE}
begin
Bit := Bit and 7;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt8(Value xor (UInt8(1) shl Bit));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BTC(var Value: UInt16; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   DX, 15
    BTC   word ptr [RCX], DX
  {$ELSE}
    AND   SI, 15
    BTC   word ptr [RDI], SI
  {$ENDIF}
{$ELSE}
    AND   DX, 15
    BTC   word ptr [EAX], DX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Bit := Bit and 15;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt16(Value xor (UInt16(1) shl Bit));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BTC(var Value: UInt32; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   EDX, 31
    BTC   dword ptr [RCX], EDX
  {$ELSE}
    AND   ESI, 31
    BTC   dword ptr [RDI], ESI
  {$ENDIF}
{$ELSE}
    AND   EDX, 31
    BTC   dword ptr [EAX], EDX
{$ENDIF}
    SETC  AL
end;
{$ELSE}
begin
Bit := Bit and 31;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt32(Value xor (UInt32(1) shl Bit));
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BTC(var Value: UInt64; Bit: Integer): Boolean;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RDX, 63
    BTC   qword ptr [RCX], RDX
  {$ELSE}
    AND   RSI, 63
    BTC   qword ptr [RDI], RSI
  {$ENDIF}
{$ELSE}
    AND   EDX, 63
    CMP   EDX, 32
    JAE   @TestHigh

    BTC   dword ptr [Value], EDX
    JMP   @SetResult

  @TestHigh:
    AND   EDX, 31
    BTC   dword ptr [EAX + 4], EDX
{$ENDIF}
  @SetResult:
    SETC  AL
end;
{$ELSE}
begin
Bit := Bit and 63;
Result := ((Value shr Bit) and 1) <> 0;
Value := UInt64(Value xor (UInt64(1) shl Bit));
end;
{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                       Bit test and set to a given value
================================================================================
-------------------------------------------------------------------------------}

Function BitSetTo(var Value: UInt8; Bit: Integer; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BitSetTo(var Value: UInt16; Bit: Integer; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BitSetTo(var Value: UInt32; Bit: Integer; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BitSetTo(var Value: UInt64; Bit: Integer; NewValue: Boolean): Boolean;
begin
If NewValue then Result := BTS(Value,Bit)
  else Result := BTR(Value,Bit);
end;

{-------------------------------------------------------------------------------
================================================================================
                             Bit scan forward (BSF)
================================================================================
-------------------------------------------------------------------------------}

Function BSF(Value: UInt8): Integer;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
    XOR   RAX, RAX
  {$IFDEF Windows}
    AND   RCX, $FF
    BSF   AX, CX
  {$ELSE}
    AND   RDI, $FF
    BSF   AX, DI
  {$ENDIF}
    JNZ   @RoutineEnd
    MOV   RAX, -1
  @RoutineEnd:
{$ELSE}
    AND   EAX, $FF
    BSF   AX, AX
    JNZ   @RoutineEnd
    MOV   EAX, -1
  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 7 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BSF(Value: UInt16): Integer;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
    XOR   RAX, RAX
  {$IFDEF Windows}
    BSF   AX, CX
  {$ELSE}
    BSF   AX, DI
  {$ENDIF}
    JNZ   @RoutineEnd
    MOV   RAX, -1
  @RoutineEnd:
{$ELSE}
    AND   EAX, $FFFF
    BSF   AX, AX
    JNZ   @RoutineEnd
    MOV   EAX, -1
  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 15 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BSF(Value: UInt32): Integer;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
    XOR   RAX, RAX
  {$IFDEF Windows}
    BSF   EAX, ECX
  {$ELSE}
    BSF   EAX, EDI
  {$ENDIF}
    JNZ   @RoutineEnd
    MOV   RAX, -1
  @RoutineEnd:
{$ELSE}
    BSF   EAX, EAX
    JNZ   @RoutineEnd
    MOV   EAX, -1
  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 31 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BSF(Value: UInt64): Integer;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BSF   RAX, RCX
  {$ELSE}
    BSF   RAX, RDI
  {$ENDIF}
    JNZ   @RoutineEnd
    MOV   EAX, -1
  @RoutineEnd:
{$ELSE}
    BSF   EAX, dword ptr [Value]
    JNZ   @RoutineEnd

    BSF   EAX, dword ptr [Value + 4]
    JNZ   @Add32

    MOV   EAX, -33

  @Add32:
    ADD   EAX, 32

  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 0 to 63 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                             Bit scan reversed (BSR)
================================================================================
-------------------------------------------------------------------------------}

Function BSR(Value: UInt8): Integer;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
    XOR   RAX, RAX
  {$IFDEF Windows}
    AND   RCX, $FF
    BSR   AX, CX
  {$ELSE}
    AND   RDI, $FF
    BSR   AX, DI
  {$ENDIF}
    JNZ   @RoutineEnd
    MOV   RAX, -1
  @RoutineEnd:
{$ELSE}
    AND   EAX, $FF
    BSR   AX,  AX
    JNZ   @RoutineEnd
    MOV   EAX, -1
  @RoutineEnd:
{$ENDIF}

end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 7 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BSR(Value: UInt16): Integer;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
    XOR   RAX, RAX
  {$IFDEF Windows}
    BSR   AX, CX
  {$ELSE}
    BSR   AX, DI
  {$ENDIF}
    JNZ   @RoutineEnd
    MOV   RAX, -1
  @RoutineEnd:
{$ELSE}
    AND   EAX, $FFFF
    BSR   AX, AX
    JNZ   @RoutineEnd
    MOV   EAX, -1
  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 15 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BSR(Value: UInt32): Integer;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
    XOR   RAX, RAX
  {$IFDEF Windows}
    BSR   EAX, ECX
  {$ELSE}
    BSR   EAX, EDI
  {$ENDIF}
    JNZ   @RoutineEnd
    MOV   RAX,  -1
  @RoutineEnd:
{$ELSE}
    BSR   EAX, EAX
    JNZ   @RoutineEnd
    MOV   EAX, -1
  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 31 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BSR(Value: UInt64): Integer;
{$IFNDEF PurePascal}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    BSR   RAX, RCX
  {$ELSE}
    BSR   RAX, RDI
  {$ENDIF}
    JNZ   @RoutineEnd
    MOV   RAX, -1
  @RoutineEnd:
{$ELSE}
    BSR   EAX, dword ptr [Value + 4]
    JZ    @ScanLow

    ADD   EAX, 32
    JMP   @RoutineEnd

  @ScanLow:
    BSR   EAX, dword ptr [Value]

    JNZ   @RoutineEnd
    MOV   EAX, -1

  @RoutineEnd:
{$ENDIF}
end;
{$ELSE}
var
  i:  Integer;
begin
Result := -1;
For i := 63 downto 0 do
  If (Value shr i) and 1 <> 0 then
    begin
      Result := i;
      Break;
    end;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                Population count
================================================================================
-------------------------------------------------------------------------------}

{$IFDEF UseLookupTable}
const
  PopCountTable: array[UInt8] of UInt8 = (
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8);
{$ENDIF}

//------------------------------------------------------------------------------

Function Fce_PopCount_8_Pas(Value: UInt8): Integer; register;
{$IFDEF UseLookupTable}
begin
Result := PopCountTable[Value];
end;
{$ELSE}
begin
Value := (Value and UInt8($55)) + ((Value shr 1) and UInt8($55));
Value := (Value and UInt8($33)) + ((Value shr 2) and UInt8($33));
Value := (Value and UInt8($0F)) + ((Value shr 4) and UInt8($0F));
Result := Integer(Value);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_PopCount_16_Pas(Value: UInt16): Integer; register;
{$IFDEF UseLookupTable}
begin
Result := PopCountTable[UInt8(Value)] + PopCountTable[UInt8(Value shr 8)];
end;
{$ELSE}
begin
Value := (Value and UInt16($5555)) + ((Value shr 1) and UInt16($5555));
Value := (Value and UInt16($3333)) + ((Value shr 2) and UInt16($3333));
Value := (Value and UInt16($0F0F)) + ((Value shr 4) and UInt16($0F0F));
Value := (Value and UInt16($00FF)) + ((Value shr 8) and UInt16($00FF));
Result := Integer(Value);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_PopCount_32_Pas(Value: UInt32): Integer; register;
{$IFDEF UseLookupTable}
begin
Result := PopCountTable[UInt8(Value)] + PopCountTable[UInt8(Value shr 8)] +
  PopCountTable[UInt8(Value shr 16)] + PopCountTable[UInt8(Value shr 24)];
end;
{$ELSE}
begin
Value := (Value and UInt32($55555555)) + ((Value shr 1) and UInt32($55555555));
Value := (Value and UInt32($33333333)) + ((Value shr 2) and UInt32($33333333));
Value := (Value and UInt32($0F0F0F0F)) + ((Value shr 4) and UInt32($0F0F0F0F));
Value := (Value and UInt32($00FF00FF)) + ((Value shr 8) and UInt32($00FF00FF));
Value := (Value and UInt32($0000FFFF)) + ((Value shr 16) and UInt32($0000FFFF));
Result := Integer(Value);
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_PopCount_64_Pas(Value: UInt64): Integer; register;
{$IFDEF UseLookupTable}
begin
{$IFDEF CPU64bit}
Result := PopCountTable[UInt8(Value)] + PopCountTable[UInt8(Value shr 8)] +
  PopCountTable[UInt8(Value shr 16)] + PopCountTable[UInt8(Value shr 24)] +
  PopCountTable[UInt8(Value shr 32)] + PopCountTable[UInt8(Value shr 40)] +
  PopCountTable[UInt8(Value shr 48)] + PopCountTable[UInt8(Value shr 56)];
{$ELSE}
Result := Fce_PopCount_32_Pas(Int64Rec(Value).Lo) + Fce_PopCount_32_Pas(Int64Rec(Value).Hi);
{$ENDIF}
end;
{$ELSE}
begin
Value := (Value and UInt64($5555555555555555)) + ((Value shr 1) and UInt64($5555555555555555));
Value := (Value and UInt64($3333333333333333)) + ((Value shr 2) and UInt64($3333333333333333));
Value := (Value and UInt64($0F0F0F0F0F0F0F0F)) + ((Value shr 4) and UInt64($0F0F0F0F0F0F0F0F));
Value := (Value and UInt64($00FF00FF00FF00FF)) + ((Value shr 8) and UInt64($00FF00FF00FF00FF));
Value := (Value and UInt64($0000FFFF0000FFFF)) + ((Value shr 16) and UInt64($0000FFFF0000FFFF));
Value := (Value and UInt64($00000000FFFFFFFF)) + ((Value shr 32) and UInt64($00000000FFFFFFFF));
Result := Integer(Value);
end;
{$ENDIF}

//==============================================================================

{$IFDEF ASM_Extensions}

Function Fce_PopCount_8_Asm(Value: UInt8): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOVZX   RAX, CL
  {$ELSE}
    MOVZX   RAX, DIL
  {$ENDIF}
{$ELSE}
    AND     EAX, $FF
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $66, $F3, $0F, $B8, $C0   // POPCNT  AX, AX
{$ELSE}
    POPCNT  AX, AX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_PopCount_16_Asm(Value: UInt16): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOVZX   RAX, CX
  {$ELSE}
    MOVZX   RAX, DI
  {$ENDIF}
{$ELSE}
    AND     EAX, $FFFF
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $66, $F3, $0F, $B8, $C0   // POPCNT  AX, AX
{$ELSE}
    POPCNT  AX, AX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_PopCount_32_Asm(Value: UInt32): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     EAX, ECX
  {$ELSE}
    MOV     EAX, EDI
  {$ENDIF}
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $F3, $0F, $B8, $C0  // POPCNT  EAX, EAX
{$ELSE}
    POPCNT  EAX, EAX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_PopCount_64_Asm(Value: UInt64): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     RAX, RCX
  {$ELSE}
    MOV     RAX, RDI
  {$ENDIF}
  {$IFDEF ASM_MachineCode}
    DB  $F3, $48, $0F, $B8, $C0   // POPCNT  RAX, RAX
  {$ELSE}
    POPCNT  RAX, RAX
  {$ENDIF}
{$ELSE}
    MOV     EAX, dword ptr [Value]
    MOV     EDX, dword ptr [Value + 4]
  {$IFDEF ASM_MachineCode}
    DB  $F3, $0F, $B8, $C0        // POPCNT  EAX, EAX
    DB  $F3, $0F, $B8, $D2        // POPCNT  EDX, EDX
  {$ELSE}
    POPCNT  EAX, EAX
    POPCNT  EDX, EDX
  {$ENDIF}
    ADD     EAX, EDX
{$ENDIF}
end;

{$ENDIF}

//==============================================================================

var
  Var_PopCount_8: Function(Value: UInt8): Integer; register = Fce_PopCount_8_Pas;
  Var_PopCount_16: Function(Value: UInt16): Integer; register = Fce_PopCount_16_Pas;
  Var_PopCount_32: Function(Value: UInt32): Integer; register = Fce_PopCount_32_Pas;
  Var_PopCount_64: Function(Value: UInt64): Integer; register = Fce_PopCount_64_Pas;

//------------------------------------------------------------------------------

Function PopCount(Value: UInt8): Integer;
begin
Result := Var_PopCount_8(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PopCount(Value: UInt16): Integer;
begin
Result := Var_PopCount_16(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PopCount(Value: UInt32): Integer;
begin
Result := Var_PopCount_32(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PopCount(Value: UInt64): Integer;
begin
Result := Var_PopCount_64(Value);
end;

{-------------------------------------------------------------------------------
================================================================================
                               Nibble manipulation
================================================================================
-------------------------------------------------------------------------------}

Function GetHighNibble(Value: UInt8): TNibble;
begin
Result := (Value shr 4) and $0F;
end;

//------------------------------------------------------------------------------

Function GetLowNibble(Value: UInt8): TNibble;
begin
Result := Value and $0F;
end;

//------------------------------------------------------------------------------

Function SetHighNibble(Value: UInt8; SetTo: TNibble): UInt8;
begin
Result := (Value and $0F) or UInt8((SetTo and $0F) shl 4);
end;

//------------------------------------------------------------------------------

Function SetLowNibble(Value: UInt8; SetTo: TNibble): UInt8;
begin
Result := (Value and $F0) or UInt8(SetTo and $0F);
end;

//------------------------------------------------------------------------------

procedure SetHighNibbleValue(var Value: UInt8; SetTo: TNibble);
begin
Value := SetHighNibble(Value,SetTo);
end;

//------------------------------------------------------------------------------

procedure SetLowNibbleValue(var Value: UInt8; SetTo: TNibble);
begin
Value := SetLowNibble(Value,SetTo);
end;

{-------------------------------------------------------------------------------
================================================================================
                                 Get flag state
================================================================================
-------------------------------------------------------------------------------}

Function GetFlagState(Value,FlagBitmask: UInt8; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetFlagState(Value,FlagBitmask: UInt16; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetFlagState(Value,FlagBitmask: UInt32; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetFlagState(Value,FlagBitmask: UInt64; ExactMatch: Boolean = False): Boolean;
begin
If ExactMatch then
  Result := (Value and FlagBitmask) = FlagBitmask
else
  Result := (Value and FlagBitmask) <> 0;
end;

{-------------------------------------------------------------------------------
================================================================================
                                    Set flag
================================================================================
-------------------------------------------------------------------------------}

Function SetFlag(Value,FlagBitmask: UInt8): UInt8;
begin
Result := Value or FlagBitmask;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlag(Value,FlagBitmask: UInt16): UInt16;
begin
Result := Value or FlagBitmask;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlag(Value,FlagBitmask: UInt32): UInt32;
begin
Result := Value or FlagBitmask;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlag(Value,FlagBitmask: UInt64): UInt64;
begin
Result := Value or FlagBitmask;
end;

//==============================================================================

procedure SetFlagValue(var Value: UInt8; FlagBitmask: UInt8);
begin
Value := SetFlag(Value,FlagBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagValue(var Value: UInt16; FlagBitmask: UInt16);
begin
Value := SetFlag(Value,FlagBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagValue(var Value: UInt32; FlagBitmask: UInt32);
begin
Value := SetFlag(Value,FlagBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagValue(var Value: UInt64; FlagBitmask: UInt64);
begin
Value := SetFlag(Value,FlagBitmask);
end;

//==============================================================================

Function SetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
var
  TempBitmask:  UInt8;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
var
  TempBitmask:  UInt16;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
var
  TempBitmask:  UInt32;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;
var
  TempBitmask:  UInt64;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := SetFlag(Value,TempBitmask);
end;

//==============================================================================

Function SetFlags(Value: UInt8; Flags: array of UInt8): UInt8;
begin
Result := SetFlags_8(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlags(Value: UInt16; Flags: array of UInt16): UInt16;
begin
Result := SetFlags_16(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlags(Value: UInt32; Flags: array of UInt32): UInt32;
begin
Result := SetFlags_32(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlags(Value: UInt64; Flags: array of UInt64): UInt64;
begin
Result := SetFlags_64(Value,Flags);
end;

//==============================================================================

procedure SetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
begin
Value := SetFlags_8(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
begin
Value := SetFlags_16(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
begin
Value := SetFlags_32(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);
begin
Value := SetFlags_64(Value,Flags);
end;

//==============================================================================

procedure SetFlagsValue(var Value: UInt8; Flags: array of UInt8);
begin
SetFlagsValue_8(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagsValue(var Value: UInt16; Flags: array of UInt16);
begin
SetFlagsValue_16(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagsValue(var Value: UInt32; Flags: array of UInt32);
begin
SetFlagsValue_32(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagsValue(var Value: UInt64; Flags: array of UInt64);
begin
SetFlagsValue_64(Value,Flags);
end;

{-------------------------------------------------------------------------------
================================================================================
                                   Reset flag
================================================================================
-------------------------------------------------------------------------------}

Function ResetFlag(Value,FlagBitmask: UInt8): UInt8;
begin
Result := Value and not FlagBitmask;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResetFlag(Value,FlagBitmask: UInt16): UInt16;
begin
Result := Value and not FlagBitmask;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResetFlag(Value,FlagBitmask: UInt32): UInt32;
begin
Result := Value and not FlagBitmask;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResetFlag(Value,FlagBitmask: UInt64): UInt64;
begin
Result := Value and not FlagBitmask;
end;

//==============================================================================

procedure ResetFlagValue(var Value: UInt8; FlagBitmask: UInt8);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ResetFlagValue(var Value: UInt16; FlagBitmask: UInt16);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ResetFlagValue(var Value: UInt32; FlagBitmask: UInt32);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ResetFlagValue(var Value: UInt64; FlagBitmask: UInt64);
begin
Value := ResetFlag(Value,FlagBitmask);
end;

//==============================================================================

Function ResetFlags_8(Value: UInt8; Flags: array of UInt8): UInt8;
var
  TempBitmask:  UInt8;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResetFlags_16(Value: UInt16; Flags: array of UInt16): UInt16;
var
  TempBitmask:  UInt16;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResetFlags_32(Value: UInt32; Flags: array of UInt32): UInt32;
var
  TempBitmask:  UInt32;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResetFlags_64(Value: UInt64; Flags: array of UInt64): UInt64;
var
  TempBitmask:  UInt64;
  i:            Integer;
begin
TempBitmask := 0;
For i := Low(Flags) to High(flags) do
  TempBitmask := TempBitmask or Flags[i];
Result := ResetFlag(Value,TempBitmask);
end;

//==============================================================================

Function ResetFlags(Value: UInt8; Flags: array of UInt8): UInt8;
begin
Result := ResetFlags_8(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResetFlags(Value: UInt16; Flags: array of UInt16): UInt16;
begin
Result := ResetFlags_16(Value,Flags);
end;
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResetFlags(Value: UInt32; Flags: array of UInt32): UInt32;
begin
Result := ResetFlags_32(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResetFlags(Value: UInt64; Flags: array of UInt64): UInt64;
begin
Result := ResetFlags_64(Value,Flags);
end;

//==============================================================================

procedure ResetFlagsValue_8(var Value: UInt8; Flags: array of UInt8);
begin
Value := ResetFlags_8(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ResetFlagsValue_16(var Value: UInt16; Flags: array of UInt16);
begin
Value := ResetFlags_16(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ResetFlagsValue_32(var Value: UInt32; Flags: array of UInt32);
begin
Value := ResetFlags_32(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ResetFlagsValue_64(var Value: UInt64; Flags: array of UInt64);
begin
Value := ResetFlags_64(Value,Flags);
end;

//==============================================================================

procedure ResetFlagsValue(var Value: UInt8; Flags: array of UInt8);
begin
ResetFlagsValue_8(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ResetFlagsValue(var Value: UInt16; Flags: array of UInt16);
begin
ResetFlagsValue_16(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ResetFlagsValue(var Value: UInt32; Flags: array of UInt32);
begin
ResetFlagsValue_32(Value,Flags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ResetFlagsValue(var Value: UInt64; Flags: array of UInt64);
begin
ResetFlagsValue_64(Value,Flags);
end;

{-------------------------------------------------------------------------------
================================================================================
                                 Set flag state
================================================================================
-------------------------------------------------------------------------------}

Function SetFlagState(Value,FlagBitmask: UInt8; NewState: Boolean): UInt8;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlagState(Value,FlagBitmask: UInt16; NewState: Boolean): UInt16;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlagState(Value,FlagBitmask: UInt32; NewState: Boolean): UInt32;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetFlagState(Value,FlagBitmask: UInt64; NewState: Boolean): UInt64;
begin
If NewState then
  Result := SetFlag(Value,FlagBitmask)
else
  Result := ResetFlag(Value,FlagBitmask);
end;

//==============================================================================

procedure SetFlagStateValue(var Value: UInt8; FlagBitmask: UInt8; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagStateValue(var Value: UInt16; FlagBitmask: UInt16; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagStateValue(var Value: UInt32; FlagBitmask: UInt32; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetFlagStateValue(var Value: UInt64; FlagBitmask: UInt64; NewState: Boolean);
begin
Value := SetFlagState(Value,FlagBitmask,NewState);
end;

{-------------------------------------------------------------------------------
================================================================================
                                    Get bits
================================================================================
-------------------------------------------------------------------------------}

Function GetBits(Value: UInt8; FromBit,ToBit: Integer; ShiftDown: Boolean = True): UInt8;
begin
Result := Value and UInt8(($FF shl (FromBit and 7)) and ($FF shr (7 - (ToBit and 7))));
If ShiftDown then
  Result := Result shr (FromBit and 7);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetBits(Value: UInt16; FromBit,ToBit: Integer; ShiftDown: Boolean = True): UInt16;
begin
Result := Value and UInt16(($FFFF shl (FromBit and 15)) and ($FFFF shr (15 - (ToBit and 15))));
If ShiftDown then
  Result := Result shr (FromBit and 15);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetBits(Value: UInt32; FromBit,ToBit: Integer; ShiftDown: Boolean = True): UInt32;
begin
Result := Value and UInt32(($FFFFFFFF shl (FromBit and 31)) and ($FFFFFFFF shr (31 - (ToBit and 31))));
If ShiftDown then
  Result := Result shr (FromBit and 31);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetBits(Value: UInt64; FromBit,ToBit: Integer; ShiftDown: Boolean = True): UInt64;
begin
Result := Value and UInt64((UInt64($FFFFFFFFFFFFFFFF) shl (FromBit and 63)) and (UInt64($FFFFFFFFFFFFFFFF) shr (63 - (ToBit and 63))));
If ShiftDown then
  Result := Result shr (FromBit and 63);
end;

{-------------------------------------------------------------------------------
================================================================================
                                    Set bits
================================================================================
-------------------------------------------------------------------------------}

Function SetBits(Value,NewBits: UInt8; FromBit,ToBit: Integer; ShiftUp: Boolean = True): UInt8;
var
  Mask: UInt8;
begin
If ShiftUp then
  NewBits := UInt8(NewBits shl (FromBit and 7));
Mask := UInt8(($FF shl (FromBit and 7)) and ($FF shr (7 - (ToBit and 7))));
Result := (Value and not Mask) or (NewBits and Mask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetBits(Value,NewBits: UInt16; FromBit,ToBit: Integer; ShiftUp: Boolean = True): UInt16;
var
  Mask: UInt16;
begin
If ShiftUp then
  NewBits := UInt16(NewBits shl (FromBit and 15));
Mask := UInt16(($FFFF shl (FromBit and 15)) and ($FFFF shr (15 - (ToBit and 15))));
Result := (Value and not Mask) or (NewBits and Mask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetBits(Value,NewBits: UInt32; FromBit,ToBit: Integer; ShiftUp: Boolean = True): UInt32;
var
  Mask: UInt32;
begin
If ShiftUp then
  NewBits := UInt32(NewBits shl (FromBit and 31));
Mask := UInt32(($FFFFFFFF shl (FromBit and 31)) and ($FFFFFFFF shr (31 - (ToBit and 31))));
Result := (Value and not Mask) or (NewBits and Mask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SetBits(Value,NewBits: UInt64; FromBit,ToBit: Integer; ShiftUp: Boolean = True): UInt64;
var
  Mask: UInt64;
begin
If ShiftUp then
  NewBits := UInt64(NewBits shl (FromBit and 63));
Mask := UInt64((UInt64($FFFFFFFFFFFFFFFF) shl (FromBit and 63)) and (UInt64($FFFFFFFFFFFFFFFF) shr (63 - (ToBit and 63))));
Result := (Value and not Mask) or (NewBits and Mask);
end;

//==============================================================================

procedure SetBitsValue(var Value: UInt8; NewBits: UInt8; FromBit,ToBit: Integer; ShiftUp: Boolean = True);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit,ShiftUp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetBitsValue(var Value: UInt16; NewBits: UInt16; FromBit,ToBit: Integer; ShiftUp: Boolean = True);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit,ShiftUp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetBitsValue(var Value: UInt32; NewBits: UInt32; FromBit,ToBit: Integer; ShiftUp: Boolean = True);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit,ShiftUp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SetBitsValue(var Value: UInt64; NewBits: UInt64; FromBit,ToBit: Integer; ShiftUp: Boolean = True);
begin
Value := SetBits(Value,NewBits,FromBit,ToBit,ShiftUp);
end;

{-------------------------------------------------------------------------------
================================================================================
                                  Reverse bits
================================================================================
-------------------------------------------------------------------------------}

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

//------------------------------------------------------------------------------

Function ReverseBits(Value: UInt8): UInt8;
begin
Result := UInt8(RevBitsTable[UInt8(Value)]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ReverseBits(Value: UInt16): UInt16;
begin
Result := UInt16((UInt16(RevBitsTable[UInt8(Value)]) shl 8) or
                  UInt16(RevBitsTable[UInt8(Value shr 8)]));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ReverseBits(Value: UInt32): UInt32;
begin
Result := UInt32((UInt32(RevBitsTable[UInt8(Value)]) shl 24) or
                 (UInt32(RevBitsTable[UInt8(Value shr 8)]) shl 16) or
                 (UInt32(RevBitsTable[UInt8(Value shr 16)]) shl 8) or
                  UInt32(RevBitsTable[UInt8(Value shr 24)]));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ReverseBits(Value: UInt64): UInt64;
begin
Int64Rec(Result).Hi := ReverseBits(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := ReverseBits(Int64Rec(Value).Hi);
end;

//==============================================================================

procedure ReverseBitsValue(var Value: UInt8);
begin
Value := ReverseBits(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ReverseBitsValue(var Value: UInt16);
begin
Value := ReverseBits(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ReverseBitsValue(var Value: UInt32);
begin
Value := ReverseBits(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure ReverseBitsValue(var Value: UInt64);
begin
Value := ReverseBits(Value);
end;

{-------------------------------------------------------------------------------
================================================================================
                               Leading zero count
================================================================================
-------------------------------------------------------------------------------}

Function Fce_LZCount_8_Pas(Value: UInt8): Integer; register;
var
  i:  Integer;
begin
Result := 8;
For i := 0 to 7 do
  If (Value and (UInt8($80) shr i)) <> 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_LZCount_16_Pas(Value: UInt16): Integer; register;
var
  i:  Integer;
begin
Result := 16;
For i := 0 to 15 do
  If (Value and (UInt16($8000) shr i)) <> 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_LZCount_32_Pas(Value: UInt32): Integer; register;
var
  i:  Integer;
begin
Result := 32;
For i := 0 to 31 do
  If (Value and (UInt32($80000000) shr i)) <> 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_LZCount_64_Pas(Value: UInt64): Integer; register;
var
  i:  Integer;
begin
Result := 64;
For i := 0 to 63 do
  If (Value and (UInt64($8000000000000000) shr i)) <> 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

//==============================================================================

{$IFDEF ASM_Extensions}

Function Fce_LZCount_8_Asm(Value: UInt8): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOVZX   RAX, CL
  {$ELSE}
    MOVZX   RAX, DIL
  {$ENDIF}
{$ELSE}
    AND     EAX, $FF
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $66, $F3, $0F, $BD, $C0   // LZCNT   AX,  AX
{$ELSE}
    LZCNT   AX, AX
    SUB     AX, 8
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_LZCount_16_Asm(Value: UInt16): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOVZX   RAX, CX
  {$ELSE}
    MOVZX   RAX, DI
  {$ENDIF}
{$ELSE}
    AND     EAX, $FFFF
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $66, $F3, $0F, $BD, $C0   // LZCNT   AX,  AX
{$ELSE}
    LZCNT   AX, AX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_LZCount_32_Asm(Value: UInt32): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     EAX, ECX
  {$ELSE}
    MOV     EAX, EDI
  {$ENDIF}
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB   $F3, $0F, $BD, $C0       // LZCNT  EAX, EAX
{$ELSE}
    LZCNT   EAX, EAX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_LZCount_64_Asm(Value: UInt64): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     RAX, RCX
  {$ELSE}
    MOV     RAX, RDI
  {$ENDIF}
  {$IFDEF ASM_MachineCode}
    DB  $F3, $48, $0F, $BD, $C0   // LZCNT  RAX, RAX
  {$ELSE}
    LZCNT   RAX, RAX
  {$ENDIF}
{$ELSE}
    MOV     EAX, dword ptr [Value + 4]
    TEST    EAX, EAX
    JZ      @ScanLow

  {$IFDEF ASM_MachineCode}
    DB   $F3, $0F, $BD, $C0       // LZCNT  EAX, EAX
  {$ELSE}
    LZCNT   EAX, EAX
  {$ENDIF}
    JMP     @RoutineEnd

  @ScanLow:
    MOV     EAX, dword ptr [Value]
  {$IFDEF ASM_MachineCode}
    DB   $F3, $0F, $BD, $C0       // LZCNT  EAX, EAX
  {$ELSE}
    LZCNT   EAX, EAX
  {$ENDIF}
    ADD     EAX, 32

  @RoutineEnd:
{$ENDIF}
end;

{$ENDIF}

//==============================================================================

var
  Var_LZCount_8: Function(Value: UInt8): Integer; register = Fce_LZCount_8_Pas;
  Var_LZCount_16: Function(Value: UInt16): Integer; register = Fce_LZCount_16_Pas;
  Var_LZCount_32: Function(Value: UInt32): Integer; register = Fce_LZCount_32_Pas;
  Var_LZCount_64: Function(Value: UInt64): Integer; register = Fce_LZCount_64_Pas;

//------------------------------------------------------------------------------

Function LZCount(Value: UInt8): Integer;
begin
Result := Var_LZCount_8(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LZCount(Value: UInt16): Integer;
begin
Result := Var_LZCount_16(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LZCount(Value: UInt32): Integer;
begin
Result := Var_LZCount_32(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LZCount(Value: UInt64): Integer;
begin
Result := Var_LZCount_64(Value);
end;

{-------------------------------------------------------------------------------
================================================================================
                              Trailing zero count
================================================================================
-------------------------------------------------------------------------------}

Function Fce_TZCount_8_Pas(Value: UInt8): Integer; register;
var
  i:  Integer;
begin
Result := 8;
For i := 0 to 7 do
  If ((Value shr i) and 1) <> 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_TZCount_16_Pas(Value: UInt16): Integer; register;
var
  i:  Integer;
begin
Result := 16;
For i := 0 to 15 do
  If ((Value shr i) and 1) <> 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_TZCount_32_Pas(Value: UInt32): Integer; register;
var
  i:  Integer;
begin
Result := 32;
For i := 0 to 31 do
  If ((Value shr i) and 1) <> 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_TZCount_64_Pas(Value: UInt64): Integer; register;
var
  i:  Integer;
begin
Result := 64;
For i := 0 to 63 do
  If ((Value shr i) and 1) <> 0 then
    begin
      Result := i;
      Break{For i};
    end;
end;

//==============================================================================

{$IFDEF ASM_Extensions}

Function Fce_TZCount_8_Asm(Value: UInt8): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOVZX   RAX, CL
  {$ELSE}
    MOVZX   RAX, DIL
  {$ENDIF}
{$ELSE}
    AND     EAX, $FF
{$ENDIF}
    MOV     AH, $FF
{$IFDEF ASM_MachineCode}
    DB  $66, $F3, $0F, $BC, $C0   // TZCNT   AX,  AX
{$ELSE}
    TZCNT   AX, AX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_TZCount_16_Asm(Value: UInt16): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOVZX   RAX, CX
  {$ELSE}
    MOVZX   RAX, DI
  {$ENDIF}
{$ELSE}
    AND     EAX, $FFFF
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $66, $F3, $0F, $BC, $C0   // TZCNT   AX,  AX
{$ELSE}
    TZCNT   AX, AX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_TZCount_32_Asm(Value: UInt32): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     EAX, ECX
  {$ELSE}
    MOV     EAX, EDI
  {$ENDIF}
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB   $F3, $0F, $BC, $C0       // TZCNT  EAX, EAX
{$ELSE}
    TZCNT   EAX, EAX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_TZCount_64_Asm(Value: UInt64): Integer; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     RAX, RCX
  {$ELSE}
    MOV     RAX, RDI
  {$ENDIF}
  {$IFDEF ASM_MachineCode}
    DB  $F3, $48, $0F, $BC, $C0   // TZCNT  RAX, RAX
  {$ELSE}
    TZCNT   RAX, RAX
  {$ENDIF}
{$ELSE}
    MOV     EAX, dword ptr [Value]
    TEST    EAX, EAX
    JZ      @ScanHigh

  {$IFDEF ASM_MachineCode}
    DB   $F3, $0F, $BC, $C0       // TZCNT  EAX, EAX
  {$ELSE}
    TZCNT   EAX, EAX
  {$ENDIF}
    JMP     @RoutineEnd

  @ScanHigh:
    MOV     EAX, dword ptr [Value + 4]
  {$IFDEF ASM_MachineCode}
    DB   $F3, $0F, $BC, $C0       // TZCNT  EAX, EAX
  {$ELSE}
    TZCNT   EAX, EAX
  {$ENDIF}
    ADD     EAX, 32

  @RoutineEnd:
{$ENDIF}
end;

{$ENDIF}

//==============================================================================

var
  Var_TZCount_8: Function(Value: UInt8): Integer; register = Fce_TZCount_8_Pas;
  Var_TZCount_16: Function(Value: UInt16): Integer; register = Fce_TZCount_16_Pas;
  Var_TZCount_32: Function(Value: UInt32): Integer; register = Fce_TZCount_32_Pas;
  Var_TZCount_64: Function(Value: UInt64): Integer; register = Fce_TZCount_64_Pas;

//------------------------------------------------------------------------------

Function TZCount(Value: UInt8): Integer;
begin
Result := Var_TZCount_8(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TZCount(Value: UInt16): Integer;
begin
Result := Var_TZCount_16(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TZCount(Value: UInt32): Integer;
begin
Result := Var_TZCount_32(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TZCount(Value: UInt64): Integer;
begin
Result := Var_TZCount_64(Value);
end;

{-------------------------------------------------------------------------------
================================================================================
                                  Extract bits
================================================================================
-------------------------------------------------------------------------------}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}

Function Fce_ExtractBits_8_Pas(Value: UInt8; Start,Length: Integer): UInt8; register;
begin
If UInt8(Start) <= 7 then
  begin
    If UInt8(Length) <= 7 then
      Result := UInt8(Value shr UInt8(Start)) and UInt8(Int8(UInt8(1) shl UInt8(Length)) - 1)
    else
      Result := UInt8(Value shr UInt8(Start)) and UInt8($FF);
  end
else Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ExtractBits_16_Pas(Value: UInt16; Start,Length: Integer): UInt16; register;
begin
If UInt8(Start) <= 15 then
  begin
    If UInt8(Length) <= 15 then
      Result := UInt16(Value shr UInt8(Start)) and UInt16(Int16(UInt16(1) shl UInt8(Length)) - 1)
    else
      Result := UInt16(Value shr UInt8(Start)) and UInt16($FFFF);
  end
else Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ExtractBits_32_Pas(Value: UInt32; Start,Length: Integer): UInt32; register;
begin
If UInt8(Start) <= 31 then
  begin
    If UInt8(Length) <= 31 then
      Result := UInt32(Value shr UInt8(Start)) and UInt32(Int32(UInt32(1) shl UInt8(Length)) - 1)
    else
      Result := UInt32(Value shr UInt8(Start)) and UInt32($FFFFFFFF);
  end
else Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ExtractBits_64_Pas(Value: UInt64; Start,Length: Integer): UInt64; register;
begin
If UInt8(Start) <= 63 then
  begin
    If UInt8(Length) <= 63 then
      Result := UInt64(Value shr UInt8(Start)) and UInt64(Int64(UInt64(1) shl UInt8(Length)) - 1)
    else
      Result := UInt64(Value shr UInt8(Start)) and UInt64($FFFFFFFFFFFFFFFF);
  end
else Result := 0;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//==============================================================================

{$IFDEF ASM_Extensions}

Function Fce_ExtractBits_8_Asm(Value: UInt8; Start,Length: Integer): UInt8; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOVZX   RAX, CL
    SHL     R8, 8
    AND     RDX, $FF
    OR      RDX, R8
  {$ELSE}
    MOVZX   RAX, DIL
    SHL     RDX, 8
    MOV     DL, SIL
  {$ENDIF}
{$ELSE}
    AND     EAX, $FF
    MOV     DH, CL
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $C4, $E2, $68, $F7, $C0   // BEXTR  EAX, EAX, EDX
{$ELSE}
    BEXTR   EAX, EAX, EDX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ExtractBits_16_Asm(Value: UInt16; Start,Length: Integer): UInt16; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOVZX   RAX, CX
    SHL     R8, 8
    AND     RDX, $FF
    OR      RDX, R8
  {$ELSE}
    MOVZX   RAX, DI
    SHL     RDX, 8
    MOV     DL, SIL
  {$ENDIF}
{$ELSE}
    AND     EAX, $FFFF
    MOV     DH, CL
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $C4, $E2, $68, $F7, $C0   // BEXTR  EAX, EAX, EDX
{$ELSE}
    BEXTR   EAX, EAX, EDX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ExtractBits_32_Asm(Value: UInt32; Start,Length: Integer): UInt32; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     EAX, ECX
    SHL     R8, 8
    AND     RDX, $FF
    OR      RDX, R8
  {$ELSE}
    MOV     EAX, EDI
    SHL     RDX, 8
    MOV     DL, SIL
  {$ENDIF}
{$ELSE}
    MOV     DH, CL
{$ENDIF}
{$IFDEF ASM_MachineCode}
    DB  $C4, $E2, $68, $F7, $C0   // BEXTR  EAX, EAX, EDX
{$ELSE}
    BEXTR   EAX, EAX, EDX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ExtractBits_64_Asm(Value: UInt64; Start,Length: Integer): UInt64; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     RAX, RCX
    SHL     R8, 8
    AND     RDX, $FF
    OR      RDX, R8
  {$ELSE}
    MOV     RAX, RDI
    SHL     RDX, 8
    MOV     DL, SIL
  {$ENDIF}
  {$IFDEF ASM_MachineCode}
    DB  $C4, $E2, $E8, $F7, $C0   // BEXTR  RAX, RAX, RDX
  {$ELSE}
    BEXTR   RAX, RAX, RDX
  {$ENDIF}
{$ELSE}
    MOV     CL, AL
    MOV     CH, DL

    AND     EAX, $FF
    AND     EDX, $FF
    ADD     EAX, EDX

    XOR     EDX, EDX

    CMP     CL, 31
    JA      @AllHigh

    CMP     EAX, 32
    JBE     @AllLow

    // extraction is done across low and high dwords boundary
    MOV     EAX, dword ptr [Value]
    MOV     EDX, dword ptr [Value + 4]

    // extract from low dword
  {$IFDEF ASM_MachineCode}
    DB $C4, $E2, $70, $F7, $C0        // BEXTR  EAX, EAX, ECX
  {$ELSE}
    BEXTR   EAX, EAX, ECX
  {$ENDIF}

    // extract form high dword
    PUSH    ECX
    ADD     CH, CL
    SUB     CH, 32
    XOR     CL, CL

  {$IFDEF ASM_MachineCode}
    DB $C4, $E2, $70, $F7, $D2        // BEXTR  EDX, EDX, ECX
  {$ELSE}
    BEXTR   EDX, EDX, ECX
  {$ENDIF}

    // combine results
    POP     ECX
    PUSH    EBX

    XOR     EBX, EBX
    SHRD    EBX, EDX, CL
    SHR     EDX, CL
    OR      EAX, EBX

    POP     EBX
    JMP     @RoutineEnd

  // extraction is done only from low dword
  @AllLow:

    MOV     EAX, dword ptr [Value]
  {$IFDEF ASM_MachineCode}
    DB $C4, $E2, $70, $F7, $C0        // BEXTR  EAX, EAX, ECX
  {$ELSE}
    BEXTR   EAX, EAX, ECX
  {$ENDIF}
    JMP     @RoutineEnd

  // extraction is done only from high dword
  @AllHigh:

    SUB     CL, 32
    MOV     EAX, dword ptr [Value + 4]
  {$IFDEF ASM_MachineCode}
    DB $C4, $E2, $70, $F7, $C0        // BEXTR  EAX, EAX, ECX
  {$ELSE}
    BEXTR   EAX, EAX, ECX
  {$ENDIF}

  @RoutineEnd:
{$ENDIF}
end;

{$ENDIF}

//==============================================================================

var
  Var_ExtractBits_8: Function(Value: UInt8; Start,Length: Integer): UInt8; register = Fce_ExtractBits_8_Pas;
  Var_ExtractBits_16: Function(Value: UInt16; Start,Length: Integer): UInt16; register = Fce_ExtractBits_16_Pas;
  Var_ExtractBits_32: Function(Value: UInt32; Start,Length: Integer): UInt32; register = Fce_ExtractBits_32_Pas;
  Var_ExtractBits_64: Function(Value: UInt64; Start,Length: Integer): UInt64; register = Fce_ExtractBits_64_Pas;

//------------------------------------------------------------------------------

Function ExtractBits(Value: UInt8; Start,Length: Integer): UInt8;
begin
Result := Var_ExtractBits_8(Value,Start,Length);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ExtractBits(Value: UInt16; Start,Length: Integer): UInt16;
begin
Result := Var_ExtractBits_16(Value,Start,Length);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ExtractBits(Value: UInt32; Start,Length: Integer): UInt32;
begin
Result := Var_ExtractBits_32(Value,Start,Length);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ExtractBits(Value: UInt64; Start,Length: Integer): UInt64;
begin
Result := Var_ExtractBits_64(Value,Start,Length);
end;


{-------------------------------------------------------------------------------
================================================================================
                                  Deposit bits
================================================================================
-------------------------------------------------------------------------------}

Function DepositBits(Value,NewBits: UInt8; Start,Length: Integer): UInt8;
var
  Mask: UInt8;
begin
If UInt8(Start) <= 7 then
  begin
    If UInt8(Length) > 0 then
      begin
        Length := UInt8(Length);
        Mask := UInt8(UInt8($FF) shl Start);
        If Start + Length <= 8 then
          Mask := Mask and (UInt8($FF) shr (8 - (Start + Length)));
        Result := (Value and not Mask) or (UInt8(NewBits shl Start) and Mask);
      end
    else Result := Value;
  end
else Result := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DepositBits(Value,NewBits: UInt16; Start,Length: Integer): UInt16;
var
  Mask: UInt16;
begin
If UInt8(Start) <= 15 then
  begin
    If UInt8(Length) > 0 then
      begin
        Length := UInt8(Length);
        Mask := UInt16(UInt16($FFFF) shl Start);
        If Start + Length <= 16 then
          Mask := Mask and (UInt16($FFFF) shr (16 - (Start + Length)));
        Result := (Value and not Mask) or (UInt16(NewBits shl Start) and Mask);
      end
    else Result := Value;
  end
else Result := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DepositBits(Value,NewBits: UInt32; Start,Length: Integer): UInt32;
var
  Mask: UInt32;
begin
If UInt8(Start) <= 31 then
  begin
    If UInt8(Length) > 0 then
      begin
        Length := UInt8(Length);
        Mask := UInt32(UInt32($FFFFFFFF) shl Start);
        If Start + Length <= 32 then
          Mask := Mask and (UInt32($FFFFFFFF) shr (32 - (Start + Length)));
        Result := (Value and not Mask) or (UInt32(NewBits shl Start) and Mask);
      end
    else Result := Value;
  end
else Result := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DepositBits(Value,NewBits: UInt64; Start,Length: Integer): UInt64;
var
  Mask: UInt64;
begin
If UInt8(Start) <= 63 then
  begin
    If UInt8(Length) > 0 then
      begin
        Length := UInt8(Length);
        Mask := UInt64(UInt64($FFFFFFFFFFFFFFFF) shl Start);
        If Start + Length <= 64 then
          Mask := Mask and (UInt64($FFFFFFFFFFFFFFFF) shr (64 - (Start + Length)));
        Result := (Value and not Mask) or (UInt64(NewBits shl Start) and Mask);
      end
    else Result := Value;        
  end
else Result := Value;
end;

//==============================================================================

procedure DepositBitsValue(var Value: UInt8; NewBits: UInt8; Start,Length: Integer);
begin
Value := DepositBits(Value,NewBits,Start,Length);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DepositBitsValue(var Value: UInt16; NewBits: UInt16; Start,Length: Integer);
begin
Value := DepositBits(Value,NewBits,Start,Length);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DepositBitsValue(var Value: UInt32; NewBits: UInt32; Start,Length: Integer);
begin
Value := DepositBits(Value,NewBits,Start,Length);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure DepositBitsValue(var Value: UInt64; NewBits: UInt64; Start,Length: Integer);
begin
Value := DepositBits(Value,NewBits,Start,Length);
end;


{-------------------------------------------------------------------------------
================================================================================
                              Parallel bits extract
================================================================================
-------------------------------------------------------------------------------}

Function Fce_ParallelBitsExtract_8_Pas(Value,Mask: UInt8): UInt8; register;
var
  i:  Integer;
begin
Result := 0;
For i := 7 downto 0 do
  If ((Mask shr i) and 1) <> 0 then
    Result := UInt8(Result shl 1) or UInt8((Value shr i) and 1);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsExtract_16_Pas(Value,Mask: UInt16): UInt16; register;
var
  i:  Integer;
begin
Result := 0;
For i := 15 downto 0 do
  If ((Mask shr i) and 1) <> 0 then
    Result := UInt16(Result shl 1) or UInt16((Value shr i) and 1);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsExtract_32_Pas(Value,Mask: UInt32): UInt32; register;
var
  i:  Integer;
begin
Result := 0;
For i := 31 downto 0 do
  If ((Mask shr i) and 1) <> 0 then
    Result := UInt32(Result shl 1) or UInt32((Value shr i) and 1);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsExtract_64_Pas(Value,Mask: UInt64): UInt64; register;
var
  i:  Integer;
begin
Result := 0;
For i := 63 downto 0 do
  If ((Mask shr i) and 1) <> 0 then
    Result := UInt64(Result shl 1) or UInt64((Value shr i) and 1);
end;

//==============================================================================

{$IFDEF ASM_Extensions}

Function Fce_ParallelBitsExtract_8_Asm(Value,Mask: UInt8): UInt8; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RCX, $FF
    AND   RDX, $FF
    DB  $C4, $E2, $72, $F5, $C2     // PEXT   EAX,  ECX,  EDX
  {$ELSE}
    AND   RDI, $FF
    AND   RSI, $FF
    DB  $C4, $E2, $42, $F5, $C6     // PEXT   EAX,  EDI,  ESI
  {$ENDIF}
{$ELSE}
    AND   EAX, $FF
    AND   EDX, $FF
    DB  $C4, $E2, $7A, $F5, $C2     // PEXT   EAX,  EAX,  EDX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsExtract_16_Asm(Value,Mask: UInt16): UInt16; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RCX, $FFFF
    AND   RDX, $FFFF
    DB  $C4, $E2, $72, $F5, $C2     // PEXT   EAX,  ECX,  EDX
  {$ELSE}
    AND   RDI, $FFFF
    AND   RSI, $FFFF
    DB  $C4, $E2, $42, $F5, $C6     // PEXT   EAX,  EDI,  ESI
  {$ENDIF}
{$ELSE}
    AND   EAX, $FFFF
    AND   EDX, $FFFF
    DB  $C4, $E2, $7A, $F5, $C2     // PEXT   EAX,  EAX,  EDX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsExtract_32_Asm(Value,Mask: UInt32): UInt32; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    DB  $C4, $E2, $72, $F5, $C2     // PEXT   EAX,  ECX,  EDX
  {$ELSE}
    DB  $C4, $E2, $42, $F5, $C6     // PEXT   EAX,  EDI,  ESI
  {$ENDIF}
{$ELSE}
    DB  $C4, $E2, $7A, $F5, $C2     // PEXT   EAX,  EAX,  EDX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsExtract_64_Asm(Value,Mask: UInt64): UInt64; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    DB  $C4, $E2, $F2, $F5, $C2     // PEXT   RAX,  RCX,  RDX
  {$ELSE}
    DB  $C4, $E2, $C2, $F5, $C6     // PEXT   RAX,  RDI,  RSI
  {$ENDIF}
{$ELSE}
    MOV     EAX, dword ptr [Value]
    MOV     EDX, dword ptr [Value + 4]
    MOV     ECX, dword ptr [Mask]

    DB  $C4, $E2, $7A, $F5, $C1         // PEXT  EAX,  EAX,  ECX
    DB  $C4, $E2, $6A, $F5, $55, $0C    // PEXT  EDX,  EDX,  dword ptr [EBP + 12 {Mask + 4}]

    // combine results
    TEST    ECX, ECX
    JNZ     @Shift

    // low dword is empty
    MOV     EAX, EDX
    XOR     EDX, EDX
    JMP     @RoutineEnd

  @Shift:
    NOT     ECX
  {$IFDEF ASM_MachineCode}
    DB  $F3, $0F, $B8, $C9              // POPCNT  ECX,  ECX
  {$ELSE}
    POPCNT  ECX, ECX
  {$ENDIF}

    PUSH    EBX

    XOR     EBX, EBX
    SHRD    EBX, EDX, CL
    SHR     EDX, CL
    OR      EAX, EBX

    POP     EBX

  @RoutineEnd:
{$ENDIF}
end;

{$ENDIF}

//==============================================================================

var
  Var_ParallelBitsExtract_8: Function(Value,Mask: UInt8): UInt8; register = Fce_ParallelBitsExtract_8_Pas;
  Var_ParallelBitsExtract_16: Function(Value,Mask: UInt16): UInt16; register = Fce_ParallelBitsExtract_16_Pas;
  Var_ParallelBitsExtract_32: Function(Value,Mask: UInt32): UInt32; register = Fce_ParallelBitsExtract_32_Pas;
  Var_ParallelBitsExtract_64: Function(Value,Mask: UInt64): UInt64; register = Fce_ParallelBitsExtract_64_Pas;

//------------------------------------------------------------------------------

Function ParallelBitsExtract(Value,Mask: UInt8): UInt8;
begin
Result := Var_ParallelBitsExtract_8(Value,Mask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ParallelBitsExtract(Value,Mask: UInt16): UInt16;
begin
Result := Var_ParallelBitsExtract_16(Value,Mask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ParallelBitsExtract(Value,Mask: UInt32): UInt32;
begin
Result := Var_ParallelBitsExtract_32(Value,Mask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ParallelBitsExtract(Value,Mask: UInt64): UInt64;
begin
Result := Var_ParallelBitsExtract_64(Value,Mask);
end;

{-------------------------------------------------------------------------------
================================================================================
                              Parallel bits deposit
================================================================================
-------------------------------------------------------------------------------}

Function Fce_ParallelBitsDeposit_8_Pas(Value,Mask: UInt8): UInt8; register;
var
  i:  Integer;
begin
Result := 0;
For i := 0 to 7 do
  begin
    If ((Mask shr i) and 1) <> 0 then
      begin
        Result := Result or UInt8(UInt8(Value and 1) shl i);
        Value := Value shr 1;
      end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsDeposit_16_Pas(Value,Mask: UInt16): UInt16; register;
var
  i:  Integer;
begin
Result := 0;
For i := 0 to 15 do
  begin
    If ((Mask shr i) and 1) <> 0 then
      begin
        Result := Result or UInt16(UInt16(Value and 1) shl i);
        Value := Value shr 1;
      end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsDeposit_32_Pas(Value,Mask: UInt32): UInt32; register;
var
  i:  Integer;
begin
Result := 0;
For i := 0 to 31 do
  begin
    If ((Mask shr i) and 1) <> 0 then
      begin
        Result := Result or UInt32(UInt32(Value and 1) shl i);
        Value := Value shr 1;
      end;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsDeposit_64_Pas(Value,Mask: UInt64): UInt64; register;
var
  i:  Integer;
begin
Result := 0;
For i := 0 to 63 do
  begin
    If ((Mask shr i) and 1) <> 0 then
      begin
        Result := Result or UInt64(UInt64(Value and 1) shl i);
        Value := Value shr 1;
      end;
  end;
end;

//==============================================================================

{$IFDEF ASM_Extensions}

Function Fce_ParallelBitsDeposit_8_Asm(Value,Mask: UInt8): UInt8; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RCX, $FF
    AND   RDX, $FF
    DB  $C4, $E2, $73, $F5, $C2     // PDEP   EAX,  ECX,  EDX
  {$ELSE}
    AND   RDI, $FF
    AND   RSI, $FF
    DB  $C4, $E2, $43, $F5, $C6     // PDEP   EAX,  EDI,  ESI
  {$ENDIF}
{$ELSE}
    AND   EAX, $FF
    AND   EDX, $FF
    DB  $C4, $E2, $7B, $F5, $C2     // PDEP   EAX,  EAX,  EDX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsDeposit_16_Asm(Value,Mask: UInt16): UInt16; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    AND   RCX, $FFFF
    AND   RDX, $FFFF
    DB  $C4, $E2, $73, $F5, $C2     // PDEP   EAX,  ECX,  EDX
  {$ELSE}
    AND   RDI, $FFFF
    AND   RSI, $FFFF
    DB  $C4, $E2, $43, $F5, $C6     // PDEP   EAX,  EDI,  ESI
  {$ENDIF}
{$ELSE}
    AND   EAX, $FFFF
    AND   EDX, $FFFF
    DB  $C4, $E2, $7B, $F5, $C2     // PDEP   EAX,  EAX,  EDX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsDeposit_32_Asm(Value,Mask: UInt32): UInt32; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    DB  $C4, $E2, $73, $F5, $C2     // PDEP   EAX,  ECX,  EDX
  {$ELSE}
    DB  $C4, $E2, $43, $F5, $C6     // PDEP   EAX,  EDI,  ESI
  {$ENDIF}
{$ELSE}
    DB  $C4, $E2, $7B, $F5, $C2     // PDEP   EAX,  EAX,  EDX
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Fce_ParallelBitsDeposit_64_Asm(Value,Mask: UInt64): UInt64; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    DB  $C4, $E2, $F3, $F5, $C2     // PDEP   RAX,  RCX,  RDX
  {$ELSE}
    DB  $C4, $E2, $C3, $F5, $C6     // PDEP   RAX,  RDI,  RSI
  {$ENDIF}
{$ELSE}
    XOR     EAX, EAX
    MOV     EDX, dword ptr [Value]
    MOV     ECX, dword ptr [Mask]

    TEST    ECX, ECX
    JZ      @DepositHigh

    DB  $C4, $E2, $6B, $F5, $C1       // PDEP   EAX,  EDX,  ECX

  {$IFDEF ASM_MachineCode}
    DB  $F3, $0F, $B8, $C9            // POPCNT ECX,  ECX
  {$ELSE}
    POPCNT  ECX, ECX
  {$ENDIF}

    CMP     ECX, 32
    CMOVAE  EDX, dword ptr [Value + 4]
    JAE     @DepositHigh

  @Shift:
    PUSH    EBX

    MOV     EBX, dword ptr [Value + 4]
    SHRD    EDX, EBX, CL

    POP     EBX

  @DepositHigh:
    DB  $C4, $E2, $6B, $F5, $55, $0C  // PDEP   EDX,  EDX,  dword ptr [EBP + 12 {Mask + 4}]
{$ENDIF}
end;

{$ENDIF}

//==============================================================================

var
  Var_ParallelBitsDeposit_8: Function(Value,Mask: UInt8): UInt8; register = Fce_ParallelBitsDeposit_8_Pas;
  Var_ParallelBitsDeposit_16: Function(Value,Mask: UInt16): UInt16; register = Fce_ParallelBitsDeposit_16_Pas;
  Var_ParallelBitsDeposit_32: Function(Value,Mask: UInt32): UInt32; register = Fce_ParallelBitsDeposit_32_Pas;
  Var_ParallelBitsDeposit_64: Function(Value,Mask: UInt64): UInt64; register = Fce_ParallelBitsDeposit_64_Pas;

//------------------------------------------------------------------------------

Function ParallelBitsDeposit(Value,Mask: UInt8): UInt8;
begin
Result := Var_ParallelBitsDeposit_8(Value,Mask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ParallelBitsDeposit(Value,Mask: UInt16): UInt16;
begin
Result := Var_ParallelBitsDeposit_16(Value,Mask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ParallelBitsDeposit(Value,Mask: UInt32): UInt32;
begin
Result := Var_ParallelBitsDeposit_32(Value,Mask);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ParallelBitsDeposit(Value,Mask: UInt64): UInt64;
begin
Result := Var_ParallelBitsDeposit_64(Value,Mask);
end;

{-------------------------------------------------------------------------------
================================================================================
                                   Bit parity
================================================================================
-------------------------------------------------------------------------------}

Function BitParity(Value: UInt8): Boolean;
begin
Value := Value xor (Value shr 4);
Value := Value xor (Value shr 2);
Value := Value xor (Value shr 1);
Result := (Value and 1) = 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BitParity(Value: UInt16): Boolean;
begin
Value := Value xor (Value shr 8);
Value := Value xor (Value shr 4);
Value := Value xor (Value shr 2);
Value := Value xor (Value shr 1);
Result := (Value and 1) = 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BitParity(Value: UInt32): Boolean;
begin
Value := Value xor (Value shr 16);
Value := Value xor (Value shr 8);
Value := Value xor (Value shr 4);
Value := Value xor (Value shr 2);
Value := Value xor (Value shr 1);
Result := (Value and 1) = 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BitParity(Value: UInt64): Boolean;
begin
Value := Value xor (Value shr 32);
Value := Value xor (Value shr 16);
Value := Value xor (Value shr 8);
Value := Value xor (Value shr 4);
Value := Value xor (Value shr 2);
Value := Value xor (Value shr 1);
Result := (Value and 1) = 0;
end;


{===============================================================================
--------------------------------------------------------------------------------

                               Pointer operations

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                           Pointer arithmetic helpers
================================================================================
-------------------------------------------------------------------------------}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}

Function PtrAdvance(Ptr: Pointer; Offset: TMemOffset): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(PtrUInt(Ptr) + PtrUInt(Offset));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function PtrAdvance(Ptr: Pointer; Count: Integer; Stride: TMemSize): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(PtrUInt(Ptr) + PtrUInt(PtrInt(Count) * PtrInt(Stride)));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure PtrAdvanceVar(var Ptr: Pointer; Offset: TMemOffset);
begin
Ptr := PtrAdvance(Ptr,Offset);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure PtrAdvanceVar(var Ptr: Pointer; Count: Integer; Stride: TMemSize);
begin
Ptr := PtrAdvance(Ptr,Count,Stride);
end;

//------------------------------------------------------------------------------

Function AdvancePtr(Ptr: Pointer; Offset: TMemOffset): Pointer;
begin
Result := PtrAdvance(Ptr,Offset)
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function AdvancePtr(Ptr: Pointer; Count: Integer; Stride: TMemSize): Pointer;
begin
Result := PtrAdvance(Ptr,Count,Stride);
end;

//------------------------------------------------------------------------------

procedure AdvancePtrVar(var Ptr: Pointer; Offset: TMemOffset);
begin
Ptr := PtrAdvance(Ptr,Offset);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure AdvancePtrVar(var Ptr: Pointer; Count: Integer; Stride: TMemSize);
begin
Ptr := PtrAdvance(Ptr,Count,Stride);
end;

{-------------------------------------------------------------------------------
================================================================================
                               Address comparison
================================================================================
-------------------------------------------------------------------------------}

Function PtrCompare(A,B: Pointer): Integer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If PtrUInt(A) < PtrUInt(B) then
  Result := -1
else If PtrUInt(A) > PtrUInt(B) then
  Result := +1
else
  Result := 0;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{-------------------------------------------------------------------------------
================================================================================
                            Memory address alignment
================================================================================
-------------------------------------------------------------------------------}

Function AlignmentBytes(Alignment: TMemoryAlignment): TMemSize;
begin
case Alignment of
  maNone,
  ma8bit,ma1byte:       Result := 1;
  ma16bit,ma2byte:      Result := 2;
  ma32bit,ma4byte:      Result := 4;
  ma64bit,ma8byte:      Result := 8;
  ma128bit,ma16byte:    Result := 16;
  ma256bit,ma32byte:    Result := 32;
  ma512bit,ma64byte:    Result := 64;
  ma1024bit,ma128byte:  Result := 128;
  ma2048bit,ma256byte:  Result := 256;
else
  raise EBOInvalidValue.CreateFmt('AlignmentBytes: Invalid memory alignment (%d).',[Ord(Alignment)]);
end;
end;

//------------------------------------------------------------------------------

Function CheckAlignment(Address: Pointer; Alignment: TMemoryAlignment): Boolean;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := (PtrUInt(Address) and PtrUInt(Pred(AlignmentBytes(Alignment)))) = 0;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function Misalignment(Address: Pointer; Alignment: TMemoryAlignment): TMemSize;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := TMemSize(PtrUInt(Address) - (PtrUInt(Address) and not PtrUInt(Pred(AlignmentBytes(Alignment)))));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function AlignmentOffset(Address: Pointer; Alignment: TMemoryAlignment): TMemSize;
var
  Malign: TMemSize;
begin
Malign := Misalignment(Address,Alignment);
If Malign <> 0 then
  Result := AlignmentBytes(Alignment) - Malign
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function ResolveAlignment(Address: Pointer; ByteAlignments: Boolean = False): TMemoryAlignment;

  Function LowAlignment: TMemoryAlignment;
  begin
    If ByteAlignments then
      Result := ma1byte
    else
      Result := ma8bit;
  end;

  Function HighAlignment: TMemoryAlignment;
  begin
    If ByteAlignments then
      Result := ma256byte
    else
      Result := ma2048bit;
  end;

var
  i:  TMemoryAlignment;
begin
Result := maNone;
For i := HighAlignment downto LowAlignment do
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  If (PtrUInt(Address) and PtrUInt(Pred(AlignmentBytes(i)))) = 0 then
{$IFDEF FPCDWM}{$POP}{$ENDIF}
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function AlignedMemory(Address: Pointer; Alignment: TMemoryAlignment): Pointer;
var
  AlignBytes: TMemSize;
begin
// to remove a need for two calls to AlignmentBytes...
AlignBytes := AlignmentBytes(Alignment);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer((PtrUInt(Address) + PtrUInt(Pred(AlignBytes))) and not PtrUInt(Pred(AlignBytes)));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure AlignMemory(var Address: Pointer; Alignment: TMemoryAlignment);
begin
Address := AlignedMemory(Address,Alignment);
end;


{===============================================================================
--------------------------------------------------------------------------------

                             Binary data operations

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                             Binary data comparison
================================================================================
-------------------------------------------------------------------------------}

Function CompareData(const A; SizeA: TMemSize; const B; SizeB: TMemSize; CompareMethod: TCompareMethod): Integer;


  Function CompareBytes: Integer;
  var
    i:    TMemSize;
    PtrA: PByte;
    PtrB: PByte;
  begin
    Result := 0;
    PtrA := @A;
    PtrB := @B;
    For i := 1 to MemSizeMin(SizeA,SizeB) do  // when either size is zero, this will execute no cycle
      begin
        If PtrA^ <> PtrB^ then
          begin
            If PtrA^ < PtrB^ then
              Result := -1
            else
              Result := +1;
            Exit;
          end;
        Inc(PtrA);
        Inc(PtrB);
      end;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function CompareSizes: Integer;
  begin
    If SizeA < SizeB then
      Result := -1
    else If SizeA > SizeB then
      Result := +1
    else
      Result := 0;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function ComparePadFront: Integer;

    procedure DoLoadAndMove(var Pad: TMemSize; var Ptr: PByte; out Buff: Byte);
    begin
      If Pad > 0 then
        begin
          Buff := 0;
          Dec(Pad);
        end
      else
        begin
          Buff := Ptr^;
          Inc(Ptr);
        end;
    end;

  var
    i:            TMemSize;
    PadA,PadB:    TMemSize;
    PtrA,PtrB:    PByte;
    BuffA,BuffB:  Byte;
  begin
    Result := 0;
    If SizeA < SizeB then
      begin
        PadA := SizeB - SizeA;
        PadB := 0;
      end
    else
      begin
        PadA := 0;
        PadB := SizeA - SizeB;
      end;
    PtrA := @A;
    PtrB := @B;
    For i := 1 to MemSizeMax(SizeA,SizeB) do
      begin
        DoLoadAndMove(PadA,PtrA,BuffA);
        DoLoadAndMove(PadB,PtrB,BuffB);
        // do comparison on loaded buffers
        If BuffA <> BuffB then
          begin
            If BuffA < BuffB then
              Result := -1
            else
              Result := +1;
            Exit;
          end;
      end;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function ComparePadBack: Integer;
  var
    i:            TMemSize;
    PtrA,PtrB:    PByte;
    BuffA,BuffB:  Byte;
  begin
    Result := 0;
    PtrA := @A;
    PtrB := @B;
    For i := 1 to MemSizeMax(SizeA,SizeB) do
      begin
        If i > SizeA then
          BuffA := 0
        else
          BuffA := PtrA^;
        If i > SizeB then
          BuffB := 0
        else
          BuffB := PtrB^;
        If BuffA <> BuffB then
          begin
            If BuffA < BuffB then
              Result := -1
            else
              Result := +1;
            Exit;
          end;
        Inc(PtrA);
        Inc(PtrB);
      end;
  end;

begin
case CompareMethod of
  cmDataSize:     begin
                    Result := CompareBytes;
                    If Result = 0 then
                      Result := CompareSizes;
                  end;
  cmEqSizeData:   If SizeA = SizeB then
                    Result := CompareBytes
                  else
                    raise EBOSizeMismatch.CreateFmt('CompareData: Mismatch in data sizes (%d,%d).',[SizeA,SizeB]);
  cmDataPadFront: If SizeA <> SizeB then
                    Result := ComparePadFront
                  else
                    Result := CompareBytes;
  cmDataPadBack:  If SizeA <> SizeB then
                    Result := ComparePadBack
                  else
                    Result := CompareBytes;
else
 {cmSizeData}
  Result := CompareSizes;
  If Result = 0 then
    Result := CompareBytes;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareData(A,B: array of UInt8; CompareMethod: TCompareMethod): Integer;

  Function IntegerMin(A,B: Integer): Integer;{$IFDEF CanInline} inline; {$ENDIF}
  begin
    If A < B then
      Result := A
    else
      Result := B;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function IntegerMax(A,B: Integer): Integer;{$IFDEF CanInline} inline; {$ENDIF}
  begin
    If A > B then
      Result := A
    else
      Result := B;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function CompareItems: Integer;
  var
    i:  Integer;
  begin
    Result := 0;
    For i := 0 to IntegerMin(High(A),High(B)) do
      If A[i] <> B[i] then
        begin
          If A[i] < B[i] then
            Result := -1
          else
            Result := +1;
          Exit;
        end;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function CompareSizes: Integer;
  begin
    If Length(A) < Length(B) then
      Result := -1
    else If Length(A) > Length(B) then
      Result := +1
    else
      Result := 0;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function ComparePadFront: Integer;
  var
    i:            Integer;
    OffA,OffB:    Integer;
    BuffA,BuffB:  Byte;
  begin
    Result := 0;
    If Length(A) < Length(B) then
      begin
        OffA := Length(B) - Length(A);
        OffB := 0;
      end
    else
      begin
        OffA := 0;
        OffB := Length(A) - Length(B);
      end;
    For i := 0 to IntegerMax(High(A),High(B)) do
      begin
        If i - OffA >= Low(A) then
          BuffA := A[i - OffA]
        else
          BuffA := 0;
        If i - OffB >= Low(B) then
          BuffB := B[i - OffB]
        else
          BuffB := 0;
        // do comparison on loaded buffers
        If BuffA <> BuffB then
          begin
            If BuffA < BuffB then
              Result := -1
            else
              Result := +1;
            Exit;
          end;
      end;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function ComparePadback: Integer;
  var
    i:            Integer;
    BuffA,BuffB:  Byte;
  begin
    Result := 0;
    For i := 0 to IntegerMax(High(A),High(B)) do
      begin
        If i > High(A) then
          BuffA := 0
        else
          BuffA := A[i];
        If i > High(B) then
          BuffB := 0
        else
          BuffB := B[i];
        If BuffA <> BuffB then
          begin
            If BuffA < BuffB then
              Result := -1
            else
              Result := +1;
            Exit;
          end;
      end;
  end;

begin
case CompareMethod of
  cmDataSize:     begin
                    Result := CompareItems;
                    If Result = 0 then
                      Result := CompareSizes;
                  end;
  cmEqSizeData:   If Length(A) = Length(B) then
                    Result := CompareItems
                  else
                    raise EBOSizeMismatch.CreateFmt('CompareData: Mismatch in data sizes (%d,%d).',[Length(A),Length(B)]);
  cmDataPadFront: If Length(A) <> Length(B) then
                    Result := ComparePadFront
                  else
                    Result := CompareItems;
  cmDataPadBack:  If Length(A) <> Length(B) then
                    Result := ComparePadBack
                  else
                    Result := CompareItems;
else
 {cmSizeData}
  Result := CompareSizes;
  If Result = 0 then
    Result := CompareItems;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareData(const A; SizeA: TMemSize; const B; SizeB: TMemSize; AllowSizeDiff: Boolean = True): Integer;
begin
If AllowSizeDiff then
  Result := CompareData(A,SizeA,B,SizeB,cmSizeData)
else
  Result := CompareData(A,SizeA,B,SizeB,cmEqSizeData);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareData(A,B: array of UInt8; AllowSizeDiff: Boolean = True): Integer;
begin
If AllowSizeDiff then
  Result := CompareData(A,B,cmSizeData)
else
  Result := CompareData(A,B,cmEqSizeData);
end;

{-------------------------------------------------------------------------------
================================================================================
                              Binary data equality
================================================================================
-------------------------------------------------------------------------------}

Function SameData(const A; SizeA: TMemSize; const B; SizeB: TMemSize): Boolean;
const
  SD_BYTE_COEF = {$IFDEF CPU64bit}SizeOf(UInt64){$ELSE}SizeOf(UInt32){$ENDIF};
var
  i:    TMemSize;
  PtrA: Pointer;
  PtrB: Pointer;
begin
If SizeA = SizeB then
  begin
    Result := True;
    PtrA := @A;
    PtrB := @B;
    // test on whole Q/DWords
    If SizeA >= (16 * SD_BYTE_COEF) then
      begin
        For i := 1 to (SizeA div SD_BYTE_COEF) do
          begin
          {$IFDEF CPU64bit}
            If PUInt64(PtrA)^ <> PUInt64(PtrB)^ then
          {$ELSE}
            If PUInt32(PtrA)^ <> PUInt32(PtrB)^ then
          {$ENDIF}
              begin
                Result := False;
                Exit;
              end;
            PtrA := PtrAdvance(PtrA,SD_BYTE_COEF);
            PtrB := PtrAdvance(PtrB,SD_BYTE_COEF);
          end;
        SizeA := SizeA mod SD_BYTE_COEF;
      end;
    // test remaining bytes
    For i := 1 to SizeA do  // when size is zero, this will execute no cycle
      begin
        If PByte(PtrA)^ <> PByte(PtrB)^ then
          begin
            Result := False;
            Exit;
          end;
        Inc(PByte(PtrA));
        Inc(PByte(PtrB));
      end;
  end
else Result := False; // buffers differ in size
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameData(A,B: array of UInt8): Boolean;
var
  i:        Integer;
  APacked:  Boolean;
  BPacked:  Boolean;
begin
// Let's not assume the bytes are inherently packed in the memory...
If Length(A) = Length(B) then
  begin
    If Length(A) > 0 then
      begin
       If Length(A) > 128 then
          begin
          {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
            APacked := PtrUInt(@A[1]) - PtrUInt(@A[0]) <= 1;
            BPacked := PtrUInt(@B[1]) - PtrUInt(@B[0]) <= 1;
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
          end
        else
          begin
            APacked := False;
            BPacked := False;
          end;
        If APacked and BPacked then
          // arrays are packed, compare them as buffers
          Result := SameData(A[Low(A)],Length(A),B[Low(B)],Length(B))
        else
          begin
            // array are not packed, compare item by item
            Result := True;
            For i := Low(A) to High(A) do
              If A[i] <> B[i] then
                begin
                  Result := False;
                  Break{For i};
                end;
          end;
      end
    else Result := True;  // both arrays are empty
  end
else Result := False; // arrays differ in length
end;

{-------------------------------------------------------------------------------
================================================================================
                               Buffer shift down
================================================================================
-------------------------------------------------------------------------------}

procedure BufferShiftDown(var Buffer; BufferSize: TMemSize; Shift: TMemSize);
begin
If (Shift > 0) and (Shift < BufferSize) then
  Move(PtrAdvance(Addr(Buffer),Shift)^,Buffer,BufferSize - Shift);
end;

{-------------------------------------------------------------------------------
================================================================================
                                  Bits copying
================================================================================
-------------------------------------------------------------------------------}

procedure CopyBits(Source,Destination: Pointer; BitCount: TMemSize);
var
  Mask:     UInt8;
  DestTemp: PUInt8;
begin
If (Source <> Destination) and (BitCount > 0) then
  begin
    If (BitCount and 7) <> 0 then
      begin
        // mask is used in any case, so let's calculate it here
        Mask := UInt8($FF) shr (8 - (BitCount and 7));
        // check whether the destination starts inside of source....
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        If (PtrUInt(Source) < PtrUInt(Destination)) and
          ((PtrUInt(Source) + PtrUInt((BitCount + 7) shr 3)) > PtrUInt(Destination)) then
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
          begin
            // source can be overwritten, do backward copy...
            // first do the last, partial byte
            DestTemp := PtrAdvance(Destination,Pred((BitCount + 7) shr 3));
            PUInt8(DestTemp)^ := (PUInt8(DestTemp)^ and not Mask) or
              (PUInt8(PtrAdvance(Source,Pred((BitCount + 7) shr 3)))^ and Mask);
          {
            And now complete bytes - System.Move is assumed to be optimized and
            protected againts overwrites, therefore it is used instead of
            custom code.
          }
            System.Move(Source^,Destination^,BitCount shr 3);
          end
        else
          begin
            // source cannot be overwritten, do normal forward copy...
            // first full bytes...
            System.Move(Source^,Destination^,BitCount shr 3);
            DestTemp := PtrAdvance(Destination,BitCount shr 3);
            // ...and now the rest
            PUInt8(DestTemp)^ := (PUInt8(DestTemp)^ and not Mask) or
              (PUInt8(PtrAdvance(Source,BitCount shr 3))^ and Mask);
          end;
      end
    else System.Move(Source^,Destination^,BitCount shr 3);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure CopyBits(Source,Destination: Pointer; SrcBitOffset,DstBitOffset,BitCount: TMemSize);

  Function DestinationIsWithinSource: Boolean;
  var
    SrcByteCount: TMemSize;
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    If (PtrUInt(Destination) >= PtrUInt(Source)) then
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
      begin
      {
        Destination pointer is somewhere abowe or at the source pointer.

        Note - ByteCount cannot resolve to zero, because this function is not
               called if BitCount is zero.
      }
        SrcByteCount := TMemSize((SrcBitOffset + BitCount + 7) shr 3){equivalent to "Ceil((SrcBitOffset + BitCount) / 8)"};
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        If PtrUInt(Destination) < (PtrUInt(Source) + PtrUInt(SrcByteCount)) then
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
          begin
            // destination pointer is inside of source bytes...
            If Destination = Source then
              // destination and source pointer are equal, result depends on bit offsets
              Result := DstBitOffset >= SrcBitOffset
          {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
            else If (PtrUInt(Destination) + 1) = (PtrUInt(Source) + PtrUInt(SrcByteCount)) then
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
            {
              Destination pointer points to the last byte of source memory,
              result depends on bit count and destination bit offset.

              In reality we calculate number of source bits in the last source
              byte and use this number for comparison.
            }
              Result := DstBitOffset < (({$IFNDEF CPU64bit}Int64{$ENDIF}(SrcBitOffset) + BitCount) - ((SrcByteCount - 1) shl 3))
            else
              // destination pointer is somewhere in the middle, just return true
              Result := True;
          end
        // destination pointer is completely behind the source byes
        else Result := False;
      end
    // destination pointer is below source pointer, so it cannot be inside the source memory
    else Result := False;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
const
  ChunkSize = {$IFDEF CPU64Bit}7{$ELSE}3{$ENDIF};
type
  TLocalLong = {$IFDEF CPU64Bit}UInt64{$ELSE}UInt32{$ENDIF};
var
  ByteWriteMask:  UInt16;
  LongWriteMask:  TLocalLong;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function CBO16(Value: UInt16): UInt16;  // correct byte order
  begin
    Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Value);
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function CBO(Value: TLocalLong): TLocalLong;
  begin
    Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Value);
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  procedure DoFullLongCopy;
  type
    TChunk = packed array[0..Pred(ChunkSize)] of UInt8;

    Function ReadChunk: TLocalLong;
    var
      Chunk:  TChunk;
    begin
      Chunk := TChunk(Source^);
      Result := TLocalLong(Chunk[0])
        or (TLocalLong(Chunk[1]) shl 8) or (TLocalLong(Chunk[2]) shl 16)
    {$IFDEF CPU64Bit}
        or (TLocalLong(Chunk[3]) shl 24) or (TLocalLong(Chunk[4]) shl 32)
        or (TLocalLong(Chunk[5]) shl 40) or (TLocalLong(Chunk[6]) shl 48)
    {$ENDIF};
    end;

    procedure WriteChunk(Value: TLocalLong);
    var
      Chunk:  TChunk;
    begin
      Chunk[0] := Value and $FF;
      Chunk[1] := (Value shr 8) and $FF;
      Chunk[2] := (Value shr 16) and $FF;
    {$IFDEF CPU64Bit}
      Chunk[3] := (Value shr 24) and $FF;
      Chunk[4] := (Value shr 32) and $FF;
      Chunk[5] := (Value shr 40) and $FF;
      Chunk[6] := (Value shr 48) and $FF;
    {$ENDIF};
      TChunk(Destination^) := Chunk;
    end;

  var
    Buffer: TLocalLong;
  begin
    // read buffer
    If SrcBitOffset <> 0 then
      Buffer := CBO(TLocalLong(Source^)) shr SrcBitOffset
    else
      Buffer := ReadChunk;
    // write buffer
    If DstBitOffset <> 0 then
      TLocalLong(Destination^) := CBO(((Buffer shl DstBitOffset) and not LongWriteMask) or (CBO(TLocalLong(Destination^)) and LongWriteMask))
    else
      WriteChunk(Buffer);
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  procedure DoFullByteCopy;
  var
    Buffer: UInt8;
  begin
    // read buffer
    If SrcBitOffset <> 0 then
      Buffer := UInt8(CBO16(PUInt16(Source)^) shr SrcBitOffset)
    else
      Buffer := PUInt8(Source)^;
    // write buffer
    If DstBitOffset <> 0 then
      PUInt16(Destination)^ := CBO16((UInt16(Buffer) shl DstBitOffset) or (CBO16(PUInt16(Destination)^) and ByteWriteMask))
    else
      PUInt8(Destination)^ := Buffer;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  procedure DoPartialByteCopy;
  var
    Buffer: UInt8;
  begin
    // load bits
    If (SrcBitOffset + (BitCount and 7)) > 8 then
      // source bits span byte boundary
      Buffer := UInt8(GetBits(CBO16(PUInt16(Source)^),Integer(SrcBitOffset),Pred(Integer(SrcBitOffset + (BitCount and 7))),True))
    else
      // all source bits are within one byte
      Buffer := GetBits(PUInt8(Source)^,Integer(SrcBitOffset),Pred(Integer(SrcBitOffset + (BitCount and 7))),True);
    // store bits
    If (DstBitOffset + (BitCount and 7)) > 8 then
      // destination span byte boundary
      PUInt16(Destination)^ := CBO16(SetBits(CBO16(PUInt16(Destination)^),UInt16(UInt16(Buffer) shl DstBitOffset),
        Integer(DstBitOffset),Pred(Integer(DstBitOffset + (BitCount and 7))),False))
    else
      // destination is within one byte
      PUInt8(Destination)^ := SetBits(PUInt8(Destination)^,UInt8(Buffer shl DstBitOffset),
        Integer(DstBitOffset),Pred(Integer(DstBitOffset + (BitCount and 7))),False);
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

var
  BytesToCopy:  TMemSize;
  i:            TMemSize;
begin
If BitCount > 0 then
  begin
    // rectify pointers and bit shifts for large (>7) shift values
    Inc(PUInt8(Source),SrcBitOffset shr 3);
    Inc(PUInt8(Destination),DstBitOffset shr 3);
    SrcBitOffset := SrcBitOffset and 7;
    DstBitOffset := DstBitOffset and 7;
    If (SrcBitOffset <> 0) or (DstBitOffset <> 0) then
      begin
        // at least one bit offset is non-zero
        If (Source <> Destination) or (SrcBitOffset <> DstBitOffset) then
          begin
            BytesToCopy := BitCount shr 3;
            ByteWriteMask := not UInt16(UInt16($00FF) shl DstBitOffset);
          {$IFDEF CPU64Bit}
            LongWriteMask := not UInt64(UInt64($00FFFFFFFFFFFFFF) shl DstBitOffset);
          {$ELSE}
            LongWriteMask := not UInt32(UInt32($00FFFFFF) shl DstBitOffset);
          {$ENDIF}
            If DestinationIsWithinSource then
              begin
                // destination starts somewhere withing the source memory, do backward copy
                Source := PtrAdvance(Source,(BitCount + 7) shr 3);
                Destination := PtrAdvance(Destination,(BitCount + 7) shr 3);
                // first the trailing partial byte
                If (BitCount and 7) <> 0 then
                  begin
                    // first advance poiters
                    Dec(PUInt8(Source));
                    Dec(PUInt8(Destination));
                    DoPartialByteCopy;
                  end;
                // chunks
                For i := 1 to (BytesToCopy div ChunkSize) do
                  begin
                    Dec(PUInt8(Source),ChunkSize);
                    Dec(PUInt8(Destination),ChunkSize);
                    DoFullLongCopy;
                    Dec(BytesToCopy,ChunkSize);
                  end;
                // "whole" bytes
                For i := 1 to BytesToCopy do
                  begin
                    Dec(PUInt8(Source));
                    Dec(PUInt8(Destination));
                    DoFullByteCopy;
                  end;
              end
            else
              begin
                // destination is not within the source, do "normal" forward copy
                // chunks
                For i := 1 to (BytesToCopy div ChunkSize) do
                  begin
                    DoFullLongCopy;
                    Inc(PUInt8(Source),ChunkSize);
                    Inc(PUInt8(Destination),ChunkSize);
                    Dec(BytesToCopy,ChunkSize);
                  end;
                // "whole" bytes
                For i := 1 to BytesToCopy do
                  begin
                    DoFullByteCopy;
                    Inc(PUInt8(Source));
                    Inc(PUInt8(Destination));
                  end;
                // and the remaining bits, if any
                If (BitCount and 7) <> 0 then
                  DoPartialByteCopy;
              end;
          end;
      end
    // both bit offsets are zero, call simplified implementation
    else CopyBits(Source,Destination,BitCount);
  end;
end;

{-------------------------------------------------------------------------------
================================================================================
                              Memory space filling
================================================================================
-------------------------------------------------------------------------------}

procedure FillByte(var Dst; Count: TMemSize; Value: UInt8);
{$IFNDEF PurePascal}
asm
{
                    win32 & lin32         win64             lin64
      @Dst               EAX               RCX               RDI
     Count               EDX               RDX               RSI
     Value                CL               R8B                DL
}
{$IFDEF x64}
  // unify arguments distribution somewhat...
  {$IFDEF Windows}
      PUSH    RDI

      MOV     RDI, RCX
      MOV     RCX, RDX
      MOVZX   RAX, R8B
  {$ELSE}
      MOV     RCX, RSI
      MOVZX   RAX, DL
  {$ENDIF}
{
  ...so by now the arguments are:

     @Dst - RDI
    Count - RCX
    Value - AL (higher bits are explicitly zeroed)

    RDX is used locally to preserve actual value of count.
}
      // prepare data
      MOV     R9, $0101010101010101
      IMUL    RAX, R9
      // clear direction flag
      CLD

      // do not use complex algorithm for small data
      CMP     RCX, 128
      JBE     @StoreQuads

      // align destination on 8-byte boundary for burst stores
      TEST    RDI, 7
      JZ      @StoreBlocks

      // preserve original count in RDX
      MOV     RDX, RCX
      MOV     RCX, RDI
      AND     RCX, 7
      SUB     RCX, 8
      NEG     RCX
      SUB     RDX, RCX

      // write RCX bytes (AL)
      REP STOSB

      MOV     RCX, RDX

@StoreBlocks:
      // destination pointer is aligned, copy blocks of 64 bytes
      CMP     RCX, 64
      JB      @StoreQuads
      MOV     RDX, RCX
      SHR     RCX, 6

      // decrement the count in one go
      MOV     R9, RCX
      SHL     R9, 6
      SUB     RDX, R9

{
  Store with non-temporal hint.

    NOTE - instructions MOVNTI and MFENCE are both part of SSE2 instruction set
           extension, but since all 64bit systems require SSE2 to be present
           and all 64bit-capable CPUs should support this extension, we do not
           test its support here (it is hard-tested in unit initialization).
}
@BlockLoop:
      MOVNTI  qword ptr [RDI], RAX
      MOVNTI  qword ptr [RDI + 8], RAX
      MOVNTI  qword ptr [RDI + 16], RAX
      MOVNTI  qword ptr [RDI + 24], RAX
      MOVNTI  qword ptr [RDI + 32], RAX
      MOVNTI  qword ptr [RDI + 40], RAX
      MOVNTI  qword ptr [RDI + 48], RAX
      MOVNTI  qword ptr [RDI + 56], RAX

      ADD     RDI, 64
      DEC     RCX
      JNZ     @BlockLoop

      // ensure memory integrity after non-temporal stores
      MFENCE

      MOV     RCX, RDX

@StoreQuads:
      // write remaining quadwords
      MOV     RDX, RCX
      SHR     RDX, 3
      JZ      @StoreBytes
      XCHG    RCX, RDX

      // write RCX quadwords (RAX)
      REP STOSQ

      MOV     RCX, RDX
      // there can now be at most 7 bytes left
      AND     RCX, 7

@StoreBytes:
      // write remaining (RCX) bytes (AL)
      REP STOSB

@RoutineEnd:
  {$IFDEF Windows}
      POP     RDI
  {$ENDIF}

{$ELSE}
      // nothing spectacular here...

      PUSH  EDI
      CLD

      MOV   EDI, EAX

      AND   ECX, $FF
      IMUL  EAX, ECX, $01010101

      MOV   ECX, EDX
      SHR   ECX, 2

      REP STOSD

      MOV   ECX, EDX
      AND   ECX, 3

      REP STOSB

      POP   EDI

{$ENDIF}
end;
{$ELSE}
var
  Buff: PUInt8;
  i:    TMemSize;
begin
If Count > 0 then
  begin
    Buff := @Dst;
    For i := 0 to Pred(Count) do
      begin
        Buff^ := Value;
        Inc(Buff);
      end;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure FillWord(var Dst; Count: TMemSize; Value: UInt16);
{$IFNDEF PurePascal}
asm
{$IFDEF x64}

  {$IFDEF Windows}
      PUSH    RDI

      MOV     RDI, RCX
      MOV     RCX, RDX
      MOVZX   RAX, R8W
  {$ELSE}
      MOV     RCX, RSI
      MOVZX   RAX, DX
  {$ENDIF}

      // clear R8 register, in case we will use it later
      XOR     R8, R8
      MOV     R9, $0001000100010001
      IMUL    RAX, R9

      CMP     RCX, 64
      JBE     @StoreQuads

      TEST    RDI, 7
      JZ      @StoreBlocks

    {
      Destination is not aligned, align it on 8-byte boundary.

      First calculate how many BYTES we need to write to align the address.

      Then write entire quad (count here is above 64, so no problem if we
      write too far), increment the adress by number of bytes previously
      calculated.

      Store the number in R8 for further use and calculate how many whole words
      we have written - decrement count by this number.

      If odd number of bytes was written, then we need to decrement the count
      by one more, ROR the data by 8 bits and also write a remainder byte at
      the complete end of processing.
    }
      MOV     R9, RDI
      AND     R9, 7
      SUB     R9, 8
      NEG     R9

      MOV     qword ptr [RDI], RAX
      ADD     RDI, R9

      MOV     R8, R9
      SHR     R9, 1   // div 2
      SBB     RCX, R9

      TEST    R8, 1
      JZ      @StoreBlocks
      ROR     RAX, 8

@StoreBlocks:

      CMP     RCX, 32
      JB      @StoreQuads
      MOV     RDX, RCX
      SHR     RCX, 5

      MOV     R9, RCX
      SHL     R9, 5
      SUB     RDX, R9

@BlockLoop:
      MOVNTI  qword ptr [RDI], RAX
      MOVNTI  qword ptr [RDI + 8], RAX
      MOVNTI  qword ptr [RDI + 16], RAX
      MOVNTI  qword ptr [RDI + 24], RAX
      MOVNTI  qword ptr [RDI + 32], RAX
      MOVNTI  qword ptr [RDI + 40], RAX
      MOVNTI  qword ptr [RDI + 48], RAX
      MOVNTI  qword ptr [RDI + 56], RAX

      ADD     RDI, 64
      DEC     RCX
      JNZ     @BlockLoop

      MFENCE

      MOV     RCX, RDX

@StoreQuads:

      MOV     RDX, RCX
      SHR     RDX, 2
      JZ      @StoreWords
      XCHG    RCX, RDX

      REP STOSQ

      MOV     RCX, RDX
      AND     RCX, 3

@StoreWords:

      REP STOSW

      // write remainder byte if needed
      TEST    R8, 1
      JZ      @RoutineEnd
      MOV     byte ptr [RDI], AL

@RoutineEnd:
  {$IFDEF Windows}
      POP     RDI
  {$ENDIF}

{$ELSE}

      PUSH  EDI
      CLD

      MOV   EDI, EAX

      AND   ECX, $FFFF
      IMUL  EAX, ECX, $00010001

      MOV   ECX, EDX
      SHR   ECX, 1

      REP STOSD

      MOV   ECX, EDX
      AND   ECX, 1

      REP STOSW

      POP   EDI

{$ENDIF}
end;
{$ELSE}
var
  Buff: PUInt16;
  i:    TMemSize;
begin
If Count > 0 then
  begin
    Buff := @Dst;
    For i := 0 to Pred(Count) do
      begin
        Buff^ := Value;
        Inc(Buff);
      end;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure FillLong(var Dst; Count: TMemSize; Value: UInt32);
{$IFNDEF PurePascal}
asm
{$IFDEF x64}

  {$IFDEF Windows}
      PUSH    RDI

      MOV     RDI, RCX
      MOV     RCX, RDX
      MOV     EAX, R8D  // higher 32bits of RAX are implicitly cleared
  {$ELSE}
      MOV     RCX, RSI
      MOV     EAX, EDX
  {$ENDIF}

      // clear R8 register, in case we will use it later
      XOR     R8, R8
      MOV     R9, $0000000100000001
      IMUL    RAX, R9

      CMP     RCX, 32
      JBE     @StoreQuads

      TEST    RDI, 7
      JZ      @StoreBlocks

      // aligning...
      MOV     R9, RDI
      AND     R9, 7
      SUB     R9, 8
      NEG     R9

      MOV     qword ptr [RDI], RAX
      ADD     RDI, R9

      MOV     R8, R9
      AND     R8, 3   // mod 4
      SHR     R9, 2   // div 4
      SUB     RCX, R9

      TEST    R8, 3
      JZ      @StoreBlocks
      LEA     RDX, [RCX - 1]
      LEA     RCX, [R8 * 8]
      ROR     RAX, CL
      MOV     RCX, RDX

@StoreBlocks:

      CMP     RCX, 16
      JB      @StoreQuads
      MOV     RDX, RCX
      SHR     RCX, 4

      MOV     R9, RCX
      SHL     R9, 4
      SUB     RDX, R9

@BlockLoop:
      MOVNTI  qword ptr [RDI], RAX
      MOVNTI  qword ptr [RDI + 8], RAX
      MOVNTI  qword ptr [RDI + 16], RAX
      MOVNTI  qword ptr [RDI + 24], RAX
      MOVNTI  qword ptr [RDI + 32], RAX
      MOVNTI  qword ptr [RDI + 40], RAX
      MOVNTI  qword ptr [RDI + 48], RAX
      MOVNTI  qword ptr [RDI + 56], RAX

      ADD     RDI, 64
      DEC     RCX
      JNZ     @BlockLoop

      MFENCE

      MOV     RCX, RDX

@StoreQuads:

      MOV     RDX, RCX
      SHR     RDX, 1
      JZ      @StoreLongs
      XCHG    RCX, RDX

      REP STOSQ

      MOV     RCX, RDX
      AND     RCX, 1

@StoreLongs:

      REP STOSD

      // write remainder bytes if needed
      TEST    R8, 3
      JZ      @RoutineEnd
      SUB     R8, 4
      NEG     R8

@ByteLoop:
      MOV     byte ptr [RDI], AL

      ROR     RAX, 8
      INC     RDI
      DEC     R8
      JNZ     @ByteLoop

@RoutineEnd:
  {$IFDEF Windows}
      POP     RDI
  {$ENDIF}

{$ELSE}

      PUSH  EDI
      CLD

      MOV   EDI, EAX      
      MOV   EAX, ECX
      MOV   ECX, EDX

      REP STOSD

      POP   EDI

{$ENDIF}
end;
{$ELSE}
var
  Buff: PUInt32;
  i:    TMemSize;
begin
If Count > 0 then
  begin
    Buff := @Dst;
    For i := 0 to Pred(Count) do
      begin
        Buff^ := Value;
        Inc(Buff);
      end;
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure FillQuad(var Dst; Count: TMemSize; Value: UInt64);
{$IFNDEF PurePascal}
asm
{$IFDEF x64}

{$IFDEF Windows}
    PUSH    RDI

    MOV     RDI, RCX
    MOV     RCX, RDX
    MOV     RAX, R8
{$ELSE}
    MOV     RCX, RSI
    MOV     RAX, RDX
{$ENDIF}

    XOR     R8, R8

    CMP     RCX, 16
    JBE     @StoreQuads

    TEST    RDI, 7
    JZ      @StoreBlocks

    // aligning...
    MOV     R8, RDI
    AND     R8, 7
    LEA     R9, [R8 - 8]
    NEG     R9

    MOV     qword ptr [RDI], RAX
    ADD     RDI, R9

    MOV     RDX, RCX
    LEA     RCX, [R9 * 8]
    ROR     RAX, CL
    LEA     RCX, [RDX - 1]

@StoreBlocks:

    CMP     RCX, 8
    JB      @StoreQuads
    MOV     RDX, RCX
    SHR     RCX, 3

    MOV     R9, RCX
    SHL     R9, 3
    SUB     RDX, R9

@BlockLoop:
    MOVNTI  qword ptr [RDI], RAX
    MOVNTI  qword ptr [RDI + 8], RAX
    MOVNTI  qword ptr [RDI + 16], RAX
    MOVNTI  qword ptr [RDI + 24], RAX
    MOVNTI  qword ptr [RDI + 32], RAX
    MOVNTI  qword ptr [RDI + 40], RAX
    MOVNTI  qword ptr [RDI + 48], RAX
    MOVNTI  qword ptr [RDI + 56], RAX

    ADD     RDI, 64
    DEC     RCX
    JNZ     @BlockLoop

    MFENCE

    MOV     RCX, RDX

@StoreQuads:

    REP STOSQ

    // write remainder bytes if needed
    TEST    R8, 7
    JZ      @RoutineEnd

@ByteLoop:
    MOV     byte ptr [RDI], AL

    ROR     RAX, 8
    INC     RDI
    DEC     R8
    JNZ     @ByteLoop

@RoutineEnd:
{$IFDEF Windows}
    POP     RDI
{$ENDIF}

{$ELSE}
      // value is passed on stack

      PUSH  EBX

      CMP   EDX, 0
      JBE   @RoutineEnd

      MOV   EBX, dword ptr [Value]
      MOV   ECX, dword ptr [Value + 4]

@MainLoop:
      MOV   dword ptr [EAX], EBX
      MOV   dword ptr [EAX + 4], ECX

      ADD   EAX, 8
      DEC   EDX
      JNZ   @MainLoop

@RoutineEnd:
      POP   EBX

{$ENDIF}
end;
{$ELSE}
var
  Buff: PUInt64;
  i:    TMemSize;
begin
If Count > 0 then
  begin
    Buff := @Dst;
    For i := 0 to Pred(Count) do
      begin
        Buff^ := Value;
        Inc(Buff);
      end;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure FillMemory(Mem: Pointer; Size: TMemSize; Value: UInt8);
begin
FillByte(Mem^,Size,Value);
end;

//------------------------------------------------------------------------------

procedure ZeroMemory(Mem: Pointer; Size: TMemSize);
begin
FillByte(Mem^,Size,0);
end;

{-------------------------------------------------------------------------------
================================================================================
                              Memory space copying
================================================================================
-------------------------------------------------------------------------------}

procedure CopyMemory(Dst,Src: Pointer; Size: TMemSize);
begin
MoveMemory(Dst,Src,Size);
end;

//------------------------------------------------------------------------------

procedure MoveMemory(Dst,Src: Pointer; Size: TMemSize);
begin
while Size > TMemSize(High(PtrInt)) do
  begin
    System.Move(Src^,Dst^,High(PtrInt));
    PtrAdvanceVar(Src,High(PtrInt));
    PtrAdvanceVar(Dst,High(PtrInt));
    Size := Size - TMemSize(High(PtrInt));
  end;
If Size > 0 then
  System.Move(Src^,Dst^,PtrInt(Size));
end;

{-------------------------------------------------------------------------------
================================================================================
                                  Memory search
================================================================================
-------------------------------------------------------------------------------}
{-------------------------------------------------------------------------------
    Memory search - auxiliary functions
-------------------------------------------------------------------------------}

Function PrepSearchOpts(LeadingPartialMatch,TrailingPartialMatch: Boolean): TBOSearchOptions;
begin
Result := [];
If LeadingPartialMatch then
  Include(Result,soLeadPartialMatch);
If TrailingPartialMatch then
  Include(Result,soTrailPartialMatch);
end;

{-------------------------------------------------------------------------------
    Memory search - main implementation
-------------------------------------------------------------------------------}

Function FindBytes(const Bytes; Count: TMemSize; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult;

  Function SameBytes(A,B: PByte; Cnt: TMemSize): Boolean;
  begin
    Result := True;
    while Cnt > 0 do
      begin
        If A^ <> B^ then
          begin
            Result := False;
            Break{while...};
          end;
        Dec(Cnt);
        Inc(A);
        Inc(B);
      end;
  end;

var
  BytesWorkPtr:   Pointer;
  BufferWorkPtr:  Pointer;
  BytesRemaining: TMemSize;
  i:              TMemSize;
begin
Position := -1;
Result := srNotFound;
// sanity checks
If Count > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindBytes: Too many bytes to search for.');
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindBytes: Memory buffer too large.');
If (Count > 0) and (Size > 0) then
  case Count of
    // call optimized routines for small data
    1:  Result := FindByte(UInt8(Bytes),Buffer,Size,Position,Options);
    2:  Result := FindWord(UInt16(Bytes),Buffer,Size,Position,Options);
    4:  Result := FindLong(UInt32(Bytes),Buffer,Size,Position,Options);
    8:  Result := FindQuad(UInt64(Bytes),Buffer,Size,Position,Options);
  else
    // general processing, leading partial match search
    BufferWorkPtr := @Buffer;
    // note Count cannot be 0 here, it was chacked earlier
    BytesWorkPtr := PtrAdvance(@Bytes,TMemOffset(Count) - 1);
    If [soLeadPartialMatch,soPartialMatch] * Options <> [] then
      For i := 1 to MemSizeMin(Pred(Count),Size) do
        begin
          If SameBytes(BufferWorkPtr,BytesWorkPtr,i) then
            begin
              Position := TMemOffset(i) - TMemOffset(Count);
              Result := srFoundLeadPartial;
              Exit;
            end
          else Dec(PUInt8(BytesWorkPtr));
        end;
    // whole data search
    BytesRemaining := Size;
    while BytesRemaining >= Count do
      begin
        // do first byte comparison here to avoid unnecessary rapid calls to SameBytes
        If PUInt8(BufferWorkPtr)^ = UInt8(Bytes) then
          If SameBytes(BufferWorkPtr,@Bytes,Count) then
            begin
              Position := Size - BytesRemaining;
              Result := srFound;
              Exit;
            end;
        Inc(PUInt8(BufferWorkPtr));
        Dec(BytesRemaining);
      end;
    // trailing partial match search
    If [soTrailPartialMatch,soPartialMatch] * Options <> [] then
      For i := MemSizeMin(Pred(Count),Size) downto 1 do
        begin
          If SameBytes(BufferWorkPtr,@Bytes,i) then
            begin
              Position := TMemOffset(Size) - TMemOffset(i);
              Result := srFoundTrailPartial;
              Exit;
            end
          else Inc(PUInt8(BufferWorkPtr));
        end;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function FindByte(Value: UInt8; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult;
var
  WorkPtr:        PUInt8;
  BytesRemaining: TMemSize;
begin
Position := -1;
Result := srNotFound;
// ensure we can actually return the offset
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindByte: Memory buffer too large.');
WorkPtr := PUInt8(@Buffer);
BytesRemaining := Size;
// searching for bytes, no need to do partial checks
while BytesRemaining > 0 do
  begin
    If WorkPtr^ = Value then
      begin
        // we have found the value
        Position := Size - BytesRemaining;
        Result := srFound;
        Break{while...};
      end;
    Inc(WorkPtr);
    Dec(BytesRemaining);
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function FindWord(Value: UInt16; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult;
var
  WorkPtr:        PUInt16;
  BytesRemaining: TMemSize;
begin
Position := -1;
Result := srNotFound;
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindWord: Memory buffer too large.');
If Size > 0 then
  begin
    WorkPtr := PUInt16(@Buffer);
    // leading partial match check
    If [soLeadPartialMatch,soPartialMatch] * Options <> [] then
    {$IFDEF ENDIAN_BIG}
      If UInt8(Value) = PUInt8(WorkPtr)^ then
    {$ELSE}
      If UInt8(Value shr 8) = PUInt8(WorkPtr)^ then
    {$ENDIF}
        begin
          Position := -1;
          Result := srFoundLeadPartial;
          Exit;
        end;
    // check the data
    BytesRemaining := Size;
    while BytesRemaining >= SizeOf(Value) do
      begin
        If WorkPtr^ = Value then
          begin
            Position := Size - BytesRemaining;
            Result := srFound;
            Exit;
          end;
        Inc(PUInt8(WorkPtr)); // increment the working pointer only by one
        Dec(BytesRemaining);
      end;
  {
    Trailing partial match check.

    Note that by this point, WorkPtr always points to a byte that is exactly
    SizeOf(Value) - 1 remote from the end of data, so we can directly use it
    to do partial check.
  }
    If [soTrailPartialMatch,soPartialMatch] * Options <> [] then
    {$IFDEF ENDIAN_BIG}
      If UInt8(Value shr 8) = PUInt8(WorkPtr)^ then
    {$ELSE}
      If UInt8(Value) = PUInt8(WorkPtr)^ then
    {$ENDIF}
        begin
          Position := TMemOffset(Size) - 1;
          Result := srFoundTrailPartial;
        end;
  end;
end;

//------------------------------------------------------------------------------

Function FindLong(Value: UInt32; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult;
var
  WorkPtr:        PUInt32;
  BytesRemaining: TMemSize;
  Temp:           UInt32;
  i:              TMemSize;
begin
Position := -1;
Result := srNotFound;
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindLong: Memory buffer too large.');
If Size > 0 then
  begin
    WorkPtr := PUInt32(@Buffer);
    If [soLeadPartialMatch,soPartialMatch] * Options <> [] then
      For i := 1 to MemSizeMin(Pred(SizeOf(UInt32)),Size) do
        begin
          Temp := 0;
          Move(WorkPtr^,Temp,i);
        {$IFDEF ENDIAN_BIG}
          If Temp = UInt32(Value shl (8 * (SizeOf(UInt32) - i))) then
        {$ELSE}
          If Temp = Value shr (8 * (SizeOf(UInt32) - i)) then
        {$ENDIF}
            begin
              Position := TMemOffset(i) - SizeOf(UInt32);
              Result := srFoundLeadPartial;
              Exit;
            end;
        end;
    BytesRemaining := Size;
    while BytesRemaining >= SizeOf(UInt32) do
      begin
        If WorkPtr^ = Value then
          begin
            Position := Size - BytesRemaining;
            Result := srFound;
            Exit;
          end;
        Inc(PUInt8(WorkPtr));
        Dec(BytesRemaining);
      end;
    If [soTrailPartialMatch,soPartialMatch] * Options <> [] then
      For i := MemSizeMin(Pred(SizeOf(UInt32)),Size) downto 1 do
        begin
          Temp := 0;
          Move(WorkPtr^,Temp,i);
        {$IFDEF ENDIAN_BIG}
          If Temp = Value and UInt32(UInt32(-1) shl (8 * (SizeOf(UInt32) - i))) then
        {$ELSE}
          If Temp = Value and {mask}(UInt32(-1) shr (8 * (SizeOf(UInt32) - i))) then
        {$ENDIF}
            begin
              Position := TMemOffset(Size) - TMemOffset(i);
              Result := srFoundTrailPartial;
              Exit;
            end;
          Inc(PUInt8(WorkPtr));
        end;
  end;
end;

//------------------------------------------------------------------------------

Function FindQuad(Value: UInt64; const Buffer; Size: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult;
var
  WorkPtr:        PUInt64;
  BytesRemaining: TMemSize;
  Temp:           UInt64;
  i:              TMemSize;
begin
Position := -1;
Result := srNotFound;
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindQuad: Memory buffer too large.');
If Size > 0 then
  begin
    WorkPtr := PUInt64(@Buffer);  
    If [soLeadPartialMatch,soPartialMatch] * Options <> [] then
      For i := 1 to MemSizeMin(Pred(SizeOf(UInt64)),Size) do
        begin
          Temp := 0;
          Move(WorkPtr^,Temp,i);
        {$IFDEF ENDIAN_BIG}
          If Temp = UInt64(Value shl (8 * (SizeOf(UInt64) - i))) then
        {$ELSE}
          If Temp = Value shr (8 * (SizeOf(UInt64) - i)) then
        {$ENDIF}
            begin
              Position := TMemOffset(i) - SizeOf(UInt64);
              Result := srFoundLeadPartial;
              Exit;
            end;
        end;
    BytesRemaining := Size;
    while BytesRemaining >= SizeOf(UInt64) do
      begin
        If WorkPtr^ = Value then
          begin
            Position := Size - BytesRemaining;
            Result := srFound;
            Exit;
          end;
        Inc(PUInt8(WorkPtr));
        Dec(BytesRemaining);
      end;
    If [soTrailPartialMatch,soPartialMatch] * Options <> [] then
      For i := MemSizeMin(Pred(SizeOf(UInt64)),Size) downto 1 do
        begin
          Temp := 0;
          Move(WorkPtr^,Temp,i);
        {$IFDEF ENDIAN_BIG}
          If Temp = Value and UInt64(UInt64(-1) shl (8 * (SizeOf(UInt64) - i))) then
        {$ELSE}
          If Temp = Value and (UInt64(-1) shr (8 * (SizeOf(UInt64) - i))) then
        {$ENDIF}
            begin
              Position := TMemOffset(Size) - TMemOffset(i);
              Result := srFoundTrailPartial;
              Exit;
            end;
          Inc(PUInt8(WorkPtr));
        end;
  end;
end;

//==============================================================================

Function FindBytes(const Bytes; Count: TMemSize; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
begin
Position := -1;
Result := srNotFound;
{
  This check has to be done here, even if it is later repeated in called Find*.
  This is because later, the Size is already decremented by From, so the check
  might falsely succeed there.
}
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindBytes: Memory buffer too large.');
If Offset < Size then
  begin
    Result := FindBytes(Bytes,Count,PtrAdvance(@Buffer,TMemOffset(Offset))^,Size - Offset,Position,Options);
    If (Result <> srNotFound) and (soAbsolutePosition in Options) then
      Position := Position + TMemOffset(Offset);
  end;
end;

//------------------------------------------------------------------------------

Function FindByte(Value: UInt8; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
begin
Position := -1;
Result := srNotFound;
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindByte: Memory buffer too large.');
If Offset < Size then
  begin
    Result := FindByte(Value,PtrAdvance(@Buffer,TMemOffset(Offset))^,Size - Offset,Position,Options);
    If (Result <> srNotFound) and (soAbsolutePosition in Options) then
      Position := Position + TMemOffset(Offset);
  end;
end;

//------------------------------------------------------------------------------

Function FindWord(Value: UInt16; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
begin
Position := -1;
Result := srNotFound;
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindWord: Memory buffer too large.');
If Offset < Size then
  begin
    Result := FindWord(Value,PtrAdvance(@Buffer,TMemOffset(Offset))^,Size - Offset,Position,Options);
    If (Result <> srNotFound) and (soAbsolutePosition in Options) then
      Position := Position + TMemOffset(Offset);
  end;
end;

//------------------------------------------------------------------------------

Function FindLong(Value: UInt32; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
begin
Position := -1;
Result := srNotFound;
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindLong: Memory buffer too large.');
If Offset < Size then
  begin
    Result := FindLong(Value,PtrAdvance(@Buffer,TMemOffset(Offset))^,Size - Offset,Position,Options);
    If (Result <> srNotFound) and (soAbsolutePosition in Options) then
      Position := Position + TMemOffset(Offset);
  end;
end;

//------------------------------------------------------------------------------

Function FindQuad(Value: UInt64; const Buffer; Size: TMemSize; Offset: TMemSize; out Position: TMemOffset; Options: TBOSearchOptions = []): TBOSearchResult; overload;
begin
Position := -1;
Result := srNotFound;
If Size > TMemSize(High(TMemOffset)) then
  raise EBOInvalidValue.Create('FindQuad: Memory buffer too large.');
If Offset < Size then
  begin
    Result := FindQuad(Value,PtrAdvance(@Buffer,TMemOffset(Offset))^,Size - Offset,Position,Options);
    If (Result <> srNotFound) and (soAbsolutePosition in Options) then
      Position := Position + TMemOffset(Offset);
  end;
end;

//==============================================================================

Function FindBytes(const Bytes; Count: TMemSize; Memory: Pointer; Size: TMemSize; LeadingPartialMatch: Boolean = False; TrailingPartialMatch: Boolean = False): TMemOffset;
begin
If FindBytes(Bytes,Count,Memory^,Size,Result,PrepSearchOpts(LeadingPartialMatch,TrailingPartialMatch)) = srNotFound then
  Result := -TMemOffset(Count); // this is ok, Count is checked for TMemOffset bounds in FindBytes
end;

//------------------------------------------------------------------------------

Function FindByte(Value: UInt8; Memory: Pointer; Size: TMemSize): TMemOffset;
begin
If FindByte(Value,Memory^,Size,Result,[]) = srNotFound then
  Result := -SizeOf(UInt8);
end;

//------------------------------------------------------------------------------

Function FindWord(Value: UInt16; Memory: Pointer; Size: TMemSize; LeadingPartialMatch: Boolean = False; TrailingPartialMatch: Boolean = False): TMemOffset;
begin
If FindWord(Value,Memory^,Size,Result,PrepSearchOpts(LeadingPartialMatch,TrailingPartialMatch)) = srNotFound then
  Result := -SizeOf(UInt16);
end;

//------------------------------------------------------------------------------

Function FindLong(Value: UInt32; Memory: Pointer; Size: TMemSize; LeadingPartialMatch: Boolean = False; TrailingPartialMatch: Boolean = False): TMemOffset;
begin
If FindLong(Value,Memory^,Size,Result,PrepSearchOpts(LeadingPartialMatch,TrailingPartialMatch)) = srNotFound then
  Result := -SizeOf(UInt32);
end;

//------------------------------------------------------------------------------

Function FindQuad(Value: UInt64; Memory: Pointer; Size: TMemSize; LeadingPartialMatch: Boolean = False; TrailingPartialMatch: Boolean = False): TMemOffset;
begin
If FindQuad(Value,Memory^,Size,Result,PrepSearchOpts(LeadingPartialMatch,TrailingPartialMatch)) = srNotFound then
  Result := -SizeOf(UInt64);
end;


{===============================================================================
--------------------------------------------------------------------------------

                                Other operations

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                                Low-level compare
================================================================================
-------------------------------------------------------------------------------}

Function LLDecodeFlags(Flags: UInt16): TBOStatusFlags;
begin
Result := [];
If Flags and BO_FLAG_CARRY <> 0 then
  Include(Result,flCarry);
If Flags and BO_FLAG_PARITY <> 0 then
  Include(Result,flParity);
If Flags and BO_FLAG_AUXCARRY <> 0 then
  Include(Result,flAuxCarry);
If Flags and BO_FLAG_ZERO <> 0 then
  Include(Result,flZero);
If Flags and BO_FLAG_SIGN <> 0 then
  Include(Result,flSign);
If Flags and BO_FLAG_OVERFLOW <> 0 then
  Include(Result,flOverflow);
end;

//------------------------------------------------------------------------------
{$IFNDEF PurePascal}
const
  BO_FLAG_MASK_CMP  = $08D5;

//------------------------------------------------------------------------------

Function LLCompareRaw(A,B: UInt8): UInt16;
{
  Arguments/result register use (64b/32b/16b/8b values):

                    win32 & lin32         win64             lin64
         A           -/EAX/AX/AL      RCX/ECX/CX/CL     RDI/EDI/DI/DIL
         B           -/EDX/DX/DL      RDX/EDX/DX/DL     RSI/ESI/SI/SIL
    Result               AX                AX                AX
}
asm
{$IFDEF x64}
  {$IFDEF Windows}
    CMP     CL, DL
  {$ELSE}
    CMP     DIL, SIL
  {$ENDIF}
    PUSHFQ
    POP     RAX
    AND     RAX, BO_FLAG_MASK_CMP
{$ELSE}
    CMP     AL, DL
    PUSHFD
    POP     EAX
    AND     EAX, BO_FLAG_MASK_CMP
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LLCompareRaw(A,B: UInt16): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    CMP     CX, DX
  {$ELSE}
    CMP     DI, SI
  {$ENDIF}
    PUSHFQ
    POP     RAX
    AND     RAX, BO_FLAG_MASK_CMP
{$ELSE}
    CMP     AX, DX
    PUSHFD
    POP     EAX
    AND     EAX, BO_FLAG_MASK_CMP
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LLCompareRaw(A,B: UInt32): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    CMP     ECX, EDX
  {$ELSE}
    CMP     EDI, ESI
  {$ENDIF}
    PUSHFQ
    POP     RAX
    AND     RAX, BO_FLAG_MASK_CMP
{$ELSE}
    CMP     EAX, EDX
    PUSHFD
    POP     EAX
    AND     EAX, BO_FLAG_MASK_CMP
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF x64}
Function LLCompareRaw(A,B: UInt64): UInt16;
asm
{$IFDEF Windows}
    CMP     RCX, RDX
{$ELSE}
    CMP     RDI, RSI
{$ENDIF}
    PUSHFQ
    POP     RAX
    AND     RAX, BO_FLAG_MASK_CMP
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function LLCompare(A,B: UInt8): TBOStatusFlags;
begin
Result := LLDecodeFlags(LLCompareRaw(A,B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LLCompare(A,B: UInt16): TBOStatusFlags;
begin
Result := LLDecodeFlags(LLCompareRaw(A,B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LLCompare(A,B: UInt32): TBOStatusFlags;
begin
Result := LLDecodeFlags(LLCompareRaw(A,B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF x64}
Function LLCompare(A,B: UInt64): TBOStatusFlags;
begin
Result := LLDecodeFlags(LLCompareRaw(A,B));
end;
{$ENDIF}

{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                                 Low-level test
================================================================================
-------------------------------------------------------------------------------}
{$IFNDEF PurePascal}
const
  BO_FLAG_MASK_TEST = $00C4;

//------------------------------------------------------------------------------

Function LLTestRaw(A,B: UInt8): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    TEST    CL, DL
  {$ELSE}
    TEST    DIL, SIL
  {$ENDIF}
    PUSHFQ
    POP     RAX
    AND     RAX, BO_FLAG_MASK_TEST
{$ELSE}
    TEST    AL, DL
    PUSHFD
    POP     EAX
    AND     EAX, BO_FLAG_MASK_TEST
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LLTestRaw(A,B: UInt16): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    TEST    CX, DX
  {$ELSE}
    TEST    DI, SI
  {$ENDIF}
    PUSHFQ
    POP     RAX
    AND     RAX, BO_FLAG_MASK_TEST
{$ELSE}
    TEST    AX, DX
    PUSHFD
    POP     EAX
    AND     EAX, BO_FLAG_MASK_TEST
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LLTestRaw(A,B: UInt32): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    TEST    ECX, EDX
  {$ELSE}
    TEST    EDI, ESI
  {$ENDIF}
    PUSHFQ
    POP     RAX
    AND     RAX, BO_FLAG_MASK_TEST
{$ELSE}
    TEST    EAX, EDX
    PUSHFD
    POP     EAX
    AND     EAX, BO_FLAG_MASK_TEST
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF x64}
Function LLTestRaw(A,B: UInt64): UInt16;
asm
{$IFDEF Windows}
    TEST    RCX, RDX
{$ELSE}
    TEST    RDI, RSI
{$ENDIF}
    PUSHFQ
    POP     RAX
    AND     RAX, BO_FLAG_MASK_TEST
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function LLTest(A,B: UInt8): TBOStatusFlags;
begin
Result := LLDecodeFlags(LLTestRaw(A,B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LLTest(A,B: UInt16): TBOStatusFlags;
begin
Result := LLDecodeFlags(LLTestRaw(A,B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LLTest(A,B: UInt32): TBOStatusFlags;
begin
Result := LLDecodeFlags(LLTestRaw(A,B));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF x64}
Function LLTest(A,B: UInt64): TBOStatusFlags;
begin
Result := LLDecodeFlags(LLTestRaw(A,B));
end;
{$ENDIF}

{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------

                                      UIM

--------------------------------------------------------------------------------
===============================================================================}

{-------------------------------------------------------------------------------
================================================================================
                         Unit implementation management
================================================================================
-------------------------------------------------------------------------------}
var
  varImplManager: TImplementationManager = nil;

//------------------------------------------------------------------------------

{$IFNDEF ASM_Extensions}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
Function UIM_CheckASMSupport(Func: TUIM_BitOps_Function): Boolean;
begin
Result := False;
{$IFDEF ASM_Extensions}
with TSimpleCPUID.Create do
try
  case Func of
    fnPopCount8,fnPopCount16,fnPopCount32,fnPopCount64:
      Result := Info.SupportedExtensions.POPCNT;
    fnLZCount8,fnLZCount16,fnLZCount32,fnLZCount64:
      Result := Info.ExtendedProcessorFeatures.LZCNT;
    fnTZCount8,fnTZCount16,fnTZCount32,fnTZCount64,
    fnExtractBits8,fnExtractBits16,fnExtractBits32,fnExtractBits64:
      Result := Info.ProcessorFeatures.BMI1;
    fnParallelBitsExtract8,fnParallelBitsExtract16,fnParallelBitsExtract32,
    fnParallelBitsDeposit8,fnParallelBitsDeposit16,fnParallelBitsDeposit32:
      Result := Info.ProcessorFeatures.BMI2;
    fnParallelBitsExtract64:
      Result := Info.ProcessorFeatures.BMI2{$IFNDEF x64} and Info.SupportedExtensions.POPCNT{$ENDIF};
    fnParallelBitsDeposit64:
      Result := Info.ProcessorFeatures.BMI2{$IFNDEF x64} and Info.SupportedExtensions.POPCNT and Info.ProcessorFeatures.CMOV{$ENDIF};
  else
    raise EBOUnknownFunction.CreateFmt('UIM_CheckASMSupport: Unknown function (%d).',[Ord(Func)]);
  end;
finally
  Free;
end;
{$ENDIF}
end;
{$IFNDEF ASM_Extensions}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//==============================================================================

Function UIM_BitOps_AvailableFuncImpl(Func: TUIM_BitOps_Function): TUIM_BitOps_Implementations;
begin
case Func of
  fnPopCount8,fnPopCount16,fnPopCount32,fnPopCount64,
  fnLZCount8,fnLZCount16,fnLZCount32,fnLZCount64,
  fnTZCount8,fnTZCount16,fnTZCount32,fnTZCount64,
  fnExtractBits8,fnExtractBits16,fnExtractBits32,fnExtractBits64,
  fnParallelBitsExtract8,fnParallelBitsExtract16,fnParallelBitsExtract32,fnParallelBitsExtract64,
  fnParallelBitsDeposit8,fnParallelBitsDeposit16,fnParallelBitsDeposit32,fnParallelBitsDeposit64:
    Result := [imNone,imPascal{$IFDEF ASM_Extensions},imAssembly{$ENDIF}];
else
  raise EBOUnknownFunction.CreateFmt('UIM_BitOps_AvailableFuncImpl: Unknown function (%d).',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

Function UIM_BitOps_SupportedFuncImpl(Func: TUIM_BitOps_Function): TUIM_BitOps_Implementations;
begin
Result := [imNone,imPascal];
case Func of
  fnPopCount8,fnPopCount16,fnPopCount32,fnPopCount64,
  fnLZCount8,fnLZCount16,fnLZCount32,fnLZCount64,
  fnTZCount8,fnTZCount16,fnTZCount32,fnTZCount64,
  fnExtractBits8,fnExtractBits16,fnExtractBits32,fnExtractBits64,
  fnParallelBitsExtract8,fnParallelBitsExtract16,fnParallelBitsExtract32,fnParallelBitsExtract64,
  fnParallelBitsDeposit8,fnParallelBitsDeposit16,fnParallelBitsDeposit32,fnParallelBitsDeposit64:
    If UIM_CheckASMSupport(Func) then
      Include(Result,imAssembly);
else
  raise EBOUnknownFunction.CreateFmt('UIM_BitOps_SupportedFuncImpl: Unknown function (%d).',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

Function UIM_BitOps_GetFuncImpl(Func: TUIM_BitOps_Function): TUIM_BitOps_Implementation;
var
  SelectedImplID: TUIMIdentifier;
begin
If varImplManager.RoutingFindObj(TUIMIdentifier(Func)).Selected(SelectedImplID) then
  Result := TUIM_BitOps_Implementation(SelectedImplID)
else
  raise EBONoImplementation.Create('UIM_BitOps_GetFuncImpl: No implementation selected.');
end;

//------------------------------------------------------------------------------

Function UIM_BitOps_SetFuncImpl(Func: TUIM_BitOps_Function; NewImpl: TUIM_BitOps_Implementation): TUIM_BitOps_Implementation;
begin
Result := UIM_BitOps_GetFuncImpl(Func);
varImplManager.RoutingFindObj(TUIMIdentifier(Func)).Select(TUIMIdentifier(NewImpl));
end;

{-------------------------------------------------------------------------------
================================================================================
                               Unit initialization
================================================================================
-------------------------------------------------------------------------------}

procedure UnitInitialize;
const
  NilPtr:   Pointer = nil;
  ImplsVar: array[TUIM_BitOps_Function] of PPointer = (
    @@Var_PopCount_8,@@Var_PopCount_16,@@Var_PopCount_32,@@Var_PopCount_64,
    @@Var_LZCount_8,@@Var_LZCount_16,@@Var_LZCount_32,@@Var_LZCount_64,
    @@Var_TZCount_8,@@Var_TZCount_16,@@Var_TZCount_32,@@Var_TZCount_64,
    @@Var_ExtractBits_8,@@Var_ExtractBits_16,@@Var_ExtractBits_32,@@Var_ExtractBits_64,
    @@Var_ParallelBitsExtract_8,@@Var_ParallelBitsExtract_16,@@Var_ParallelBitsExtract_32,@@Var_ParallelBitsExtract_64,
    @@Var_ParallelBitsDeposit_8,@@Var_ParallelBitsDeposit_16,@@Var_ParallelBitsDeposit_32,@@Var_ParallelBitsDeposit_64);
  ImplsPas: array[TUIM_BitOps_Function] of Pointer = (
    @Fce_PopCount_8_Pas,@Fce_PopCount_16_Pas,@Fce_PopCount_32_Pas,@Fce_PopCount_64_Pas,
    @Fce_LZCount_8_Pas,@Fce_LZCount_16_Pas,@Fce_LZCount_32_Pas,@Fce_LZCount_64_Pas,
    @Fce_TZCount_8_Pas,@Fce_TZCount_16_Pas,@Fce_TZCount_32_Pas,@Fce_TZCount_64_Pas,
    @Fce_ExtractBits_8_Pas,@Fce_ExtractBits_16_Pas,@Fce_ExtractBits_32_Pas,@Fce_ExtractBits_64_Pas,
    @Fce_ParallelBitsExtract_8_Pas,@Fce_ParallelBitsExtract_16_Pas,@Fce_ParallelBitsExtract_32_Pas,@Fce_ParallelBitsExtract_64_Pas,
    @Fce_ParallelBitsDeposit_8_Pas,@Fce_ParallelBitsDeposit_16_Pas,@Fce_ParallelBitsDeposit_32_Pas,@Fce_ParallelBitsDeposit_64_Pas);
{$IFDEF ASM_Extensions}
  ImplsAsm: array[TUIM_BitOps_Function] of Pointer = (
    @Fce_PopCount_8_Asm,@Fce_PopCount_16_Asm,@Fce_PopCount_32_Asm,@Fce_PopCount_64_Asm,
    @Fce_LZCount_8_Asm,@Fce_LZCount_16_Asm,@Fce_LZCount_32_Asm,@Fce_LZCount_64_Asm,
    @Fce_TZCount_8_Asm,@Fce_TZCount_16_Asm,@Fce_TZCount_32_Asm,@Fce_TZCount_64_Asm,
    @Fce_ExtractBits_8_Asm,@Fce_ExtractBits_16_Asm,@Fce_ExtractBits_32_Asm,@Fce_ExtractBits_64_Asm,
    @Fce_ParallelBitsExtract_8_Asm,@Fce_ParallelBitsExtract_16_Asm,@Fce_ParallelBitsExtract_32_Asm,@Fce_ParallelBitsExtract_64_Asm,
    @Fce_ParallelBitsDeposit_8_Asm,@Fce_ParallelBitsDeposit_16_Asm,@Fce_ParallelBitsDeposit_32_Asm,@Fce_ParallelBitsDeposit_64_Asm);
{$ENDIF}
var
  i:  TUIM_BitOps_Function;
begin
varImplManager := TImplementationManager.Create;
For i := Low(TUIM_BitOps_Function) to High(TUIM_BitOps_Function) do
  begin
    with varImplManager.RoutingAddObj(TUIMIdentifier(i),ImplsVar[i]^) do
      begin
        Add(TUIMIdentifier(imNone),NilPtr);
        Add(TUIMIdentifier(imPascal),ImplsPas[i],[ifSelect]);
      {$IFDEF ASM_Extensions}
        Add(TUIMIdentifier(imAssembly),ImplsAsm[i]);
      {$ELSE}
        AddAlias(TUIMIdentifier(imPascal),TUIMIdentifier(imAssembly));
      {$ENDIF}
      end;
    If UIM_CheckASMSupport(i) then
      UIM_BitOps_SetFuncImpl(i,imAssembly)
  end;
{$IFNDEF PurePascal}
with TSimpleCPUID.Create do
try
{$IFDEF x64}
  If not Info.SupportedExtensions.SSE2 then
    raise EBOUnsupportedPlatform.Create('UnitInitialize: SSE2 extension is required for x86-64 system.');
{$ENDIF}
finally
  Free;
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure UnitFinalize;
begin
FreeAndNil(varImplManager);
end;

//------------------------------------------------------------------------------

initialization
  UnitInitialize;

finalization
  UnitFinalize;

end.
