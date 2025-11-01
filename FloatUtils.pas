{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  FloatUtils

    Main purpose of this library is to provide some general utilities for
    work with floating point numbers. It combines functionality from libraries
    Float80Utils and Float16Utils (which are now deprecated) and adds some
    new things.

    Currently, four floating point types are supported (all conforming to
    IEEE 754 standard) - 16bit half precision (binary16), 32bit single
    precision (binary32), 64bit double precision (binary64) and 80bit double
    extended precision floating point numbers. There are no plans for more
    types, but if there will be any demand, support for real48, float128,
    float256 and bfloat16 might be added.
    For supported types, it provides explicit conversions to and from closest
    generally supported types (half <-> single, single <-> double, double <->
    extended), encoding and decoding (extraction of and building from number
    parts - sign, exponent and mantissa(significand)/fraction), casting to and
    from integral types or structures, meta-information (non-numeric info)
    about given numbers (eg. whether it is inifinite, denormal, ...), sign
    manipulation (not invoking any FPU hardware), direct access to floating
    point hardware (x87 and SSE/AVX - eg. selecting rounding mode, exceptions
    masking and more), for float16 some basic arithmetics (see its declaration
    for more details) and last but not least a number of constants (eg. special
    values).

    Conversions might seem to be pointless, especially for single <-> double,
    but they are here to provide working implementation on systems that does
    not support given types (this goes mainly for float16 and float80 on Win64)
    or when fine control over the conversion is required.

      NOTE - Conversions to and from half precision floating point numbers
             still need some testing (mainly exceptions raising).

  Version 1.0 (2025-11-01)

  Last change 2025-11-01

  ©2025 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.FloatUtils

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    BasicUIM      - github.com/TheLazyTomcat/Lib.BasicUIM
  * SimpleCPUID   - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils   - github.com/TheLazyTomcat/Lib.UInt64Utils

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol FloatUtils_UseAuxExceptions for details).

  Library SimpleCPUID is required only when PurePascal symbol is not defined.

  Libraries AuxExceptions and SimpleCPUID might also be required as an indirect
  dependencies.

  Indirect dependencies:
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit FloatUtils;
{
  FloatUtils_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$IFDEF FloatUtils_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{
  FloatUtils_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  FloatUtils_UseAuxExceptions to achieve this.
}
{$IF Defined(FloatUtils_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IFDEF ENDIAN_BIG}
  {$MESSAGE FATAL 'Big-endian architectures not supported.'}
{$ENDIF}

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

//------------------------------------------------------------------------------
// do not touch following
{$IF Defined(CanInline) and Defined(FPC)}
  {$DEFINE CanInlineFPC}
{$ELSE}
  {$UNDEF CanInlineFPC}
{$IFEND}

interface

uses
  SysUtils,
  AuxTypes
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
--------------------------------------------------------------------------------
                           Library-specific exceptions
--------------------------------------------------------------------------------
===============================================================================}
type
  EFUException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EFUUnsupportedOp = class(EFUException);
  EFUInvalidFlag   = class(EFUException);
  EFUInvalidValue  = class(EFUException);
  EFUInvalidState  = class(EFUException);
  EFUStateMismatch = class(EFUException);

{
  EFUEmulationException is a common class for exceptions raised in code that is
  emulating behavior of x87 FPU or SSE/AVX unit during conversions of floating
  point numbers.

    WARNING - do NOT instantiate (create intances/objects of) this class or any
              of its descendants in code outside of this library. These classes
              interact with this unit in a way that is not publicly documented
              and can do changes you might not expect.
}
  EFUEmulationException = class(EFUException)
  protected
    class Function GetClassErrorMessage: String; virtual;
    procedure Initialize; virtual; abstract;
  public
    constructor Create;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                           Constants and helper types
--------------------------------------------------------------------------------
===============================================================================}
(*
  Following constants can be used to discern whether the type Extended is
  declared as 80bit float, or it is merely an alias for 64bit float (double),
  which is the case in 64bit Windows applications.

  Since they are declared as true constants, they can be used in conditional
  compilation - several forms are provided to allow for different styles of
  testing, for exmple:

      {$IF ExtendedIs80bits}...{$IFEND}

      {$IF ExtendedIs80bitsN <> 0}...{$IFEND}

      {$IF Declared(ExtendedIs80bitsE)}...{$IFEND}

  But note that you can still use good old in-place test of the form...

      {$IF SizeOf(Extended) = 10}
*)
const
  ExtendedIs80bits = {$IF SizeOf(Extended) = 10}True{$ELSE}False{$IFEND};
  ExtendedIs80bitsN = {$IF SizeOf(Extended) = 10}1{$ELSE}0{$IFEND};
{$IF SizeOf(Extended) = 10}
  ExtendedIs80bitsE = True;
{$IFEND}

  ExtendedIs64bits = {$IF SizeOf(Extended) = 8}True{$ELSE}False{$IFEND};
  ExtendedIs64bitsN = {$IF SizeOf(Extended) = 8}1{$ELSE}0{$IFEND};
{$IF SizeOf(Extended) = 8}
  ExtendedIs64bitsE = True;
{$IFEND}

// sanity/paranoia check
{$IF (SizeOf(Extended) <> 8) and (SizeOf(Extended) <> 10)}
  {$MESSAGE FATAL 'Unsupported size of type Extended.'}
{$IFEND}

//------------------------------------------------------------------------------
{
  TFloat80Overlay

  Use this type as an overlay for double extended (80bit/10byte) floats to
  access their individual parts (the float80 cannot be overlayed by any
  existing simple integral type).

  Field Mantissa (also known as significand) includes both integer bit (bit 63)
  and a fraction (bits 0..62).

  SignExponent overlays sign (bit 15) and an exponent (bits 0..14).
}
type
  TFloat80Overlay = packed record
    case Integer of
      0:  (Mantissa:      UInt64;
           SignExponent:  UInt16);
      1:  (Part64:        UInt64;
           Part16:        UInt16);
      2:  (Words:         array[0..4] of UInt16);
      3:  (Bytes:         array[0..9] of UInt8);
  end;
  PFloat80Overlay = ^TFloat80Overlay;

//------------------------------------------------------------------------------
{
  Exponents in floating point values are stored biased. To get true value of
  exponent, you need to subtract exponent bias from its stored value (note the
  true exponent can be negative). Following constants give values of exponent
  bias for different floating point types (widths).

    WARNING - some values of biased exponent (zero, maximum value) have
              special meaning and cannot be used to calculate true unbiased
              exponent.
}
const
  FLOAT16_EXPONENTBIAS = 15;
  FLOAT32_EXPONENTBIAS = 127;
  FLOAT64_EXPONENTBIAS = 1023;
  FLOAT80_EXPONENTBIAS = 16383;

{
  Following are minimum and maximum values of exponent for different floating
  point types. Both unbiased (*_EXPONENT*) and biased (*_BEXPONENT*) forms are
  provided.

  Note that these extreme values cannot be used to encode a normalized number,
  only values of (MIN + 1) and (MAX - 1) can be used for that purpose. Within
  the floating point format, min and max exponents are used to encode special
  values (zero, non-numbers, denormals, infinities, ...).
}
  FLOAT16_EXPONENTMIN  = -15; // unbiased min
  FLOAT16_EXPONENTMAX  = 16;  // unbiased max
  FLOAT16_BEXPONENTMIN = 0;   // biased min (this is always 0)
  FLOAT16_BEXPONENTMAX = 31;  // biased max

  FLOAT32_EXPONENTMIN  = -127;
  FLOAT32_EXPONENTMAX  = 128;
  FLOAT32_BEXPONENTMIN = 0;
  FLOAT32_BEXPONENTMAX = 255;

  FLOAT64_EXPONENTMIN  = -1023;
  FLOAT64_EXPONENTMAX  = 1024;
  FLOAT64_BEXPONENTMIN = 0;
  FLOAT64_BEXPONENTMAX = 2047;

  FLOAT80_EXPONENTMIN = -16383;
  FLOAT80_EXPONENTMAX = 16384;
  FLOAT80_BEXPONENTMIN = 0;
  FLOAT80_BEXPONENTMAX = 32767;

//------------------------------------------------------------------------------
{
  Following set of constants is to be used to extract or set individual parts
  of floating point number (mantissa, exponent, sign, ...).

  Since floats cannot be directly combined with bitmasks or bitshifted, it is
  necessary to first convert or overlay them to integral types or structures.

  Shifts are meant for situations where you want the masked part to be shifted
  to the low bits, eg. to ascertain its numerical value.

    NOTE - 16, 32 and 64bit floats do not store integer bit, it is implied
           there. Provided masks and shifts are for virtual position of that
           bit, meaning it would be there if it was explicit.
           They can be used on mantissa returned from decoding functions (eg.
           DecodeSingle, DecodeFloat64, ...), as they can provide the integer
           bit if so instructed.
}
const
  FLOAT16_MASK_SIGN = UInt16($8000);  // sign bit
  FLOAT16_MASK_EXP  = UInt16($7C00);  // exponent
  FLOAT16_MASK_FRAC = UInt16($03FF);  // fractional part of mantissa
  FLOAT16_MASK_FHB  = UInt16($0200);  // highest bit of the fraction
  FLOAT16_MASK_INTB = UInt16($0400);  // virtual position of implicit mantissa integer bit

  FLOAT16_SHIFT_SIGN = 15;
  FLOAT16_SHIFT_EXP  = 10;
  FLOAT16_SHIFT_FHB  = 9;
  FLOAT16_SHIFT_INTB = 10;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  FLOAT32_MASK_SIGN = UInt32($80000000);
  FLOAT32_MASK_EXP  = UInt32($7F800000);
  FLOAT32_MASK_FRAC = UInt32($007FFFFF);
  FLOAT32_MASK_FHB  = UInt32($00400000);
  FLOAT32_MASK_INTB = UInt32($00800000);

  FLOAT32_SHIFT_SIGN = 31;
  FLOAT32_SHIFT_EXP  = 23;
  FLOAT32_SHIFT_FHB  = 22;
  FLOAT32_SHIFT_INTB = 23;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  FLOAT64_MASK_SIGN = UInt64($8000000000000000);
  FLOAT64_MASK_EXP  = UInt64($7FF0000000000000);
  FLOAT64_MASK_FRAC = UInt64($000FFFFFFFFFFFFF);
  FLOAT64_MASK_FHB  = UInt64($0008000000000000);
  FLOAT64_MASK_INTB = UInt64($0010000000000000);

  FLOAT64_SHIFT_SIGN = 63;
  FLOAT64_SHIFT_EXP  = 52;
  FLOAT64_SHIFT_FHB  = 51;
  FLOAT64_SHIFT_INTB = 52;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{
  Since float80 cannot be directly overlayed by any simple integral type, there
  is a record declared for that purpose (TFloat80Overlay). Following masks and
  shifts are meant to be used on fields of that structure - MASK16 and SHIFT16
  on field Part16 (or SignExponent), MASK64 and SHIFT64 on Part64 (Mantissa).

  Also note that float80, unlike other floating point types, explicitly stores
  integer bit of the mantissa.
}
  FLOAT80_MASK16_SIGN = UInt16($8000);
  FLOAT80_MASK16_EXP  = UInt16($7FFF);
  FLOAT80_MASK64_FRAC = UInt64($7FFFFFFFFFFFFFFF);
  FLOAT80_MASK64_FHB  = UInt64($4000000000000000);
  FLOAT80_MASK64_INTB = UInt64($8000000000000000);  // explicit mantissa integer bit

  FLOAT80_SHIFT16_SIGN = 15;
  FLOAT80_SHIFT16_EXP  = 0;
  FLOAT80_SHIFT64_FHB  = 62;
  FLOAT80_SHIFT64_INTB = 63;

{-------------------------------------------------------------------------------
    Special floating point values
-------------------------------------------------------------------------------}
{
  Following "constants" were copied form library AuxMath, see there for more
  details and description of why it is implemented the way it is.

  You can find the AuxMath library in this repositiory:

      github.com/TheLazyTomcat/Lib.AuxMath
}
const
  Float16Min:         Float16 = ($01,$00);  // 5.96046e-8
  Float16Max:         Float16 = ($FF,$7B);  // 65504
  Float16MinNormal:   Float16 = ($00,$04);  // 6.10351562500000e-5 (lowest possible normalized value)
  Float16MaxDenormal: Float16 = ($FF,$03);  // 6.09755516052246e-5 (highest possible denormalized value)
  Float16QNaN:        Float16 = ($FF,$7F);  // quiet NaN
  Float16SNaN:        Float16 = ($FF,$7D);  // signaled NaN
  Float16NaN:         Float16 = ($FF,$7F);  // quiet NaN
  Float16Infinity:    Float16 = ($00,$7C);  // positive infinity
  Float16Zero:        Float16 = ($00,$00);  // (+)0
  Float16One:         Float16 = ($00,$3C);  // +1.0
  Float16Indefinite:  Float16 = ($00,$FE);  // indefinite quiet NaN

  MinFloat16: Float16 = ($01,$00);  // Float16Min
  MaxFloat16: Float16 = ($FF,$7B);  // Float16Max

  HalfMin:          Half = ($01,$00); // 5.96046e-8
  HalfMax:          Half = ($FF,$7B); // 65504
  HalfMinNormal:    Half = ($00,$04); // 6.10351562500000e-5
  HalfMaxDenormal:  Half = ($FF,$03); // 6.09755516052246e-5
  HalfQNaN:         Half = ($FF,$7F); // quiet NaN
  HalfSNaN:         Half = ($FF,$7D); // signaled NaN
  HalfNaN:          Half = ($FF,$7F); // quiet NaN
  HalfInfinity:     Half = ($00,$7C); // positive infinity
  HalfZero:         Half = ($00,$00); // (+)0
  HalfOne:          Half = ($00,$3C); // +1.0
  HalfIndefinite:   Half = ($00,$FE); // indefinite quiet NaN

  MinHalf: Half = ($01,$00); // HalfMin
  MaxHalf: Half = ($FF,$7B); // HalfMax

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const
  iFloat32Min:          UInt32 = $00000001; // 1.40129846432482e-45
  iFloat32Max:          UInt32 = $7F7FFFFF; // 3.40282346638529e+38
  iFloat32MinNormal:    UInt32 = $00800000; // 1.17549435082229e-38
  iFloat32MaxDenormal:  UInt32 = $007FFFFF; // 1.17549421069244e-38
  iFloat32QNaN:         UInt32 = $7FFFFFFF; // quiet NaN
  iFloat32SNaN:         UInt32 = $7FBFFFFF; // signaled NaN
  iFloat32NaN:          UInt32 = $7FFFFFFF; // quiet NaN
  iFloat32Infinity:     UInt32 = $7F800000; // positive infinity
  iFloat32Indefinite:   UInt32 = $FFC00000; // indefinite quiet NaN

var
  Float32Min:         Float32 absolute iFloat32Min;
  Float32Max:         Float32 absolute iFloat32Max;
  Float32MinNormal:   Float32 absolute iFloat32MinNormal;
  Float32MaxDenormal: Float32 absolute iFloat32MaxDenormal;
  Float32QNaN:        Float32 absolute iFloat32QNaN;
  Float32SNaN:        Float32 absolute iFloat32SNaN;
  Float32NaN:         Float32 absolute iFloat32NaN;
  Float32Infinity:    Float32 absolute iFloat32Infinity;
  Float32Indefinite:  Float32 absolute iFloat32Indefinite;

  SingleMin:          Single absolute iFloat32Min;
  SingleMax:          Single absolute iFloat32Max;
  SingleMinNormal:    Single absolute iFloat32MinNormal;
  SingleMaxDenormal:  Single absolute iFloat32MaxDenormal;
  SingleQNaN:         Single absolute iFloat32QNaN;
  SingleSNaN:         Single absolute iFloat32SNaN;
  SingleNaN:          Single absolute iFloat32NaN;
  SingleInfinity:     Single absolute iFloat32Infinity;
  SingleIndefinite:   Single absolute iFloat32Indefinite;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const
  iFloat64Min:          UInt64 = UInt64($0000000000000001); // 4.94065645841247e-324
  iFloat64Max:          UInt64 = UInt64($7FEFFFFFFFFFFFFF); // 1.79769313486232e+308
  iFloat64MinNormal:    UInt64 = UInt64($0010000000000000); // 2.2250738585072014e-308
  iFloat64MaxDenormal:  UInt64 = UInt64($000FFFFFFFFFFFFF); // 2.2250738585072009e-308
  iFloat64QNaN:         UInt64 = UInt64($7FFFFFFFFFFFFFFF); // quiet NaN
  iFloat64SNaN:         UInt64 = UInt64($7FF7FFFFFFFFFFFF); // signaled NaN
  iFloat64NaN:          UInt64 = UInt64($7FFFFFFFFFFFFFFF); // quiet NaN
  iFloat64Infinity:     UInt64 = UInt64($7FF0000000000000); // positive infinity
  iFloat64Indefinite:   UInt64 = UInt64($FFF8000000000000); // indefinite quiet NaN

var
  Float64Min:         Float64 absolute iFloat64Min;
  Float64Max:         Float64 absolute iFloat64Max;
  Float64MinNormal:   Float64 absolute iFloat64MinNormal;
  Float64MaxDenormal: Float64 absolute iFloat64MaxDenormal;
  Float64QNaN:        Float64 absolute iFloat64QNaN;
  Float64SNaN:        Float64 absolute iFloat64SNaN;
  Float64NaN:         Float64 absolute iFloat64NaN;
  Float64Infinity:    Float64 absolute iFloat64Infinity;
  Float64Indefinite:  Float64 absolute iFloat64Indefinite;

  DoubleMin:          Double absolute iFloat64Min;
  DoubleMax:          Double absolute iFloat64Max;
  DoubleMinNormal:    Double absolute iFloat64MinNormal;
  DoubleMaxDenormal:  Double absolute iFloat64MaxDenormal;
  DoubleQNaN:         Double absolute iFloat64QNaN;
  DoubleSNaN:         Double absolute iFloat64SNaN;
  DoubleNaN:          Double absolute iFloat64NaN;
  DoubleInfinity:     Double absolute iFloat64Infinity;
  DoubleIndefinite:   Double absolute iFloat64Indefinite;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const
  iFloat80Min:          TFloat80Overlay = (Mantissa: UInt64($0000000000000001); SignExponent: $0000); // 3.64519953188247460253e-4951
  iFloat80Max:          TFloat80Overlay = (Mantissa: UInt64($FFFFFFFFFFFFFFFF); SignExponent: $7FFE); // 1.18973149535723176502e+4932
  iFloat80MinNormal:    TFloat80Overlay = (Mantissa: UInt64($8000000000000000); SignExponent: $0001); // 3.36210314311209350626e-4932
  iFloat80MaxDenormal:  TFloat80Overlay = (Mantissa: UInt64($7FFFFFFFFFFFFFFF); SignExponent: $0000); // 3.36210314311209350590E-4932
  iFloat80QNaN:         TFloat80Overlay = (Mantissa: UInt64($FFFFFFFFFFFFFFFF); SignExponent: $7FFF); // quiet NaN
  iFloat80SNaN:         TFloat80Overlay = (Mantissa: UInt64($BFFFFFFFFFFFFFFF); SignExponent: $7FFF); // signaled NaN
  iFloat80NaN:          TFloat80Overlay = (Mantissa: UInt64($FFFFFFFFFFFFFFFF); SignExponent: $7FFF); // quiet NaN
  iFloat80Infinity:     TFloat80Overlay = (Mantissa: UInt64($8000000000000000); SignExponent: $7FFF); // positive infinity
  iFloat80Indefinite:   TFloat80Overlay = (Mantissa: UInt64($C000000000000000); SignExponent: $FFFF); // indefinite quiet NaN

  iFloat80MinPseudoDenormal:  TFloat80Overlay = (Mantissa: UInt64($8000000000000000); SignExponent: $0000); // 3.36210314311209350626e-4932 (lowest possible pseudo-denormal)
  iFloat80MaxPseudoDenormal:  TFloat80Overlay = (Mantissa: UInt64($FFFFFFFFFFFFFFFF); SignExponent: $0000); // 6.72420628622418701216e-4932 (highest possible pseudo-denormal)
  iFloat80MinUnnormal:        TFloat80Overlay = (Mantissa: UInt64($0000000000000000); SignExponent: $0001); // lowest possible unnormal
  iFloat80MaxUnnormal:        TFloat80Overlay = (Mantissa: UInt64($7FFFFFFFFFFFFFFF); SignExponent: $7FFE); // highest possible unnormal
  iFloat80PseudoQNaN:         TFloat80Overlay = (Mantissa: UInt64($7FFFFFFFFFFFFFFF); SignExponent: $7FFF); // quiet pseudo-NaN
  iFloat80PseudoSNaN:         TFloat80Overlay = (Mantissa: UInt64($3FFFFFFFFFFFFFFF); SignExponent: $7FFF); // signaled pseudo-NaN
  iFloat80PseudoNaN:          TFloat80Overlay = (Mantissa: UInt64($7FFFFFFFFFFFFFFF); SignExponent: $7FFF); // quiet pseudo-NaN
  iFloat80PseudoInfinity:     TFloat80Overlay = (Mantissa: UInt64($0000000000000000); SignExponent: $7FFF); // pseudo-infinity

var
  Float80Min:         Float80 absolute iFloat80Min;
  Float80Max:         Float80 absolute iFloat80Max;
  Float80MinNormal:   Float80 absolute iFloat80MinNormal;
  Float80MaxDenormal: Float80 absolute iFloat80MaxDenormal;
  Float80QNaN:        Float80 absolute iFloat80QNaN;
  Float80SNaN:        Float80 absolute iFloat80SNaN;
  Float80NaN:         Float80 absolute iFloat80NaN;
  Float80Infinity:    Float80 absolute iFloat80Infinity;
  Float80Indefinite:  Float80 absolute iFloat80Indefinite;

  Float80MinPseudoDenormal: Float80 absolute iFloat80MinPseudoDenormal;
  Float80MaxPseudoDenormal: Float80 absolute iFloat80MaxPseudoDenormal;
  Float80MinUnnormal:       Float80 absolute iFloat80MinUnnormal;
  Float80MaxUnnormal:       Float80 absolute iFloat80MaxUnnormal;
  Float80PseudoQNaN:        Float80 absolute iFloat80PseudoQNaN;
  Float80PseudoSNaN:        Float80 absolute iFloat80PseudoSNaN;
  Float80PseudoNaN:         Float80 absolute iFloat80PseudoNaN;
  Float80PseudoInfinity:    Float80 absolute iFloat80PseudoInfinity;

{$IF SizeOf(Extended) = 10}
  ExtendedMin:          Extended absolute iFloat80Min;
  ExtendedMax:          Extended absolute iFloat80Max;
  ExtendedMinNormal:    Extended absolute iFloat80MinNormal;
  ExtendedMaxDenormal:  Extended absolute iFloat80MaxDenormal;
  ExtendedQNaN:         Extended absolute iFloat80QNaN;
  ExtendedSNaN:         Extended absolute iFloat80SNaN;
  ExtendedNaN:          Extended absolute iFloat80NaN;
  ExtendedInfinity:     Extended absolute iFloat80Infinity;
  ExtendedIndefinite:   Extended absolute iFloat80Indefinite;

  ExtendedMinPseudoDenormal:  Extended absolute iFloat80MinPseudoDenormal;
  ExtendedMaxPseudoDenormal:  Extended absolute iFloat80MaxPseudoDenormal;
  ExtendedMinUnnormal:        Extended absolute iFloat80MinUnnormal;
  ExtendedMaxUnnormal:        Extended absolute iFloat80MaxUnnormal;
  ExtendedPseudoQNaN:         Extended absolute iFloat80PseudoQNaN;
  ExtendedPseudoSNaN:         Extended absolute iFloat80PseudoSNaN;
  ExtendedPseudoNaN:          Extended absolute iFloat80PseudoNaN;
  ExtendedPseudoInfinity:     Extended absolute iFloat80PseudoInfinity;
{$ELSE}
  ExtendedMin:          Extended absolute iFloat64Min;
  ExtendedMax:          Extended absolute iFloat64Max;
  ExtendedMinNormal:    Extended absolute iFloat64MinNormal;
  ExtendedMaxDenormal:  Extended absolute iFloat64MaxDenormal;
  ExtendedQNaN:         Extended absolute iFloat64QNaN;
  ExtendedSNaN:         Extended absolute iFloat64SNaN;
  ExtendedNaN:          Extended absolute iFloat64NaN;
  ExtendedInfinity:     Extended absolute iFloat64Infinity;
  ExtendedIndefinite:   Extended absolute iFloat64Indefinite;
{$IFEND}

{===============================================================================
--------------------------------------------------------------------------------
           Common types for status, control and exceptions management
--------------------------------------------------------------------------------
===============================================================================}
{
  Common types used in, possibly abstracted, access to x87 FPU and SSE/AVX unit
  states.

  Since both hardware units use the same exceptions and rounding modes, there
  is no point in declaring them separately (and giving the individual enum
  values some contrived prefixes).
}
type
  TFURoundingMode = (rmNearest,rmDown,rmUp,rmTruncate);

  TFUException = (excInvalidOp,excDenormal,excDivByZero,excOverflow,
    excUnderflow,excPrecision,excStackOverflow,excStackUnderflow);

  TFUNumericException = excInvalidOp..excPrecision; // no stack faults

{===============================================================================
--------------------------------------------------------------------------------
                   x87 FPU control and status (CS) management
--------------------------------------------------------------------------------
===============================================================================}
{
  Following constants are here for direct low-level probing or alteration of
  status (X87SW_*) and control (X87CW_*) words as provided by the hardware or
  emulation system (see further).

  For meaning and description of individual bits and fields, please refer to
  documentation of x87 floating point unit provided by Intel or AMD.
}
const
  // status word bitmasks
  X87SW_EFLAG_InvalidOP = UInt16($0001);
  X87SW_EFLAG_Denormal  = UInt16($0002);
  X87SW_EFLAG_DivByZero = UInt16($0004);
  X87SW_EFLAG_Overflow  = UInt16($0008);
  X87SW_EFLAG_Underflow = UInt16($0010);
  X87SW_EFLAG_Precision = UInt16($0020);

  X87SW_EFLAG_All = UInt16($003F);

  X87SW_StackFault       = UInt16($0040);
  X87SW_ExceptionSummary = UInt16($0080);
  X87SW_FPUBusy          = UInt16($8000);

  X87SW_ConditionCode_C0 = UInt16($0100);
  X87SW_ConditionCode_C1 = UInt16($0200);
  X87SW_ConditionCode_C2 = UInt16($0400);
  X87SW_ConditionCode_C3 = UInt16($4000);

  X87SW_TopOfStack = UInt16($3800); // bits 11..13

  X87SW_SHIFT_TopOfStack = 11;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const
  // control word bitmasks
  X87CW_EMASK_InvalidOP = UInt16($0001);
  X87CW_EMASK_Denormal  = UInt16($0002);
  X87CW_EMASK_DivByZero = UInt16($0004);
  X87CW_EMASK_Overflow  = UInt16($0008);
  X87CW_EMASK_Underflow = UInt16($0010);
  X87CW_EMASK_Precision = UInt16($0020);

  X87CW_EMASK_All = UInt16($003F);

  X87CW_InfinityControl = UInt16($1000);

  X87CW_Precision = UInt16($0300);  // bits 8..9
  X87CW_Rounding  = UInt16($0C00);  // bits 10..11

  X87CW_SHIFT_Precision = 8;
  X87CW_SHIFT_Rounding  = 10;

//------------------------------------------------------------------------------
const
{
  X87CW_InitialValue

  Value the x87 FPU control word has after reset or F(N)INIT (see description
  of function F80CEnvironmentInit for more details).
}
  X87CW_InitialValue = $037F;
{
  X87CW_DefaultValue

  Default value for normal use (see F80CControlWordInit).
}
  X87CW_DefaultValue = $1332;

{===============================================================================
    x87 FPU CS management - low-lewel access declaration
===============================================================================}
{
  Low-level access functions are directly accessing control word and status
  word registers of the x87 FPU (as such, they must be implemented in assembly,
  see further). Note that status word is read-only here - there are ways to
  change it, but that is beyond the scope of this library.

  They are all, with notable exception being X87ControlWordInit, subject to
  unit implementation managment (UIM), meaning their implementation can be
  selected (currently between none, pascal and assembly). But considering their
  function (see above), they can only do their work in asm implementation. If
  they are routed to pascal implementation, they will do nothing and will
  always raise an EFUUnsupportedOp exception.
  They are automatically routed to assembly during unit initialization, only
  if x87 is not supported on current system or if this unit is compiled in
  PurePascal mode, they get routed to pascal implementation and therefore
  should not be used. You can discern their current routing using UIM function
  UIM_FloatUtils_GetFuncImpl.
}
{
  X87StatusWordGet

  Returns current value of x87 FPU status word.

  Uses non-waiting instruction, which means pending unmasked FPU exceptions
  are NOT handled.
}
Function X87StatusWordGet: UInt16;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87ControlWordGet

  Returns current value of x87 FPU control word.

  Uses non-waiting instruction, pending unmasked FPU exceptions are NOT
  handled.
}
Function X87ControlWordGet: UInt16;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87ControlWordSet

  Changes value of x87 FPU control word to a passed value (NewValue).

  Pending FPU exceptions that were unmasked prior a call to this function will
  be raised, but the new value will still be stored even in that situation.
  Exceptions unmasked by the new value are not immediately raised. They will be
  raised by the next waiting instruction (you can call X87ExceptionsRaise for
  that purpose).
}
procedure X87ControlWordSet(NewValue: UInt16);{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87EnvironmentInit

  Re-initializes the the entire x87 floating point unit to its initial/default
  state.

  Sets control word to $037F (all exceptions masked), rounding to nearest,
  precision to 64bit, tag word is set to all ones and other environment
  registers are zeroed, including status word.
  Data registers are left unchanged, but as tag word is set to all ones,
  they are all marked as empty.

  Call this function if you want to be sure the unit is properly initialized
  in programs where x87 FPU is not used as a primary mean of floating point
  arithmetics and/or is not automatically initialized - note that it usually
  is, even on systems where it is not used. Generally speaking, you should
  have no need to call this function.

    WARNING - the initialization must be done explicitly for (in) every
              execution thread that is about to use x87 unit.

  It can also be used to repair state of floating point unit after raising an
  exception (this is also usually done automatically by runtime library).

  Each call to this function should be followed by a call to X87ControlWordInit,
  unless you want to manually select non-default CW settings.

  Uses non-waiting instruction, pending unmasked FPU exceptions are NOT
  handled.
}
procedure X87EnvironmentInit;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87ControlWordInit

  Sets x87 control word to a value of $1332 - denormal, underflow and
  precision exceptions are masked (others are unmasked), precision is set to
  extended, rounding is set to nearest and infinity control bit is 1.

  See description of X87EnvironmentInit for more information about when to use
  this call.

    NOTE - this routine is not subject to UIM and therefore its implementation
           cannot be selected - it is only a macro calling X87ControlWordSet.
}
procedure X87ControlWordInit;{$IFDEF CanInline} inline;{$ENDIF}

{
  X87FloatDataGet

  This function will load the entire x87 FPU stack (ie. stored floating point
  values) into provided FloatData structure - x87 has 8 data registers, all
  of them are provided. Each register is also accompanied by its respective
  tag, which can be used to discern nature of the stored number.

  If parameter StackOrder is false (default), then the values are ordered
  according to in which data register they reside - Regs[0] for R0, Regs[1]
  for R1 and so on. When set to true, then the values are in order they appear
  in the fpu stack (see description of x87 FPU in Intel or AMD documentation
  for details about fpu stack) - Regs[0] for ST(0), Regs[1] for ST(1), ...
}
type
  TX87RegisterTag = (rtValid,rtZero,rtSpecial{nan, inf, den, inv},rtEmpty);

  TX87FloatData = record
    Regs:   array[0..7] of record
      Tag:    TX87RegisterTag;
      Data:   packed record case Integer of
        0: (Float:    Float80);
        1: (Overlay:  TFloat80Overlay);
        2: (MMX:      packed record case Integer of // what MMX sees
          0: (Full:     UInt64);
          1: (Bytes:    packed array[0..8] of UInt8);
          2: (Words:    packed array[0..4] of UInt16);
          3: (DWords:   packed array[0..2] of UInt32);
          4: (QWords:   UInt64);
        end);
      end;
    end;
  end;

procedure X87FloatDataGet(out FloatData: TX87FloatData; StackOrder: Boolean = False);

{===============================================================================
    x87 FPU CS management - abstracted access declaration
===============================================================================}
{
  Following types and functions are designed to provide a more convenient
  interface for x87 FPU status and control management by abstracting from it.

  But note that these functions are still using low-level access functions,
  and therefore all in-there mentioned limitations are in effect here too (eg.
  exceptions are raised if assembly implementation cannot be used).
}
type
{
  Order of flags in the TX87StatusFlag enum is retarded (mainly placement of
  sfConditionCodeC1), but there is reason for it, believe me ;)
}
  TX87StatusFlag  = (sfStackFault,sfExceptionSummary,sfConditionCodeC1,sfFPUBusy,
                     sfConditionCodeC0,sfConditionCodeC2,sfConditionCodeC3);
  TX87StatusFlags = set of TX87StatusFlag;

  TX87PrecisionMode = (pmSingle,pmReserved,pmDouble,pmExtended);

  TX87RoundingMode = TFURoundingMode;

  TX87ControlFlag  = (cfInfinityControl);
  TX87ControlFlags = set of TX87ControlFlag;

  TX87Exception  = TFUNumericException;
  TX87Exceptions = set of TX87Exception;

const
  X87ExceptionsAll = [Low(TX87Exception)..High(TX87Exception)];

//------------------------------------------------------------------------------
{
  X87TopOfStackGet

  Returns current top of the stack pointer from the x87 FPU status word.
}
Function X87TopOfStackGet: Integer;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87StatusFlagGet

  Returns current value of selected flag in the x87 FPU status word.
}
Function X87StatusFlagGet(Flag: TX87StatusFlag): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87StatusFlagsGet

  Returns status of all recognized flags in the x87 FPU status word. When the
  flag is set, it is included in the result, when it is clear, it is excluded.
}
Function X87StatusFlagsGet: TX87StatusFlags;{$IFDEF CanInlineFPC} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  X87PrecisionModeGet

  Returns current value of x87 FPU precision mode.
}
Function X87PrecisionModeGet: TX87PrecisionMode;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87PrecisionModeSet

  Sets x87 FPU precision mode to a selected NewValue and returns its previous
  value.
}
Function X87PrecisionModeSet(NewValue: TX87PrecisionMode): TX87PrecisionMode;

{
  X87RoundingModeGet

  Returns current value of x87 PFU rounding mode.
}
Function X87RoundingModeGet: TX87RoundingMode;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87RoundingModeSet

  Sets x87 FPU rounding mode to a selected NewValue and returns its previous
  value.
}
Function X87RoundingModeSet(NewValue: TX87RoundingMode): TX87RoundingMode;

{
  X87ControlFlagGet

  Returns current value of selected flag in the x87 FPU control word.
}
Function X87ControlFlagGet(Flag: TX87ControlFlag): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87ControlFlagSet

  Sets value of selected flag in the x87 FPU control word to a NewValue and
  returns previous state of this flag.
}
Function X87ControlFlagSet(Flag: TX87ControlFlag; NewValue: Boolean): Boolean;

{
  X87ControlFlagsGet

  Returns status of all flags in the x87 FPU control word. When the flag is
  set, it is included in the result, when it is clear, it is excluded.
}
Function X87ControlFlagsGet: TX87ControlFlags;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87ControlFlagsSet

  Sets new status of all flags in the x87 FPU control word. If a flag is
  included in the NewValue, it will be set, when it is not included, it will
  be cleared. Return value is previous state of all control flags.
}
Function X87ControlFlagsSet(NewValue: TX87ControlFlags): TX87ControlFlags;

//------------------------------------------------------------------------------
{
  X87ExceptionMaskGet

  Returns current value of selected x87 FPU exception mask bit.
}
Function X87ExceptionMaskGet(Exception: TX87Exception): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87ExceptionMaskSet

  Sets value of selected exception mask bit in the x87 FPU control word to a
  NewValue and returns previous value of this bit.

  When the bit is set (true), the selected exception will be masked and not
  raised on its occurence. When clear (false), the exception is unmasked and
  can be raised.
}
Function X87ExceptionMaskSet(Exception: TX87Exception; NewValue: Boolean): Boolean;

{
  X87ExceptionMasksGet

  Returns status of all exception mask bits in the x87 FPU control word. When
  the bit is set, the exception is included in the result, when it is clear,
  the exception is excluded from the result.
}
Function X87ExceptionMasksGet: TX87Exceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87ExceptionMasksSet

  Sets new value of all exception mask bits in the x87 FPU control word. If an
  exception is included in the NewValue, the mask bit will be set for that
  particular exception, when it is not included, the mask bit will be cleared.

  Return value is previous state of all exception mask bits.
}
Function X87ExceptionMasksSet(NewValue: TX87Exceptions): TX87Exceptions;

{
  X87ExceptionFlagGet

  Returns current value of selected x87 FPU exception flag bit.

  If the bit is set (True returned), it means the selected exception was
  encountered, but not yet raised (ie. the exception handler was not called).
}
Function X87ExceptionFlagGet(Exception: TX87Exception): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  GetX87ExceptionFlags

  Returns status of all exception flag bits in the x87 FPU status word. When
  the bit is set, the exception is included in the result, when it is clear,
  the exception is excluded from the result.
}
Function X87ExceptionFlagsGet: TX87Exceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  X87ExceptionsClear

  Executes FNCLEX instruction - it clears all exception flag bits, FPU busy
  flag, summary status flag and stack fault flag in x87 FPU status word.
  Condition codes are undefined, top of the stack stays unaffected.

  Note that it does NOT handle pending unmasked floating-point exceptions.

  This function is subject to UIM - its implementation can be selected. But
  note that only assembly implementation can provide the described operation,
  pascal version only raises an EFUUnsupportedOp exception.
}
procedure X87ExceptionsClear;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  X87ExceptionsRaise

  Executes (F)WAIT instruction - pending unmasked exceptions are raised, ie.
  their handler is called.

  Note that it might not clear exception flags in status word if no exception
  is truly raised (eg. because they are all masked).

  This function is subject to UIM - its implementation can be selected. But
  only its assembly implementation can execute the described operation, pascal
  version only raises an EFUUnsupportedOp exception.
}
procedure X87ExceptionsRaise;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                     F80 conversion (F80C) state management
--------------------------------------------------------------------------------
===============================================================================}
{
  Conversion to and from double extended floating point numbers (80bit float80)
  provided by this library are implemented in two forms - in pascal ("manual"
  conversion that is not using FPU, it operates only using integer arithmetics
  and logical operations) and in assembly (uses x87 FPU). You can select which
  one will be called using unit implementation management (UIM) functions (see
  further down).

  Because the conversion is not a trivial process, it can produce special
  results (NaN, infinity, zero from non-zero number, ...) and/or raise number
  of different exceptions. But there might be a situation where the behavior
  needs to be altered, eg. by suppressing some exceptions, changing precision
  or rounding mode and so on.

  There arises a problem - the two implementations, being completely different,
  cannot use the same state and control mechanism - assembly uses x87 FPU state
  whereas pascal uses its own thread variables. Pascal impl. was written to
  emulate behavior of x87 as closely as possible, but it still canot use the
  x87 status and control registers simply because it was written to function
  even on systems without x87 FPU.

  Following set of functions is provided to access current state used by the
  conversions, irrespective of what implementation is selected to run.
  If conversion is routed to assembly, then these functions will access the x87
  hardware using X87* functions (see above). If conversion is routed to pascal,
  then they will work on an emulated state.

      NOTE - emulated state is automatically initialized (status word is set
             to 0, control word to $1332), so there is no need to explicitly
             init it.

      NOTE - the two states do not automatically share information. If you make
             change in one state and then switch implementation routing, the
             changes will not be propagated and the conversion will now use
             different state with possibly different settings.

      WARNING - the states are instantiated per-thread, meaning each thread
                has its own setting - change made in one thread is not seen
                by any other thread.
}
{===============================================================================
    F80C state management - exceptions declaration
===============================================================================}
type
  TF80CFlagException  = TFUNumericException;
  TF80CFlagExceptions = set of TF80CFlagException;

  TF80CRaiseException  = TFUException;
  TF80CRaiseExceptions = set of TF80CRaiseException;

//------------------------------------------------------------------------------
{
  EF80CException

  This is a common ancestor for all exceptions raised by pascal-implemented
  float80 conversions when the process encounters a condition that warrants
  an exception raising.

  Multiple different floating point exceptions might be signaled at the same
  time, but only one exception object is created and raised. For this reason,
  this common ancestor implements properties that can be used to discern which
  error states were encountered:

    PendingExceptions  - lists all floating point exceptions that were set in
                         status word when the object was instantiated

    MaskedExceptions   - pending floating point exceptions that are masked (ie.
                         are ignored)

    UnmaskedExceptions - unmasked floating point exceptions, note that this set
                         includes only basic exceptions which have their flags
                         in status word (ie. not stack faults)

    RaisedExceptions   - this set contains all exceptions that were pending and
                         unmasked when the exception object was created, the
                         stack faults are resolved and present if they were
                         signaled

  There are also properties offering status and control words as they were when
  the object was created, so you can probe them for further information.

    NOTE - when instantiating this class (creating object), functions
           F80CEnvironmentInit and F80CControlWordInit are called (in
           that order), clearing all exception flags and setting default
           exception mask, be aware of that.
}
type
  EF80CException = class(EFUEmulationException)
  protected
    fStatusWord:    UInt16;
    fControlWord:   UInt16;
    fPendingExcs:   TF80CFlagExceptions;
    fMaskedExcs:    TF80CFlagExceptions;
    fUnmaskedExcs:  TF80CFlagExceptions;
    fRaisedExcs:    TF80CRaiseExceptions;
    procedure Initialize; override;
  public
    property StatusWord: UInt16 read fStatusWord;
    property ControlWord: UInt16 read fControlWord;
    property PendingExceptions: TF80CFlagExceptions read fPendingExcs;
    property MaskedExceptions: TF80CFlagExceptions read fMaskedExcs;
    property UnmaskedExceptions: TF80CFlagExceptions read fUnmaskedExcs;
    property RaisedExceptions: TF80CRaiseExceptions read fRaisedExcs;
  end;

//------------------------------------------------------------------------------
{
  Common ancestor EF80CException is never internally instantiated, instead
  the process selects one of the following classes to suit the encountered
  error state.
  If multiple floating point exceptions are signaled at the same time, then
  the class is selected according to floating point exception priority (see
  implementation for details).

    NOTE - EF80CStackFault is not raised, it is declared only as a common
           ancestor for stack fault exceptions.
}
type
  EF80CInvalidOp = class(EF80CException);
  EF80CDenormal  = class(EF80CException);
  EF80CDivByZero = class(EF80CException);
  EF80COverflow  = class(EF80CException);
  EF80CUnderflow = class(EF80CException);
  EF80CPrecision = class(EF80CException);

  EF80CStackFault     = class(EF80CException);
  EF80CStackUnderflow = class(EF80CStackFault);
  EF80CStackOverflow  = class(EF80CStackFault);

{===============================================================================
    F80C state management - low-lewel access declaration
===============================================================================}
{
  F80CEmulated

  Returns true when F80C* functions are accessing the pascal-implemeted
  (emulated) state, false when they are working with x87 FPU (ie. are merely
  calling X87* functions).
}
Function F80CEmulated: Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CStatusWordGet

  Returns current value of F80C status word.
}
Function F80CStatusWordGet: UInt16;

{
  F80CControlWordGet

  Returns current value of F80C control word.
}
Function F80CControlWordGet: UInt16;

{
  F80CControlWordSet

  Changes value of F80C control word to a passed value (NewValue).

  For details of exceptions masking, refer to X87ControlWordSet (behavior of
  pascal impl. is the same).
}
procedure F80CControlWordSet(NewValue: UInt16);

{
  F80CEnvironmentInit

  Re-initializes F80C state, whatever it currently is.

  In emulated mode, it sets status word to zero and control word to a value of
  $037F (all exceptions masked, rounding to nearest, precision is 64bit).
  If the routing goes to assembly implementation, then refer to description
  of X87EnvironmentInit for more information.

  Each call to this function should be followed by call to F80CControlWordInit,
  unless you want to set control word to non-default value.

  There is no need to call this function when float80 conversions are routed
  to pascal implementation and therefore the F80C* functions are accessing
  emulated state.
}
procedure F80CEnvironmentInit;

{
  F80CControlWordInit

  Sets F80C control word to a value of $1332 - denormal, underflow and
  precision exceptions are masked (others are unmasked), precision is set to
  extended, rounding is set to nearest and infinity control bit is 1.

  If the routing goes to assembly implementation, then refer to description
  of X87ControlWordInit for more information.
}
procedure F80CControlWordInit;{$IF Defined(CanInline) and not Defined(FPC)} inline;{$IFEND}

{===============================================================================
    F80C state management - abstracted access declaration
===============================================================================}
type
  TF80CStatusFlag  = TX87StatusFlag;
  TF80CStatusFlags = set of TF80CStatusFlag;

  TF80CPrecisionMode = TX87PrecisionMode;

  TF80CRoundingMode = TX87RoundingMode;

  TF80CControlFlag = TX87ControlFlag;
  TF80CControlFlags = set of TF80CControlFlag;

  TF80CException  = TX87Exception;
  TF80CExceptions = set of TF80CException;

const
  F80CExceptionsAll = [Low(TF80CException)..High(TF80CException)];

//------------------------------------------------------------------------------
{
  F80CTopOfStackGet

  Returns current top of the stack pointer from the F80C status word.

  Pascal implementation does not use this value and therefore it will, in that
  case, always return 0.
}
Function F80CTopOfStackGet: Integer;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CStatusFlagGet

  Returns current value of selected flag in the F80C status word.

  Note that only some status flags are managed by the pascal implementation,
  others are ignored and should be always clear (these include condition codes
  C0, C2 and C3).
}
Function F80CStatusFlagGet(Flag: TF80CStatusFlag): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CStatusFlagsGet

  Returns status of all recognized flags in the F80C status word. When the flag
  is set, it is included in the result, when it is clear, it is excluded.
}
Function F80CStatusFlagsGet: TF80CStatusFlags;{$IFDEF CanInlineFPC} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  F80CPrecisionModeGet

  Returns current value of F80C precision mode.
}
Function F80CPrecisionModeGet: TF80CPrecisionMode;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CPrecisionModeSet

  Sets F80C precision mode to a selected NewValue and returns its previous
  value.
}
Function F80CPrecisionModeSet(NewValue: TF80CPrecisionMode): TF80CPrecisionMode;

{
  F80CRoundingModeGet

  Returns current value of F80C rounding mode.
}
Function F80CRoundingModeGet: TF80CRoundingMode;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CRoundingModeSet

  Sets F80C rounding mode to a selected NewValue and returns its previous value.
}
Function F80CRoundingModeSet(NewValue: TF80CRoundingMode): TF80CRoundingMode;

{
  F80CControlFlagGet

  Returns current value of selected flag in the F80C control word.

  Pascal implementation currently ignores all control flags (though they can be
  manually set).
}
Function F80CControlFlagGet(Flag: TF80CControlFlag): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CControlFlagSet

  Sets value of selected flag in the F80C control word to a NewValue and
  returns previous state of this flag.
}
Function F80CControlFlagSet(Flag: TF80CControlFlag; NewValue: Boolean): Boolean;

{
  F80CControlFlagsGet

  Returns status of all flags in the F80C control word. When the flag is set,
  it is included in the result, when it is clear, it is excluded.
}
Function F80CControlFlagsGet: TF80CControlFlags;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CControlFlagsSet

  Sets new status of all flags in the F80C control word. If a flag is included
  in the NewValue, it will be set, when it is not included, it will be cleared.
  Return value is previous state of all control flags.
}
Function F80CControlFlagsSet(NewValue: TF80CControlFlags): TF80CControlFlags;

//------------------------------------------------------------------------------
{
  F80CExceptionMaskGet

  Returns current value of selected F80C exception mask bit.
}
Function F80CExceptionMaskGet(Exception: TF80CException): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CExceptionMaskSet

  Sets value of selected exception mask bit in the F80C control word to a
  NewValue and returns previous value of this bit.

  When the bit is set (true), the selected exception will be masked and not
  raised on its occurence. When clear (false), the exception is unmasked and
  can be raised.
}
Function F80CExceptionMaskSet(Exception: TF80CException; NewValue: Boolean): Boolean;

{
  F80CExceptionMasksGet

  Returns status of all exception mask bits in the F80C control word. When the
  bit is set, the exception is included in the result, when it is clear, the
  exception is excluded from the result.
}
Function F80CExceptionMasksGet: TF80CExceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CExceptionMasksSet

  Sets new value of all exception mask bits in the F80C control word. If an
  exception is included in the NewValue, the mask bit will be set for that
  particular exception, when it is not included, the mask bit will be cleared.

  Return value is previous state of all exception mask bits.
}
Function F80CExceptionMasksSet(NewValue: TF80CExceptions): TF80CExceptions;

{
  F80CExceptionFlagGet

  Returns current value of selected F80C exception flag bit.

  If the bit is set (True returned), it means the selected exception was
  encountered, but not yet raised (ie. the exception handler was not called).
}
Function F80CExceptionFlagGet(Exception: TF80CException): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F80CExceptionFlagsGet

  Returns status of all exception flag bits in the F80C status word. When the
  bit is set, the exception is included in the result, when it is clear, the
  exception is excluded from the result.
}
Function F80CExceptionFlagsGet: TF80CExceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  F80CExceptionsClear

  For pascal implementation, it clears entire status word (sets it to zero)
  within the emulated state, effectively setting all flags (exception, status,
  condition codes) to clear. For assembly, it calls X87ExceptionsClear - see
  description of that function for more details.

  It does NOT raise pending unmasked floating-point exceptions.
}
procedure F80CExceptionsClear;

{
  F80CExceptionsRaise

  Raises all pending unmasked exceptions, if there are any.

  In pascal impl., it means raising an exception of class that is a descendant
  of EF80CException (see its declaration for more details). Also note that this
  exception object reinitializes current state, but of course only when it is
  created - meaning this function, when it raises this exception, also clears
  exception flags, when it does not raise an exception, then nothing is changed.
}
procedure F80CExceptionsRaise;

{===============================================================================
--------------------------------------------------------------------------------
                     Float80 <-> Float64 conversions (F80C)
--------------------------------------------------------------------------------
===============================================================================}
{
  Following functions provide a conversion between double precision (64bit)
  floating point numbers (Double, Float64) and double extended precision
  (80bit) floats (Extended, Float80).

  Normally, these conversions are done automatically and are managed by the
  compiler. But in applications compiled for 64bit Windows, the type Extended
  is declared only as an alias to Double, ie. it is not 80 bits wide, only 64
  bits, and there is no normal way of getting 80bit float - these explicit
  conversions are here fo this and other similar situations.

  Functions accepting both float numbers or pointers to them are provided -
  those taking pointers are here for situations where directly passing float
  value might create some problems (eg. exceptions due to checks).
  But make sure the pointers are pointing to a correct value, or at least
  memory location of appropriate size.

  The conversion process is subject to UIM (unit implementation management,
  see further down), meaning you can select which implementation will be
  called - you can select between pure pascal and assembly.
  Assembly implementation uses x87 floating point unit to do the work, pascal
  is doing the conversion "manually" (the number is decoded, the parts
  recalculated and then the result is constructed from them).

  Processing in x87 hardware can raise floating point exceptions and is
  affected by rounding mode and other FPU settings. Pascal implementation
  closely emulates this behavior. To change these settings, irrespective of
  which implementation is selected, use F80C state management functions above.

  Note that pre-computation and post-computation character of all exceptions
  is observed, meaning, in some situations, the destination (result) can be
  updated even when an exception is raised.

  When type Extended is declared as an alias to type Double (eg. in Win64),
  then functions DoubleToExtended and ExtendedToDouble will not perform any
  conversion, they only assign the given value into result.
}
procedure Float64ToFloat80(Float64Ptr,Float80Ptr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function Float64ToFloat80(const Value: Float64): Float80; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

procedure DoubleToExtended(DoublePtr,ExtendedPtr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function DoubleToExtended(const Value: Double): Extended; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure Float80ToFloat64(Float80Ptr,Float64Ptr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function Float80ToFloat64(const Value: Float80): Float64; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

procedure ExtendedToDouble(ExtendedPtr,DoublePtr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function ExtendedToDouble(const Value: Extended): Double; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                   SSE/AVX control and status (CS) management
--------------------------------------------------------------------------------
===============================================================================}
{
  You can use following constants to probe and alter individual bits and bit
  fields in control and status register (MXCSR) of floating point vector units.
  This register is used by all versions of SSE and AVX extensions.

  For meaning and description of individual bits and fields, please refer to
  documentation of MXCSR register provided by Intel or AMD.
}
const
  MXCSR_EFLAG_InvalidOP = UInt32($00000001);
  MXCSR_EFLAG_Denormal  = UInt32($00000002);
  MXCSR_EFLAG_DivByZero = UInt32($00000004);
  MXCSR_EFLAG_Overflow  = UInt32($00000008);
  MXCSR_EFLAG_Underflow = UInt32($00000010);
  MXCSR_EFLAG_Precision = UInt32($00000020);

  MXCSR_EFLAG_All = UInt32($0000003F);

  MXCSR_EMASK_InvalidOP = UInt32($00000080);
  MXCSR_EMASK_Denormal  = UInt32($00000100);
  MXCSR_EMASK_DivByZero = UInt32($00000200);
  MXCSR_EMASK_Overflow  = UInt32($00000400);
  MXCSR_EMASK_Underflow = UInt32($00000800);
  MXCSR_EMASK_Precision = UInt32($00001000);

  MXCSR_EMASK_All = UInt32($00001F80);

  MXCSR_DenormalsAreZeros = UInt32($00000040);
  MXCSR_FlushToZero       = UInt32($00008000);

  MXCSR_Rounding = UInt32($00006000); // bits 13..14

  MXCSR_SHIFT_Rounding = 13;

//------------------------------------------------------------------------------
const
{
  MXCSR_InitialValue

  Value the SSE/AVX control and status register (MXCSR) has after a hardware
  power-up or reset (see description of function F16CEnvironmentInit for more
  details).
}
  MXCSR_InitialValue = $00001F80;
{
  MXCSR_DefaultValue

  Default value for normal use (see F16CControlAndStatusInit).
}
  MXCSR_DefaultValue = $00001900;

{===============================================================================
    SSE/AVX CS management - low-lewel access declaration
===============================================================================}
{
  Following set of functions is here to provide direct access to control and
  status register (MXCSR) of floating point vector units, that is all versions
  of Streaming SIMD Extension (SSE) and Advanced Vector Extension (AVX).

  Functions for each unit are provided separately (SSE*/AVX*), but since these
  units both operate on the same register, they are only aliases for common
  code and can be used interchangeably.

  Functions *ControlAndStatusGet, *ControlAndStatusSet and *FloatDataGet are
  subject to UIM, so their implementation can be selected. But because they
  need to directly access the hardware, they can work only when implemented in
  assembly - you can still route them to pascal implementation, but in that
  case they only raise an EFUUnsupportedOp exception and do nothing.
  They are routed to assembly impl. automatically at unit initialization, but
  only if current system supports any vector extension from SSE or AVX families.
  If no such extension is supported, then they are routed to pascal impl. and
  should not be used, as calling them just results in an exception. You can
  discern their current routing using UIM function UIM_FloatUtils_GetFuncImpl.
}
{
  SSEControlAndStatusGet
  AVXControlAndStatusGet

  Returns current value of MXCSR (control and status register of floating point
  vector units).
}
Function SSEControlAndStatusGet: UInt32;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXControlAndStatusGet: UInt32;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEControlAndStatusSet
  AVXControlAndStatusSet

  Changes value of MXCSR register to a passed value (NewValue).

  The provided value is combined with MXCSR mask (for details see function
  *ControlAndStatusMaskGet) before being loaded into the register, so you do
  not need to do it explicitly.

  Unlike in x87 FPU, vector units do not raise pending unmasked exceptions.
  If any were present in the old state, or are present in newly written state,
  they will not be raised. Any pending exceptions will be signaled only when
  some future vector instruction operating on vector registers raise its own
  exception.
}
procedure SSEControlAndStatusSet(NewValue: UInt32);{$IFDEF CanInlineFPC} inline;{$ENDIF}
procedure AVXControlAndStatusSet(NewValue: UInt32);{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEControlAndStatusMaskGet
  AVXControlAndStatusMaskGet

  Provides a mask that indicates which bits in MXCSR register are supported by
  the current hardware and can be written into. It needs to be combined (using
  logical AND) with value being written into MXCSR register - this is because
  writing anything but zero to reserved parts of that register is not allowed
  and will raise an exception.

    NOTE - functions *ControlAndStatusSet do this combining automatically,
           so this mask is publicly provided only for informative reasons.

  Bits set to 1 in the mask are supported by the hardware, those set to 0 are
  not supported and must not be written into (ie. set to 1).

  The mask is obtained from hardware at unit initialization and is buffered
  in global variable from which it is fetched. If no suitable vector unit is
  supported, then this mask reads as zero.
}
Function SSEControlAndStatusMaskGet: UInt32;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXControlAndStatusMaskGet: UInt32;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEControlAndStatusSupportsDAZ
  AVXControlAndStatusSupportsDAZ

  Inidicates whether current hardware supports DAZ (denormals are zeroes) bit
  (bit 6) in MXCSR register. True means it is supported, false means it is not.

  Only very old implementations of first version of SSE (before Pentium 4) do
  not support this bit. But you should not assume it is automatically supported
  on modern hardware and if you plan to use it, you should still check its
  support.
  That being said, setting it to true will not raise an exception even on
  hardware that dos not support it, as in that case it will be masked-out by
  MXCSR mask before being written.
}
Function SSEControlAndStatusSupportsDAZ: Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXControlAndStatusSupportsDAZ: Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEEnvironmentInit
  AVXEnvironmentInit

  Sets MXCRS register to value of $00001F80 (its initial value, ie. a value
  it has after hardware power-up) - all exceptions are masked, DAZ and FTZ
  bits are both zero, rounding is set to nearest.
}
procedure SSEEnvironmentInit;{$IFDEF CanInline} inline;{$ENDIF}
procedure AVXEnvironmentInit;{$IFDEF CanInline} inline;{$ENDIF}

{
  SSEControlAndStatusInit
  AVXControlAndStatusInit

  Sets value of register MXCSR to $00001900 (a default value that prepares
  it for normal use) - denormal, underflow and precision exceptions are masked
  (others are unmasked), DAZ and FTZ are zero, rounding is set to nearest.

  You should always call this function if you plan to use vector units for
  normal floating point aritmentics and the register is not already prepared
  (eg. in 32bit applications it almost certainly isn't).

    WARNING - the initialization must be done explicitly in every execution
              thread that needs to use the vector unit.
}
procedure SSEControlAndStatusInit;{$IFDEF CanInline} inline;{$ENDIF}
procedure AVXControlAndStatusInit;{$IFDEF CanInline} inline;{$ENDIF}

{
  TVectorFloatData

  This structure is used to return data from floating point vector units (SSE
  and/or AVX).

  It provides overlayed array fields of most of normally used vector types
  (with entries being floats or integers of different sizes), so the
  interpretation of stored data is up to you, just select the proper field.

  There are several vector extensions, each with different width and number of
  vector registers. And because not all systems support the most modern ones,
  it is necessary to also provide information about how many registers were
  really loaded and how much data is in each register.

  Number of filled registers is given in field RegisterCount - so registers
  from 0 up to (RegisterCount - 1) will contain data.
  How much data each register contains can be discerned from RegisterWidth or
  RegisterSize - to get number of primitives in register, just divide these
  by size of the used primitive (eg. for register width of 256bits and Float64
  primitives - register array will contain (256 / 64) = 4 Float64 values).

  Registers and values that are not loaded will be zeroed.
}
type
  TVectorFloatData = record
    RegisterCount:  Integer;
    RegisterWidth:  Integer;  // in bits
    RegisterSize:   Integer;  // in bytes
    Registers:      packed array[0..31] of packed record case Integer of
       // floating point vectors...
       0: (Float16Vector: packed array[0..31] of Float16);
       1: (HalfVector:    packed array[0..31] of Half);
       2: (Float32Vector: packed array[0..15] of Float32);
       3: (SingleVector:  packed array[0..15] of Single);
       4: (Float64Vector: packed array[0..7] of Float64);
       5: (DoubleVector:  packed array[0..7] of Double);
       // integer vectors...
       6: (ByteVector:    packed array[0..63] of UInt8);
       7: (UInt8Vector:   packed array[0..63] of UInt8);
       8: (Int8Vector:    packed array[0..63] of Int8);
       9: (WordVector:    packed array[0..31] of UInt16);
      10: (UInt16Vector:  packed array[0..31] of UInt16);
      11: (Int16Vector:   packed array[0..31] of Int16);
      12: (DWordVector:   packed array[0..15] of UInt32);
      13: (UInt32Vector:  packed array[0..15] of UInt32);
      14: (Int32Vector:   packed array[0..15] of Int32);
      15: (QWordVector:   packed array[0..7] of UInt64);
      16: (UInt64Vector:  packed array[0..7] of UInt64);
      17: (Int64Vector:   packed array[0..7] of Int64);
    end;
  end;

{
  SSEFloatDataGet
  AVXFloatDataGet

  Loads data from registers of floating point vector unit with the largest data
  set that is available and supported on current system into provided FloatData
  structure (see description of type TVectorFloatData for more details).
}
procedure SSEFloatDataGet(out FloatData: TVectorFloatData);{$IFDEF CanInlineFPC} inline;{$ENDIF}
procedure AVXFloatDataGet(out FloatData: TVectorFloatData);{$IFDEF CanInlineFPC} inline;{$ENDIF}

{===============================================================================
    SSE/AVX CS management - abstracted access declaration
===============================================================================}
{
  Set of functions and types providing slightly more convenient interface to
  access to control and status register (MXCSR) of floating point vector units
  (SSE/AVX).

  Note that these functions are only an abstraction over low-level access (SSE*
  and AVX* functions above), maning you still need to consider limitations and
  potential issues mentiond there.

  As for low-level functions, two versions of each function are provided, one
  with SSE prefix and one with AVX prefix - they are both equivalent in all
  cases and can be used interchangeably.
}
type
  TSSERoundingMode = TFURoundingMode;
  TAVXRoundingMode = TSSERoundingMode;

  TSSEControlFlag  = (cfDenormalsAreZeros,cfFlushToZero);
  TSSEControlFlags = set of TSSEControlFlag;
  TAVXControlFlag  = TSSEControlFlag;
  TAVXControlFlags = TSSEControlFlags;

  TSSEException  = TFUNumericException;
  TSSEExceptions = set of TSSEException;
  TAVXException  = TSSEException;
  TAVXExceptions = TSSEExceptions;

const
  SSEExceptionsAll = [Low(TSSEException)..High(TSSEException)];
  AVXExceptionsAll = [Low(TAVXException)..High(TAVXException)];

//------------------------------------------------------------------------------
{
  SSERoundingModeGet
  AVXRoundingModeGet

  Returns current value of fp-vector units (SSE/AVX) rounding mode.
}
Function SSERoundingModeGet: TSSERoundingMode;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXRoundingModeGet: TAVXRoundingMode;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSERoundingModeSet
  AVXRoundingModeSet

  Sets SSE/AVX rounding mode to a selected NewValue and returns its previous
  value.
}
Function SSERoundingModeSet(NewValue: TSSERoundingMode): TSSERoundingMode;
Function AVXRoundingModeSet(NewValue: TAVXRoundingMode): TAVXRoundingMode;

{
  SSEControlFlagGet
  AVXControlFlagGet

  Returns current value of selected flag in the SSE/AVX control and status
  register.
}
Function SSEControlFlagGet(Flag: TSSEControlFlag): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXControlFlagGet(Flag: TAVXControlFlag): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEControlFlagSet
  AVXControlFlagSet

  Sets value of selected flag in the SSE/AVX control and status register to a
  NewValue and returns previous state of this flag.
}
Function SSEControlFlagSet(Flag: TSSEControlFlag; NewValue: Boolean): Boolean;
Function AVXControlFlagSet(Flag: TAVXControlFlag; NewValue: Boolean): Boolean;

{
  SSEControlFlagsGet
  AVXControlFlagsGet

  Returns status of all flags in the SSE/AVX control and status register.
  When the flag is set, it is included in the result, when it is clear, it is
  excluded.
}
Function SSEControlFlagsGet: TSSEControlFlags;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXControlFlagsGet: TAVXControlFlags;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEControlFlagsSet
  AVXControlFlagsSet

  Sets new status of all known control flags in the SSE/AVX control and status
  register. If a flag is included in the NewValue, it will be set, when it is
  not included, it will be cleared. Return value is previous state of all
  control flags.
}
Function SSEControlFlagsSet(NewValue: TSSEControlFlags): TSSEControlFlags;
Function AVXControlFlagsSet(NewValue: TAVXControlFlags): TAVXControlFlags;

//------------------------------------------------------------------------------
{
  SSEExceptionMaskGet
  AVXExceptionMaskGet

  Returns current value of selected vector exception mask bit.
}
Function SSEExceptionMaskGet(Exception: TSSEException): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXExceptionMaskGet(Exception: TAVXException): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEExceptionMaskSet
  AVXExceptionMaskSet

  Sets value of selected exception mask bit in the vector control and status
  register to a NewValue and returns previous value of this bit.

  When the bit is set (true), the selected exception will be masked and not
  raised on its occurence. When clear (false), the exception is unmasked and
  can be raised.
}
Function SSEExceptionMaskSet(Exception: TSSEException; NewValue: Boolean): Boolean;
Function AVXExceptionMaskSet(Exception: TAVXException; NewValue: Boolean): Boolean;

{
  SSEExceptionMasksGet
  AVXExceptionMasksGet

  Returns status of all exception mask bits in the SSE/AVX control and status
  register. When the bit is set, the exception is included in the result, when
  it is clear, the exception is excluded from the result.
}
Function SSEExceptionMasksGet: TSSEExceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXExceptionMasksGet: TAVXExceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEExceptionMasksSet
  AVXExceptionMasksSet

  Sets new value of all exception mask bits in the SSE/AVX control and status
  register. If an exception is included in the NewValue, the mask bit will be
  set for that particular exception, when it is not included, the mask bit will
  be cleared.

  Returns previous state of all exception mask bits.
}
Function SSEExceptionMasksSet(NewValue: TSSEExceptions): TSSEExceptions;
Function AVXExceptionMasksSet(NewValue: TAVXExceptions): TAVXExceptions;

{
  SSEExceptionFlagGet
  AVXExceptionFlagGet

  Returns current value of selected vector exception flag bit.

  If the bit is set (True returned), it means the selected exception was
  encountered, but not yet raised (ie. the exception handler was not called).
}
Function SSEExceptionFlagGet(Exception: TSSEException): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXExceptionFlagGet(Exception: TAVXException): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEExceptionFlagSet
  AVXExceptionFlagSet

  Sets exception flag bit in MXCSR register for the selected exception to a
  NewValue.

  Unlike the x87 status word, the entire control and status register can be
  directly changed, including exception flags. But note that it does NOT mean
  you can raise any exception at any time.
  If you signal exception this way, it will not be raised by next SSE/AVX
  instruction - it will be signaled only when some future instruction generates
  its own exception. Then this exceptions will be raised along with it.
}
Function SSEExceptionFlagSet(Exception: TSSEException; NewValue: Boolean): Boolean;
Function AVXExceptionFlagSet(Exception: TAVXException; NewValue: Boolean): Boolean;

{
  SSEExceptionFlagsGet
  AVXExceptionFlagsGet

  Returns status of all exception flag bits in the SSE/AVX control and status
  register. When the bit is set, the exception is included in the result, when
  it is clear, the exception is excluded from the result.
}
Function SSEExceptionFlagsGet: TSSEExceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function AVXExceptionFlagsGet: TAVXExceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  SSEExceptionFlagsSet
  AVXExceptionFlagsSet

  Sets status of all exception flags in MXCSR register. Exceptions included in
  NewValue will be set (to 1), those excluded will be cleared (to 0).

  For exception signaling refer to description of function SSEExceptionFlagSet/
  AVXExceptionFlagSet.
}
Function SSEExceptionFlagsSet(NewValue: TSSEExceptions): TSSEExceptions;
Function AVXExceptionFlagsSet(NewValue: TAVXExceptions): TAVXExceptions;

{
  SSEExceptionsClear
  AVXExceptionsClear

  Clears all exception flags in vector control and status register (MXCSR).
}
procedure SSEExceptionsClear;{$IFDEF CanInline} inline;{$ENDIF}
procedure AVXExceptionsClear;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                     F16 conversion (F16C) state management
--------------------------------------------------------------------------------
===============================================================================}
{
  Similarly to conversions from/to 80bit floats (F80C), conversions from/to
  16bit floats are implemented in two forms - in assembly and in pure pascal
  code. Pascal code uses only integer arithmetics and logical operations,
  assembly uses instruction set extension F16C provided by newer processors.
  You can select which implementation will be used using UIM functions (see
  further) - but make sure your CPU can run the assembly implementation
  (UIM_FloatUtils_SupportedFuncImpl can be used for that).

  As in case of F80C, pascal code uses an emulated state to control exception
  masking, rounding and other settings, whereas assembly code uses floating
  point vector control and status register (MXCSR). Again, the emulated state
  is written to be maximally similar to hardware behavior.

  Following types and functions are here to access currently used state,
  irrespective of its nature (whethether it is an emulated state or hardware
  state).

      NOTE - emulated state is automatically initialized (the control and
             status "register" is set to $00001900), there is no need to
             explicitly initialize it.

      NOTE - the two states do not share information. If you make change in
             one state and then switch implementation routing, the changes
             will not be propagated and the conversion will now use different
             state with possibly different settings.

      WARNING - the states are instantiated per-thread, meaning each thread
                has its own setting - change made in one thread is not seen
                by any other thread.
}
{===============================================================================
    F16C state management - exceptions declaration
===============================================================================}
type
  TF16CFlagException  = TFUNumericException;
  TF16CFlagExceptions = set of TF16CFlagException;

  TF16CRaiseException  = TFUNumericException; // there are no stack faults
  TF16CRaiseExceptions = set of TF16CRaiseException;

{
  EF16CException

  Common ancestor class for all exceptions raised by pascal-implemented
  float16 conversions when the process encounters an exceptions condition.

  As is the case for x87 exceptions, multiple different floating point
  exceptions might be signaled at the same time - there is even a possibility
  of unmasked pending exceptions that originated from code before the faulting
  instruction, this is because vector instructions do not check for and raise
  pending unmasked exceptions, they only raise whatever they "produce". This
  class provides properties that can be used to discern which error states were
  encountered:

    PendingExceptions  - floating point exceptions that were set in control and
                         status register when the object was instantiated

    MaskedExceptions   - pending floating point exceptions that are currently
                         masked

    RaisedExceptions   - exceptions that were pending and unmasked when the
                         exception object was created

  You can also access property ControlAndStatus that provides value stored in
  control and status register upon creation of this exception.

    NOTE - when instantiating this class (creating object), functions
           F16CEnvironmentInit and F16CControlAndStatusInit are called
           (in that order), clearing all exception flags and setting
           default exception mask, control flags and rounding mode.
}
type
  EF16CException = class(EFUEmulationException)
  protected
    fControlAndStatus:  UInt32;
    fPendingExcs:       TF16CFlagExceptions;
    fMaskedExcs:        TF16CFlagExceptions;
    fRaisedExcs:        TF16CRaiseExceptions;
    procedure Initialize; override;
  public
    property ControlAndStatus: UInt32 read fControlAndStatus;
    property PendingExceptions: TF16CFlagExceptions read fPendingExcs;
    property MaskedExceptions: TF16CFlagExceptions read fMaskedExcs;
    property RaisedExceptions: TF16CRaiseExceptions read fRaisedExcs;
  end;

//------------------------------------------------------------------------------
{
  Common ancestor EF16CException is not internally instantiated, instead
  the process selects one of the following classes to suit the encountered
  error state.
  If multiple floating point exceptions are signaled at the same time, then
  the class is selected according to floating point exception priority (see
  implementation for details).
}
type
  EF16CInvalidOp = class(EF16CException);
  EF16CDenormal  = class(EF16CException);
  EF16CDivByZero = class(EF16CException);
  EF16COverflow  = class(EF16CException);
  EF16CUnderflow = class(EF16CException);
  EF16CPrecision = class(EF16CException);

{===============================================================================
    F16C state management - low-lewel access declaration
===============================================================================}
{
  F16CEmulated

  Returns true when F16C* functions are accessing the pascal-implemeted
  (emulated) state, false when they are working with hardware implementation
  (in which case they are only calling SSE* functions).
}
Function F16CEmulated: Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F16CControlAndStatusGet

  Returns current value of floating point vector control and status register
  (MXCSR).
}
Function F16CControlAndStatusGet: UInt32;

{
  F16CControlAndStatusSet

  Changes value of control and status register to a passed value (NewValue).

  For details of exceptions masking, refer to SSEControlAndStatusSet (pascal
  implementation behaves the same).
}
procedure F16CControlAndStatusSet(NewValue: UInt32);

{
  F16CEnvironmentInit

  Re-initializes F16C state, irrespective of its nature.

  In both modes (pascal and asm), this function only sets control and status
  register to an initial value of $00001F80 (all exceptions masked, rounding to
  nearest, all control flags cleared). No data are changed (including eg.
  AVX512 mask registers K0..K7).

  Each call to this function should be followed by a call to function
  F16CControlAndStatusInit to preperly setup control and status register
  for further operations.

  There is no need to call this function when float16 conversions are routed
  to pascal implementation and therefore the F16C* functions are accessing
  emulated state.
}
procedure F16CEnvironmentInit;

{
  F16CControlAndStatusInit

  Sets F16C control and status register to a value of $00001900 - denormal,
  underflow and precision exceptions are masked (others are unmasked), rounding
  is set to nearest and all control flags are cleared.

  This function works exatly the same in both modes (emulated and hardware).
}
procedure F16CControlAndStatusInit;

{===============================================================================
    F16C state management - abstracted access declaration
===============================================================================}
type
  TF16CRoundingMode = TSSERoundingMode;

  TF16CControlFlag  = TSSEControlFlag;
  TF16CControlFlags = set of TF16CControlFlag;

  TF16CException  = TSSEException;
  TF16CExceptions = set of TF16CException;

const
  F16CExceptionsAll = [Low(TF16CException)..High(TF16CException)];

//------------------------------------------------------------------------------
{
  F16CRoundingModeGet

  Returns current value of F16C rounding mode.
}
Function F16CRoundingModeGet: TF16CRoundingMode;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F16CRoundingModeSet

  Sets F16C rounding mode to a selected NewValue and returns its previous value.
}
Function F16CRoundingModeSet(NewValue: TF16CRoundingMode): TF16CRoundingMode;

{
  F16CControlFlagGet

  Returns current value of selected F16C control flag.
}
Function F16CControlFlagGet(Flag: TF16CControlFlag): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F16CControlFlagSet

  Sets value of selected F16C control flag to a NewValue and returns previous
  state of this flag.
}
Function F16CControlFlagSet(Flag: TF16CControlFlag; NewValue: Boolean): Boolean;

{
  F16CControlFlagsGet

  Returns status of all F16C control flags. When the flag is set, it is
  included in the result, when it is clear, it is excluded.
}
Function F16CControlFlagsGet: TF16CControlFlags;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F16CControlFlagsSet

  Sets new status of all F16C control flags. If a flag is included in the
  NewValue, it will be set, when it is not included, it will be cleared.
  Returns previous state of all control flags.
}
Function F16CControlFlagsSet(NewValue: TF16CControlFlags): TF16CControlFlags;

//------------------------------------------------------------------------------
{
  F16CExceptionMaskGet

  Returns current value of selected F16C exception mask bit.
}
Function F16CExceptionMaskGet(Exception: TF16CException): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F16CExceptionMaskSet

  Sets value of selected F16C exception mask bit to a NewValue and returns
  previous value of this bit.

  When the bit is set (true), the selected exception will be masked and not
  raised on its occurence. When clear (false), the exception is unmasked and
  can be raised.
}
Function F16CExceptionMaskSet(Exception: TF16CException; NewValue: Boolean): Boolean;

{
  F16CExceptionMasksGet

  Returns status of all F16C exception mask bits. When the bit is set, the
  exception is included in the result, when it is clear, the exception is
  excluded from the result.
}
Function F16CExceptionMasksGet: TF16CExceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F16CExceptionMasksSet

  Sets new value of all F16C exception mask bits. If an exception is included
  in the NewValue, the mask bit will be set for that particular exception, when
  it is not included, the mask bit will be cleared.

  Return value is previous state of all exception mask bits.
}
Function F16CExceptionMasksSet(NewValue: TF16CExceptions): TF16CExceptions;

{
  F16CExceptionFlagGet

  Returns current value of selected F16C exception flag bit.

  If the bit is set (True returned), it means the selected exception was
  encountered, but not raised (the exception handler was not called, eg.
  because that exception is masked).
}
Function F16CExceptionFlagGet(Exception: TF16CException): Boolean;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F16CExceptionFlagSet

  Sets value of selected F16C exception flag bit to a NewValue and returns
  previous value of this bit.

  Setting exception flag bit to 1 has no immediate effect, even if that
  particular exception is unmasked, because vector instructions do not check
  for and raise pending exceptions.
}
Function F16CExceptionFlagSet(Exception: TF16CException; NewValue: Boolean): Boolean;

{
  F16CExceptionFlagsGet

  Returns status of all F16C exception flag bits. When the bit is set, the
  exception is included in the result, when it is clear, the exception is
  excluded from the result.
}
Function F16CExceptionFlagsGet: TF16CExceptions;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{
  F16CExceptionFlagsSet

  Sets new value of all F16C exception flag bits. If an exception is included
  in the NewValue, the flag bit will be set for that particular exception, when
  it is not included, the flag bit will be cleared.

  Returns state of all exception flag bits.
}
Function F16CExceptionFlagsSet(NewValue: TF16CExceptions): TF16CExceptions;

{
  F16CExceptionsClear

  Clears all exception flag bits in currently used control and status register.
}
procedure F16CExceptionsClear;

{===============================================================================
--------------------------------------------------------------------------------
                     Float16 <-> Float32 conversions (F16C)
--------------------------------------------------------------------------------
===============================================================================}
{
  Set of functions providing conversion between half precision (16bit) floating
  point numbers (Half, Float16) and single precision (32bit) floating point
  numbers (Single, Float32).

  These are here because there are, as far as I know, no compilers that can
  do this implicitly, mainly because the 16bit float type is not supported by
  broadly available hardware.
  That being said, newer x86 processors implement instruction set extentions
  that provide support either for conversion between float16 and float32
  (F16C extension) or even for basic arithmetics (AVX512-FP16 extension).

  The conversion routines are subject to UIM (unit implementation management),
  which means you can select which implementation will be called - you can
  select between pure pascal and assembly.

  Assembly implementation currently uses F16C extension where available (it
  is working on float vector registers, here on XMM), namely instructions
  VCTVPH2PS and VCTVPS2PH (VEX-encoded variants).

    WARNING - not all processors must support this extension, check its
              availability before selecting asm implementetation (you can
              use UIM function UIM_FloatUtils_SupportedFuncImpl for that).

  Pascal is doing the conversions using only integer arithmetics and logical
  operations (the number is decoded, its parts processed and then the result
  is constructed from them). It was written to emulate current assembly
  implementation as close as possible, which means following things:

    The exceptions raising corresponds to F16C extension, which differs from
    usual float conversions (eg. between float32 and float64) done on x87 FPU
    or SSE/AVX units. It also differs from float16 conversions done by FP16
    extension from AVX512 (EVEX-encoded instructions VCVTPH2PSX and VCVTPS2PHX),
    be aware of that. For details about these differences please refer to
    documentation of F16C extension (eg. in Intel Developer's manual).

    Even though pre-computation and post-computation nature of exceptions is
    observed, the result (output) will never be updated, even if only post-
    computation exception is raised. This is because assembly does the
    conversion purely in registers and only after that stores the result into
    output. So, if the conversion raises an exception, the storing is skipped
    and therefore the output is never updated.

  As in float80 conversions, both functions accepting floats or pointers to
  them are provided. But again, make sure the pointers are pointing to correct
  value or memory location of appropriate size.
}
procedure Float16ToFloat32(Float16Ptr,Float32Ptr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function Float16ToFloat32(const Value: Float16): Float32; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

procedure HalfToSingle(HalfPtr,SinglePtr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function HalfToSingle(const Value: Half): Single; overload;

procedure Float32ToFloat16(Float32Ptr,Float16Ptr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function Float32ToFloat16(const Value: Float32): Float16; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

procedure SingleToHalf(SinglePtr,HalfPtr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function SingleToHalf(const Value: Single): Half; overload;

//------------------------------------------------------------------------------
{
  Following functions are expecting pointers to packed vector of four singles
  (SingleVec4Ptr, Float32Vec4Ptr) and packed vector of four halfs (HalfVec4Ptr,
  Float16Vec4Ptr).

  Pascal implementation merely does four consecutive conversions, but in
  assembly, all four conversions are done in a single instruction. This is
  because the assembly implementation uses F16C extension to SIMD floating
  point vector units (SSE/AVX), therefore single instruction operates on all
  four values.

  There is a posibility to operate on 8 or even 16 values with newer vector
  units, but currently the implementation is written to operate on XMM (128bit)
  registers and I have no plans of writing for wider operations.
}
procedure Float16ToFloat32Vec4(Float16Vec4Ptr,Float32Vec4Ptr: Pointer);{$IFDEF CanInlineFPC} inline;{$ENDIF}
procedure HalfToSingleVec4(HalfVec4Ptr,SingleVec4Ptr: Pointer);{$IFDEF CanInlineFPC} inline;{$ENDIF}

procedure Float32ToFloat16Vec4(Float32Vec4Ptr,Float16Vec4Ptr: Pointer);{$IFDEF CanInlineFPC} inline;{$ENDIF}
procedure SingleToHalfVec4(SingleVec4Ptr,HalfVec4Ptr: Pointer);{$IFDEF CanInlineFPC} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                Float16 utilities
--------------------------------------------------------------------------------
===============================================================================}
{
  Following few functions are here to provide some basic logic and arithmetic
  operations working on half precision (16bit) floating point numbers.

  In current implementation, all float16 input arguments are converted to 32bit
  floats and the corresponding operations are performed on those 32bit floats
  using standard language constructs/operators or RTL functions. If operation
  produces new (32bit) floating point value, then this new value is converted
  back to float16 which is then returned.
  This is so because there is no wide-spread hardware that supports operations
  on half precision floating point numbers, and writing them in software is way
  beyond the scope of this library.
  Note that I am aware of AVX512-FP16 instruction set extension which provides
  all that is needed here, but it is rather new feature and I am not sure how
  many existing processors support it. Therefore I have decided to not use it.

  What the functions do should be completely obvious from their names, so most
  of them is not described (if not sure, ask author or consult source code).
}
{===============================================================================
    Float16 utilities - comparison functions declaration
===============================================================================}

Function IsEqual(const A,B: Float16): Boolean;{$IFDEF CanInline} inline;{$ENDIF}          // A = B
Function IsLess(const A,B: Float16): Boolean;{$IFDEF CanInline} inline;{$ENDIF}           // A < B
Function IsGreater(const A,B: Float16): Boolean;{$IFDEF CanInline} inline;{$ENDIF}        // A > B
Function IsLessOrEqual(const A,B: Float16): Boolean;{$IFDEF CanInline} inline;{$ENDIF}    // A <= B
Function IsGreaterOrEqual(const A,B: Float16): Boolean;{$IFDEF CanInline} inline;{$ENDIF} // A >= B

//------------------------------------------------------------------------------
type
  TFUValueRelationship = -1..1;
{
  CompareValue

  Retuns -1 (or, generally, a negative value) if the first value (A) is smaller
  than the second value (B), +1 (positive value) when A is larger than B and 0
  (zero) when the two given value are equal.

  Value of Epsilon (both in CompareValue and SameValue) is maximum amount by
  which A and B can differ and still be considered the same value.
}
Function CompareValue(const A,B: Float16; const Epsilon: Float16): TFUValueRelationship; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function CompareValue(const A,B: Float16): TFUValueRelationship; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

Function SameValue(const A,B: Float16; const Epsilon: Float16): Boolean; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function SameValue(const A,B: Float16): Boolean; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{===============================================================================
    Float16 utilities - basic arithmetic functions declaration
===============================================================================}

Function Add(const A,B: Float16): Float16;{$IFDEF CanInline} inline;{$ENDIF}      // A + B
Function Subtract(const A,B: Float16): Float16;{$IFDEF CanInline} inline;{$ENDIF} // A - B
Function Multiply(const A,B: Float16): Float16;{$IFDEF CanInline} inline;{$ENDIF} // A * B
Function Divide(const A,B: Float16): Float16;{$IFDEF CanInline} inline;{$ENDIF}   // A / B

{$IFDEF FPC}
{-------------------------------------------------------------------------------
================================================================================
                          Float16 operators overloading
================================================================================
-------------------------------------------------------------------------------}
{
  Operators overloading is currently implemented only for FPC (afaik Delphi
  does not have operator overloading for primitive types and arrays), and it
  functions merely as a wrapper around float16 utility functions (eg. IsLess,
  IsEqual, Add, Multiply, ...).
}
// assignment operators
operator := (Value: Half): Single;{$IFDEF CanInline} inline;{$ENDIF}
operator := (Value: Single): Half;{$IFDEF CanInline} inline;{$ENDIF}

// explicit assignment operators
operator explicit (Value: Half): Single;{$IFDEF CanInline} inline;{$ENDIF}
operator explicit (Value: Single): Half;{$IFDEF CanInline} inline;{$ENDIF}

// comparison operators
operator = (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator > (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator < (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator >= (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator <= (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
operator <> (A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

// unary operators
operator + (A: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator - (A: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}

// arithmetic (binary) operators
operator + (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator - (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator * (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator / (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}

{$ENDIF}
{===============================================================================
--------------------------------------------------------------------------------
                  F32 <-> F64 conversion (FXC) state management
--------------------------------------------------------------------------------
===============================================================================}
{
  Conversions between single precision (float32) and double precision (float64)
  floating point numbers (FXC) are implemented in four ways.
  Two are in assembly, where one uses x87 floating point unit to do the
  conversion and the other is using SSE vector unit (specifically SSE2
  extension, instructions CVTSD2SS and CVTSS2SD).
  The remaining two are implemented in pascal, using only integer arithmetics
  and logical/binary operations. One pascal implementation emulates conversion
  as performed by x87, the other emulates behavior of SSE2 instructions.

  This means, among others, that you must take care what implementation you
  select, as behavior between x87 and SSE versions, be it assembly or pascal
  code, is different.

      NOTE - emulated states are automatically initialized, there is no need to
             explicitly initialize them.

      NOTE - the four states do not share information. If you make change in
             one state and then switch implementation routing, the changes
             will not be propagated and the conversion will now use different
             state with possibly different settings.

      WARNING - the states are instantiated per-thread, meaning each thread
                has its own setting - change made in one thread is not seen
                by any other thread.
}
{
  TFXCModeOfOperation

  This enumeration is used to indicate in what mode the single <-> double
  conversions (FXC) are currently running (ie. what implememtation is selected
  to execute).
}
type
  TFXCModeOfOperation = (modPascalX87,modAssemblyX87,modPascalSSE,modAssemblySSE);

{
  TFXCControlAndStatus

  This type is used when getting or setting the FXC state, be it emulated or
  hardware state.

    NOTE - take great care to access only fields that are valid for given
           mode of operation.
}
  TFXCControlAndStatus = record
    case ModeOfOperation: TFXCModeOfOperation of
      modPascalX87,modAssemblyX87: (
        StatusWord:       UInt16;
        ControlWord:      UInt16);
      modPascalSSE,modAssemblySSE: (
        ControlAndStatus: UInt32);
  end;

{===============================================================================
    FXC state management - exceptions declaration
===============================================================================}
type
  TFXCFlagException  = TFUNumericException;
  TFXCFlagExceptions = set of TFXCFlagException;

  TFXCRaiseException  = TFUException; // including stack faults
  TFXCRaiseExceptions = set of TFXCRaiseException;

{
  EFXCException

  Common ancestor class for all exceptions raised by pascal-implemented FXC
  conversions when the process encounters an exceptions condition.

  Multiple different floating point exceptions might be signaled at the same
  time, use provided properties to discern what error conditions were signaled
  when the exception was raised.
  Be aware that these exceptions can be raised both from x87 emulation and SSE2
  emulation, and signaled exceptions will correspond to that (eg. SSE will
  never signal stack faults). You can use property ControlAndStatus, field
  ModeOfOperation do discern in what mode the exception was raised.
  Also remember that different modes provide status data in different manner.
  SSE emulation stores its state in ControlAndStatus field, x87 emulation in
  fields ControlWord and StatusWord of ControlAndStatus property.

    NOTE - when instantiating this class (creating object), functions
           FXCEnvironmentInit and FXCControlAndStatusInit are called
           (in that order), clearing all exception flags and setting
           default exception mask, control flags and rounding mode.
}
type
  EFXCException = class(EFUEmulationException)
  protected
    fControlAndStatus:  TFXCControlAndStatus;
    fPendingExcs:       TFXCFlagExceptions;
    fMaskedExcs:        TFXCFlagExceptions;
    fUnmaskedExcs:      TFXCFlagExceptions;
    fRaisedExcs:        TFXCRaiseExceptions;
    procedure Initialize; override;
  public
    property ControlAndStatus: TFXCControlAndStatus read fControlAndStatus;
    property PendingExceptions: TFXCFlagExceptions read fPendingExcs;
    property MaskedExceptions: TFXCFlagExceptions read fMaskedExcs;
    property UnmaskedExceptions: TFXCFlagExceptions read fUnmaskedExcs;
    property RaisedExceptions: TFXCRaiseExceptions read fRaisedExcs;
  end;

//------------------------------------------------------------------------------
{
  Common ancestor EFXCException is not internally instantiated, instead the
  process selects one of the following classes to suit the encountered error
  state.
  If multiple floating point exceptions are signaled at the same time, then
  the class is selected according to floating point exception priority (see
  implementation for details).
}
type
  EFXCInvalidOp = class(EFXCException);
  EFXCDenormal  = class(EFXCException);
  EFXCDivByZero = class(EFXCException);
  EFXCOverflow  = class(EFXCException);
  EFXCUnderflow = class(EFXCException);
  EFXCPrecision = class(EFXCException);

  EFXCStackFault     = class(EFXCException);
  EFXCStackUnderflow = class(EFXCStackFault);
  EFXCStackOverflow  = class(EFXCStackFault);

{===============================================================================
    FXC state management - low-lewel access declaration
===============================================================================}
{
  FXCControlAndStatus

  These functions are provided as inline constructors for argument NewValue of
  type TFXCControlAndStatus as passed to FXCControlAndStatusSet.

  They properly set field ModeOfOperation according to provided data - that
  being said, make sure you use proper overload for current mode of operation
  (see description of mentioned function for more details).
}
Function FXCControlAndStatus(StatusWord,ControlWord: UInt16): TFXCControlAndStatus; overload;
Function FXCControlAndStatus(StatusAndControl: UInt32): TFXCControlAndStatus; overload;

//------------------------------------------------------------------------------
{
  FXCEmulated

  Returns True when FXC conversions are routed to an emulated implementation,
  False when routed to assembly code.
}
Function FXCEmulated: Boolean;

{
  FXCModeOfOperation

  Indicates what implementation is currently selected for FXC conversions.
}
Function FXCModeOfOperation: TFXCModeOfOperation;

{
  FXCControlAndStatusGet

  Returns state used by currently selected FXC implementation. What is returned
  and how depends on current mode of operation - you can discern it from field
  ModeOfOperation within the returned structure.

    modPascalX87    - fields StatusWord and ControlWord will contain emulated
                      status and control words respectively.

    modAssemblyX87  - field StatusWord will contain value of x87 FPU status
                      word registe, field ControlWord will cantain value of x87
                      control word register.

    modPascalSSE    - field ControlAndStatus will contain value of emulated
                      control and status register.

    modAssemblySSE  - field ControlAndStatus will contain value of vector unit
                      (SSE/AVX) control and status register (register MXCSR).

    NOTE - make sure you only access the correct fields for indicated mode of
           operation. Values in other fields are undefined.
}
Function FXCControlAndStatusGet: TFXCControlAndStatus;

{
  FXCControlAndStatusSet

  Changes state currently used by the FXC conversions.

    NOTE - in x87 modes, this function can raise pending unmasked exceptions.

    WARNING - you cannot use this function to change mode of operation, that
              can only be changed using UIM functions selecting implementation
              for fnFloatXConversions function group.

  You must provide new value that is compatible with current mode of operation,
  otherwise an EFUInvalidValue exception will be raised.

    For modPascalX87 and modAssemblyX87, the NewValue must have field
    ModeOfOperation set to either modPascalX87 or modAssemblyX87, and new
    state must be provided in field ControlWord (field StatusWord is ignored
    here).

    Current mode of operation of modPascalSSE and modAssemblySSE requires the
    ModeOfOperation to be again set to either modPascalSSE or modAssemblySSE
    and state to be provided in field ControlAndStatus.
}
procedure FXCControlAndStatusSet(NewValue: TFXCControlAndStatus);

{
  FXCEnvironmentInit

  Initializes currently used FXC state, irrespective of its nature.

  For more details, you can refer to following functions:

      X87EnvironmentInit
      F80CEnvironmentInit
      SSEEnvironmentInit/AVXEnvironmentInit
      F16CEnvironmentInit

  ..., as the actual initialization directly corresponds to those functions
  (depending on current mode of operation).
}
procedure FXCEnvironmentInit;

{
  FXCControlAndStatusInit

  Sets currently used FXC state to a ready-to-use value.

  As in the case of FXCEnvironmentInit, you can refer to other corresponding
  functions for more details. Those functions are:

      X87ControlWordInit
      F80CControlWordInit
      SSEControlAndStatusInit/AVXControlAndStatusInit
      F16CControlAndStatusInit

  Note that in some cases (x87), this function can raise pending unmasked
  exceptions.
}
procedure FXCControlAndStatusInit;

{===============================================================================
    FXC state management - abstracted access declaration
===============================================================================}
type
  TFXCRoundingMode = TFURoundingMode;

{
  Only flags (both status and control) that are really affecting anything are
  included.
}
  TFXCStatusFlag  = sfStackFault..sfConditionCodeC1;
  TFXCStatusFlags = set of TFXCStatusFlag;

  TFXCControlFlag  = cfDenormalsAreZeros..cfFlushToZero;
  TFXCControlFlags = set of TFXCControlFlag;

  TFXCException  = TFUNumericException;
  TFXCExceptions = set of TFXCException;

const
  FXCExceptionsAll = [Low(TFXCException)..High(TFXCException)];

//------------------------------------------------------------------------------
{
  FXCStatusFlagGet

  Returns state of the selected status flag in the FXC state.

  What flags are supported depends on current mode of operation - unsupported
  flags will always read as false.

  Implementation in x87 assembly or x87 emulation supports all curently defined
  status flags, SSE implementations supports none.
}
Function FXCStatusFlagGet(Flag: TFXCStatusFlag): Boolean;

{
  FXCStatusFlagsGet

  Returns status of all known status flags in the FXC state.

  If the flag is supported and is set in the state, it will be included in the
  result, otherwise it will be excluded.
}
Function FXCStatusFlagsGet: TFXCStatusFlags;

{
  FXCRoundingModeGet

  Returns rounding mode as set in the current FXC state, irrespective of its
  nature.
}
Function FXCRoundingModeGet: TFXCRoundingMode;

{
  FXCRoundingModeSet

  Changes rounding mode used by FXC to a value given in argument NewValue and
  returns its previous value.
}
Function FXCRoundingModeSet(NewValue: TFXCRoundingMode): TFXCRoundingMode;

{
  FXCControlFlagGet

  Returns state of the selected control flag in the FXC state.

  Set of supported flags depends on current mode of operation, unsupported
  flags will always be raported as clear (False).

  Implementation in x87 assembly or x87 emulation currently does not support
  any control flag, they are all meant for SSE implementation.
}
Function FXCControlFlagGet(Flag: TFXCControlFlag): Boolean;

{
  FXCControlFlagSet

  Changes state of selected control flag in FXC stat to a NewValue and returns
  its previous state.

  If selected flag is not supported by current mode of operation, then the
  function returns false, and nothing is changed in the state.
}
Function FXCControlFlagSet(Flag: TFXCControlFlag; NewValue: Boolean): Boolean;

{
  FXCControlFlagsGet

  Returns status of all known control flags in the FXC state.

  If the flag is supported and is set in the state, it will be included in the
  result, otherwise it will be excluded.
}
Function FXCControlFlagsGet: TFXCControlFlags;

{
  FXCControlFlagsSet

  Changes state of all control flags in FXC state to a NewValue and returns
  their previous state. If a flag is included in the NewValue, it will be set,
  when it is not included, it will be cleared.

  Flags unsupported by the current mode of operation are ignored and never
  included in resulting set.
}
Function FXCControlFlagsSet(NewValue: TFXCControlFlags): TFXCControlFlags;

//------------------------------------------------------------------------------
{
  FXCExceptionMaskGet

  Returns current value of selected FXC exception mask bit.
}
Function FXCExceptionMaskGet(Exception: TFXCException): Boolean;

{
  FXCExceptionMaskSet

  Sets value of selected FXC exception mask bit to a NewValue and returns
  previous value of this bit.

  When the bit is set (true), the selected exception will be masked and not
  raised. When clear (false), the exception is unmasked and can be raised.
}
Function FXCExceptionMaskSet(Exception: TFXCException; NewValue: Boolean): Boolean;

{
  FXCExceptionMasksGet

  Returns status of all FXC exception mask bits. When the bit is set, the
  exception is included in the result, when it is clear, the exception is
  excluded from the result.
}
Function FXCExceptionMasksGet: TFXCExceptions;

{
  FXCExceptionMasksSet

  Sets new value of all FXC exception mask bits. If an exception is included
  in the NewValue, the mask bit will be set for that particular exception, when
  it is not included, the mask bit will be cleared.

  Return value is previous state of all exception mask bits.
}
Function FXCExceptionMasksSet(NewValue: TFXCExceptions): TFXCExceptions;

{
  FXCExceptionFlagGet

  Returns current value of selected FXC exception flag bit.

  If the bit is set (True returned), it means the selected exception was
  encountered, but not raised.
}
Function FXCExceptionFlagGet(Exception: TFXCException): Boolean;

{
  FXCExceptionFlagSet

  Sets value of selected FXC exception flag bit to a NewValue and returns
  previous value of this bit.

  This function works only when current mode of operation is working with (or
  emulating) SSE state. In x87 modes, it does nothing and returns current value
  of the selected exception flag bit.
}
Function FXCExceptionFlagSet(Exception: TFXCException; NewValue: Boolean): Boolean;

{
  FXCExceptionFlagsGet

  Returns status of all FXC exception flag bits. When the bit is set, the
  exception is included in the result, when it is clear, the exception is
  excluded from the result.
}
Function FXCExceptionFlagsGet: TFXCExceptions;

{
  FXCExceptionFlagsSet

  Sets new value of all FXC exception flag bits. If an exception is included
  in the NewValue, the flag bit will be set for that particular exception, when
  it is not included, the flag bit will be cleared.

  This function changes the bits only when operating on or emulating SSE state,
  it does nothing for x87 modes of operation.

  Returns state of all exception flag bits.
}
Function FXCExceptionFlagsSet(NewValue: TFXCExceptions): TFXCExceptions;

{
  FXCExceptionsClear

  Clears all exception flag bits in FXC state.
}
procedure FXCExceptionsClear;

{
  FXCExceptionsRaise

  In x87 modes, it raises unmasked pending exceptions. In SSE modes of
  operation, it does nothing - no exceptions are raised even if there are
  unmasked pending exceptions in the current state.

  Note that when an exception is raised, then the state is reinitialized. This
  clears exception flags and affects other bits and fields within the state.
}
procedure FXCExceptionsRaise;

{===============================================================================
--------------------------------------------------------------------------------
                     Float32 <-> Float64 conversions (FXC)
--------------------------------------------------------------------------------
===============================================================================}
{
  Following functions can be used for explicit conversion between single
  precision (32bit, float32) and double precision (64bit, float64) floating
  point numbers.
  These conversions are fully supported by the programming language itself,
  but there might be situations where one does not want to use the implicit
  conversions - eg. to have more control over specific settings.

  These conversion routines are subject to UIM (unit implementation management),
  so you can select which implementation will be called. Currently there are
  four implementations provided - see description of FXC state management
  section for more details.

  When selecting any assembly implementation, make sure your hardware supports
  that (you can use UIM functions for that purpose).
}
procedure Float32ToFloat64(Float32Ptr,Float64Ptr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function Float32ToFloat64(Value: Float32): Float64; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

procedure SingleToDouble(SinglePtr,DoublePtr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function SingleToDouble(Value: Single): Double; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure Float64ToFloat32(Float64Ptr,Float32Ptr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function Float64ToFloat32(Value: Float64): Float32; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

procedure DoubletoSingle(DoublePtr,SinglePtr: Pointer); overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}
Function DoubletoSingle(Value: Double): Single; overload;{$IFDEF CanInlineFPC} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              State synchronization
--------------------------------------------------------------------------------
===============================================================================}
{
  Conversion routines (eg. from and to half-precision floating point numbers)
  provided by this library are controlled using semiglobal states (each state
  is global within the context of executing thread, not trully global for the
  entire process). Each conversion can operate either using hardware state or
  its own emulated state. Emulated states are separate for each conversion
  flavour (eg. F16C state is distinct from FXC state), whereas hardware states,
  given their nature, are shared between conversions that are using the same
  hardware (ie. there is only one SSE/AVX unit, so everyone is using that one).

  State synchronization mechanism is here to provide means of comparing two
  states and potentially copying settings from one state to another, no matter
  their nature.
}
{
  TFUStateSyncTarget

  This enumeration type is used to identify source and/or destination state
  (in-here termed targets) when doing the synchronization action.

  Individual values can be ambiguous ir unambiguous. Unambiguous values always
  identify the same target, whereas ambiguous can identify different targets
  depending on other circumstances - function StateSyncResolveTarget can be
  used to convert ambiguous target to unambiguous (refer to its description
  for more details, ie. which targets are ambiguous and to which unambiguous
  targets they resolve under which circumstances).

  Two targets (and therefore states) can be either compatible or incompatible.
  When comparing or copying settings between compatible targets, then all used
  (ie. used by at least one conversion process) flags and fields within those
  states are compared or copied (unused are ignored or preserved in the
  destination). Between incompatible targets, only the common flags and fields
  are compared or copied - these are currently all exception flags, all
  exception masks and rounding mode, nothing more.
  You can use funtion StateSyncCompatibleTargets to test whether two given
  targets are compatible or not.

  At this moment, there are following two groups of mutualy compatible targets
  (all targets within one group are compatible, between groups none are - note
  that only unambiguous targets are listed, ambiguous targets always resolve
  to one of them so they are not listed):

    * sstF80CEmulated, sstF80CNative, sstFXCEmulatedX87, sstFXCNativeX87,
      sstX87

        Targets within this group can compare or exchange all exception flags,
        all exception masks, rounding mode, precision mode, stack fault, FPU
        busy and exception summary status flags and C1 condition code.

    * sstF16CEmulated, sstF16CNative, sstFXCEmulatedSSE, sstFXCNativeSSE,
      sstSSE, sstAVX

        These targets can compare or exchange all exception flags, all
        exception masks, rounding mode and DAZ and FTZ control flags.

  "CntrState" or "counter-state" here means natural counterpart for currently
  used state (eg. native SSE state for emulated SSE state).
}
type
  TFUStateSyncTarget = (sstF80CState,sstF16CState,sstFXCState,sstF80CCntrState,
                        sstF16CCntrState,sstFXCCntrState,sstF80CEmulated,
                        sstF16CEmulated,sstFXCEmulatedX87,sstFXCEmulatedSSE,
                        sstF80CNative,sstF16CNative,sstFXCNativeX87,
                        sstFXCNativeSSE,sstX87,sstSSE,sstAVX);

{
  StateSyncResolveTarget

  Resolves ambiguous targets to unambiguous targets based on several global
  variables. If unambiguous value is given, then it is returned without change.

  Following value swaps are performed in current implementation:

    sstF80CState     ... F80CEmulated = True  -> sstF80CEmulated
                         F80CEmulated = False -> sstF80CNative (sstX87)

    sstF16CState     ... F16CEmulated = True  -> sstF16CEmulated
                         F16CEmulated = False -> sstF16CNative (sstAVX)

    sstFXCState      ... FXCModeOfOperation = modPascalX87   -> sstFXCEmulatedX87
                         FXCModeOfOperation = modAssemblyX87 -> sstFXCNativeX87 (sstX87)
                         FXCModeOfOperation = modPascalSSE   -> sstFXCEmulatedSSE
                         FXCModeOfOperation = modAssemblySSE -> sstFXCNativeSSE (sstSSE)

    sstF80CAntiState ... F80CEmulated = True  -> sstF80CNative (sstX87)
                         F80CEmulated = False -> sstF80CEmulated

    sstF16CAntiState ... F16CEmulated = True  -> sstF16CNative (sstAVX)
                         F16CEmulated = False -> sstF16CEmulated

    sstFXCAntiState  ... FXCModeOfOperation = modPascalX87   -> sstFXCNativeX87 (sstX87)
                         FXCModeOfOperation = modAssemblyX87 -> sstFXCEmulatedX87
                         FXCModeOfOperation = modPascalSSE   -> sstFXCNativeSSE (sstSSE)
                         FXCModeOfOperation = modAssemblySSE -> sstFXCEmulatedSSE

  Argument FinalTarget, when set to true, will cause that native targets (eg.
  sstFXCNativeX87) are resolved to their final hardware targets (for mentioned
  sstFXCNativeX87 it would be sstX87) - above, these final targets are listed
  in brackets.
}
Function StateSyncResolveTarget(SyncTarget: TFUStateSyncTarget; FinalTarget: Boolean = False): TFUStateSyncTarget;

{
  StateSyncCompatibleTargets

  Indicates whether two given targets are compatible (True returned) or not
  (False) - see description of type TFUStateSyncTarget for explanation of
  compatibility.

  Ambiguous targets are automatically resolved before compatibility check and
  can therefore be used for arguments.
}
Function StateSyncCompatibleTargets(SyncTargetA,SyncTargetB: TFUStateSyncTarget): Boolean;

{
  StateSyncDistinctTargets

  Inidicates whether the two given targets identify two distinct states, taking
  into account ambiguity of some targets and also fact that SSE and AVX units
  are using the same control and status register (MXCSR), meaning their states
  are technically one and the same (and so here targets sstSSE and sstAVX are
  also seen to be the same).

  Ambiguous targets can be used without any limitation.
}
Function StateSyncDistinctTargets(SyncTargetA,SyncTargetB: TFUStateSyncTarget): Boolean;

//------------------------------------------------------------------------------
{
  TFUStateSyncBufferPayloadType

  Simple enum that is used to mark what type of data a TFUStateSyncBuffer
  variable holds.
}
type
  TFUStateSyncBufferPayloadType = (bptX87,bptMXCSR);

{
  TFUStateSyncBuffer

  This type is used as a storage space for state that is being compared or
  copied to some other state.
  
  Do not directly access its fields as it can be changed in the future (exempt
  from this rule is field SourceTarget - it is guaranteed to be always present,
  so you are explicitly allowed to use it).

    WARNING - do not use this buffer for streaming or IO, it is meant only as
              an immediate storage.
}
type
  TFUStateSyncBuffer = record
    SourceTarget:     TFUStateSyncTarget;
    case PayloadType: TFUStateSyncBufferPayloadType of
      bptX87:   (StatusWord:        UInt16;
                 ControlWord:       UInt16);
      bptMXCSR: (ControlAndStatus:  UInt32);
  end;
  PFUStateSyncBuffer = ^TFUStateSyncBuffer;

{
  StateSyncSave

  Stores information (settings) from state selected by SyncTarget into provided
  storage buffer.

  Ambiguous targets can be used as they are automatically resolved.
}
procedure StateSyncSave(SyncTarget: TFUStateSyncTarget; out Buffer: TFUStateSyncBuffer);

{
  StateSyncLoad

  Loads compatible settings from provided storage buffer into state selected
  by SyncTarget. Make sure the buffer was previously filled by StateSyncSave,
  otherwise behavior of this function is undefined and how the selected state
  will be changed is completely unpredictable.

    WARNING - what settings are stored in the buffer depends on what state
              was previously saved there - be aware of that, because loading
              it into incompatible state might not have expected effect as
              not all used fields will be updated/changed.
              You can use function StateSyncCompatibleTargets on buffer field
              SourceTarget (unless you changed it) to discern whether the
              buffer is compatible with SyncTarget or not. 

  Note that loading the buffer does not invalidate its content, it can be used
  as many time as needed (eg. loaded into several different states or kept as
  state backup).

  Ambiguous targets are automatically resolved.
}
procedure StateSyncLoad(SyncTarget: TFUStateSyncTarget; const Buffer: TFUStateSyncBuffer);

{
  StateSyncCompare

  Compares compatible settings from provided storage buffer with corresponding
  bits and fields in the state selected by SyncTarget. If all settings match,
  then True is returned. If any setting differs, then False is returned.

  Ambiguous targets are automatically resolved.
}
Function StateSyncCompare(SyncTarget: TFUStateSyncTarget; const Buffer: TFUStateSyncBuffer): Boolean;

//------------------------------------------------------------------------------
{
  TFUStateSyncAction

  This enumeration is used to select which action should be performed when
  doing states synchronization.

    ssaCompare

      The two synchronized states are compared. If they match, true is returned,
      otherwise false is returned.
      The states are not compared in their entirety - depending on whether they
      are compatible or not, only selected flags and fields are compared (see
      TFUStateSyncTarget for compatibility info).

    ssaCompareStrict

      Same as ssaCompare, but in case the states do not match an exception of
      class EFUStateMismatch is raised.

    ssaCopyToCurrent

      Copies selected settings (see compatibility info in description of type
      TFUStateSyncTarget) from source target to destination target. Flags and
      fields not copied are preserved in the destination state.

    ssaCopyFromCurrent

      Copies selected settings from destination target to source target (yes,
      that way). Flags and fields not copied are preserved in the source state.

  The enumerations are named this way ("to current", "from current") because
  primary interface of this entire section (functions F*CStateSynchronize)
  is working with implicit targets, not explicitly selected ones - there, the
  implicit target is the "current".

    NOTE - all functions working around this type (ie. *StateSynchronize
           functions) are just macros using StateSyncSave, StateSyncLoad
           and StateSyncCompare.
}
type
  TFUStateSyncAction = (ssaCompare,ssaCompareStrict,ssaCopyToCurrent,
                        ssaCopyFromCurrent);

{
  StateSynchronize

  Performs synchronizing action prescribed by argument SyncAction on the two
  given targets. For return values and other information about the actions,
  see description of type TFUStateSyncAction.

  Ambiguous targets are automatically resolved.
}
Function StateSynchronize(SyncDestination: TFUStateSyncTarget; SyncSource: TFUStateSyncTarget; SyncAction: TFUStateSyncAction): Boolean;

//------------------------------------------------------------------------------
{
  F80CStateSynchronize

  First overload executes selected synchronization action on state that is
  currently used for float80 conversions (F80C). Destination target is the used
  state, source is its currently unused counterpart (eg. if emulated state is
  being used, then source target is x87 FPU state).

  Second overload works the same, but source target is explicitly selected by
  argument SyncSource.

  Ambiguous targets are automatically resolved.
}
Function F80CStateSynchronize(SyncAction: TFUStateSyncAction): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function F80CStateSynchronize(SyncSource: TFUStateSyncTarget; SyncAction: TFUStateSyncAction): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{
  F16CStateSynchronize

  Works the same as F80CStateSynchronize, except that these functions are
  working on a state used by float16 conversions (F16C).

  Ambiguous targets are automatically resolved.
}
Function F16CStateSynchronize(SyncAction: TFUStateSyncAction): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function F16CStateSynchronize(SyncSource: TFUStateSyncTarget; SyncAction: TFUStateSyncAction): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{
  FXCStateSynchronize

  Works the same as F80CStateSynchronize, but these functions are working on
  a state used by float32 <-> float64 conversions (FXC).

  Ambiguous targets are automatically resolved.
}
Function FXCStateSynchronize(SyncAction: TFUStateSyncAction): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function FXCStateSynchronize(SyncSource: TFUStateSyncTarget; SyncAction: TFUStateSyncAction): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                 Floats mapping
--------------------------------------------------------------------------------
===============================================================================}
{
  Following set of functions provides a mean of mapping floating point numbers
  directly to integral types - note they are NOT converting the values, they
  are just moving the data. The returned integers will have the same bit
  pattern as given float numbers.
  This can be used eg. when probing individual parts of floating point format
  (sign, exponent, mantissa/fraction).

  Since 80bit floats cannot be directly mapped to any existing simple integral
  type (because they are to large), they are split into two parts - higher 16
  bits (sign with exponent) and lower 64 bits (mantissa). When mapping to
  float80, both parts must also be explicitly provided.

    NOTE - when mapping to and from type extended, a F64 <-> F80 conversion
           will be performed if this type is declared only as an alias to
           double (64bit float).
}

procedure MapToFloat16Buffer(Value: UInt16; out Buffer);
Function MapToFloat16(Value: UInt16): Float16;{$IFDEF CanInline} inline;{$ENDIF}
Function MapToHalf(Value: UInt16): Half;{$IFDEF CanInline} inline;{$ENDIF}

Function MapFromFloat16Buffer(const Buffer): UInt16;
Function MapFromFloat16(const Value: Float16): UInt16;{$IFDEF CanInline} inline;{$ENDIF}
Function MapFromHalf(const Value: Half): UInt16;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure MapToFloat32Buffer(Value: UInt32; out Buffer);
Function MapToFloat32(Value: UInt32): Float32;{$IFDEF CanInline} inline;{$ENDIF}
Function MapToSingle(Value: UInt32): Single;{$IFDEF CanInline} inline;{$ENDIF}

Function MapFromFloat32Buffer(const Buffer): UInt32;
Function MapFromFloat32(const Value: Float32): UInt32;{$IFDEF CanInline} inline;{$ENDIF}
Function MapFromSingle(const Value: Single): UInt32;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure MapToFloat64Buffer(Value: UInt64; out Buffer);
Function MapToFloat64(Value: UInt64): Float64;{$IFDEF CanInline} inline;{$ENDIF}
Function MapToDouble(Value: UInt64): Double;{$IFDEF CanInline} inline;{$ENDIF}

Function MapFromFloat64Buffer(const Buffer): UInt64;
Function MapFromFloat64(const Value: Float64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
Function MapFromDouble(const Value: Double): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure MapToFloat80Buffer(High16: UInt16; Low64: UInt64; out Buffer);
Function MapToFloat80(High16: UInt16; Low64: UInt64): Float80;{$IFDEF CanInline} inline;{$ENDIF}
Function MapToExtended(High16: UInt16; Low64: UInt64): Extended;{$IFDEF CanInline} inline;{$ENDIF}

procedure MapFromFloat80Buffer(const Buffer; out High16: UInt16; out Low64: UInt64);
procedure MapFromFloat80(const Value: Float80; out High16: UInt16; out Low64: UInt64);{$IFDEF CanInline} inline;{$ENDIF}
procedure MapFromExtended(const Value: Extended; out High16: UInt16; out Low64: UInt64);{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                          Floats encoding and decoding
--------------------------------------------------------------------------------
===============================================================================}
{
  Following set of functions provides a way of constructing (encoding) floating
  point numbers from their individual parts (sign, exponent, mantissa/fraction)
  and also decoding floats into those parts.

    Mantissa for encoding can have any value, but only its observed part is
    used (bits that are really present in the number), other bits are ignored.
    The same goes for decoding, where only bits that are really part of the
    mantissa in the given number are returned, other bits are zeroed.
    Note that what parts of mantissa are observed or returned can be changed
    by option optIntegerBit (see further).

    Exponent is clamped (limited) to an allowable range for the number. When
    given exponent is outside this bound, then limiting value closest to the
    given value is selected. Expected and returned exponent is normally in its
    unbiased form (true value), but see further (option optExponentBias).
    Note that the entire exponent range can be selected, but its extreme values
    are not valid for normalized numbers, they are used for special values
    (NaN, infinity, ...).

    Sign of True means the sign bit will be/was set in the number (ie. the
    number was or will be negative), False means the sign bit will be/was clear
    (positive number).

  These functions are created in a way that allows encoding and decoding of
  any floating point value, not only normalized numbers (so NaNs, inifinities,
  denormals, even invalid encodings are accepted and processed).

  The processing can be altered by selecting options from TFUTranscodeOption
  enumeration, these are:

    optExponentBias

      When selected for encoding, it informs the function that provided
      exponent is already biased and bias must not be added to it.
      For decoding it forces the function to also return biased exponent.

    optIntegerBit

      This option is ignored by encoding functions, except for encoding into
      double extended (80bit) floats. There, when included, it forces the
      encoding to take the entire provided mantisa as is, including value of
      integer bit (63). When not included, then the function implies integer
      bit in the result from given exponent and ignores its value in the
      provided mantissa (only fraction is used).

      For decoding, it forces the function to include value of integer bit
      in the returned mantissa (one bit above the fraction). When not active,
      then this bit is always zero.
      Note that in most floating point formats, this bit is implied and not
      explicitly stored, only in 80bit floats it is present. These functions
      can and will return even its implied value.

        NOTE - you can use constants FLOATxx_MASK(16)_INTB to mask its position
               in the mantissa (there are also constants FLOATxx_SHIFT(16)_INTB
               to shift it into/from lowest bit).

  Following table sums up observed mantissa widths and allowed exponent ranges
  for different floating point types:

                          |    mantissa width    |      exponent range
      type                | (with optIntegerBit) |  (with optExponentBias)
    -------------------------------------------------------------------------
      Float16 (Half)      |     10 bits (11)     |       -15..16 (0..31)
      Float32 (Single)    |     23 bits (24)     |     -127..128 (0..255)
      Float64 (Double)    |     52 bits (53)     |   -1023..1024 (0..2047)
      Float80 (Extended*) |     63 bits (64)     | -16383..16384 (0..32767)

    * - type Extended can be declared only as an alias to Double (mainly in
        Win64 applications), be aware of that.

  When type Extended is declared as an alias for type Double (64bit float),
  then function EncodeExtended will encode the provided parts into type Float80
  and then converts it into 64bit float which it then returns.
  DecodeExtended first converts given value into Float80 and then decodes this
  full 80bit number.

  In functions accepting untyped buffers, you must ensure that the buffer
  actually points to a floating point number of appropriate type for that
  particular function, or at least memory location that is of correct size.
}
type
  TFUTranscodeOption  = (optExponentBias,optIntegerBit);
  TFUTranscodeOptions = set of TFUTranscodeOption;

procedure EncodeFloat16Buffer(Mantissa: UInt16; Exponent: Integer; Sign: Boolean; out Buffer; Options: TFUTranscodeOptions = []);
Function EncodeFloat16(Mantissa: UInt16; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Float16;{$IFDEF CanInline} inline;{$ENDIF}
Function EncodeHalf(Mantissa: UInt16; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Half;{$IFDEF CanInline} inline;{$ENDIF}

procedure DecodeFloat16Buffer(const Buffer; out Mantissa: UInt16; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
procedure DecodeFloat16(const Value: Float16; out Mantissa: UInt16; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);{$IFDEF CanInline} inline;{$ENDIF}
procedure DecodeHalf(const Value: Half; out Mantissa: UInt16; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure EncodeFloat32Buffer(Mantissa: UInt32; Exponent: Integer; Sign: Boolean; out Buffer; Options: TFUTranscodeOptions = []);
Function EncodeFloat32(Mantissa: UInt32; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Float32;{$IFDEF CanInline} inline;{$ENDIF}
Function EncodeSingle(Mantissa: UInt32; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Single;{$IFDEF CanInline} inline;{$ENDIF}

procedure DecodeFloat32Buffer(const Buffer; out Mantissa: UInt32; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
procedure DecodeFloat32(const Value: Float32; out Mantissa: UInt32; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);{$IFDEF CanInline} inline;{$ENDIF}
procedure DecodeSingle(const Value: Single; out Mantissa: UInt32; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure EncodeFloat64Buffer(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; out Buffer; Options: TFUTranscodeOptions = []);
Function EncodeFloat64(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Float64;{$IFDEF CanInline} inline;{$ENDIF}
Function EncodeDouble(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Double;{$IFDEF CanInline} inline;{$ENDIF}

procedure DecodeFloat64Buffer(const Buffer; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
procedure DecodeFloat64(const Value: Float64; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);{$IFDEF CanInline} inline;{$ENDIF}
procedure DecodeDouble(const Value: Double; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure EncodeFloat80Buffer(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; out Buffer; Options: TFUTranscodeOptions = []);
Function EncodeFloat80(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Float80;{$IFDEF CanInline} inline;{$ENDIF}
Function EncodeExtended(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Extended;{$IFDEF CanInline} inline;{$ENDIF}

procedure DecodeFloat80Buffer(const Buffer; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
procedure DecodeFloat80(const Value: Float80; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);{$IFDEF CanInline} inline;{$ENDIF}
procedure DecodeExtended(const Value: Extended; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                             Number metainformation
--------------------------------------------------------------------------------
===============================================================================}
{
  Following set of functions provide selected metainformations (non-numerical
  informations) about given floating point numbers.

  This is mainly intended for situations where one needs to discern whether the
  number does or does not have some special value (infinity, not-a-number,
  denormal, ...). For more information about these special values, you can
  refer eg. to Intel developer's manual or IEEE-754 standard.

    IsZero        - returns true when the value is zero, false otherwise

    IsDenormal    - indicates whether the number is in denormalized form
                    (extremely small numbers)

    IsNaN         - indicates whether the value is NaN (SNaN, QNaN or QNaN
                    indefinite), which means it is not a number

    IsIndefinite  - indicates whether the given value is an indefinite QNaN,
                    which is value returned by x87 or SSE/AVX as a response
                    to some masked exceptions

    IsInfinite    - indicates whether the given number is infinite (positive or
                    negative infinity)

    IsNormal      - returns true when the value is non-zero normalized floating
                    point number, false otherwise

                      WARNING - IsNormal returns false for value of zero


  There are also some special functions specifically for double extended
  (80bit) floats, namely:

    IsValid           - returns true when the given number has valid encoding,
                        false otherwise (ie. is a pseudo value or unnormal)

    IsPseudoDenormal  - indicates whether the given value is pseudo denormal
                        (denormal with mantissa integer bit set to 1)

    IsPseudoNaN       - indicates whether the number is pseudo NaN (NaN with
                        mantissa integer bit of 0)

    IsPseudoInfinity  - indicates whether the number is pseudo infinity
                        (infinity with mantissa integer bit of 0)

    IsUnnormal        - indicates whether the number is unnormal, which is
                        seemingly normalized number (non-zero exponent) but
                        with mantissa integer bit of 0
}
Function IsZero(const Value: Float16): Boolean; overload;
Function IsDenormal(const Value: Float16): Boolean; overload;
Function IsNaN(const Value: Float16): Boolean; overload;
Function IsIndefinite(const Value: Float16): Boolean; overload;
Function IsInfinite(const Value: Float16): Boolean; overload;
Function IsNormal(const Value: Float16): Boolean; overload;

//------------------------------------------------------------------------------

Function IsZero(const Value: Float32): Boolean; overload;
Function IsDenormal(const Value: Float32): Boolean; overload;
Function IsNaN(const Value: Float32): Boolean; overload;
Function IsIndefinite(const Value: Float32): Boolean; overload;
Function IsInfinite(const Value: Float32): Boolean; overload;
Function IsNormal(const Value: Float32): Boolean; overload;

//------------------------------------------------------------------------------

Function IsZero(const Value: Float64): Boolean; overload;
Function IsDenormal(const Value: Float64): Boolean; overload;
Function IsNaN(const Value: Float64): Boolean; overload;
Function IsIndefinite(const Value: Float64): Boolean; overload;
Function IsInfinite(const Value: Float64): Boolean; overload;
Function IsNormal(const Value: Float64): Boolean; overload;

//------------------------------------------------------------------------------

Function IsZero(const Value: Float80): Boolean; overload;
Function IsDenormal(const Value: Float80): Boolean; overload;
Function IsNaN(const Value: Float80): Boolean; overload;
Function IsIndefinite(const Value: Float80): Boolean; overload;
Function IsInfinite(const Value: Float80): Boolean; overload;
Function IsNormal(const Value: Float80): Boolean; overload;

// invalid encodings...
Function IsValid(const Value: Float80): Boolean;
Function IsPseudoDenormal(const Value: Float80): Boolean;
Function IsPseudoNaN(const Value: Float80): Boolean;
Function IsPseudoInfinity(const Value: Float80): Boolean;
Function IsUnnormal(const Value: Float80): Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                                Sign manipulation
--------------------------------------------------------------------------------
===============================================================================}
{
  Set of functions allowing obtaining of sign from floating point numbers and
  also its basic manipulations. This all can be done using normal arithmetic
  operators, but that inadvertently involves current floating point hardware
  (x87 or SEE/AVX).
  Implementation of these functions is done in a way that fpu is NOT used.

    Sign  - indicates sign of the number, -1 for negative values (or generally
            values with sign bit set, as these can be infinities, NaNs and
            others), 0 for zeroes - it returns 0 for both positive and negative
            zero (floats have negative zero) - and +1 for positive values
            (values with sign bit clear)

    Abs   - returns absolute value of given number by masking-out the sign bit

    Neg   - flips sign of the given value


  Functions accepting double extended (80bit) floats are working correctly even
  with invalid encodings (pseudo values and unnormals).
}
type
  TFUValueSign = -1..1;

Function Sign(const Value: Float16): TFUValueSign; overload;
Function Abs(const Value: Float16): Float16; overload;
Function Neg(const Value: Float16): Float16; overload;

//------------------------------------------------------------------------------

Function Sign(const Value: Float32): TFUValueSign; overload;
Function Abs(const Value: Float32): Float32; overload;
Function Neg(const Value: Float32): Float32; overload;

//------------------------------------------------------------------------------

Function Sign(const Value: Float64): TFUValueSign; overload;
Function Abs(const Value: Float64): Float64; overload;
Function Neg(const Value: Float64): Float64; overload;

//------------------------------------------------------------------------------

Function Sign(const Value: Float80): TFUValueSign; overload;
Function Abs(const Value: Float80): Float80; overload;
Function Neg(const Value: Float80): Float80; overload;

{===============================================================================
--------------------------------------------------------------------------------
                         Unit implementation management
--------------------------------------------------------------------------------
===============================================================================}
{
  WARNING - be wery careful when changing the selected implementation, as there
            is absolutely no thread-safety protection.

  For full description of this section, please refer to the same section in
  BitOps library (github.com/TheLazyTomcat/Lib.BitOps), file BitOps.pas.

--------------------------------------------------------------------------------
                                 >>> WARNING <<<
--------------------------------------------------------------------------------

  Although it is possible to separately select implementation for each function
  that is subject to unit implementation management (there are enum values for
  that and UIM is prepared for it), it is NOT recommended (it is allowed mainly
  for debugging purposes, not for normal use).

  There are several groups of functions, where routines in each group are
  bound together, usually by working on some global state. If you would select
  different implementation for functions in the same group, you would break
  this bond and further behavior would be undefined.

  To properly select imlementation, it must be done for the entire group. To
  allow for this, there are enum values that operates on these groups instead
  of individual functions. Those are:

      fnX87FPUAccess        - functions accessing x87 FPU state, these include:

          fnX87StatusWordGet
          fnX87ControlWordGet
          fnX87ControlWordSet
          fnX87EnvironmentInit
          fnX87FloatDataGet
          fnX87ExceptionsClear
          fnX87ExceptionsRaise
          fnX87SaveEnvironment
          fnX87LoadEnvironment

      fnFloat80Conversions  - conversion from/to double extended (80bit) floats:

          fnFloat64ToFloat80
          fnFloat80ToFloat64

      fnVECAccess           - access to SSE/AVX vector units state:

          fnVECControlAndStatusGet
          fnVECControlAndStatusSet
          fnVECFloatDataGet

      fnFloat16Conversions  - conversion from/to half precision (16bit) floats:

          fnFloat16ToFloat32
          fnFloat32ToFloat16
          fnFloat16ToFloat32Vec4
          fnFloat32ToFloat16Vec4

      fnFloatXConversions   - conversions between float32 and float64>

          fnFloat32ToFloat64
          fnFloat64ToFloat32

  So, when changing implementation, just use one of the five groups and UIM
  will select it for all functions in that group in one step.
  Also note that groups fnFloat80Conversions, fnFloat16Conversions and
  fnFloatXConversions are changing specific global variables, meaning changing
  these groups instead of their individual functions is even more important.

  If you use the group enum in UIM_FloatUtils_GetFuncImpl, it will return
  selected implemetation of the first function within that group - be aware
  of that.

  UIM_FloatUtils_AvailableFuncImpl and UIM_FloatUtils_SupportedFuncImpl are
  returning information that is valid for all functions within the selected
  group.
}
type
  TUIM_FloatUtils_Function = (
    fnX87StatusWordGet,fnX87ControlWordGet,fnX87ControlWordSet,
    fnX87EnvironmentInit,fnX87FloatDataGet,
    fnX87ExceptionsClear,fnX87ExceptionsRaise,
    fnX87SaveEnvironment,fnX87LoadEnvironment,
    fnFloat64ToFloat80,fnFloat80ToFloat64,
    fnVECControlAndStatusGet,fnVECControlAndStatusSet,fnVECFloatDataGet,
    fnFloat16ToFloat32,fnFloat32ToFloat16,
    fnFloat16ToFloat32Vec4,fnFloat32ToFloat16Vec4,
    fnFloat32ToFloat64,fnFloat64ToFloat32,
    fnX87FPUAccess,fnFloat80Conversions,fnVECAccess,fnFloat16Conversions,
    fnFloatXConversions);

  TUIM_FloatUtils_Implementation = (imNone,imPascal,imAssembly,imPascalX87,
                                    imPascalSSE,imAssemblyX87,imAssemblySSE);

  TUIM_FloatUtils_Implementations = set of TUIM_FloatUtils_Implementation;

//------------------------------------------------------------------------------
{
  Returns which implementations are available for the selected function.
}
Function UIM_FloatUtils_AvailableFuncImpl(Func: TUIM_FloatUtils_Function): TUIM_FloatUtils_Implementations;

{
  Returns which implementations are supported and can be safely selected for
  a given function.
}
Function UIM_FloatUtils_SupportedFuncImpl(Func: TUIM_FloatUtils_Function): TUIM_FloatUtils_Implementations;

{
  Returns value indicating what implementation of the selected function is
  executed when calling the function.

  If Func is set to a function group and StrictGroupCheck is set to True, then
  the entire group is checked whether all functions in that group are currently
  set to the same implementation - if this check fails, then EUIMInvalidState
  exception is raised. When StrictGroupCheck is false then no such check is
  performed.
  If Func is set to a specific function instead of group, then StrictGroupCheck
  parameter is ignored.
}
Function UIM_FloatUtils_GetFuncImpl(Func: TUIM_FloatUtils_Function; StrictGroupCheck: Boolean = False): TUIM_FloatUtils_Implementation;

{
  Routes selected function to a selected implementation.

  If you select implementation that the given function does not offer (ie. is
  not indicated by UIM_FloatUtils_AvailableFuncImpl), then an exception of type
  EUIMInvalidIdentifier is raised.

  If Func is set to a function group and StrictGroupCheck is set to True, then
  the entire group is checked whether all functions in that group are currently
  set to the same implementation - if this check fails, then EUIMInvalidState
  exception is raised and implementation is not changed. When StrictGroupCheck
  is false then no such check is performed.
  If Func is set to a specific function instead of group, then StrictGroupCheck
  parameter is ignored.

  Returned value is the previous routing.

  WARNING - when selecting imNone as an implementation for any function, the
            routing is set to nil, and because the routing mechanism, for the
            sake of performance, does not check validity, it will result in an
            exception when calling this function.

  WARNING - when selecting unsupported implementation, calling the function
            will almost certainly result in an system/external exception (eg.
            invalid instruction).
}
Function UIM_FloatUtils_SetFuncImpl(Func: TUIM_FloatUtils_Function; NewImpl: TUIM_FloatUtils_Implementation; StrictGroupCheck: Boolean = False): TUIM_FloatUtils_Implementation;

implementation

uses
  Math,
  BasicUIM, UInt64Utils{$IFNDEF PurePascal}, SimpleCPUID{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                           Library-specific exceptions
--------------------------------------------------------------------------------
===============================================================================}

class Function EFUEmulationException.GetClassErrorMessage: String;
const
  CLASS_MSGS: array[TFUException] of record
    Classes:    array[0..2] of TClass;
    ErrMessage: String;
  end = (
    (Classes: (EF80CInvalidOp,      EF16CInvalidOp, EFXCInvalidOp);       ErrMessage: 'Invalid floating point operation.'),
    (Classes: (EF80CDenormal,       EF16CDenormal,  EFXCDenormal);        ErrMessage: 'Denormal floating point operand.'),
    (Classes: (EF80CDivByZero,      EF16CDivByZero, EFXCDivByZero);       ErrMessage: 'Floating point division by zero.'),
    (Classes: (EF80COverflow,       EF16COverflow,  EFXCOverflow);        ErrMessage: 'Floating point numeric overflow.'),
    (Classes: (EF80CUnderflow,      EF16CUnderflow, EFXCUnderflow);       ErrMessage: 'Floating point numeric underflow.'),
    (Classes: (EF80CPrecision,      EF16CPrecision, EFXCPrecision);       ErrMessage: 'Inexact floating point result.'),
    (Classes: (EF80CStackUnderflow, nil,            EFXCStackUnderflow);  ErrMessage: 'Floating point unit stack underflow.'),
    (Classes: (EF80CStackOverflow,  nil,            EFXCStackOverflow);   ErrMessage: 'Floating point unit stack overflow.'));
var
  i:  TFUException;
  j:  Integer;
begin
For i := Low(CLASS_MSGS) to High(CLASS_MSGS) do
  For j := Low(CLASS_MSGS[i].Classes) to High(CLASS_MSGS[i].Classes) do
    If Self = CLASS_MSGS[i].Classes[j] then
      begin
        Result := CLASS_MSGS[i].ErrMessage;
        Exit;
      end;
// if here, then self is not of any class defined in CLASS_MSGS
If Self = EF80CException then
  Result := 'Error in conversion of double extended float.'
else If Self = EF16CException then
  Result := 'Error in conversion of half float.'
else If Self = EFXCException then
  Result := 'Error in single-double conversion.'
else
  Result := 'Unknown floating point exception.';
end;

//------------------------------------------------------------------------------

constructor EFUEmulationException.Create;
begin
inherited Create(GetClassErrorMessage);
Initialize;
end;

{===============================================================================
--------------------------------------------------------------------------------
                   x87 FPU control and status (CS) management
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    x87 FPU CS management - low-lewel access implementation
===============================================================================}
{$IFNDEF PurePascal}

Function X87StatusWordGet_ASM: UInt16; register; assembler;
asm
    FNSTSW  AX
end;

//------------------------------------------------------------------------------

Function X87ControlWordGet_ASM: UInt16; register; assembler;
asm
{$IFDEF x64}
    SUB     RSP, 8
    FNSTCW  word ptr [RSP]
    MOV     AX, word ptr [RSP]
    ADD     RSP, 8
{$ELSE}
    SUB     ESP, 4  // create temporary storage on stack
    FNSTCW  word ptr [ESP]
    MOV     AX, word ptr [ESP]
    ADD     ESP, 4  // clear the stack
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure X87ControlWordSet_ASM(NewValue: UInt16); register; assembler;
asm
{$IFDEF x64}
    SUB     RSP, 8
  {$IFDEF Windows}
    MOV     word ptr [RSP], CX
  {$ELSE}
    MOV     word ptr [RSP], DI
  {$ENDIF}
    FLDCW   word ptr [RSP]
    ADD     RSP, 8
{$ELSE}
    SUB     ESP, 4
    MOV     word ptr [ESP], AX
    FLDCW   word ptr [ESP]
    ADD     ESP, 4
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure X87EnvironmentInit_ASM; register; assembler;
asm
    FNINIT
end;

//------------------------------------------------------------------------------

procedure X87FloatDataGet_ASM(Storage: Pointer); register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    FNSAVE  [RCX]
    FRSTOR  [RCX]
  {$ELSE}
    FNSAVE  [RDI]
    FRSTOR  [RDI]
  {$ENDIF}
{$ELSE}
    FNSAVE  [EAX] // this will initialize FPU...
    FRSTOR  [EAX] // ...so we restore it again
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure X87SaveEnvironment_ASM(Storage: Pointer); register; assembler;
asm
{
  Instruction FNSTENV masks all exceptions after storing the environment, so,
  to preserve the state as it was before, we immediately load back the stored
  control word (conveniently it is at offset 0, so we can use the same pointer)
  with the original exception mask bitset.
}
{$IFDEF x64}
  {$IFDEF Windows}
    FNSTENV [RCX]
    FLDCW   word ptr [RCX]
  {$ELSE}
    FNSTENV [RDI]
    FLDCW   word ptr [RDI]
  {$ENDIF}
{$ELSE}
    FNSTENV [EAX]
    FLDCW   word ptr [EAX]
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure X87LoadEnvironment_ASM(Storage: Pointer); register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    FLDENV  [RCX]
  {$ELSE}
    FLDENV  [RDI]
  {$ENDIF}
{$ELSE}
    FLDENV  [EAX]
{$ENDIF}
end;

{$ENDIF}
//==============================================================================

Function X87StatusWordGet_PAS: UInt16; register;
begin
{$IFDEF FPC}
Result := 0;
{$ENDIF}
raise EFUUnsupportedOp.Create('X87StatusWordGet_PAS: x87 FPU operation not supported.');
end;

//------------------------------------------------------------------------------

Function X87ControlWordGet_PAS: UInt16; register;
begin
{$IFDEF FPC}
Result := 0;
{$ENDIF}
raise EFUUnsupportedOp.Create('X87ControlWordGet_PAS: x87 FPU operation not supported.');
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure X87ControlWordSet_PAS(NewValue: UInt16); register;
begin
raise EFUUnsupportedOp.Create('X87ControlWordSet_PAS: x87 FPU operation not supported.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure X87EnvironmentInit_PAS; register;
begin
raise EFUUnsupportedOp.Create('X87EnvironmentInit_PAS: x87 FPU operation not supported.');
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure X87FloatDataGet_PAS(Storage: Pointer); register;
begin
raise EFUUnsupportedOp.Create('X87FloatDataGet_PAS: x87 FPU operation not supported.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure X87SaveEnvironment_PAS(Storage: Pointer); register;
begin
raise EFUUnsupportedOp.Create('X87SaveEnvironment_PAS: x87 FPU operation not supported.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure X87LoadEnvironment_PAS(Storage: Pointer); register;
begin
raise EFUUnsupportedOp.Create('X87LoadEnvironment_PAS: x87 FPU operation not supported.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================
var
  VAR_X87StatusWordGet:   Function: UInt16; register = X87StatusWordGet_PAS;
  VAR_X87ControlWordGet:  Function: UInt16; register = X87ControlWordGet_PAS;
  VAR_X87ControlWordSet:  procedure(NewValue: UInt16); register = X87ControlWordSet_PAS;
  VAR_X87EnvironmentInit: procedure; register = X87EnvironmentInit_PAS;
  VAR_X87FloatDataGet:    procedure(Storage: Pointer); register = X87FloatDataGet_PAS;
  VAR_X87SaveEnvironment: procedure(Storage: Pointer); register = X87SaveEnvironment_PAS;
  VAR_X87LoadEnvironment: procedure(Storage: Pointer); register = X87LoadEnvironment_PAS;

//==============================================================================

Function X87StatusWordGet: UInt16;
begin
Result := VAR_X87StatusWordGet;
end;

//------------------------------------------------------------------------------

Function X87ControlWordGet: UInt16;
begin
Result := VAR_X87ControlWordGet;
end;

//------------------------------------------------------------------------------

procedure X87ControlWordSet(NewValue: UInt16);
begin
VAR_X87ControlWordSet(NewValue);
end;

//------------------------------------------------------------------------------

procedure X87EnvironmentInit;
begin
VAR_X87EnvironmentInit;
end;

//------------------------------------------------------------------------------

procedure X87ControlWordInit;
begin
X87ControlWordSet(X87CW_DefaultValue);
end;

//------------------------------------------------------------------------------

Function X87SWTopOfStackGet(StatusWord: UInt16): Integer; forward;

procedure X87FloatDataGet(out FloatData: TX87FloatData; StackOrder: Boolean = False);

  Function NumberToTag(Num: Integer): TX87RegisterTag;
  begin
    case Num of
      0:  Result := rtValid;
      1:  Result := rtZero;
      2:  Result := rtSpecial;
    else
      Result := rtEmpty;
    end;
  end;

var
  // 108 bytes of storage (32bit protected mode format), p1..p4 are paddings
  FSAVEStorage: packed record
    CW,p1:  UInt16;               // control word, padding
    SW,p2:  UInt16;               // status word, padding
    TW,p3:  UInt16;               // tag word, padding
    FIP:    UInt32;               // instruction pointer offset
    FCS:    UInt16;               // instruction pointer selector
    FOP:    UInt16;               // last instruction opcode
    FDP:    UInt32;               // data pointer offset
    FDS,p4: UInt16;               // data pointer selector, padding
    Stack:  packed array[0..7] of // fp registers, stack-ordered (ST(0), ST(1), ...)
      packed array[0..9] of Byte;
  end;
  TopOfStack: Integer;
  i,Index:          Integer;
begin
VAR_X87FloatDataGet(@FSAVEStorage);
TopOfStack := X87SWTopOfStackGet(FSAVEStorage.SW);
If StackOrder then
  begin
    For i := Low(FloatData.Regs) to High(FloatData.Regs) do
      begin
        Index := (i + TopOfStack) and 7;
        FloatData.Regs[i].Tag := NumberToTag((FSAVEStorage.TW shr (Index * 2)) and 3);
        FloatData.Regs[i].Data.Overlay := TFloat80Overlay(FSAVEStorage.Stack[i]);
      end;
  end
else
  begin
    For i := Low(FloatData.Regs) to High(FloatData.Regs) do
      begin
        Index := (i + TopOfStack) and 7;
        FloatData.Regs[i].Tag := NumberToTag((FSAVEStorage.TW shr (i * 2)) and 3);
        FloatData.Regs[Index].Data.Overlay := TFloat80Overlay(FSAVEStorage.Stack[i]);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure X87SaveEnvironment(Storage: Pointer);
begin
VAR_X87SaveEnvironment(Storage);
end;

//------------------------------------------------------------------------------

procedure X87LoadEnvironment(Storage: Pointer);
begin
VAR_X87LoadEnvironment(Storage);
end;

{===============================================================================
    x87 FPU CS management - abstracted access internals
===============================================================================}

Function X87SWTopOfStackGet(StatusWord: UInt16): Integer;
begin
Result := (StatusWord and X87SW_TopOfStack) shr X87SW_SHIFT_TopOfStack;
end;

//------------------------------------------------------------------------------

Function X87SWStatusFlagGet(StatusWord: UInt16; Flag: TX87StatusFlag): Boolean;
begin
case Flag of
  sfStackFault:       Result := (StatusWord and X87SW_StackFault) <> 0;
  sfExceptionSummary: Result := (StatusWord and X87SW_ExceptionSummary) <> 0;
  sfFPUBusy:          Result := (StatusWord and X87SW_FPUBusy) <> 0;
  sfConditionCodeC0:  Result := (StatusWord and X87SW_ConditionCode_C0) <> 0;
  sfConditionCodeC1:  Result := (StatusWord and X87SW_ConditionCode_C1) <> 0;
  sfConditionCodeC2:  Result := (StatusWord and X87SW_ConditionCode_C2) <> 0;
  sfConditionCodeC3:  Result := (StatusWord and X87SW_ConditionCode_C3) <> 0;
else
  raise EFUInvalidFlag.CreateFmt('X87SWStatusFlagGet: Invalid status flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function X87SWStatusFlagsGet(StatusWord: UInt16): TX87StatusFlags;
begin
Result := [];
If (StatusWord and X87SW_StackFault) <> 0 then
  Include(Result,sfStackFault);
If (StatusWord and X87SW_ExceptionSummary) <> 0 then
  Include(Result,sfExceptionSummary);
If (StatusWord and X87SW_FPUBusy) <> 0 then
  Include(Result,sfFPUBusy);
If (StatusWord and X87SW_ConditionCode_C0) <> 0 then
  Include(Result,sfConditionCodeC0);
If (StatusWord and X87SW_ConditionCode_C1) <> 0 then
  Include(Result,sfConditionCodeC1);
If (StatusWord and X87SW_ConditionCode_C2) <> 0 then
  Include(Result,sfConditionCodeC2);
If (StatusWord and X87SW_ConditionCode_C3) <> 0 then
  Include(Result,sfConditionCodeC3);
end;

//==============================================================================

Function X87CWPrecisionModeGet(ControlWord: UInt16): TX87PrecisionMode;
begin
case (ControlWord and X87CW_Precision) shr X87CW_SHIFT_Precision of
  0:  Result := pmSingle;
  2:  Result := pmDouble;
  3:  Result := pmExtended;
else
  Result := pmReserved;
end;
end;

//------------------------------------------------------------------------------

Function X87CWPrecisionModeSet(var ControlWord: UInt16; NewValue: TX87PrecisionMode): TX87PrecisionMode;
begin
Result := X87CWPrecisionModeGet(ControlWord);
ControlWord := ControlWord and not X87CW_Precision;
case NewValue of
  pmSingle:   ; // do nothing, precision is already zeroed
  pmDouble:   ControlWord := ControlWord or UInt16(2 shl X87CW_SHIFT_Precision);
  pmExtended: ControlWord := ControlWord or UInt16(3 shl X87CW_SHIFT_Precision);
else
  ControlWord := ControlWord or UInt16(1 shl X87CW_SHIFT_Precision);
end;
end;

//------------------------------------------------------------------------------

Function X87CWRoundingModeGet(ControlWord: UInt16): TX87RoundingMode;
begin
case (ControlWord and X87CW_Rounding) shr X87CW_SHIFT_Rounding of
  1:  Result := rmDown;
  2:  Result := rmUp;
  3:  Result := rmTruncate;
else
  Result := rmNearest;
end;
end;

//------------------------------------------------------------------------------

Function X87CWRoundingModeSet(var ControlWord: UInt16; NewValue: TX87RoundingMode): TX87RoundingMode;
begin
Result := X87CWRoundingModeGet(ControlWord);
ControlWord := ControlWord and not X87CW_Rounding;
case NewValue of
  rmDown:     ControlWord := ControlWord or UInt16(1 shl X87CW_SHIFT_Rounding);
  rmUp:       ControlWord := ControlWord or UInt16(2 shl X87CW_SHIFT_Rounding);
  rmTruncate: ControlWord := ControlWord or UInt16(3 shl X87CW_SHIFT_Rounding);
end;
{
  No need to deal with other cases, rounding field is zeroed which defaults to
  rmNearest.
}
end;

//------------------------------------------------------------------------------

Function X87CWControlFlagGet(ControlWord: UInt16; Flag: TX87ControlFlag): Boolean;
begin
case Flag of
  cfInfinityControl:  Result := (ControlWord and X87CW_InfinityControl) <> 0;
else
  raise EFUInvalidFlag.CreateFmt('X87CWControlFlagGet: Invalid control flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function X87CWControlFlagSet(var ControlWord: UInt16; Flag: TX87ControlFlag; NewValue: Boolean): Boolean;

  procedure SetBit(BitMask: UInt16);
  begin
    If NewValue then
      ControlWord := ControlWord or BitMask
    else
      ControlWord := ControlWord and not BitMask;
  end;

begin
Result := X87CWControlFlagGet(ControlWord,Flag);
case Flag of
  cfInfinityControl:  SetBit(X87CW_InfinityControl);
else
  raise EFUInvalidFlag.CreateFmt('X87CWControlFlagSet: Invalid control flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function X87CWControlFlagsGet(ControlWord: UInt16): TX87ControlFlags;
begin
Result := [];
If (ControlWord and X87CW_InfinityControl) <> 0 then
  Include(Result,cfInfinityControl);
end;

//------------------------------------------------------------------------------

Function X87CWControlFlagsSet(var ControlWord: UInt16; NewValue: TX87ControlFlags): TX87ControlFlags;

  procedure SetBit(BitMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      ControlWord := ControlWord or BitMask
    else
      ControlWord := ControlWord and not BitMask;
  end;

begin
Result := X87CWControlFlagsGet(ControlWord);
SetBit(X87CW_InfinityControl,cfInfinityControl in NewValue);
end;

//==============================================================================

Function X87CWExceptionMaskGet(ControlWord: UInt16; Exception: TX87Exception): Boolean;
begin
case Exception of
  excInvalidOp: Result := (ControlWord and X87CW_EMASK_InvalidOP) <> 0;
  excDenormal:  Result := (ControlWord and X87CW_EMASK_Denormal) <> 0;
  excDivByZero: Result := (ControlWord and X87CW_EMASK_DivByZero) <> 0;
  excOverflow:  Result := (ControlWord and X87CW_EMASK_Overflow) <> 0;
  excUnderflow: Result := (ControlWord and X87CW_EMASK_Underflow) <> 0;
  excPrecision: Result := (ControlWord and X87CW_EMASK_Precision) <> 0;
else
  raise EFUInvalidFlag.CreateFmt('X87CWExceptionMaskGet: Invalid x87 exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function X87CWExceptionMaskSet(var ControlWord: UInt16; Exception: TX87Exception; NewValue: Boolean): Boolean;

  procedure SetBit(BitMask: UInt16);
  begin
    If NewValue then
      ControlWord := ControlWord or BitMask
    else
      ControlWord := ControlWord and not BitMask;
  end;

begin
Result := X87CWExceptionMaskGet(ControlWord,Exception);
case Exception of
  excInvalidOp: SetBit(X87CW_EMASK_InvalidOP);
  excDenormal:  SetBit(X87CW_EMASK_Denormal);
  excDivByZero: SetBit(X87CW_EMASK_DivByZero);
  excOverflow:  SetBit(X87CW_EMASK_Overflow);
  excUnderflow: SetBit(X87CW_EMASK_Underflow);
  excPrecision: SetBit(X87CW_EMASK_Precision);
else
  raise EFUInvalidFlag.CreateFmt('X87CWExceptionMaskSet: Invalid x87 exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function X87CWExceptionMasksGet(ControlWord: UInt16): TX87Exceptions;
begin
Result := [];
If (ControlWord and X87CW_EMASK_InvalidOP) <> 0 then
  Include(Result,excInvalidOp);
If (ControlWord and X87CW_EMASK_Denormal) <> 0 then
  Include(Result,excDenormal);
If (ControlWord and X87CW_EMASK_DivByZero) <> 0 then
  Include(Result,excDivByZero);
If (ControlWord and X87CW_EMASK_Overflow) <> 0 then
  Include(Result,excOverflow);
If (ControlWord and X87CW_EMASK_Underflow) <> 0 then
  Include(Result,excUnderflow);
If (ControlWord and X87CW_EMASK_Precision) <> 0 then
  Include(Result,excPrecision);
end;

//------------------------------------------------------------------------------

Function X87CWExceptionMasksSet(var ControlWord: UInt16; NewValue: TX87Exceptions): TX87Exceptions;

  procedure SetBit(BitMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      ControlWord := ControlWord or BitMask
    else
      ControlWord := ControlWord and not BitMask;
  end;

begin
Result := X87CWExceptionMasksGet(ControlWord);
SetBit(X87CW_EMASK_InvalidOP,excInvalidOp in NewValue);
SetBit(X87CW_EMASK_Denormal,excDenormal in NewValue);
SetBit(X87CW_EMASK_DivByZero,excDivByZero in NewValue);
SetBit(X87CW_EMASK_Overflow,excOverflow in NewValue);
SetBit(X87CW_EMASK_Underflow,excUnderflow in NewValue);
SetBit(X87CW_EMASK_Precision,excPrecision in NewValue);
end;

//------------------------------------------------------------------------------

Function X87SWExceptionFlagGet(StatusWord: UInt16; Exception: TX87Exception): Boolean;
begin
case Exception of
  excInvalidOp: Result := (StatusWord and X87SW_EFLAG_InvalidOP) <> 0;
  excDenormal:  Result := (StatusWord and X87SW_EFLAG_Denormal) <> 0;
  excDivByZero: Result := (StatusWord and X87SW_EFLAG_DivByZero) <> 0;
  excOverflow:  Result := (StatusWord and X87SW_EFLAG_Overflow) <> 0;
  excUnderflow: Result := (StatusWord and X87SW_EFLAG_Underflow) <> 0;
  excPrecision: Result := (StatusWord and X87SW_EFLAG_Precision) <> 0;
else
  raise EFUInvalidFlag.CreateFmt('X87SWExceptionFlagGet: Invalid x87 exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function X87SWExceptionFlagsGet(StatusWord: UInt16): TX87Exceptions;
begin
Result := [];
If (StatusWord and X87SW_EFLAG_InvalidOP) <> 0 then
  Include(Result,excInvalidOp);
If (StatusWord and X87SW_EFLAG_Denormal) <> 0 then
  Include(Result,excDenormal);
If (StatusWord and X87SW_EFLAG_DivByZero) <> 0 then
  Include(Result,excDivByZero);
If (StatusWord and X87SW_EFLAG_Overflow) <> 0 then
  Include(Result,excOverflow);
If (StatusWord and X87SW_EFLAG_Underflow) <> 0 then
  Include(Result,excUnderflow);
If (StatusWord and X87SW_EFLAG_Precision) <> 0 then
  Include(Result,excPrecision);
end;

//==============================================================================
{$IFNDEF PurePascal}

procedure X87ExceptionsClear_ASM; register; assembler;
asm
    FNCLEX
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

procedure X87ExceptionsRaise_ASM; register; assembler;
asm
    FWAIT
end;

//------------------------------------------------------------------------------
{$ENDIF}

procedure X87ExceptionsClear_PAS; register;
begin
raise EFUUnsupportedOp.Create('X87ExceptionsClear_PAS: x87 FPU operation not supported.');
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

procedure X87ExceptionsRaise_PAS; register;
begin
raise EFUUnsupportedOp.Create('X87ExceptionsRaise_PAS: x87 FPU operation not supported.');
end;

//------------------------------------------------------------------------------
var
  VAR_X87ExceptionsClear: procedure; register = X87ExceptionsClear_PAS;
  VAR_X87ExceptionsRaise: procedure; register = X87ExceptionsRaise_PAS;

{===============================================================================
    x87 FPU CS management - abstracted access implementation
===============================================================================}
{-------------------------------------------------------------------------------
    x87 FPU CS management - status word abstracted access
-------------------------------------------------------------------------------}

Function X87TopOfStackGet: Integer;
begin
Result := X87SWTopOfStackGet(X87StatusWordGet);
end;

//------------------------------------------------------------------------------

Function X87StatusFlagGet(Flag: TX87StatusFlag): Boolean;
begin
Result := X87SWStatusFlagGet(X87StatusWordGet,Flag);
end;

//------------------------------------------------------------------------------

Function X87StatusFlagsGet: TX87StatusFlags;
begin
Result := X87SWStatusFlagsGet(X87StatusWordGet);
end;

{-------------------------------------------------------------------------------
    x87 FPU CS management - control word abstracted access
-------------------------------------------------------------------------------}

Function X87PrecisionModeGet: TX87PrecisionMode;
begin
Result := X87CWPrecisionModeGet(X87ControlWordGet);
end;

//------------------------------------------------------------------------------

Function X87PrecisionModeSet(NewValue: TX87PrecisionMode): TX87PrecisionMode;
var
  ControlWord:  UInt16;
begin
ControlWord := X87ControlWordGet;
Result := X87CWPrecisionModeSet(ControlWord,NewValue);
X87ControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function X87RoundingModeGet: TX87RoundingMode;
begin
Result := X87CWRoundingModeGet(X87ControlWordGet);
end;

//------------------------------------------------------------------------------

Function X87RoundingModeSet(NewValue: TX87RoundingMode): TX87RoundingMode;
var
  ControlWord:  UInt16;
begin
ControlWord := X87ControlWordGet;
Result := X87CWRoundingModeSet(ControlWord,NewValue);
X87ControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function X87ControlFlagGet(Flag: TX87ControlFlag): Boolean;
begin
Result := X87CWControlFlagGet(X87ControlWordGet,Flag);
end;

//------------------------------------------------------------------------------

Function X87ControlFlagSet(Flag: TX87ControlFlag; NewValue: Boolean): Boolean;
var
  ControlWord:  UInt16;
begin
ControlWord := X87ControlWordGet;
Result := X87CWControlFlagSet(ControlWord,Flag,NewValue);
X87ControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function X87ControlFlagsGet: TX87ControlFlags;
begin
Result := X87CWControlFlagsGet(X87ControlWordGet);
end;

//------------------------------------------------------------------------------

Function X87ControlFlagsSet(NewValue: TX87ControlFlags): TX87ControlFlags;
var
  ControlWord:  UInt16;
begin
ControlWord := X87ControlWordGet;
Result := X87CWControlFlagsSet(ControlWord,NewValue);
X87ControlWordSet(ControlWord);
end;

{-------------------------------------------------------------------------------
    x87 FPU CS management - exceptions abstraction
-------------------------------------------------------------------------------}

Function X87ExceptionMaskGet(Exception: TX87Exception): Boolean;
begin
Result := X87CWExceptionMaskGet(X87ControlWordGet,Exception);
end;

//------------------------------------------------------------------------------

Function X87ExceptionMaskSet(Exception: TX87Exception; NewValue: Boolean): Boolean;
var
  ControlWord:  UInt16;
begin
ControlWord := X87ControlWordGet;
Result := X87CWExceptionMaskSet(ControlWord,Exception,NewValue);
X87ControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function X87ExceptionMasksGet: TX87Exceptions;
begin
Result := X87CWExceptionMasksGet(X87ControlWordGet);
end;

//------------------------------------------------------------------------------

Function X87ExceptionMasksSet(NewValue: TX87Exceptions): TX87Exceptions;
var
  ControlWord:  UInt16;
begin
ControlWord := X87ControlWordGet;
Result := X87CWExceptionMasksSet(ControlWord,NewValue);
X87ControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function X87ExceptionFlagGet(Exception: TX87Exception): Boolean;
begin
Result := X87SWExceptionFlagGet(X87StatusWordGet,Exception);
end;

//------------------------------------------------------------------------------

Function X87ExceptionFlagsGet: TX87Exceptions;
begin
Result := X87SWExceptionFlagsGet(X87StatusWordGet);
end;

//------------------------------------------------------------------------------

procedure X87ExceptionsClear;
begin
VAR_X87ExceptionsClear;
end;

//------------------------------------------------------------------------------

procedure X87ExceptionsRaise;
begin
VAR_X87ExceptionsRaise;
end;


{===============================================================================
--------------------------------------------------------------------------------
                     F80 conversion (F80C) state management
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    F80C state management - emulated state
===============================================================================}
var
  VAR_F80CEmulated: Boolean = True;

type
  TF80CState = record
    Initialized:  Boolean;
    StatusWord:   UInt16;
    ControlWord:  UInt16;
    ExceptState:  record  // see F80CExceptStatePrepare for explanation
      StatusWord:   UInt16;
      ControlWord:  UInt16;
    end;
  end;
  PF80CState = ^TF80CState;

threadvar
  // automatically initialized to all-zero (field Initialized is false)
  THRVAR_F80CState: TF80CState;

//------------------------------------------------------------------------------
{
  Pointers are used to access the state thread variable - this is to limit
  calls to GetTLS (or equivalent function in non-windows systems).
}
Function F80CGetStatePtr: PF80CState;
begin
Result := @THRVAR_F80CState;
If not Result^.Initialized then
  begin
    Result^.Initialized := True;
    Result^.StatusWord := 0;
    Result^.ControlWord := X87CW_DefaultValue;
    // ExceptState can be ignored right now, it is prepared when needed
  end;
end;

//------------------------------------------------------------------------------
{
  Sometimes F80C exceptions are not raised using current values of status and
  control words, but values they had before some change was made on them.

  For example when setting new value to control word when there are pending
  unmasked exceptions. These exeptions are raised only after the new value is
  stored into the control word, but the exception needs the old values.

  For this reason, there is an except state field in the F80C state that is
  used to store current values of SW and CW, so they can be used later for
  exception raising. F80CExceptStatePrepare function, when called, will make
  this copy.

  It also returns pointer to the current state, so it can be used in place of
  F80CGetStatePtr where needed and appropriate.
}
Function F80CExceptStatePrepare: PF80CState;
begin
Result := F80CGetStatePtr;
Result^.ExceptState.StatusWord := Result^.StatusWord;
Result^.ExceptState.ControlWord := Result^.ControlWord;
end;

{===============================================================================
    F80C state management - exceptions implementation
===============================================================================}
{-------------------------------------------------------------------------------
    F80C state management - EF80CException class implementation
-------------------------------------------------------------------------------}

procedure EF80CException.Initialize;
var
  LocalState: TF80CState;
begin
LocalState := F80CGetStatePtr^;
fStatusWord := LocalState.ExceptState.StatusWord;
fControlWord := LocalState.ExceptState.ControlWord;
fPendingExcs := X87SWExceptionFlagsGet(fStatusWord);
fMaskedExcs := fPendingExcs * X87CWExceptionMasksGet(fControlWord);
fUnmaskedExcs := fPendingExcs - X87CWExceptionMasksGet(fControlWord);
// raise only unmasked exceptions
fRaisedExcs := fUnmaskedExcs;
// resolve stack faults
If (excInvalidOp in fRaisedExcs) and X87SWStatusFlagGet(fStatusWord,sfStackFault) then
  begin
    Exclude(fRaisedExcs,excInvalidOp);
    If X87SWStatusFlagGet(fStatusWord,sfConditionCodeC1) then
      Include(fRaisedExcs,excStackOverflow)
    else
      Include(fRaisedExcs,excStackUnderflow);
  end;
{
  For compatibility with EMathError - clear all pending exceptions in the
  global state and set control word to default value.
}
F80CEnvironmentInit;
F80CControlWordInit;
end;

{-------------------------------------------------------------------------------
    F80C state management - exception selection and raise
-------------------------------------------------------------------------------}

procedure F80CSelectAndRaiseException;
var
  LocalState: TF80CState;
  Unmasked:   TX87Exceptions;
begin
{
  Select unmasked pending exception according to floating point exception
  priority and raise appropriate exception object. The priority is as follows:

    - invalid operation (stack faults, unsupported formats, SNaN)
    - QNaN (not an exception)
    - other inv-op, division by zero
    - denormal operand
    - numeric underflow and overflow, possibly with inexact result
    - inexact result
}
LocalState := F80CGetStatePtr^;
{
  This function is only called when exception summary flag is set, so there
  should be at least one unmasked exception.
}
Unmasked := X87SWExceptionFlagsGet(LocalState.ExceptState.StatusWord) -
            X87CWExceptionMasksGet(LocalState.ExceptState.ControlWord);
If excInvalidOp in Unmasked then
  begin
    If X87SWStatusFlagGet(LocalState.ExceptState.StatusWord,sfStackFault) then
      begin
        If X87SWStatusFlagGet(LocalState.ExceptState.StatusWord,sfConditionCodeC1) then
          raise EF80CStackOverflow.Create
        else
          raise EF80CStackUnderflow.Create;
      end
    else raise EF80CInvalidOp.Create;
  end;
If excDivByZero in Unmasked then
  raise EF80CDivByZero.Create;
If excDenormal in Unmasked then
  raise EF80CDenormal.Create;
If excOverflow in Unmasked then
  raise EF80COverflow.Create;
If excUnderflow in Unmasked then
  raise EF80CUnderflow.Create;
If excPrecision in Unmasked then
  raise EF80CPrecision.Create;
end;

{===============================================================================
    F80C state management - low-lewel access implementation
===============================================================================}

Function F80CEmulated: Boolean;
begin
Result := VAR_F80CEmulated;
end;

//------------------------------------------------------------------------------

Function F80CStatusWordGet: UInt16;
begin
If VAR_F80CEmulated then
  Result := F80CGetStatePtr^.StatusWord
else
  Result := X87StatusWordGet;
end;

//------------------------------------------------------------------------------

Function F80CControlWordGet: UInt16;
begin
If VAR_F80CEmulated then
  Result := F80CGetStatePtr^.ControlWord
else
  Result := X87ControlWordGet;
end;

//------------------------------------------------------------------------------

procedure F80CControlWordSet(NewValue: UInt16);
var
  StatePtr: PF80CState;
begin
If VAR_F80CEmulated then
  begin
    StatePtr := F80CExceptStatePrepare;
    // now store the new value
    StatePtr^.ControlWord := NewValue;
  {
    Exception flag bits and mask bits are at corresponding locations, we can
    therefore combine them. Words are masked separately just for the code to
    be clearer (it is technically possible to combine the words and mask only
    the result).
  }
    If ((StatePtr^.StatusWord and X87SW_EFLAG_All) and not (StatePtr^.ControlWord and X87CW_EMASK_All)) <> 0 then
      // fpu busy flag is reflecting the state of exception summary flag
      StatePtr^.StatusWord := StatePtr^.StatusWord or (X87SW_ExceptionSummary or X87SW_FPUBusy)
    else
      StatePtr^.StatusWord := StatePtr^.StatusWord and not (X87SW_ExceptionSummary or X87SW_FPUBusy);
  {
    Only now, when the new value is stored, raise the old unmasked pending
    exceptions.
    This is to better emulate behavior of real x87 FPU - it raises (calls
    handler) the pending unmasked exceptions but it too first changes the
    control word.
  }
    If (StatePtr^.ExceptState.StatusWord and X87SW_ExceptionSummary) <> 0 then
      // note that following exception will clear all exception flags (status word)
      F80CSelectAndRaiseException;
  end
else X87ControlWordSet(NewValue);
end;

//------------------------------------------------------------------------------

procedure F80CEnvironmentInit;
var
  StatePtr: PF80CState;
begin
If VAR_F80CEmulated then
  begin
    StatePtr := F80CGetStatePtr;
    StatePtr^.StatusWord := 0;
    StatePtr^.ControlWord := X87CW_InitialValue;
  end
else X87EnvironmentInit;
end;

//------------------------------------------------------------------------------

procedure F80CControlWordInit;
begin
F80CControlWordSet(X87CW_DefaultValue);
end;

{===============================================================================
    F80C state management - abstracted access implementation
===============================================================================}
{-------------------------------------------------------------------------------
    F80C state management - status word abstracted access
-------------------------------------------------------------------------------}

Function F80CTopOfStackGet: Integer;
begin
Result := X87SWTopOfStackGet(F80CStatusWordGet);
end;

//------------------------------------------------------------------------------

Function F80CStatusFlagGet(Flag: TF80CStatusFlag): Boolean;
begin
Result := X87SWStatusFlagGet(F80CStatusWordGet,Flag);
end;

//------------------------------------------------------------------------------

Function F80CStatusFlagsGet: TF80CStatusFlags;
begin
Result := X87SWStatusFlagsGet(F80CStatusWordGet);
end;

{-------------------------------------------------------------------------------
    F80C state management - control word abstracted access
-------------------------------------------------------------------------------}

Function F80CPrecisionModeGet: TF80CPrecisionMode;
begin
Result := X87CWPrecisionModeGet(F80CControlWordGet);
end;

//------------------------------------------------------------------------------

Function F80CPrecisionModeSet(NewValue: TF80CPrecisionMode): TF80CPrecisionMode;
var
  ControlWord:  UInt16;
begin
ControlWord := F80CControlWordGet;
Result := X87CWPrecisionModeSet(ControlWord,NewValue);
F80CControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function F80CRoundingModeGet: TF80CRoundingMode;
begin
Result := X87CWRoundingModeGet(F80CControlWordGet);
end;

//------------------------------------------------------------------------------

Function F80CRoundingModeSet(NewValue: TF80CRoundingMode): TF80CRoundingMode;
var
  ControlWord:  UInt16;
begin
ControlWord := F80CControlWordGet;
Result := X87CWRoundingModeSet(ControlWord,NewValue);
F80CControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function F80CControlFlagGet(Flag: TF80CControlFlag): Boolean;
begin
Result := X87CWControlFlagGet(F80CControlWordGet,Flag);
end;

//------------------------------------------------------------------------------

Function F80CControlFlagSet(Flag: TF80CControlFlag; NewValue: Boolean): Boolean;
var
  ControlWord:  UInt16;
begin
ControlWord := F80CControlWordGet;
Result := X87CWControlFlagSet(ControlWord,Flag,NewValue);
F80CControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function F80CControlFlagsGet: TF80CControlFlags;
begin
Result := X87CWControlFlagsGet(F80CControlWordGet);
end;

//------------------------------------------------------------------------------

Function F80CControlFlagsSet(NewValue: TF80CControlFlags): TF80CControlFlags;
var
  ControlWord:  UInt16;
begin
ControlWord := F80CControlWordGet;
Result := X87CWControlFlagsSet(ControlWord,NewValue);
F80CControlWordSet(ControlWord);
end;

{-------------------------------------------------------------------------------
    F80C state management - exceptions abstraction
-------------------------------------------------------------------------------}

Function F80CExceptionMaskGet(Exception: TF80CException): Boolean;
begin
Result := X87CWExceptionMaskGet(F80CControlWordGet,Exception);
end;

//------------------------------------------------------------------------------

Function F80CExceptionMaskSet(Exception: TF80CException; NewValue: Boolean): Boolean;
var
  ControlWord:  UInt16;
begin
ControlWord := F80CControlWordGet;
Result := X87CWExceptionMaskSet(ControlWord,Exception,NewValue);
F80CControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function F80CExceptionMasksGet: TF80CExceptions;
begin
Result := X87CWExceptionMasksGet(F80CControlWordGet);
end;

//------------------------------------------------------------------------------

Function F80CExceptionMasksSet(NewValue: TF80CExceptions): TF80CExceptions;
var
  ControlWord:  UInt16;
begin
ControlWord := F80CControlWordGet;
Result := X87CWExceptionMasksSet(ControlWord,NewValue);
F80CControlWordSet(ControlWord);
end;

//------------------------------------------------------------------------------

Function F80CExceptionFlagGet(Exception: TF80CException): Boolean;
begin
Result := X87SWExceptionFlagGet(F80CStatusWordGet,Exception);
end;

//------------------------------------------------------------------------------

Function F80CExceptionFlagsGet: TF80CExceptions;
begin
Result := X87SWExceptionFlagsGet(F80CStatusWordGet);
end;

//------------------------------------------------------------------------------

procedure F80CExceptionsClear;
begin
If VAR_F80CEmulated then
  F80CGetStatePtr^.StatusWord := 0
else
  X87ExceptionsClear;
end;

//------------------------------------------------------------------------------

procedure F80CExceptionsRaise;
var
  StatePtr: PF80CState;
begin
If VAR_F80CEmulated then
  begin
    StatePtr := F80CExceptStatePrepare;
    If X87SWStatusFlagGet(StatePtr^.ExceptState.StatusWord,sfExceptionSummary) then
    {
      Note that following exception will clear exception flags (and more) in
      status word within the emulated state (THRVAR_F80CState).
    }
      F80CSelectAndRaiseException;
  end
else X87ExceptionsRaise;
end;


{===============================================================================
--------------------------------------------------------------------------------
                     Float80 <-> Float64 conversions (F80C)
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    F80C - axiliary functions
===============================================================================}

procedure F80CSignalExceptions(Exceptions: TF80CRaiseExceptions);
var
  StatePtr: PF80CState;
begin
StatePtr := F80CGetStatePtr;
If excInvalidOp in Exceptions then
  StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_InvalidOp;
If excDenormal in Exceptions then
  StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_Denormal;
If excDivByZero in Exceptions then
  StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_DivByZero;
If excOverflow in Exceptions then
  StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_Overflow;
If excUnderflow in Exceptions then
  StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_Underflow;
If excPrecision in Exceptions then
  StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_Precision;
If ([excStackOverflow,excStackUnderflow] * Exceptions) <> [] then
  begin
    StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_InvalidOp;
    StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_StackFault;
  {
    If both stack overflow and underflow are signaled, then overflow takes
    precedence and underflow is ignored.

    Note that C1 is not explicitly cleared - this is to preserve its state in
    case it was set previously.
  }
    If excStackOverflow in Exceptions then
      StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_ConditionCode_C1;
  end;
// set exception summary flag and raise the unmasked exceptions
If ((StatePtr^.StatusWord and X87SW_EFLAG_All) and not (StatePtr^.ControlWord and X87CW_EMASK_All)) <> 0 then
  StatePtr^.StatusWord := StatePtr^.StatusWord or (X87SW_ExceptionSummary or X87SW_FPUBusy)
else
  StatePtr^.StatusWord := StatePtr^.StatusWord and not (X87SW_ExceptionSummary or X87SW_FPUBusy);
F80CExceptionsRaise;
end;

//------------------------------------------------------------------------------

procedure F80CSignalException(Exception: TF80CRaiseException);
begin
F80CSignalExceptions([Exception]);
end;

//==============================================================================
{
  There is a need for calculation with higher width than 64 bits in conversion
  from Float80 to Float64 (in mantissa denormalization).
  Following routines and types implement bare minimum required for calculations
  on 65 bits wide integer.
}
type
  TUInt65 = record
    Low64:  UInt64;
    Bit64:  UInt8;
  end;

const
  UInt65_ZERO: TUInt65 = (Low64: 0; Bit64: 0);

  UI65_CMP_SMALLER = -1;
  UI65_CMP_LARGER  = +1;

//------------------------------------------------------------------------------

Function UInt65Get(Low64: UInt64; Bit64: Byte): TUInt65;
begin
Result.Low64 := Low64;
Result.Bit64 := Bit64 and 1;
end;

//------------------------------------------------------------------------------

Function UInt65Not(const Value: TUInt65): TUInt65;
begin
Result.Low64 := not Value.Low64;
Result.Bit64 := (not Value.Bit64) and 1;
end;

//------------------------------------------------------------------------------

Function UInt65And(const A,B: TUInt65): TUInt65;
begin
Result.Low64 := A.Low64 and B.Low64;
Result.Bit64 := (A.Bit64 and B.Bit64) and 1;
end;

//------------------------------------------------------------------------------

Function UInt65Add(const A,B: TUInt65): TUInt65;{$IFNDEF PurePascal} register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
          A              EAX^              RDX^         RSI(SIL):RDI
          B              EDX^               R8^          RCX(CL):RDX
     Result              ECX^              RCX^          RDX(DL):RAX

 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  In 32bit systems, pointers to the records are passed in registers as usual,
  pointer to the result is passed as a third implicit output argument.

  It is the same in 64bit windows, except that the result pointer is passed in
  first, not last.

  In 64bit non-windows systems, everything is passed directly in registers.
  Result is also returned in register pair.

    NOTE - the provided asm implementation is NOT significantly faster than
           pascal (maybe a little), it is here because I wanted to try it :-P

-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    // adding bits 0..63
    MOV     RAX, qword ptr [RDX]
    ADD     RAX, qword ptr [R8]
    MOV     qword ptr [RCX], RAX

    // adding bit 64
    MOV     AL, byte ptr [RDX + 8]
    ADC     AL, byte ptr [R8 + 8]
    MOV     byte ptr [RCX + 8], AL
  {$ELSE}
    // linux is simple, do it in place and ...
    ADD     RDI, RDX
    ADC     SIL, CL
    AND     RSI, 1

    // ... only copy result to proper registers
    MOV     RAX, RDI
    MOV     RDX, RSI
  {$ENDIF}
{$ELSE}
    // preserve value of EBX across this routine
    PUSH    EBX
    MOV     EBX, EAX

    // adding bits 0..31
    MOV     EAX, dword ptr [EBX]
    ADD     EAX, dword ptr [EDX]
    MOV     dword ptr [ECX], EAX

    // adding bits 32..63
    MOV     EAX, dword ptr [EBX + 4]
    ADC     EAX, dword ptr [EDX + 4]
    MOV     dword ptr [ECX + 4], EAX

    // adding bit 64
    MOV     AL, byte ptr [EBX + 8]
    ADC     AL, byte ptr [EDX + 8]
    AND     AL, 1
    MOV     byte ptr [ECX + 8], AL

    // restore register EBX
    POP     EBX
{$ENDIF}
end;
{$ELSE}
var
  Temp: Int32;
begin
// following approach seems to be fastest
Temp := Int32(Int64Rec(A.Low64).Words[0]) + Int32(Int64Rec(B.Low64).Words[0]);
Int64Rec(Result.Low64).Words[0] := UInt16(Temp);
Temp := Int32(Int64Rec(A.Low64).Words[1]) + Int32(Int64Rec(B.Low64).Words[1]) + (Temp shr 16);
Int64Rec(Result.Low64).Words[1] := UInt16(Temp);
Temp := Int32(Int64Rec(A.Low64).Words[2]) + Int32(Int64Rec(B.Low64).Words[2]) + (Temp shr 16);
Int64Rec(Result.Low64).Words[2] := UInt16(Temp);
Temp := Int32(Int64Rec(A.Low64).Words[3]) + Int32(Int64Rec(B.Low64).Words[3]) + (Temp shr 16);
Int64Rec(Result.Low64).Words[3] := UInt16(Temp);
Result.Bit64 := (A.Bit64 + B.Bit64 + UInt8(Temp shr 16)) and 1;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function UInt65Sub(const A,B: TUInt65): TUInt65;{$IFNDEF PurePascal} register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     RAX, qword ptr [RDX]
    SUB     RAX, qword ptr [R8]
    MOV     qword ptr [RCX], RAX

    MOV     AL, byte ptr [RDX + 8]
    SBB     AL, byte ptr [R8 + 8]
    MOV     byte ptr [RCX + 8], AL
  {$ELSE}
    SUB     RDI, RDX
    SBB     SIL, CL
    AND     RSI, 1

    MOV     RAX, RDI
    MOV     RDX, RSI
  {$ENDIF}
{$ELSE}
    PUSH    EBX
    MOV     EBX, EAX

    MOV     EAX, dword ptr [EBX]
    SUB     EAX, dword ptr [EDX]
    MOV     dword ptr [ECX], EAX

    MOV     EAX, dword ptr [EBX + 4]
    SBB     EAX, dword ptr [EDX + 4]
    MOV     dword ptr [ECX + 4], EAX

    MOV     AL, byte ptr [EBX + 8]
    SBB     AL, byte ptr [EDX + 8]
    AND     AL, 1
    MOV     byte ptr [ECX + 8], AL

    POP     EBX
{$ENDIF}
end;
{$ELSE}
var
  Temp: Int32;
begin
Temp := Int32(Int64Rec(A.Low64).Words[0]) - Int32(Int64Rec(B.Low64).Words[0]);
Int64Rec(Result.Low64).Words[0] := UInt16(Temp);
Temp := Int32(Int64Rec(A.Low64).Words[1]) - Int32(Int64Rec(B.Low64).Words[1]) - ((Temp shr 16) and 1);
Int64Rec(Result.Low64).Words[1] := UInt16(Temp);
Temp := Int32(Int64Rec(A.Low64).Words[2]) - Int32(Int64Rec(B.Low64).Words[2]) - ((Temp shr 16) and 1);
Int64Rec(Result.Low64).Words[2] := UInt16(Temp);
Temp := Int32(Int64Rec(A.Low64).Words[3]) - Int32(Int64Rec(B.Low64).Words[3]) - ((Temp shr 16) and 1);
Int64Rec(Result.Low64).Words[3] := UInt16(Temp);
Result.Bit64 := (A.Bit64 - B.Bit64 - UInt8((Temp shr 16) and 1)) and 1;
end;
{$ENDIF}

//------------------------------------------------------------------------------

Function UInt65RShift(const Value: TUInt65; Shift: Byte): TUInt65;
begin
If Shift <= 0 then
  Result := Value
else If (Shift > 0) and (Shift < 64) then
  begin
    Result.Low64 := (Value.Low64 shr Shift) or UInt64(UInt64(Value.Bit64 and 1) shl (64 - Shift));
    Result.Bit64 := 0;
  end
else If Shift = 64 then
  Result := UInt65Get(Value.Bit64 and 1,0)
else
  Result := UInt65_ZERO
end;

//------------------------------------------------------------------------------

Function UInt65Compare(const A,B: TUInt65): Integer;
begin
If (A.Bit64 and 1) > (B.Bit64 and 1) then
  Result := UI65_CMP_LARGER
else If (A.Bit64 and 1) < (B.Bit64 and 1) then
  Result := UI65_CMP_SMALLER
else
  begin
    If Int64Rec(A.Low64).Hi > Int64Rec(B.Low64).Hi then
      Result := UI65_CMP_LARGER
    else If Int64Rec(A.Low64).Hi < Int64Rec(B.Low64).Hi then
      Result := UI65_CMP_SMALLER
    else
      begin
        If Int64Rec(A.Low64).Lo > Int64Rec(B.Low64).Lo then
          Result := UI65_CMP_LARGER
        else If Int64Rec(A.Low64).Lo < Int64Rec(B.Low64).Lo then
          Result := UI65_CMP_SMALLER
        else
          Result := 0;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function UInt65IsZero(const Value: TUInt65): Boolean;
begin
Result := (Value.Low64 = 0) and ((Value.Bit64 and 1) = 0);
end;

{===============================================================================
    F80C - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    F80C - implementation internals
-------------------------------------------------------------------------------}
{$IFNDEF PurePascal}

procedure Float64ToFloat80_ASM(F64Ptr,F80Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F64Ptr              EAX               RCX               RDI
     F80Ptr              EDX               RDX               RSI
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    FLD     qword ptr [RCX]
    FSTP    tbyte ptr [RDX]
  {$ELSE}
    FLD     qword ptr [RDI]
    FSTP    tbyte ptr [RSI]
  {$ENDIF}
{$ELSE}
    FLD     qword ptr [EAX]
    FSTP    tbyte ptr [EDX]
{$ENDIF}
    FWAIT
end;

//------------------------------------------------------------------------------

procedure Float80ToFloat64_ASM(F80Ptr,F64Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F80Ptr              EAX               RCX               RDI
     F64Ptr              EDX               RDX               RSI
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    FLD     tbyte ptr [RCX]
    FSTP    qword ptr [RDX]
  {$ELSE}
    FLD     tbyte ptr [RDI]
    FSTP    qword ptr [RSI]
  {$ENDIF}
{$ELSE}
    FLD     tbyte ptr [EAX]
    FSTP    qword ptr [EDX]
{$ENDIF}
    FWAIT
end;

{$ENDIF}
//==============================================================================
const
  // 15360, difference between F80 and F64 bias
  F80C_BIAS_DIFF = FLOAT80_EXPONENTBIAS - FLOAT64_EXPONENTBIAS;

//------------------------------------------------------------------------------

procedure Float64ToFloat80_PAS(F64Ptr,F80Ptr: Pointer); register;

  procedure BuildResult(SignExponent: UInt16; Mantissa: UInt64);
  begin
    TFloat80Overlay(F80Ptr^).Mantissa := Mantissa;
    TFloat80Overlay(F80Ptr^).SignExponent := SignExponent;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Function LeadZeroCount(Value: UInt64): Integer;
  begin
    If Value <> 0 then
      begin
        Result := 0;
        while (Value and UInt64($8000000000000000)) = 0  do
          begin
            Value := UInt64(Value shl 1);
            Inc(Result);
          end;
      end
    else Result := 64;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const
  SGN_SHIFT = FLOAT64_SHIFT_SIGN - FLOAT80_SHIFT16_SIGN;    // 48, (r)shifting bit 63 to bit 15
  MAN_SHIFT = 11;                                           // creating 63 bit F80 fraction from 52 bit F64 fraction (lshift)
var
  Sign:           UInt64;   // unshifted (sign is in bit 63)
  Exponent:       Int32;    // biased exponent (bias 1023)
  Mantissa:       UInt64;   // only fraction, without integer bit
  MantissaShift:  Integer;
begin
// raise pending unmasked exceptions before continuing
F80CExceptionsRaise;
Sign := UInt64(F64Ptr^) and FLOAT64_MASK_SIGN;
Exponent := Int32((UInt64(F64Ptr^) and FLOAT64_MASK_EXP) shr FLOAT64_SHIFT_EXP);
Mantissa := UInt64(F64Ptr^) and FLOAT64_MASK_FRAC;
case Exponent of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // zero exponent - zero or denormal
  0:    If Mantissa <> 0 then
          begin
            // non-zero mantissa - denormal
            F80CSignalException(excDenormal);
          {
            Normalize

            Shift mantissa left so that its highest set bit will be shifted
            to integer bit (bit 63), also correct exponent to reflect this
            change.

            And now for the +12 in following calculation - this is somewhat
            complicated, so bear with me...

              We have denormalized F64 number at hand, which means the exponent
              is implicitly -1022 (NOT -1023 as one might expect from biased
              exponent of zero and bias 1023) and implicit integer bit of zero.
              The mantissa consists only of fraction obtained from F64 (bits
              0..51), bits 52..63 (12 bits) are always zero as they were masked
              when mantissa was obtained - but we can imagine that bit 52 is
              an integer bit (since it is zero for denormals anyway). So,
              technically, we have the entire mantissa in bits 0..52.

              Now we need to count how many times we need to exponentiate the
              mantissa (left-shift it) so that the first set (1) bit is moved
              to the integer bit (position 52) - we simply count leading zeroes.
              But remember that highest 11 bits do not belong to the mantissa,
              yet they are counted (this is to simplify shifting of the mantisa
              into the final result). So we simply decrement the number of
              counted leading zeroes by 11 - so why the f*** we are adding 12?!

              Because devil is hidden in detail, and here it is the exponent.
              We have source exponent of -1022, minus the exponentiation (shift)
              count. But -1022 when biased for F80 is not equal to 15360
              (F80C_BIAS_DIFF), but 15361 (F80C_BIAS_DIFF + 1).

              So the final (biased) F80 exponent equals to:

                  (F80C_BIAS_DIFF + 1) - (MantissaShift - 11)

              When we simplify, we get what you see below :)
          }
            MantissaShift := LeadZeroCount(Mantissa);
            BuildResult(UInt16(Sign shr SGN_SHIFT) or UInt16(F80C_BIAS_DIFF + 12 - MantissaShift),
                        UInt64(Mantissa shl MantissaShift));
          end
        // return signed zero
        else BuildResult(UInt16(Sign shr SGN_SHIFT),0);

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // max exponent - infinity or NaN
  $7FF: If Mantissa <> 0 then
          begin
            // not a number
            If (Mantissa and FLOAT64_MASK_FHB) = 0 then
              begin
                // signaled NaN
                F80CSignalException(excInvalidOp);
                // if no exception was raised, return quiet signed NaN with mantissa
                BuildResult(UInt16(Sign shr SGN_SHIFT) or FLOAT80_MASK16_EXP,
                            UInt64(Mantissa shl MAN_SHIFT) or FLOAT80_MASK64_FHB or FLOAT80_MASK64_INTB)
              end
            // quiet signed NaN with mantissa
            else BuildResult(UInt16(Sign shr SGN_SHIFT) or FLOAT80_MASK16_EXP,
                             UInt64(Mantissa shl MAN_SHIFT) or FLOAT80_MASK64_INTB);
          end
        // signed infinity
        else BuildResult(UInt16(Sign shr SGN_SHIFT) or FLOAT80_MASK16_EXP,FLOAT80_MASK64_INTB);

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  // normal number
  BuildResult(UInt16(Sign shr SGN_SHIFT) or UInt16(Exponent + F80C_BIAS_DIFF),
              UInt64(Mantissa shl MAN_SHIFT) or FLOAT80_MASK64_INTB);
end;
end;

//------------------------------------------------------------------------------

procedure Float80ToFloat64_PAS(F80Ptr,F64Ptr: Pointer); register;
const
{
  Default shift of mantissa - it is shifted by 11 because we need to shift bit
  62 (highest bit of fraction in F80 mantissa) to position 51 (highest bit of
  fraction in F64 mantissa).
}
  MAN_SHIFT = 11;
var
  RoundingMode: TX87RoundingMode; // cached for use in nested subroutines

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Function DenormalizeMantissa(Sign,Mantissa: UInt64; Shift: Integer; out DataLoss: Boolean): UInt64;
  var
    Mantissa65: TUInt65;
    Mask:       TUInt65;
    Low,High:   TUInt65;
  begin
    DataLoss := False;
    If (Shift > 0) and (Shift <= 64) then
      begin
        Mantissa65 := UInt65Get(Mantissa,0);
        // mask bits that will be shifted (to the right) out of the mantissa
        Mask := UInt65RShift(UInt65Get(UInt64(-1),0),64 - Shift);
        If not UInt65IsZero(UInt65And(Mantissa65,Mask)) then
          begin
            // some shifted out bits are not zero
            DataLoss := True;
            // bits not shifted out
            Low := UInt65And(Mantissa65,UInt65Not(Mask));
          {
            Bits not shifted out plus one (the one is added to the least
            significant bit that is NOT shifted out).
          }
            High := UInt65Add(Low,UInt65Add(Mask,UInt65Get(1,0)));
            // now calculate the result depending on rounding mode
            case RoundingMode of
              rmDown:     If Sign <> 0 then
                            Result := UInt65RShift(High,Shift).Low64
                          else
                            Result := UInt65RShift(Low,Shift).Low64;
              rmUp:       If Sign <> 0 then
                            Result := UInt65RShift(Low,Shift).Low64
                          else
                            Result := UInt65RShift(High,Shift).Low64;
              rmTruncate: Result := UInt65RShift(Low,Shift).Low64;
            else
             {rmNearest}
              // use value (Low or High) that is closer to the original mantissa
              case UInt65Compare(UInt65Sub(Mantissa65,Low),UInt65Sub(High,Mantissa65)) of
                UI65_CMP_SMALLER: Result := UInt65RShift(Low,Shift).Low64;
                UI65_CMP_LARGER:  Result := UInt65RShift(High,Shift).Low64;
              else
              {
                Low and High are equally distant from mantissa, select the one
                which has clear (0) lowest bit.
              }
                If UInt65IsZero(UInt65And(Low,UInt65Add(Mask,UInt65Get(1,0)))) then
                  Result := UInt65RShift(Low,Shift).Low64
                else
                  Result := UInt65RShift(High,Shift).Low64;
              end;
            end;
          end
        // all shifted out bits are zero
        else Result := Mantissa shr Shift;
      end
    // following cases should never happen, but to be complete...
    else If Shift > 64 then
      Result := 0
    else
      Result := Mantissa;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Function ShiftMantissa(Sign,Mantissa: UInt64; out DataLoss: Boolean): UInt64;
  const
    MANTISSA_MASK = UInt64($7FF);
  var
    Low,High: UInt64;
    CmpRes:   Integer;
  begin
  {
    Implicit mantissa shift is 11, creating static mask of $7FF. Passed
    mantissa must have integer bit cleared.
  }
    DataLoss := False;
    If (Mantissa and MANTISSA_MASK) <> 0 then
      begin
        // non-zero bits are shifted out...
        DataLoss := True;
        Low := Mantissa and not MANTISSA_MASK;
        High := Low + (MANTISSA_MASK + 1);
        case RoundingMode of
          rmDown:     If Sign <> 0 then
                        Result := High shr MAN_SHIFT
                      else
                        Result := Low shr MAN_SHIFT;
          rmUp:       If Sign <> 0 then
                        Result := Low shr MAN_SHIFT
                      else
                        Result := High shr MAN_SHIFT;
          rmTruncate: Result := Low shr MAN_SHIFT;
        else
         {rmNearest}
          // select value closer to given mantissa
          CmpRes := CompareUInt64(Mantissa - Low,High - Mantissa);
          If CmpRes < 0 then
            Result := Low shr MAN_SHIFT
          else If CmpRes > 0 then
            Result := High shr MAN_SHIFT
          else
            begin
            {
              Both values are the same distance from mantissa, select the one
              with clear lowest bit.
            }
              If (Low and (MANTISSA_MASK + 1)) = 0 then
                Result := Low shr MAN_SHIFT
              else
                Result := High shr MAN_SHIFT;
            end;
        end;
      end
    else Result := Mantissa shr MAN_SHIFT;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
var
  Sign:     UInt64; // sign is pre-shifted to bit 63
  Exponent: Int32;  // biased exponent (bias 16383)
  Mantissa: UInt64; // includes integer bit
  DataLoss: Boolean;
  Temp:     UInt64;
  Overflow: Boolean;
begin
F80CExceptionsRaise;
RoundingMode := F80CRoundingModeGet;
// 48 because we are left-shifting bit 15 to bit 63
Sign := UInt64(UInt64(TFloat80Overlay(F80Ptr^).SignExponent and FLOAT80_MASK16_SIGN) shl 48);
Exponent := Int32(TFloat80Overlay(F80Ptr^).SignExponent and FLOAT80_MASK16_EXP);
Mantissa := TFloat80Overlay(F80Ptr^).Mantissa;
// check unsupported encodings...
If ((Exponent > 0) and (Exponent <= FLOAT80_MASK16_EXP)) and ((Mantissa and FLOAT80_MASK64_INTB) = 0) then
  begin
  {
    Unnormals (seemingly normalized numbers, but with integer bit of 0),
    pseudo-infinities and pseudo-NaNs (both with integer bit 0).
  }
    F80CSignalException(excInvalidOp);
  {
    Return negative QNaN (QNaN floating point indefinite).

    The constants are casted to UInt64 because older FPC is throwing
    nonsensical error without it.
  }
    UInt64(F64Ptr^) := UInt64(FLOAT64_MASK_SIGN or FLOAT64_MASK_EXP or FLOAT64_MASK_FHB);
  end
else
  case Exponent of
    //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    // exponent of zero - zero or denormal
    0:      If Mantissa <> 0 then
              begin
              {
                non-zero mantissa - denormals

                Note that psedo-denormals (denormals with integer bit 1) are
                treated as usual denormals (with integer bit 0).

                Also note that real x87 FPU is not signaling denormal exception
                when converting from extended precision numbers.
              }
                F80CSignalException(excUnderflow);
                If ((RoundingMode = rmUp) and (Sign = 0)) or
                   ((RoundingMode = rmDown) and (Sign <> 0)) then
                  // return signed smallest representable number (denormal)
                  UInt64(F64Ptr^) := Sign or UInt64(1)
                else
                  // convert to signed zero
                  UInt64(F64Ptr^) := Sign;
                F80CSignalException(excPrecision);
              end
            // mantissa of 0 - return signed zero
            else UInt64(F64Ptr^) := Sign;

    //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  {
    Exponent 1..15307 (-16382..-1076 unbiased) - exponent too small to be
    represented in double even as denormal.
  }
    $1..
    $3BCB:  begin
              F80CSignalException(excUnderflow);
              If ((RoundingMode = rmUp) and (Sign = 0)) or
                 ((RoundingMode = rmDown) and (Sign <> 0)) then
                // return signed smallest representable number (denormal)
                UInt64(F64Ptr^) := Sign or UInt64(1)
              else
                // convert to signed zero
                UInt64(F64Ptr^) := Sign;
              F80CSignalException(excPrecision);
            end;

    //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  {
    Exponent 15308..15359 (-1075..-1024 unbiased) - exponent still too small to
    be represented in double, but can be denormalized to fit (result will have
    implicit exponent of -1022, explicit 0).
  }
    $3BCC..
    $3BFF:  begin
            {
              denormalize

              Conversion followed by a gradual underflow should take place
              here, but it is replaced by a one-time shift, which provides
              the same results and exceptions for given exponent range and
              is faster.

              Shift here can be from 13 ($3C0C - $3BFF) to 64 (see further),
              DataLoss indicates that some non-zero bits were lost when
              mantissa was shifted to the right.

                Maximum shift (64) clears the entire mantissa, but, thanks to
                rounding, one set bit may still be present at the lowest place
                and therefore result will not be zero.

                As for minimum shift - we are creating 53bit mantissa (52 bits
                of fraction and an integer bit) from 64bit mantissa, this is
                right shift of 11. So, if we shift by 11, we get normalized
                mantissa with integer bit of 1 and an exponent -1024. But we
                need to get to exponent of -1022 (implicit exponent for F64
                denormals), therefore we need to shift mantissa by another two
                places - that is how we get minimum shift of 13.
            }
              Temp := Sign or DenormalizeMantissa(Sign,Mantissa,$3C0C - Exponent,DataLoss);
            {
              post-computation exceptions

              Note that, when underflow is masked, the underflow exception can
              be signaled only when the result is also inexact.
            }
              If F80CExceptionMaskGet(excUnderflow) then
                begin
                  UInt64(F64Ptr^) := Temp;
                  // underflow exception is masked
                  If DataLoss then
                    // inexact result
                    F80CSignalExceptions([excUnderflow,excPrecision]);
                end
              else F80CSignalException(excUnderflow);
            end;

    //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  {
    Exponent 15360 (-1023 unbiased) - similar to previous case, but with more
    complexities because it can yield a normalized number as a result of
    denormalization (thanks to rounding).
  }
    $3C00:  begin
            {
              Right-shift mantissa by 11 places just to align bits. Note that
              since rounding takes place here, there is a possibility that
              mantissa overflows into bit 53 (one place above integer bit).
            }
              Temp := DenormalizeMantissa(Sign,Mantissa,MAN_SHIFT,DataLoss);
              If Temp <> UInt64(FLOAT64_MASK_INTB shl 1{one place above virtual integer bit}) then
                begin
                {
                  Mantissa has not overflowed (bit 53 is zero), meaning result
                  will still be a denormal.
                }
                  If F80CExceptionMaskGet(excUnderflow) then
                    begin
                    {
                      Right-shift the mantissa by 12 places so we shift-out the
                      integer bit into fraction and get it to exponent of -1022.
                    }
                      UInt64(F64Ptr^) := Sign or DenormalizeMantissa(Sign,Mantissa,MAN_SHIFT + 1,DataLoss);
                    {
                      Signal precision and underflow exceptions if non-zero
                      bits were shifted out.
                    }
                      If DataLoss then
                        F80CSignalExceptions([excUnderflow,excPrecision]);
                    end
                  else F80CSignalException(excUnderflow);
                end
              else
                begin
                {
                  Mantissa overflowed (bit 53 is one, all other bits are zero),
                  therefore result is promoted to a normalized number.

                  We can right-shift it by one more place to get mantissa with
                  integer bit 1, fraction of zero and exponent -1022. But there
                  is no need to do all this - we simply set F64 fraction to
                  zero, biased exponent to 1 (unbiased -1022, integer bit is
                  then implicitly 1), copy the sign and that's it.
                }
                  UInt64(F64Ptr^) := Sign or UInt64(UInt64(1) shl FLOAT64_SHIFT_EXP);
                  F80CSignalException(excPrecision);
                end;
             end;

    //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  {
    Exponent 17407..32766 (1024..16383 unbiased) - exponent too large to be
    represented in double (max. valid exponent 1023).
  }
    $43FF..
    $7FFE:  begin
              F80CSignalException(excOverflow);
              If (RoundingMode = rmTruncate) or
                 ((RoundingMode = rmUp) and (Sign <> 0)) or
                 ((RoundingMode = rmDown) and (Sign = 0)) then
                // return signed largest representable number (max. exp. - 1)
                UInt64(F64Ptr^) := Sign or UInt64(not FLOAT64_MASK_SIGN and not UInt64(UInt64(1) shl FLOAT64_SHIFT_EXP))
              else
                // convert to signed infinity
                UInt64(F64Ptr^) := Sign or FLOAT64_MASK_EXP;
              F80CSignalException(excPrecision);
            end;

    //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  {
    Maximum exponent - NaN or infinity (note that pseudo-infinities and
    pseudo-NaN are managed separately along with unnormals).
  }
    $7FFF:  If (Mantissa and FLOAT80_MASK64_FRAC) <> 0 then
              begin
                // non-zero fraction - not a number (NaN)
                If (Mantissa and FLOAT80_MASK64_FHB) = 0 then
                  begin
                    // highest bit of fraction is zero - signaling NaN
                    F80CSignalException(excInvalidOP);
                  {
                    No exception was raised (InvalidOP was masked), return
                    quiet signed NaN with truncated mantissa.
                  }
                    UInt64(F64Ptr^) := Sign or FLOAT64_MASK_EXP or FLOAT64_MASK_FHB or (Mantissa shr MAN_SHIFT);
                  end
              {
                Highest bit of fraction is non-zero - return quiet signed NaN
                with truncated mantissa.
              }
                else UInt64(F64Ptr^) := Sign or FLOAT64_MASK_EXP or FLOAT64_MASK_FHB or (Mantissa shr MAN_SHIFT);
              end
            // fraction of zero - renturn signed infinity
            else UInt64(F64Ptr^) := Sign or FLOAT64_MASK_EXP;

    //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  else
  {
    Exponent 15361..17406 (-1022..1023 unbiased) - representable normalized
    value.
  }
    Mantissa := ShiftMantissa(Sign,Mantissa and not FLOAT80_MASK64_INTB,DataLoss);
  {
    Check whether mantisa overflowed - if so, increment exponent to compensate.
    Fraction will then be zero.
  }
    If (Mantissa and not FLOAT64_MASK_FRAC) <> 0 then
      begin
        Inc(Exponent);
        Overflow := True;
      end
    else Overflow := False;
    // post-computation exceptions
    If Overflow and (Exponent > 17406) then
      // number, when constructed, will produce an infinity
      F80CSignalException(excOverflow);
    // construct the resulting number
    UInt64(F64Ptr^) := Sign or
      {exponent}(UInt64(UInt64(Exponent - F80C_BIAS_DIFF) shl FLOAT64_SHIFT_EXP) and
      FLOAT64_MASK_EXP) or {fraction}(Mantissa and FLOAT64_MASK_FRAC);
    If DataLoss then
      // inexact result
      F80CSignalException(excPrecision);
  end;
end;

//==============================================================================
var
  VAR_Float64ToFloat80: procedure(F64Ptr,F80Ptr: Pointer); register = Float64ToFloat80_PAS;
  VAR_Float80ToFloat64: procedure(F80Ptr,F64Ptr: Pointer); register = Float80ToFloat64_PAS;

{-------------------------------------------------------------------------------
    F80C - implementation of public functions
-------------------------------------------------------------------------------}

procedure Float64ToFloat80(Float64Ptr,Float80Ptr: Pointer);
begin
VAR_Float64ToFloat80(Float64Ptr,Float80Ptr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float64ToFloat80(const Value: Float64): Float80;
begin
VAR_Float64ToFloat80(@Value,@Result);
end;

//------------------------------------------------------------------------------

procedure DoubleToExtended(DoublePtr,ExtendedPtr: Pointer);
begin
{$IF SizeOf(Extended) = 10}
VAR_Float64ToFloat80(DoublePtr,ExtendedPtr);
{$ELSE}
Extended(ExtendedPtr^) := Double(DoublePtr^);
{$IFEND}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DoubleToExtended(const Value: Double): Extended;
begin
{$IF SizeOf(Extended) = 10}
VAR_Float64ToFloat80(@Value,@Result);
{$ELSE}
Result := Value;
{$IFEND}
end;

//==============================================================================

procedure Float80ToFloat64(Float80Ptr,Float64Ptr: Pointer);
begin
VAR_Float80ToFloat64(Float80Ptr,Float64Ptr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float80ToFloat64(const Value: Float80): Float64;
begin
VAR_Float80ToFloat64(@Value,@Result);
end;

//------------------------------------------------------------------------------

procedure ExtendedToDouble(ExtendedPtr,DoublePtr: Pointer);
begin
{$IF SizeOf(Extended) = 10}
VAR_Float80ToFloat64(ExtendedPtr,DoublePtr);
{$ELSE}
Double(DoublePtr^) := Extended(ExtendedPtr^);
{$IFEND}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ExtendedToDouble(const Value: Extended): Double; overload;
begin
{$IF SizeOf(Extended) = 10}
VAR_Float80ToFloat64(@Value,@Result);
{$ELSE}
Result := Value;
{$IFEND}
end;


{===============================================================================
--------------------------------------------------------------------------------
                   SSE/AVX control and status (CS) management
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    SSE/AVX CS management - control and status mask
===============================================================================}
{
  VAR_VECControlAndStatusMask

  Stores mask that needs to be applied to a value being written into MXCSR
  (vector status and control register).

  This variable is written into once and only once, at the unit initialization.
  It must not be written into later at any cost, that would break thread safety.
  Therefore consider this variable to be read-only.

  If compiling in pure pascal mode or if no vector extension is supported by
  the CPU, then this mask retains its default value of zero, which indicates
  that no bits and their functions are supported.
}
var
  VAR_VECControlAndStatusMask: UInt32 = 0;

//------------------------------------------------------------------------------
{$IFNDEF PurePascal}

Function ControlAndStatusMaskGet(Storage: Pointer): UInt32; register; assembler;
asm
{
  - FXSAVE does not check for pending FPU exceptions, added FWAIT to raise them
  - state saved by FXSAVE >might< contain MXCSR_MASK provided by the CPU (if it
    is zero, CPU is not providing it - this case is taken care of later)
  - position of the mask is the same in all CPU modes (offset 28) - no need to
    branch for individual modes
}
    FWAIT
{$IFDEF x64}
  {$IFDEF Windows}
    FXSAVE  [RCX]
    MOV     EAX,  dword ptr [RCX + 28]
  {$ELSE}
    FXSAVE  [RDI]
    MOV     EAX,  dword ptr [RDI + 28]
  {$ENDIF}
{$ELSE}
    FXSAVE  [EAX]
    MOV     EAX,  dword ptr [EAX + 28]
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure ControlAndStatusMaskInit;
const
  // default value for MXCSR mask, DAZ bit not supported
  DefaultMask  = UInt32($0000FFBF);
var
  Buffer:           Pointer;
  BufferInt:        PtrUInt absolute Buffer;
  AlignedBuffer:    Pointer;
  AlignedBufferInt: PtrUInt absolute AlignedBuffer;
  Mask:             UInt32; // as returned by hardware
begin
// memory for FXSAVE must be 16-byte aligned and intialized to all-zero
Buffer := AllocMem(528{512 + 16 bytes for possible alignment});
try
  AlignedBufferInt := (BufferInt + 15) and not PtrUInt($F);
  Mask := ControlAndStatusMaskGet(AlignedBuffer);
{
  If field MXCSR_MASK in FXSAVE image is zero, then CPU does not provide the
  mask and we use its default value. If it is non-zero, we use whatever the
  hardware provided.
}
  If Mask <> 0 then
    VAR_VECControlAndStatusMask := Mask
  else
    VAR_VECControlAndStatusMask := DefaultMask;
finally
  FreeMem(Buffer,528);
end;
end;

{$ENDIF}

{===============================================================================
    SSE/AVX CS management - low-lewel access implementation
===============================================================================}
{$IFNDEF PurePascal}

Function VECControlAndStatusGet_ASM: UInt32; register; assembler;
asm
{$IFDEF x64}
    SUB     RSP, 8
    STMXCSR dword ptr [RSP]
    POP     RAX               // only lower 32 bits will be used
{$ELSE}
    SUB     ESP, 4            // make room on stack
    STMXCSR dword ptr [ESP]   // store the value on stack
    POP     EAX               // pop it into result
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure VECControlAndStatusSet_ASM(NewValue: UInt32); register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    PUSH    RCX
  {$ELSE}
    PUSH    RDI
  {$ENDIF}
    LDMXCSR dword ptr [RSP]   // loading only lower 32 bits
    ADD     RSP, 8
{$ELSE}
    PUSH    EAX               // push the new value on stack
    LDMXCSR dword ptr [ESP]   // load itinto register from there
    ADD     ESP, 4            // clear the stack
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure VECFloatDataGet_SSE(Storage: Pointer); register; assembler;
asm
{
  Why am I writing the registers directly instead of using argument name
  (Storage) and then letting compiler to replace it with proper register
  identifier?
  Because I have seen FPC replace EXPLICITLY given register (it replaced
  DL with EDX - and NO, they are NOT the same), and from that time I do
  not trust it will be done correctly.

  Also note that there should be size specifiers (dqword ptr) for memory
  addressing everywhere, but FPC throws some nonsensical warnings on them,
  and Delphi seems to not care they are missing, so I got rid of them.
}
{$IFDEF x64}
  {$IFDEF Windows}
    MOVUPS  [RCX + 000], XMM0
    MOVUPS  [RCX + 064], XMM1
    MOVUPS  [RCX + 128], XMM2
    MOVUPS  [RCX + 192], XMM3
    MOVUPS  [RCX + 256], XMM4
    MOVUPS  [RCX + 320], XMM5
    MOVUPS  [RCX + 384], XMM6
    MOVUPS  [RCX + 448], XMM7

    MOVUPS  [RCX + 512], XMM8
    MOVUPS  [RCX + 576], XMM9
    MOVUPS  [RCX + 640], XMM10
    MOVUPS  [RCX + 704], XMM11
    MOVUPS  [RCX + 768], XMM12
    MOVUPS  [RCX + 832], XMM13
    MOVUPS  [RCX + 896], XMM14
    MOVUPS  [RCX + 960], XMM15
  {$ELSE}
    MOVUPS  [RDI + 000], XMM0
    MOVUPS  [RDI + 064], XMM1
    MOVUPS  [RDI + 128], XMM2
    MOVUPS  [RDI + 192], XMM3
    MOVUPS  [RDI + 256], XMM4
    MOVUPS  [RDI + 320], XMM5
    MOVUPS  [RDI + 384], XMM6
    MOVUPS  [RDI + 448], XMM7

    MOVUPS  [RDI + 512], XMM8
    MOVUPS  [RDI + 576], XMM9
    MOVUPS  [RDI + 640], XMM10
    MOVUPS  [RDI + 704], XMM11
    MOVUPS  [RDI + 768], XMM12
    MOVUPS  [RDI + 832], XMM13
    MOVUPS  [RDI + 896], XMM14
    MOVUPS  [RDI + 960], XMM15
  {$ENDIF}
{$ELSE}
    MOVUPS  [EAX + 000], XMM0
    MOVUPS  [EAX + 064], XMM1
    MOVUPS  [EAX + 128], XMM2
    MOVUPS  [EAX + 192], XMM3
    MOVUPS  [EAX + 256], XMM4
    MOVUPS  [EAX + 320], XMM5
    MOVUPS  [EAX + 384], XMM6
    MOVUPS  [EAX + 448], XMM7
{$ENDIF}
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

procedure VECFloatDataGet_AVX(Storage: Pointer); register; assembler;
asm
{
  VMOVUPS ymm2/m256, ymm1

  VEX.256.0F.WIG 11 /r

  The encoding allows for 2-byte VEX ($0F prefix is automatically implied for
  it and field W is ignored (WIG)), so...

    byte #0 ($C5)      - first byte of 2-byte VEX prefix
    byte #1 ($FC,$7C)  - second VEX byte, bit fields are as follows:

           R (7)    = 1/0   ... corresponds to inverted REX.W, ie. its inverted
                                value is prepended to reg field in ModR/M byte
                                (if zero, then value in reg is incremented by
                                8, if one then reg is not changed)
        vvvv (3..6) = 1111  ... stored inverted, unused
           L (2)    = 1     ... 256bit vectors
          pp (0..1) = 00    ... no SIMD prefix

    byte #2 ($11)       - instruction opcode
    byte #3 (ModR/M)    - encodes source and destionation registers:

         r/m (0..2) ... here used to identify register in memory addressing
         reg (3..5) ... identifies source vector register, expanded by inverted
                        R field from second byte
         mod (6..7) ... used to idetify memory addressing mode

         Let's have some values to see real examples:

            $00
                  r/m = 0  ... address given in register EAX/RAX
                  reg = 0  ... vector source register YMM0
                  mod = 0  ... absolute address in r/m register, no displacement

            $48
                  r/m = 0  ... base address in EAX/RAX
                  reg = 1  ... register YMM1
                  mod = 1  ... r/m register plus 8bit displacement

            $B0
                  r/m = 0  ... base address in EAX/RAX
                  reg = 6  ... register YMM6
                  mod = 2  ... r/m register plus 32bit displacement

            $AF
                  r/m = 7  ... base register in EDI/RDI
                  reg = 5  ... register YMM5 (or YMM13 if expanded from R)
                  mod = 2  ..  r/m register plus 32bit displacement

  Other trailing bytes, if present, are just the memory offsets (displacement).
}
{$IFDEF x64}
  {$IFDEF Windows}
    DB  $C5, $FC, $11, $01                      // VMOVUPS ymmword ptr [RCX + 000], YMM0
    DB  $C5, $FC, $11, $49, $40                 // VMOVUPS ymmword ptr [RCX + 064], YMM1
    DB  $C5, $FC, $11, $91, $80, $00, $00, $00  // VMOVUPS ymmword ptr [RCX + 128], YMM2
    DB  $C5, $FC, $11, $99, $C0, $00, $00, $00  // VMOVUPS ymmword ptr [RCX + 192], YMM3
    DB  $C5, $FC, $11, $A1, $00, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 256], YMM4
    DB  $C5, $FC, $11, $A9, $40, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 320], YMM5
    DB  $C5, $FC, $11, $B1, $80, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 384], YMM6
    DB  $C5, $FC, $11, $B9, $C0, $01, $00, $00  // VMOVUPS ymmword ptr [RCX + 448], YMM7

    DB  $C5, $7C, $11, $81, $00, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 512], YMM8
    DB  $C5, $7C, $11, $89, $40, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 576], YMM9
    DB  $C5, $7C, $11, $91, $80, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 640], YMM10
    DB  $C5, $7C, $11, $99, $C0, $02, $00, $00  // VMOVUPS ymmword ptr [RCX + 704], YMM11
    DB  $C5, $7C, $11, $A1, $00, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 768], YMM12
    DB  $C5, $7C, $11, $A9, $40, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 832], YMM13
    DB  $C5, $7C, $11, $B1, $80, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 896], YMM14
    DB  $C5, $7C, $11, $B9, $C0, $03, $00, $00  // VMOVUPS ymmword ptr [RCX + 960], YMM15
  {$ELSE}
    DB  $C5, $FC, $11, $07                      // VMOVUPS ymmword ptr [RDI + 000], YMM0
    DB  $C5, $FC, $11, $4F, $40                 // VMOVUPS ymmword ptr [RDI + 064], YMM1
    DB  $C5, $FC, $11, $97, $80, $00, $00, $00  // VMOVUPS ymmword ptr [RDI + 128], YMM2
    DB  $C5, $FC, $11, $9F, $C0, $00, $00, $00  // VMOVUPS ymmword ptr [RDI + 192], YMM3
    DB  $C5, $FC, $11, $A7, $00, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 256], YMM4
    DB  $C5, $FC, $11, $AF, $40, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 320], YMM5
    DB  $C5, $FC, $11, $B7, $80, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 384], YMM6
    DB  $C5, $FC, $11, $BF, $C0, $01, $00, $00  // VMOVUPS ymmword ptr [RDI + 448], YMM7

    DB  $C5, $7C, $11, $87, $00, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 512], YMM8
    DB  $C5, $7C, $11, $8F, $40, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 576], YMM9
    DB  $C5, $7C, $11, $97, $80, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 640], YMM10
    DB  $C5, $7C, $11, $9F, $C0, $02, $00, $00  // VMOVUPS ymmword ptr [RDI + 704], YMM11
    DB  $C5, $7C, $11, $A7, $00, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 768], YMM12
    DB  $C5, $7C, $11, $AF, $40, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 832], YMM13
    DB  $C5, $7C, $11, $B7, $80, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 896], YMM14
    DB  $C5, $7C, $11, $BF, $C0, $03, $00, $00  // VMOVUPS ymmword ptr [RDI + 960], YMM15
  {$ENDIF}
{$ELSE}
    DB  $C5, $FC, $11, $00                      // VMOVUPS ymmword ptr [EAX + 000], YMM0
    DB  $C5, $FC, $11, $48, $40                 // VMOVUPS ymmword ptr [EAX + 064], YMM1
    DB  $C5, $FC, $11, $90, $80, $00, $00, $00  // VMOVUPS ymmword ptr [EAX + 128], YMM2
    DB  $C5, $FC, $11, $98, $C0, $00, $00, $00  // VMOVUPS ymmword ptr [EAX + 192], YMM3
    DB  $C5, $FC, $11, $A0, $00, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 256], YMM4
    DB  $C5, $FC, $11, $A8, $40, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 320], YMM5
    DB  $C5, $FC, $11, $B0, $80, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 384], YMM6
    DB  $C5, $FC, $11, $B8, $C0, $01, $00, $00  // VMOVUPS ymmword ptr [EAX + 448], YMM7
{$ENDIF}
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

procedure VECFloatDataGet_AVX512(Storage: Pointer); register; assembler;
asm
(*
  VMOVUPS zmm2/m512 {k1}{z}, zmm1

  EVEX.512.0F.W0 11 /r

    byte #0 ($62)      - first byte of EVEX prefix (static value)
    byte #1 (P0)       - second EVEX byte (payload byte #0), encoded this way:

       mmm (0..2) ... decoding map (here it is always 1, corresponds to escape
                      sequence $0F)
        R' (4)    ... high-16 register specifier modifier, stored inverted
                      (expands reg field in ModR/M byte - if zero then adding
                      16)
       RXB (5..7) ... corresponds to REX.RXB (expands fields in ModR/M), stored
                      inverted

    byte #2 (P1 = $7C) - payload byte #1, here it is always $7C:

        pp (0..1) = 0    ... compressed legacy prefix (0 = no SIMD prefix)
      vvvv (3..6) = 1111 ... register specifier (inverted), here unused
         W (7)    = 0    ... operand size promotion (0, so no promotion)

    byte #3 (P2 = $48) - payload byte #2, here always $48:

       aaa (0..2) = 0 ... embedded opmask register specifier (0 = K0)
        V' (3)    = 1 ... high-16 VVVV/VIDX register specifier (stored
                          inverted, so here it is 0)
         b (4)    = 0 ... unused here (must be 0)
       L'L (5..6) = 2 ... 512bit vectors
         z (7)    = 0 ... zeroing/merging (0, so no zeroing)

    byte #4 ($11)      - opcode
    byte #5 (ModR/M)   - source and destination encodng

         r/m (0..2) ... here used to identify register in memory addressing
         reg (3..5) ... identifies source vector register, expanded by inverted
                        R (from RXB) and R' fields from second byte (resulting
                        in bit pattern R'Rrrr - giving 5bit number for values
                        0 up to 31).
         mod (6..7) ... used to idetify memory addressing mode

      Some real-world examples would be:

            $60 (P0=$F1 R'=0 R=0)
                  r/m = 0  ... base address in EAX/RAX
                  reg = 4  ... vector source register ZMM4
                  mod = 1  ... r/m register plus 8bit displacement (here
                               compressed by a factor of N=64)

            $51 (P0=$71 R'=0 R=1)
                  r/m = 1  ... base address in ECX/RCX
                  reg = 2  ... vector source register ZMM10 (2 + 8(R=1))
                  mod = 1  ... r/m register plus 8bit compressed displacement
                               (N=64)

            $79 (P0=$61 R'=1 R=1)
                  r/m = 1  ... base address in ECX/RCX
                  reg = 7  ... vector source register ZMM31
                               (7 + 8(R=1) + 16(R'=1))
                  mod = 1  ... r/m register plus 8bit compressed displacement
                               (N=64)

            $5F (P0=$E1 R'=1 R=0)
                  r/m = 7  ... base address in EDI/RDI
                  reg = 3  ... vector source register ZMM19 (3 + 16(R'=1))
                  mod = 1  ... r/m register plus 8bit compressed displacement
                               (N=64)

    byte #6 (disp)     - compressed displacement for destination (here the true
                         displacement is divided by 64 and result is stored as
                         8bit displacement)
*)
{$IFDEF x64}
  {$IFDEF Windows}
    DB  $62, $F1, $7C, $48, $11, $01      // VMOVUPS zmmword ptr [RCX + 0000], ZMM0
    DB  $62, $F1, $7C, $48, $11, $49, $01 // VMOVUPS zmmword ptr [RCX + 0064], ZMM1
    DB  $62, $F1, $7C, $48, $11, $51, $02 // VMOVUPS zmmword ptr [RCX + 0128], ZMM2
    DB  $62, $F1, $7C, $48, $11, $59, $03 // VMOVUPS zmmword ptr [RCX + 0192], ZMM3
    DB  $62, $F1, $7C, $48, $11, $61, $04 // VMOVUPS zmmword ptr [RCX + 0256], ZMM4
    DB  $62, $F1, $7C, $48, $11, $69, $05 // VMOVUPS zmmword ptr [RCX + 0320], ZMM5
    DB  $62, $F1, $7C, $48, $11, $71, $06 // VMOVUPS zmmword ptr [RCX + 0384], ZMM6
    DB  $62, $F1, $7C, $48, $11, $79, $07 // VMOVUPS zmmword ptr [RCX + 0448], ZMM7

    DB  $62, $71, $7C, $48, $11, $41, $08 // VMOVUPS zmmword ptr [RCX + 0512], ZMM8
    DB  $62, $71, $7C, $48, $11, $49, $09 // VMOVUPS zmmword ptr [RCX + 0576], ZMM9
    DB  $62, $71, $7C, $48, $11, $51, $0A // VMOVUPS zmmword ptr [RCX + 0640], ZMM10
    DB  $62, $71, $7C, $48, $11, $59, $0B // VMOVUPS zmmword ptr [RCX + 0704], ZMM11
    DB  $62, $71, $7C, $48, $11, $61, $0C // VMOVUPS zmmword ptr [RCX + 0768], ZMM12
    DB  $62, $71, $7C, $48, $11, $69, $0D // VMOVUPS zmmword ptr [RCX + 0832], ZMM13
    DB  $62, $71, $7C, $48, $11, $71, $0E // VMOVUPS zmmword ptr [RCX + 0896], ZMM14
    DB  $62, $71, $7C, $48, $11, $79, $0F // VMOVUPS zmmword ptr [RCX + 0960], ZMM15

    DB  $62, $E1, $7C, $48, $11, $41, $10 // VMOVUPS zmmword ptr [RCX + 1024], ZMM16
    DB  $62, $E1, $7C, $48, $11, $49, $11 // VMOVUPS zmmword ptr [RCX + 1088], ZMM17
    DB  $62, $E1, $7C, $48, $11, $51, $12 // VMOVUPS zmmword ptr [RCX + 1152], ZMM18
    DB  $62, $E1, $7C, $48, $11, $59, $13 // VMOVUPS zmmword ptr [RCX + 1216], ZMM19
    DB  $62, $E1, $7C, $48, $11, $61, $14 // VMOVUPS zmmword ptr [RCX + 1280], ZMM20
    DB  $62, $E1, $7C, $48, $11, $69, $15 // VMOVUPS zmmword ptr [RCX + 1344], ZMM21
    DB  $62, $E1, $7C, $48, $11, $71, $16 // VMOVUPS zmmword ptr [RCX + 1408], ZMM22
    DB  $62, $E1, $7C, $48, $11, $79, $17 // VMOVUPS zmmword ptr [RCX + 1472], ZMM23

    DB  $62, $61, $7C, $48, $11, $41, $18 // VMOVUPS zmmword ptr [RCX + 1536], ZMM24
    DB  $62, $61, $7C, $48, $11, $49, $19 // VMOVUPS zmmword ptr [RCX + 1600], ZMM25
    DB  $62, $61, $7C, $48, $11, $51, $1A // VMOVUPS zmmword ptr [RCX + 1664], ZMM26
    DB  $62, $61, $7C, $48, $11, $59, $1B // VMOVUPS zmmword ptr [RCX + 1728], ZMM27
    DB  $62, $61, $7C, $48, $11, $61, $1C // VMOVUPS zmmword ptr [RCX + 1792], ZMM28
    DB  $62, $61, $7C, $48, $11, $69, $1D // VMOVUPS zmmword ptr [RCX + 1856], ZMM29
    DB  $62, $61, $7C, $48, $11, $71, $1E // VMOVUPS zmmword ptr [RCX + 1920], ZMM30
    DB  $62, $61, $7C, $48, $11, $79, $1F // VMOVUPS zmmword ptr [RCX + 1984], ZMM31
  {$ELSE}
    DB  $62, $F1, $7C, $48, $11, $07      // VMOVUPS zmmword ptr [RDI + 0000], ZMM0
    DB  $62, $F1, $7C, $48, $11, $4F, $01 // VMOVUPS zmmword ptr [RDI + 0064], ZMM1
    DB  $62, $F1, $7C, $48, $11, $57, $02 // VMOVUPS zmmword ptr [RDI + 0128], ZMM2
    DB  $62, $F1, $7C, $48, $11, $5F, $03 // VMOVUPS zmmword ptr [RDI + 0192], ZMM3
    DB  $62, $F1, $7C, $48, $11, $67, $04 // VMOVUPS zmmword ptr [RDI + 0256], ZMM4
    DB  $62, $F1, $7C, $48, $11, $6F, $05 // VMOVUPS zmmword ptr [RDI + 0320], ZMM5
    DB  $62, $F1, $7C, $48, $11, $77, $06 // VMOVUPS zmmword ptr [RDI + 0384], ZMM6
    DB  $62, $F1, $7C, $48, $11, $7F, $07 // VMOVUPS zmmword ptr [RDI + 0448], ZMM7

    DB  $62, $71, $7C, $48, $11, $47, $08 // VMOVUPS zmmword ptr [RDI + 0512], ZMM8
    DB  $62, $71, $7C, $48, $11, $4F, $09 // VMOVUPS zmmword ptr [RDI + 0576], ZMM9
    DB  $62, $71, $7C, $48, $11, $57, $0A // VMOVUPS zmmword ptr [RDI + 0640], ZMM10
    DB  $62, $71, $7C, $48, $11, $5F, $0B // VMOVUPS zmmword ptr [RDI + 0704], ZMM11
    DB  $62, $71, $7C, $48, $11, $67, $0C // VMOVUPS zmmword ptr [RDI + 0768], ZMM12
    DB  $62, $71, $7C, $48, $11, $6F, $0D // VMOVUPS zmmword ptr [RDI + 0832], ZMM13
    DB  $62, $71, $7C, $48, $11, $77, $0E // VMOVUPS zmmword ptr [RDI + 0896], ZMM14
    DB  $62, $71, $7C, $48, $11, $7F, $0F // VMOVUPS zmmword ptr [RDI + 0960], ZMM15

    DB  $62, $E1, $7C, $48, $11, $47, $10 // VMOVUPS zmmword ptr [RDI + 1024], ZMM16
    DB  $62, $E1, $7C, $48, $11, $4F, $11 // VMOVUPS zmmword ptr [RDI + 1088], ZMM17
    DB  $62, $E1, $7C, $48, $11, $57, $12 // VMOVUPS zmmword ptr [RDI + 1152], ZMM18
    DB  $62, $E1, $7C, $48, $11, $5F, $13 // VMOVUPS zmmword ptr [RDI + 1216], ZMM19
    DB  $62, $E1, $7C, $48, $11, $67, $14 // VMOVUPS zmmword ptr [RDI + 1280], ZMM20
    DB  $62, $E1, $7C, $48, $11, $6F, $15 // VMOVUPS zmmword ptr [RDI + 1344], ZMM21
    DB  $62, $E1, $7C, $48, $11, $77, $16 // VMOVUPS zmmword ptr [RDI + 1408], ZMM22
    DB  $62, $E1, $7C, $48, $11, $7F, $17 // VMOVUPS zmmword ptr [RDI + 1472], ZMM23

    DB  $62, $61, $7C, $48, $11, $47, $18 // VMOVUPS zmmword ptr [RDI + 1536], ZMM24
    DB  $62, $61, $7C, $48, $11, $4F, $19 // VMOVUPS zmmword ptr [RDI + 1600], ZMM25
    DB  $62, $61, $7C, $48, $11, $57, $1A // VMOVUPS zmmword ptr [RDI + 1664], ZMM26
    DB  $62, $61, $7C, $48, $11, $5F, $1B // VMOVUPS zmmword ptr [RDI + 1728], ZMM27
    DB  $62, $61, $7C, $48, $11, $67, $1C // VMOVUPS zmmword ptr [RDI + 1792], ZMM28
    DB  $62, $61, $7C, $48, $11, $6F, $1D // VMOVUPS zmmword ptr [RDI + 1856], ZMM29
    DB  $62, $61, $7C, $48, $11, $77, $1E // VMOVUPS zmmword ptr [RDI + 1920], ZMM30
    DB  $62, $61, $7C, $48, $11, $7F, $1F // VMOVUPS zmmword ptr [RDI + 1984], ZMM31
  {$ENDIF}
{$ELSE}
    DB  $62, $F1, $7C, $48, $11, $00      // VMOVUPS zmmword ptr [EAX + 0000], ZMM0
    DB  $62, $F1, $7C, $48, $11, $48, $01 // VMOVUPS zmmword ptr [EAX + 0064], ZMM1
    DB  $62, $F1, $7C, $48, $11, $50, $02 // VMOVUPS zmmword ptr [EAX + 0128], ZMM2
    DB  $62, $F1, $7C, $48, $11, $58, $03 // VMOVUPS zmmword ptr [EAX + 0192], ZMM3
    DB  $62, $F1, $7C, $48, $11, $60, $04 // VMOVUPS zmmword ptr [EAX + 0256], ZMM4
    DB  $62, $F1, $7C, $48, $11, $68, $05 // VMOVUPS zmmword ptr [EAX + 0320], ZMM5
    DB  $62, $F1, $7C, $48, $11, $70, $06 // VMOVUPS zmmword ptr [EAX + 0384], ZMM6
    DB  $62, $F1, $7C, $48, $11, $78, $07 // VMOVUPS zmmword ptr [EAX + 0448], ZMM7
{$ENDIF}
end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

procedure VECFloatDataGet_ASM(out FloatData: TVectorFloatData); register;
begin
FillChar(Addr(FloatData)^,SizeOf(FloatData),0);
with TSimpleCPUID.Create do
try
{
  Get number of registers and their width based on supported extensions and
  then load them accordingly. Select largest supported extension first.
}
  If Info.SupportedExtensions.AVX512.Supported or Info.SupportedExtensions.AVX10.Vec512 then
    begin
      FloatData.RegisterCount := {$IFDEF x64}32{$ELSE}8{$ENDIF};
      FloatData.RegisterWidth := 512;
      VECFloatDataGet_AVX512(@FloatData.Registers);
    end
  else If Info.SupportedExtensions.AVX or Info.SupportedExtensions.AVX10.Vec256 then
    begin
      FloatData.RegisterCount := {$IFDEF x64}16{$ELSE}8{$ENDIF};
      FloatData.RegisterWidth := 256;
      VECFloatDataGet_AVX(@FloatData.Registers);
    end
  else If Info.SupportedExtensions.SSE or Info.SupportedExtensions.AVX10.Vec128 then
    begin
      FloatData.RegisterCount := {$IFDEF x64}16{$ELSE}8{$ENDIF};
      FloatData.RegisterWidth := 128;
      VECFloatDataGet_SSE(@FloatData.Registers);
    end;
  FloatData.RegisterSize := FloatData.RegisterWidth div 8;
finally
  Free;
end;
end;

{$ENDIF}
//==============================================================================

Function VECControlAndStatusGet_PAS: UInt32; register;
begin
{$IFDEF FPC}
Result := 0;
{$ENDIF}
raise EFUUnsupportedOp.Create('VECControlAndStatusGet_PAS: Vector unit operation not supported.');
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure VECControlAndStatusSet_PAS(NewValue: UInt32); register;
begin
raise EFUUnsupportedOp.Create('VECControlAndStatusSet_PAS: Vector unit operation not supported.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure VECFloatDataGet_PAS(out FloatData: TVectorFloatData); register;
begin
raise EFUUnsupportedOp.Create('VECFloatDataGet_PAS: Vector unit operation not supported.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================
var
  VAR_VECControlAndStatusGet: Function: UInt32; register = VECControlAndStatusGet_PAS;
  VAR_VECControlAndStatusSet: procedure(NewValue: UInt32); register = VECControlAndStatusSet_PAS;
  VAR_VECFloatDataGet: procedure(out FloatData: TVectorFloatData); register = VECFloatDataGet_PAS;

//==============================================================================

Function SSEControlAndStatusGet: UInt32;
begin
Result := VAR_VECControlAndStatusGet;
end;

//------------------------------------------------------------------------------

Function AVXControlAndStatusGet: UInt32;
begin
Result := VAR_VECControlAndStatusGet;
end;

//------------------------------------------------------------------------------

procedure SSEControlAndStatusSet(NewValue: UInt32);
begin
VAR_VECControlAndStatusSet(NewValue and VAR_VECControlAndStatusMask);
end;

//------------------------------------------------------------------------------

procedure AVXControlAndStatusSet(NewValue: UInt32);
begin
VAR_VECControlAndStatusSet(NewValue and VAR_VECControlAndStatusMask);
end;

//------------------------------------------------------------------------------

Function SSEControlAndStatusMaskGet: UInt32;
begin
Result := VAR_VECControlAndStatusMask;
end;

//------------------------------------------------------------------------------

Function AVXControlAndStatusMaskGet: UInt32;
begin
Result := VAR_VECControlAndStatusMask;
end;

//------------------------------------------------------------------------------

Function SSEControlAndStatusSupportsDAZ: Boolean;
begin
Result := (VAR_VECControlAndStatusMask and MXCSR_DenormalsAreZeros) <> 0;
end;

//------------------------------------------------------------------------------

Function AVXControlAndStatusSupportsDAZ: Boolean;
begin
Result := (VAR_VECControlAndStatusMask and MXCSR_DenormalsAreZeros) <> 0;
end;

//------------------------------------------------------------------------------

procedure SSEEnvironmentInit;
begin
SSEControlAndStatusSet(MXCSR_InitialValue);
end;

//------------------------------------------------------------------------------

procedure AVXEnvironmentInit;
begin
AVXControlAndStatusSet(MXCSR_InitialValue);
end;

//------------------------------------------------------------------------------

procedure SSEControlAndStatusInit;
begin
SSEControlAndStatusSet(MXCSR_DefaultValue);
end;

//------------------------------------------------------------------------------

procedure AVXControlAndStatusInit;
begin
AVXControlAndStatusSet(MXCSR_DefaultValue);
end;

//------------------------------------------------------------------------------

procedure SSEFloatDataGet(out FloatData: TVectorFloatData);
begin
VAR_VECFloatDataGet(FloatData);
end;

//------------------------------------------------------------------------------

procedure AVXFloatDataGet(out FloatData: TVectorFloatData);
begin
VAR_VECFloatDataGet(FloatData);
end;

{===============================================================================
    SSE/AVX CS management - abstracted access internals
===============================================================================}

Function MXCSRRoundingModeGet(MXCSR: UInt32): TSSERoundingMode;
begin
case (MXCSR and MXCSR_Rounding) shr MXCSR_SHIFT_Rounding of
  1:  Result := rmDown;
  2:  Result := rmUp;
  3:  Result := rmTruncate;
else
  Result := rmNearest;
end;
end;

//------------------------------------------------------------------------------

Function MXCSRRoundingModeSet(var MXCSR: UInt32; NewValue: TSSERoundingMode): TSSERoundingMode;
begin
Result := MXCSRRoundingModeGet(MXCSR);
MXCSR := MXCSR and not MXCSR_Rounding;
case NewValue of
  rmDown:     MXCSR := MXCSR or UInt32(1 shl MXCSR_SHIFT_Rounding);
  rmUp:       MXCSR := MXCSR or UInt32(2 shl MXCSR_SHIFT_Rounding);
  rmTruncate: MXCSR := MXCSR or UInt32(3 shl MXCSR_SHIFT_Rounding);
end;
end;

//------------------------------------------------------------------------------

Function MXCSRControlFlagGet(MXCSR: UInt32; Flag: TSSEControlFlag): Boolean;
begin
case Flag of
  cfDenormalsAreZeros:  Result := (MXCSR and MXCSR_DenormalsAreZeros) <> 0;
  cfFlushToZero:        Result := (MXCSR and MXCSR_FlushToZero) <> 0;
else
  raise EFUInvalidFlag.CreateFmt('MXCSRControlFlagGet: Invalid control flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function MXCSRControlFlagSet(var MXCSR: UInt32; Flag: TSSEControlFlag; NewValue: Boolean): Boolean;

  procedure SetBit(BitMask: UInt32);
  begin
    If NewValue then
      MXCSR := MXCSR or BitMask
    else
      MXCSR := MXCSR and not BitMask;
  end;

begin
Result := MXCSRControlFlagGet(MXCSR,Flag);
case Flag of
  cfDenormalsAreZeros:  SetBit(MXCSR_DenormalsAreZeros);
  cfFlushToZero:        SetBit(MXCSR_FlushToZero);
else
  raise EFUInvalidFlag.CreateFmt('MXCSRControlFlagSet: Invalid control flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function MXCSRControlFlagsGet(MXCSR: UInt32): TSSEControlFlags;
begin
Result := [];
If (MXCSR and MXCSR_DenormalsAreZeros) <> 0 then
  Include(Result,cfDenormalsAreZeros);
If (MXCSR and MXCSR_FlushToZero) <> 0 then
  Include(Result,cfFlushToZero);
end;

//------------------------------------------------------------------------------

Function MXCSRControlFlagsSet(var MXCSR: UInt32; NewValue: TSSEControlFlags): TSSEControlFlags;

  procedure SetBit(BitMask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or BitMask
    else
      MXCSR := MXCSR and not BitMask;
  end;

begin
Result := MXCSRControlFlagsGet(MXCSR);
SetBit(MXCSR_DenormalsAreZeros,cfDenormalsAreZeros in NewValue);
SetBit(MXCSR_FlushToZero,cfFlushToZero in NewValue);
end;

//==============================================================================

Function MXCSRExceptionMaskGet(MXCSR: UInt32; Exception: TSSEException): Boolean;
begin
case Exception of
  excInvalidOp: Result := (MXCSR and MXCSR_EMASK_InvalidOP) <> 0;
  excDenormal:  Result := (MXCSR and MXCSR_EMASK_Denormal) <> 0;
  excDivByZero: Result := (MXCSR and MXCSR_EMASK_DivByZero) <> 0;
  excOverflow:  Result := (MXCSR and MXCSR_EMASK_Overflow) <> 0;
  excUnderflow: Result := (MXCSR and MXCSR_EMASK_Underflow) <> 0;
  excPrecision: Result := (MXCSR and MXCSR_EMASK_Precision) <> 0;
else
  raise EFUInvalidFlag.CreateFmt('MXCSRExceptionMaskGet: Invalid vector exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function MXCSRExceptionMaskSet(var MXCSR: UInt32; Exception: TSSEException; NewValue: Boolean): Boolean;

  procedure SetBit(BitMask: UInt32);
  begin
    If NewValue then
      MXCSR := MXCSR or BitMask
    else
      MXCSR := MXCSR and not BitMask;
  end;

begin
Result := MXCSRExceptionMaskGet(MXCSR,Exception);
case Exception of
  excInvalidOp: SetBit(MXCSR_EMASK_InvalidOP);
  excDenormal:  SetBit(MXCSR_EMASK_Denormal);
  excDivByZero: SetBit(MXCSR_EMASK_DivByZero);
  excOverflow:  SetBit(MXCSR_EMASK_Overflow);
  excUnderflow: SetBit(MXCSR_EMASK_Underflow);
  excPrecision: SetBit(MXCSR_EMASK_Precision);
else
  raise EFUInvalidFlag.CreateFmt('MXCSRExceptionMaskSet: Invalid vector exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function MXCSRExceptionMasksGet(MXCSR: UInt32): TSSEExceptions;
begin
Result := [];
If (MXCSR and MXCSR_EMASK_InvalidOP) <> 0 then
  Include(Result,excInvalidOp);
If (MXCSR and MXCSR_EMASK_Denormal) <> 0 then
  Include(Result,excDenormal);
If (MXCSR and MXCSR_EMASK_DivByZero) <> 0 then
  Include(Result,excDivByZero);
If (MXCSR and MXCSR_EMASK_Overflow) <> 0 then
  Include(Result,excOverflow);
If (MXCSR and MXCSR_EMASK_Underflow) <> 0 then
  Include(Result,excUnderflow);
If (MXCSR and MXCSR_EMASK_Precision) <> 0 then
  Include(Result,excPrecision);
end;

//------------------------------------------------------------------------------

Function MXCSRExceptionMasksSet(var MXCSR: UInt32; NewValue: TSSEExceptions): TSSEExceptions;

  procedure SetBit(BitMask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or BitMask
    else
      MXCSR := MXCSR and not BitMask;
  end;

begin
Result := MXCSRExceptionMasksGet(MXCSR);
SetBit(MXCSR_EMASK_InvalidOP,excInvalidOp in NewValue);
SetBit(MXCSR_EMASK_Denormal,excDenormal in NewValue);
SetBit(MXCSR_EMASK_DivByZero,excDivByZero in NewValue);
SetBit(MXCSR_EMASK_Overflow,excOverflow in NewValue);
SetBit(MXCSR_EMASK_Underflow,excUnderflow in NewValue);
SetBit(MXCSR_EMASK_Precision,excPrecision in NewValue);
end;

//------------------------------------------------------------------------------

Function MXCSRExceptionFlagGet(MXCSR: UInt32; Exception: TSSEException): Boolean;
begin
case Exception of
  excInvalidOp: Result := (MXCSR and MXCSR_EFLAG_InvalidOP) <> 0;
  excDenormal:  Result := (MXCSR and MXCSR_EFLAG_Denormal) <> 0;
  excDivByZero: Result := (MXCSR and MXCSR_EFLAG_DivByZero) <> 0;
  excOverflow:  Result := (MXCSR and MXCSR_EFLAG_Overflow) <> 0;
  excUnderflow: Result := (MXCSR and MXCSR_EFLAG_Underflow) <> 0;
  excPrecision: Result := (MXCSR and MXCSR_EFLAG_Precision) <> 0;
else
  raise EFUInvalidFlag.CreateFmt('MXCSRExceptionFlagGet: Invalid vector exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function MXCSRExceptionFlagSet(var MXCSR: UInt32; Exception: TSSEException; NewValue: Boolean): Boolean;

  procedure SetBit(BitMask: UInt32);
  begin
    If NewValue then
      MXCSR := MXCSR or BitMask
    else
      MXCSR := MXCSR and not BitMask;
  end;

begin
Result := MXCSRExceptionMaskGet(MXCSR,Exception);
case Exception of
  excInvalidOp: SetBit(MXCSR_EFLAG_InvalidOP);
  excDenormal:  SetBit(MXCSR_EFLAG_Denormal);
  excDivByZero: SetBit(MXCSR_EFLAG_DivByZero);
  excOverflow:  SetBit(MXCSR_EFLAG_Overflow);
  excUnderflow: SetBit(MXCSR_EFLAG_Underflow);
  excPrecision: SetBit(MXCSR_EFLAG_Precision);
else
  raise EFUInvalidFlag.CreateFmt('MXCSRExceptionFlagSet: Invalid vector exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function MXCSRExceptionFlagsGet(MXCSR: UInt32): TSSEExceptions;
begin
Result := [];
If (MXCSR and MXCSR_EFLAG_InvalidOP) <> 0 then
  Include(Result,excInvalidOp);
If (MXCSR and MXCSR_EFLAG_Denormal) <> 0 then
  Include(Result,excDenormal);
If (MXCSR and MXCSR_EFLAG_DivByZero) <> 0 then
  Include(Result,excDivByZero);
If (MXCSR and MXCSR_EFLAG_Overflow) <> 0 then
  Include(Result,excOverflow);
If (MXCSR and MXCSR_EFLAG_Underflow) <> 0 then
  Include(Result,excUnderflow);
If (MXCSR and MXCSR_EFLAG_Precision) <> 0 then
  Include(Result,excPrecision);
end;

//------------------------------------------------------------------------------

Function MXCSRExceptionFlagsSet(var MXCSR: UInt32; NewValue: TSSEExceptions): TSSEExceptions;

  procedure SetBit(BitMask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or BitMask
    else
      MXCSR := MXCSR and not BitMask;
  end;

begin
Result := MXCSRExceptionFlagsGet(MXCSR);
SetBit(MXCSR_EFLAG_InvalidOP,excInvalidOp in NewValue);
SetBit(MXCSR_EFLAG_Denormal,excDenormal in NewValue);
SetBit(MXCSR_EFLAG_DivByZero,excDivByZero in NewValue);
SetBit(MXCSR_EFLAG_Overflow,excOverflow in NewValue);
SetBit(MXCSR_EFLAG_Underflow,excUnderflow in NewValue);
SetBit(MXCSR_EFLAG_Precision,excPrecision in NewValue);
end;

{===============================================================================
    SSE/AVX CS management - abstracted access implementation
===============================================================================}

Function SSERoundingModeGet: TSSERoundingMode;
begin
Result := MXCSRRoundingModeGet(SSEControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function AVXRoundingModeGet: TAVXRoundingMode;
begin
Result := MXCSRRoundingModeGet(AVXControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function SSERoundingModeSet(NewValue: TSSERoundingMode): TSSERoundingMode;
var
  MXCSR:  UInt32;
begin
MXCSR := SSEControlAndStatusGet;
Result := MXCSRRoundingModeSet(MXCSR,NewValue);
SSEControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function AVXRoundingModeSet(NewValue: TAVXRoundingMode): TAVXRoundingMode;
var
  MXCSR:  UInt32;
begin
MXCSR := AVXControlAndStatusGet;
Result := MXCSRRoundingModeSet(MXCSR,NewValue);
AVXControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function SSEControlFlagGet(Flag: TSSEControlFlag): Boolean;
begin
Result := MXCSRControlFlagGet(SSEControlAndStatusGet,Flag);
end;

//------------------------------------------------------------------------------

Function AVXControlFlagGet(Flag: TAVXControlFlag): Boolean;
begin
Result := MXCSRControlFlagGet(AVXControlAndStatusGet,Flag);
end;

//------------------------------------------------------------------------------

Function SSEControlFlagSet(Flag: TSSEControlFlag; NewValue: Boolean): Boolean;
var
  MXCSR:  UInt32;
begin
MXCSR := SSEControlAndStatusGet;
Result := MXCSRControlFlagSet(MXCSR,Flag,NewValue);
SSEControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function AVXControlFlagSet(Flag: TAVXControlFlag; NewValue: Boolean): Boolean;
var
  MXCSR:  UInt32;
begin
MXCSR := AVXControlAndStatusGet;
Result := MXCSRControlFlagSet(MXCSR,Flag,NewValue);
AVXControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function SSEControlFlagsGet: TSSEControlFlags;
begin
Result := MXCSRControlFlagsGet(SSEControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function AVXControlFlagsGet: TAVXControlFlags;
begin
Result := MXCSRControlFlagsGet(AVXControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function SSEControlFlagsSet(NewValue: TSSEControlFlags): TSSEControlFlags;
var
  MXCSR:  UInt32;
begin
MXCSR := SSEControlAndStatusGet;
Result := MXCSRControlFlagsSet(MXCSR,NewValue);
SSEControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function AVXControlFlagsSet(NewValue: TAVXControlFlags): TAVXControlFlags;
var
  MXCSR:  UInt32;
begin
MXCSR := AVXControlAndStatusGet;
Result := MXCSRControlFlagsSet(MXCSR,NewValue);
AVXControlAndStatusSet(MXCSR);
end;

//==============================================================================

Function SSEExceptionMaskGet(Exception: TSSEException): Boolean;
begin
Result := MXCSRExceptionMaskGet(SSEControlAndStatusGet,Exception);
end;

//------------------------------------------------------------------------------

Function AVXExceptionMaskGet(Exception: TAVXException): Boolean;
begin
Result := MXCSRExceptionMaskGet(AVXControlAndStatusGet,Exception);
end;

//------------------------------------------------------------------------------

Function SSEExceptionMaskSet(Exception: TSSEException; NewValue: Boolean): Boolean;
var
  MXCSR:  UInt32;
begin
MXCSR := SSEControlAndStatusGet;
Result := MXCSRExceptionMaskSet(MXCSR,Exception,NewValue);
SSEControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function AVXExceptionMaskSet(Exception: TAVXException; NewValue: Boolean): Boolean;
var
  MXCSR:  UInt32;
begin
MXCSR := AVXControlAndStatusGet;
Result := MXCSRExceptionMaskSet(MXCSR,Exception,NewValue);
AVXControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function SSEExceptionMasksGet: TSSEExceptions;
begin
Result := MXCSRExceptionMasksGet(SSEControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function AVXExceptionMasksGet: TAVXExceptions;
begin
Result := MXCSRExceptionMasksGet(AVXControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function SSEExceptionMasksSet(NewValue: TSSEExceptions): TSSEExceptions;
var
  MXCSR:  UInt32;
begin
MXCSR := SSEControlAndStatusGet;
Result := MXCSRExceptionMasksSet(MXCSR,NewValue);
SSEControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function AVXExceptionMasksSet(NewValue: TAVXExceptions): TAVXExceptions;
var
  MXCSR:  UInt32;
begin
MXCSR := AVXControlAndStatusGet;
Result := MXCSRExceptionMasksSet(MXCSR,NewValue);
AVXControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function SSEExceptionFlagGet(Exception: TSSEException): Boolean;
begin
Result := MXCSRExceptionFlagGet(SSEControlAndStatusGet,Exception);
end;

//------------------------------------------------------------------------------

Function AVXExceptionFlagGet(Exception: TAVXException): Boolean;
begin
Result := MXCSRExceptionFlagGet(AVXControlAndStatusGet,Exception);
end;

//------------------------------------------------------------------------------

Function SSEExceptionFlagSet(Exception: TSSEException; NewValue: Boolean): Boolean;
var
  MXCSR:  UInt32;
begin
MXCSR := SSEControlAndStatusGet;
Result := MXCSRExceptionFlagSet(MXCSR,Exception,NewValue);
SSEControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function AVXExceptionFlagSet(Exception: TAVXException; NewValue: Boolean): Boolean;
var
  MXCSR:  UInt32;
begin
MXCSR := AVXControlAndStatusGet;
Result := MXCSRExceptionFlagSet(MXCSR,Exception,NewValue);
AVXControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function SSEExceptionFlagsGet: TSSEExceptions;
begin
Result := MXCSRExceptionFlagsGet(SSEControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function AVXExceptionFlagsGet: TAVXExceptions;
begin
Result := MXCSRExceptionFlagsGet(AVXControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function SSEExceptionFlagsSet(NewValue: TSSEExceptions): TSSEExceptions;
var
  MXCSR:  UInt32;
begin
MXCSR := SSEControlAndStatusGet;
Result := MXCSRExceptionFlagsSet(MXCSR,NewValue);
SSEControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function AVXExceptionFlagsSet(NewValue: TAVXExceptions): TAVXExceptions;
var
  MXCSR:  UInt32;
begin
MXCSR := AVXControlAndStatusGet;
Result := MXCSRExceptionFlagsSet(MXCSR,NewValue);
AVXControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

procedure SSEExceptionsClear;
begin
SSEControlAndStatusSet(SSEControlAndStatusGet and not MXCSR_EFLAG_All);
end;

//------------------------------------------------------------------------------

procedure AVXExceptionsClear;
begin
AVXControlAndStatusSet(AVXControlAndStatusGet and not MXCSR_EFLAG_All);
end;


{===============================================================================
--------------------------------------------------------------------------------
                     F16 conversion (F16C) state management
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    F16C state management - emulated state
===============================================================================}
var
  VAR_F16CEmulated: Boolean = True;

type
  TF16CState = record
    Initialized:      Boolean;
    ControlAndStatus: UInt32;
  end;
  PF16CState = ^TF16CState;

threadvar
  THRVAR_F16CState: TF16CState;

//------------------------------------------------------------------------------

Function F16CGetStatePtr: PF16CState;
begin
Result := @THRVAR_F16CState;
If not Result^.Initialized then
  begin
    Result^.Initialized := True;
    Result^.ControlAndStatus := MXCSR_DefaultValue;
  end;
end;

{===============================================================================
    F16C state management - exceptions implementation
===============================================================================}
{-------------------------------------------------------------------------------
    F16C state management - EF16CException class implementation
-------------------------------------------------------------------------------}

procedure EF16CException.Initialize;
begin
fControlAndStatus := F16CControlAndStatusGet;
fPendingExcs := MXCSRExceptionFlagsGet(fControlAndStatus);
fMaskedExcs := fPendingExcs * MXCSRExceptionMasksGet(fControlAndStatus);
// raise only unmasked exceptions
fRaisedExcs := fPendingExcs - MXCSRExceptionMasksGet(fControlAndStatus);
// clear all pending exceptions and set control and status to default value
F16CEnvironmentInit;
F16CControlAndStatusInit;
end;

{-------------------------------------------------------------------------------
    F16C state management - exception selection and raise
-------------------------------------------------------------------------------}

procedure F16CSelectAndRaiseException;
var
  ControlAndStatus: UInt32;
  Unmasked:         TSSEExceptions;
begin
{
  Priority is the same as for x87 exceptions:

    - invalid operation (unsupported formats, SNaN)
    - QNaN (not an exception)
    - other inv-op, division by zero
    - denormal operand
    - numeric underflow and overflow, possibly with inexact result
    - inexact result
}
ControlAndStatus := F16CControlAndStatusGet;
Unmasked := MXCSRExceptionFlagsGet(ControlAndStatus) -
            MXCSRExceptionMasksGet(ControlAndStatus);
If Unmasked <> [] then
  begin
    If excInvalidOp in Unmasked then
      raise EF16CInvalidOp.Create;
    If excDivByZero in Unmasked then
      raise EF16CDivByZero.Create;
    If excDenormal in Unmasked then
      raise EF16CDenormal.Create;
    If excOverflow in Unmasked then
      raise EF16COverflow.Create;
    If excUnderflow in Unmasked then
      raise EF16CUnderflow.Create;
    If excPrecision in Unmasked then
      raise EF16CPrecision.Create;
  end;
end;

{===============================================================================
    F16C state management - low-lewel access implementation
===============================================================================}

Function F16CEmulated: Boolean;
begin
Result := VAR_F16CEmulated;
end;

//------------------------------------------------------------------------------

Function F16CControlAndStatusGet: UInt32;
begin
If VAR_F16CEmulated then
  Result := F16CGetStatePtr^.ControlAndStatus
else
  Result := SSEControlAndStatusGet;
end;

//------------------------------------------------------------------------------

procedure F16CControlAndStatusSet(NewValue: UInt32);
begin
If VAR_F16CEmulated then
  F16CGetStatePtr^.ControlAndStatus := NewValue
else
  SSEControlAndStatusSet(NewValue);
end;

//------------------------------------------------------------------------------

procedure F16CEnvironmentInit;
begin
If VAR_F16CEmulated then
  F16CGetStatePtr^.ControlAndStatus := MXCSR_InitialValue
else
  SSEEnvironmentInit;
end;

//------------------------------------------------------------------------------

procedure F16CControlAndStatusInit;
begin
If VAR_F16CEmulated then
  F16CGetStatePtr^.ControlAndStatus := MXCSR_DefaultValue
else
  SSEControlAndStatusInit;
end;

{===============================================================================
    F16C state management - abstracted access implementation
===============================================================================}
{-------------------------------------------------------------------------------
    F16C state management - flags and rounding abstracted access
-------------------------------------------------------------------------------}

Function F16CRoundingModeGet: TF16CRoundingMode;
begin
Result := MXCSRRoundingModeGet(F16CControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function F16CRoundingModeSet(NewValue: TF16CRoundingMode): TF16CRoundingMode;
var
  MXCSR:  UInt32;
begin
MXCSR := F16CControlAndStatusGet;
Result := MXCSRRoundingModeSet(MXCSR,NewValue);
F16CControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function F16CControlFlagGet(Flag: TF16CControlFlag): Boolean;
begin
Result := MXCSRControlFlagGet(F16CControlAndStatusGet,Flag);
end;

//------------------------------------------------------------------------------

Function F16CControlFlagSet(Flag: TF16CControlFlag; NewValue: Boolean): Boolean;
var
  MXCSR:  UInt32;
begin
MXCSR := F16CControlAndStatusGet;
Result := MXCSRControlFlagSet(MXCSR,Flag,NewValue);
F16CControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function F16CControlFlagsGet: TF16CControlFlags;
begin
Result := MXCSRControlFlagsGet(F16CControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function F16CControlFlagsSet(NewValue: TF16CControlFlags): TF16CControlFlags;
var
  MXCSR:  UInt32;
begin
MXCSR := F16CControlAndStatusGet;
Result := MXCSRControlFlagsSet(MXCSR,NewValue);
F16CControlAndStatusSet(MXCSR);
end;

{-------------------------------------------------------------------------------
    F16C state management - exceptions abstraction
-------------------------------------------------------------------------------}

Function F16CExceptionMaskGet(Exception: TF16CException): Boolean;
begin
Result := MXCSRExceptionMaskGet(F16CControlAndStatusGet,Exception);
end;

//------------------------------------------------------------------------------

Function F16CExceptionMaskSet(Exception: TF16CException; NewValue: Boolean): Boolean;
var
  MXCSR:  UInt32;
begin
MXCSR := F16CControlAndStatusGet;
Result := MXCSRExceptionMaskSet(MXCSR,Exception,NewValue);
F16CControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function F16CExceptionMasksGet: TF16CExceptions;
begin
Result := MXCSRExceptionMasksGet(F16CControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function F16CExceptionMasksSet(NewValue: TF16CExceptions): TF16CExceptions;
var
  MXCSR:  UInt32;
begin
MXCSR := F16CControlAndStatusGet;
Result := MXCSRExceptionMasksSet(MXCSR,NewValue);
F16CControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function F16CExceptionFlagGet(Exception: TF16CException): Boolean;
begin
Result := MXCSRExceptionFlagGet(F16CControlAndStatusGet,Exception);
end;

//------------------------------------------------------------------------------

Function F16CExceptionFlagSet(Exception: TF16CException; NewValue: Boolean): Boolean;
var
  MXCSR:  UInt32;
begin
MXCSR := F16CControlAndStatusGet;
Result := MXCSRExceptionFlagSet(MXCSR,Exception,NewValue);
F16CControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

Function F16CExceptionFlagsGet: TF16CExceptions;
begin
Result := MXCSRExceptionFlagsGet(F16CControlAndStatusGet);
end;

//------------------------------------------------------------------------------

Function F16CExceptionFlagsSet(NewValue: TF16CExceptions): TF16CExceptions;
var
  MXCSR:  UInt32;
begin
MXCSR := F16CControlAndStatusGet;
Result := MXCSRExceptionFlagsSet(MXCSR,NewValue);
F16CControlAndStatusSet(MXCSR);
end;

//------------------------------------------------------------------------------

procedure F16CExceptionsClear;
var
  StatePtr: PF16CState;
begin
If VAR_F16CEmulated then
  begin
    StatePtr := F16CGetStatePtr;
    StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus and not MXCSR_EFLAG_All
  end
else SSEExceptionsClear;
end;


{===============================================================================
--------------------------------------------------------------------------------
                     Float16 <-> Float32 conversions (F16C)
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    F16C - axiliary functions
===============================================================================}

procedure F16CSignalExceptions(Exceptions: TF16CRaiseExceptions);
var
  StatePtr: PF16CState;
begin
StatePtr := F16CGetStatePtr;
// exception flags are sticky, do NOT clear those that are not signaled
If excInvalidOp in Exceptions then
  StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_InvalidOp;
If excDenormal in Exceptions then
  StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_Denormal;
If excDivByZero in Exceptions then
  StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_DivByZero;
If excOverflow in Exceptions then
  StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_Overflow;
If excUnderflow in Exceptions then
  StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_Underflow;
If excPrecision in Exceptions then
  StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_Precision;
F16CSelectAndRaiseException;
end;

//------------------------------------------------------------------------------

procedure F16CSignalException(Exception: TF16CRaiseException);
begin
F16CSignalExceptions([Exception]);
end;

{===============================================================================
    F16C - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    F16C - implementation internals
-------------------------------------------------------------------------------}
type
  TFUVec4H = packed array[0..3] of Float16;
  TFUVec4S = packed array[0..3] of Float32;

//------------------------------------------------------------------------------
{$IFNDEF PurePascal}

procedure Float16ToFloat32_ASM(F16Ptr,F32Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F16Ptr              EAX               RCX               RDI
     F32Ptr              EDX               RDX               RSI
-------------------------------------------------------------------------------}
{
  Following contrived loading is here to ensure only 16 bits of memory will
  be accessed and also that XMM register beyond the loaded value is zeroed.

  I know about instructions VMOVW and VMOVSH, but they are both part of
  AVX512-FP16 (or AVX10.1) extension, which is too "new" for me.
  I wrote this library for F16C extension, and it should stay at that. Let's
  not be too progressive.
}
{$IFDEF x64}
  {$IFDEF Windows}
    MOVZX   EAX, word ptr [RCX]
  {$ELSE}
    MOVZX   EAX, word ptr [RDI]
  {$ENDIF}
{$ELSE}
    MOVZX   EAX, word ptr [EAX]   // bits 16..31 of EAX are cleared
{$ENDIF}
    MOVD    XMM0, EAX             // bits 32..127 of XMM0 are cleared

    DB  $C4, $E2, $79, $13, $C0   // VCVTPH2PS  XMM0, XMM0

{$IFDEF x64}
  {$IFDEF Windows}
    MOVSS   [RDX], XMM0           // size specier (dword ptr) is missing because of FPC...
  {$ELSE}
    MOVSS   [RSI], XMM0
  {$ENDIF}
{$ELSE}
    MOVSS   [EDX], XMM0
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Float32ToFloat16_ASM(F32Ptr,F16Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F32Ptr              EAX               RCX               RDI
     F16Ptr              EDX               RDX               RSI
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    MOVSS   XMM0, [RCX]
  {$ELSE}
    MOVSS   XMM0, [RDI]
  {$ENDIF}
{$ELSE}
    MOVSS   XMM0, [EAX]                 // bits 32..127 of XMM0 are cleared
{$ENDIF}
  {
    Immediate byte of $04 selects rounding mode that is set in MXCSR - see
    documentation of instruction VCVTPS2PH (eg. in Intel Developer's Manual)
    for more details.
  }
    DB  $C4, $E3, $79, $1D, $C0, $04    // VCVTPS2PH  XMM0, XMM0, $04

    MOVD    EAX,  XMM0
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     word ptr [RDX],  AX
  {$ELSE}
    MOV     word ptr [RSI],  AX
  {$ENDIF}
{$ELSE}
    MOV     word ptr [EDX],  AX         // saving only lower 16 bits
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Float16ToFloat32Vec4_ASM(F16Vec4Ptr,F32Vec4Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F16Vec4Ptr          EAX               RCX               RDI
     F32Vec4Ptr          EDX               RDX               RSI
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    MOVLPS  XMM0, qword ptr [RCX]
  {$ELSE}
    MOVLPS  XMM0, qword ptr [RDI]
  {$ENDIF}
{$ELSE}
    MOVLPS  XMM0, qword ptr [EAX] // can also use MOVSD or MOVQ
{$ENDIF}

    DB  $C4, $E2, $79, $13, $C0   // VCVTPH2PS  XMM0, XMM0

{$IFDEF x64}
  {$IFDEF Windows}
    MOVUPS  dqword ptr [RDX], XMM0
  {$ELSE}
    MOVUPS  dqword ptr [RSI], XMM0
  {$ENDIF}
{$ELSE}
    MOVUPS  dqword ptr [EDX], XMM0
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Float32ToFloat16Vec4_ASM(F32Vec4Ptr,F16Vec4Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F32Vec4Ptr          EAX               RCX               RDI
     F16Vec4Ptr          EDX               RDX               RSI
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    MOVUPS  XMM0, dqword ptr [RCX]
  {$ELSE}
    MOVUPS  XMM0, dqword ptr [RDI]
  {$ENDIF}
{$ELSE}
    MOVUPS  XMM0, dqword ptr [EAX]
{$ENDIF}
    // $04 - rounding selected in MXCSR is used
    DB  $C4, $E3, $79, $1D, $C0, $04  // VCVTPS2PH  XMM0, XMM0, $04

{$IFDEF x64}
  {$IFDEF Windows}
    MOVLPS  qword ptr [RDX], XMM0
  {$ELSE}
    MOVLPS  qword ptr [RSI], XMM0
  {$ENDIF}
{$ELSE}
    MOVLPS  qword ptr [EDX], XMM0
{$ENDIF}
end;

{$ENDIF}
//==============================================================================
const
  F16C_BIAS_DIFF = FLOAT32_EXPONENTBIAS - FLOAT16_EXPONENTBIAS; // 112 (127 - 15)

//------------------------------------------------------------------------------

procedure Float16ToFloat32_PAS(F16Ptr,F32Ptr: Pointer); register;

  Function LeadZeroCount(Value: UInt16): Integer;
  begin
    If Value <> 0 then
      begin
        Result := 0;
        while (Value and UInt16($8000)) = 0  do
          begin
            Value := UInt16(Value shl 1);
            Inc(Result);
          end;
      end
    else Result := 16;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const
  SGN_SHIFT = FLOAT32_SHIFT_SIGN - FLOAT16_SHIFT_SIGN;  // 16, (l)shifting bit 15 to bit 31
  MAN_SHIFT = 13;                                       // creating 23 bit F32 fraction from 10 bit F16 fraction (lshift)
var
  Sign:           UInt16;   // unshifted
  Exponent:       Int32;    // biased exponent (bias 15)
  Mantissa:       UInt16;   // fraction without integer bit
  MantissaShift:  Integer;
begin
Sign := UInt16(F16Ptr^) and FLOAT16_MASK_SIGN;
Exponent := Int32((UInt16(F16Ptr^) and FLOAT16_MASK_EXP) shr FLOAT16_SHIFT_EXP);
Mantissa := UInt16(F16Ptr^) and FLOAT16_MASK_FRAC;
case Exponent of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // zero exponent - zero or denormal
    0:  If Mantissa <> 0 then
          begin
          {
            non-zero mantissa - denormal

            Note that F16C (instruction VCVTPH2PS) will not signal denormal for
            float16 inputs and DAZ flag is ignored (ie. denormals are always
            correctly processed, not converted to zero).

            Normalize the number by shifting franction to the left so that the
            highest set (1) bit is shifted to implicit mantissa integer bit
            (position 23). Also correct exponent to reflect this shift.

            To get number of places by which we need to shift the fraction
            left, we count number of leading (high) zeroes in the 16 bit
            integer containing it - this would only move the highest set bit
            to position 15, but we need it in position 23 in the result,
            threfore we add 8 to account for that.

            As for exponent correction...

              The denormal input has implicit unbiased exponent of -14, which,
              when biased for 32bit float, gives biased exponent 113 ($71),
              that equals to (F16C_BIAS_DIFF + 1).

              Each position by which we left-shift must be corrected by
              subtracting 1 from the exponent, but there is a small problem -
              MantissaShift includes bits by which we left-shift only to get
              f16 flaction into position for f32 fraction. These must not be
              counted towards exponentiation. How many of them are there?...

                Highest bit in f16 fraction is at bit 9 (ten-th bit). To get it
                into position in f32 fraction we have to move it to position 22,
                that is a difference of 13.

                So there is 13 bits we have to uncount.

              The final exponent will be:

                (F16C_BIAS_DIFF + 1) - (MantissaShift - 13)

              After simplification:

                 F16C_BIAS_DIFF + 14 - MantissaShift
          }
            MantissaShift := LeadZeroCount(Mantissa) + 8;
            UInt32(F32Ptr^) := UInt32(UInt32(Sign) shl SGN_SHIFT) or
              UInt32(UInt32(F16C_BIAS_DIFF + 14 - MantissaShift) shl FLOAT32_SHIFT_EXP) or
              (UInt32(UInt32(Mantissa) shl MantissaShift) and FLOAT32_MASK_FRAC);
          end
        // zero mantissa - return signed zero
        else UInt32(F32Ptr^) := UInt32(UInt32(Sign) shl SGN_SHIFT);

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // max exponent - infinity or NaN
  $1F:  If Mantissa <> 0 then
          begin
            // non-zero mantissa - not a number
            If (Mantissa and FLOAT16_MASK_FHB) = 0 then
              begin
                // zero highest fraction bit - signaled NaN
                F16CSignalException(excInvalidOp);
                // no exception raised, return quiet signed NaN with mantissa
                UInt32(F32Ptr^) := UInt32(UInt32(Sign) shl SGN_SHIFT) or FLOAT32_MASK_EXP or
                                   FLOAT32_MASK_FHB or UInt32(UInt32(Mantissa) shl MAN_SHIFT)
              end
            // non-zero FHB - quiet signed NaN with mantissa
            else UInt32(F32Ptr^) := UInt32(UInt32(Sign) shl SGN_SHIFT) or FLOAT32_MASK_EXP or
                                    UInt32(UInt32(Mantissa) shl MAN_SHIFT);
          end
        // zero mantissa - return signed infinity
        else UInt32(F32Ptr^) := UInt32(UInt32(Sign) shl SGN_SHIFT) or FLOAT32_MASK_EXP;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  // normalized number
  UInt32(F32Ptr^) := UInt32(UInt32(Sign) shl SGN_SHIFT) or
    UInt32(UInt32(Exponent + F16C_BIAS_DIFF) shl FLOAT32_SHIFT_EXP) or
    UInt32(UInt32(Mantissa) shl MAN_SHIFT);
end;
end;

//------------------------------------------------------------------------------

procedure Float32ToFloat16_PAS(F32Ptr,F16Ptr: Pointer); register;
const
{
  Default shift of mantissa - shifting bit 22 (highest bit of fraction in F32
  mantissa) to position 9 (highest bit of fraction in F16 mantissa).
}
  MAN_SHIFT = 13;
  // bits removed when right-shifting the mantissa
  MASK_REMB = UInt32(not(Int32(-1) shl MAN_SHIFT));
var
  RoundingMode: TSSERoundingMode;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Function BitScanReverse(Value: UInt32): Integer;
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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Function ShiftMantissa(Sign: UInt16; Mantissa: UInt32; Shift: Integer; out DataLoss: Boolean): UInt16;
  var
    Mask:     UInt32;
    Low,High: UInt32;
  begin
    DataLoss := False;
    If (Shift > 0) and (Shift < 25) then
      begin
        Mask := UInt32(-1) shr (32 - Shift);
        If (Mantissa and Mask) <> 0 then
          begin
            // some bits are shifted-out, do rounding
            DataLoss := True;
            Low := Mantissa and not Mask;
            High := Low + (Mask + 1);
            case RoundingMode of
              rmDown:     If Sign <> 0 then
                            Result := UInt16(High shr Shift)
                          else
                            Result := UInt16(Low shr Shift);
              rmUp:       If Sign <> 0 then
                            Result := UInt16(Low shr Shift)
                          else
                            Result := UInt16(High shr Shift);
              rmTruncate: Result := UInt16(Low shr Shift);
            else
             {rmNearest}
              // select value closer to the original mantissa
              If (Mantissa - Low) > (High - Mantissa) then
                Result := UInt16(High shr Shift)
              else If (Mantissa - Low) < (High - Mantissa) then
                Result := UInt16(Low shr Shift)
              else
                begin
                  // select the one with clear lowest bit
                  If High and (Mask + 1) = 0 then
                    Result := UInt16(High shr Shift)
                  else
                    Result := UInt16(Low shr Shift);
                end;
            end;
          end
        // no bits are shifted-out, no problemo...
        else Result := UInt16(Mantissa shr Shift);
      end
    // following cases should not happen, but whatever...
    else If Shift >= 25 then
      Result := 0
    else
      Result := UInt16(Mantissa);
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
var
  Sign:           UInt16; // sign bit preshifted to bit 15
  Exponent:       Int32;  // biased exponent (bias 127)
  Mantissa:       UInt32; // only fraction, no integer bit
  HighestSetBit:  Integer;
  DataLoss:       Boolean;
  Temp:           UInt32;
  Overflow:       Boolean;
begin
RoundingMode := F16CRoundingModeGet;
// 16 - right-shifting bit 31 to bit 15
Sign := UInt16((UInt32(F32Ptr^) and FLOAT32_MASK_SIGN) shr 16);
Exponent := Int32((UInt32(F32Ptr^) and FLOAT32_MASK_EXP) shr FLOAT32_SHIFT_EXP);
Mantissa := UInt32(F32Ptr^) and FLOAT32_MASK_FRAC;
case Exponent of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // exponent of zero - zero or denormal
  0:    If (Mantissa <> 0) and not F16CControlFlagGet(cfDenormalsAreZeros) then
          begin
            // non zero fraction with DAZ mode disabled - process denormal
            // pre-computation exceptions
            F16CSignalException(excDenormal);
            // post-computation exceptions
            If not F16CExceptionMaskGet(excUnderflow) then
              begin
                // underflow unmasked (ie. raised)
                HighestSetBit := BitScanReverse(Mantissa);  // zero-based index
                If HighestSetBit >= 11 then
                  begin
                    If Mantissa and (UInt32(Int32(-1)) shr (42 - HighestSetBit)) <> 0 then
                      F16CSignalExceptions([excUnderflow,excPrecision])
                    else
                      F16CSignalException(excUnderflow);
                  end
                else F16CSignalException(excUnderflow);
              end
            // underflow masked, signal it with precision
            else F16CSignalExceptions([excUnderflow,excPrecision]);
            // no exception raised, return result
            If ((RoundingMode = rmUp) and (Sign = 0)) or
               ((RoundingMode = rmDown) and (Sign <> 0)) then
              // return signed smallest representable number (FTZ ignored)
              UInt16(F16Ptr^) := Sign or UInt16(1)
            else
              // convert to signed zero
              UInt16(F16Ptr^) := Sign;
          end
        // zero fraction or active DAZ mode, return signed zero
        else UInt16(F16Ptr^) := Sign;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Biased exponents 1..101 (-126..-26 unbiased) - too small to be represented
  in half even as denormal.
}
   1..
  $65:  begin
          If (Mantissa and MASK_REMB) = 0 then
            F16CSignalException(excUnderflow);
          F16CSignalExceptions([excUnderflow,excPrecision]);
          // no exception raised, return result
          If ((RoundingMode = rmUp) and (Sign = 0)) or
             ((RoundingMode = rmDown) and (Sign <> 0)) then
            // return signed smallest representable number (FTZ ignored)
            UInt16(F16Ptr^) := Sign or UInt16(1)
          else
            // convert to signed zero
            UInt16(F16Ptr^) := Sign;
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponents 102..111 (-25..-16 unbiased) - exponent still too small to be
  represented in half, but the result can be denormalized (result with
  implicit exponent of -14, explicit 0).
}
  $66..
  $6F:  begin
          If (Mantissa and MASK_REMB) <> 0 then
            begin
              If F16CExceptionMaskGet(excUnderflow) or not F16CExceptionMaskGet(excPrecision) then
                F16CSignalExceptions([excUnderflow,excPrecision])
              else
                F16CSignalException(excPrecision);
            end;
          Temp := ShiftMantissa(Sign,Mantissa or FLOAT32_MASK_INTB,$7E - Exponent{shift 24..15},DataLoss);
          If F16CExceptionMaskGet(excUnderflow) then
            begin
              If DataLoss then
                F16CSignalExceptions([excUnderflow,excPrecision]);
            end
          else F16CSignalException(excUnderflow);
          // no exception raised, return result (FTZ ignored)
          UInt16(F16Ptr^) := Sign or Temp;
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponent 112 (-15 unbiased) - similar to previous case, but with more
  intricacies because of a posibility of overflow and renormalization (can
  yield a normalized value thanks to rounding).
}
  $70:  begin
          Temp := ShiftMantissa(Sign,Mantissa or FLOAT32_MASK_INTB,MAN_SHIFT,DataLoss);
          If Temp <> UInt16(FLOAT16_MASK_INTB shl 1) then
            begin
              // mantissa has not overflowed, result is still a denormal
              If DataLoss then
                begin
                  If F16CExceptionMaskGet(excUnderflow) or not F16CExceptionMaskGet(excPrecision) then
                    F16CSignalExceptions([excUnderflow,excPrecision])
                  else
                    F16CSignalException(excPrecision);
                end;
              Temp := ShiftMantissa(Sign,Mantissa or FLOAT32_MASK_INTB,MAN_SHIFT + 1,DataLoss);
              If F16CExceptionMaskGet(excUnderflow) then
                begin
                  If DataLoss then
                    F16CSignalExceptions([excUnderflow,excPrecision]);
                end
              else F16CSignalException(excUnderflow);
              UInt16(F16Ptr^) := Sign or Temp;  // FTZ ignored
            end
          else
            begin
              // mantissa overflowed, result is promoted to a normalized number
              F16CSignalException(excPrecision);
              UInt16(F16Ptr^) := Sign or UInt16(UInt16(1) shl FLOAT16_SHIFT_EXP);
            end;
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponents 143..254 (+16..+127 unbiased) - too large to be represented in
  half (resulting exponent would be larger than 15).
}
  $8F..
  $FE:  begin
          If (Mantissa and MASK_REMB) = 0 then
            F16CSignalException(excOverflow);
          F16CSignalExceptions([excOverflow,excPrecision]);
          // no exception raised, return value
          If (RoundingMode = rmTruncate) or
             ((RoundingMode = rmUp) and (Sign <> 0)) or
             ((RoundingMode = rmDown) and (Sign = 0)) then
            // return signed largest representable number
            UInt16(F16Ptr^) := Sign or UInt16(not FLOAT16_MASK_SIGN and
                               UInt16(not(Int16(1) shl FLOAT16_SHIFT_EXP)))
          else
            // convert to signed infinity
            UInt16(F16Ptr^) := Sign or FLOAT16_MASK_EXP;
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // max exponent - infinity or NaN
  $FF:  If Mantissa <> 0 then
          begin
            // non-zero fraction - not a number (NaN)
            If (Mantissa and FLOAT32_MASK_FHB) = 0 then
              begin
                // highest bit of fraction set - signaling NaN
                F16CSignalException(excInvalidOp);
              {
                No exception was raised (InvalidOP was masked), return quiet
                signed NaN with truncated mantissa.
              }
                UInt16(F16Ptr^) := Sign or FLOAT16_MASK_EXP or FLOAT16_MASK_FHB or
                                   UInt16(Mantissa shr MAN_SHIFT);
              end
            // quiet signed NaN with truncated mantisssa
            else UInt16(F16Ptr^) := Sign or FLOAT16_MASK_EXP or
                                    UInt16(Mantissa shr MAN_SHIFT);
          end
        // fraction of zero - return signed infinity
        else UInt16(F16Ptr^) := Sign or FLOAT16_MASK_EXP;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  // exponents 113..142 (-14..+15 unbiased) - representable normalized number
  Mantissa := ShiftMantissa(Sign,Mantissa,MAN_SHIFT,DataLoss);
  // check if mantisa overflowed - if so, increase exponent to compensate
  If (Mantissa and not UInt32(FLOAT16_MASK_FRAC)) <> 0 then
    begin
      Inc(Exponent);
      Overflow := True;
    end
  else Overflow := False;
  // raise exceptions
  If Overflow and (Exponent > 142) then
    begin
      // overflow to infinity
      If DataLoss then
        // also an inexact result
        F16CSignalExceptions([excOverflow,excPrecision])
      else
        F16CSignalException(excOverflow);
    end
  else If DataLoss then
    // inexact result without overflow
    F16CSignalException(excPrecision);
  // no exception raised, return result
  UInt16(F16Ptr^) := Sign or
    (UInt16(UInt16(Exponent - F16C_BIAS_DIFF) shl FLOAT16_SHIFT_EXP) and
    FLOAT16_MASK_EXP) or {fraction}UInt16(Mantissa and FLOAT16_MASK_FRAC);
end;
end;

//------------------------------------------------------------------------------

procedure Float16ToFloat32Vec4_PAS(F16Vec4Ptr,F32Vec4Ptr: Pointer); register;
begin
Float16ToFloat32_PAS(F16Vec4Ptr,F32Vec4Ptr);
Float16ToFloat32_PAS(Addr(TFUVec4H(F16Vec4Ptr^)[1]),Addr(TFUVec4S(F32Vec4Ptr^)[1]));
Float16ToFloat32_PAS(Addr(TFUVec4H(F16Vec4Ptr^)[2]),Addr(TFUVec4S(F32Vec4Ptr^)[2]));
Float16ToFloat32_PAS(Addr(TFUVec4H(F16Vec4Ptr^)[3]),Addr(TFUVec4S(F32Vec4Ptr^)[3]));
end;

//------------------------------------------------------------------------------

procedure Float32ToFloat16Vec4_PAS(F32Vec4Ptr,F16Vec4Ptr: Pointer); register;
begin
Float32ToFloat16_PAS(F32Vec4Ptr,F16Vec4Ptr);
Float32ToFloat16_PAS(Addr(TFUVec4S(F32Vec4Ptr^)[1]),Addr(TFUVec4H(F16Vec4Ptr^)[1]));
Float32ToFloat16_PAS(Addr(TFUVec4S(F32Vec4Ptr^)[2]),Addr(TFUVec4H(F16Vec4Ptr^)[2]));
Float32ToFloat16_PAS(Addr(TFUVec4S(F32Vec4Ptr^)[3]),Addr(TFUVec4H(F16Vec4Ptr^)[3]));
end;

//==============================================================================
var
  VAR_Float16ToFloat32: procedure(F16Ptr,F32Ptr: Pointer); register = Float16ToFloat32_PAS;
  VAR_Float32ToFloat16: procedure(F32Ptr,F16Ptr: Pointer); register = Float32ToFloat16_PAS;

  VAR_Float16ToFloat32Vec4: procedure(F16Vec4Ptr,F32Vec4Ptr: Pointer); register = Float16ToFloat32Vec4_PAS;
  VAR_Float32ToFloat16Vec4: procedure(F32Vec4Ptr,F16Vec4Ptr: Pointer); register = Float32ToFloat16Vec4_PAS;

{-------------------------------------------------------------------------------
    F16C - public functions implementation
-------------------------------------------------------------------------------}

procedure Float16ToFloat32(Float16Ptr,Float32Ptr: Pointer);
begin
VAR_Float16ToFloat32(Float16Ptr,Float32Ptr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float16ToFloat32(const Value: Float16): Float32;
begin
VAR_Float16ToFloat32(@Value,@Result);
end;

//------------------------------------------------------------------------------

procedure HalfToSingle(HalfPtr,SinglePtr: Pointer);
begin
VAR_Float16ToFloat32(HalfPtr,SinglePtr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function HalfToSingle(const Value: Half): Single;
begin
VAR_Float16ToFloat32(@Value,@Result);
end;

//==============================================================================

procedure Float32ToFloat16(Float32Ptr,Float16Ptr: Pointer);
begin
VAR_Float32ToFloat16(Float32Ptr,Float16Ptr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float32ToFloat16(const Value: Float32): Float16;
begin
VAR_Float32ToFloat16(@Value,@Result);
end;

//------------------------------------------------------------------------------

procedure SingleToHalf(SinglePtr,HalfPtr: Pointer);
begin
VAR_Float32ToFloat16(SinglePtr,HalfPtr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SingleToHalf(const Value: Single): Half;
begin
VAR_Float32ToFloat16(@Value,@Result);
end;

//==============================================================================

procedure Float16ToFloat32Vec4(Float16Vec4Ptr,Float32Vec4Ptr: Pointer);
begin
VAR_Float16ToFloat32Vec4(Float16Vec4Ptr,Float32Vec4Ptr);
end;

//------------------------------------------------------------------------------

procedure HalfToSingleVec4(HalfVec4Ptr,SingleVec4Ptr: Pointer);
begin
VAR_Float16ToFloat32Vec4(HalfVec4Ptr,SingleVec4Ptr);
end;

//==============================================================================

procedure Float32ToFloat16Vec4(Float32Vec4Ptr,Float16Vec4Ptr: Pointer);
begin
VAR_Float32ToFloat16Vec4(Float32Vec4Ptr,Float16Vec4Ptr);
end;

//------------------------------------------------------------------------------

procedure SingleToHalfVec4(SingleVec4Ptr,HalfVec4Ptr: Pointer);
begin
VAR_Float32ToFloat16Vec4(SingleVec4Ptr,HalfVec4Ptr);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                Float16 utilities
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Float16 utilities - comparison functions implementation
===============================================================================}

Function IsEqual(const A,B: Float16): Boolean;
var
  AOverlay: UInt16 absolute A;
  BOverlay: UInt16 absolute B;
begin
Result := AOverlay = BOverlay;
end;

//------------------------------------------------------------------------------

Function IsLess(const A,B: Float16): Boolean;
begin
Result := HalfToSingle(A) < HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function IsGreater(const A,B: Float16): Boolean;
begin
Result := HalfToSingle(A) > HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function IsLessOrEqual(const A,B: Float16): Boolean;
begin
Result := HalfToSingle(A) <= HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function IsGreaterOrEqual(const A,B: Float16): Boolean;
begin
Result := HalfToSingle(A) >= HalfToSingle(B);
end;

//==============================================================================

Function CompareValue(const A,B: Float16; const Epsilon: Float16): TFUValueRelationship;
begin
Result := TFUValueRelationship(Math.CompareValue(HalfToSingle(A),HalfToSingle(B),HalfToSingle(Epsilon)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(const A,B: Float16): TFUValueRelationship;
begin
Result := TFUValueRelationship(Math.CompareValue(HalfToSingle(A),HalfToSingle(B)));
end;

//------------------------------------------------------------------------------

Function SameValue(const A,B: Float16; const Epsilon: Float16): Boolean;
begin
Result := Math.SameValue(HalfToSingle(A),HalfToSingle(B),HalfToSingle(Epsilon));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameValue(const A,B: Float16): Boolean;
begin
Result := Math.SameValue(HalfToSingle(A),HalfToSingle(B));
end;

{===============================================================================
    Float16 utilities - basic arithmetic functions implementation
===============================================================================}

Function Add(const A,B: Float16): Float16;
begin
Result := SingleToHalf(HalfToSingle(A) + HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Subtract(const A,B: Float16): Float16;
begin
Result := SingleToHalf(HalfToSingle(A) - HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Multiply(const A,B: Float16): Float16;
begin
Result := SingleToHalf(HalfToSingle(A) * HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Divide(const A,B: Float16): Float16;
begin
Result := SingleToHalf(HalfToSingle(A) / HalfToSingle(B));
end;

{$IFDEF FPC}
{-------------------------------------------------------------------------------
================================================================================
                          Float16 operators overloading
================================================================================
-------------------------------------------------------------------------------}

operator := (Value: Half): Single;
begin
Result := HalfToSingle(Value);
end;

//------------------------------------------------------------------------------

operator := (Value: Single): Half;
begin
Result := SingleToHalf(Value);
end;

//==============================================================================

operator explicit (Value: Half): Single;
begin
Result := HalfToSingle(Value);
end;

//------------------------------------------------------------------------------

operator explicit (Value: Single): Half;
begin
Result := SingleToHalf(Value);
end;

//==============================================================================

operator = (A,B: Half): Boolean;
begin
Result := IsEqual(A,B);
end;

//------------------------------------------------------------------------------

operator > (A,B: Half): Boolean;
begin
Result := IsGreater(A,B);
end;

//------------------------------------------------------------------------------

operator < (A,B: Half): Boolean;
begin
Result := IsLess(A,B);
end;

//------------------------------------------------------------------------------

operator >= (A,B: Half): Boolean;
begin
Result := IsGreaterOrEqual(A,B);
end;

//------------------------------------------------------------------------------

operator <= (A,B: Half): Boolean;
begin
Result := IsLessOrEqual(A,B);
end;

//------------------------------------------------------------------------------

operator <> (A,B: Half): Boolean;
begin
Result := not IsEqual(A,B);
end;

//==============================================================================

operator + (A: Half): Half;
begin
Result := A;
end;

//------------------------------------------------------------------------------

operator - (A: Half): Half;
begin
Result := Neg(A);
end;

//==============================================================================

operator + (A,B: Half): Half;
begin
Result := Add(A,B);
end;

//------------------------------------------------------------------------------

operator - (A,B: Half): Half;
begin
Result := Subtract(A,B);
end;

//------------------------------------------------------------------------------

operator * (A,B: Half): Half;
begin
Result := Multiply(A,B);
end;

//------------------------------------------------------------------------------

operator / (A,B: Half): Half;
begin
Result := Divide(A,B);
end;

{$ENDIF}
{===============================================================================
--------------------------------------------------------------------------------
                  F32 <-> F64 conversion (FXC) state management
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    FXC state management - emulated state
===============================================================================}
var
  VAR_FXCModeOfOperation: TFXCModeOfOperation = modPascalX87;

type
  TFXCState = record
    Initialized:      Boolean;
    StatusWord:       UInt16;
    ControlWord:      UInt16;
    ControlAndStatus: UInt32;
    ExceptState:  record
      StatusWord:   UInt16;
      ControlWord:  UInt16;
    end;
  end;
  PFXCState = ^TFXCState;

threadvar
  // automatically initialized to all-zero (field Initialized is false)
  THRVAR_FXCState:  TFXCState;

//------------------------------------------------------------------------------

Function FXCGetStatePtr: PFXCState;
begin
Result := @THRVAR_FXCState;
If not Result^.Initialized then
  begin
    Result^.Initialized := True;
    Result^.StatusWord := 0;
    Result^.ControlWord := X87CW_DefaultValue;
    Result^.ControlAndStatus := MXCSR_DefaultValue;
    // ExceptState is prepared when needed
  end;
end;

//------------------------------------------------------------------------------

Function FXCExceptStatePrepare: PFXCState;
begin
Result := FXCGetStatePtr;
Result^.ExceptState.StatusWord := Result^.StatusWord;
Result^.ExceptState.ControlWord := Result^.ControlWord;
end;

{===============================================================================
    FXC state management - exceptions implementation
===============================================================================}
{-------------------------------------------------------------------------------
    FXC state management - EFXCException class implementation
-------------------------------------------------------------------------------}

procedure EFXCException.Initialize;
var
  LocalState: TFXCState;
begin
LocalState := FXCGetStatePtr^;
fControlAndStatus.ModeOfOperation := VAR_FXCModeOfOperation;
If fControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  begin
    // exception raised in X87 mode
    fControlAndStatus.StatusWord := LocalState.ExceptState.StatusWord;
    fControlAndStatus.ControlWord := LocalState.ExceptState.ControlWord;
    fPendingExcs := X87SWExceptionFlagsGet(fControlAndStatus.StatusWord);
    fMaskedExcs := fPendingExcs * X87CWExceptionMasksGet(fControlAndStatus.ControlWord);
    fUnmaskedExcs := fPendingExcs - X87CWExceptionMasksGet(fControlAndStatus.ControlWord);
    // raise only unmasked exceptions
    fRaisedExcs := fUnmaskedExcs;
    // resolve stack faults
    If (excInvalidOp in fRaisedExcs) and X87SWStatusFlagGet(fControlAndStatus.StatusWord,sfStackFault) then
      begin
        Exclude(fRaisedExcs,excInvalidOp);
        If X87SWStatusFlagGet(fControlAndStatus.StatusWord,sfConditionCodeC1) then
          Include(fRaisedExcs,excStackOverflow)
        else
          Include(fRaisedExcs,excStackUnderflow);
      end;
  end
else
  begin
    // exception raised in SSE/AVX mode
    fControlAndStatus.ControlAndStatus := LocalState.ControlAndStatus;
    fPendingExcs := MXCSRExceptionFlagsGet(fControlAndStatus.ControlAndStatus);
    fMaskedExcs := fPendingExcs * MXCSRExceptionMasksGet(fControlAndStatus.ControlAndStatus);
    fUnmaskedExcs := fPendingExcs - MXCSRExceptionMasksGet(fControlAndStatus.ControlAndStatus);
    // raise only unmasked exceptions
    fRaisedExcs := fUnmaskedExcs;
  end;
// reinitialize the environment
FXCEnvironmentInit;
FXCControlAndStatusInit;
end;

{-------------------------------------------------------------------------------
    FXC state management - exception selection and raise
-------------------------------------------------------------------------------}

procedure FXCSelectAndRaiseException;
var
  LocalState: TFXCState;
  Unmasked:   TFXCFlagExceptions;
begin
{
  For priorities, see documentation of functions F80CSelectAndRaiseException
  or F16CSelectAndRaiseException.
}
LocalState := FXCGetStatePtr^;
// detailed behavior depends on current mode of operation
If VAR_FXCModeOfOperation in [modPascalX87,modAssemblyX87] then
  begin
    Unmasked := X87SWExceptionFlagsGet(LocalState.ExceptState.StatusWord) -
                X87CWExceptionMasksGet(LocalState.ExceptState.ControlWord);
    If excInvalidOp in Unmasked then
      begin
        If X87SWStatusFlagGet(LocalState.ExceptState.StatusWord,sfStackFault) then
          begin
            If X87SWStatusFlagGet(LocalState.ExceptState.StatusWord,sfConditionCodeC1) then
              raise EFXCStackOverflow.Create
            else
              raise EFXCStackUnderflow.Create;
          end
        else raise EFXCInvalidOp.Create;
      end;
  end
else
  begin
    Unmasked := MXCSRExceptionFlagsGet(LocalState.ControlAndStatus) -
                MXCSRExceptionMasksGet(LocalState.ControlAndStatus);
    If excInvalidOp in Unmasked then
      raise EFXCInvalidOp.Create;
  end;
// common cases (no invalid operation or stack fault exception)
If Unmasked <> [] then
  begin
    If excDivByZero in Unmasked then
      raise EFXCDivByZero.Create;
    If excDenormal in Unmasked then
      raise EFXCDenormal.Create;
    If excOverflow in Unmasked then
      raise EFXCOverflow.Create;
    If excUnderflow in Unmasked then
      raise EFXCUnderflow.Create;
    If excPrecision in Unmasked then
      raise EFXCPrecision.Create;
  end;
end;

{===============================================================================
    FXC state management - low-lewel access implementation
===============================================================================}
{-------------------------------------------------------------------------------
    FXC state management - LL auxiliary functions
-------------------------------------------------------------------------------}

Function FXCControlAndStatus(StatusWord,ControlWord: UInt16): TFXCControlAndStatus;
begin
Result.ModeOfOperation := modPascalX87;
Result.StatusWord := StatusWord;
Result.ControlWord := ControlWord;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FXCControlAndStatus(StatusAndControl: UInt32): TFXCControlAndStatus;
begin
Result.ModeOfOperation := modPascalSSE;
Result.ControlAndStatus := StatusAndControl;
end;

{-------------------------------------------------------------------------------
    FXC state management - LL main functions
-------------------------------------------------------------------------------}

Function FXCEmulated: Boolean;
begin
Result := VAR_FXCModeOfOperation in [modPascalX87,modPascalSSE];
end;

//------------------------------------------------------------------------------

Function FXCModeOfOperation: TFXCModeOfOperation;
begin
Result := VAR_FXCModeOfOperation;
end;

//------------------------------------------------------------------------------

Function FXCControlAndStatusGet: TFXCControlAndStatus;
var
  LocalState: TFXCState;
begin
LocalState := FXCGetStatePtr^;
Result.ModeOfOperation := VAR_FXCModeOfOperation;
case Result.ModeOfOperation of
  modPascalX87:   begin
                    Result.StatusWord := LocalState.StatusWord;
                    Result.ControlWord := LocalState.ControlWord;
                  end;
  modAssemblyX87: begin
                    Result.StatusWord := X87StatusWordGet;
                    Result.ControlWord := X87ControlWordGet;
                  end;
  modPascalSSE:   Result.ControlAndStatus := LocalState.ControlAndStatus;
  modAssemblySSE: Result.ControlAndStatus := SSEControlAndStatusGet;
else
  raise EFUInvalidState.CreateFmt('FXCControlAndStatusGet: Invalid mode of operation (%d).',[Ord(Result.ModeOfOperation)]);
end;
end;

//------------------------------------------------------------------------------

procedure FXCControlAndStatusSet(NewValue: TFXCControlAndStatus);
var
  StatePtr:         PFXCState;
  ModeOfOperation:  TFXCModeOfOperation;
begin
// check compatibility of mode of operation in given value and current mode
ModeOfOperation := VAR_FXCModeOfOperation;
If ModeOfOperation in [modPascalX87,modAssemblyX87] then
  begin
    If not(NewValue.ModeOfOperation in [modPascalX87,modAssemblyX87]) then
      raise EFUInvalidValue.CreateFmt('FXCControlAndStatusSet: Incompatible mode of operation (%d vs %d).',
                                      [Ord(NewValue.ModeOfOperation),Ord(ModeOfOperation)]);
  end
else
  begin
    If not(NewValue.ModeOfOperation in [modPascalSSE,modAssemblySSE]) then
      raise EFUInvalidValue.CreateFmt('FXCControlAndStatusSet: Incompatible mode of operation (%d vs %d).',
                                      [Ord(NewValue.ModeOfOperation),Ord(ModeOfOperation)]);
  end;
// note that in x87 modes, the status word is not changed
case ModeOfOperation of
  modPascalX87:   begin
                    StatePtr := FXCExceptStatePrepare;
                    StatePtr^.ControlWord := NewValue.ControlWord;
                    // process pending exceptions
                    If ((StatePtr^.StatusWord and X87SW_EFLAG_All) and not (StatePtr^.ControlWord and X87CW_EMASK_All)) <> 0 then
                      StatePtr^.StatusWord := StatePtr^.StatusWord or (X87SW_ExceptionSummary or X87SW_FPUBusy)
                    else
                      StatePtr^.StatusWord := StatePtr^.StatusWord and not (X87SW_ExceptionSummary or X87SW_FPUBusy);
                    // raise pending unmasked exceptions, if any is present
                    If (StatePtr^.ExceptState.StatusWord and X87SW_ExceptionSummary) <> 0 then
                      FXCSelectAndRaiseException;
                  end;
  modAssemblyX87: X87ControlWordSet(NewValue.ControlWord);
  modPascalSSE:   FXCGetStatePtr^.ControlAndStatus := NewValue.ControlAndStatus;
  modAssemblySSE: SSEControlAndStatusSet(NewValue.ControlAndStatus);
else
  raise EFUInvalidState.CreateFmt('FXCControlAndStatusSet: Invalid mode of operation (%d).',[Ord(ModeOfOperation)]);
end;
end;

//------------------------------------------------------------------------------

procedure FXCEnvironmentInit;
var
  StatePtr: PFXCState;
begin
StatePtr := FXCGetStatePtr;
case VAR_FXCModeOfOperation of
  modPascalX87:   begin
                    StatePtr^.StatusWord := 0;
                    StatePtr^.ControlWord := X87CW_InitialValue;
                  end;
  modAssemblyX87: X87EnvironmentInit;
  modPascalSSE:   StatePtr^.ControlAndStatus := MXCSR_InitialValue;
  modAssemblySSE: SSEEnvironmentInit;
else
  raise EFUInvalidState.CreateFmt('FXCEnvironmentInit: Invalid mode of operation (%d).',[Ord(VAR_FXCModeOfOperation)]);
end;
end;

//------------------------------------------------------------------------------

procedure FXCControlAndStatusInit;
begin
case VAR_FXCModeOfOperation of
  modPascalX87:   FXCControlAndStatusSet(FXCControlAndStatus(0{ignored},X87CW_DefaultValue));
  modAssemblyX87: X87ControlWordInit;
  modPascalSSE:   FXCGetStatePtr^.ControlAndStatus := MXCSR_DefaultValue;
  modAssemblySSE: SSEControlAndStatusInit;
else
  raise EFUInvalidState.CreateFmt('FXCControlAndStatusInit: Invalid mode of operation (%d).',[Ord(VAR_FXCModeOfOperation)]);
end;
end;

{===============================================================================
    FXC state management - abstracted access implementation
===============================================================================}
{-------------------------------------------------------------------------------
    FXC state management - flags and rounding abstracted access
-------------------------------------------------------------------------------}

Function FXCStatusFlagGet(Flag: TFXCStatusFlag): Boolean;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87SWStatusFlagGet(ControlAndStatus.StatusWord,Flag)
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function FXCStatusFlagsGet: TFXCStatusFlags;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87SWStatusFlagsGet(ControlAndStatus.StatusWord)
else
  Result := [];
end;

//------------------------------------------------------------------------------

Function FXCRoundingModeGet: TFXCRoundingMode;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87CWRoundingModeGet(ControlAndStatus.ControlWord)
else
  Result := MXCSRRoundingModeGet(ControlAndStatus.ControlAndStatus);
end;

//------------------------------------------------------------------------------

Function FXCRoundingModeSet(NewValue: TFXCRoundingMode): TFXCRoundingMode;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
// get old value and change it to a new one
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87CWRoundingModeSet(ControlAndStatus.ControlWord,NewValue)
else
  Result := MXCSRRoundingModeSet(ControlAndStatus.ControlAndStatus,NewValue);
// and store back
FXCControlAndStatusSet(ControlAndStatus);
end;

//------------------------------------------------------------------------------

Function FXCControlFlagGet(Flag: TFXCControlFlag): Boolean;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalSSE,modAssemblySSE] then
  Result := MXCSRControlFlagGet(ControlAndStatus.ControlAndStatus,Flag)
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function FXCControlFlagSet(Flag: TFXCControlFlag; NewValue: Boolean): Boolean;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalSSE,modAssemblySSE] then
  begin
    Result := MXCSRControlFlagSet(ControlAndStatus.ControlAndStatus,Flag,NewValue);
    FXCControlAndStatusSet(ControlAndStatus);
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function FXCControlFlagsGet: TFXCControlFlags;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalSSE,modAssemblySSE] then
  Result := MXCSRControlFlagsGet(ControlAndStatus.ControlAndStatus)
else
  Result := [];
end;

//------------------------------------------------------------------------------

Function FXCControlFlagsSet(NewValue: TFXCControlFlags): TFXCControlFlags;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalSSE,modAssemblySSE] then
  begin
    Result := MXCSRControlFlagSSet(ControlAndStatus.ControlAndStatus,NewValue);
    FXCControlAndStatusSet(ControlAndStatus);
  end
else Result := [];
end;

{-------------------------------------------------------------------------------
    FXC state management - exceptions abstraction
-------------------------------------------------------------------------------}

Function FXCExceptionMaskGet(Exception: TFXCException): Boolean;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87CWExceptionMaskGet(ControlAndStatus.ControlWord,Exception)
else
  Result := MXCSRExceptionMaskGet(ControlAndStatus.ControlAndStatus,Exception);
end;

//------------------------------------------------------------------------------

Function FXCExceptionMaskSet(Exception: TFXCException; NewValue: Boolean): Boolean;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87CWExceptionMaskSet(ControlAndStatus.ControlWord,Exception,NewValue)
else
  Result := MXCSRExceptionMaskSet(ControlAndStatus.ControlAndStatus,Exception,NewValue);
FXCControlAndStatusSet(ControlAndStatus);
end;

//------------------------------------------------------------------------------

Function FXCExceptionMasksGet: TFXCExceptions;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87CWExceptionMasksGet(ControlAndStatus.ControlWord)
else
  Result := MXCSRExceptionMasksGet(ControlAndStatus.ControlAndStatus);
end;

//------------------------------------------------------------------------------

Function FXCExceptionMasksSet(NewValue: TFXCExceptions): TFXCExceptions;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87CWExceptionMasksSet(ControlAndStatus.ControlWord,NewValue)
else
  Result := MXCSRExceptionMasksSet(ControlAndStatus.ControlAndStatus,NewValue);
FXCControlAndStatusSet(ControlAndStatus);
end;

//------------------------------------------------------------------------------

Function FXCExceptionFlagGet(Exception: TFXCException): Boolean;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87SWExceptionFlagGet(ControlAndStatus.StatusWord,Exception)
else
  Result := MXCSRExceptionFlagGet(ControlAndStatus.ControlAndStatus,Exception);
end;

//------------------------------------------------------------------------------

Function FXCExceptionFlagSet(Exception: TFXCException; NewValue: Boolean): Boolean;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalSSE,modAssemblySSE] then
  begin
    Result := MXCSRExceptionFlagSet(ControlAndStatus.ControlAndStatus,Exception,NewValue);
    FXCControlAndStatusSet(ControlAndStatus);
  end
// x87 does not allow direct changing of exception flags in status word
else Result := X87SWExceptionFlagGet(ControlAndStatus.ControlAndStatus,Exception);
end;

//------------------------------------------------------------------------------

Function FXCExceptionFlagsGet: TFXCExceptions;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalX87,modAssemblyX87] then
  Result := X87SWExceptionFlagsGet(ControlAndStatus.StatusWord)
else
  Result := MXCSRExceptionFlagsGet(ControlAndStatus.ControlAndStatus);
end;

//------------------------------------------------------------------------------

Function FXCExceptionFlagsSet(NewValue: TFXCExceptions): TFXCExceptions;
var
  ControlAndStatus: TFXCControlAndStatus;
begin
ControlAndStatus := FXCControlAndStatusGet;
If ControlAndStatus.ModeOfOperation in [modPascalSSE,modAssemblySSE] then
  begin
    Result := MXCSRExceptionFlagsSet(ControlAndStatus.ControlAndStatus,NewValue);
    FXCControlAndStatusSet(ControlAndStatus);
  end
else Result := X87SWExceptionFlagsGet(ControlAndStatus.ControlAndStatus);
end;

//------------------------------------------------------------------------------

procedure FXCExceptionsClear;
var
  StatePtr: PFXCState;
begin
StatePtr := FXCGetStatePtr;
case VAR_FXCModeOfOperation of
  modPascalX87:   StatePtr^.StatusWord := 0;
  modAssemblyX87: X87ExceptionsClear;
  modPascalSSE:   StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus and not MXCSR_EFLAG_All;
  modAssemblySSE: SSEExceptionsClear;
else
  raise EFUInvalidState.CreateFmt('FXCExceptionsClear: Invalid mode of operation (%d).',[Ord(VAR_FXCModeOfOperation)]);
end;
end;

//------------------------------------------------------------------------------

procedure FXCExceptionsRaise;
begin
case VAR_FXCModeOfOperation of
  modPascalX87:   If X87SWStatusFlagGet(FXCExceptStatePrepare^.ExceptState.StatusWord,sfExceptionSummary) then
                    FXCSelectAndRaiseException;
  modAssemblyX87: X87ExceptionsRaise;
{
  Vector SIMD floating point units (SSE/AVX) do not raise pending unmasked
  exceptions - therefore do nothing here for such modes.
}
  modPascalSSE,
  modAssemblySSE:;
else
  raise EFUInvalidState.CreateFmt('FXCExceptionsRaise: Invalid mode of operation (%d).',[Ord(VAR_FXCModeOfOperation)]);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                     Float32 <-> Float64 conversions (FXC)
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    FXC - axiliary functions
===============================================================================}

procedure FXCSignalExceptions(Exceptions: TFXCRaiseExceptions);
var
  StatePtr: PFXCState;
begin
StatePtr := FXCGetStatePtr;
If VAR_FXCModeOfOperation in [modPascalX87,modAssemblyX87] then
  begin
    If excInvalidOp in Exceptions then
      StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_InvalidOp;
    If excDenormal in Exceptions then
      StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_Denormal;
    If excDivByZero in Exceptions then
      StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_DivByZero;
    If excOverflow in Exceptions then
      StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_Overflow;
    If excUnderflow in Exceptions then
      StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_Underflow;
    If excPrecision in Exceptions then
      StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_EFLAG_Precision;
    If ([excStackOverflow,excStackUnderflow] * Exceptions) <> [] then
      begin
        StatePtr^.StatusWord := StatePtr^.StatusWord or (X87SW_EFLAG_InvalidOp or X87SW_StackFault);
        If excStackOverflow in Exceptions then
          StatePtr^.StatusWord := StatePtr^.StatusWord or X87SW_ConditionCode_C1;
      end;
    // set exception summary flag in the status word
    If ((StatePtr^.StatusWord and X87SW_EFLAG_All) and not (StatePtr^.ControlWord and X87CW_EMASK_All)) <> 0 then
      StatePtr^.StatusWord := StatePtr^.StatusWord or (X87SW_ExceptionSummary or X87SW_FPUBusy)
    else
      StatePtr^.StatusWord := StatePtr^.StatusWord and not (X87SW_ExceptionSummary or X87SW_FPUBusy);
    FXCExceptionsRaise;
  end
else
  begin
    If excInvalidOp in Exceptions then
      StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_InvalidOp;
    If excDenormal in Exceptions then
      StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_Denormal;
    If excDivByZero in Exceptions then
      StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_DivByZero;
    If excOverflow in Exceptions then
      StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_Overflow;
    If excUnderflow in Exceptions then
      StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_Underflow;
    If excPrecision in Exceptions then
      StatePtr^.ControlAndStatus := StatePtr^.ControlAndStatus or MXCSR_EFLAG_Precision;
    // ignore stack faults
    FXCSelectAndRaiseException;
  end;
end;

//------------------------------------------------------------------------------

procedure FXCSignalException(Exception: TFXCRaiseException);
begin
FXCSignalExceptions([Exception]);
end;

{===============================================================================
    FXC - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    FXC - assembly implementation
-------------------------------------------------------------------------------}
{$IFNDEF PurePascal}

procedure Float32ToFloat64_ASM_X87(F32Ptr,F64Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F32Ptr              EAX               RCX               RDI
     F64Ptr              EDX               RDX               RSI
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    FLD     dword ptr [RCX]
    FSTP    qword ptr [RDX]
  {$ELSE}
    FLD     dword ptr [RDI]
    FSTP    qword ptr [RSI]
  {$ENDIF}
{$ELSE}
    FLD     dword ptr [EAX]
    FSTP    qword ptr [EDX]
{$ENDIF}
    FWAIT
end;

//------------------------------------------------------------------------------

procedure Float64ToFloat32_ASM_X87(F64Ptr,F32Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F64Ptr              EAX               RCX               RDI
     F32Ptr              EDX               RDX               RSI
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    FLD     qword ptr [RCX]
    FSTP    dword ptr [RDX]
  {$ELSE}
    FLD     qword ptr [RDI]
    FSTP    dword ptr [RSI]
  {$ENDIF}
{$ELSE}
    FLD     qword ptr [EAX]
    FSTP    dword ptr [EDX]
{$ENDIF}
    FWAIT
end;

//==============================================================================

procedure Float32ToFloat64_ASM_SSE(F32Ptr,F64Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F32Ptr              EAX               RCX               RDI
     F64Ptr              EDX               RDX               RSI

-------------------------------------------------------------------------------}
{
  And again, size specifiers are omitted because FPC cannot handle them
  properly in vector instructions. For example:

    CVTSD2SS  XMM0, qword ptr [EAX]

  FPC (3.2.2 - afaik current stable) fails to compile that with following
  nonsensical message:

    Error: Asm: [cvtsd2ss xmmreg,mem32] invalid combination of opcode and operands

  ...since when is qword 32bit?!
}
{$IFDEF x64}
  {$IFDEF Windows}
    CVTSS2SD  XMM0, [RCX]
    MOVSD     [RDX], XMM0
  {$ELSE}
    CVTSS2SD  XMM0, [RDI]
    MOVSD     [RSI], XMM0
  {$ENDIF}
{$ELSE}
    CVTSS2SD  XMM0, [EAX]
    MOVSD     [EDX], XMM0
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Float64ToFloat32_ASM_SSE(F64Ptr,F32Ptr: Pointer); register; assembler;
asm
{-------------------------------------------------------------------------------
                    win32 & lin32         win64             lin64
     F64Ptr              EAX               RCX               RDI
     F32Ptr              EDX               RDX               RSI
-------------------------------------------------------------------------------}
{$IFDEF x64}
  {$IFDEF Windows}
    CVTSD2SS  XMM0, [RCX]
    MOVSS     [RDX], XMM0
  {$ELSE}
    CVTSD2SS  XMM0, [RDI]
    MOVSS     [RSI], XMM0
  {$ENDIF}
{$ELSE}
    CVTSD2SS  XMM0, [EAX]
    MOVSS     [EDX], XMM0
{$ENDIF}
end;

{$ENDIF}
{-------------------------------------------------------------------------------
    FXC - pascal implementation auxiliaries
-------------------------------------------------------------------------------}
const
  FXC_BIAS_DIFF = FLOAT64_EXPONENTBIAS - FLOAT32_EXPONENTBIAS;  // 896 (1023 - 127)

//------------------------------------------------------------------------------

Function LeadZeroCount(Value: UInt32): Integer;
begin
If Value <> 0 then
  begin
    Result := 0;
    while (Value and UInt32($80000000)) = 0  do
      begin
        Value := UInt32(Value shl 1);
        Inc(Result);
      end;
  end
else Result := 32;
end;

//------------------------------------------------------------------------------

Function ShiftMantissa(Sign: UInt32; Mantissa: UInt64; Shift: Integer; out DataLoss: Boolean; RoundingMode: TFXCRoundingMode): UInt32;
var
  Mask:     UInt64;
  Low,High: UInt64;
begin
DataLoss := False;
{
  53 is maximum right-shift that will move-out all mantissa bits, including
  integer bit.
}
If (Shift > 0) and (Shift < 54) then
  begin
    Mask := UInt64(-1) shr (64 - Shift);
    If (Mantissa and Mask) <> 0 then
      begin
        // some bits are shifted-out, do rounding
        DataLoss := True;
        Low := Mantissa and not Mask;
        High := Low + (Mask + 1);
        case RoundingMode of
          rmDown:     If Sign <> 0 then
                        Result := UInt32(High shr Shift)
                      else
                        Result := UInt32(Low shr Shift);
          rmUp:       If Sign <> 0 then
                        Result := UInt32(Low shr Shift)
                      else
                        Result := UInt32(High shr Shift);
          rmTruncate: Result := UInt32(Low shr Shift);
        else
         {rmNearest}
          // select value closer to the original mantissa
          If (Mantissa - Low) > (High - Mantissa) then
            Result := UInt32(High shr Shift)
          else If (Mantissa - Low) < (High - Mantissa) then
            Result := UInt32(Low shr Shift)
          else
            begin
              // select the one with clear lowest bit
              If High and (Mask + 1) = 0 then
                Result := UInt32(High shr Shift)
              else
                Result := UInt32(Low shr Shift);
            end;
        end;
      end
    // no bits are shifted-out, no problemo...
    else Result := UInt32(Mantissa shr Shift);
  end
// following cases should not happen, but whatever...
else If Shift >= 54 then
  Result := 0
else
  Result := UInt32(Mantissa);
end;

{-------------------------------------------------------------------------------
    FXC - pascal implementation
-------------------------------------------------------------------------------}

procedure Float32ToFloat64_PAS_X87(F32Ptr,F64Ptr: Pointer); register;
const
  SGN_SHIFT = FLOAT64_SHIFT_SIGN - FLOAT32_SHIFT_SIGN;  // 32, (l)shifting bit 63 to bit 31
  MAN_SHIFT = 29;                                       // creating 52 bit F64 fraction from 23 bit F32 fraction (lshift)
var
  Sign:           UInt32;   // unshifted (sign is in bit 31)
  Exponent:       Int32;    // biased exponent (bias 127)
  Mantissa:       UInt32;   // only fraction, without integer bit
  MantissaShift:  Integer;
begin
FXCExceptionsRaise;
Sign := UInt32(F32Ptr^) and FLOAT32_MASK_SIGN;
Exponent := Int32((UInt32(F32Ptr^) and FLOAT32_MASK_EXP) shr FLOAT32_SHIFT_EXP);
Mantissa := UInt32(F32Ptr^) and FLOAT32_MASK_FRAC;
case Exponent of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // zero exponent - zero or denormal
  0:    If Mantissa <> 0 then
          begin
            // non-zero mantissa - denormal
            FXCSignalException(excDenormal);
          {
            Normalize

            Shift mantissa left so that its highest set bit will be shifted
            to integer bit (bit 52), also correct exponent to reflect this
            change.
          }
            MantissaShift := LeadZeroCount(Mantissa) + 21;
            UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or
              UInt64(UInt64(FXC_BIAS_DIFF + 30 - MantissaShift) shl FLOAT64_SHIFT_EXP) or
              (UInt64(UInt64(Mantissa) shl MantissaShift) and FLOAT64_MASK_FRAC);
          end
        // return signed zero
        else UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT);

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // max exponent - infinity or NaN
  $FF:  If Mantissa <> 0 then
          begin
            // not a number
            If (Mantissa and FLOAT32_MASK_FHB) = 0 then
              begin
                // signaled NaN
                FXCSignalException(excInvalidOp);
                // if no exception was raised, return quiet signed NaN with mantissa
                UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or FLOAT64_MASK_EXP or
                                   FLOAT64_MASK_FHB or UInt64(UInt64(Mantissa) shl MAN_SHIFT);
              end
            // quiet signed NaN with mantissa
            else UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or FLOAT64_MASK_EXP or
                                    UInt64(UInt64(Mantissa) shl MAN_SHIFT);
          end
        // signed infinity
        else UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or FLOAT64_MASK_EXP;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  // normal number
  UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or
    UInt64(UInt64(Exponent + FXC_BIAS_DIFF) shl FLOAT64_SHIFT_EXP) or
    UInt64(UInt64(Mantissa) shl MAN_SHIFT);
end;
end;

//------------------------------------------------------------------------------

procedure Float64ToFloat32_PAS_X87(F64Ptr,F32Ptr: Pointer); register;
const
{
  Default shift of mantissa - shifting bit 51 (highest bit of fraction in F64
  mantissa) to position 22 (highest bit of fraction in F32 mantissa).
}
  MAN_SHIFT = 29;
var
  RoundingMode: TFXCRoundingMode;
  Sign:         UInt32; // sign is pre-shifted to bit 31
  Exponent:     Int32;  // biased exponent (bias 1023)
  Mantissa:     UInt64; // without integer bit
  DataLoss:     Boolean;
  Temp:         UInt64;
  Overflow:     Boolean;
begin
FXCExceptionsRaise;
RoundingMode := FXCRoundingModeGet;
// 32 - right-shifting bit 63 to bit 31
Sign := UInt32((UInt64(F64Ptr^) and FLOAT64_MASK_SIGN) shr 32);
Exponent := Int32((UInt64(F64Ptr^) and FLOAT64_MASK_EXP) shr FLOAT64_SHIFT_EXP);
Mantissa := UInt64(F64Ptr^) and FLOAT64_MASK_FRAC;
case Exponent of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // exponent of zero - zero or denormal
  0:    If Mantissa <> 0 then
          begin
          {
            non-zero mantissa - denormals

            Following two exceptions must be signaled separately, because if
            denormal is unmasked (is raised), then underflow condition is not
            encountered and therefore not flagged in status word.
          }
            FXCSignalException(excDenormal);
            FXCSignalException(excUnderflow);
            If ((RoundingMode = rmUp) and (Sign = 0)) or
               ((RoundingMode = rmDown) and (Sign <> 0)) then
              // return signed smallest representable number (denormal)
              UInt32(F32Ptr^) := Sign or UInt32(1)
            else
              // convert to signed zero
              UInt32(F32Ptr^) := Sign;
            FXCSignalException(excPrecision);
          end
        // mantissa of 0 - return signed zero
        else UInt32(F32Ptr^) := Sign;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponent 1..872 (-1022..-151 unbiased) - exponent too small to be
  represented in single even as denormal.
}
  $1..
  $368: begin
          FXCSignalException(excUnderflow);
          If ((RoundingMode = rmUp) and (Sign = 0)) or
             ((RoundingMode = rmDown) and (Sign <> 0)) then
            // return signed smallest representable number (denormal)
            UInt32(F32Ptr^) := Sign or UInt32(1)
          else
            // convert to signed zero
            UInt32(F32Ptr^) := Sign;
          FXCSignalException(excPrecision);
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponent 873..895 (-150..-128 unbiased) - exponent still too small to be
  represented in single, but can be denormalized to fit
}
  $369..
  $37F: begin
          // denormalize
          Temp := ShiftMantissa(Sign,Mantissa or FLOAT64_MASK_INTB,$39E - Exponent{shift 53..31},DataLoss,RoundingMode);
          If FXCExceptionMaskGet(excUnderflow) then
            begin
              UInt32(F32Ptr^) := Sign or Temp;
              If DataLoss then
                // inexact result
                FXCSignalExceptions([excUnderflow,excPrecision]);
            end
          else FXCSignalException(excUnderflow);
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponent 896 (-127 unbiased) - similar to previous case, can yield normalized
  number thanks to rounding.
}
  $380: begin
        {
          Align bits in mantissa so it corresponds to float32 and look whether
          it overflowed thanks to rounding.
        }
          Temp := ShiftMantissa(Sign,Mantissa or FLOAT64_MASK_INTB,MAN_SHIFT,DataLoss,RoundingMode);
          If Temp <> UInt32(FLOAT32_MASK_INTB shl 1) then
            begin
              // mantissa has not overflowed, result is still a denormal
              If FXCExceptionMaskGet(excUnderflow) then
                begin
                  UInt32(F32Ptr^) := Sign or ShiftMantissa(Sign,Mantissa or FLOAT64_MASK_INTB,MAN_SHIFT + 1,DataLoss,RoundingMode);
                  If DataLoss then
                    FXCSignalExceptions([excUnderflow,excPrecision]);
                end
              else FXCSignalException(excUnderflow);
            end
          else
            begin
              // mantissa overflowed, result is promoted to a normalized number
              UInt32(F32Ptr^) := Sign or UInt32(UInt32(1) shl FLOAT32_SHIFT_EXP);
              FXCSignalException(excPrecision);
            end;
         end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponent 1151..2046 (128..1023 unbiased) - exponent too large to be
  represented in single (max. valid exponent 127).
}
  $47F..
  $7FE: begin
          FXCSignalException(excOverflow);
          If (RoundingMode = rmTruncate) or
             ((RoundingMode = rmUp) and (Sign <> 0)) or
             ((RoundingMode = rmDown) and (Sign = 0)) then
            // return signed largest representable number (max. exp. - 1)
            UInt32(F32Ptr^) := Sign or UInt32(UInt32(not Int32(FLOAT32_MASK_SIGN)) and
                               UInt32(not(Int32(1) shl FLOAT32_SHIFT_EXP)))
          else
            // convert to signed infinity
            UInt32(F32Ptr^) := Sign or FLOAT32_MASK_EXP;
          FXCSignalException(excPrecision);
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // maximum exponent - NaN or infinity
  $7FF: If (Mantissa and FLOAT64_MASK_FRAC) <> 0 then
          begin
            // non-zero fraction - not a number (NaN)
            If (Mantissa and FLOAT64_MASK_FHB) = 0 then
              begin
                // highest bit of fraction is zero - signaling NaN
                FXCSignalException(excInvalidOP);
                // no exception raised, return quiet signed NaN
                UInt32(F32Ptr^) := Sign or FLOAT32_MASK_EXP or FLOAT32_MASK_FHB or (Mantissa shr MAN_SHIFT);
              end
            // non-zero FHB - return quiet signed NaN with truncated mantissa
            else UInt32(F32Ptr^) := Sign or FLOAT32_MASK_EXP or FLOAT32_MASK_FHB or (Mantissa shr MAN_SHIFT);
          end
        // fraction of zero - renturn signed infinity
        else UInt32(F32Ptr^) := Sign or FLOAT32_MASK_EXP;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  // exponent 897..1150 (-126..127 unbiased) - representable normalized value
  Mantissa := ShiftMantissa(Sign,Mantissa and not FLOAT64_MASK_INTB,MAN_SHIFT,DataLoss,RoundingMode);
  // check and process potential mantissa overflow
  If (Mantissa and not UInt64(FLOAT32_MASK_FRAC)) <> 0 then
    begin
      Inc(Exponent);
      Overflow := True;
    end
  else Overflow := False;
  If Overflow and (Exponent > 1150) then
    // overflow to infinity
    FXCSignalException(excOverflow);
  // construct the resulting number
  UInt32(F32Ptr^) := Sign or
   {exponent}(UInt32(UInt32(Exponent - FXC_BIAS_DIFF) shl FLOAT32_SHIFT_EXP) and
   FLOAT32_MASK_EXP) or {fraction}(Mantissa and FLOAT32_MASK_FRAC);
  If DataLoss then
    // inexact result
    FXCSignalException(excPrecision);
end;
end;

//==============================================================================

procedure Float32ToFloat64_PAS_SSE(F32Ptr,F64Ptr: Pointer); register;
const
  SGN_SHIFT = FLOAT64_SHIFT_SIGN - FLOAT32_SHIFT_SIGN;  // 32, (l)shifting bit 63 to bit 31
  MAN_SHIFT = 29;                                       // creating 52 bit F64 fraction from 23 bit F32 fraction (lshift)
var
  Sign:           UInt32;   // unshifted (sign is in bit 31)
  Exponent:       Int32;    // biased exponent (bias 127)
  Mantissa:       UInt32;   // only fraction, without integer bit
  MantissaShift:  Integer;
begin
Sign := UInt32(F32Ptr^) and FLOAT32_MASK_SIGN;
Exponent := Int32((UInt32(F32Ptr^) and FLOAT32_MASK_EXP) shr FLOAT32_SHIFT_EXP);
Mantissa := UInt32(F32Ptr^) and FLOAT32_MASK_FRAC;
case Exponent of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // zero exponent - zero or denormal
  0:    If (Mantissa <> 0) and not FXCControlFlagGet(cfDenormalsAreZeros) then
          begin
            // non-zero mantissa (denormal) and DAZ mode inactive
            FXCSignalException(excDenormal);
          {
            Normalize

            Shift mantissa left so that its highest set bit will be shifted
            to integer bit (bit 52), also correct exponent to reflect this
            change.
          }
            MantissaShift := LeadZeroCount(Mantissa) + 21;
            UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or
              UInt64(UInt64(FXC_BIAS_DIFF + 30 - MantissaShift) shl FLOAT64_SHIFT_EXP) or
              (UInt64(UInt64(Mantissa) shl MantissaShift) and FLOAT64_MASK_FRAC);
          end
        // zero or DAZ mode, return signed zero
        else UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT);

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // max exponent - infinity or NaN
  $FF:  If Mantissa <> 0 then
          begin
            // not a number
            If (Mantissa and FLOAT32_MASK_FHB) = 0 then
              begin
                // signaled NaN
                FXCSignalException(excInvalidOp);
                // if no exception was raised, return quiet signed NaN with mantissa
                UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or FLOAT64_MASK_EXP or
                                   FLOAT64_MASK_FHB or UInt64(UInt64(Mantissa) shl MAN_SHIFT);
              end
            // quiet signed NaN with mantissa
            else UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or FLOAT64_MASK_EXP or
                                    UInt64(UInt64(Mantissa) shl MAN_SHIFT);
          end
        // signed infinity
        else UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or FLOAT64_MASK_EXP;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  // normal number
  UInt64(F64Ptr^) := UInt64(UInt64(Sign) shl SGN_SHIFT) or
    UInt64(UInt64(Exponent + FXC_BIAS_DIFF) shl FLOAT64_SHIFT_EXP) or
    UInt64(UInt64(Mantissa) shl MAN_SHIFT);
end;
end;

//------------------------------------------------------------------------------

procedure Float64ToFloat32_PAS_SSE(F64Ptr,F32Ptr: Pointer); register;

  Function BitScanReverse(Value: UInt64): Integer;
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

const
{
  Default shift of mantissa - shifting bit 51 (highest bit of fraction in F64
  mantissa) to position 22 (highest bit of fraction in F32 mantissa).
}
  MAN_SHIFT = 29;
  // bits removed when right-shifting the mantissa
  MASK_REMB = not UInt64(UInt64(-1) shl MAN_SHIFT);
var
  RoundingMode:   TFXCRoundingMode;
  Sign:           UInt32; // sign is pre-shifted to bit 31
  Exponent:       Int32;  // biased exponent (bias 1023)
  Mantissa:       UInt64; // without integer bit
  HighestSetBit:  Integer;
  DataLoss:       Boolean;
  Temp:           UInt64;
  Overflow:       Boolean;
begin
RoundingMode := FXCRoundingModeGet;
// 32 - right-shifting bit 63 to bit 31
Sign := UInt32((UInt64(F64Ptr^) and FLOAT64_MASK_SIGN) shr 32);
Exponent := Int32((UInt64(F64Ptr^) and FLOAT64_MASK_EXP) shr FLOAT64_SHIFT_EXP);
Mantissa := UInt64(F64Ptr^) and FLOAT64_MASK_FRAC;
case Exponent of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // exponent of zero - zero or denormal
  0:    If (Mantissa <> 0) and not FXCControlFlagGet(cfDenormalsAreZeros) then
          begin
            // non-zero mantissa (denormal) and DAZ mode inactive
            FXCSignalException(excDenormal);
            If not FXCExceptionMaskGet(excUnderflow) then
              begin
                // underflow unmasked (ie. raised)
                HighestSetBit := BitScanReverse(Mantissa);  // zero-based index
              {
                Don't ask me about the magic numbers here, just accept them.
                Sadly, I cannot give you any meaningfull answer. >;/

                I see it this way (and I am probably wrong here, feel free to
                correct me):

                  If the mantissa is left-shifted so that its highest set bit
                  becomes integer bit (bit 52), and there is any set bit in
                  lowest 29 bits (these would be shifted-out with default right
                  shift), then precision exception is signaled.
              }
                If HighestSetBit >= 24 then
                  begin
                    If Mantissa and (UInt64(Int64(-1)) shr (87 - HighestSetBit)) <> 0 then
                      FXCSignalExceptions([excUnderflow,excPrecision])
                    else
                      FXCSignalException(excUnderflow);
                  end
                else FXCSignalException(excUnderflow);
              end
            // underflow masked, signal it along with precision
            else FXCSignalExceptions([excUnderflow,excPrecision]);
            If ((RoundingMode = rmUp) and (Sign = 0)) or
               ((RoundingMode = rmDown) and (Sign <> 0)) then
              begin
                If not FXCControlFlagGet(cfFlushToZero) then
                  // return signed smallest representable number (denormal)
                  UInt32(F32Ptr^) := Sign or UInt32(1)
                else
                  // flush to signed zero
                  UInt32(F32Ptr^) := Sign;
              end
              // convert to signed zero
            else UInt32(F32Ptr^) := Sign;
          end
        // mantissa of 0 or DAZ - return signed zero
        else UInt32(F32Ptr^) := Sign;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponent 1..872 (-1022..-151 unbiased) - exponent too small to be
  represented in single even as denormal.
}
  $1..
  $368: begin
          If Mantissa and MASK_REMB = 0 then
            FXCSignalException(excUnderflow);
          FXCSignalExceptions([excUnderflow,excPrecision]);
          If ((RoundingMode = rmUp) and (Sign = 0)) or
             ((RoundingMode = rmDown) and (Sign <> 0)) then
            begin
              If not FXCControlFlagGet(cfFlushToZero) then
              {
                FTZ mode is inactive, return signed smallest representable
                number (denormal).
              }
                UInt32(F32Ptr^) := Sign or UInt32(1)
              else
                // FTZ active, return signed zero
                UInt32(F32Ptr^) := Sign;
            end
          // convert to signed zero
          else UInt32(F32Ptr^) := Sign;
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponent 873..895 (-150..-128 unbiased) - exponent still too small to be
  represented in single, but can be denormalized to fit
}
  $369..
  $37F: begin
          If Mantissa and MASK_REMB <> 0 then
            begin
              If FXCExceptionMaskGet(excUnderflow) or not FXCExceptionMaskGet(excPrecision) then
                FXCSignalExceptions([excUnderflow,excPrecision])
              else
                FXCSignalException(excPrecision);
            end;
          // denormalize
          Temp := ShiftMantissa(Sign,Mantissa or FLOAT64_MASK_INTB,$39E - Exponent{shift 53..31},DataLoss,RoundingMode);
          If FXCExceptionMaskGet(excUnderflow) then
            begin
              If DataLoss then
                // inexact result
                FXCSignalExceptions([excUnderflow,excPrecision]);
            end
          else FXCSignalException(excUnderflow);
          If FXCControlFlagGet(cfFlushToZero) then
            begin
              // FTZ active, are we losing anything by flushing to zero?...
              If Temp <> 0 then
                FXCSignalExceptions([excUnderflow,excPrecision]);
              // return signed zero
              UInt32(F32Ptr^) := Sign;
            end
          else UInt32(F32Ptr^) := Sign or Temp;
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponent 896 (-127 unbiased) - similar to previous case, can yield normalized
  number thanks to rounding.
}
  $380: begin
        {
          Align bits in mantissa so it corresponds to float32 and look whether
          it overflowed thanks to rounding.
        }
          Temp := ShiftMantissa(Sign,Mantissa or FLOAT64_MASK_INTB,MAN_SHIFT,DataLoss,RoundingMode);
          If Temp <> UInt32(FLOAT32_MASK_INTB shl 1) then
            begin
              // mantissa has not overflowed, result is still a denormal
              If DataLoss then
                begin
                  If FXCExceptionMaskGet(excUnderflow) or not FXCExceptionMaskGet(excPrecision) then
                    FXCSignalExceptions([excUnderflow,excPrecision])
                  else
                    FXCSignalException(excPrecision);
                end;
              Temp := ShiftMantissa(Sign,Mantissa or FLOAT64_MASK_INTB,MAN_SHIFT + 1,DataLoss,RoundingMode);
              If FXCExceptionMaskGet(excUnderflow) then
                begin
                  If DataLoss then
                    FXCSignalExceptions([excUnderflow,excPrecision]);
                end
              else FXCSignalException(excUnderflow);
              If FXCControlFlagGet(cfFlushToZero) then
                begin
                  If Temp <> 0 then
                    FXCSignalExceptions([excUnderflow,excPrecision]);
                  UInt32(F32Ptr^) := Sign;
                end
              else UInt32(F32Ptr^) := Sign or Temp;
            end
          else
            begin
              // mantissa overflowed, result is promoted to a normalized number
              FXCSignalException(excPrecision);
              UInt32(F32Ptr^) := Sign or UInt32(UInt32(1) shl FLOAT32_SHIFT_EXP);
            end;
         end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
{
  Exponent 1151..2046 (128..1023 unbiased) - exponent too large to be
  represented in single (max. valid exponent 127).
}
  $47F..
  $7FE: begin
          If Mantissa and MASK_REMB = 0 then
            FXCSignalException(excOverflow);
          FXCSignalExceptions([excOverflow,excPrecision]);
          If (RoundingMode = rmTruncate) or
             ((RoundingMode = rmUp) and (Sign <> 0)) or
             ((RoundingMode = rmDown) and (Sign = 0)) then
            // return signed largest representable number (max. exp. - 1)
            UInt32(F32Ptr^) := Sign or UInt32(UInt32(not Int32(FLOAT32_MASK_SIGN)) and
                               UInt32(not(Int32(1) shl FLOAT32_SHIFT_EXP)))
          else
            // convert to signed infinity
            UInt32(F32Ptr^) := Sign or FLOAT32_MASK_EXP;
        end;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  // maximum exponent - NaN or infinity
  $7FF: If (Mantissa and FLOAT64_MASK_FRAC) <> 0 then
          begin
            // non-zero fraction - not a number (NaN)
            If (Mantissa and FLOAT64_MASK_FHB) = 0 then
              begin
                // highest bit of fraction is zero - signaling NaN
                FXCSignalException(excInvalidOP);
                // no exception raised, return quiet signed NaN
                UInt32(F32Ptr^) := Sign or FLOAT32_MASK_EXP or FLOAT32_MASK_FHB or (Mantissa shr MAN_SHIFT);
              end
            // non-zero FHB - return quiet signed NaN with truncated mantissa
            else UInt32(F32Ptr^) := Sign or FLOAT32_MASK_EXP or FLOAT32_MASK_FHB or (Mantissa shr MAN_SHIFT);
          end
        // fraction of zero - renturn signed infinity
        else UInt32(F32Ptr^) := Sign or FLOAT32_MASK_EXP;

  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  // exponent 897..1150 (-126..127 unbiased) - representable normalized value
  Mantissa := ShiftMantissa(Sign,Mantissa and not FLOAT64_MASK_INTB,MAN_SHIFT,DataLoss,RoundingMode);
  // check and process potential mantissa overflow
  If (Mantissa and not UInt64(FLOAT32_MASK_FRAC)) <> 0 then
    begin
      Inc(Exponent);
      Overflow := True;
    end
  else Overflow := False;
  If Overflow and (Exponent > 1150) then
    begin
      // overflow to infinity
      If DataLoss then
        // also an inexact result
        FXCSignalExceptions([excOverflow,excPrecision])
      else
        FXCSignalException(excOverflow);
    end
  else If DataLoss then
    // inexact result without overflow
    FXCSignalException(excPrecision);
  // construct the resulting number
  UInt32(F32Ptr^) := Sign or
   {exponent}(UInt32(UInt32(Exponent - FXC_BIAS_DIFF) shl FLOAT32_SHIFT_EXP) and
   FLOAT32_MASK_EXP) or {fraction}UInt32(Mantissa and FLOAT32_MASK_FRAC);
end;
end;

//==============================================================================
var
  VAR_Float32ToFloat64: procedure(F32Ptr,F64Ptr: Pointer); register = Float32ToFloat64_PAS_X87;
  VAR_Float64ToFloat32: procedure(F64Ptr,F32Ptr: Pointer); register = Float64ToFloat32_PAS_X87;

{-------------------------------------------------------------------------------
    FXC - public functions implementation
-------------------------------------------------------------------------------}

procedure Float32ToFloat64(Float32Ptr,Float64Ptr: Pointer);
begin
VAR_Float32ToFloat64(Float32Ptr,Float64Ptr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float32ToFloat64(Value: Float32): Float64;
begin
VAR_Float32ToFloat64(@Value,@Result);
end;

//------------------------------------------------------------------------------

procedure SingleToDouble(SinglePtr,DoublePtr: Pointer);
begin
VAR_Float32ToFloat64(SinglePtr,DoublePtr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SingleToDouble(Value: Single): Double;
begin
VAR_Float32ToFloat64(@Value,@Result);
end;

//==============================================================================

procedure Float64ToFloat32(Float64Ptr,Float32Ptr: Pointer);
begin
VAR_Float64ToFloat32(Float64Ptr,Float32Ptr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float64ToFloat32(Value: Float64): Float32;
begin
VAR_Float64ToFloat32(@Value,@Result);
end;

//------------------------------------------------------------------------------

procedure DoubletoSingle(DoublePtr,SinglePtr: Pointer);
begin
VAR_Float64ToFloat32(DoublePtr,SinglePtr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DoubletoSingle(Value: Double): Single;
begin
VAR_Float64ToFloat32(@Value,@Result);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              State synchronization
--------------------------------------------------------------------------------
===============================================================================}
type
  TFUX87StateEnvironmentImage = packed record
    CW,p1:  UInt16;   // control word, padding
    SW,p2:  UInt16;   // status word, padding
    TW,p3:  UInt16;   // tag word, padding
    FIP:    UInt32;   // instruction pointer offset
    FCS:    UInt16;   // instruction pointer selector
    FOP:    UInt16;   // last instruction opcode
    FDP:    UInt32;   // data pointer offset
    FDS,p4: UInt16;   // data pointer selector, padding
  end; {should be 28 bytes in size}

const
  // masks for assignment/comparison of compatible targets
  SYNC_X87SW_MASK_COMP = UInt16(
      X87SW_EFLAG_All or
      X87SW_ConditionCode_C1 or
      X87SW_FPUBusy or
      X87SW_ExceptionSummary or
      X87SW_StackFault);
  SYNC_X87CW_MASK_COMP = UInt16(
      X87CW_EMASK_ALL or
      X87CW_Precision or
      X87CW_Rounding);

  SYNC_MXCSR_MASK_COMP = UInt32(
      MXCSR_EFLAG_All or
      MXCSR_EMASK_All or
      MXCSR_Rounding or
      MXCSR_DenormalsAreZeros or
      MXCSR_FlushToZero);

{
  Masks for assignment/comparison of incompatible targets (only bits and fields
  common to both x87 control and status words and MXCSR register).
}
  SYNC_X87SW_MASK_INCOMP = UInt16(
      X87SW_EFLAG_All);
  SYNC_X87CW_MASK_INCOMP = UInt16(
      X87CW_EMASK_ALL or
      X87CW_Rounding);

  SYNC_MXCSR_MASK_INCOMP = UInt32(
      MXCSR_EFLAG_All or
      MXCSR_EMASK_All or
      MXCSR_Rounding);

{===============================================================================
    State synchronization - implementation
===============================================================================}

Function StateSyncResolveTarget(SyncTarget: TFUStateSyncTarget; FinalTarget: Boolean = False): TFUStateSyncTarget;

  Function TargetIfThen(Condition: Boolean; OnTrue,OnFalse: TFUStateSyncTarget): TFUStateSyncTarget;
  begin
    If Condition then
      Result := OnTrue
    else
      Result := OnFalse;
  end;

begin
case SyncTarget of
  sstF80CState:     Result := TargetIfThen(F80CEmulated,sstF80CEmulated,TargetIfThen(FinalTarget,sstX87,sstF80CNative));
  sstF16CState:     Result := TargetIfThen(F16CEmulated,sstF16CEmulated,TargetIfThen(FinalTarget,sstAVX,sstF16CNative));
  sstFXCState:      case FXCModeOfOperation of
                      modAssemblyX87: Result := TargetIfThen(FinalTarget,sstX87,sstFXCNativeX87);
                      modPascalSSE:   Result := sstFXCEmulatedSSE;
                      modAssemblySSE: Result := TargetIfThen(FinalTarget,sstSSE,sstFXCNativeSSE);
                    else
                     {modPascalX87}
                      Result := sstFXCEmulatedX87;
                    end;
  sstF80CCntrState: Result := TargetIfThen(not F80CEmulated,sstF80CEmulated,TargetIfThen(FinalTarget,sstX87,sstF80CNative));
  sstF16CCntrState: Result := TargetIfThen(not F16CEmulated,sstF16CEmulated,TargetIfThen(FinalTarget,sstAVX,sstF16CNative));
  sstFXCCntrState:  case FXCModeOfOperation of
                      modAssemblyX87: Result := sstFXCEmulatedX87;
                      modPascalSSE:   Result := TargetIfThen(FinalTarget,sstSSE,sstFXCNativeSSE);
                      modAssemblySSE: Result := sstFXCEmulatedSSE;
                    else
                     {modPascalX87}
                      Result := TargetIfThen(FinalTarget,sstX87,sstFXCNativeX87);
                    end;
  sstF80CNative,
  sstFXCNativeX87:  Result := TargetIfThen(FinalTarget,sstX87,SyncTarget);
  sstF16CNative:    Result := TargetIfThen(FinalTarget,sstAVX,SyncTarget);
  sstFXCNativeSSE:  Result := TargetIfThen(FinalTarget,sstSSE,SyncTarget);
else
  Result := SyncTarget;
end;
end;

//------------------------------------------------------------------------------

Function StateSyncCompatibleTargets(SyncTargetA,SyncTargetB: TFUStateSyncTarget): Boolean;
const
  CompGroupA = [sstF80CEmulated,sstF80CNative,sstFXCEmulatedX87,sstFXCNativeX87,sstX87];
  CompGroupB = [sstF16CEmulated,sstF16CNative,sstFXCEmulatedSSE,sstFXCNativeSSE,sstSSE,sstAVX];
begin
SyncTargetA := StateSyncResolveTarget(SyncTargetA);
SyncTargetB := StateSyncResolveTarget(SyncTargetB);
If SyncTargetA <> SyncTargetB then
  Result := ([SyncTargetA,SyncTargetB] <= CompGroupA) or ([SyncTargetA,SyncTargetB] <= CompGroupB)
else
  Result := True;
end;

//------------------------------------------------------------------------------

Function StateSyncDistinctTargets(SyncTargetA,SyncTargetB: TFUStateSyncTarget): Boolean;
begin
SyncTargetA := StateSyncResolveTarget(SyncTargetA,True);
SyncTargetB := StateSyncResolveTarget(SyncTargetB,True);
{
  Technically SSE and AVX states are the same - both these vector units are
  using MXCSR register for their control and status.
}
Result := (SyncTargetA <> SyncTargetB) and not([SyncTargetA,SyncTargetB] <= [sstSSE,sstAVX]);
end;

//==============================================================================

procedure StateSyncSave(SyncTarget: TFUStateSyncTarget; out Buffer: TFUStateSyncBuffer);

  procedure EncodeBuffer(StatusWord,ControlWord: UInt16); overload;
  begin
    Buffer.PayloadType := bptX87;
    Buffer.StatusWord := StatusWord;
    Buffer.ControlWord := ControlWord;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  procedure EncodeBuffer(ControlAndStatus: UInt32); overload;
  begin
    Buffer.PayloadType := bptMXCSR;
    Buffer.ControlAndStatus := ControlAndStatus;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
var
  X87StateEnvImage: TFUX87StateEnvironmentImage;
begin
SyncTarget := StateSyncResolveTarget(SyncTarget);
Buffer.SourceTarget := SyncTarget;
case SyncTarget of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF80CEmulated:    with F80CGetStatePtr^ do
                        EncodeBuffer(StatusWord,ControlWord);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF16CEmulated:    EncodeBuffer(F16CGetStatePtr^.ControlAndStatus);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstFXCEmulatedX87:  with FXCGetStatePtr^ do
                        EncodeBuffer(StatusWord,ControlWord);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstFXCEmulatedSSE:  EncodeBuffer(FXCGetStatePtr^.ControlAndStatus);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF80CNative,
  sstFXCNativeX87,
  sstX87:             begin
                        X87SaveEnvironment(@X87StateEnvImage);
                        EncodeBuffer(X87StateEnvImage.SW,X87StateEnvImage.CW);
                      end;
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF16CNative,
  sstFXCNativeSSE,
  sstSSE,sstAVX:      EncodeBuffer(SSEControlAndStatusGet);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  raise EFUInvalidValue.CreateFmt('StateSyncSave: Unknown synchronization target (%d).',[Ord(SyncTarget)]);
end;
end;

//------------------------------------------------------------------------------

procedure StateSyncLoad(SyncTarget: TFUStateSyncTarget; const Buffer: TFUStateSyncBuffer);

  procedure DecodeBuffer(var StatusWord,ControlWord: UInt16); overload;
  begin
    case Buffer.PayloadType of
      bptMXCSR: begin
        StatusWord := (StatusWord and not SYNC_X87SW_MASK_INCOMP) or
          UInt16(Buffer.ControlAndStatus and MXCSR_EFLAG_ALL);
        ControlWord := (ControlWord and not SYNC_X87CW_MASK_INCOMP) or
          UInt16((Buffer.ControlAndStatus and MXCSR_EMASK_ALL) shr 7) or
          UInt16((Buffer.ControlAndStatus and MXCSR_Rounding) shr 3);
      end;
    else
     {bptX87}
      StatusWord := (StatusWord and not SYNC_X87SW_MASK_COMP) or
        (Buffer.StatusWord and SYNC_X87SW_MASK_COMP);
      ControlWord := (ControlWord and not SYNC_X87CW_MASK_COMP) or
        (Buffer.ControlWord and SYNC_X87CW_MASK_COMP);
    end;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  procedure DecodeBuffer(var ControlAndStatus: UInt32); overload;
  begin
    case Buffer.PayloadType of
      bptMXCSR:
        ControlAndStatus := (ControlAndStatus and not SYNC_MXCSR_MASK_COMP) or
          (Buffer.ControlAndStatus and SYNC_MXCSR_MASK_COMP);
    else
     {bptX87}
      ControlAndStatus := (ControlAndStatus and not SYNC_MXCSR_MASK_INCOMP) or
        UInt32(Buffer.StatusWord and X87SW_EFLAG_ALL) or
        UInt32((Buffer.ControlWord and X87CW_EMASK_ALL) shl 7) or
        UInt32((Buffer.ControlWord and X87CW_Rounding) shl 3)
    end;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
var
  X87StateEnvImage: TFUX87StateEnvironmentImage;
  ControlAndStatus: UInt32;
begin
SyncTarget := StateSyncResolveTarget(SyncTarget);
case SyncTarget of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF80CEmulated:    with F80CGetStatePtr^ do
                        DecodeBuffer(StatusWord,ControlWord);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF16CEmulated:    DecodeBuffer(F16CGetStatePtr^.ControlAndStatus);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstFXCEmulatedX87:  with FXCGetStatePtr^ do
                        DecodeBuffer(StatusWord,ControlWord);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstFXCEmulatedSSE:  DecodeBuffer(FXCGetStatePtr^.ControlAndStatus);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF80CNative,
  sstFXCNativeX87,
  sstX87:             begin
                        X87SaveEnvironment(@X87StateEnvImage);
                        DecodeBuffer(X87StateEnvImage.SW,X87StateEnvImage.CW);
                        X87LoadEnvironment(@X87StateEnvImage);
                      end;
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF16CNative,
  sstFXCNativeSSE,
  sstSSE,sstAVX:      begin
                        ControlAndStatus := SSEControlAndStatusGet;
                        DecodeBuffer(ControlAndStatus);
                        SSEControlAndStatusSet(ControlAndStatus);
                      end;
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  raise EFUInvalidValue.CreateFmt('StateSyncLoad: Unknown synchronization target (%d).',[Ord(SyncTarget)]);
end;
end;

//------------------------------------------------------------------------------

Function StateSyncCompare(SyncTarget: TFUStateSyncTarget; const Buffer: TFUStateSyncBuffer): Boolean;

  Function CompareBuffer(StatusWord,ControlWord: UInt16): Boolean; overload;
  begin
    case Buffer.PayloadType of
      bptMXCSR: begin
        Result :=
          ((StatusWord and SYNC_X87SW_MASK_INCOMP) =
           UInt16(Buffer.ControlAndStatus and MXCSR_EFLAG_All)) and
         ((ControlWord and SYNC_X87CW_MASK_INCOMP) =
          (UInt16((Buffer.ControlAndStatus and MXCSR_EMASK_ALL) shr 7) or
           UInt16((Buffer.ControlAndStatus and MXCSR_Rounding) shr 3)));
      end;
    else
     {bptX87}
      Result :=
        ((StatusWord and SYNC_X87SW_MASK_COMP) =
         (Buffer.StatusWord and SYNC_X87SW_MASK_COMP)) and
        ((ControlWord and SYNC_X87CW_MASK_COMP) =
         (Buffer.ControlWord and SYNC_X87CW_MASK_COMP));
    end;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Function CompareBuffer(ControlAndStatus: UInt32): Boolean; overload;
  begin
    case Buffer.PayloadType of
      bptMXCSR:
        Result :=
          (ControlAndStatus and SYNC_MXCSR_MASK_COMP) =
          (Buffer.ControlAndStatus and SYNC_MXCSR_MASK_COMP)
    else
     {bptX87}
      Result := (ControlAndStatus and SYNC_MXCSR_MASK_INCOMP) =
        (UInt32(Buffer.StatusWord and X87SW_EFLAG_ALL) or
         UInt32((Buffer.ControlWord and X87CW_EMASK_ALL) shl 7) or
         UInt32((Buffer.ControlWord and X87CW_Rounding) shl 3));
    end;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
var
  X87StateEnvImage: TFUX87StateEnvironmentImage;
begin
SyncTarget := StateSyncResolveTarget(SyncTarget);
case SyncTarget of
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF80CEmulated:    with F80CGetStatePtr^ do
                        Result := CompareBuffer(StatusWord,ControlWord);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF16CEmulated:    Result := CompareBuffer(F16CGetStatePtr^.ControlAndStatus);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstFXCEmulatedX87:  with FXCGetStatePtr^ do
                        Result := CompareBuffer(StatusWord,ControlWord);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstFXCEmulatedSSE:  Result := CompareBuffer(FXCGetStatePtr^.ControlAndStatus);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF80CNative,
  sstFXCNativeX87,
  sstX87:             begin
                        X87SaveEnvironment(@X87StateEnvImage);
                        Result := CompareBuffer(X87StateEnvImage.SW,X87StateEnvImage.CW);
                      end;
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
  sstF16CNative,
  sstFXCNativeSSE,
  sstSSE,sstAVX:      Result := CompareBuffer(SSEControlAndStatusGet);
  //--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
else
  raise EFUInvalidValue.CreateFmt('StateSyncCompare: Unknown synchronization target (%d).',[Ord(SyncTarget)]);
end;
end;

//==============================================================================

Function StateSynchronize(SyncDestination: TFUStateSyncTarget; SyncSource: TFUStateSyncTarget; SyncAction: TFUStateSyncAction): Boolean;
var
  StateSyncBuffer:  TFUStateSyncBuffer;
begin
case SyncAction of
  ssaCompare,
  ssaCompareStrict:   begin
    StateSyncSave(SyncSource,StateSyncBuffer);
    Result := StateSyncCompare(SyncDestination,StateSyncBuffer);
    If not Result and (SyncAction = ssaCompareStrict) then
      raise EFUStateMismatch.Create('SyncDestination: States do not match.');
  end;
  ssaCopyToCurrent:   begin
    StateSyncSave(SyncSource,StateSyncBuffer);
    StateSyncLoad(SyncDestination,StateSyncBuffer);
    Result := True;
  end;
  ssaCopyFromCurrent: begin
    // swap source and destination
    StateSyncSave(SyncDestination,StateSyncBuffer);
    StateSyncLoad(SyncSource,StateSyncBuffer);
    Result := True;
  end;
else
  raise EFUInvalidValue.CreateFmt('StateSynchronize: Unknown synchronization action (%d).',[Ord(SyncAction)]);
end;
end;

{-------------------------------------------------------------------------------
    State synchronization - F80C implementation
-------------------------------------------------------------------------------}

Function F80CStateSynchronize(SyncAction: TFUStateSyncAction): Boolean;
begin
Result := StateSynchronize(sstF80CState,sstF80CCntrState,SyncAction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function F80CStateSynchronize(SyncSource: TFUStateSyncTarget; SyncAction: TFUStateSyncAction): Boolean;
begin
Result := StateSynchronize(sstF80CState,SyncSource,SyncAction);
end;

{-------------------------------------------------------------------------------
    State synchronization - F16C implementation
-------------------------------------------------------------------------------}

Function F16CStateSynchronize(SyncAction: TFUStateSyncAction): Boolean;
begin
Result := StateSynchronize(sstF16CState,sstF16CCntrState,SyncAction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function F16CStateSynchronize(SyncSource: TFUStateSyncTarget; SyncAction: TFUStateSyncAction): Boolean;
begin
Result := StateSynchronize(sstF16CState,SyncSource,SyncAction);
end;

{-------------------------------------------------------------------------------
    State synchronization - FXC implementation
-------------------------------------------------------------------------------}

Function FXCStateSynchronize(SyncAction: TFUStateSyncAction): Boolean;
begin
Result := StateSynchronize(sstFXCState,sstFXCCntrState,SyncAction);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function FXCStateSynchronize(SyncSource: TFUStateSyncTarget; SyncAction: TFUStateSyncAction): Boolean;
begin
Result := StateSynchronize(sstFXCState,SyncSource,SyncAction);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Floats mapping
--------------------------------------------------------------------------------
===============================================================================}

procedure MapToFloat16Buffer(Value: UInt16; out Buffer);
begin
UInt16(Buffer) := Value;
end;

//------------------------------------------------------------------------------

Function MapToFloat16(Value: UInt16): Float16;
begin
MapToFloat16Buffer(Value,Result);
end;

//------------------------------------------------------------------------------

Function MapToHalf(Value: UInt16): Half;
begin
MapToFloat16Buffer(Value,Result);
end;

//------------------------------------------------------------------------------

Function MapFromFloat16Buffer(const Buffer): UInt16;
begin
Result := UInt16(Buffer);
end;

//------------------------------------------------------------------------------

Function MapFromFloat16(const Value: Float16): UInt16;
begin
Result := MapFromFloat16Buffer(Value);
end;

//------------------------------------------------------------------------------

Function MapFromHalf(const Value: Half): UInt16;
begin
Result := MapFromFloat16Buffer(Value);
end;

//==============================================================================

procedure MapToFloat32Buffer(Value: UInt32; out Buffer);
begin
UInt32(Buffer) := Value;
end;

//------------------------------------------------------------------------------

Function MapToFloat32(Value: UInt32): Float32;
begin
MapToFloat32Buffer(Value,Result);
end;

//------------------------------------------------------------------------------

Function MapToSingle(Value: UInt32): Single;
begin
MapToFloat32Buffer(Value,Result);
end;

//------------------------------------------------------------------------------

Function MapFromFloat32Buffer(const Buffer): UInt32;
begin
Result := UInt32(Buffer);
end;

//------------------------------------------------------------------------------

Function MapFromFloat32(const Value: Float32): UInt32;
begin
Result := MapFromFloat32Buffer(Value);
end;

//------------------------------------------------------------------------------

Function MapFromSingle(const Value: Single): UInt32;
begin
Result := MapFromFloat32Buffer(Value);
end;

//==============================================================================

procedure MapToFloat64Buffer(Value: UInt64; out Buffer);
begin
UInt64(Buffer) := Value;
end;

//------------------------------------------------------------------------------

Function MapToFloat64(Value: UInt64): Float64;
begin
MapToFloat64Buffer(Value,Result);
end;

//------------------------------------------------------------------------------

Function MapToDouble(Value: UInt64): Double;
begin
MapToFloat64Buffer(Value,Result);
end;

//------------------------------------------------------------------------------

Function MapFromFloat64Buffer(const Buffer): UInt64;
begin
Result := UInt64(Buffer);
end;

//------------------------------------------------------------------------------

Function MapFromFloat64(const Value: Float64): UInt64;
begin
Result := MapFromFloat64Buffer(Value);
end;

//------------------------------------------------------------------------------

Function MapFromDouble(const Value: Double): UInt64;
begin
Result := MapFromFloat64Buffer(Value);
end;

//==============================================================================

procedure MapToFloat80Buffer(High16: UInt16; Low64: UInt64; out Buffer);
var
  Overlay:  TFloat80Overlay absolute Buffer;
begin
Overlay.Part16 := High16;
Overlay.Part64 := Low64;
end;

//------------------------------------------------------------------------------

Function MapToFloat80(High16: UInt16; Low64: UInt64): Float80;
begin
MapToFloat80Buffer(High16,Low64,Result);
end;

//------------------------------------------------------------------------------

Function MapToExtended(High16: UInt16; Low64: UInt64): Extended;
{$IF SizeOf(Extended) = 10}
begin
MapToFloat80Buffer(High16,Low64,Result);
{$ELSE}
var
  Float80Value: Float80;
begin
MapToFloat80Buffer(High16,Low64,Float80Value);
Float80ToFloat64(@Float80Value,@Result);
{$IFEND}
end;

//------------------------------------------------------------------------------

procedure MapFromFloat80Buffer(const Buffer; out High16: UInt16; out Low64: UInt64);
var
  Overlay:  TFloat80Overlay absolute Buffer;
begin
High16 := Overlay.Part16;
Low64 := Overlay.Part64;
end;

//------------------------------------------------------------------------------

procedure MapFromFloat80(const Value: Float80; out High16: UInt16; out Low64: UInt64);
begin
MapFromFloat80Buffer(Value,High16,Low64);
end;

//------------------------------------------------------------------------------

procedure MapFromExtended(const Value: Extended; out High16: UInt16; out Low64: UInt64);
{$IF SizeOf(Extended) = 10}
begin
MapFromFloat80Buffer(Value,High16,Low64);
{$ELSE}
var
  Float80Value: Float80;
begin
Float64ToFloat80(@Value,@Float80Value);
MapFromFloat80Buffer(Float80Value,High16,Low64);
{$IFEND}
end;


{===============================================================================
--------------------------------------------------------------------------------
                          Floats encoding and decoding
--------------------------------------------------------------------------------
===============================================================================}

Function ClampValue(Value,Low,High: Integer): Integer;
begin
{
  There are no sanity checks (eg. Low <= High) because this function is
  strictly internal.
}
If Value < Low then
  Result := Low
else If Value > High then
  Result := High
else
  Result := Value;
end;

//==============================================================================

procedure EncodeFloat16Buffer(Mantissa: UInt16; Exponent: Integer; Sign: Boolean; out Buffer; Options: TFUTranscodeOptions = []);
begin
{
  Store fraction of mantissa, ignore other bits, including integer bit (it is
  implicit in float16).
}
UInt16(Buffer) := Mantissa and FLOAT16_MASK_FRAC;
// prepare value of exponent
If optExponentBias in Options then
  // given exponent is already biased
  Exponent := ClampValue(Exponent,FLOAT16_BEXPONENTMIN,FLOAT16_BEXPONENTMAX)
else
  // unbiased exponent
  Exponent := ClampValue(Exponent,FLOAT16_EXPONENTMIN,FLOAT16_EXPONENTMAX) + FLOAT16_EXPONENTBIAS;
// store the exponent
UInt16(Buffer) := UInt16(Buffer) or (UInt16(UInt16(Exponent) shl FLOAT16_SHIFT_EXP) and FLOAT16_MASK_EXP);
// set sign
If Sign then
  UInt16(Buffer) := UInt16(Buffer) or FLOAT16_MASK_SIGN;
end;

//------------------------------------------------------------------------------

Function EncodeFloat16(Mantissa: UInt16; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Float16;
begin
EncodeFloat16Buffer(Mantissa,Exponent,Sign,Result,Options);
end;

//------------------------------------------------------------------------------

Function EncodeHalf(Mantissa: UInt16; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Half;
begin
EncodeFloat16Buffer(Mantissa,Exponent,Sign,Result,Options);
end;

//------------------------------------------------------------------------------

procedure DecodeFloat16Buffer(const Buffer; out Mantissa: UInt16; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
// get mantissa (its fraction) and, if asked for, include mantissa integer bit
If (optIntegerBit in Options) and ((UInt16(Buffer) and FLOAT16_MASK_EXP) <> 0{not a zero or denormal}) then
  Mantissa := (UInt16(Buffer) and FLOAT16_MASK_FRAC) or FLOAT16_MASK_INTB
else
  Mantissa := UInt16(Buffer) and FLOAT16_MASK_FRAC;
// extract exponent, unbias it if biased exponent is not requested
Exponent := Integer((UInt16(Buffer) and FLOAT16_MASK_EXP) shr FLOAT16_SHIFT_EXP);
If not (optExponentBias in Options) then
  Exponent := Exponent - FLOAT16_EXPONENTBIAS;
// finally get sign
Sign := (UInt16(Buffer) and FLOAT16_MASK_SIGN) <> 0;
end;

//------------------------------------------------------------------------------

procedure DecodeFloat16(const Value: Float16; out Mantissa: UInt16; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
DecodeFloat16Buffer(Value,Mantissa,Exponent,Sign,Options);
end;

//------------------------------------------------------------------------------

procedure DecodeHalf(const Value: Half; out Mantissa: UInt16; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
DecodeFloat16Buffer(Value,Mantissa,Exponent,Sign,Options);
end;

//==============================================================================

procedure EncodeFloat32Buffer(Mantissa: UInt32; Exponent: Integer; Sign: Boolean; out Buffer; Options: TFUTranscodeOptions = []);
begin
UInt32(Buffer) := Mantissa and FLOAT32_MASK_FRAC;
If optExponentBias in Options then
  Exponent := ClampValue(Exponent,FLOAT32_BEXPONENTMIN,FLOAT32_BEXPONENTMAX)
else
  Exponent := ClampValue(Exponent,FLOAT32_EXPONENTMIN,FLOAT32_EXPONENTMAX) + FLOAT32_EXPONENTBIAS;
UInt32(Buffer) := UInt32(Buffer) or (UInt32(UInt32(Exponent) shl FLOAT32_SHIFT_EXP) and FLOAT32_MASK_EXP);
If Sign then
  UInt32(Buffer) := UInt32(Buffer) or FLOAT32_MASK_SIGN;
end;

//------------------------------------------------------------------------------

Function EncodeFloat32(Mantissa: UInt32; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Float32;
begin
EncodeFloat32Buffer(Mantissa,Exponent,Sign,Result,Options);
end;

//------------------------------------------------------------------------------

Function EncodeSingle(Mantissa: UInt32; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Single;
begin
EncodeFloat32Buffer(Mantissa,Exponent,Sign,Result,Options);
end;

//------------------------------------------------------------------------------

procedure DecodeFloat32Buffer(const Buffer; out Mantissa: UInt32; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
If (optIntegerBit in Options) and ((UInt32(Buffer) and FLOAT32_MASK_EXP) <> 0) then
  Mantissa := (UInt32(Buffer) and FLOAT32_MASK_FRAC) or FLOAT32_MASK_INTB
else
  Mantissa := UInt32(Buffer) and FLOAT32_MASK_FRAC;
Exponent := Integer((UInt32(Buffer) and FLOAT32_MASK_EXP) shr FLOAT32_SHIFT_EXP);
If not (optExponentBias in Options) then
  Exponent := Exponent - FLOAT32_EXPONENTBIAS;
Sign := (UInt32(Buffer) and FLOAT32_MASK_SIGN) <> 0;
end;

//------------------------------------------------------------------------------

procedure DecodeFloat32(const Value: Float32; out Mantissa: UInt32; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
DecodeFloat32Buffer(Value,Mantissa,Exponent,Sign,Options);
end;

//------------------------------------------------------------------------------

procedure DecodeSingle(const Value: Single; out Mantissa: UInt32; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
DecodeFloat32Buffer(Value,Mantissa,Exponent,Sign,Options);
end;

//==============================================================================

procedure EncodeFloat64Buffer(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; out Buffer; Options: TFUTranscodeOptions = []);
begin
UInt64(Buffer) := Mantissa and FLOAT64_MASK_FRAC;
If optExponentBias in Options then
  Exponent := ClampValue(Exponent,FLOAT64_BEXPONENTMIN,FLOAT64_BEXPONENTMAX)
else
  Exponent := ClampValue(Exponent,FLOAT64_EXPONENTMIN,FLOAT64_EXPONENTMAX) + FLOAT64_EXPONENTBIAS;
UInt64(Buffer) := UInt64(Buffer) or (UInt64(UInt64(Exponent) shl FLOAT64_SHIFT_EXP) and FLOAT64_MASK_EXP);
If Sign then
  UInt64(Buffer) := UInt64(Buffer) or FLOAT64_MASK_SIGN;
end;

//------------------------------------------------------------------------------

Function EncodeFloat64(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Float64;
begin
EncodeFloat64Buffer(Mantissa,Exponent,Sign,Result,Options);
end;

//------------------------------------------------------------------------------

Function EncodeDouble(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Double;
begin
EncodeFloat64Buffer(Mantissa,Exponent,Sign,Result,Options);
end;

//------------------------------------------------------------------------------

procedure DecodeFloat64Buffer(const Buffer; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
If (optIntegerBit in Options) and ((UInt64(Buffer) and FLOAT64_MASK_EXP) <> 0) then
  Mantissa := (UInt64(Buffer) and FLOAT64_MASK_FRAC) or FLOAT64_MASK_INTB
else
  Mantissa := UInt64(Buffer) and FLOAT64_MASK_FRAC;
Exponent := Integer((UInt64(Buffer) and FLOAT64_MASK_EXP) shr FLOAT64_SHIFT_EXP);
If not (optExponentBias in Options) then
  Exponent := Exponent - FLOAT64_EXPONENTBIAS;
Sign := (UInt64(Buffer) and FLOAT64_MASK_SIGN) <> 0;
end;

//------------------------------------------------------------------------------

procedure DecodeFloat64(const Value: Float64; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
DecodeFloat64Buffer(Value,Mantissa,Exponent,Sign,Options);
end;

//------------------------------------------------------------------------------

procedure DecodeDouble(const Value: Double; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
DecodeFloat64Buffer(Value,Mantissa,Exponent,Sign,Options);
end;

//==============================================================================

procedure EncodeFloat80Buffer(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; out Buffer; Options: TFUTranscodeOptions = []);
var
  Overlay:  TFLoat80Overlay absolute Buffer;
begin
// we will need the expoment when storing mantissa, so pre-calculate it
If optExponentBias in Options then
  Exponent := ClampValue(Exponent,FLOAT80_BEXPONENTMIN,FLOAT80_BEXPONENTMAX)
else
  Exponent := ClampValue(Exponent,FLOAT80_EXPONENTMIN,FLOAT80_EXPONENTMAX) + FLOAT80_EXPONENTBIAS;
If not (optIntegerBit in Options) then
  begin
    // imply integer bit from exponent
    If Exponent = 0 then
      // biased exponent is zero, integer bit must be also zero
      Overlay.Mantissa := Mantissa and not FLOAT80_MASK64_INTB
    else
      // biased exp. is non-zero, integer bit must be 1
      Overlay.Mantissa := Mantissa or FLOAT80_MASK64_INTB;
  end
// integer bit is explicitly stored in provided mantissa
else Overlay.Mantissa := Mantissa;
// store pre-calculated exponent
Overlay.SignExponent := UInt16(Exponent) and FLOAT80_MASK16_EXP;
// set sign
If Sign then
  Overlay.SignExponent := Overlay.SignExponent or FLOAT80_MASK16_SIGN;
end;

//------------------------------------------------------------------------------

Function EncodeFloat80(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Float80;
begin
EncodeFloat80Buffer(Mantissa,Exponent,Sign,Result,Options);
end;

//------------------------------------------------------------------------------

Function EncodeExtended(Mantissa: UInt64; Exponent: Integer; Sign: Boolean; Options: TFUTranscodeOptions = []): Extended;
{$IF SizeOf(Extended) = 10}
begin
EncodeFloat80Buffer(Mantissa,Exponent,Sign,Result,Options);
{$ELSE}
var
  Float80Value: Float80;
begin
EncodeFloat80Buffer(Mantissa,Exponent,Sign,Float80Value,Options);
Float80ToFloat64(@Float80Value,@Result);
{$IFEND}
end;

//------------------------------------------------------------------------------

procedure DecodeFloat80Buffer(const Buffer; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
var
  Overlay:  TFLoat80Overlay absolute Buffer;
begin
{
  If integer bit is requested, just copy the entire mantissa as it explicitly
  contains it. If not, then mask it and leave it always empty.
}
If optIntegerBit in Options then
  Mantissa := Overlay.Mantissa
else
  Mantissa := Overlay.Mantissa and not FLOAT80_MASK64_INTB;
// get exponent
Exponent := Integer(Overlay.SignExponent and FLOAT80_MASK16_EXP);
If not (optExponentBias in Options) then
  Exponent := Exponent - FLOAT80_EXPONENTBIAS;
// get sign
Sign := (Overlay.SignExponent and FLOAT80_MASK16_SIGN) <> 0;
end;

//------------------------------------------------------------------------------

procedure DecodeFloat80(const Value: Float80; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
begin
DecodeFloat80Buffer(Value,Mantissa,Exponent,Sign,Options);
end;

//------------------------------------------------------------------------------

procedure DecodeExtended(const Value: Extended; out Mantissa: UInt64; out Exponent: Integer; out Sign: Boolean; Options: TFUTranscodeOptions = []);
{$IF SizeOf(Extended) = 10}
begin
DecodeFloat80Buffer(Value,Mantissa,Exponent,Sign,Options);
{$ELSE}
var
  Float80Value: Float80;
begin
Float64ToFloat80(@Value,@Float80Value);
DecodeFloat80Buffer(Float80Value,Mantissa,Exponent,Sign,Options);
{$IFEND}
end;


{===============================================================================
--------------------------------------------------------------------------------
                             Number metainformation
--------------------------------------------------------------------------------
===============================================================================}
const
  // because FPC cannot grasp unsigned 32bit constants with highest bit set...
  FLOAT32_MASK_NSIGN = UInt32($7FFFFFFF);

//------------------------------------------------------------------------------

Function IsZero(const Value: Float16): Boolean;
var
  Overlay:  UInt16 absolute Value;
begin
// bits other than sign are zero
Result := (Overlay and not FLOAT16_MASK_SIGN) = 0;
end;

//------------------------------------------------------------------------------

Function IsDenormal(const Value: Float16): Boolean;
var
  Overlay:  UInt16 absolute Value;
begin
// zero exponent, non-zero fraction, sign ignored
Result := ((Overlay and FLOAT16_MASK_EXP) = 0) and ((Overlay and FLOAT16_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsNaN(const Value: Float16): Boolean;
var
  Overlay:  UInt16 absolute Value;
begin
// max exponent and non-zero fraction, sign ignored
Result := ((Overlay and FLOAT16_MASK_EXP) = FLOAT16_MASK_EXP) and ((Overlay and FLOAT16_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsIndefinite(const Value: Float16): Boolean;
var
  Overlay:  UInt16 absolute Value;
begin
{
  sign must be 1, exponent must be at maximum allowable value, fraction must
  have highest bit set and other bits must be clear
}
Result := ((Overlay and FLOAT16_MASK_SIGN) <> 0) and
          ((Overlay and FLOAT16_MASK_EXP) = FLOAT16_MASK_EXP) and
          ((Overlay and FLOAT16_MASK_FRAC) = FLOAT16_MASK_FHB);
end;

//------------------------------------------------------------------------------

Function IsInfinite(const Value: Float16): Boolean;
var
  Overlay:  UInt16 absolute Value;
begin
// max exponent and zero fraction, sign ignored
Result := ((Overlay and FLOAT16_MASK_EXP) = FLOAT16_MASK_EXP) and ((Overlay and FLOAT16_MASK_FRAC) = 0);
end;

//------------------------------------------------------------------------------

Function IsNormal(const Value: Float16): Boolean;
var
  Overlay:  UInt16 absolute Value;
  Exponent: Integer;
begin
// non-zero less than max exponent, any fraction, sign ignored
Exponent := Integer((Overlay and FLOAT16_MASK_EXP) shr FLOAT16_SHIFT_EXP);
Result := (Exponent > FLOAT16_BEXPONENTMIN) and (Exponent < FLOAT16_BEXPONENTMAX);
end;

//==============================================================================

Function IsZero(const Value: Float32): Boolean;
var
  Overlay:  UInt32 absolute Value;
begin
Result := (Overlay and FLOAT32_MASK_NSIGN) = 0;
end;

//------------------------------------------------------------------------------

Function IsDenormal(const Value: Float32): Boolean;
var
  Overlay:  UInt32 absolute Value;
begin
Result := ((Overlay and FLOAT32_MASK_EXP) = 0) and ((Overlay and FLOAT32_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsNaN(const Value: Float32): Boolean;
var
  Overlay:  UInt32 absolute Value;
begin
Result := ((Overlay and FLOAT32_MASK_EXP) = FLOAT32_MASK_EXP) and ((Overlay and FLOAT32_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsIndefinite(const Value: Float32): Boolean;
var
  Overlay:  UInt32 absolute Value;
begin
Result := ((Overlay and FLOAT32_MASK_SIGN) <> 0) and
          ((Overlay and FLOAT32_MASK_EXP) = FLOAT32_MASK_EXP) and
          ((Overlay and FLOAT32_MASK_FRAC) = FLOAT32_MASK_FHB);
end;

//------------------------------------------------------------------------------

Function IsInfinite(const Value: Float32): Boolean;
var
  Overlay:  UInt32 absolute Value;
begin
Result := ((Overlay and FLOAT32_MASK_EXP) = FLOAT32_MASK_EXP) and ((Overlay and FLOAT32_MASK_FRAC) = 0);
end;

//------------------------------------------------------------------------------

Function IsNormal(const Value: Float32): Boolean;
var
  Overlay:  UInt32 absolute Value;
  Exponent: Integer;
begin
Exponent := Integer((Overlay and FLOAT32_MASK_EXP) shr FLOAT32_SHIFT_EXP);
Result := (Exponent > FLOAT32_BEXPONENTMIN) and (Exponent < FLOAT32_BEXPONENTMAX);
end;

//==============================================================================

Function IsZero(const Value: Float64): Boolean;
var
  Overlay:  UInt64 absolute Value;
begin
Result := (Overlay and not FLOAT64_MASK_SIGN) = 0;
end;

//------------------------------------------------------------------------------

Function IsDenormal(const Value: Float64): Boolean;
var
  Overlay:  UInt64 absolute Value;
begin
Result := ((Overlay and FLOAT64_MASK_EXP) = 0) and ((Overlay and FLOAT64_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsNaN(const Value: Float64): Boolean;
var
  Overlay:  UInt64 absolute Value;
begin
Result := ((Overlay and FLOAT64_MASK_EXP) = FLOAT64_MASK_EXP) and ((Overlay and FLOAT64_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsIndefinite(const Value: Float64): Boolean;
var
  Overlay:  UInt64 absolute Value;
begin
Result := ((Overlay and FLOAT64_MASK_SIGN) <> 0) and
          ((Overlay and FLOAT64_MASK_EXP) = FLOAT64_MASK_EXP) and
          ((Overlay and FLOAT64_MASK_FRAC) = FLOAT64_MASK_FHB);
end;

//------------------------------------------------------------------------------

Function IsInfinite(const Value: Float64): Boolean;
var
  Overlay:  UInt64 absolute Value;
begin
Result := ((Overlay and FLOAT64_MASK_EXP) = FLOAT64_MASK_EXP) and ((Overlay and FLOAT64_MASK_FRAC) = 0);
end;

//------------------------------------------------------------------------------

Function IsNormal(const Value: Float64): Boolean;
var
  Overlay:  UInt64 absolute Value;
  Exponent: Integer;
begin
Exponent := Integer((Overlay and FLOAT64_MASK_EXP) shr FLOAT64_SHIFT_EXP);
Result := (Exponent > FLOAT64_BEXPONENTMIN) and (Exponent < FLOAT64_BEXPONENTMAX);
end;

//==============================================================================

Function IsZero(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
// zero exponent, zero mantissa
Result := ((Overlay.SignExponent and FLOAT80_MASK16_EXP) = 0) and (Overlay.Mantissa = 0);
end;

//------------------------------------------------------------------------------

Function IsDenormal(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
// zero exponent, integer bit 0, non-zero fraction, sign ignored
Result := ((Overlay.SignExponent and FLOAT80_MASK16_EXP) = 0) and
          ((Overlay.Mantissa and FLOAT80_MASK64_INTB) = 0) and
          ((Overlay.Mantissa and FLOAT80_MASK64_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsNaN(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
// max exponent, integer bit 1, non-zero fraction, sign ignored
Result := ((Overlay.SignExponent and FLOAT80_MASK16_EXP) = FLOAT80_MASK16_EXP) and
          ((Overlay.Mantissa and FLOAT80_MASK64_INTB) <> 0) and
          ((Overlay.Mantissa and FLOAT80_MASK64_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsIndefinite(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
{
  sign 1, max exponent, integer bit 1, fraction must have highest bit set (1)
  and other bits clear (0)
}
Result := (Overlay.SignExponent = (FLOAT80_MASK16_SIGN or FLOAT80_MASK16_EXP)) and
          (Overlay.Mantissa = (FLOAT80_MASK64_INTB or FLOAT80_MASK64_FHB));
end;

//------------------------------------------------------------------------------

Function IsInfinite(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
// max exponent, integer bit 1, zero fraction, sign ignored
Result := ((Overlay.SignExponent and FLOAT80_MASK16_EXP) = FLOAT80_MASK16_EXP) and
          (Overlay.Mantissa = FLOAT80_MASK64_INTB);
end;

//------------------------------------------------------------------------------

Function IsNormal(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
  Exponent: Integer;
begin
// non-zero less than max exponent, integer bit 1, any fraction, sign ignored
Exponent := Integer((Overlay.SignExponent and FLOAT80_MASK16_EXP) shr FLOAT80_SHIFT16_EXP);
Result := (Exponent > FLOAT80_BEXPONENTMIN) and (Exponent < FLOAT80_BEXPONENTMAX) and
          ((Overlay.Mantissa and FLOAT80_MASK64_INTB) <> 0);
end;

//------------------------------------------------------------------------------

Function IsValid(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
{
  Non-zero exponent with zero integer bit or zero exponent with non-zero
  integer bit are both unsuported encodings - so if exponent is zero, then
  mantissa integer bit must also be zero and vice-versa.

  Note that pseudo-denormals (zero exponent, non-zero integer bit) are also not
  valid, but can be processed by x87 without raising an InvalidOP exception
  (they are silently converted to usual denormals or zero).
}
Result := ((Overlay.SignExponent and FLOAT80_MASK16_EXP) = 0) = ((Overlay.Mantissa and FLOAT80_MASK64_INTB) = 0);
end;

//------------------------------------------------------------------------------

Function IsPseudoDenormal(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
// zero exponent, integer bit 1, non-zero mantissa, sign ignored
Result := ((Overlay.SignExponent and FLOAT80_MASK16_EXP) = 0) and
          ((Overlay.Mantissa and FLOAT80_MASK64_INTB) <> 0) and
          ((Overlay.Mantissa and FLOAT80_MASK64_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsPseudoNaN(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
// max exponent, integer bit 0, non-zero fraction, sign ignored
Result := ((Overlay.SignExponent and FLOAT80_MASK16_EXP) = FLOAT80_MASK16_EXP) and
          ((Overlay.Mantissa and FLOAT80_MASK64_INTB) = 0) and
          ((Overlay.Mantissa and FLOAT80_MASK64_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsPseudoInfinity(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
// max exponent, integer bit 0, zero fraction, sign ignored
Result := ((Overlay.SignExponent and FLOAT80_MASK16_EXP) = FLOAT80_MASK16_EXP) and (Overlay.Mantissa = 0);
end;

//------------------------------------------------------------------------------

Function IsUnnormal(const Value: Float80): Boolean;
var
  Overlay:  TFloat80Overlay absolute Value;
  Exponent: Integer;
begin
// non-zero less than max exponent, integer bit 0, any fraction, sign ignored
Exponent := Integer((Overlay.SignExponent and FLOAT80_MASK16_EXP) shr FLOAT80_SHIFT16_EXP);
Result := (Exponent > FLOAT80_BEXPONENTMIN) and (Exponent < FLOAT80_BEXPONENTMAX) and
          ((Overlay.Mantissa and FLOAT80_MASK64_INTB) = 0);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                Sign manipulation
--------------------------------------------------------------------------------
===============================================================================}

Function Sign(const Value: Float16): TFUValueSign;
var
  Overlay:  UInt16 absolute Value;
begin
If (Overlay and not FLOAT16_MASK_SIGN) <> 0 then
  begin
    If (Overlay and FLOAT16_MASK_SIGN) <> 0 then
      Result := -1
    else
      Result := 1;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

Function Abs(const Value: Float16): Float16;
var
  ValOverlay: UInt16 absolute Value;
  ResOverlay: UInt16 absolute Result;
begin
ResOverlay := ValOverlay and not FLOAT16_MASK_SIGN;
end;

//------------------------------------------------------------------------------

Function Neg(const Value: Float16): Float16;
var
  ValOverlay: UInt16 absolute Value;
  ResOverlay: UInt16 absolute Result;
begin
ResOverlay := ValOverlay xor FLOAT16_MASK_SIGN;
end;

//==============================================================================

Function Sign(const Value: Float32): TFUValueSign;
var
  Overlay:  UInt32 absolute Value;
begin
If (Overlay and FLOAT32_MASK_NSIGN) <> 0 then
  begin
    If (Overlay and FLOAT32_MASK_SIGN) <> 0 then
      Result := -1
    else
      Result := 1;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

Function Abs(const Value: Float32): Float32;
var
  ValOverlay: UInt32 absolute Value;
  ResOverlay: UInt32 absolute Result;
begin
ResOverlay := ValOverlay and FLOAT32_MASK_NSIGN;
end;

//------------------------------------------------------------------------------

Function Neg(const Value: Float32): Float32;
var
  ValOverlay: UInt32 absolute Value;
  ResOverlay: UInt32 absolute Result;
begin
ResOverlay := ValOverlay xor FLOAT32_MASK_SIGN;
end;

//==============================================================================

Function Sign(const Value: Float64): TFUValueSign;
var
  Overlay:  UInt64 absolute Value;
begin
If (Overlay and not FLOAT64_MASK_SIGN) <> 0 then
  begin
    If (Overlay and FLOAT64_MASK_SIGN) <> 0 then
      Result := -1
    else
      Result := 1;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

Function Abs(const Value: Float64): Float64;
var
  ValOverlay: UInt64 absolute Value;
  ResOverlay: UInt64 absolute Result;
begin
ResOverlay := ValOverlay and not FLOAT64_MASK_SIGN;
end;

//------------------------------------------------------------------------------

Function Neg(const Value: Float64): Float64;
var
  ValOverlay: UInt64 absolute Value;
  ResOverlay: UInt64 absolute Result;
begin
ResOverlay := ValOverlay xor FLOAT64_MASK_SIGN;
end;

//==============================================================================

Function Sign(const Value: Float80): TFUValueSign;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
If ((Overlay.SignExponent and not FLOAT80_MASK16_SIGN) <> 0) or (Overlay.Mantissa <> 0) then
  begin
    If (Overlay.SignExponent and FLOAT80_MASK16_SIGN) <> 0 then
      Result := -1
    else
      Result := 1;
  end
else Result := 0;

end;

//------------------------------------------------------------------------------

Function Abs(const Value: Float80): Float80;
var
  ValOverlay: TFloat80Overlay absolute Value;
  ResOverlay: TFloat80Overlay absolute Result;
begin
ResOverlay.SignExponent := ValOverlay.SignExponent and not FLOAT80_MASK16_SIGN;
ResOverlay.Mantissa := ValOverlay.Mantissa;
end;

//------------------------------------------------------------------------------

Function Neg(const Value: Float80): Float80;
var
  ValOverlay: TFloat80Overlay absolute Value;
  ResOverlay: TFloat80Overlay absolute Result;
begin
ResOverlay.SignExponent := ValOverlay.SignExponent xor FLOAT80_MASK16_SIGN;
ResOverlay.Mantissa := ValOverlay.Mantissa;
end;


{===============================================================================
--------------------------------------------------------------------------------
                         Unit implementation management
--------------------------------------------------------------------------------
===============================================================================}
var
  varImplManager: TImplementationManager = nil;

//------------------------------------------------------------------------------

Function UIM_FloatUtils_AvailableFuncImpl(Func: TUIM_FloatUtils_Function): TUIM_FloatUtils_Implementations;
var
  i:  Integer;
begin
If Func < fnX87FPUAccess then
  begin
    // individual functions
    Result := [];
    with varImplManager.RoutingFindObj(TUIMIdentifier(Func)) do
      For i := LowIndex to HighIndex do
        If ifAvailable in Implementations[i].ImplementationFlags then
          Include(Result,TUIM_FloatUtils_Implementation(Implementations[i].ImplementationID));
  end
// groups
else Result := UIM_FloatUtils_AvailableFuncImpl(TUIM_FloatUtils_Function(varImplManager.
       RoutingGroupFindObj(TUIMIdentifier(Func)).Representative(rsFirst,False).RoutingID));
end;

//------------------------------------------------------------------------------

Function UIM_FloatUtils_SupportedFuncImpl(Func: TUIM_FloatUtils_Function): TUIM_FloatUtils_Implementations;
var
  i:  Integer;
begin
If Func < fnX87FPUAccess then
  begin
    Result := [];
    with varImplManager.RoutingFindObj(TUIMIdentifier(Func)) do
      For i := LowIndex to HighIndex do
        // testing whether the two are subset of flags, meaning both must be there
        If [ifAvailable,ifSupported] <= Implementations[i].ImplementationFlags then
          Include(Result,TUIM_FloatUtils_Implementation(Implementations[i].ImplementationID));
  end
else Result := UIM_FloatUtils_SupportedFuncImpl(TUIM_FloatUtils_Function(varImplManager.
       RoutingGroupFindObj(TUIMIdentifier(Func)).Representative(rsFirst,False).RoutingID));
end;

//------------------------------------------------------------------------------

Function UIM_FloatUtils_GetFuncImpl(Func: TUIM_FloatUtils_Function; StrictGroupCheck: Boolean = False): TUIM_FloatUtils_Implementation;
begin
If Func < fnX87FPUAccess then
  Result := TUIM_FloatUtils_Implementation(varImplManager.RoutingFindObj(TUIMIdentifier(Func)).Selected)
else
  Result := UIM_FloatUtils_GetFuncImpl(TUIM_FloatUtils_Function(varImplManager.
    RoutingGroupFindObj(TUIMIdentifier(Func)).Representative(rsFirst,StrictGroupCheck).RoutingID));
end;

//------------------------------------------------------------------------------

Function UIM_FloatUtils_SetFuncImpl(Func: TUIM_FloatUtils_Function; NewImpl: TUIM_FloatUtils_Implementation; StrictGroupCheck: Boolean = False): TUIM_FloatUtils_Implementation;
begin
Result := UIM_FloatUtils_GetFuncImpl(Func,StrictGroupCheck);
If Func >= fnX87FPUAccess then
  begin
    varImplManager.RoutingGroupFindObj(TUIMIdentifier(Func)).Select(TUIMIdentifier(NewImpl));
    case Func of
      fnFloat80Conversions: VAR_F80CEmulated := NewImpl <> imAssembly;
      fnFloat16Conversions: VAR_F16CEmulated := NewImpl <> imAssembly;
      fnFloatXConversions:  case NewImpl of
                              imPascalSSE:    VAR_FXCModeOfOperation := modPascalSSE;
                              imAssembly,
                              imAssemblyX87:  VAR_FXCModeOfOperation := modAssemblyX87;
                              imAssemblySSE:  VAR_FXCModeOfOperation := modAssemblySSE;
                            else
                             {imNone,imPascal,imPascalX87}
                              VAR_FXCModeOfOperation := modPascalX87;
                            end;
    end;
  end
else varImplManager.RoutingFindObj(TUIMIdentifier(Func)).Select(TUIMIdentifier(NewImpl));
end;


{===============================================================================
--------------------------------------------------------------------------------
                               Unit initialization
--------------------------------------------------------------------------------
===============================================================================}

procedure UnitInitialize;
const
  NilPtr: Pointer = nil;
{$IFNDEF PurePascal}
type
  TUIMSuppGrp = (sgX87,sgVec,sgF16C,sgSSE2);
var
  Support: array[TUIMSuppGrp] of Boolean;
{$ENDIF}
begin
varImplManager := TImplementationManager.Create;
// init indicator variables
VAR_F80CEmulated := True;
VAR_F16CEmulated := True;
{$IFNDEF PurePascal}
// discern what is supported on current system
with TSimpleCPUID.Create do
try
  Support[sgX87]  := Info.SupportedExtensions.X87 and not Info.SupportedExtensions.EmulatedX87;
  Support[sgVec]  := Info.SupportedExtensions.SSE {applies to all SSE versions} or
                     Info.SupportedExtensions.AVX {also applies to AVX2 and AVX extensions} or
                     Info.SupportedExtensions.AVX512.Supported or
                     Info.SupportedExtensions.AVX10.Supported;
  Support[sgF16C] := Info.SupportedExtensions.F16C;
  Support[sgSSE2] := Info.SupportedExtensions.SSE2;
finally
  Free;
end;
{$ENDIF}
// fill the routing list and select default implementations (pascal/pascalX87)
varImplManager.RoutingGroupBegin(TUIMIdentifier(fnX87FPUAccess));
AddRouting(varImplManager,TUIMIdentifier(fnX87StatusWordGet),@VAR_X87StatusWordGet,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@X87StatusWordGet_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@X87StatusWordGet_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@X87StatusWordGet_ASM,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnX87ControlWordGet),@VAR_X87ControlWordGet,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@X87ControlWordGet_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@X87ControlWordGet_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@X87ControlWordGet_ASM,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnX87ControlWordSet),@VAR_X87ControlWordSet,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@X87ControlWordSet_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@X87ControlWordSet_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@X87ControlWordSet_ASM,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnX87EnvironmentInit),@VAR_X87EnvironmentInit,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@X87EnvironmentInit_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@X87EnvironmentInit_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@X87EnvironmentInit_ASM,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnX87FloatDataGet),@VAR_X87FloatDataGet,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@X87FloatDataGet_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@X87FloatDataGet_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@X87FloatDataGet_ASM,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnX87ExceptionsClear),@VAR_X87ExceptionsClear,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@X87ExceptionsClear_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@X87ExceptionsClear_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@X87ExceptionsClear_ASM,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnX87ExceptionsRaise),@VAR_X87ExceptionsRaise,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@X87ExceptionsRaise_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@X87ExceptionsRaise_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@X87ExceptionsRaise_ASM,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnX87SaveEnvironment),@VAR_X87SaveEnvironment,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@X87SaveEnvironment_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@X87SaveEnvironment_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@X87SaveEnvironment_ASM,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnX87LoadEnvironment),@VAR_X87LoadEnvironment,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@X87LoadEnvironment_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@X87LoadEnvironment_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@X87LoadEnvironment_ASM,Support[sgX87]){$ENDIF}],1);

varImplManager.RoutingGroupBegin(TUIMIdentifier(fnFloat80Conversions));
AddRouting(varImplManager,TUIMIdentifier(fnFloat64ToFloat80),@VAR_Float64ToFloat80,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@Float64ToFloat80_PAS),
  ImplInfo(TUIMIdentifier(imPascalX87),@Float64ToFloat80_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@Float64ToFloat80_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@Float64ToFloat80_ASM,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnFloat80ToFloat64),@VAR_Float80ToFloat64,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@Float80ToFloat64_PAS),
  ImplInfo(TUIMIdentifier(imPascalX87),@Float80ToFloat64_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@Float80ToFloat64_ASM,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblyX87),@Float80ToFloat64_ASM,Support[sgX87]){$ENDIF}],1);

varImplManager.RoutingGroupBegin(TUIMIdentifier(fnVECAccess));
AddRouting(varImplManager,TUIMIdentifier(fnVECControlAndStatusGet),@VAR_VECControlAndStatusGet,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@VECControlAndStatusGet_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@VECControlAndStatusGet_ASM,Support[sgVec]),
  ImplInfo(TUIMIdentifier(imAssemblySSE),@VECControlAndStatusGet_ASM,Support[sgVec]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnVECControlAndStatusSet),@VAR_VECControlAndStatusSet,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@VECControlAndStatusSet_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@VECControlAndStatusSet_ASM,Support[sgVec]),
  ImplInfo(TUIMIdentifier(imAssemblySSE),@VECControlAndStatusSet_ASM,Support[sgVec]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnVECFloatDataGet),@VAR_VECFloatDataGet,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@VECFloatDataGet_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@VECFloatDataGet_ASM,Support[sgVec]),
  ImplInfo(TUIMIdentifier(imAssemblySSE),@VECFloatDataGet_ASM,Support[sgVec]){$ENDIF}],1);

varImplManager.RoutingGroupBegin(TUIMIdentifier(fnFloat16Conversions));
AddRouting(varImplManager,TUIMIdentifier(fnFloat16ToFloat32),@VAR_Float16ToFloat32,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@Float16ToFloat32_PAS),
  ImplInfo(TUIMIdentifier(imPascalSSE),@Float16ToFloat32_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@Float16ToFloat32_ASM,Support[sgF16C]),
  ImplInfo(TUIMIdentifier(imAssemblySSE),@Float16ToFloat32_ASM,Support[sgF16C]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnFloat32ToFloat16),@VAR_Float32ToFloat16,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@Float32ToFloat16_PAS),
  ImplInfo(TUIMIdentifier(imPascalSSE),@Float32ToFloat16_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@Float32ToFloat16_ASM,Support[sgF16C]),
  ImplInfo(TUIMIdentifier(imAssemblySSE),@Float32ToFloat16_ASM,Support[sgF16C]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnFloat16ToFloat32Vec4),@VAR_Float16ToFloat32Vec4,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@Float16ToFloat32Vec4_PAS),
  ImplInfo(TUIMIdentifier(imPascalSSE),@Float16ToFloat32Vec4_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@Float16ToFloat32Vec4_ASM,Support[sgF16C]),
  ImplInfo(TUIMIdentifier(imAssemblySSE),@Float16ToFloat32Vec4_ASM,Support[sgF16C]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnFloat32ToFloat16Vec4),@VAR_Float32ToFloat16Vec4,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascal),@Float32ToFloat16Vec4_PAS),
  ImplInfo(TUIMIdentifier(imPascalSSE),@Float32ToFloat16Vec4_PAS){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssembly),@Float32ToFloat16Vec4_ASM,Support[sgF16C]),
  ImplInfo(TUIMIdentifier(imAssemblySSE),@Float32ToFloat16Vec4_ASM,Support[sgF16C]){$ENDIF}],1);

varImplManager.RoutingGroupBegin(TUIMIdentifier(fnFloatXConversions));
AddRouting(varImplManager,TUIMIdentifier(fnFloat32ToFloat64),@VAR_Float32ToFloat64,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascalX87),@Float32ToFloat64_PAS_X87),
  ImplInfo(TUIMIdentifier(imPascalSSE),@Float32ToFloat64_PAS_SSE),
  ImplInfo(TUIMIdentifier(imPascal),@Float32ToFloat64_PAS_X87){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssemblyX87),@Float32ToFloat64_ASM_X87,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblySSE),@Float32ToFloat64_ASM_SSE,Support[sgSSE2]),
  ImplInfo(TUIMIdentifier(imAssembly),@Float32ToFloat64_ASM_X87,Support[sgX87]){$ENDIF}],1);
AddRouting(varImplManager,TUIMIdentifier(fnFloat64ToFloat32),@VAR_Float64ToFloat32,[
  ImplInfo(TUIMIdentifier(imNone),NilPtr),
  ImplInfo(TUIMIdentifier(imPascalX87),@Float64ToFloat32_PAS_X87),
  ImplInfo(TUIMIdentifier(imPascalSSE),@Float64ToFloat32_PAS_SSE),
  ImplInfo(TUIMIdentifier(imPascal),@Float64ToFloat32_PAS_X87){$IFNDEF PurePascal},
  ImplInfo(TUIMIdentifier(imAssemblyX87),@Float64ToFloat32_ASM_X87,Support[sgX87]),
  ImplInfo(TUIMIdentifier(imAssemblySSE),@Float64ToFloat32_ASM_SSE,Support[sgSSE2]),
  ImplInfo(TUIMIdentifier(imAssembly),@Float64ToFloat32_ASM_X87,Support[sgX87]){$ENDIF}],1);
varImplManager.RoutingGroupEnd;
{$IFNDEF PurePascal}
// following is compiled only in non-pure-pacal mode
{
  Select assembly implementations where available and supported.

  UIM_FloatUtils_SetFuncImpl is called with group enums to ensure that the
  implementation is set the same for entire routine groups.

  Global variables indicating emulation (indicators) are also managed within
  calls to UIM_FloatUtils_SetFuncImpl.
}
If imAssembly in UIM_FloatUtils_SupportedFuncImpl(fnX87FPUAccess) then
  UIM_FloatUtils_SetFuncImpl(fnX87FPUAccess,imAssembly);
{$IF SizeOf(Extended) = 10}
{
  Route float80 conversions to x87 FPU only when we can assume it was properly
  initialized - ie. when type Extended is 80 bits wide, meaning compiler has to
  use x87.
}
If imAssembly in UIM_FloatUtils_SupportedFuncImpl(fnFloat80Conversions) then
  UIM_FloatUtils_SetFuncImpl(fnFloat80Conversions,imAssembly);
{$IFEND}
If imAssembly in UIM_FloatUtils_SupportedFuncImpl(fnVECAccess) then
  begin
    ControlAndStatusMaskInit;
    UIM_FloatUtils_SetFuncImpl(fnVECAccess,imAssembly);
  end;
If imAssembly in UIM_FloatUtils_SupportedFuncImpl(fnFloat16Conversions) then
  UIM_FloatUtils_SetFuncImpl(fnFloat16Conversions,imAssembly);
// for fnFloatXConversions, imAssembly corresponds to imAssemblyX87
If imAssembly in UIM_FloatUtils_SupportedFuncImpl(fnFloatXConversions) then
  UIM_FloatUtils_SetFuncImpl(fnFloatXConversions,imAssembly);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure UnitFinalize;
begin
FreeAndNil(varImplManager);
end;

//==============================================================================

initialization
  UnitInitialize;

finalization
  UnitFinalize;

end.