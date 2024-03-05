{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Float16Utils

    Main purpose of this library is to provide routines for conversion from and
    to half precision (16bit) floating point numbers (Single -> Half, Half ->
    Single).
    It also provides functions for basic arithmetic and comparison, as well as
    overloaded operators when compiled using FPC. But note that these functions
    only converts arguments given as halfs into single-precision (32bit) floats
    and operates on them.
    
    F16C instruction extension (for x86(-64) CPUs) is used when symbol
    AllowF16CExtension is defined, PurePascal is not defined, and when (and
    only when) it is supported by the CPU and OS.

    Implemented Half conforms to IEEE 754-2008, meaning it has one sign bit
    (value is negative when sign bit is set, positive otherwise), 5 bits of
    biased exponent (exponent bias is 15) and 11 bit mantissa (10 bits
    explicitly stored, highest bit is assumed to be zero for denormal numbers
    and zero, one otherwise)

      NOTE - type Half is declared in unit AuxTypes, not here.

  Version 1.1.4 (2023-04-15)

  Last change 2024-03-05

  ©2017-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Float16

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    BasicUIM    - github.com/TheLazyTomcat/Lib.BasicUIM
  * SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID

  SimpleCPUID is required only when AllowF16CExtension symbol is defined and
  PurePascal symbol is not defined.

===============================================================================}
unit Float16Utils;
{
  Float16Utils_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$IFDEF Float16Utils_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IFDEF ENDIAN_BIG}
  {$MESSAGE FATAL 'Big-endian architecture not supported'}
{$ENDIF}

{$IFDEF FPC}
  {$MODE ObjFPC}{$MODESWITCH CLASSICPROCVARS+}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
    {$DEFINE ASMSuppressSizeWarnings}
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

{
  AllowF16CExtension

  When defined, allows the use of F16C extension in ASM. The extension is used
  only when both CPU and OS supports it, otherwise pascal implementation is
  called instead.
  Has no meaning when PurePascal symbol is defined.

  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol Float16Utils_AllowF16CExtension_Off.
}
{$DEFINE AllowF16CExtension}
{$IFDEF Float16Utils_AllowF16CExtension_Off}
  {$UNDEF AllowF16CExtension}
{$ENDIF}

//------------------------------------------------------------------------------

// do not touch following...
{$IF not Defined(PurePascal) and Defined(AllowF16CExtension)}
  {$DEFINE F16U_ASM_IMPL}
{$IFEND}

interface

uses
  SysUtils,
  AuxTypes {contains declaration of type Half};

{-------------------------------------------------------------------------------
    Some predefined Half values and other useful constants
-------------------------------------------------------------------------------}
const
  Infinity: Half = ($00,$7C); // positive infinity
  NaN:      Half = ($00,$7E); // quiet NaN
  MaxHalf:  Half = ($FF,$7B); // 65504
  MinHalf:  Half = ($01,$00); // 5.96046e-8
  PlusOne:  Half = ($00,$3C); // +1.0
  MinusOne: Half = ($00,$BC); // -1.0
  One:      Half = ($00,$3C); // +1.0
  Zero:     Half = ($00,$00); // (+)0

  FLOAT16_EXPONENTBIAS = 15;
  FLOAT32_EXPONENTBIAS = 127;

{===============================================================================
    Library-specific exceptions - declaration
===============================================================================}
type
  EF16UException = class(Exception);

  EF16UInvalidFlag      = class(EF16UException);
  EF16UUnknownFunction  = class(EF16UException);
  EF16UNoImplementation = class(EF16UException);

{-------------------------------------------------------------------------------
    Library-specific exceptions - floating-point exceptions
-------------------------------------------------------------------------------}
{
  When this exception (and its descendants) is created by calling a constructor
  that does not end with "NoClear", and when the MXCSR register is currently
  emulated, all exception flag bits will be cleared.
  When created using "NoClear" constructor, no exception flag bit is changed.
}
type
  EF16UFPUException = class(EF16UException)
  protected
    fExceptionFlags:  UInt32;
    Function DefaultMessage: String; virtual; abstract;
  public
    constructor CreateNoClear(const Msg: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    constructor Create(const Msg: String);
    constructor CreateDefMsgNoClear({$IFNDEF FPC}Dummy: Integer = 0{$ENDIF});
    constructor CreateDefMsg;
    // ExceptionFlags holds state of exception flags before this exception was created
    property ExceptionFlags: UInt32 read fExceptionFlags;
  end;

{-------------------------------------------------------------------------------
    Library-specific exceptions - individual floating-point exception classes
-------------------------------------------------------------------------------}
type
  EF16UInvalidOp = class(EF16UFPUException) // invalid operation/operand
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UDenormal = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UDivByZero = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UOverflow = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UUnderflow = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF16UPrecision = class(EF16UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

{-------------------------------------------------------------------------------
================================================================================
                               Auxiliary routines
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Auxiliary routines - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary routines - SSE status and control register (MXCSR) access
-------------------------------------------------------------------------------}
// some constants for MXCSR
const
  MXCSR_EFLAG_InvalidOP = UInt32($00000001);
  MXCSR_EFLAG_Denormal  = UInt32($00000002);
  MXCSR_EFLAG_DivByZero = UInt32($00000004);
  MXCSR_EFLAG_Overflow  = UInt32($00000008);
  MXCSR_EFLAG_Underflow = UInt32($00000010);
  MXCSR_EFLAG_Precision = UInt32($00000020);

  MXCSR_EMASK_InvalidOP = UInt32($00000080);
  MXCSR_EMASK_Denormal  = UInt32($00000100);
  MXCSR_EMASK_DivByZero = UInt32($00000200);
  MXCSR_EMASK_Overflow  = UInt32($00000400);
  MXCSR_EMASK_Underflow = UInt32($00000800);
  MXCSR_EMASK_Precision = UInt32($00001000);

  MXCSR_DenormalsAreZeros = UInt32($00000040);
  MXCSR_FlushToZero       = UInt32($00008000);

  MXCSR_Rounding = UInt32($00006000); // bits 13..14

  MXCSR_SHIFT_Rounding = 13;

{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Low-level access
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}
{
  GetMXCSR

  Returns current value of MXCSR register.
}
Function GetMXCSR: UInt32;

{
  SetMXCSR

  Sets MXCSR register to a passed value.
}
procedure SetMXCSR(NewValue: UInt32);

{
  EmulatedMXCSR

  Returns false when a real MXCSR register is used, true when operating on an
  emulated local implementation.
}
Function EmulatedMXCSR: Boolean;

{
  Sets MXCSR register to $00001900 - denormal, underflow and precision
  exceptions are masked (others are unmasked), rounding is set to nearest,
  DAZ and FTZ bits are cleared.

  Call this routine only when MXCSR is NOT emulated (ie. a real CPU register is
  used) and the program is compiled so that SSE is not used as a primary mean
  of floating point arithmetics and/or is not automatically initialized (if the
  MXCSR equals to $00001F80 - a default value - you can safely assume it was
  not properly initialized).

  WARNING - the initialization must be done in each execution thread.
}
procedure InitMXCSR;{$IFDEF CanInline} inline;{$ENDIF}

{
  GetMXCSRMask

  Returns a bitmask used when reading and writing the MXCSR register. Zeroes
  are marking reserved bits, ones are marking used bits.

  This value is only informative, the masking is done automatically in calls to
  functions GetMXCSR and SetMXCSR.
}
Function GetMXCSRMask: UInt32;

{
  GetMXCSRSupportsDAZ

  Returns true when DAZ bit, and therefore denormals-are-zeros mode, is
  supported by the used implementation of MXCSR (be it true SSE register or
  an emulation). False when not supported.
}
Function GetMXCSRSupportsDAZ: Boolean;

{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Abstracted access
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}

type
  TSSERoundingMode = (rmNearest,rmDown,rmUp,rmTruncate);

  TSSEException = (excInvalidOp,excDenormal,excDivByZero,excOverflow,
                   excUnderflow,excPrecision);

  TSSEExceptions = set of TSSEException;

  TSSEFlag = (flDenormalsAreZeros,flFlushToZero);

  TSSEFlags = set of TSSEFlag;

const
  AllSSEExceptions = [excInvalidOp,excDenormal,excDivByZero,excOverflow,
                      excUnderflow,excPrecision];

//------------------------------------------------------------------------------
{
  GetSSERoundingMode

  Returns current value of rounding mode from MXCSR.
}
Function GetSSERoundingMode: TSSERoundingMode;

{
  SetSSERoundingMode

  Sets rounding mode to a selected NewValue and returns previous value of
  rounding mode.
}
Function SetSSERoundingMode(NewValue: TSSERoundingMode): TSSERoundingMode;

//------------------------------------------------------------------------------
{
  GetSSEExceptionMask

  Returns current value of selected exception mask bit.
}
Function GetSSEExceptionMask(SSEException: TSSEException): Boolean;

{
  SetSSEExceptionMask

  Sets value of selected exception mask bit in MXCSR to a NewValue and returns
  previous value of this bit.

  When the bit is set (true), the selected exception will be masked and not
  raised on its occurence.
  When clear (false), the exception is unmasked and can be raised.
}
Function SetSSEExceptionMask(SSEException: TSSEException; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetSSEExceptionMasks

  Returns status of all exception mask bits in MXCSR. When the bit is set, the
  exception is included in the result, when it is clear, the exception is
  excluded from the result.
}
Function GetSSEExceptionMasks: TSSEExceptions;

{
  SetSSEExceptionMasks

  Sets new value of all exception mask bits in MXCSR. If an exception is
  included in the NewValue, the mask bit will be set, when it is not included,
  the mask bit will be cleared.

  Returns previous state of all exception mask bits.
}
Function SetSSEExceptionMasks(NewValue: TSSEExceptions): TSSEExceptions;

//------------------------------------------------------------------------------
{
  GetSSEExceptionFlag

  Returns current value of selected exception flag bit.
}
Function GetSSEExceptionFlag(SSEException: TSSEException): Boolean;

{
  SetSSEExceptionFlag

  Sets value of selected exception flag bit in MXCSR to a NewValue and returns
  previous value of this bit.
}
Function SetSSEExceptionFlag(SSEException: TSSEException; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetSSEExceptionFlags

  Returns status of all exception flag bits in MXCSR. When the bit is set,
  the exception is included in the result, when it is clear, the exception is
  excluded from the result.
}
Function GetSSEExceptionFlags: TSSEExceptions;

{
  SetSSEExceptionFlags

  Sets new value of all exception flag bits in MXCSR. If an exception is
  included in the NewValue, the flag bit will be set, when it is not included,
  the flag bit will be cleared.

  Returns previous state of all exception flag bits.
}
Function SetSSEExceptionFlags(NewValue: TSSEExceptions): TSSEExceptions;

//------------------------------------------------------------------------------
{
  GetSSEFlag

  Returns current value of selected flag bit.
}
Function GetSSEFlag(Flag: TSSEFlag): Boolean;

{
  SetSSEFlag

  Sets value of selected flag bit in MXCSR to a NewValue and returns previous
  value of this bit.
}
Function SetSSEFlag(Flag: TSSEFlag; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetSSEFlags

  Returns status of all flag bits in MXCSR. When the bit is set, the flag is
  included in the result, when it is clear, the flag is excluded from the
  result.
}
Function GetSSEFlags: TSSEFlags;

{
  SetSSEFlags

  Sets new value of all flag bits in MXCSR. If a flag is included in the
  NewValue, the bit will be set, when it is not included, the bit will be
  cleared.

  Returns previous state of all flag bits.
}
procedure SetSSEFlags(NewValue: TSSEFlags);

//------------------------------------------------------------------------------
{
  ClearSSEExceptions

  Clears (sets to 0) lower 6 bits of MXCSR - that is, all exception flag bits.
}
procedure ClearSSEExceptions;{$IF Defined(CanInline) and not Defined(FPC)} inline;{$IFEND}

{
  RaiseSSEExceptions(MXCSR)

  Raises first encountered exception according to flags set in the passed MXCSR.

  Parameter Mask controls whether to honor exception masking (true) or not
  (false) when raising an exception (when honored, the masked exceptions are
  NOT raised, when not honored, all exceptions can be raised, even those
  masked).
  Mask bits are taken from the parameter MXCSR, not from the actual register.

  The exception flag bits are traversed one by one and, when a set bit is
  encountered, it is cleared and a corresponding exception is raised (if
  allowed by masking - see parameter Mask).
  Only one exception is raised in each call, even when multiple bits are set.
  The order in which the bits are traversed, and therefore the order of
  exception raising is:

    InvalidOP
    Denormal
    DivByZero
    Underflow
    Overflow
    Precision
}
procedure RaiseSSEExceptions(var MXCSR: UInt32; Mask: Boolean = True); overload;

{
  RaiseSSEExceptions

  Calls the first overload with an input being current value of MXCSR (be it
  real register or emulation).
  Note that MXCSR register is NOT affected by this function.
}
procedure RaiseSSEExceptions(Mask: Boolean = True); overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - conversion functions
-------------------------------------------------------------------------------}
{
  MapFloat16ToWord
  MapHalfToWord

  Directly maps type half (float16) to a 16bit unsigned integer - no convesion
  is done.
}
Function MapFloat16ToWord(Value: Float16): UInt16;
Function MapHalfToWord(Value: Half): UInt16;{$IFDEF CanInline} inline;{$ENDIF}

{
  MapWordToFloat16
  MapWordToHalf

  Directly maps 16bit unsigned integer to type half (float16) - no convesion
  is done.
}
Function MapWordToFloat16(Value: UInt16): Float16;
Function MapWordToHalf(Value: UInt16): Half;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure Float16ToFloat32(Float16Ptr,Float32Ptr: Pointer); overload;
procedure HalfToSingle(HalfPtr,SinglePtr: Pointer); overload;

procedure Float32ToFloat16(Float32Ptr,Float16Ptr: Pointer); overload;
procedure SingleToHalf(SinglePtr,HalfPtr: Pointer); overload;

Function Float16ToFloat32(Value: Float16): Float32; overload;
Function HalfToSingle(Value: Half): Single; overload;

Function Float32ToFloat16(Value: Float32): Float16; overload;
Function SingleToHalf(Value: Single): Half; overload;

//------------------------------------------------------------------------------
{
  Following functions are expecting pointers to packed vector of four singles
  (SinglePtr, Float32Ptr) and packed vector of four halfs (HalfPtr, Float16Ptr).
}
procedure Float16ToFloat32Vec4(Float16Ptr,Float32Ptr: Pointer);
procedure HalfToSingleVec4(HalfPtr,SinglePtr: Pointer);

procedure Float32ToFloat16Vec4(Float32Ptr,Float16Ptr: Pointer);
procedure SingleToHalfVec4(SinglePtr,HalfPtr: Pointer);

{-------------------------------------------------------------------------------
================================================================================
                               Number information
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Number information - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Number information - number class
-------------------------------------------------------------------------------}

Function IsZero(const Value: Half): Boolean;
Function IsDenormal(const Value: Half): Boolean;
Function IsNaN(const Value: Half): Boolean;
Function IsInfinite(const Value: Half): Boolean;
Function IsNormal(const Value: Half): Boolean;  // returns false on zero

{-------------------------------------------------------------------------------
    Number information - sign-related
-------------------------------------------------------------------------------}
type
  TValueSign = -1..1;

Function Sign(const Value: Half): TValueSign;
Function Abs(const Value: Half): Half;
Function Neg(const Value: Half): Half;

{-------------------------------------------------------------------------------
================================================================================
                              Comparison functions
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Comparison functions - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Comparison functions - basic comparison
-------------------------------------------------------------------------------}

Function IsEqual(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function IsLess(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function IsGreater(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function IsLessOrEqual(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function IsGreaterOrEqual(const A,B: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Comparison functions - ordered comparison
-------------------------------------------------------------------------------}
type
  TValueRelationship = -1..1; // to preven problems (because delphi vs. FPC)

Function CompareValue(const A,B: Half; Epsilon: Half): TValueRelationship;{$IF Defined(CanInline) and not Defined(FPC)} inline;{$IFEND} overload;
Function CompareValue(const A,B: Half): TValueRelationship;{$IF Defined(CanInline) and not Defined(FPC)} inline;{$IFEND} overload;

Function SameValue(const A,B: Half; Epsilon: Half): Boolean;{$IF Defined(CanInline) and not Defined(FPC)} inline;{$IFEND} overload;
Function SameValue(const A,B: Half): Boolean;{$IF Defined(CanInline) and not Defined(FPC)} inline;{$IFEND} overload;

{-------------------------------------------------------------------------------
================================================================================
                              Arithmetic functions
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Arithmetic functions - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Arithmetic functions - basic arithmetic
-------------------------------------------------------------------------------}

Function Add(const A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
Function Subtract(const A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
Function Multiply(const A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
Function Divide(const A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                              Floats encode/decode
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Floats encode/decode - declaration
===============================================================================}

procedure MapToFloat16Buffer(out Buffer; Value: UInt16);
Function MapToFloat16(Value: UInt16): Float16;{$IFDEF CanInline} inline;{$ENDIF}
Function MapToHalf(Value: UInt16): Half;{$IFDEF CanInline} inline;{$ENDIF}

Function MapFromFloat16Buffer(const Buffer): UInt16;
Function MapFromFloat16(const Value: Float16): UInt16;{$IFDEF CanInline} inline;{$ENDIF}
Function MapFromHalf(const Value: Half): UInt16;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  EncodeFloat16Buffer
  EncodeFloat16
  EncodeHalf

  When BiasedExp is true, it indicates that the passed exponent is already
  biased and will be stored as is. When false, the passed exponent will be
  biased before storing.

    NOTE - the valid range for exponent is -15..+16 when biased, 0..31
           when unbiased. The exponent is clamped (limited to a prescribed
           range) before biasing and storing.

  Integer bit, when passed in the mantissa, is ignored - it is implied for
  half-precision float.

    NOTE - only lowest 10 bits of the mantissa are used, other bits gets
           masked-out before storage.
}
procedure EncodeFloat16Buffer(out Buffer; Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False);
Function EncodeFloat16(Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False): Float16;{$IFDEF CanInline} inline;{$ENDIF}
Function EncodeHalf(Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False): Half;{$IFDEF CanInline} inline;{$ENDIF}

{
  DecodeFloat16Buffer
  DecodeFloat16
  DecodeHalf

  When BiasedExp is set to true, the returned exponent is exponent as it is
  stored in the value, that is, biased. When false, the returned exponent is
  unbiased (its true value).

    NOTE - returned exponent will be within range of -15..+16 when biased,
           0..31 when unbiased.

  When IntBit is set to true, the returned mantissa contains the integer bit
  (bit 10) inferred from the number class (0 for denormals and zero,
  1 otherwise). When false, the integer bit is masked-out and is zero,
  irrespective of actual value.

    NOTE - only lowest 10 (11 with integer bit) bits of the mantissa are valid,
           other bits will always be zero.
}
procedure DecodeFloat16Buffer(const Buffer; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
procedure DecodeFloat16(const Value: Float16; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);{$IFDEF CanInline} inline;{$ENDIF}
procedure DecodeHalf(const Value: Half; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

procedure MapToFloat32Buffer(out Buffer; Value: UInt32);
Function MapToFloat32(Value: UInt32): Float32;{$IFDEF CanInline} inline;{$ENDIF}
Function MapToSingle(Value: UInt32): Single;{$IFDEF CanInline} inline;{$ENDIF}

Function MapFromFloat32Buffer(const Buffer): UInt32;
Function MapFromFloat32(const Value: Float32): UInt32;{$IFDEF CanInline} inline;{$ENDIF}
Function MapFromSingle(const Value: Single): UInt32;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

{
  EncodeFloat32Buffer
  EncodeFloat32
  EncodeSingle

  When BiasedExp is true, it indicates that the passed exponent is already
  biased and will be stored as is. When false, the passed exponent will be
  biased before storing.

    NOTE - the valid range for exponent is -127..+128 when biased, 0..255
           when unbiased. The exponent is clamped (limited to a prescribed
           range) before biasing and storing.

  Integer bit, when passed in the mantissa, is ignored - it is implied for
  single-precision float.

    NOTE - only lowest 23 bits of the mantissa are used, other bits gets
           masked-out before storage.
}
procedure EncodeFloat32Buffer(out Buffer; Mantissa: UInt32; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False);
Function EncodeFloat32(Mantissa: UInt32; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False): Float32;{$IFDEF CanInline} inline;{$ENDIF}
Function EncodeSingle(Mantissa: UInt32; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False): Single;{$IFDEF CanInline} inline;{$ENDIF}

{
  DecodeFloat32Buffer
  DecodeFloat32
  DecodeSingle

  When BiasedExp is set to true, the returned exponent is exponent as it is
  stored in the value, that is, biased. When false, the returned exponent is
  unbiased (its true value).

    NOTE - returned exponent will be within range of -127..+128 when biased,
           0..255 when unbiased.

  When IntBit is set to true, the returned mantissa contains the integer bit
  (bit 23) inferred from the number class (0 for denormals and zero,
  1 otherwise). When false, the integer bit is masked-out and is zero,
  irrespective of actual value.

    NOTE - only lowest 23 (24 with integer bit) bits of the mantissa are valid,
           other bits will always be zero.
}
procedure DecodeFloat32Buffer(const Buffer; out Mantissa: UInt32; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
procedure DecodeFloat32(const Value: Float32; out Mantissa: UInt32; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);{$IFDEF CanInline} inline;{$ENDIF}
procedure DecodeSingle(const Value: Single; out Mantissa: UInt32; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);{$IFDEF CanInline} inline;{$ENDIF}


{$IFDEF FPC}
{-------------------------------------------------------------------------------
================================================================================
                              Operators overloading
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Operators overloading - declaration
===============================================================================}
{
  Operators overloading is currently implemented only for FPC.
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

// arithmetic operators
operator + (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator - (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator * (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}
operator / (A,B: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}

{$ENDIF}

{-------------------------------------------------------------------------------
================================================================================
                         Unit implementation management
================================================================================
-------------------------------------------------------------------------------}
{
  WARNING - be wery careful when changing the selected implementation, as there
            is absolutely no thread-safety protection.

  For full description of this section, please refer to the same section in
  BitOps library (github.com/TheLazyTomcat/Lib.BitOps), file BitOps.pas.
}

type
  TUIM_Float16Utils_Function = (fnGetMXCSR,fnSetMXCSR,
                                fnHalfToSingle,fnSingleToHalf,
                                fnHalfToSingle4x,fnSingleToHalf4x);

  TUIM_Float16Utils_Implementation = (imNone,imPascal,imAssembly);

  TUIM_Float16Utils_Implementations = set of TUIM_Float16Utils_Implementation;

//------------------------------------------------------------------------------

{
  Returns which implementations are available for the selected function.
}
Function UIM_Float16Utils_AvailableFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementations;

{
  Returns which implementations are supported and can be safely selected for
  a given function.
}
Function UIM_Float16Utils_SupportedFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementations;

{
  Returns value indicating what implementation of the selected function is
  executed when calling the function.
}
Function UIM_Float16Utils_GetFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementation;

{
  Routes selected function to a selected implementation.

  Returned value is the previous routing.

  NOTE - when routing GetMXCSR or SetMXCSR (fnGetMXCSR, fnSetMXCSR), both
         functions are set to the same implementation - sanity protection,
         so they do not operate on different domains

  NOTE - when asm implementation cannot be used, and you still select it,
         the function will be routed to pascal version

  WARNING - when selecting imNone as an implementation for some function, the
            routing is set to nil, and because the routing mechanism, for the
            sake of speed, does not check validity, it will result in an
            exception when calling this function

  WANRING - when selecting unsupported implementation, calling the function
            will almost certainly result in an system exception (invalid
            instruction).
}
Function UIM_Float16Utils_SetFuncImpl(Func: TUIM_Float16Utils_Function; NewImpl: TUIM_Float16Utils_Implementation): TUIM_Float16Utils_Implementation;

implementation

uses
{$IF Defined(AllowF16CExtension) and not Defined(PurePascal)}
  SimpleCPUID,
{$IFEND}
  BasicUIM,
  Math;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{-------------------------------------------------------------------------------
    Internal constants
-------------------------------------------------------------------------------}

const
  F16_MASK_SIGN = UInt16($8000);  // sign bit
  F16_MASK_EXP  = UInt16($7C00);  // exponent
  F16_MASK_FRAC = UInt16($03FF);  // fraction/mantissa
  F16_MASK_NSGN = UInt16($7FFF);  // non-sign bits
  F16_MASK_FHB  = UInt16($0200);  // highest bit of the mantissa
  F16_MASK_INTB = UInt16($0400);  // otherwise implicit integer bit of the mantissa

  F32_MASK_SIGN = UInt32($80000000);
  F32_MASK_EXP  = UInt32($7F800000);
  F32_MASK_FRAC = UInt32($007FFFFF);
{$IFNDEF FPC} // not used anywhere
  F32_MASK_NSGN = UInt32($7FFFFFFF);
{$ENDIF}
  F32_MASK_FHB  = UInt32($00400000);
  F32_MASK_INTB = UInt32($00800000); 
  F32_MASK_REMB = UInt32($00001FFF);  // 13 bits removed from single mantissa when converting to half mantissa

{===============================================================================
    Library-specific exceptions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Library-specific exceptions - floating-point exceptions
-------------------------------------------------------------------------------}

constructor EF16UFPUException.CreateNoClear(const Msg: String{$IFNDEF FPC}; Dummy: Integer{$ENDIF});
begin
inherited Create(Msg);
fExceptionFlags := GetMXCSR and $3F;
end;

//------------------------------------------------------------------------------

constructor EF16UFPUException.Create(const Msg: String);
begin
CreateNoClear(Msg);
// these exceptions should not change e-flags in true MXCSR when it is not used
If EmulatedMXCSR then
  ClearSSEExceptions;
end;

//------------------------------------------------------------------------------

constructor EF16UFPUException.CreateDefMsgNoClear({$IFNDEF FPC}Dummy: Integer{$ENDIF});
begin
CreateNoClear(DefaultMessage);
end;

//------------------------------------------------------------------------------

constructor EF16UFPUException.CreateDefMsg;
begin
Create(DefaultMessage);
end;

{-------------------------------------------------------------------------------
    Library-specific exceptions - individual floating-point exception classes
-------------------------------------------------------------------------------}

Function EF16UInvalidOp.DefaultMessage: String;
begin
Result := 'Invalid floating point operand';
end;

//==============================================================================

Function EF16UDenormal.DefaultMessage: String;
begin
Result := 'Denormal floating point operand';
end;

//==============================================================================

Function EF16UDivByZero.DefaultMessage: String;
begin
Result := 'Floating point division by zero';
end;

//==============================================================================

Function EF16UOverflow.DefaultMessage: String;
begin
Result := 'Floating point arithmetic overflow';
end;

//==============================================================================

Function EF16UUnderflow.DefaultMessage: String;
begin
Result := 'Floating point arithmetic underflow';
end;

//==============================================================================

Function EF16UPrecision.DefaultMessage: String;
begin
Result := 'Inexact floating point result';
end;

{-------------------------------------------------------------------------------
================================================================================
                               Auxiliary routines
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Auxiliary routines - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary routines - internal functions
-------------------------------------------------------------------------------} 
{
  WARNING - MXCSR_MASK is initialized once and only once, at the unit
            initialization. It must not be written into later at any cost, that
            would break thread safety.
            Therefore consider this variable to be read-only.
}
var
  MXCSR_MASK: UInt32;

//------------------------------------------------------------------------------

{$IFDEF F16U_ASM_IMPL}
Function MXCSR_MASK_Load(Mem: Pointer): UInt32; register; assembler;
asm
{
  - FXSAVE does not check for pending FPU exceptions, therefore added FWAIT to
    be sure
  - state saved by FXSAVE can contain MXCSR_MASK provided by the CPU (if it is
    zero, CPU is not providing it)
  - position of the mask is the same in all CPU modes (offset 28) - no need to
    branch for individual modes
}
    FWAIT
    FXSAVE  [Mem]
    MOV     EAX,  dword ptr [Mem + 28]  
end;
{$ENDIF}

//------------------------------------------------------------------------------

{$IFNDEF F16U_ASM_IMPL}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
procedure MXCSR_MASK_Init(EmulatedImpl: Boolean);
{$IFDEF F16U_ASM_IMPL}
var
  Buff: Pointer;
  Mask: UInt32;
begin
If not EmulatedImpl then
  begin
    // memory for FXSAVE must be 16-byte aligned and intialized to all-zero
    Buff := AllocMem(528{512 + 16 for alignment});
    try
    {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
      If (PtrUInt(Buff) and PtrUInt($F)) = 0 then
        Mask := MXCSR_MASK_Load(Buff)
      else
        Mask := MXCSR_MASK_Load(Pointer((PtrUInt(Buff) + 16) and not PtrUInt($F)));
    {$IFDEF FPCDWM}{$POP}{$ENDIF}
    finally
      FreeMem(Buff,528);
    end;
    If Mask <> 0 then
      MXCSR_MASK := Mask
    else
      MXCSR_MASK := $0000FFBF;  // default mask, note that DAZ bit is not supported
  end
else MXCSR_MASK := $0000FFFF;  // DAZ bit is supported in pascal emulation
end;
{$ELSE}
begin
MXCSR_MASK := $0000FFFF;  // DAZ bit supported
end;
{$ENDIF}
{$IFNDEF F16U_ASM_IMPL}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - SSE status and control register (MXCSR) access
-------------------------------------------------------------------------------}

threadvar
  Pas_MXCSR:  UInt32;
  MXCSRInit:  Boolean;  // compiler initializes this to false

//------------------------------------------------------------------------------

{$IFDEF F16U_ASM_IMPL}

Function Fce_GetMXCSR_Asm: UInt32; register; assembler;
var
  Temp: UInt32;
asm
    STMXCSR   dword ptr [Temp]
    MOV       EAX,  dword ptr [Temp]
  {$IFDEF x64}
    AND       EAX,  dword ptr [RIP + MXCSR_MASK]
  {$ELSE}
    AND       EAX,  dword ptr [MXCSR_MASK]
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Fce_SetMXCSR_Asm(NewValue: UInt32); register; assembler;
var
  Temp: UInt32;
asm
  {$IFDEF x64}
    AND       NewValue, dword ptr [RIP + MXCSR_MASK]
  {$ELSE}
    AND       NewValue, dword ptr [MXCSR_MASK]
  {$ENDIF}
    MOV       dword ptr [Temp], NewValue
    LDMXCSR   dword ptr [Temp]
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function Fce_GetMXCSR_Pas: UInt32; register;
begin
If not MXCSRInit then
  begin
  {
    rounding set to nearest, masked precission, underflow and denormal
    exceptions, DAZ and FTZ flags are cleared, exception flags are all cleared

    note - hardware initialization value is $00001F80
  }
    Pas_MXCSR := $00001900;
    MXCSRInit := True;
  end;
Result := Pas_MXCSR and MXCSR_MASK;
end;

//------------------------------------------------------------------------------

procedure Fce_SetMXCSR_Pas(NewValue: UInt32); register;
begin
Pas_MXCSR := NewValue and MXCSR_MASK;
MXCSRInit := True;
end;

//------------------------------------------------------------------------------

var
  Var_GetMXCSR: Function: UInt32; register = Fce_GetMXCSR_Pas;
  Var_SetMXCSR: procedure(NewValue: UInt32); register = Fce_SetMXCSR_Pas;

{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Low-level access
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}

Function GetMXCSR: UInt32;
begin
Result := Var_GetMXCSR;
end;

//------------------------------------------------------------------------------

procedure SetMXCSR(NewValue: UInt32);
begin
Var_SetMXCSR(NewValue);
end;

//------------------------------------------------------------------------------

Function EmulatedMXCSR: Boolean;
begin
{$IFDEF F16U_ASM_IMPL}
If Assigned(@Var_GetMXCSR) then
  Result := UIM_Float16Utils_GetFuncImpl(fnGetMXCSR) = imPascal
else
  raise EF16UNoImplementation.Create('EmulatedMXCSR: Unassigned routing.');
{$ELSE}
Result := True;
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure InitMXCSR;
begin
SetMXCSR($00001900);
end;

//------------------------------------------------------------------------------

Function GetMXCSRMask: UInt32;
begin
Result := MXCSR_MASK;
end;

//------------------------------------------------------------------------------

Function GetMXCSRSupportsDAZ: Boolean;
begin
Result := (MXCSR_MASK and MXCSR_DenormalsAreZeros) <> 0;
end;

{--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
    Abstracted access
 --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --}

Function GetSSERoundingMode: TSSERoundingMode;
begin
case (GetMXCSR and MXCSR_Rounding) shr MXCSR_SHIFT_Rounding of
  1:  Result := rmDown;
  2:  Result := rmUp;
  3:  Result := rmTruncate;
else
  Result := rmNearest;
end;
end;

//------------------------------------------------------------------------------

Function SetSSERoundingMode(NewValue: TSSERoundingMode): TSSERoundingMode;
var
  Num:  UInt32;
begin
Result := GetSSERoundingMode;
case NewValue of
  rmDown:     Num := 1;
  rmUp:       Num := 2;
  rmTruncate: Num := 3;
else
 {rmNearest}
  Num := 0;
end;
SetMXCSR((GetMXCSR and not MXCSR_Rounding) or (Num shl MXCSR_SHIFT_Rounding));
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionMask(SSEException: TSSEException): Boolean;
begin
case SSEException of
  excInvalidOp: Result := (GetMXCSR and MXCSR_EMASK_InvalidOP) <> 0;
  excDenormal:  Result := (GetMXCSR and MXCSR_EMASK_Denormal) <> 0;
  excDivByZero: Result := (GetMXCSR and MXCSR_EMASK_DivByZero) <> 0;
  excOverflow:  Result := (GetMXCSR and MXCSR_EMASK_Overflow) <> 0;
  excUnderflow: Result := (GetMXCSR and MXCSR_EMASK_Underflow) <> 0;
  excPrecision: Result := (GetMXCSR and MXCSR_EMASK_Precision) <> 0;
else
  raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionMask: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionMask(SSEException: TSSEException; NewValue: Boolean): Boolean;

  procedure SetBit(Bitmask: UInt32);
  begin
    If NewValue then
      SetMXCSR(GetMXCSR or Bitmask)
    else
      SetMXCSR(GetMXCSR and not Bitmask);
  end;

begin
Result := GetSSEExceptionMask(SSEException);
case SSEException of
  excInvalidOp: SetBit(MXCSR_EMASK_InvalidOP);
  excDenormal:  SetBit(MXCSR_EMASK_Denormal);
  excDivByZero: SetBit(MXCSR_EMASK_DivByZero);
  excOverflow:  SetBit(MXCSR_EMASK_Overflow);
  excUnderflow: SetBit(MXCSR_EMASK_Underflow);
  excPrecision: SetBit(MXCSR_EMASK_Precision);
else
  raise EF16UInvalidFlag.CreateFmt('SetSSEExceptionMask: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionMasks: TSSEExceptions;
var
  MXCSR:  UInt32;
  i:      TSSEException;
begin
Result := [];
MXCSR := GetMXCSR;
For i := Low(TSSEException) to High(TSSEException) do
  case i of
    excInvalidOp: If (MXCSR and MXCSR_EMASK_InvalidOP) <> 0 then Include(Result,i);
    excDenormal:  If (MXCSR and MXCSR_EMASK_Denormal) <> 0 then Include(Result,i);
    excDivByZero: If (MXCSR and MXCSR_EMASK_DivByZero) <> 0 then Include(Result,i);
    excOverflow:  If (MXCSR and MXCSR_EMASK_Overflow) <> 0 then Include(Result,i);
    excUnderflow: If (MXCSR and MXCSR_EMASK_Underflow) <> 0 then Include(Result,i);
    excPrecision: If (MXCSR and MXCSR_EMASK_Precision) <> 0 then Include(Result,i);
  else
    raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionMasks: Invalid SSE exception (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionMasks(NewValue: TSSEExceptions): TSSEExceptions;
var
  MXCSR:  UInt32;

  procedure SetBit(Bitmask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or Bitmask
    else
      MXCSR := MXCSR and not Bitmask;
  end;

begin
Result := GetSSEExceptionMasks;
MXCSR := GetMXCSR;
SetBit(MXCSR_EMASK_InvalidOP,excInvalidOp in NewValue);
SetBit(MXCSR_EMASK_Denormal,excDenormal in NewValue);
SetBit(MXCSR_EMASK_DivByZero,excDivByZero in NewValue);
SetBit(MXCSR_EMASK_Overflow,excOverflow in NewValue);
SetBit(MXCSR_EMASK_Underflow,excUnderflow in NewValue);
SetBit(MXCSR_EMASK_Precision,excPrecision in NewValue);
SetMXCSR(MXCSR);
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionFlag(SSEException: TSSEException): Boolean;
begin
case SSEException of
  excInvalidOp: Result := (GetMXCSR and MXCSR_EFLAG_InvalidOP) <> 0;
  excDenormal:  Result := (GetMXCSR and MXCSR_EFLAG_Denormal) <> 0;
  excDivByZero: Result := (GetMXCSR and MXCSR_EFLAG_DivByZero) <> 0;
  excOverflow:  Result := (GetMXCSR and MXCSR_EFLAG_Overflow) <> 0;
  excUnderflow: Result := (GetMXCSR and MXCSR_EFLAG_Underflow) <> 0;
  excPrecision: Result := (GetMXCSR and MXCSR_EFLAG_Precision) <> 0;
else
  raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionFlag: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionFlag(SSEException: TSSEException; NewValue: Boolean): Boolean;

  procedure SetBit(Bitmask: UInt32);
  begin
    If NewValue then
      SetMXCSR(GetMXCSR or Bitmask)
    else
      SetMXCSR(GetMXCSR and not Bitmask);
  end;

begin
Result := GetSSEExceptionFlag(SSEException);
case SSEException of
  excInvalidOp: SetBit(MXCSR_EFLAG_InvalidOP);
  excDenormal:  SetBit(MXCSR_EFLAG_Denormal);
  excDivByZero: SetBit(MXCSR_EFLAG_DivByZero);
  excOverflow:  SetBit(MXCSR_EFLAG_Overflow);
  excUnderflow: SetBit(MXCSR_EFLAG_Underflow);
  excPrecision: SetBit(MXCSR_EFLAG_Precision);
else
  raise EF16UInvalidFlag.CreateFmt('SetSSEExceptionFlag: Invalid SSE exception (%d).',[Ord(SSEException)]);
end;
end;

//------------------------------------------------------------------------------

Function GetSSEExceptionFlags: TSSEExceptions;
var
  MXCSR:  UInt32;
  i:      TSSEException;
begin
Result := [];
MXCSR := GetMXCSR;
For i := Low(TSSEException) to High(TSSEException) do
  case i of
    excInvalidOp: If (MXCSR and MXCSR_EFLAG_InvalidOP) <> 0 then Include(Result,i);
    excDenormal:  If (MXCSR and MXCSR_EFLAG_Denormal) <> 0 then Include(Result,i);
    excDivByZero: If (MXCSR and MXCSR_EFLAG_DivByZero) <> 0 then Include(Result,i);
    excOverflow:  If (MXCSR and MXCSR_EFLAG_Overflow) <> 0 then Include(Result,i);
    excUnderflow: If (MXCSR and MXCSR_EFLAG_Underflow) <> 0 then Include(Result,i);
    excPrecision: If (MXCSR and MXCSR_EFLAG_Precision) <> 0 then Include(Result,i);
  else
    raise EF16UInvalidFlag.CreateFmt('GetSSEExceptionFlags: Invalid SSE exception (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetSSEExceptionFlags(NewValue: TSSEExceptions): TSSEExceptions;
var
  MXCSR:  UInt32;

  procedure SetBit(Bitmask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or Bitmask
    else
      MXCSR := MXCSR and not Bitmask;
  end;

begin
Result := GetSSEExceptionFlags;
MXCSR := GetMXCSR;
SetBit(MXCSR_EFLAG_InvalidOP,excInvalidOp in NewValue);
SetBit(MXCSR_EFLAG_Denormal,excDenormal in NewValue);
SetBit(MXCSR_EFLAG_DivByZero,excDivByZero in NewValue);
SetBit(MXCSR_EFLAG_Overflow,excOverflow in NewValue);
SetBit(MXCSR_EFLAG_Underflow,excUnderflow in NewValue);
SetBit(MXCSR_EFLAG_Precision,excPrecision in NewValue);
SetMXCSR(MXCSR);
end;

//------------------------------------------------------------------------------

Function GetSSEFlag(Flag: TSSEFlag): Boolean;
begin
case Flag of
  flDenormalsAreZeros:  Result := (GetMXCSR and MXCSR_DenormalsAreZeros) <> 0;
  flFlushToZero:        Result := (GetMXCSR and MXCSR_FlushToZero) <> 0;
else
  raise EF16UInvalidFlag.CreateFmt('GetSSEFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function SetSSEFlag(Flag: TSSEFlag; NewValue: Boolean): Boolean;

  procedure SetBit(Bitmask: UInt32);
  begin
    If NewValue then
      SetMXCSR(GetMXCSR or Bitmask)
    else
      SetMXCSR(GetMXCSR and not Bitmask);
  end;
  
begin
Result := GetSSEFlag(Flag);
case Flag of
  flDenormalsAreZeros:  SetBit(MXCSR_DenormalsAreZeros);
  flFlushToZero:        SetBit(MXCSR_FlushToZero);
else
  raise EF16UInvalidFlag.CreateFmt('SetSSEFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function GetSSEFlags: TSSEFlags;
var
  MXCSR:  UInt32;
  i:      TSSEFlag;
begin
Result := [];
MXCSR := GetMXCSR;
For i := Low(TSSEFlag) to High(TSSEFlag) do
  case i of
    flDenormalsAreZeros:  If (MXCSR and MXCSR_DenormalsAreZeros) <> 0 then Include(Result,i);
    flFlushToZero:        If (MXCSR and MXCSR_FlushToZero) <> 0 then Include(Result,i);
  else
    raise EF16UInvalidFlag.CreateFmt('GetX87Flags: Invalid flag (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

procedure SetSSEFlags(NewValue: TSSEFlags);
var
  MXCSR:  UInt32;

  procedure SetBit(Bitmask: UInt32; NewState: Boolean);
  begin
    If NewState then
      MXCSR := MXCSR or Bitmask
    else
      MXCSR := MXCSR and not Bitmask;
  end;

begin
MXCSR := GetMXCSR;
SetBit(MXCSR_DenormalsAreZeros,flDenormalsAreZeros in NewValue);
SetBit(MXCSR_FlushToZero,flFlushToZero in NewValue);
SetMXCSR(MXCSR);
end;

//------------------------------------------------------------------------------

procedure ClearSSEExceptions;
begin
SetMXCSR(GetMXCSR and $FFFFFFC0);
end;

//------------------------------------------------------------------------------

procedure RaiseSSEExceptions(var MXCSR: UInt32; Mask: Boolean = True);
begin
If ((MXCSR and MXCSR_EFLAG_InvalidOP) <> 0) and (not Mask or ((MXCSR and MXCSR_EMASK_InvalidOP) = 0)) then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_InvalidOP;
    raise EF16UInvalidOp.CreateDefMsgNoClear;
  end;
If ((MXCSR and MXCSR_EFLAG_Denormal) <> 0) and (not Mask or ((MXCSR and MXCSR_EMASK_Denormal) = 0)) then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_Denormal;
    raise EF16UDenormal.CreateDefMsgNoClear;
  end;
If ((MXCSR and MXCSR_EFLAG_DivByZero) <> 0) and (not Mask or ((MXCSR and MXCSR_EMASK_DivByZero) = 0)) then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_DivByZero;
    raise EF16UDivByZero.CreateDefMsgNoClear;
  end;
If ((MXCSR and MXCSR_EFLAG_Overflow) <> 0) and (not Mask or ((MXCSR and MXCSR_EMASK_Overflow) = 0)) then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_Overflow;
    raise EF16UOverflow.CreateDefMsgNoClear;
  end;
If ((MXCSR and MXCSR_EFLAG_Underflow) <> 0) and (not Mask or ((MXCSR and MXCSR_EMASK_Underflow) = 0)) then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_Underflow;
    raise EF16UUnderflow.CreateDefMsgNoClear;
  end;
If ((MXCSR and MXCSR_EFLAG_Precision) <> 0) and (not Mask or ((MXCSR and MXCSR_EMASK_Precision) = 0)) then
  begin
    MXCSR := MXCSR and not MXCSR_EFLAG_Precision;
    raise EF16UPrecision.CreateDefMsgNoClear;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RaiseSSEExceptions(Mask: Boolean = True);
var
  TempMXCSR:  UInt32;
begin
TempMXCSR := GetMXCSR;
RaiseSSEExceptions(TempMXCSR,Mask);
end;

{-------------------------------------------------------------------------------
    Auxiliary routines - conversion functions
-------------------------------------------------------------------------------}

procedure SignalSSEExceptions(Exceptions: TSSEExceptions); overload;
var
  i:      TSSEException;
  Masks:  TSSEExceptions;
begin
SetSSEExceptionFlags(GetSSEExceptionFlags + Exceptions);
Masks := GetSSEExceptionMasks;
For i := Low(TSSEException) to High(TSSEException) do
  If (i in Exceptions) and not(i in Masks) then
    case i of
      excDenormal:  raise EF16UDenormal.CreateDefMsg;
      excDivByZero: raise EF16UDivByZero.CreateDefMsg;
      excOverflow:  raise EF16UOverflow.CreateDefMsg;
      excUnderflow: raise EF16UUnderflow.CreateDefMsg;
      excPrecision: raise EF16UPrecision.CreateDefMsg;
    else
     {excInvalidOp}
      raise EF16UInvalidOP.CreateDefMsg;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SignalSSEExceptions(Exceptions: UInt32); overload;
var
  TempMXCSR:  UInt32;
begin
// faster implementation
TempMXCSR := GetMXCSR;
SetMXCSR(TempMXCSR or Exceptions);
If (Exceptions and MXCSR_EFLAG_InvalidOP <> 0) and (TempMXCSR and MXCSR_EMASK_InvalidOP = 0) then
  raise EF16UInvalidOP.CreateDefMsg;
If (Exceptions and MXCSR_EFLAG_Denormal <> 0) and (TempMXCSR and MXCSR_EMASK_Denormal = 0) then
  raise EF16UDenormal.CreateDefMsg;
If (Exceptions and MXCSR_EFLAG_DivByZero <> 0) and (TempMXCSR and MXCSR_EMASK_DivByZero = 0) then
  raise EF16UDivByZero.CreateDefMsg;
If (Exceptions and MXCSR_EFLAG_Overflow <> 0) and (TempMXCSR and MXCSR_EMASK_Overflow = 0) then
  raise EF16UOverflow.CreateDefMsg;
If (Exceptions and MXCSR_EFLAG_Underflow <> 0) and (TempMXCSR and MXCSR_EMASK_Underflow = 0) then
  raise EF16UUnderflow.CreateDefMsg;
If (Exceptions and MXCSR_EFLAG_Precision <> 0) and (TempMXCSR and MXCSR_EMASK_Precision = 0) then
  raise EF16UPrecision.CreateDefMsg;
end;

//------------------------------------------------------------------------------

procedure Fce_HalfToSingle_Pas(HalfPtr,SinglePtr: Pointer); register;
var
  Sign:           UInt16;
  Exponent:       Int32;  // biased exponent (true exponent + 15)
  Mantissa:       UInt16;
  MantissaShift:  Integer;

  Function HighZeroCount(Value: UInt16): Integer;
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

begin
Sign := PUInt16(HalfPtr)^ and F16_MASK_SIGN;
Exponent := Int32((PUInt16(HalfPtr)^ and F16_MASK_EXP) shr 10);
Mantissa := PUInt16(HalfPtr)^ and F16_MASK_FRAC;
case Exponent of

        // zero exponent - zero or denormal
    0:  If Mantissa <> 0 then
          begin
          {
            denormal, normalize...

            ...shift mantissa left so that its highest set bit will be
            shifted to implicit integer bit (bit 23), also correct
            exponent to reflect this change

            DAZ bit ignored, denormal exceptions not signaled
          }
            MantissaShift := HighZeroCount(Mantissa) + 8;
            PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or
                                   UInt32(UInt32(126 - MantissaShift) shl 23) or
                                  (UInt32(UInt32(Mantissa) shl MantissaShift) and F32_MASK_FRAC);
          end
        // return signed zero
        else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16);

        // max exponent - infinity or NaN
  $1F:  If Mantissa <> 0 then
          begin
            // not a number
            If (Mantissa and F16_MASK_FHB) = 0 then
              begin
                // signaled NaN
                SignalSSEExceptions(MXCSR_EFLAG_InvalidOp);
                // no exception raised, return quiet signed NaN with mantissa
                PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or F32_MASK_EXP or
                                       F32_MASK_FHB or UInt32(UInt32(Mantissa) shl 13)
              end
            // quiet signed NaN with mantissa
            else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or F32_MASK_EXP or
                                        UInt32(UInt32(Mantissa) shl 13);
          end
        // return signed infinity
        else PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or F32_MASK_EXP;
        
else
  // normal number
  PUInt32(SinglePtr)^ := UInt32(UInt32(Sign) shl 16) or
                         UInt32(UInt32(Exponent + 112{127 - 15}) shl 23) or
                         UInt32(UInt32(Mantissa) shl 13);
end;
end;

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf_Pas(SinglePtr,HalfPtr: Pointer); register;
var
  Sign:         UInt32;
  Exponent:     Int32;  // biased exponent (true exponent + 127)
  Mantissa:     UInt32;
  RoundMode:    TSSERoundingMode;
  ManHighSet:   Integer;
  ResultTemp:   UInt16;
  BitsLost:     Boolean;
  ConvMantissa: UInt32;
  ConvBitsLost: Boolean;
  ManOverflow:  Boolean;
  ExcsTemp:     UInt32;

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

  Function ShiftMantissa(Value: UInt32; Shift: Byte; out DataLoss: Boolean): UInt32;
  var
    Mask:     UInt32;
    Low,High: UInt32;
  begin
    DataLoss := False;
    If (Shift > 0) and (Shift < 25) then
      begin
        Mask := UInt32($FFFFFFFF) shr (32 - Shift);
        If (Value and Mask) <> 0 then
          begin
            DataLoss := True;
            Low := Value and not Mask;
            High := Low + (Mask + 1);
            case RoundMode of
              rmDown:     If Sign <> 0 then
                            Result := High shr Shift
                          else
                            Result := Low shr Shift;
              rmUp:       If Sign <> 0 then
                            Result := Low shr Shift
                          else
                            Result := High shr Shift;
              rmTruncate: Result := Low shr Shift;
            else
             {rmNearest}
              If (Value - Low) > (High - Value) then
                Result := High shr Shift
              else If (Value - Low) < (High - Value) then
                Result := Low shr Shift
              else
                begin
                  // select the one with clear lowest bit
                  If High and (Mask + 1) = 0 then
                    Result := High shr Shift
                  else
                    Result := Low shr Shift;
                end;
            end;
          end
        else Result := Value shr Shift;
      end
    // following cases should not happen, but whatever...  
    else If Shift >= 25 then
      Result := 0
    else
      Result := Value;
  end;

begin
RoundMode := GetSSERoundingMode;
Sign := PUInt32(SinglePtr)^ and F32_MASK_SIGN;
Exponent := (PUInt32(SinglePtr)^ and F32_MASK_EXP) shr 23;
Mantissa := PUInt32(SinglePtr)^ and F32_MASK_FRAC;
case Exponent of

        // exponent of zero - zero or denormal
    0:  If Mantissa <> 0 then
          begin
            // non-zero mantissa - denormals
            If not GetSSEFlag(flDenormalsAreZeros) then
              begin
                // DAZ mode inactive
                // pre-computation exceptions
                SignalSSEExceptions(MXCSR_EFLAG_Denormal);
                // post-computation exceptions (FTZ ignored)
                If not GetSSEExceptionMask(excUnderflow) then
                  begin
                    ManHighSet := BitScanReverse(Mantissa); // returns zero-based index!
                    If ManHighSet >= 11{mantissa width, including integer bit} then
                      begin
                        If Mantissa and (UInt32(Int32(-1)) shr (42 - ManHighSet)) <> 0 then
                          SignalSSEExceptions(MXCSR_EFLAG_Underflow or MXCSR_EFLAG_Precision)
                        else
                          SignalSSEExceptions(MXCSR_EFLAG_Underflow);
                      end
                    else SignalSSEExceptions(MXCSR_EFLAG_Underflow);
                  end
                else SignalSSEExceptions(MXCSR_EFLAG_Underflow or MXCSR_EFLAG_Precision);
                // no exception raised, return result
                If ((RoundMode = rmUp) and (Sign = 0)) or
                   ((RoundMode = rmDown) and (Sign <> 0)) then
                  // return signed smallest representable number
                  PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16(1)
                else
                  // convert to signed zero
                  PUInt16(HalfPtr)^ := UInt16(Sign shr 16);
              end
            // DAZ mode - return signed zero
            else PUInt16(HalfPtr)^ := UInt16(Sign shr 16);
          end
        // mantissa of 0 - return signed zero
        else PUInt16(HalfPtr)^ := UInt16(Sign shr 16);

      {
        exponent 1..101 (-126..-26 unbiased) - exponent too small to be
        represented in half even as denormal
      }
   1..
  $65:  begin
          // post-computation exceptions (FTZ ignored)
          If Mantissa and F32_MASK_REMB = 0 then
            SignalSSEExceptions(MXCSR_EFLAG_Underflow);
          SignalSSEExceptions(MXCSR_EFLAG_Underflow or MXCSR_EFLAG_Precision);          
          // no exception raised, return result
          If ((RoundMode = rmUp) and (Sign = 0)) or
             ((RoundMode = rmDown) and (Sign <> 0)) then
            // return signed smallest representable number
            PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16(1)
          else
            // convert to signed zero
            PUInt16(HalfPtr)^ := UInt16(Sign shr 16);
        end;

      {
        exponent 102..111 (-25..-14 unbiased) - exponent still too small to be
        represented in half, but the result can be denormalized (implicit
        exponent of -14, explicit 0)
      }
  $66..
  $6F:  begin
          // post-computation exceptions (FTZ ignored)
          ShiftMantissa(Mantissa,13,BitsLost);
          If BitsLost then
            begin
              If GetSSEExceptionMask(excUnderflow) or not GetSSEExceptionMask(excPrecision) then
                SignalSSEExceptions(MXCSR_EFLAG_Underflow or MXCSR_EFLAG_Precision)
              else
                SignalSSEExceptions(MXCSR_EFLAG_Precision);
            end;
          ResultTemp := UInt16(Sign shr 16) or UInt16(ShiftMantissa(Mantissa or F32_MASK_INTB,$7E - Exponent,BitsLost));
          If GetSSEExceptionMask(excUnderflow) then
            begin
              If BitsLost then
                SignalSSEExceptions(MXCSR_EFLAG_Underflow or MXCSR_EFLAG_Precision);
            end
          else SignalSSEExceptions(MXCSR_EFLAG_Underflow);
          // no exception raised, return result
          PUInt16(HalfPtr)^ := ResultTemp;
        end;

      {
        exponent 112 (-15 unbiased) - similar to previous case, but with more
        intricacies because of a posibility of overflow and renormalization
      }
  $70:  begin
          // post-computation exceptions (FTZ ignored)
          ConvMantissa := ShiftMantissa(Mantissa or F32_MASK_INTB,13,ConvBitsLost);
          If ConvMantissa <> (F16_MASK_INTB shl 1) then
            begin
              // denormal after conversion
              If ConvBitsLost then
                begin
                  If GetSSEExceptionMask(excUnderflow) or not GetSSEExceptionMask(excPrecision) then
                    SignalSSEExceptions(MXCSR_EFLAG_Underflow or MXCSR_EFLAG_Precision)
                  else
                    SignalSSEExceptions(MXCSR_EFLAG_Precision);                
                end;
              // gradual denormalization
              ShiftMantissa(ConvMantissa,1,BitsLost);
              If GetSSEExceptionMask(excUnderflow) then
                begin
                  If BitsLost or ConvBitsLost then
                    SignalSSEExceptions(MXCSR_EFLAG_Underflow or MXCSR_EFLAG_Precision);
                end
              else SignalSSEExceptions(MXCSR_EFLAG_Underflow);
              // no exception raised, return result
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16(ShiftMantissa(Mantissa or F32_MASK_INTB,14,BitsLost));
            end
          else
            begin
              // result promoted to a normalized number (some bits always lost)            
              SignalSSEExceptions(MXCSR_EFLAG_Precision);
              PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_INTB;
            end;
        end;

      {
        exponent 143..254 (+16..+127 unbiased) - too large to be represented
        in half (resulting exponent would be larger than 15)
      }
  $8F..
  $FE:  begin
          // post-computation exceptions
          If Mantissa and F32_MASK_REMB = 0 then
            SignalSSEExceptions(MXCSR_EFLAG_Overflow);
          SignalSSEExceptions(MXCSR_EFLAG_Overflow or MXCSR_EFLAG_Precision);
          // no exception raised, return value
          If (RoundMode = rmTruncate) or
             ((RoundMode = rmUp) and (Sign <> 0)) or
             ((RoundMode = rmDown) and (Sign = 0)) then
            // return signed largest representable number
            PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or UInt16($7BFF)
          else
            // convert to signed infinity
            PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP;
        end;

        // max exponent - infinity or NaN
  $FF:  If Mantissa <> 0 then
          begin
            // not a number (NaN)
            If (Mantissa and F32_MASK_FHB) = 0 then
              begin
                // signaling NaN
                SignalSSEExceptions(MXCSR_EFLAG_InvalidOp);
                // no exception, return quiet signed NaN with truncated mantissa
                PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP or F16_MASK_FHB or
                                     UInt16(Mantissa shr 13);
              end
            // quiet signed NaN with truncated mantisssa
            else PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP or
                                      UInt16(Mantissa shr 13);
          end
        // return signed infinity
        else PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or F16_MASK_EXP;

else 
  // exponent 113..142 (-14..+15 unbiased) - representable numbers, normalized value
  Mantissa := ShiftMantissa(Mantissa,13,BitsLost);
  // check if mantisa overflowed - if so, increase exponent to compensate
  If Mantissa > F16_MASK_FRAC then
    begin
      Inc(Exponent);
      ManOverflow := True;
    end
  else ManOverflow := False;    
  // post-computation exceptions
  ExcsTemp := 0;
  If ManOverflow and (Exponent > 142) then
    // overflowed to infinity
    ExcsTemp := ExcsTemp or MXCSR_EFLAG_Overflow;
  If BitsLost then
    ExcsTemp := ExcsTemp or MXCSR_EFLAG_Precision;
  SignalSSEExceptions(ExcsTemp);
  // no exception raised, return result
  PUInt16(HalfPtr)^ := UInt16(Sign shr 16) or
                       UInt16((Exponent - 112) shl 10) or
                       UInt16(Mantissa and F16_MASK_FRAC);
end;
end;

//==============================================================================

procedure Fce_HalfToSingle4x_Pas(HalfPtr,SinglePtr: Pointer); register;
begin
Fce_HalfToSingle_Pas(HalfPtr,SinglePtr);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Fce_HalfToSingle_Pas(Pointer(PtrUInt(HalfPtr) + 2),Pointer(PtrUInt(SinglePtr) + 4));
Fce_HalfToSingle_Pas(Pointer(PtrUInt(HalfPtr) + 4),Pointer(PtrUInt(SinglePtr) + 8));
Fce_HalfToSingle_Pas(Pointer(PtrUInt(HalfPtr) + 6),Pointer(PtrUInt(SinglePtr) + 12));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf4x_Pas(SinglePtr,HalfPtr: Pointer); register;
begin
Fce_SingleToHalf_Pas(SinglePtr,HalfPtr);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Fce_SingleToHalf_Pas(Pointer(PtrUInt(SinglePtr) + 4),Pointer(PtrUInt(HalfPtr) + 2));
Fce_SingleToHalf_Pas(Pointer(PtrUInt(SinglePtr) + 8),Pointer(PtrUInt(HalfPtr) + 4));
Fce_SingleToHalf_Pas(Pointer(PtrUInt(SinglePtr) + 12),Pointer(PtrUInt(HalfPtr) + 6));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

{$IFNDEF PurePascal}

{$IFDEF ASMSuppressSizeWarnings}
  {$PUSH}
  {$WARN 2087 OFF}  //  Suppresses warnings on following $WARN
  {$WARN 7121 OFF}  //  Warning: Check size of memory operand "op: memory-operand-size is X bits, but expected [Y bits]"
{$ENDIF}

procedure Fce_HalfToSingle_Asm(HalfPtr,SinglePtr: Pointer); register; assembler;
asm
    MOVZX   EAX,  word ptr [HalfPtr]
    MOVD    XMM0, EAX

    DB  $C4, $E2, $79, $13, $C0         // VCVTPH2PS  XMM0, XMM0

    MOVSS   dword ptr [SinglePtr], XMM0
end;

//------------------------------------------------------------------------------

procedure Fce_SingleToHalf_Asm(SinglePtr,HalfPtr: Pointer); register; assembler;
asm
    MOVSS   XMM0, dword ptr [SinglePtr]

    // $04 - rounding selected in MXCSR is used
    DB  $C4, $E3, $79, $1D, $C0, $04    // VCVTPS2PH  XMM0, XMM0, $04

    MOVD    EAX,  XMM0
    MOV     word ptr [HalfPtr],  AX
end;

//------------------------------------------------------------------------------

procedure Fce_HalfToSingle4x_Asm(HalfPtr,SinglePtr: Pointer); register; assembler;
asm
    MOVSD   XMM0, qword ptr [HalfPtr]

    DB  $C4, $E2, $79, $13, $C0         // VCVTPH2PS  XMM0, XMM0

    MOVUPS  dqword ptr [SinglePtr],  XMM0
end;

//------------------------------------------------------------------------------

procedure Fce_SingletoHalf4x_Asm(SinglePtr,HalfPtr: Pointer); register; assembler;
asm
    MOVUPS  XMM0, dqword ptr [SinglePtr]

    // $04 - rounding selected in MXCSR is used
    DB  $C4, $E3, $79, $1D, $C0, $04    // VCVTPS2PH  XMM0, XMM0, $04

    MOVSD   qword ptr [HalfPtr],   XMM0
end;

{$IFDEF ASMSuppressSizeWarnings}
  {$POP}
{$ENDIF}

{$ENDIF}

//==============================================================================

var
  Var_HalfToSingle:   procedure(HalfPtr,SinglePtr: Pointer); register = Fce_HalfToSingle_Pas;
  Var_SingleToHalf:   procedure(SinglePtr,HalfPtr: Pointer); register = Fce_SingleToHalf_Pas;
  Var_HalfToSingle4x: procedure(HalfPtr,SinglePtr: Pointer); register = Fce_HalfToSingle4x_Pas;
  Var_SingleToHalf4x: procedure(SinglePtr,HalfPtr: Pointer); register = Fce_SingleToHalf4x_Pas;

//==============================================================================

Function MapFloat16ToWord(Value: Float16): UInt16;
var
  _Value: UInt16 absolute Value;
begin
Result := _Value;
end;

//------------------------------------------------------------------------------

Function MapHalfToWord(Value: Half): UInt16;
begin
Result := MapFloat16ToWord(Value);
end;

//------------------------------------------------------------------------------

Function MapWordToFloat16(Value: UInt16): Float16;
var
  _Result: UInt16 absolute Result;
begin
_Result := Value;
end;

//------------------------------------------------------------------------------

Function MapWordToHalf(Value: UInt16): Half;
begin
Result := MapWordToFloat16(Value);
end;

//==============================================================================

procedure Float16ToFloat32(Float16Ptr,Float32Ptr: Pointer);
begin
Var_HalfToSingle(Float16Ptr,Float32Ptr);
end;

//------------------------------------------------------------------------------

procedure HalfToSingle(HalfPtr,SinglePtr: Pointer);
begin
Var_HalfToSingle(HalfPtr,SinglePtr);
end;

//------------------------------------------------------------------------------

procedure Float32ToFloat16(Float32Ptr,Float16Ptr: Pointer);
begin
Var_SingleToHalf(Float32Ptr,Float16Ptr);
end;

//------------------------------------------------------------------------------

procedure SingleToHalf(SinglePtr,HalfPtr: Pointer);
begin
Var_SingleToHalf(SinglePtr,HalfPtr);
end;

//------------------------------------------------------------------------------

Function Float16ToFloat32(Value: Float16): Float32;
begin
Var_HalfToSingle(@Value,@Result);
end;

//------------------------------------------------------------------------------

Function HalfToSingle(Value: Half): Single;
begin
Var_HalfToSingle(@Value,@Result);
end;

//------------------------------------------------------------------------------

Function Float32ToFloat16(Value: Float32): Float16;
begin
Var_SingleToHalf(@Value,@Result);
end;

//------------------------------------------------------------------------------

Function SingleToHalf(Value: Single): Half;
begin
Var_SingleToHalf(@Value,@Result);
end;

//==============================================================================

procedure Float16ToFloat32Vec4(Float16Ptr,Float32Ptr: Pointer);
begin
Var_HalfToSingle4x(Float16Ptr,Float32Ptr);
end;

//------------------------------------------------------------------------------

procedure HalfToSingleVec4(HalfPtr,SinglePtr: Pointer);
begin
Var_HalfToSingle4x(HalfPtr,SinglePtr);
end;

//------------------------------------------------------------------------------

procedure Float32ToFloat16Vec4(Float32Ptr,Float16Ptr: Pointer);
begin
Var_SingleToHalf4x(Float32Ptr,Float16Ptr);
end;

//------------------------------------------------------------------------------

procedure SingleToHalfVec4(SinglePtr,HalfPtr: Pointer);
begin
Var_SingleToHalf4x(SinglePtr,HalfPtr);
end;


{-------------------------------------------------------------------------------
================================================================================
                               Number information
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Number information - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Number information - number class
-------------------------------------------------------------------------------}

Function IsZero(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// bits other than sign are zero
Result := _Value and F16_MASK_NSGN = 0;
end;

//------------------------------------------------------------------------------

Function IsDenormal(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// zero exponent, non-zero mantissa
Result := ((_Value and F16_MASK_EXP) = 0) and ((_Value and F16_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsNaN(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// max exponent and non-zero mantissa
Result := ((_Value and F16_MASK_EXP) = F16_MASK_EXP) and ((_Value and F16_MASK_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsInfinite(const Value: Half): Boolean;
var
  _Value: UInt16 absolute Value;
begin
// max exponent and zero mantissa
Result := ((_Value and F16_MASK_EXP) = F16_MASK_EXP) and ((_Value and F16_MASK_FRAC) = 0);
end;

//------------------------------------------------------------------------------

Function IsNormal(const Value: Half): Boolean;
var
  _Value:   UInt16 absolute Value;
  Exponent: UInt16;
begin
// non-zero less than max exponent, any mantissa
Exponent := (_Value and F16_MASK_EXP) shr 10;
Result := (Exponent > 0) and (Exponent < $1F);
end;

{-------------------------------------------------------------------------------
    Number information - sign-related
-------------------------------------------------------------------------------}

Function Sign(const Value: Half): TValueSign;
var
  _Value: UInt16 absolute Value;
begin
If (_Value and F16_MASK_NSGN) <> 0 then
  begin
    If (_Value and F16_MASK_SIGN) <> 0 then
      Result := -1
    else
      Result := 1;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

Function Abs(const Value: Half): Half;
var
  _Value:   UInt16 absolute Value;
  _Result:  UInt16 absolute Result;
begin
_Result := _Value and F16_MASK_NSGN;
end;

//------------------------------------------------------------------------------

Function Neg(const Value: Half): Half;
var
  _Value:   UInt16 absolute Value;
  _Result:  UInt16 absolute Result;
begin
_Result := _Value xor F16_MASK_SIGN;
end;


{-------------------------------------------------------------------------------
================================================================================
                              Comparison functions
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Comparison functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Comparison functions - basic comparison
-------------------------------------------------------------------------------}

Function IsEqual(const A,B: Half): Boolean;
var
  _A: UInt16 absolute A;
  _B: UInt16 absolute B;
begin
Result := _A = _B;
end;

//------------------------------------------------------------------------------

Function IsLess(const A,B: Half): Boolean;
begin
Result := HalfToSingle(A) < HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function IsGreater(const A,B: Half): Boolean;
begin
Result := HalfToSingle(A) > HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function IsLessOrEqual(const A,B: Half): Boolean;
begin
Result := HalfToSingle(A) <= HalfToSingle(B);
end;

//------------------------------------------------------------------------------

Function IsGreaterOrEqual(const A,B: Half): Boolean;
begin
Result := HalfToSingle(A) >= HalfToSingle(B);
end;

{-------------------------------------------------------------------------------
    Comparison functions - ordered comparison
-------------------------------------------------------------------------------}

Function CompareValue(const A,B: Half; Epsilon: Half): TValueRelationship;
begin
Result := TValueRelationship(Math.CompareValue(HalfToSingle(A),HalfToSingle(B),HalfToSingle(Epsilon)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareValue(const A,B: Half): TValueRelationship;
begin
Result := TValueRelationship(Math.CompareValue(HalfToSingle(A),HalfToSingle(B),0.0));
end;

//------------------------------------------------------------------------------

Function SameValue(const A,B: Half; Epsilon: Half): Boolean;
begin
Result := Math.SameValue(HalfToSingle(A),HalfToSingle(B),HalfToSingle(Epsilon));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameValue(const A,B: Half): Boolean;
begin
Result := Math.SameValue(HalfToSingle(A),HalfToSingle(B),0.0);
end;

{-------------------------------------------------------------------------------
================================================================================
                              Arithmetic functions
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Arithmetic functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Arithmetic functions - basic arithmetic
-------------------------------------------------------------------------------}

Function Add(const A,B: Half): Half;
begin
Result := SingleToHalf(HalfToSingle(A) + HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Subtract(const A,B: Half): Half;
begin
Result := SingleToHalf(HalfToSingle(A) - HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Multiply(const A,B: Half): Half;
begin
Result := SingleToHalf(HalfToSingle(A) * HalfToSingle(B));
end;

//------------------------------------------------------------------------------

Function Divide(const A,B: Half): Half;
begin
Result := SingleToHalf(HalfToSingle(A) / HalfToSingle(B));
end;


{-------------------------------------------------------------------------------
================================================================================
                              Floats encode/decode
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Floats encode/decode - implementation
===============================================================================}

procedure MapToFloat16Buffer(out Buffer; Value: UInt16);
var
  _Result:  UInt16 absolute Buffer;
begin
_Result := Value;
end;

//------------------------------------------------------------------------------

Function MapToFloat16(Value: UInt16): Float16;
begin
MapToFloat16Buffer(Result,Value);
end;

//------------------------------------------------------------------------------

Function MapToHalf(Value: UInt16): Half;
begin
MapToFloat16Buffer(Result,Value);
end;

//------------------------------------------------------------------------------

Function MapFromFloat16Buffer(const Buffer): UInt16;
var
  _Value: UInt16 absolute Buffer;
begin
Result := _Value;
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

// auxiliary routine
Function ClampExp(ExpVal: Int16; Low,High: Int16): Int16;
begin
If ExpVal < Low then
  Result := Low
else If ExpVal > High then
  Result := High
else
  Result := ExpVal;
end;

//------------------------------------------------------------------------------

procedure EncodeFloat16Buffer(out Buffer; Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False);
var
  _Result:  UInt16 absolute Buffer;
begin
_Result := Mantissa and F16_MASK_FRAC;
If BiasedExp then
  _Result := _Result or ((UInt16(ClampExp(Exponent,0,31)) shl 10) and F16_MASK_EXP)
else
  _Result := _Result or ((UInt16(ClampExp(Exponent,-15,16) + FLOAT16_EXPONENTBIAS) shl 10) and F16_MASK_EXP);
If Sign then
  _Result := _Result or F16_MASK_SIGN;
end;

//------------------------------------------------------------------------------

Function EncodeFloat16(Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False): Float16;
begin
EncodeFloat16Buffer(Result,Mantissa,Exponent,Sign,BiasedExp);
end;

//------------------------------------------------------------------------------

Function EncodeHalf(Mantissa: UInt16; Exponent: Int8; Sign: Boolean; BiasedExp: Boolean = False): Half;
begin
EncodeFloat16Buffer(Result,Mantissa,Exponent,Sign,BiasedExp);
end;

//------------------------------------------------------------------------------

procedure DecodeFloat16Buffer(const Buffer; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
var
  _Value: UInt16 absolute Buffer;
begin
Sign := (_Value and F16_MASK_SIGN) <> 0;
If BiasedExp then
  Exponent := (_Value and F16_MASK_EXP) shr 10
else
  Exponent := ((_Value and F16_MASK_EXP) shr 10) - FLOAT16_EXPONENTBIAS;
If IntBit then
  begin
    If ((_Value and F16_MASK_EXP) = 0){zero or denormal} then
      Mantissa := _Value and F16_MASK_FRAC
    else
      Mantissa := (_Value and F16_MASK_FRAC) or F16_MASK_INTB;
  end
else Mantissa := _Value and F16_MASK_FRAC;
end;

//------------------------------------------------------------------------------

procedure DecodeFloat16(const Value: Float16; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
begin
DecodeFloat16Buffer(Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
end;

//------------------------------------------------------------------------------

procedure DecodeHalf(const Value: Half; out Mantissa: UInt16; out Exponent: Int8; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
begin
DecodeFloat16Buffer(Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
end;

//==============================================================================

procedure MapToFloat32Buffer(out Buffer; Value: UInt32);
var
  _Result: UInt32 absolute Buffer;
begin
_Result := Value;
end;

//------------------------------------------------------------------------------

Function MapToFloat32(Value: UInt32): Float32;
begin
MapToFloat32Buffer(Result,Value);
end;

//------------------------------------------------------------------------------

Function MapToSingle(Value: UInt32): Single;
begin
MapToFloat32Buffer(Result,Value);
end;
 
//------------------------------------------------------------------------------

Function MapFromFloat32Buffer(const Buffer): UInt32;
var
  _Value: UInt32 absolute Buffer;
begin
Result := _Value;
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

procedure EncodeFloat32Buffer(out Buffer; Mantissa: UInt32; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False);
var
  _Result:  UInt32 absolute Buffer;
begin
_Result := Mantissa and F32_MASK_FRAC;
If BiasedExp then
  _Result := _Result or ((UInt32(ClampExp(Exponent,0,255)) shl 23) and F32_MASK_EXP)
else
  _Result := _Result or ((UInt32(ClampExp(Exponent,-127,128) + FLOAT32_EXPONENTBIAS) shl 23) and F32_MASK_EXP);
If Sign then
  _Result := _Result or F32_MASK_SIGN;
end;
  
//------------------------------------------------------------------------------

Function EncodeFloat32(Mantissa: UInt32; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False): Float32;
begin
EncodeFloat32Buffer(Result,Mantissa,Exponent,Sign,BiasedExp);
end;
  
//------------------------------------------------------------------------------

Function EncodeSingle(Mantissa: UInt32; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False): Single;
begin
EncodeFloat32Buffer(Result,Mantissa,Exponent,Sign,BiasedExp);
end;

//------------------------------------------------------------------------------

procedure DecodeFloat32Buffer(const Buffer; out Mantissa: UInt32; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
var
  _Value: UInt32 absolute Buffer;
begin
Sign := (_Value and F32_MASK_SIGN) <> 0;
If BiasedExp then
  Exponent := (_Value and F32_MASK_EXP) shr 23
else
  Exponent := ((_Value and F32_MASK_EXP) shr 23) - FLOAT32_EXPONENTBIAS;
If IntBit then
  begin
    If ((_Value and F32_MASK_EXP) = 0){zero or denormal} then
      Mantissa := _Value and F32_MASK_FRAC
    else
      Mantissa := (_Value and F32_MASK_FRAC) or F32_MASK_INTB;
  end
else Mantissa := _Value and F32_MASK_FRAC;
end;

//------------------------------------------------------------------------------

procedure DecodeFloat32(const Value: Float32; out Mantissa: UInt32; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
begin
DecodeFloat32Buffer(Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
end;

//------------------------------------------------------------------------------

procedure DecodeSingle(const Value: Single; out Mantissa: UInt32; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
begin
DecodeFloat32Buffer(Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
end;

{$IFDEF FPC}
{-------------------------------------------------------------------------------
================================================================================
                              Operators overloading
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    Operators overloading - implementation
===============================================================================}

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

{-------------------------------------------------------------------------------
================================================================================
                         Unit implementation management
================================================================================
-------------------------------------------------------------------------------}
var
  varImplManager: TImplementationManager = nil;  

//------------------------------------------------------------------------------

{$IFNDEF F16U_ASM_IMPL}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
Function UIM_CheckASMSupport(Func: TUIM_Float16Utils_Function): Boolean;
begin
Result := False;
{$IFDEF F16U_ASM_IMPL}
with TSimpleCPUID.Create do
try
  case Func of
    fnGetMXCSR,fnSetMXCSR,
    fnHalfToSingle,fnSingleToHalf,
    fnHalfToSingle4x,fnSingleToHalf4x:
      // SSE2 for MOVD instruction
      Result := Info.SupportedExtensions.F16C and Info.SupportedExtensions.SSE2 and
                Info.ProcessorFeatures.FXSR{FXSAVE used when obtaining MXCSR mask};
  else
    raise EF16UUnknownFunction.CreateFmt('UIM_CheckASMSupport: Unknown function (%d).',[Ord(Func)]);
  end;
finally
  Free;
end;
{$ENDIF}
end;
{$IFNDEF F16U_ASM_IMPL}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//==============================================================================

Function UIM_Float16Utils_AvailableFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementations;
begin
case Func of
  fnGetMXCSR,fnSetMXCSR,
  fnHalfToSingle,fnSingleToHalf,
  fnHalfToSingle4x,fnSingleToHalf4x:
    Result := [imNone,imPascal{$IFDEF F16U_ASM_IMPL},imAssembly{$ENDIF}];
else
  raise EF16UUnknownFunction.CreateFmt('UIM_Float16Utils_AvailableFuncImpl: Unknown function (%d).',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

Function UIM_Float16Utils_SupportedFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementations;
begin
Result := [imNone,imPascal];
case Func of
  fnGetMXCSR,fnSetMXCSR,
  fnHalfToSingle,fnSingleToHalf,
  fnHalfToSingle4x,fnSingleToHalf4x:  If UIM_CheckASMSupport(Func) then
                                        Include(Result,imAssembly);
else
  raise EF16UUnknownFunction.CreateFmt('UIM_Float16Utils_SupportedFuncImpl: Unknown function (%d).',[Ord(Func)]);
end;
end;

//------------------------------------------------------------------------------

Function UIM_Float16Utils_GetFuncImpl(Func: TUIM_Float16Utils_Function): TUIM_Float16Utils_Implementation;
var
  SelectedImplID: TUIMIdentifier;
begin
If varImplManager.FindObj(TUIMIdentifier(Func)).Selected(SelectedImplID) then
  Result := TUIM_Float16Utils_Implementation(SelectedImplID)
else
  raise EF16UNoImplementation.Create('UIM_Float16Utils_GetFuncImpl: No implementation selected.');
end;

//------------------------------------------------------------------------------

Function UIM_Float16Utils_SetFuncImpl(Func: TUIM_Float16Utils_Function; NewImpl: TUIM_Float16Utils_Implementation): TUIM_Float16Utils_Implementation;
begin
Result := UIM_Float16Utils_GetFuncImpl(Func);
varImplManager.FindObj(TUIMIdentifier(Func)).Select(TUIMIdentifier(NewImpl));
// make sure GetMXCSR and SetMXCSR have the same implementation selected
case Func of
  fnGetMXCSR: varImplManager.FindObj(TUIMIdentifier(fnSetMXCSR)).Select(TUIMIdentifier(NewImpl));
  fnSetMXCSR: varImplManager.FindObj(TUIMIdentifier(fnGetMXCSR)).Select(TUIMIdentifier(NewImpl));
end;
end;

{-------------------------------------------------------------------------------
================================================================================
                               Unit initialization
================================================================================
-------------------------------------------------------------------------------}

procedure UnitInitialize;
const
  NilPtr:   Pointer = nil;
  ImplsVar: array[TUIM_Float16Utils_Function] of PPointer = (@@Var_GetMXCSR,@@Var_SetMXCSR,
    @@Var_HalfToSingle,@@Var_SingleToHalf,@@Var_HalfToSingle4x,@@Var_SingleToHalf4x);
  ImplsPas: array[TUIM_Float16Utils_Function] of Pointer = (@Fce_GetMXCSR_Pas,@Fce_SetMXCSR_Pas,
    @Fce_HalfToSingle_Pas,@Fce_SingleToHalf_Pas,@Fce_HalfToSingle4x_Pas,@Fce_SingleToHalf4x_Pas);
{$IFDEF F16U_ASM_IMPL}
  ImplsAsm: array[TUIM_Float16Utils_Function] of Pointer = (@Fce_GetMXCSR_Asm,@Fce_SetMXCSR_Asm,
    @Fce_HalfToSingle_Asm,@Fce_SingleToHalf_Asm,@Fce_HalfToSingle4x_Asm,@Fce_SingleToHalf4x_Asm);
{$ENDIF}
var
  i:  TUIM_Float16Utils_Function;
begin
MXCSR_MASK_Init(not UIM_CheckASMSupport(fnGetMXCSR));
varImplManager := TImplementationManager.Create;
For i := Low(TUIM_Float16Utils_Function) to High(TUIM_Float16Utils_Function) do
  begin
    with varImplManager.AddObj(TUIMIdentifier(i),ImplsVar[i]^) do
      begin
        Add(TUIMIdentifier(imNone),NilPtr);
        Add(TUIMIdentifier(imPascal),ImplsPas[i],[ifSelect]);
      {$IFDEF F16U_ASM_IMPL}
        Add(TUIMIdentifier(imAssembly),ImplsAsm[i]);
      {$ELSE}
        AddAlias(TUIMIdentifier(imPascal),TUIMIdentifier(imAssembly));
      {$ENDIF}
      end;
    If UIM_CheckASMSupport(i) then
      UIM_Float16Utils_SetFuncImpl(i,imAssembly);
  end;
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
