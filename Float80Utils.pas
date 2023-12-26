{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Float80Utils

    Main aim of this library is to provide a mean of converting to and from
    double-extended-precision (80bit) floating point numbers.
    It is meant for environments where type Extended is declared only as an
    alias for type Double (64bit float) - typically 64bit applications where
    SSE, which does not support 80bit floats, is used as a primary FPU.

    Beyond the conversion routines, there are also some utilities - namely
    functions for number information or encoding/decoding the float80 type
    from/to its constituent parts (mantissa, exponent, sign).    

    From user perspective, there is not much difference, but it must be noted
    that the unit can be compiled in two modes, each totally different.

      First, default mode, is using assembly to directly access x87 FPU and does
      the conversion there.
      In this mode, auxiliry functions provided here (access to status and
      control word, exceptions masking and so on) operates directly on real x87
      registers and exceptions raising is also managed by the x87 FPU.

      Second is PurePascal mode. In it, a complete pascal implementation of
      conversion is used instead, so it can be called even on systems with no
      suitable FPU.
      Auxiliary functions are operating on local software implementation of
      status and control word and do not access the hardware.
      This implementation partially emulates the conversion how it is done on
      x87, including exception raising (exception masking and pre- and
      post-calculation nature of exceptions are honored) and changes given by
      selected rounding mode (note that precision mode does not affect
      conversions even on real x87 FPU).
      But remember it is only an emulation, not simulation - there are
      differences, notably condition codes are not affected by exceptions, and
      when raising an unmasked exception, the exception status bits are not set,
      summary flag bit is not set both for masked and unmasked exceptions.
      Top of the stack, busy and stack fault flags are outright ignored.

  Version 1.1 (2021-02-03)

  Last change 2023-12-19

  ©2020-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.Float80Utils

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes

===============================================================================}
unit Float80Utils; 
{
  Float80_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$IFDEF Float80_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IFDEF ENDIAN_BIG}
  // sadly, I have no way of developing for BE systems :(
  {$MESSAGE FATAL 'Big-endian architecture not supported'}
{$ENDIF}

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
  {$INLINE ON}
  {$DEFINE CanInline}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes {contains declaration of Float80 type};

{===============================================================================
    Library-specific exceptions - declaration
===============================================================================}

type
  EF80UException = class(Exception);

  EF80UInvalidFlag = class(EF80UException);

{-------------------------------------------------------------------------------
    Library-specific exceptions - FPU exceptions
-------------------------------------------------------------------------------}
type
  EF80UFPUException = class(EF80UException)
  protected
    fControlWord: UInt16;
    fStatusWord:  UInt16;
    Function DefaultMessage: String; virtual; abstract;
  public
    constructor CreateNoClear(const Msg: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
    constructor Create(const Msg: String);
    constructor CreateDefMsgNoClear({$IFNDEF FPC}Dummy: Integer = 0{$ENDIF});
    constructor CreateDefMsg;
  {
    ControlWord and StatusWord properties hold content of control word and
    status word registers respectively as they were when this exception was
    created - they can be used for example to probe masked signaled exceptions.
  }
    // StatusWord holds content of status word as it was when this exception
    // was created - can be used to probe masked signaled exceptions
    property ControlWord: UInt16 read fControlWord;
    property StatusWord: UInt16 read fStatusWord;
  end;

  // FPU stack errors
  EF80UStackFault = class(EF80UFPUException);

{-------------------------------------------------------------------------------
    Library-specific exceptions - individual FPU exception classes
-------------------------------------------------------------------------------}
type
  EF80UStackOverflow = class(EF80UStackFault)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80UStackUnderflow = class(EF80UStackFault)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80UInvalidOp = class(EF80UFPUException) // invalid operation/operand
  protected
    Function DefaultMessage: String; override;
  end;

  EF80UDenormal = class(EF80UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80UDivByZero = class(EF80UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80UOverflow = class(EF80UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80UUnderflow = class(EF80UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

  EF80UPrecision = class(EF80UFPUException)
  protected
    Function DefaultMessage: String; override;
  end;

{===============================================================================
    Auxiliary routines - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary routines - x87 status word access
-------------------------------------------------------------------------------}
{
  Note that x87 status register is read only - it can only be changed by
  clearing exceptions.
}

const
  // status word masks
  X87SW_EX_InvalidOP = UInt16($0001);
  X87SW_EX_Denormal  = UInt16($0002);
  X87SW_EX_DivByZero = UInt16($0004);
  X87SW_EX_Overflow  = UInt16($0008);
  X87SW_EX_Underflow = UInt16($0010);
  X87SW_EX_Precision = UInt16($0020);

  X87SW_StackFault       = UInt16($0040);
  X87SW_ExceptionSummary = UInt16($0080);
  X87SW_FPUBusy          = UInt16($8000);

  X87SW_ConditionCode_C0 = UInt16($0100);
  X87SW_ConditionCode_C1 = UInt16($0200);
  X87SW_ConditionCode_C2 = UInt16($0400);
  X87SW_ConditionCode_C3 = UInt16($4000);

  X87SW_TopOfStack = UInt16($3800); // bits 11..13

  X87SW_SHIFT_TopOfStack = 11;

//------------------------------------------------------------------------------

{
  EmulatedX87StatusWord

  Returns true when a real x87 status register is used, false when operating
  on an emulated local implementation of status word.
}
Function EmulatedX87StatusWord: Boolean;{$IF Defined(CanInline) and not Defined(FPC)} inline;{$IFEND}

{
  GetX87StatusWord

  Returns current value of status word.
}
Function GetX87StatusWord: UInt16;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - x87 control word access
-------------------------------------------------------------------------------}
const
  // control word masks
  X87CW_EMASK_InvalidOP = UInt16($0001);
  X87CW_EMASK_Denormal  = UInt16($0002);
  X87CW_EMASK_DivByZero = UInt16($0004);
  X87CW_EMASK_Overflow  = UInt16($0008);
  X87CW_EMASK_Underflow = UInt16($0010);
  X87CW_EMASK_Precision = UInt16($0020);

  X87CW_InfinityControl = UInt16($1000);

  X87CW_Precision = UInt16($0300);  // bits 8..9
  X87CW_Rounding  = UInt16($0C00);  // bits 10..11

  X87CW_SHIFT_Precision = 8;
  X87CW_SHIFT_Rounding  = 10;

//------------------------------------------------------------------------------
{
  EmulatedX87ControlWord

  Returns true when a real x87 control register is used, false when operating
  on an emulated local implementation of control word.
}
Function EmulatedX87ControlWord: Boolean;{$IFDEF CanInline} inline;{$ENDIF}

{
  GetX87ControlWord

  Returns current value of control word.
}
Function GetX87ControlWord: UInt16;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{
  SetX87ControlWord

  Set control word to a passed value.
}
procedure SetX87ControlWord(NewValue: UInt16);{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{
  Sets x87 control word to $1372 - denormal, underflow and precision exceptions
  are masked (others are unmasked), precision is set to extended, rounding is
  set to nearest and infinity control bit is 1.

  Call this routine only when the control word is NOT emulated (ie. a real CPU
  register is used) and the program is compiled so that x87 FPU is not used as
  primary mean of floating point arithmetics and/or is not automatically
  initialized (if the control word is $037F - a default value set by F(N)INIT
  instruction - you can safely assume it was not properly initialized).

  WARNING - the initialization must be done in each execution thread.
}
procedure InitX87ControlWord;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - abstracted SW and CW access
-------------------------------------------------------------------------------}
type
  TX87PrecisionMode = (pmSingle,pmReserved,pmDouble,pmExtended);

  TX87RoundingMode = (rmNearest,rmDown,rmUp,rmTruncate);

  TX87StatusFlag = (sfStackFault,sfExceptionSummary,sfFPUBusy,sfConditionCodeC0,
                    sfConditionCodeC1,sfConditionCodeC2,sfConditionCodeC3);

  TX87StatusFlags = set of TX87StatusFlag;

  TX87ControlFlag = (cfInfinityControl);

  TX87ControlFlags = set of TX87ControlFlag;

//------------------------------------------------------------------------------
{
  GetX87PrecisionMode

  Returns current value of precision mode from control word.
}
Function GetX87PrecisionMode: TX87PrecisionMode;

{
  SetX87PrecisionMode

  Sets precision mode to a selected NewValue and returns previous value of
  precision mode.
}
Function SetX87PrecisionMode(NewValue: TX87PrecisionMode): TX87PrecisionMode;

//------------------------------------------------------------------------------
{
  GetX87RoundingMode

  Returns current value of rounding mode from control word.
}
Function GetX87RoundingMode: TX87RoundingMode;

{
  SetX87RoundingMode

  Sets rounding mode to a selected NewValue and returns previous value of
  rounding mode.
}
Function SetX87RoundingMode(NewValue: TX87RoundingMode): TX87RoundingMode;

//------------------------------------------------------------------------------
{
  GetX87TopOfStack

  Returns current top of the stack pointer from the status word.
}
Function GetX87TopOfStack: Integer;

{
  SetX87TopOfStack

  Sets top of the stack to a selected NewValue and returns previous value of
  rounding mode.

  WARNING - it does not change the top of the stack when operating on real
            status register (ie. not in PurePascal mode) as status register is
            read-only.
}
Function SetX87TopOfStack(NewValue: Integer): Integer;

//------------------------------------------------------------------------------
{
  GetX87StatusFlag

  Returns current value of selected flag in the status word.
}
Function GetX87StatusFlag(Flag: TX87StatusFlag): Boolean;

{
  SetX87StatusFlag

  Sets value of selected flag in the status word to a NewValue and returns
  previous state of this flag.

  WARNING - it does not change the flag when operating on real status register
            (read-only status register).
}
Function SetX87StatusFlag(Flag: TX87StatusFlag; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetX87StatusFlags

  Returns status of all flags in the status word. When the flag is set, it is
  included in the result, when it is clear, it is excluded.
}
Function GetX87StatusFlags: TX87StatusFlags;

{
  SetX87StatusFlags

  Sets new status of all flags in the status word. If a flag is included
  in the NewValue, it will be set, when it is not included, it will be cleared.
  Return value is previous state of all status flags.

  WARNING - it does not change any flag when operating on real status register
            (read-only status register).
}
Function SetX87StatusFlags(NewValue: TX87StatusFlags): TX87StatusFlags;

//------------------------------------------------------------------------------
{
  GetX87ControlFlag

  Returns current value of selected flag in the control word.
}
Function GetX87ControlFlag(Flag: TX87ControlFlag): Boolean;

{
  SetX87ControlFlag

  Sets value of selected flag in the control word to a NewValue and returns
  previous state of this flag.
}
Function SetX87ControlFlag(Flag: TX87ControlFlag; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetX87ControlFlags

  Returns status of all flags in the control word. When the flag is set, it is
  included in the result, when it is clear, it is excluded.
}
Function GetX87ControlFlags: TX87ControlFlags;

{
  SetX87ControlFlags

  Sets new status of all flags in the control word. If a flag is included
  in the NewValue, it will be set, when it is not included, it will be cleared.
  Return value is previous state of all control flags.
}
Function SetX87ControlFlags(NewValue: TX87ControlFlags): TX87ControlFlags;

{-------------------------------------------------------------------------------
    Auxiliary routines - abstracted x87 exception flags access
-------------------------------------------------------------------------------}
type
  TX87Exception = (excInvalidOp,excDenormal,excDivByZero,excOverflow,
                   excUnderflow,excPrecision);

  TX87Exceptions = set of TX87Exception;

const
  AllX87Exceptions = [excInvalidOp,excDenormal,excDivByZero,excOverflow,
                      excUnderflow,excPrecision];

//------------------------------------------------------------------------------
{
  GetX87ExceptionMask

  Returns current value of selected exception mask bit.
}
Function GetX87ExceptionMask(Exception: TX87Exception): Boolean;

{
  SetX87ExceptionMask

  Sets value of selected exception mask bit in the control word to a NewValue
  and returns previous value of this bit.

  When the bit is set (true), the selected exception will be masked and not
  raised on its occurence.
  When clear (false), the exception is unmasked and can be raised.
}
Function SetX87ExceptionMask(Exception: TX87Exception; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetX87ExceptionMasks

  Returns status of all exception mask bits in the control word. When the bit
  is set, the exception is included in the result, when it is clear, the
  exception is excluded from the result.
}
Function GetX87ExceptionMasks: TX87Exceptions;

{
  SetX87ExceptionMasks

  Sets new value of all exception mask bits in the control word. If an
  exception is included in the NewValue, the mask bit will be set, when it is
  not included, the mask bit will be cleared.

  Return value is previous state of all exception mask bits.
}
Function SetX87ExceptionMasks(NewValue: TX87Exceptions): TX87Exceptions;

//------------------------------------------------------------------------------
{
  GetX87ExceptionFlag

  Returns current value of selected exception flag bit.
}
Function GetX87ExceptionFlag(Exception: TX87Exception): Boolean;

{
  SetX87ExceptionFlag

  Sets value of selected exception flag bit in the status word to a NewValue
  and returns previous value of this bit.

  WARNING - changes nothing when operating on real status register as it is
            read-only.
}
Function SetX87ExceptionFlag(Exception: TX87Exception; NewValue: Boolean): Boolean;

//------------------------------------------------------------------------------
{
  GetX87ExceptionFlags

  Returns status of all exception flag bits in the status word. When the bit
  is set, the exception is included in the result, when it is clear, the
  exception is excluded from the result.
}
Function GetX87ExceptionFlags: TX87Exceptions;

{
  SetX87ExceptionFlags

  Sets new value of all exception flag bits in the status word. If an exception
  is included in the NewValue, the flag bit will be set, when it is not
  included, the flag bit will be cleared.

  Return value is previous state of all exception flag bits.

  WARNING - changes nothing when operating on real status register.
}
Function SetX87ExceptionFlags(NewValue: TX87Exceptions): TX87Exceptions;

//------------------------------------------------------------------------------
{
  ClearX87Exceptions

  In PurePascal it completely clears the status word (resets all exception
  flag bits, sets top of stack to 0, clears all condition codes and other
  flags)

  When operating on real status register, it just executes FCLEX instruction
  (it clears exception flag bits, FPU busy flag, summary status flag and
  stack fault flag, condition codes and top of the stack are undefined).
}
procedure ClearX87Exceptions;{$IFNDEF PurePascal} register; assembler;{$ENDIF}

{
  RaiseX87Exceptions

  Raises first encountered exception according to flags set in the passed
  status word.

  Parameter Mask controls whether to honor exception masking (true) or not
  (false) when raising an exception (when honored, the masked exceptions are
  NOT raised, when not honored, all exceptions can be raised, even those
  masked).
  Mask bits are taken from the parameter ConrolWord, not from the actual
  register.

  The exception flag bits are traversed one by one and, when a set bit is
  encountered, it is cleared and a corresponding exception is raised (if
  allowed by masking - see parameter Mask).
  Only one exception is raised in each call, even when multiple bits are set.
  The order in which the bits are traversed and therefore the order of
  exceptions raising is:

    InvalidOP/StackUnderflow/StackOverflow (they all fall in one group)
    Denormal
    DivByZero
    Underflow
    Overflow
    Precision
}
procedure RaiseX87Exceptions(var StatusWord: UInt16; Mask: Boolean = True; ConrolWord: UInt16 = 0); overload;

{
  RaiseX87Exceptions

  Calls the first overload with an input being current value of status word
  (be it a real register or emulation).

  Note that this function will NOT change the status word.
}
procedure RaiseX87Exceptions(Mask: Boolean = True); overload;

{===============================================================================
--------------------------------------------------------------------------------
                         Float80 <-> Float64 conversions
--------------------------------------------------------------------------------
===============================================================================}
type
  // overlay for easier work with 10-byte extended precision float
  TFloat80Overlay = packed record
    case Integer of
      0:  (Part_64:       UInt64;
           Part_16:       UInt16);
      1:  (Mantissa:      UInt64;
           SignExponent:  UInt16);
      2:  (Words:         array[0..4] of UInt16);
      3:  (Bytes:         array[0..9] of UInt8);
  end;
  PFloat80Overlay = ^TFloat80Overlay;

const
  FLOAT64_EXPONENTBIAS = 1023;
  FLOAT80_EXPONENTBIAS = 16383;
  
{===============================================================================
    Conversion routines - declaration
===============================================================================}

procedure Float64ToFloat80(Float64Ptr,Float80Ptr: Pointer);{$IFNDEF PurePascal} register; assembler;{$ENDIF} overload;

Function Float64ToFloat80(Value: Float64): Float80;{$IFDEF CanInline} inline;{$ENDIF} overload;

procedure DoubleToExtended(DoublePtr,ExtendedPtr: Pointer);{$IFDEF CanInline} inline;{$ENDIF} overload;

Function DoubleToExtended(Value: Float64): Float80;{$IFDEF CanInline} inline;{$ENDIF} overload;

//------------------------------------------------------------------------------

procedure Float80ToFloat64(Float80Ptr,Float64Ptr: Pointer);{$IFNDEF PurePascal} register; assembler;{$ENDIF} overload;

Function Float80ToFloat64(Value: Float80): Float64;{$IFDEF CanInline} inline;{$ENDIF} overload;

procedure ExtendedToDouble(ExtendedPtr,DoublePtr: Pointer);{$IFDEF CanInline} inline;{$ENDIF} overload;

Function ExtendedToDouble(Value: Float80): Float64;{$IFDEF CanInline} inline;{$ENDIF} overload;

{===============================================================================
--------------------------------------------------------------------------------
                               Float80 utilities                                                                                           
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Utility routines - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Utility routines - number information functions
-------------------------------------------------------------------------------}

Function IsValid(const Value: Float80): Boolean;

Function IsZero(const Value: Float80): Boolean;
Function IsDenormal(const Value: Float80): Boolean;
Function IsNaN(const Value: Float80): Boolean;
Function IsInfinite(const Value: Float80): Boolean;
Function IsNormal(const Value: Float80): Boolean; // returns false on zero 

Function IsPseudoValue(const Value: Float80): Boolean;{$IFDEF CanInline} inline;{$ENDIF}  // not IsValid

Function IsPseudoDenormal(const Value: Float80): Boolean;
Function IsPseudoNaN(const Value: Float80): Boolean;
Function IsPseudoInfinity(const Value: Float80): Boolean;
Function IsUnnormal(const Value: Float80): Boolean;

{-------------------------------------------------------------------------------
    Utility routines - sign-related functions
-------------------------------------------------------------------------------}
type
  TFloat80ValueSign = -1..1;

{
  Following three routines will raise and EF80UInvalidOp exception when an
  invalidly encoded number is passed.
}
Function Sign(const Value: Float80): TFloat80ValueSign;
Function Abs(const Value: Float80): Float80;
Function Neg(const Value: Float80): Float80;

{-------------------------------------------------------------------------------
    Utility routines - floats encoding/decoding
-------------------------------------------------------------------------------}

procedure MapToFloat80Buffer(out Buffer; High16: UInt16; Low64: UInt64);
Function MapToFloat80(High16: UInt16; Low64: UInt64): Float80;{$IFDEF CanInline} inline;{$ENDIF}
Function MapToExtended(High16: UInt16; Low64: UInt64): Extended;

procedure MapFromFloat80Buffer(const Buffer; out High16: UInt16; out Low64: UInt64);
procedure MapFromFloat80(const Value: Float80; out High16: UInt16; out Low64: UInt64);{$IFDEF CanInline} inline;{$ENDIF}
procedure MapFromExtended(const Value: Extended; out High16: UInt16; out Low64: UInt64);

//------------------------------------------------------------------------------
{
  EncodeFloat80Buffer
  EncodeFloat80
  EncodeExtended

  When BiasedExp is true, it indicates that the passed exponent is already
  biased and will be stored as is. When false, the passed exponent will be
  biased before storing.

    NOTE - the valid range for exponent is -16383..+16384 when biased, 0..32767
           when unbiased. The exponent is clamped (limited to a prescribed
           range) before biasing and storing.

  When IntBit is true, it indicates that the passed mantissa contains the
  integer bit (bit 63) and will be stored as is. When false, the integer bit
  in passed mantissa is ignored and its value for storage is implied from the
  exponent.
}
procedure EncodeFloat80Buffer(out Buffer; Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
Function EncodeFloat80(Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True): Float80; {$IFDEF CanInline} inline;{$ENDIF}
Function EncodeExtended(Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True): Extended;{$IFDEF CanInline} inline;{$ENDIF}

{
  DecodeFloat80Buffer
  DecodeFloat80
  DecodeExtended

  When BiasedExp is set to true, the returned exponent is exponent as is
  stored in the value, that is, biased. When false, the returned exponent is
  unbiased (its true value).

    NOTE - returned exponent will be within range of -16383..+16384 when
           biased, 0..32767 when unbiased.

  When IntBit is set to true, the returned mantissa contains the integer bit
  (bit 63) as it is stored in the number. When false, the integer bit is
  masked-out and is zero, irrespective of its actual value.
}
procedure DecodeFloat80Buffer(const Buffer; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
procedure DecodeFloat80(const Value: Float80; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);{$IFDEF CanInline} inline;{$ENDIF}
procedure DecodeExtended(const Value: Extended; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  Since conversions in this library are vorking with double-precision (64bit)
  floats, one might want to decode/encode these floats too. Threfore...
}

procedure MapToFloat64Buffer(out Buffer; Value: UInt64);
Function MapToFloat64(Value: UInt64): Float64;{$IFDEF CanInline} inline;{$ENDIF}
Function MapToDouble(Value: UInt64): Double;{$IFDEF CanInline} inline;{$ENDIF}

Function MapFromFloat64Buffer(const Buffer): UInt64;
Function MapFromFloat64(const Value: Float64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
Function MapFromDouble(const Value: Double): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------
{
  EncodeFloat64Buffer
  EncodeFloat64
  EncodeDouble

  When BiasedExp is true, it indicates that the passed exponent is already
  biased and will be stored as is. When false, the passed exponent will be
  biased before storing.

    NOTE - the valid range for exponent is -1023..+1024 when biased, 0..2047
           when unbiased. The exponent is clamped (limited to a prescribed
           range) before biasing and storing.

  Integer bit, when passed in the mantissa, is ignored - it is implied for
  double-precision float.

    NOTE - only lowest 52 bits of the mantissa are used, other bits gets
           masked-out before storage.
}
procedure EncodeFloat64Buffer(out Buffer; Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False);
Function EncodeFloat64(Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False): Float64;{$IFDEF CanInline} inline;{$ENDIF}
Function EncodeDouble(Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False): Double;{$IFDEF CanInline} inline;{$ENDIF}

{
  DecodeFloat64Buffer
  DecodeFloat64
  DecodeDouble

  When BiasedExp is set to true, the returned exponent is exponent as is
  stored in the value, that is, biased. When false, the returned exponent is
  unbiased (its true value).

    NOTE - returned exponent will be within range of -1023..+1024 when biased,
           0..2047 when unbiased.

  When IntBit is set to true, the returned mantissa contains the integer bit
  (bit 52) inferred from the number class (0 for denormals and zero,
  1 otherwise). When false, the integer bit is masked-out and is zero,
  irrespective of actual value.

    NOTE - only lowest 52 bits (53 with integer bit) of the mantissa are valid,
           other bits will always be zero.
}
procedure DecodeFloat64Buffer(const Buffer; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
procedure DecodeFloat64(const Value: Float64; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);{$IFDEF CanInline} inline;{$ENDIF}
procedure DecodeDouble(const Value: Double; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);{$IFDEF CanInline} inline;{$ENDIF}

implementation

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}}   // Parameter "$1" not used
{$ENDIF}

// do not place this any higher
{$IF SizeOf(Extended) = 8}
  {$DEFINE Extended64}
{$ELSEIF SizeOf(Extended) = 10}
  {$UNDEF Extended64}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported platform, type extended must be 8 or 10 bytes.'}
{$IFEND}

{===============================================================================
    Internal constants and types
===============================================================================}
const
  F64_MASK_SIGN = UInt64($8000000000000000);  // sign bit
{$IFNDEF FPC}
  F64_MASK_NSGN = UInt64($7FFFFFFFFFFFFFFF);  // non-sign bits
{$ENDIF}
  F64_MASK_EXP  = UInt64($7FF0000000000000);  // exponent
  F64_MASK_FRAC = UInt64($000FFFFFFFFFFFFF);  // fraction/mantissa
{$IFDEF PurePascal}
  F64_MASK_FHB  = UInt64($0008000000000000);  // highest bit of the mantissa
{$ENDIF}
  F64_MASK_INTB = UInt64($0010000000000000);  // integer bit of the mantissa

  F80_MASK16_SIGN = UInt16($8000);
  F80_MASK16_NSGN = UInt16($7FFF);
  F80_MASK16_EXP  = UInt16($7FFF);
  F80_MASK64_FRAC = UInt64($7FFFFFFFFFFFFFFF);
{$IFDEF PurePascal}
  F80_MASK64_FHB  = UInt64($4000000000000000);
{$ENDIF}
  F80_MASK64_INTB = UInt64($8000000000000000);

{===============================================================================
    Library-specific exceptions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Library-specific exceptions - FPU exceptions
-------------------------------------------------------------------------------}

constructor EF80UFPUException.CreateNoClear(const Msg: String{$IFNDEF FPC}; Dummy: Integer = 0{$ENDIF});
begin
inherited Create(Msg);
fStatusWord := GetX87StatusWord;
end;

//------------------------------------------------------------------------------

constructor EF80UFPUException.Create(const Msg: String);
begin
CreateNoClear(Msg);
If EmulatedX87StatusWord then
  ClearX87Exceptions;
end;

//------------------------------------------------------------------------------

constructor EF80UFPUException.CreateDefMsgNoClear({$IFNDEF FPC}Dummy: Integer = 0{$ENDIF});
begin
CreateNoClear(DefaultMessage);
end;

//------------------------------------------------------------------------------

constructor EF80UFPUException.CreateDefMsg;
begin
Create(DefaultMessage);
end;

{-------------------------------------------------------------------------------
    Library-specific exceptions - individual FPU exception classes
-------------------------------------------------------------------------------}

Function EF80UStackOverflow.DefaultMessage: String;
begin
Result := 'Floting point unit stack overflow';
end;

//------------------------------------------------------------------------------

Function EF80UStackUnderflow.DefaultMessage: String;
begin
Result := 'Floting point unit stack underflow';
end;

//------------------------------------------------------------------------------

Function EF80UInvalidOp.DefaultMessage: String;
begin
Result := 'Invalid floating point operand';
end;

//------------------------------------------------------------------------------

Function EF80UDenormal.DefaultMessage: String;
begin
Result := 'Denormal floating point operand';
end;

//------------------------------------------------------------------------------

Function EF80UDivByZero.DefaultMessage: String;
begin
Result := 'Floating point division by zero';
end;

//------------------------------------------------------------------------------

Function EF80UOverflow.DefaultMessage: String;
begin
Result := 'Floating point arithmetic overflow';
end;

//------------------------------------------------------------------------------

Function EF80UUnderflow.DefaultMessage: String;
begin
Result := 'Floating point arithmetic underflow';
end;

//------------------------------------------------------------------------------

Function EF80UPrecision.DefaultMessage: String;
begin
Result := 'Inexact floating point result';
end;


{===============================================================================
    Auxiliary routines - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary routines - x87 status word access
-------------------------------------------------------------------------------}

{$IFDEF PurePascal}
threadvar
  Pas_X87SW:  UInt16;
  SWInit:     Boolean;  // initalized by the compiler to false
{$ENDIF}

//------------------------------------------------------------------------------

Function EmulatedX87StatusWord: Boolean;
begin
{$IFDEF PurePascal}
Result := True;
{$ELSE}
Result := False;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetX87StatusWord: UInt16;
{$IFNDEF PurePascal}
asm
    FSTSW   AX
end;
{$ELSE}
begin
If not SWInit then
  begin
    Pas_X87SW := 0;
    SWInit := True;
  end;
Result := Pas_X87SW;
end;
{$ENDIF}

{-------------------------------------------------------------------------------
    Auxiliary routines - x87 control word access
-------------------------------------------------------------------------------}

{$IFDEF PurePascal}
threadvar
  Pas_X87CW:  UInt16;
  CWInit:     Boolean;  // initalized by the compiler to false
{$ENDIF}

//------------------------------------------------------------------------------

Function EmulatedX87ControlWord: Boolean;
begin
{$IFDEF PurePascal}
Result := True;
{$ELSE}
Result := False;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetX87ControlWord: UInt16;
{$IFNDEF PurePascal}
var
  Temp: UInt16;
asm
    FSTCW   word ptr [Temp]
    MOV     AX, word ptr [Temp]
end;
{$ELSE}
begin
If not CWInit then
  begin
  {
    denormal, underflow and precision exceptions are masked, precision set to
    extended, rounding set to nearest and infinity control bit set
  }
    Pas_X87CW := $1372;
    CWInit := True;
  end;
Result := Pas_X87CW;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure SetX87ControlWord(NewValue: UInt16);
{$IFNDEF PurePascal}
var
  Temp: UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    MOV     word ptr [Temp], CX
  {$ELSE}
    MOV     word ptr [Temp], DI
  {$ENDIF}
{$ELSE}
    MOV     word ptr [Temp], AX
{$ENDIF}
    FLDCW   word ptr [Temp]
end;
{$ELSE}
begin
Pas_X87CW := NewValue;
CWInit := True;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure InitX87ControlWord;
begin
SetX87ControlWord($1372);
end;

{-------------------------------------------------------------------------------
    Auxiliary routines - abstracted SW and CW access
-------------------------------------------------------------------------------}

Function GetX87PrecisionMode: TX87PrecisionMode;
begin
case (GetX87ControlWord and X87CW_Precision) shr X87CW_SHIFT_Precision of
  0:  Result := pmSingle;
  2:  Result := pmDouble;
  3:  Result := pmExtended;
else
  Result := pmReserved;
end;
end;

//------------------------------------------------------------------------------

Function SetX87PrecisionMode(NewValue: TX87PrecisionMode): TX87PrecisionMode;
var
  Num:  UInt16;
begin
Result := GetX87PrecisionMode;
case NewValue of
  pmSingle:   Num := 0;
  pmDouble:   Num := 2;
  pmExtended: Num := 3;
else
  Num := 1;
end;
SetX87ControlWord((GetX87ControlWord and not X87CW_Precision) or UInt16(Num shl X87CW_SHIFT_Precision));
end;

//------------------------------------------------------------------------------

Function GetX87RoundingMode: TX87RoundingMode;
begin
case (GetX87ControlWord and X87CW_Rounding) shr X87CW_SHIFT_Rounding of
  1:  Result := rmDown;
  2:  Result := rmUp;
  3:  Result := rmTruncate;
else
  Result := rmNearest;
end;
end;

//------------------------------------------------------------------------------

Function SetX87RoundingMode(NewValue: TX87RoundingMode): TX87RoundingMode;
var
  Num:  UInt16;
begin
Result := GetX87RoundingMode;
case NewValue of
  rmDown:     Num := 1;
  rmUp:       Num := 2;
  rmTruncate: Num := 3;
else
  Num := 0;
end;
SetX87ControlWord((GetX87ControlWord and not X87CW_Rounding) or UInt16(Num shl X87CW_SHIFT_Rounding));
end;

//------------------------------------------------------------------------------

Function GetX87TopOfStack: Integer;
begin
Result := (GetX87StatusWord and X87SW_TopOfStack) shr X87SW_SHIFT_TopOfStack;
end;

//------------------------------------------------------------------------------

{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
Function SetX87TopOfStack(NewValue: Integer): Integer;
begin
Result := GetX87TopOfStack; // initializes Pas_X87SW
{$IFDEF PurePascal}
Pas_X87SW := (Pas_X87SW and not X87SW_TopOfStack) or ((NewValue shl X87SW_SHIFT_TopOfStack) and X87SW_TopOfStack);
{$ENDIF}
end;
{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//------------------------------------------------------------------------------

Function GetX87StatusFlag(Flag: TX87StatusFlag): Boolean;
begin
case Flag of
  sfStackFault:       Result := (GetX87StatusWord and X87SW_StackFault) <> 0;
  sfExceptionSummary: Result := (GetX87StatusWord and X87SW_ExceptionSummary) <> 0;
  sfFPUBusy:          Result := (GetX87StatusWord and X87SW_FPUBusy) <> 0;
  sfConditionCodeC0:  Result := (GetX87StatusWord and X87SW_ConditionCode_C0) <> 0;
  sfConditionCodeC1:  Result := (GetX87StatusWord and X87SW_ConditionCode_C1) <> 0;
  sfConditionCodeC2:  Result := (GetX87StatusWord and X87SW_ConditionCode_C2) <> 0;
  sfConditionCodeC3:  Result := (GetX87StatusWord and X87SW_ConditionCode_C3) <> 0;
else
  raise EF80UInvalidFlag.CreateFmt('GetX87StatusFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
Function SetX87StatusFlag(Flag: TX87StatusFlag; NewValue: Boolean): Boolean;
{$IFDEF PurePascal}

  procedure SetBit(BitMask: UInt16);
  begin
    If NewValue then
      Pas_X87SW:= Pas_X87SW or BitMask
    else
      Pas_X87SW:= Pas_X87SW and not BitMask;
  end;

begin
Result := GetX87StatusFlag(Flag);
case Flag of
  sfStackFault:       SetBit(X87SW_StackFault);
  sfExceptionSummary: SetBit(X87SW_ExceptionSummary);
  sfFPUBusy:          SetBit(X87SW_FPUBusy);
  sfConditionCodeC0:  SetBit(X87SW_ConditionCode_C0);
  sfConditionCodeC1:  SetBit(X87SW_ConditionCode_C1);
  sfConditionCodeC2:  SetBit(X87SW_ConditionCode_C2);
  sfConditionCodeC3:  SetBit(X87SW_ConditionCode_C3);
else
  raise EF80UInvalidFlag.CreateFmt('SetX87StatusFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;
{$ELSE}
begin
Result := GetX87StatusFlag(Flag);
end;
{$ENDIF}
{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//------------------------------------------------------------------------------

Function GetX87StatusFlags: TX87StatusFlags;
var
  SW: UInt16;
  i:  TX87StatusFlag;
begin
Result := [];
SW := GetX87StatusWord;
For i := Low(TX87StatusFlag) to High(TX87StatusFlag) do
  case i of
    sfStackFault:       If (SW and X87SW_StackFault) <> 0 then Include(Result,i);
    sfExceptionSummary: If (SW and X87SW_ExceptionSummary) <> 0 then Include(Result,i);
    sfFPUBusy:          If (SW and X87SW_FPUBusy) <> 0 then Include(Result,i);
    sfConditionCodeC0:  If (SW and X87SW_ConditionCode_C0) <> 0 then Include(Result,i);
    sfConditionCodeC1:  If (SW and X87SW_ConditionCode_C1) <> 0 then Include(Result,i);
    sfConditionCodeC2:  If (SW and X87SW_ConditionCode_C2) <> 0 then Include(Result,i);
    sfConditionCodeC3:  If (SW and X87SW_ConditionCode_C3) <> 0 then Include(Result,i);
  else
    raise EF80UInvalidFlag.CreateFmt('GetX87StatusFlags: Invalid flag (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
Function SetX87StatusFlags(NewValue: TX87StatusFlags): TX87StatusFlags;
{$IFDEF PurePascal}

  procedure SetBit(BitMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      Pas_X87SW := Pas_X87SW or BitMask
    else
      Pas_X87SW := Pas_X87SW and not BitMask;
  end;

begin
Result := GetX87StatusFlags;
SetBit(X87SW_StackFault,sfStackFault in NewValue);
SetBit(X87SW_ExceptionSummary,sfExceptionSummary in NewValue);
SetBit(X87SW_FPUBusy,sfFPUBusy in NewValue);
SetBit(X87SW_ConditionCode_C0,sfConditionCodeC0 in NewValue);
SetBit(X87SW_ConditionCode_C1,sfConditionCodeC1 in NewValue);
SetBit(X87SW_ConditionCode_C2,sfConditionCodeC2 in NewValue);
SetBit(X87SW_ConditionCode_C3,sfConditionCodeC3 in NewValue);
end;
{$ELSE}
begin
Result := GetX87StatusFlags;
end;
{$ENDIF}
{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//------------------------------------------------------------------------------

Function GetX87ControlFlag(Flag: TX87ControlFlag): Boolean;
begin
case Flag of
  cfInfinityControl:  Result := (GetX87ControlWord and X87CW_InfinityControl) <> 0;
else
  raise EF80UInvalidFlag.CreateFmt('GetX87ControlFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function SetX87ControlFlag(Flag: TX87ControlFlag; NewValue: Boolean): Boolean;

  procedure SetBit(BitMask: UInt16);
  begin
    If NewValue then
      SetX87ControlWord(GetX87ControlWord or BitMask)
    else
      SetX87ControlWord(GetX87ControlWord and not BitMask);
  end;

begin
Result := GetX87ControlFlag(Flag);
case Flag of
  cfInfinityControl:  SetBit(X87CW_InfinityControl);
else
  raise EF80UInvalidFlag.CreateFmt('SetX87ControlFlag: Invalid flag (%d).',[Ord(Flag)]);
end;
end;

//------------------------------------------------------------------------------

Function GetX87ControlFlags: TX87ControlFlags;
var
  CW: UInt16;
  i:  TX87ControlFlag;
begin
Result := [];
CW := GetX87ControlWord;
For i := Low(TX87ControlFlag) to High(TX87ControlFlag) do
  case i of
    cfInfinityControl:  If (CW and X87CW_InfinityControl) <> 0 then Include(Result,i);
  else
    raise EF80UInvalidFlag.CreateFmt('GetX87ControlFlags: Invalid flag (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetX87ControlFlags(NewValue: TX87ControlFlags): TX87ControlFlags;
var
  CW: UInt16;

  procedure SetBit(BitMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      CW := CW or BitMask
    else
      CW := CW and not BitMask;
  end;

begin
Result := GetX87ControlFlags;
CW := GetX87ControlWord;
SetBit(X87CW_InfinityControl,cfInfinityControl in NewValue);
SetX87ControlWord(CW);
end;

{-------------------------------------------------------------------------------
    Auxiliary routines - abstracted x87 exception flags access
-------------------------------------------------------------------------------}

Function GetX87ExceptionMask(Exception: TX87Exception): Boolean;
begin
case Exception of
  excInvalidOp: Result := (GetX87ControlWord and X87CW_EMASK_InvalidOP) <> 0;
  excDenormal:  Result := (GetX87ControlWord and X87CW_EMASK_Denormal) <> 0;
  excDivByZero: Result := (GetX87ControlWord and X87CW_EMASK_DivByZero) <> 0;
  excOverflow:  Result := (GetX87ControlWord and X87CW_EMASK_Overflow) <> 0;
  excUnderflow: Result := (GetX87ControlWord and X87CW_EMASK_Underflow) <> 0;
  excPrecision: Result := (GetX87ControlWord and X87CW_EMASK_Precision) <> 0;
else
  raise EF80UInvalidFlag.CreateFmt('GetX87ExceptionMask: Invalid X87 exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function SetX87ExceptionMask(Exception: TX87Exception; NewValue: Boolean): Boolean;

  procedure SetBit(BitMask: UInt16);
  begin
    If NewValue then
      SetX87ControlWord(GetX87ControlWord or BitMask)
    else
      SetX87ControlWord(GetX87ControlWord and not BitMask);
  end;

begin
Result := GetX87ExceptionMask(Exception);
case Exception of
  excInvalidOp: SetBit(X87CW_EMASK_InvalidOP);
  excDenormal:  SetBit(X87CW_EMASK_Denormal);
  excDivByZero: SetBit(X87CW_EMASK_DivByZero);
  excOverflow:  SetBit(X87CW_EMASK_Overflow);
  excUnderflow: SetBit(X87CW_EMASK_Underflow);
  excPrecision: SetBit(X87CW_EMASK_Precision);
else
  raise EF80UInvalidFlag.CreateFmt('SetX87ExceptionMask: Invalid X87 exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

Function GetX87ExceptionMasks: TX87Exceptions;
var
  CW: UInt16;
  i:  TX87Exception;
begin
Result := [];
CW := GetX87ControlWord;
For i := Low(TX87Exception) to High(TX87Exception) do
  case i of
    excInvalidOp: If (CW and X87CW_EMASK_InvalidOP) <> 0 then Include(Result,i);
    excDenormal:  If (CW and X87CW_EMASK_Denormal) <> 0 then Include(Result,i);
    excDivByZero: If (CW and X87CW_EMASK_DivByZero) <> 0 then Include(Result,i);
    excOverflow:  If (CW and X87CW_EMASK_Overflow) <> 0 then Include(Result,i);
    excUnderflow: If (CW and X87CW_EMASK_Underflow) <> 0 then Include(Result,i);
    excPrecision: If (CW and X87CW_EMASK_Precision) <> 0 then Include(Result,i);
  else
    raise EF80UInvalidFlag.CreateFmt('GetX87ExceptionMasks: Invalid X87 exception (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

Function SetX87ExceptionMasks(NewValue: TX87Exceptions): TX87Exceptions;
var
  CW: UInt16;

  procedure SetBit(BitMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      CW := CW or BitMask
    else
      CW := CW and not BitMask;
  end;

begin
Result := GetX87ExceptionMasks;
CW := GetX87ControlWord;
SetBit(X87CW_EMASK_InvalidOP,excInvalidOp in NewValue);
SetBit(X87CW_EMASK_Denormal,excDenormal in NewValue);
SetBit(X87CW_EMASK_DivByZero,excDivByZero in NewValue);
SetBit(X87CW_EMASK_Overflow,excOverflow in NewValue);
SetBit(X87CW_EMASK_Underflow,excUnderflow in NewValue);
SetBit(X87CW_EMASK_Precision,excPrecision in NewValue);
SetX87ControlWord(CW);
end;

//------------------------------------------------------------------------------

Function GetX87ExceptionFlag(Exception: TX87Exception): Boolean;
begin
case Exception of
  excInvalidOp: Result := (GetX87StatusWord and X87SW_EX_InvalidOP) <> 0;
  excDenormal:  Result := (GetX87StatusWord and X87SW_EX_Denormal) <> 0;
  excDivByZero: Result := (GetX87StatusWord and X87SW_EX_DivByZero) <> 0;
  excOverflow:  Result := (GetX87StatusWord and X87SW_EX_Overflow) <> 0;
  excUnderflow: Result := (GetX87StatusWord and X87SW_EX_Underflow) <> 0;
  excPrecision: Result := (GetX87StatusWord and X87SW_EX_Precision) <> 0;
else
  raise EF80UInvalidFlag.CreateFmt('GetX87ExceptionFlag: Invalid X87 exception (%d).',[Ord(Exception)]);
end;
end;

//------------------------------------------------------------------------------

{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
Function SetX87ExceptionFlag(Exception: TX87Exception; NewValue: Boolean): Boolean;
{$IFDEF PurePascal}

  procedure SetBit(BitMask: UInt16);
  begin
    If NewValue then
      Pas_X87SW := Pas_X87SW or BitMask
    else
      Pas_X87SW := Pas_X87SW and not BitMask
  end;

begin
Result := GetX87ExceptionFlag(Exception);
case Exception of
  excInvalidOp: SetBit(X87SW_EX_InvalidOP);
  excDenormal:  SetBit(X87SW_EX_Denormal);
  excDivByZero: SetBit(X87SW_EX_DivByZero);
  excOverflow:  SetBit(X87SW_EX_Overflow);
  excUnderflow: SetBit(X87SW_EX_Underflow);
  excPrecision: SetBit(X87SW_EX_Precision);
else
  raise EF80UInvalidFlag.CreateFmt('SetX87ExceptionFlag: Invalid X87 exception (%d).',[Ord(Exception)]);
end;
end;
{$ELSE}
begin
Result := GetX87ExceptionFlag(Exception);
end;
{$ENDIF}
{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//------------------------------------------------------------------------------

Function GetX87ExceptionFlags: TX87Exceptions;
var
  SW: UInt16;
  i:  TX87Exception;
begin
Result := [];
SW := GetX87StatusWord;
For i := Low(TX87Exception) to High(TX87Exception) do
  case i of
    excInvalidOp: If (SW and X87SW_EX_InvalidOP) <> 0 then Include(Result,i);
    excDenormal:  If (SW and X87SW_EX_Denormal) <> 0 then Include(Result,i);
    excDivByZero: If (SW and X87SW_EX_DivByZero) <> 0 then Include(Result,i);
    excOverflow:  If (SW and X87SW_EX_Overflow) <> 0 then Include(Result,i);
    excUnderflow: If (SW and X87SW_EX_Underflow) <> 0 then Include(Result,i);
    excPrecision: If (SW and X87SW_EX_Precision) <> 0 then Include(Result,i);
  else
    raise EF80UInvalidFlag.CreateFmt('GetX87ExceptionFlags: Invalid X87 exception (%d).',[Ord(i)]);
  end;
end;

//------------------------------------------------------------------------------

{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}{$ENDIF}
Function SetX87ExceptionFlags(NewValue: TX87Exceptions): TX87Exceptions;
{$IFDEF PurePascal}

  procedure SetBit(BitMask: UInt16; NewState: Boolean);
  begin
    If NewState then
      Pas_X87SW := Pas_X87SW or BitMask
    else
      Pas_X87SW := Pas_X87SW and not BitMask;
  end;

begin
Result := GetX87ExceptionFlags;
SetBit(X87SW_EX_InvalidOP,excInvalidOp in NewValue);
SetBit(X87SW_EX_Denormal,excDenormal in NewValue);
SetBit(X87SW_EX_DivByZero,excDivByZero in NewValue);
SetBit(X87SW_EX_Overflow,excOverflow in NewValue);
SetBit(X87SW_EX_Underflow,excUnderflow in NewValue);
SetBit(X87SW_EX_Precision,excPrecision in NewValue);
end;
{$ELSE}
begin
Result := GetX87ExceptionFlags;
end;
{$ENDIF}
{$IFNDEF PurePascal}{$IFDEF FPCDWM}{$POP}{$ENDIF}{$ENDIF}

//------------------------------------------------------------------------------

procedure ClearX87Exceptions;
{$IFNDEF PurePascal}
asm
    FCLEX
end;
{$ELSE}
begin
Pas_X87SW := 0;
SWInit := True;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure RaiseX87Exceptions(var StatusWord: UInt16; Mask: Boolean = True; ConrolWord: UInt16 = 0);
begin
If ((StatusWord and X87SW_EX_InvalidOP) <> 0) and (not Mask or ((ConrolWord and X87CW_EMASK_InvalidOP) = 0)) then
  begin
    StatusWord := StatusWord and not X87SW_EX_InvalidOP;
    If (StatusWord and X87SW_StackFault) <> 0 then
      begin
        If (StatusWord and X87SW_ConditionCode_C1) <> 0 then
          raise EF80UStackOverflow.CreateDefMsgNoClear
        else
          raise EF80UStackUnderflow.CreateDefMsgNoClear;
      end
    else raise EF80UInvalidOp.CreateDefMsgNoClear;
  end;
If ((StatusWord and X87SW_EX_Denormal) <> 0) and (not Mask or ((ConrolWord and X87CW_EMASK_Denormal) = 0)) then
  begin
    StatusWord := StatusWord and not X87SW_EX_Denormal;
    raise EF80UDenormal.CreateDefMsgNoClear;
  end;
If ((StatusWord and X87SW_EX_DivByZero) <> 0) and (not Mask or ((ConrolWord and X87CW_EMASK_DivByZero) = 0)) then
  begin
    StatusWord := StatusWord and not X87SW_EX_DivByZero;
    raise EF80UDivByZero.CreateDefMsgNoClear;
  end;
If ((StatusWord and X87SW_EX_Overflow) <> 0) and (not Mask or ((ConrolWord and X87CW_EMASK_Overflow) = 0)) then
  begin
    StatusWord := StatusWord and not X87SW_EX_Overflow;
    raise EF80UOverflow.CreateDefMsgNoClear;
  end;
If ((StatusWord and X87SW_EX_Underflow) <> 0) and (not Mask or ((ConrolWord and X87CW_EMASK_Overflow) = 0)) then
  begin
    StatusWord := StatusWord and not X87SW_EX_Underflow;
    raise EF80UUnderflow.CreateDefMsgNoClear;
  end;
If ((StatusWord and X87SW_EX_Precision) <> 0) and (not Mask or ((ConrolWord and X87CW_EMASK_Precision) = 0)) then
  begin
    StatusWord := StatusWord and not X87SW_EX_Precision;
    raise EF80UPrecision.CreateDefMsgNoClear;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure RaiseX87Exceptions(Mask: Boolean = True);
var
  TempStatusWord: UInt16;
begin
TempStatusWord := GetX87StatusWord;
RaiseX87Exceptions(TempStatusWord,Mask,GetX87ControlWord);
end;


{===============================================================================
--------------------------------------------------------------------------------
                         Float80 <-> Float64 conversions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Conversion routines - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Conversion routines - axiliary routines
-------------------------------------------------------------------------------}
{
  There is a need for calculation with higher width than 64bits in conversion
  from Float80 to Float64 (in mantissa denormalization).
  Following routines and types implement bare minimum required for calculations
  on 65bit wide integer.
}
type
  UInt65 = record
    Low64:  UInt64;
    Bit65:  Byte;
  end;

const
  UInt65_ZERO: UInt65 = (Low64: 0; Bit65: 0);

  UI65_CMP_SMALLER = -1;
  UI65_CMP_LARGER  = +1;

//------------------------------------------------------------------------------

Function UInt65Get(Low64: UInt64; Bit65: Byte): UInt65;
begin
Result.Low64 := Low64;
Result.Bit65 := Bit65 and 1;
end;

//------------------------------------------------------------------------------

Function UInt65Not(Value: UInt65): UInt65;
begin
Result.Low64 := not Value.Low64;
Result.Bit65 := (not Value.Bit65) and 1;
end;

//------------------------------------------------------------------------------

Function UInt65And(A,B: UInt65): UInt65;
begin
Result.Low64 := A.Low64 and B.Low64;
Result.Bit65 := (A.Bit65 and B.Bit65) and 1;
end;

//------------------------------------------------------------------------------

Function UInt65Add(Value: UInt65; Add: UInt65): UInt65;
var
  Temp:   Int64;
  Carry:  Boolean;
begin
Temp := Int64(Int64Rec(Value.Low64).Lo) + Int64(Int64Rec(Add.Low64).Lo);
Carry := Temp > $FFFFFFFF;
Int64Rec(Result.Low64).Lo := Temp and $FFFFFFFF;  
If Carry then
  Temp := Int64(Int64Rec(Value.Low64).Hi) + Int64(Int64Rec(Add.Low64).Hi) + 1
else
  Temp := Int64(Int64Rec(Value.Low64).Hi) + Int64(Int64Rec(Add.Low64).Hi);
Carry := Temp > $FFFFFFFF;
Int64Rec(Result.Low64).Hi := Temp and $FFFFFFFF;
If Carry then
  Temp := Int64(Value.Bit65 and 1) + Int64(Add.Bit65 and 1) + 1
else
  Temp := Int64(Value.Bit65 and 1) + Int64(Add.Bit65 and 1);
Result.Bit65 := Byte(Temp and 1);
end;

//------------------------------------------------------------------------------

Function UInt65Subtract(Value: UInt65; Sub: UInt65): UInt65;
var
  Temp:   Int64;
  Borrow: Boolean;
begin
Temp := Int64(Int64Rec(Value.Low64).Lo) - Int64(Int64Rec(Sub.Low64).Lo);
Borrow := Temp < 0;
Int64Rec(Result.Low64).Lo := Temp and $FFFFFFFF;  
If Borrow then
  Temp := Int64(Int64Rec(Value.Low64).Hi) - Int64(Int64Rec(Sub.Low64).Hi) - 1
else
  Temp := Int64(Int64Rec(Value.Low64).Hi) - Int64(Int64Rec(Sub.Low64).Hi);
Borrow := Temp < 0;
Int64Rec(Result.Low64).Hi := Temp and $FFFFFFFF;
If Borrow then
  Temp := Int64(Value.Bit65 and 1) - Int64(Sub.Bit65 and 1) - 1
else
  Temp := Int64(Value.Bit65 and 1) - Int64(Sub.Bit65 and 1);
Result.Bit65 := Byte(Temp and 1);
end;

//------------------------------------------------------------------------------

Function UInt65RShift(Value: UInt65; Shift: Byte): UInt65;
begin
If Shift <= 0 then
  Result := Value
else If (Shift > 0) and (Shift < 64) then
  begin
    Result.Low64 := Value.Low64 shr Shift or (UInt64(Value.Bit65 and 1) shl (64 - Shift));
    Result.Bit65 := 0;
  end
else If Shift = 64 then
  Result := UInt65Get(Value.Bit65 and 1,0)
else
  Result := UInt65_ZERO
end;

//------------------------------------------------------------------------------

Function UInt65Compare(A,B: UInt65): Integer;
begin
If (A.Bit65 and 1) > (B.Bit65 and 1) then
  Result := UI65_CMP_LARGER
else If (A.Bit65 and 1) < (B.Bit65 and 1) then
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

Function UInt65IsZero(Value: UInt65): Boolean;
begin
Result := (Value.Low64 = 0) and ((Value.Bit65 and 1) = 0);
end;

//==============================================================================

procedure SignalX87Exceptions(Exceptions: TX87Exceptions); overload;
var
  i:      TX87Exception;
  Masks:  TX87Exceptions;
begin
SetX87ExceptionFlags(GetX87ExceptionFlags + Exceptions);
Masks := GetX87ExceptionMasks;
// note that stack exceptions are not observed
For i := Low(TX87Exception) to High(TX87Exception) do
  If (i in Exceptions) and not(i in Masks) then
    case i of
      excDenormal:  raise EF80UDenormal.CreateDefMsg;
      excDivByZero: raise EF80UDivByZero.CreateDefMsg;
      excOverflow:  raise EF80UOverflow.CreateDefMsg;
      excUnderflow: raise EF80UUnderflow.CreateDefMsg;
      excPrecision: raise EF80UPrecision.CreateDefMsg;
    else
     {excInvalidOp}
      raise EF80UInvalidOP.CreateDefMsg;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SignalX87Exceptions(Exceptions: UInt32); overload;
var
  TempCW: UInt16;
begin
// faster implementation
TempCW := GetX87ControlWord;
{$IFDEF PurePascal}
Pas_X87SW := GetX87StatusWord or Exceptions;
{$ENDIF}
If (Exceptions and X87SW_EX_InvalidOP <> 0) and (TempCW and X87CW_EMASK_InvalidOP = 0) then
  raise EF80UInvalidOP.CreateDefMsg;
If (Exceptions and X87SW_EX_Denormal <> 0) and (TempCW and X87CW_EMASK_Denormal = 0) then
  raise EF80UDenormal.CreateDefMsg;
If (Exceptions and X87SW_EX_DivByZero <> 0) and (TempCW and X87CW_EMASK_DivByZero = 0) then
  raise EF80UDivByZero.CreateDefMsg;
If (Exceptions and X87SW_EX_Overflow <> 0) and (TempCW and X87CW_EMASK_Overflow = 0) then
  raise EF80UOverflow.CreateDefMsg;
If (Exceptions and X87SW_EX_Underflow <> 0) and (TempCW and X87CW_EMASK_Underflow = 0) then
  raise EF80UUnderflow.CreateDefMsg;
If (Exceptions and X87SW_EX_Precision <> 0) and (TempCW and X87CW_EMASK_Precision = 0) then
  raise EF80UPrecision.CreateDefMsg;
end;

{-------------------------------------------------------------------------------
    Conversion routines - Float64 -> Float80 conversion
-------------------------------------------------------------------------------}

procedure Float64ToFloat80(Float64Ptr,Float80Ptr: Pointer);{$IFNDEF PurePascal} register; assembler;
asm
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
{$ELSE}
var
  Sign:           UInt64;
  Exponent:       Int32;  // biased exponent (bias 1023)
  Mantissa:       UInt64;
  MantissaShift:  Integer;

  procedure BuildExtendedResult(SigExp: UInt16; Man: UInt64);
  begin
    PFloat80Overlay(Float80Ptr)^.Mantissa := Man;
    PFloat80Overlay(Float80Ptr)^.SignExponent := SigExp;
  end;

  Function HighZeroCount(Value: UInt64): Integer;
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

begin
Sign := UInt64(Float64Ptr^) and F64_MASK_SIGN;
Exponent := Int32((UInt64(Float64Ptr^) and F64_MASK_EXP) shr 52);
Mantissa := UInt64(Float64Ptr^) and F64_MASK_FRAC;
case Exponent of

        // zero exponent - zero or denormal
  0:    If Mantissa <> 0 then
          begin
            // denormal
            SignalX87Exceptions(X87SW_EX_Denormal);
          {
            normalize...

            ...shift mantissa left so that its highest set bit will be shifted
            to integer bit (bit 63), also correct exponent to reflect this
            change
          }
            MantissaShift := HighZeroCount(Mantissa);
            BuildExtendedResult(UInt16(Sign shr 48) or UInt16(15372 - MantissaShift),
                                UInt64(Mantissa shl MantissaShift));
          end
        // return signed zero
        else BuildExtendedResult(UInt16(Sign shr 48),0);

        // max exponent - infinity or NaN
  $7FF: If Mantissa <> 0 then
          begin
            // not a number
            If (Mantissa and F64_MASK_FHB) = 0 then
              begin
                // signaled NaN
                SignalX87Exceptions(X87SW_EX_InvalidOp);
                // if no exception was raised, return quiet signed NaN with mantissa
                BuildExtendedResult(UInt16(Sign shr 48) or F80_MASK16_EXP,
                                    UInt64(Mantissa shl 11) or F80_MASK64_FHB or F80_MASK64_INTB)
              end
            // quiet signed NaN with mantissa
            else BuildExtendedResult(UInt16(Sign shr 48) or F80_MASK16_EXP,
                                     UInt64(Mantissa shl 11) or F80_MASK64_INTB);
          end  
        // signed infinity
        else BuildExtendedResult(UInt16(Sign shr 48) or F80_MASK16_EXP,F80_MASK64_INTB);

else
  // normal number
  BuildExtendedResult(UInt16(Sign shr 48) or UInt16(Exponent + 15360{16383 - 1023}),
                      UInt64(Mantissa shl 11) or F80_MASK64_INTB);
end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float64ToFloat80(Value: Float64): Float80;
begin
Float64ToFloat80(@Value,@Result);
end;

//------------------------------------------------------------------------------

procedure DoubleToExtended(DoublePtr,ExtendedPtr: Pointer);
begin
Float64ToFloat80(DoublePtr,ExtendedPtr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DoubleToExtended(Value: Float64): Float80;
begin
Result := Float64ToFloat80(Value);
end;

{-------------------------------------------------------------------------------
    Conversion routines - Float80 -> Float64 conversion
-------------------------------------------------------------------------------}

procedure Float80ToFloat64(Float80Ptr,Float64Ptr: Pointer); register;{$IFNDEF PurePascal} assembler;
asm
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
{$ELSE}
var
  Sign:         UInt64;
  Exponent:     Int32;    // biased exponent (bias 16383)
  Mantissa:     UInt64;   // including integer bit
  RoundMode:    TX87RoundingMode;
  ResultTemp:   UInt64;
  BitsLost:     Boolean;
  ManOverflow:  Boolean;

  Function ShiftMantissaDenorm(Value: UInt64; Shift: Byte; out DataLoss: Boolean): UInt64;
  var
    Value65:  UInt65;
    Mask:     UInt65;
    Low:      UInt65;
    High:     UInt65;
    DiffLow:  UInt65;
    DiffHigh: UInt65;
  begin
    // min possible shift 12, max possible shift 64
    // everything must be calculated in at least 65bits
    DataLoss := False;
    If (Shift > 0) and (Shift <= 64) then
      begin
        Value65 := UInt65Get(Value,0);
        Mask := UInt65RShift(UInt65Get(UInt64($FFFFFFFFFFFFFFFF),0),64 - Shift);
        If not UInt65IsZero(UInt65And(Value65,Mask)) then
          begin
            DataLoss := True;
            Low := UInt65And(Value65,UInt65Not(Mask));
            High := UInt65Add(Low,UInt65Add(Mask,UInt65Get(1,0))); 
            case RoundMode of
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
              DiffLow := UInt65Subtract(Value65,Low);
              DiffHigh := UInt65Subtract(High,Value65);
              case UInt65Compare(DiffLow,DiffHigh) of
                UI65_CMP_SMALLER: Result := UInt65RShift(Low,Shift).Low64;
                UI65_CMP_LARGER:  Result := UInt65RShift(High,Shift).Low64;
              else
                // select the one with clear lowest bit
                If UInt65IsZero(UInt65And(Low,UInt65Add(Mask,UInt65Get(1,0)))) then
                  Result := UInt65RShift(Low,Shift).Low64
                else
                  Result := UInt65RShift(High,Shift).Low64;
              end;
            end;

          end
        else Result := Value shr Shift;
      end
    // following cases should never happen, but...
    else If Shift > 64 then
      Result := 0
    else
      Result := Value;
  end;

  Function ShiftMantissa(Value: UInt64; out DataLoss: Boolean): UInt64;
  const
    SM_MASK = UInt64($7FF);
  var
    Low:      UInt64;
    High:     UInt64;
    DiffLow:  UInt64;
    DiffHigh: UInt64;
  begin
    // implicit shift 11, static mask $7FF
    DataLoss := False;
    If (Value and SM_MASK) <> 0 then
      begin
        DataLoss := True;
        Low := Value and not SM_MASK;
        High := Low + (SM_MASK + 1);
        case RoundMode of
          rmDown:     If Sign <> 0 then
                        Result := High shr 11
                      else
                        Result := Low shr 11;
          rmUp:       If Sign <> 0 then
                        Result := Low shr 11
                      else
                        Result := High shr 11;
          rmTruncate: Result := Low shr 11;
        else
         {rmNearest}
          DiffLow := Value - Low;
          DiffHigh := High - Value;
          If DiffLow < DiffHigh then
            Result := Low shr 11
          else If DiffLow > DiffHigh then
            Result := High shr 11
          else
            begin
              // select the one with clear lowest bit
              If (Low and (SM_MASK + 1)) = 0 then
                Result := Low shr 11
              else
                Result := High shr 11;
            end;
        end;
      end
    else Result := Value shr 11;
  end;

begin
RoundMode := GetX87RoundingMode;
Sign := UInt64(PFloat80Overlay(Float80Ptr)^.SignExponent and F80_MASK16_SIGN) shl 48;
Exponent := Int32(PFloat80Overlay(Float80Ptr)^.SignExponent and F80_MASK16_EXP);
Mantissa := PFloat80Overlay(Float80Ptr)^.Mantissa;
// check unsupported encodings...
If ((Exponent > 0) and (Exponent <= F80_MASK16_EXP)) and ((Mantissa and F80_MASK64_INTB) = 0) then
  begin
  {
    unnormals (seemingly normalized numbers, but with integer bit of 0),
    pseudo-infinities and pseudo-NaNs (both with integer bit 0)
  }
    SignalX87Exceptions(X87SW_EX_InvalidOp);
  {
    return negative QNaN (QNaN floating point indefinite)

    the constants are casted to UInt64 because older FPC is throwing
    nonsensical error without it
  }
    PUInt64(Float64Ptr)^ := UInt64(F64_MASK_SIGN or F64_MASK_EXP or F64_MASK_FHB);
  end
else
  case Exponent of
            // exponent of zero - zero or denormal
    0:      If Mantissa <> 0 then
              begin
              {
                non-zero mantissa - denormals

                Note that psedo-denormals (denormals with integer bit 1) are
                treated as usual denormals (with integer bit 0).

                Also note that X87 is not signaling denormal exception when
                converting from extended precision numbers.
              }
                // post-computation exceptions
                SignalX87Exceptions(X87SW_EX_Underflow);
                If ((RoundMode = rmUp) and (Sign = 0)) or
                   ((RoundMode = rmDown) and (Sign <> 0)) then
                  // return signed smallest representable number
                  PUInt64(Float64Ptr)^ := Sign or UInt64(1)
                else
                  // convert to signed zero
                  PUInt64(Float64Ptr)^ := Sign;
                SignalX87Exceptions(X87SW_EX_Precision);
              end
            // mantissa of 0 - return signed zero
            else PUInt64(Float64Ptr)^ := Sign;

          {
            exponent 1..15307 (-16382..-1076 unbiased) - exponent too small
            to be represented in double even as denormal
          }
    $1..
    $3BCB:  begin
              // post-computation exceptions
              SignalX87Exceptions(X87SW_EX_Underflow);
              If ((RoundMode = rmUp) and (Sign = 0)) or
                 ((RoundMode = rmDown) and (Sign <> 0)) then
                // return signed smallest representable number
                PUInt64(Float64Ptr)^ := Sign or UInt64(1)
              else
                // convert to signed zero
                PUInt64(Float64Ptr)^ := Sign;
              SignalX87Exceptions(X87SW_EX_Precision);
            end;

          {
            exponent 15308..15359 (-1075..-1024 unbiased) - exponent still too
            small to be represented in double, but can be denormalized to fit
            (result will have implicit exponent of -1022, explicit 0)
          }
    $3BCC..
    $3BFF:  begin
            {
              denormalize

              Conversion followed by a gradual underflow should take place here
              - but it is replaced by a one-time shift, which provides the same
              results and exceptions for given exponent range and is faster.
            }
              ResultTemp := Sign or ShiftMantissaDenorm(Mantissa,$3C0C - Exponent,BitsLost);
            {
              post-computation exceptions

              Note that, when underflow is masked, the underflow exception can
              be signalled only when the result is also inexact.
            }
              If GetX87ExceptionMask(excUnderflow) then
                begin
                  // underflow exception masked
                  If BitsLost then
                    begin
                      // inexact result
                      SignalX87Exceptions(X87SW_EX_Underflow);
                      PUInt64(Float64Ptr)^ := ResultTemp;
                      SignalX87Exceptions(X87SW_EX_Precision);
                    end
                  else PUInt64(Float64Ptr)^ := ResultTemp;
                end
              else SignalX87Exceptions(X87SW_EX_Underflow);
            end;

          {
            exponent 15360 (-1023 unbiased) - similar to previous case, but
            with more complexities because it can yield a normalized number
            as a result of denormalization.
          }
    $3C00:  begin
              ResultTemp := ShiftMantissaDenorm(Mantissa,11,BitsLost);
              If ResultTemp <> (F64_MASK_INTB shl 1) then
                begin
                  // result still a denormal
                  If GetX87ExceptionMask(excUnderflow) then
                    begin
                      PUInt64(Float64Ptr)^ := Sign or ShiftMantissaDenorm(Mantissa,12,BitsLost);
                      If BitsLost or ((ResultTemp and 1) <> 0) then
                        SignalX87Exceptions(X87SW_EX_Underflow or X87SW_EX_Precision);
                    end
                  else SignalX87Exceptions(X87SW_EX_Underflow);
                end
              else
                begin
                  // result promoted to a normalized number
                  PUInt64(Float64Ptr)^ := Sign or F64_MASK_INTB;
                  SignalX87Exceptions(X87SW_EX_Precision);
                end;
             end;

          {
            exponent 17407..32766 (1024..16383 unbiased) - exponent too large
            to be represented in double (max. valid exponent 1023)
          }
    $43FF..
    $7FFE:  begin
              // post-computation exceptions
              SignalX87Exceptions(X87SW_EX_Overflow);
              If (RoundMode = rmTruncate) or
                 ((RoundMode = rmUp) and (Sign <> 0)) or
                 ((RoundMode = rmDown) and (Sign = 0)) then
                // return signed largest representable number
                PUInt64(Float64Ptr)^ := Sign or UInt64($7FEFFFFFFFFFFFFF)
              else
                // convert to signed infinity
                PUInt64(Float64Ptr)^ := Sign or F64_MASK_EXP;
              SignalX87Exceptions(X87SW_EX_Precision);
            end;

          {
            maximum exponent - NaN or infinity (note that pseudo-NaN and
            pseudo-infinities are managed separately along with unnormals)
          }
    $7FFF:  If (Mantissa and F80_MASK64_FRAC) <> 0 then
              begin
                // not a number (NaN)
                If (Mantissa and F80_MASK64_FHB) = 0 then
                  begin
                    // signaling NaN
                    SignalX87Exceptions(X87SW_EX_InvalidOP);
                    // no exception, return quiet signed NaN with truncated mantissa
                    PUInt64(Float64Ptr)^ := Sign or F64_MASK_EXP or F64_MASK_FHB or (Mantissa shr 11);
                  end
                // quiet signed NaN with truncated mantissa
                else PUInt64(Float64Ptr)^ := Sign or F64_MASK_EXP or F64_MASK_FHB or (Mantissa shr 11);
              end
            // renturn signed infinity
            else PUInt64(Float64Ptr)^ := Sign or F64_MASK_EXP;

  else
    // exponent 15361..17406 (-1022..1023 unbiased) - representable normalized value
    Mantissa := ShiftMantissa(Mantissa and not F80_MASK64_INTB,BitsLost);
    // check if mantisa overflowed - if so, increase exponent to compensate (mantissa will be zero)
    If Mantissa > F64_MASK_FRAC then
      begin
        Inc(Exponent);
        ManOverflow := True;
      end
    else ManOverflow := False;
    // post-computation exceptions
    If ManOverflow and (Exponent > 17406) then
      // number was converted to infinity
      SignalX87Exceptions(X87SW_EX_Overflow);
    PUInt64(Float64Ptr)^ := Sign or ((UInt64(Exponent - 15360) shl 52) and F64_MASK_EXP) or (Mantissa and F64_MASK_FRAC);
    If BitsLost then
      // inexact result
      SignalX87Exceptions(X87SW_EX_Precision);
  end;
end;
{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Float80ToFloat64(Value: Float80): Float64;
begin
Float80ToFloat64(@Value,@Result);
end;

//------------------------------------------------------------------------------

procedure ExtendedToDouble(ExtendedPtr,DoublePtr: Pointer);
begin
Float80ToFloat64(ExtendedPtr,DoublePtr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ExtendedToDouble(Value: Float80): Float64;
begin
Result := Float80ToFloat64(Value);
end;

{===============================================================================
--------------------------------------------------------------------------------
                               Float80 utilities                                                                                           
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Utility routines - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Utility routines - number information functions
-------------------------------------------------------------------------------}

Function IsValid(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
{
  Non-zero exponent with zero integer bit, or zero exponent with non-zero
  integer bit are both unsuported encodings.

  Note that pseudo-denormals (zero exponent, non-zero integer bit) are also not
  valid, but can be processed by x87 without raising an InvalidOP exception
  (they are silently converted to usual denormals or zero).
}
Result := not(((_Value.SignExponent and F80_MASK16_EXP) = 0) xor ((_Value.Mantissa and F80_MASK64_INTB) = 0));
end;

//------------------------------------------------------------------------------

Function IsZero(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
// zero exponent and mantissa
Result := ((_Value.SignExponent and F80_MASK16_EXP) = 0) and (_Value.Mantissa = 0);
end;

//------------------------------------------------------------------------------

Function IsDenormal(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
// zero exponent, integer bit 0, non-zero fraction
Result := ((_Value.SignExponent and F80_MASK16_EXP) = 0) and
          ((_Value.Mantissa and F80_MASK64_INTB) = 0) and
          ((_Value.Mantissa and F80_MASK64_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsNaN(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
// max exponent, integer bit 1, non-zero fraction
Result := ((_Value.SignExponent and F80_MASK16_EXP) = F80_MASK16_EXP) and
          ((_Value.Mantissa and F80_MASK64_INTB) <> 0) and
          ((_Value.Mantissa and F80_MASK64_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsInfinite(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
// max exponent, integer bit 1, zero fraction
Result := ((_Value.SignExponent and F80_MASK16_EXP) = F80_MASK16_EXP) and
          ((_Value.Mantissa and F80_MASK64_INTB) <> 0) and
          ((_Value.Mantissa and F80_MASK64_FRAC) = 0);
end;

//------------------------------------------------------------------------------

Function IsNormal(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
// exponent >0 and <max, integer bit 1, any fraction
Result := (((_Value.SignExponent and F80_MASK16_EXP) > 0) and
           ((_Value.SignExponent and F80_MASK16_EXP) < F80_MASK16_EXP)) and
          ((_Value.Mantissa and F80_MASK64_INTB) <> 0);
end;

//------------------------------------------------------------------------------

Function IsPseudoValue(const Value: Float80): Boolean;
begin
Result := not IsValid(Value);
end;

//------------------------------------------------------------------------------

Function IsPseudoDenormal(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
// zero exponent, non-zero mantissa with integer bit 1
Result := ((_Value.SignExponent and F80_MASK16_EXP) = 0) and
          ((_Value.Mantissa <> 0) and ((_Value.Mantissa and F80_MASK64_INTB) <> 0));
end;

//------------------------------------------------------------------------------

Function IsPseudoNaN(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
// max exponent, integer bit 0, non-zero fraction
Result := ((_Value.SignExponent and F80_MASK16_EXP) = F80_MASK16_EXP) and
          ((_Value.Mantissa and F80_MASK64_INTB) = 0) and
          ((_Value.Mantissa and F80_MASK64_FRAC) <> 0);
end;

//------------------------------------------------------------------------------

Function IsPseudoInfinity(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
// max exponent, integer bit 0, zero fraction
Result := ((_Value.SignExponent and F80_MASK16_EXP) = F80_MASK16_EXP) and
          ((_Value.Mantissa and F80_MASK64_INTB) = 0) and
          ((_Value.Mantissa and F80_MASK64_FRAC) = 0);
end;

//------------------------------------------------------------------------------

Function IsUnnormal(const Value: Float80): Boolean;
var
  _Value: TFloat80Overlay absolute Value;
begin
// exponent >0 and <max, integer bit 0, any fraction
Result := (((_Value.SignExponent and F80_MASK16_EXP) > 0) and
           ((_Value.SignExponent and F80_MASK16_EXP) < F80_MASK16_EXP)) and
          ((_Value.Mantissa and F80_MASK64_INTB) = 0);
end;

{-------------------------------------------------------------------------------
    Utility routines - sign-related functions
-------------------------------------------------------------------------------}

Function Sign(const Value: Float80): TFloat80ValueSign;
var
  _Value: TFloat80Overlay absolute Value;
begin
If IsValid(Value) then
  begin
    If ((_Value.SignExponent and F80_MASK16_NSGN) <> 0) or (_Value.Mantissa <> 0) then
      begin
        If (_Value.SignExponent and F80_MASK16_SIGN) <> 0 then
          Result := -1
        else
          Result := 1;
      end
    else Result := 0;
  end
else raise EF80UInvalidOp.CreateDefMsgNoClear;
end;

//------------------------------------------------------------------------------

Function Abs(const Value: Float80): Float80;
var
  _Value:   TFloat80Overlay absolute Value;
  _Result:  TFloat80Overlay absolute Result;
begin
If IsValid(Value) then
  begin
    _Result.Mantissa := _Value.Mantissa;
    _Result.SignExponent := _Value.SignExponent and not F80_MASK16_SIGN;
  end
else raise EF80UInvalidOp.CreateDefMsgNoClear;
end;

//------------------------------------------------------------------------------

Function Neg(const Value: Float80): Float80;
var
  _Value:   TFloat80Overlay absolute Value;
  _Result:  TFloat80Overlay absolute Result;
begin
If IsValid(Value) then
  begin
    _Result.Mantissa := _Value.Mantissa;
    _Result.SignExponent := _Value.SignExponent xor F80_MASK16_SIGN;
  end
else raise EF80UInvalidOp.CreateDefMsgNoClear;
end;

{-------------------------------------------------------------------------------
    Utility routines - value encoding and decoding
-------------------------------------------------------------------------------}

procedure MapToFloat80Buffer(out Buffer; High16: UInt16; Low64: UInt64);
var
  _Result:  TFloat80Overlay absolute Buffer;
begin
_Result.Part_16 := High16;
_Result.Part_64 := Low64;
end;

//------------------------------------------------------------------------------

Function MapToFloat80(High16: UInt16; Low64: UInt64): Float80;
begin
MapToFloat80Buffer(Result,High16,Low64);
end;

//------------------------------------------------------------------------------

Function MapToExtended(High16: UInt16; Low64: UInt64): Extended;
{$IFDEF Extended64}
// extended is 64bit
var
  F80Result:  Float80;
begin
MapToFloat80Buffer(F80Result,High16,Low64);
Float80ToFloat64(@F80Result,@Result);
{$ELSE}
// extended is 80bit
begin
MapToFloat80Buffer(Result,High16,Low64);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure MapFromFloat80Buffer(const Buffer; out High16: UInt16; out Low64: UInt64);
var
  _Value: TFloat80Overlay absolute Buffer;
begin
High16 := _Value.Part_16;
Low64 := _Value.Part_64;
end;

//------------------------------------------------------------------------------

procedure MapFromFloat80(const Value: Float80; out High16: UInt16; out Low64: UInt64);
begin
MapFromFloat80Buffer(Value,High16,Low64);
end;

//------------------------------------------------------------------------------

procedure MapFromExtended(const Value: Extended; out High16: UInt16; out Low64: UInt64);
{$IFDEF Extended64}
var
  F80Value: Float80;
begin
Float64ToFloat80(@Value,@F80Value);
MapFromFloat80Buffer(F80Value,High16,Low64);
{$ELSE}
begin
MapFromFloat80Buffer(Value,High16,Low64);
{$ENDIF}
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

procedure EncodeFloat80Buffer(out Buffer; Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
var
  _Result:  TFloat80Overlay absolute Buffer;
begin
If Sign then
  _Result.SignExponent := F80_MASK16_SIGN
else
  _Result.SignExponent := 0;
If BiasedExp then
  _Result.SignExponent := _Result.SignExponent or (UInt16(ClampExp(Exponent,0,32767)) and F80_MASK16_EXP)
else
  _Result.SignExponent := _Result.SignExponent or (UInt16(ClampExp(Exponent,-16383,16384) + FLOAT80_EXPONENTBIAS) and F80_MASK16_EXP);
If IntBit then
  _Result.Mantissa := Mantissa
else
  begin
    // imply integer bit from exponent...
    If (_Result.SignExponent and F80_MASK16_EXP) = 0 then
      // zero exponent (zero or denormal) - integer bit 0
      _Result.Mantissa := Mantissa and not F80_MASK64_INTB
    else
      // non-zero exponent - integer bit 1
      _Result.Mantissa := Mantissa or F80_MASK64_INTB;
  end;
end;

//------------------------------------------------------------------------------

Function EncodeFloat80(Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True): Float80;
begin
EncodeFloat80Buffer(Result,Mantissa,Exponent,Sign,BiasedExp,IntBit);
end;

//------------------------------------------------------------------------------

Function EncodeExtended(Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True): Extended;
{$IFDEF Extended64}
// extended is 64bit
var
  F80Result:  Float80;
begin
EncodeFloat80Buffer(F80Result,Mantissa,Exponent,Sign,BiasedExp,IntBit);
Float80ToFloat64(@F80Result,@Result);
{$ELSE}
begin
EncodeFloat80Buffer(Result,Mantissa,Exponent,Sign,BiasedExp,IntBit);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure DecodeFloat80Buffer(const Buffer; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
var
  _Value: TFloat80Overlay absolute Buffer;
begin
If IntBit then
  Mantissa := _Value.Mantissa
else
  Mantissa := _Value.Mantissa and not F80_MASK64_INTB;
If BiasedExp then
  Exponent := Int16(_Value.SignExponent and F80_MASK16_EXP)
else
  Exponent := Int16(_Value.SignExponent and F80_MASK16_EXP) - FLOAT80_EXPONENTBIAS;
Sign := (_Value.SignExponent and F80_MASK16_SIGN) <> 0;
end;

//------------------------------------------------------------------------------

procedure DecodeFloat80(const Value: Float80; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
begin
DecodeFloat80Buffer(Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
end;

//------------------------------------------------------------------------------

procedure DecodeExtended(const Value: Extended; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
{$IFDEF Extended64}
var
  F80Value: Float80;
begin
Float64ToFloat80(@Value,@F80Value);
DecodeFloat80Buffer(F80Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
{$ELSE}
begin
DecodeFloat80Buffer(Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
{$ENDIF}
end;

//==============================================================================

procedure MapToFloat64Buffer(out Buffer; Value: UInt64);
var
  _Result: Float64 absolute Buffer;
begin
_Result := Value;
end;

//------------------------------------------------------------------------------

Function MapToFloat64(Value: UInt64): Float64;
begin
MapToFloat64Buffer(Result,Value);
end;

//------------------------------------------------------------------------------

Function MapToDouble(Value: UInt64): Double;
begin
MapToFloat64Buffer(Result,Value);
end;

//------------------------------------------------------------------------------

Function MapFromFloat64Buffer(const Buffer): UInt64;
var
  _Value: UInt64 absolute Buffer;
begin
Result := _Value;
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

procedure EncodeFloat64Buffer(out Buffer; Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False);
var
  _Result:  UInt64 absolute Buffer;
begin
_Result := Mantissa and F64_MASK_FRAC;
If BiasedExp then
  _Result := _Result or ((UInt64(ClampExp(Exponent,0,2047)) shl 52) and F64_MASK_EXP)
else
  _Result := _Result or ((UInt64(ClampExp(Exponent,-1023,1024) + FLOAT64_EXPONENTBIAS) shl 52) and F64_MASK_EXP);
If Sign then
  _Result := _Result or F64_MASK_SIGN;
end;

//------------------------------------------------------------------------------

Function EncodeFloat64(Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False): Float64;
begin
EncodeFloat64Buffer(Result,Mantissa,Exponent,Sign,BiasedExp);
end;

//------------------------------------------------------------------------------

Function EncodeDouble(Mantissa: UInt64; Exponent: Int16; Sign: Boolean; BiasedExp: Boolean = False): Double;
begin
EncodeFloat64Buffer(Result,Mantissa,Exponent,Sign,BiasedExp);
end;

//------------------------------------------------------------------------------

procedure DecodeFloat64Buffer(const Buffer; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
var
  _Value: UInt64 absolute Buffer;
begin
If IntBit then
  begin
    If ((_Value and F64_MASK_EXP) = 0){zero or denormal} then
      Mantissa := _Value and F64_MASK_FRAC
    else
      Mantissa := (_Value and F64_MASK_FRAC) or F64_MASK_INTB;
  end
else Mantissa := _Value and F64_MASK_FRAC;
If BiasedExp then
  Exponent := Int16((_Value and F64_MASK_EXP) shr 52)
else
  Exponent := Int16((_Value and F64_MASK_EXP) shr 52) - FLOAT64_EXPONENTBIAS;
Sign := (_Value and F64_MASK_SIGN) <> 0;
end;

//------------------------------------------------------------------------------

procedure DecodeFloat64(const Value: Float64; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
begin
DecodeFloat64Buffer(Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
end;

//------------------------------------------------------------------------------

procedure DecodeDouble(const Value: Double; out Mantissa: UInt64; out Exponent: Int16; out Sign: Boolean; BiasedExp: Boolean = False; IntBit: Boolean = True);
begin
DecodeFloat64Buffer(Value,Mantissa,Exponent,Sign,BiasedExp,IntBit);
end;

end.
