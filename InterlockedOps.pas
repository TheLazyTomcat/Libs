{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  InterlockedOps

    This library provides a set of functions, each performing an atomic
    operation on a variable - that is, each function is guaranteed to complete
    its operation in a thread-safe manner.

    It has been created as a replacement and extension of WinAPI-provided
    interlocked functions, but can be used on any system.
    Note that some functions, although equally named, behaves differently than
    those from WinAPI - see description of each function for details.

      WARNING - almost entire library is implemented in assembly, and because
                of that, it can only be compiled for x86 (IA-32) and x86-64
                (AMD64) processors.

    Functions for 8bit, 16bit, 32bit and 64bit integer variables, both signed
    and unsigned, are implemented. There are also variants for pointers and
    some functions are implemented for boolean variables.

    64bit variants are available in 32bit environment only when symbol
    EnableVal64onSys32 is defined (see symbols define section for more
    information). In 64bit environment, they are always available.

    There are also some functions accepting 128bit variables, but these are
    only available in 64bit environment and only when symbol EnableVal128 is
    defined.

    Unless noted otherwise in function description, there are no requirements
    for memory alignment of any of the passed parameters (as-per Intel
    Developers Manual, which explicitly states "The integrity of the LOCK
    prefix is not affected by the alignment of the memory field. Memory locking
    is observed for arbitrarily misaligned fields.").

    Note that upon return of any of the provided function, the accessed variable
    can already have a different value than expected if it was accessed by other
    thread(s). Whatever the function returns is a state that was valid during
    the internal lock.

  Version 1.4.2 (2022-02-21)

  Last change 2023-12-26

  ©2021-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.InterlockedOps

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
  * SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID

  SimpleCPUID is required only when symbol AssertInstructions is defined.

===============================================================================}
unit InterlockedOps;
{
  InterlockedOps_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.

    NOTE - this unit cannot be compiled without asm, but there it is for the
           sake of completeness.
}
{$IFDEF InterlockedOps_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported CPU architecture.'}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$ASMMODE Intel}
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

//------------------------------------------------------------------------------
{
  EnableVal64onSys32

  When defined, 64bit variants of all functions (that is, variants working with
  64bit parameters) are provided in 32bit environment. When not defined, these
  variants are available only in 64bit environment.

  Since implementing proper locking of 64bit primitive in 32bit mode is
  depending on instructions CMPXCHG8B and CMOV, which are not present on very
  old processors, this symbol is here to disable such code if there is a need
  to run this library on legacy hardware.

  Note that whether the unit was compiled with support for 64bit variables
  can be discerned from public constant ILO_64BIT_VARS - when true, the 64bit
  variants are provided, otherwise they are not.

    WARNING - when binary compiled with both this symbol and symbol
              AssertInstructions defined is run on hardware that does not
              support needed instructions, then an exception of type
              EILOUnsupportedInstruction is raised during unit initialization.

  By default enabled.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol InterlockedOps_EnableVal64onSys32_Off.
}
{$DEFINE EnableVal64onSys32}
{$IFDEF InterlockedOps_EnableVal64onSys32_Off}
  {$UNDEF EnableVal64onSys32}
{$ENDIF}

{
  EnableVal128

  When enabled, 128bit variants of function InterlockedCompareExchange are
  provided, otherwise they are not.

  Whether the unit was compiled with support for 128bit variables can be
  discerned from public constant ILO_128BIT_VARS - when true, the 128bit
  variants are provided, otherwise they are not.

    WARNING - these functions are available only in 64bit mode, never in 32bit
              mode. Therefore, this symbol has no effect in 32bit mode.

    WARNING - when binary compiled with both this symbol and symbol
              AssertInstructions defined is run on hardware that does not
              support needed instructions, then an exception of type
              EILOUnsupportedInstruction is raised during unit initialization.

  By default enabled.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol InterlockedOps_EnableVal128_Off.
}
{$DEFINE EnableVal128}
{$IFDEF InterlockedOps_EnableVal128_Off}
  {$UNDEF EnableVal128}
{$ENDIF}

{
  AssertInstructions

  When defined, a check whether the current CPU supports needed instructions
  is performed during unit initialization. If they are not supported, then an
  EILOUnsupportedInstruction exception is raised.

  When not defined, no check is made - but be warned that this may lead to
  errors when calling functions that are using those instructions.

  This symbol is here for situations where CPU can run the instruction but
  CPUID for some reason reports it as unsupported (behavior observed on
  virtualized guest systems).

  By default enabled.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol InterlockedOps_AssertInstructions_Off.
}
{$DEFINE AssertInstructions}
{$IFDEF InterlockedOps_AssertInstructions_Off}
  {$UNDEF AssertInstructions}
{$ENDIF}

//------------------------------------------------------------------------------
// do not touch following define checks

{$IF Defined(EnableVal64onSys32) or Defined(x64)}
  {$DEFINE IncludeVal64}
{$IFEND}

{$IF Defined(EnableVal128) and Defined(x64)}
  {$DEFINE IncludeVal128}
{$IFEND}

{$IF Defined(PurePascal) and not Defined(CompTest)}
  {$MESSAGE WARN 'This unit cannot be compiled in PurePascal mode.'}
{$IFEND}

interface

uses
  SysUtils,
  AuxTypes;

{===============================================================================
    Informative public constants
===============================================================================}
const
  ILO_64BIT_VARS  = {$IFDEF IncludeVal64}True{$ELSE}False{$ENDIF};
  ILO_128BIT_VARS = {$IFDEF IncludeVal128}True{$ELSE}False{$ENDIF};

{===============================================================================
    Some helper types
===============================================================================}
{
  Type UInt128 is here only to provide 128 bits long type that could be used in
  calls to 128bit variants of provided functions.
}
type
  UInt128 = packed record
    case Integer of
      0:(Low:     UInt64;
         High:    UInt64);
      1:(QWords:  array[0..1] of UInt64);
      2:(DWords:  array[0..3] of UInt32);
      3:(Words:   array[0..7] of UInt16);
      4:(Bytes:   array[0..15] of UInt8);
  end;
   PUInt128 =  ^UInt128;
  PPUInt128 = ^PUInt128;

{===============================================================================
    Library-specific exception classes
===============================================================================}
type
  EILOException = class(Exception);

  EILOUnsupportedInstruction   = class(EILOException);
  EILOUnsupportedImplementation = class(EILOException);

{===============================================================================
--------------------------------------------------------------------------------
                             Interlocked increment
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedIncrement

    Increments variable (pointed to by) I by one and returns the resulting
    (incremented) value.

-------------------------------------------------------------------------------}

Function InterlockedIncrement8(I: Pointer): UInt8; register; assembler;
Function InterlockedIncrement16(I: Pointer): UInt16; register; assembler;
Function InterlockedIncrement32(I: Pointer): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedIncrement64(I: Pointer): UInt64; register; assembler;
{$ENDIF}
Function InterlockedIncrementPtr(I: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedIncrement(var I: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedIncrement(var I: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedIncrement(var I: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedIncrement(var I: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}  

Function InterlockedIncrement(var I: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedIncrement(var I: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedIncrement(var I: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedIncrement(var I: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedIncrement(var I: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                             Interlocked decrement
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedDecrement

    Decrements variable (pointed to by) I by one and returns the resulting
    (decremented) value.

-------------------------------------------------------------------------------}

Function InterlockedDecrement8(I: Pointer): UInt8; register; assembler;
Function InterlockedDecrement16(I: Pointer): UInt16; register; assembler;
Function InterlockedDecrement32(I: Pointer): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedDecrement64(I: Pointer): UInt64; register; assembler;
{$ENDIF}
Function InterlockedDecrementPtr(I: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedDecrement(var I: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedDecrement(var I: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedDecrement(var I: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedDecrement(var I: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedDecrement(var I: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedDecrement(var I: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedDecrement(var I: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedDecrement(var I: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedDecrement(var I: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                        Interlocked decrement if positive
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedDecrementIfPositive

    Decrements variable (pointed to by) I by one, but only when it is
    greater/above 0, and returns the original value - note that this differs
    from normal decrement, where the new value is returned. This is to allow
    checks whether the decrement actually took place or not.

    Function behaves differently for signed and unsigned integers. For example,
    signed value of -1 will NOT be decremented, whereas the same value as
    unsigned (255 for 8bit integer) will be decremented.

    Variants accepting only pointer are treating the pointed variable as
    unsigned integer, pointers are also treated as unsigned integers.

    WARNING - Unlike InterlockedDecrement, this function returns the original
              value of I, not the new value!

-------------------------------------------------------------------------------}

Function InterlockedDecrementIfPositive8(I: Pointer): UInt8;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedDecrementIfPositive16(I: Pointer): UInt16;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedDecrementIfPositive32(I: Pointer): UInt32;{$IFDEF CanInline} inline;{$ENDIF}
{$IFDEF IncludeVal64}
Function InterlockedDecrementIfPositive64(I: Pointer): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}
Function InterlockedDecrementIfPositivePtr(I: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedDecrementIfPositive(var I: UInt8): UInt8; overload; register; assembler;
Function InterlockedDecrementIfPositive(var I: Int8): Int8; overload; register; assembler;

Function InterlockedDecrementIfPositive(var I: UInt16): UInt16; overload; register; assembler;
Function InterlockedDecrementIfPositive(var I: Int16): Int16; overload; register; assembler;

Function InterlockedDecrementIfPositive(var I: UInt32): UInt32; overload; register; assembler;
Function InterlockedDecrementIfPositive(var I: Int32): Int32; overload; register; assembler;

{$IFDEF IncludeVal64}
Function InterlockedDecrementIfPositive(var I: UInt64): UInt64; overload; register; assembler;
Function InterlockedDecrementIfPositive(var I: Int64): Int64; overload; register; assembler;
{$ENDIF}

Function InterlockedDecrementIfPositive(var I: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              Interlocked addition
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedAdd

    Sets variable (pointed to by) A to a sum A and B and returns the resulting
    value.

-------------------------------------------------------------------------------}

Function InterlockedAdd8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedAdd16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedAdd32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedAdd64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedAddPtr(A: Pointer; B: PtrInt): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAddPtr(A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedAdd(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAdd(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedAdd(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAdd(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedAdd(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAdd(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedAdd(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAdd(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedAdd(var A: Pointer; B: PtrInt): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAdd(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                            Interlocked subtraction
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedSub

    Subtracts B from variable (pointed to by) A and returns the resulting
    value.

-------------------------------------------------------------------------------}

Function InterlockedSub8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedSub16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedSub32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedSub64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedSubPtr(A: Pointer; B: PtrInt): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedSubPtr(A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedSub(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedSub(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedSub(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedSub(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedSub(var A: UInt32; B: UInt32): UInt32; overload{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedSub(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedSub(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedSub(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedSub(var A: Pointer; B: PtrInt): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedSub(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              Interlocked negation
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedNeg

    Negates (changes sign) of variable (pointed to by) I and returns the
    resulting value.

    Note that unsigned integers are treated as signed integers with the same
    bit pattern. For example UInt8(255) is seen as Int8(-1), and its negative
    is therefore UInt8(1).

-------------------------------------------------------------------------------}

Function InterlockedNeg8(I: Pointer): UInt8; register; assembler;
Function InterlockedNeg16(I: Pointer): UInt16; register; assembler;
Function InterlockedNeg32(I: Pointer): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedNeg64(I: Pointer): UInt64; register; assembler;
{$ENDIF}
Function InterlockedNegPtr(I: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedNeg(var I: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedNeg(var I: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedNeg(var I: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedNeg(var I: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedNeg(var I: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedNeg(var I: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedNeg(var I: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedNeg(var I: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedNeg(var I: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                            Interlocked logical not
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedNot

    Performs logical NOT (flips all bits) of variable (pointed to by) I and
    returns the resulting value.

-------------------------------------------------------------------------------}

Function InterlockedNot8(I: Pointer): UInt8; register; assembler;
Function InterlockedNot16(I: Pointer): UInt16; register; assembler;
Function InterlockedNot32(I: Pointer): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedNot64(I: Pointer): UInt64; register; assembler;
{$ENDIF}
Function InterlockedNotPtr(I: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedNotBool(I: Pointer): Boolean; register; assembler;

//------------------------------------------------------------------------------

Function InterlockedNot(var I: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedNot(var I: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedNot(var I: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedNot(var I: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedNot(var I: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedNot(var I: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedNot(var I: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedNot(var I: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedNot(var I: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedNot(var I: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                            Interlocked logical and
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedAnd

    Performs logical AND of variable (pointed to by) A with B, stores the
    result back in A and returns it.

    WARNING - this function differs from InterlockedAnd provided by WinAPI.
              There, it returns original value of the variable, here it returns
              the resulting value.
              WinAPI behavior is provided by function InterlockedExchangeAnd.

-------------------------------------------------------------------------------}

Function InterlockedAnd8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedAnd16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedAnd32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedAnd64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedAndPtr(A: Pointer; B: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAndBool(A: Pointer; B: Boolean): Boolean; register; assembler;

//------------------------------------------------------------------------------

Function InterlockedAnd(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAnd(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedAnd(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAnd(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedAnd(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAnd(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedAnd(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedAnd(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedAnd(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedAnd(var A: Boolean; B: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                             Interlocked logical or
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedOr

    Performs logical OR of variable (pointed to by) A with B, stores the
    result back in A and returns it.

    WARNING - this function differs from InterlockedOr provided by WinAPI.
              There, it returns original value of the variable, here it returns
              the resulting value.
              WinAPI behavior is provided by function InterlockedExchangeOr.
              
-------------------------------------------------------------------------------}

Function InterlockedOr8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedOr16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedOr32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedOr64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedOrPtr(A: Pointer; B: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedOrBool(A: Pointer; B: Boolean): Boolean; register; assembler;

//------------------------------------------------------------------------------

Function InterlockedOr(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedOr(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedOr(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedOr(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedOr(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedOr(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedOr(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedOr(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedOr(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedOr(var A: Boolean; B: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                             Interlocked logical xor
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedXor

    Performs logical XOR of variable (pointed to by) A with B, stores the
    result back in A and returns it.

    WARNING - this function differs from InterlockedXor provided by WinAPI.
              There, it returns original value of the variable, here it returns
              the resulting value.
              WinAPI behavior is provided by function InterlockedExchangeXor.

-------------------------------------------------------------------------------}

Function InterlockedXor8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedXor16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedXor32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedXor64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedXorPtr(A: Pointer; B: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedXorBool(A: Pointer; B: Boolean): Boolean; register; assembler;

//------------------------------------------------------------------------------

Function InterlockedXor(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedXor(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedXor(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedXor(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedXor(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedXor(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedXor(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedXor(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedXor(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedXor(var A: Boolean; B: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              Interlocked exchange
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedExchange

    Sets variable (pointed to by) A to a value of B and returns original value
    of A.

-------------------------------------------------------------------------------}

Function InterlockedExchange8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedExchange16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedExchange32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedExchange64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedExchangePtr(A: Pointer; B: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeBool(A: Pointer; B: Boolean): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchange(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchange(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchange(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchange(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchange(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchange(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedExchange(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchange(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedExchange(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchange(var A: Boolean; B: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                          Interlocked exchange and add
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedExchangeAdd

    Sets variable (pointed to by) A to a sum of A and B, stores the result back
    in A and returns original value of A.

-------------------------------------------------------------------------------}

Function InterlockedExchangeAdd8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedExchangeAdd16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedExchangeAdd32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedExchangeAdd64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedExchangeAddPtr(A: Pointer; B: PtrInt): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAddPtr(A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeAdd(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAdd(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeAdd(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAdd(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeAdd(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAdd(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedExchangeAdd(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAdd(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedExchangeAdd(var A: Pointer; B: PtrInt): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAdd(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                       Interlocked exchange and subtract
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedExchangeSub

    Subtracts B from variable (pointed to by) A, stores the result back in A
    and returns original value of A.
    
-------------------------------------------------------------------------------}

Function InterlockedExchangeSub8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedExchangeSub16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedExchangeSub32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedExchangeSub64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedExchangeSubPtr(A: Pointer; B: PtrInt): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeSubPtr(A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeSub(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeSub(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeSub(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeSub(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeSub(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeSub(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedExchangeSub(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeSub(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedExchangeSub(var A: Pointer; B: PtrInt): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeSub(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                       Interlocked exchange and negation
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedExchangeNeg

    Negates (changes sign) of variable (pointed to by) I and returns its
    original value.

    Note that unsigned integers are treated as signed integers with the same
    bit pattern. For example UInt8(255) is seen as Int8(-1), and its negative
    is therefore UInt8(1).
    
-------------------------------------------------------------------------------}

Function InterlockedExchangeNeg8(I: Pointer): UInt8; register; assembler;
Function InterlockedExchangeNeg16(I: Pointer): UInt16; register; assembler;
Function InterlockedExchangeNeg32(I: Pointer): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedExchangeNeg64(I: Pointer): UInt64; register; assembler;
{$ENDIF}
Function InterlockedExchangeNegPtr(I: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeNeg(var I: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeNeg(var I: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeNeg(var I: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeNeg(var I: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeNeg(var I: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeNeg(var I: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedExchangeNeg(var I: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeNeg(var I: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedExchangeNeg(var I: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked exchange and logical not
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedExchangeNot

    Performs logical NOT (flips all bits) of variable (pointed to by) I and
    returns its original value.

-------------------------------------------------------------------------------}

Function InterlockedExchangeNot8(I: Pointer): UInt8; register; assembler;
Function InterlockedExchangeNot16(I: Pointer): UInt16; register; assembler;
Function InterlockedExchangeNot32(I: Pointer): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedExchangeNot64(I: Pointer): UInt64; register; assembler;
{$ENDIF}
Function InterlockedExchangeNotPtr(I: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeNotBool(I: Pointer): Boolean; register; assembler;

//------------------------------------------------------------------------------

Function InterlockedExchangeNot(var I: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeNot(var I: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeNot(var I: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeNot(var I: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeNot(var I: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeNot(var I: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedExchangeNot(var I: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeNot(var I: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedExchangeNot(var I: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeNot(var I: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked exchange and logical and
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedExchangeAnd

    Performs logical AND of variable (pointed to by) A with B, stores the
    result back in A and returns original value of A.

-------------------------------------------------------------------------------}

Function InterlockedExchangeAnd8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedExchangeAnd16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedExchangeAnd32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedExchangeAnd64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedExchangeAndPtr(A: Pointer; B: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAndBool(A: Pointer; B: Boolean): Boolean; register; assembler;

//------------------------------------------------------------------------------

Function InterlockedExchangeAnd(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAnd(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeAnd(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAnd(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeAnd(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAnd(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedExchangeAnd(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeAnd(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedExchangeAnd(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeAnd(var A: Boolean; B: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked exchange and logical or
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedExchangeOr

    Performs logical OR of variable (pointed to by) A with B, stores the
    result back in A and returns original value of A.

-------------------------------------------------------------------------------}

Function InterlockedExchangeOr8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedExchangeOr16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedExchangeOr32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedExchangeOr64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedExchangeOrPtr(A: Pointer; B: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeOrBool(A: Pointer; B: Boolean): Boolean; register; assembler;

//------------------------------------------------------------------------------

Function InterlockedExchangeOr(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeOr(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeOr(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeOr(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeOr(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeOr(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedExchangeOr(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeOr(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedExchangeOr(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeOr(var A: Boolean; B: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked exchange and logical xor
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedExchangeXor

    Performs logical XOR of variable (pointed to by) A with B, stores the
    result back in A and returns original value of A.

-------------------------------------------------------------------------------}

Function InterlockedExchangeXor8(A: Pointer; B: UInt8): UInt8; register; assembler;
Function InterlockedExchangeXor16(A: Pointer; B: UInt16): UInt16; register; assembler;
Function InterlockedExchangeXor32(A: Pointer; B: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedExchangeXor64(A: Pointer; B: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedExchangeXorPtr(A: Pointer; B: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeXorBool(A: Pointer; B: Boolean): Boolean; register; assembler;

//------------------------------------------------------------------------------

Function InterlockedExchangeXor(var A: UInt8; B: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeXor(var A: Int8; B: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeXor(var A: UInt16; B: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeXor(var A: Int16; B: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeXor(var A: UInt32; B: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeXor(var A: Int32; B: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedExchangeXor(var A: UInt64; B: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeXor(var A: Int64; B: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedExchangeXor(var A: Pointer; B: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedExchangeXor(var A: Boolean; B: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                       Interlocked exchange and operation
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedExchangeOp

    Performs operation implemented in function passed as Op parameter (normally
    calls this function) on value of variable (pointed to by) A with value given
    in parameter B, stores the result back in A and returns original value of A,
    everything in an atomic manner - that is, everything is completed
    thread-safe.

    WARNING - The Op function must be maximally simple, short, fast, thread
              safe and ideally reentrant and should also be self-contained
              (no call to any other function - be wary of implicit calls
              generated by the compiler, no use of global variables, ...)

    WARNING - Function Op must always be assigned (the assignement is, for the
              sake of performance, NOT checked).

    NOTE - Op can be called multiple-times within a single call to interlocked
           function and the arguments can differ in each invocation.

-------------------------------------------------------------------------------}

type
  TInterlockedOp8u = Function(A,B: UInt8): UInt8; register;
  TInterlockedOp8s = Function(A,B: Int8): Int8; register;

  TInterlockedOp16u = Function(A,B: UInt16): UInt16; register;
  TInterlockedOp16s = Function(A,B: Int16): Int16; register;

  TInterlockedOp32u = Function(A,B: UInt32): UInt32; register;
  TInterlockedOp32s = Function(A,B: Int32): Int32; register;

{$IFDEF IncludeVal64}
  TInterlockedOp64u = Function(A,B: UInt64): UInt64; register;
  TInterlockedOp64s = Function(A,B: Int64): Int64; register;
{$ENDIF}

  TInterlockedOpPtr = Function(A,B: Pointer): Pointer; register;

  TInterlockedOpBool = Function(A,B: Boolean): Boolean; register;

//------------------------------------------------------------------------------

Function InterlockedExchangeOp8(A: Pointer; B: UInt8; Op: TInterlockedOp8u): UInt8;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeOp16(A: Pointer; B: UInt16; Op: TInterlockedOp16u): UInt16;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeOp32(A: Pointer; B: UInt32; Op: TInterlockedOp32u): UInt32;{$IFDEF CanInline} inline;{$ENDIF}
{$IFDEF IncludeVal64}
Function InterlockedExchangeOp64(A: Pointer; B: UInt64; Op: TInterlockedOp64u): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}
Function InterlockedExchangeOpPtr(A: Pointer; B: Pointer; Op: TInterlockedOpPtr): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedExchangeOpBool(A: Pointer; B: Boolean; Op: TInterlockedOpBool): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeOp(var A: UInt8; B: UInt8; Op: TInterlockedOp8u): UInt8; overload;
Function InterlockedExchangeOp(var A: Int8; B: Int8; Op: TInterlockedOp8s): Int8; overload;

Function InterlockedExchangeOp(var A: UInt16; B: UInt16; Op: TInterlockedOp16u): UInt16; overload;
Function InterlockedExchangeOp(var A: Int16; B: Int16; Op: TInterlockedOp16s): Int16; overload;

Function InterlockedExchangeOp(var A: UInt32; B: UInt32; Op: TInterlockedOp32u): UInt32; overload;
Function InterlockedExchangeOp(var A: Int32; B: Int32; Op: TInterlockedOp32s): Int32; overload;

{$IFDEF IncludeVal64}
Function InterlockedExchangeOp(var A: UInt64; B: UInt64; Op: TInterlockedOp64u): UInt64; overload;
Function InterlockedExchangeOp(var A: Int64; B: Int64; Op: TInterlockedOp64s): Int64; overload;
{$ENDIF}

Function InterlockedExchangeOp(var A: Pointer; B: Pointer; Op: TInterlockedOpPtr): Pointer; overload;

Function InterlockedExchangeOp(var A: Boolean; B: Boolean; Op: TInterlockedOpBool): Boolean; overload;

{===============================================================================
--------------------------------------------------------------------------------
                        Interlocked compare and exchange                         
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedCompareExchange

    Compares value of variable (pointed to by) Destination with Comparand.
    When they are equal, the Destination is set to a value passed in Exchange.
    If they do not match, then nothing is done.
    Whether the exchange took place or not is indicated by value returned in
    Exchanged (True - exchange was performed, False - nothing was done).
    Returns original value of Destination.

    WARNING - 128bit variant requires the value Destination to be located in
              memory at 128bit-aligned address, otherwise it will fail with an
              exception.

-------------------------------------------------------------------------------}

Function InterlockedCompareExchange8(Destination: Pointer; Exchange,Comparand: UInt8; out Exchanged: Boolean): UInt8; overload; register; assembler;
Function InterlockedCompareExchange16(Destination: Pointer; Exchange,Comparand: UInt16; out Exchanged: Boolean): UInt16; overload; register; assembler;
Function InterlockedCompareExchange32(Destination: Pointer; Exchange,Comparand: UInt32; out Exchanged: Boolean): UInt32; overload; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedCompareExchange64(Destination: Pointer; Exchange,Comparand: UInt64; out Exchanged: Boolean): UInt64; overload; register; assembler;
{$ENDIF}
{$IFDEF IncludeVal128}
Function InterlockedCompareExchange128(Destination: Pointer; Exchange,Comparand: UInt128; out Exchanged: Boolean): UInt128; overload; register; assembler;
{$ENDIF}
Function InterlockedCompareExchangePtr(Destination: Pointer; Exchange,Comparand: Pointer; out Exchanged: Boolean): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchangeBool(Destination: Pointer; Exchange,Comparand: Boolean; out Exchanged: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedCompareExchange(var Destination: UInt8; Exchange,Comparand: UInt8; out Exchanged: Boolean): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchange(var Destination: Int8; Exchange,Comparand: Int8; out Exchanged: Boolean): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedCompareExchange(var Destination: UInt16; Exchange,Comparand: UInt16; out Exchanged: Boolean): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchange(var Destination: Int16; Exchange,Comparand: Int16; out Exchanged: Boolean): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedCompareExchange(var Destination: UInt32; Exchange,Comparand: UInt32; out Exchanged: Boolean): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchange(var Destination: Int32; Exchange,Comparand: Int32; out Exchanged: Boolean): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedCompareExchange(var Destination: UInt64; Exchange,Comparand: UInt64; out Exchanged: Boolean): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchange(var Destination: Int64; Exchange,Comparand: Int64; out Exchanged: Boolean): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

{$IFDEF IncludeVal128}
Function InterlockedCompareExchange(var Destination: UInt128; Exchange,Comparand: UInt128; out Exchanged: Boolean): UInt128; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedCompareExchange(var Destination: Pointer; Exchange,Comparand: Pointer; out Exchanged: Boolean): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedCompareExchange(var Destination: Boolean; Exchange,Comparand: Boolean; out Exchanged: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------

  InterlockedCompareExchange

    Compares value of variable (pointed to by) Destination with Comparand.
    When they are equal, the Destination is set to a value passed in Exchange.
    If they do not match, then nothing is done.
    Returns original value of Destination.

    WARNING - 128bit variant requires the value Destination to be located in
              memory at 128bit-aligned address, otherwise it will fail with an
              exception.

-------------------------------------------------------------------------------}

Function InterlockedCompareExchange8(Destination: Pointer; Exchange,Comparand: UInt8): UInt8; overload; register; assembler;
Function InterlockedCompareExchange16(Destination: Pointer; Exchange,Comparand: UInt16): UInt16; overload; register; assembler;
Function InterlockedCompareExchange32(Destination: Pointer; Exchange,Comparand: UInt32): UInt32; overload; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedCompareExchange64(Destination: Pointer; Exchange,Comparand: UInt64): UInt64; overload; register; assembler;
{$ENDIF}
{$IFDEF IncludeVal128}
Function InterlockedCompareExchange128(Destination: Pointer; Exchange,Comparand: UInt128): UInt128; overload; register; assembler;
{$ENDIF}
Function InterlockedCompareExchangePtr(Destination: Pointer; Exchange,Comparand: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchangeBool(Destination: Pointer; Exchange,Comparand: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedCompareExchange(var Destination: UInt8; Exchange,Comparand: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchange(var Destination: Int8; Exchange,Comparand: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedCompareExchange(var Destination: UInt16; Exchange,Comparand: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchange(var Destination: Int16; Exchange,Comparand: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedCompareExchange(var Destination: UInt32; Exchange,Comparand: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchange(var Destination: Int32; Exchange,Comparand: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedCompareExchange(var Destination: UInt64; Exchange,Comparand: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedCompareExchange(var Destination: Int64; Exchange,Comparand: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

{$IFDEF IncludeVal128}
Function InterlockedCompareExchange(var Destination: UInt128; Exchange,Comparand: UInt128): UInt128; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedCompareExchange(var Destination: Pointer; Exchange,Comparand: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedCompareExchange(var Destination: Boolean; Exchange,Comparand: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              Interlocked bit test
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedBitTest

    Returns value of bit (True = 1/set, False = 0/clear) selected by
    a parameter Bit in variable (pointed to by) I.

    Actual bit position in masked so only lowest 3, 4, 5 or 6 bits, depending
    on the width of the variable, of the passed Bit parameter are used,
    effectively taking modulo 8, 16, 32 or 64 of the value.

-------------------------------------------------------------------------------}

Function InterlockedBitTest8(I: Pointer; Bit: Integer): Boolean; register; assembler;
Function InterlockedBitTest16(I: Pointer; Bit: Integer): Boolean; register; assembler;
Function InterlockedBitTest32(I: Pointer; Bit: Integer): Boolean; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedBitTest64(I: Pointer; Bit: Integer): Boolean; register; assembler;
{$ENDIF}
Function InterlockedBitTestPtr(I: Pointer; Bit: Integer): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTest(var I: UInt8; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTest(var I: Int8; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTest(var I: UInt16; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTest(var I: Int16; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTest(var I: UInt32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTest(var I: Int32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedBitTest(var I: UInt64; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTest(var I: Int64; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedBitTest(var I: Pointer; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                         Bit string interlocked bit test                         
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedBitTestStr

    Returns value of bit selected by a parameter BitOffset in bit string
    pointed to by Str.
    Length of the bit string is not explicitly limited and BitOffset can have
    a negative value.

    This function always accesses only one byte of memory.

-------------------------------------------------------------------------------}

Function InterlockedBitTestStr(Str: Pointer; BitOffset: PtrInt): Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                          Interlocked bit test and set
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedBitTestAndSet

    Sets a bit (changes it to 1) selected by a parameter Bit in variable
    (pointed to by) I and returns original value of this bit.

    Actual bit position in masked so only lowest 3, 4, 5 or 6 bits, depending
    on the width of the variable, of the passed Bit parameter are used,
    effectively taking modulo 8, 16, 32 or 64 of the value.

-------------------------------------------------------------------------------}

Function InterlockedBitTestAndSet8(I: Pointer; Bit: Integer): Boolean; register; assembler;
Function InterlockedBitTestAndSet16(I: Pointer; Bit: Integer): Boolean; register; assembler;
Function InterlockedBitTestAndSet32(I: Pointer; Bit: Integer): Boolean; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedBitTestAndSet64(I: Pointer; Bit: Integer): Boolean; register; assembler;
{$ENDIF}
Function InterlockedBitTestAndSetPtr(I: Pointer; Bit: Integer): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestAndSet(var I: UInt8; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndSet(var I: Int8; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTestAndSet(var I: UInt16; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndSet(var I: Int16; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTestAndSet(var I: UInt32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndSet(var I: Int32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedBitTestAndSet(var I: UInt64; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndSet(var I: Int64; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedBitTestAndSet(var I: Pointer; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                     Bit string interlocked bit test and set
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedBitTestAndSetStr

    Sets a bit selected by paramter BitOffset in a bit string pointed to by Str
    and returns original value of this bit.
    Length of the bit string is not explicitly limited and BitOffset can have
    a negative value.

    For detailed information of how the memory is accessed, please refer to
    documentation of IA-32/AMD64 processors, namely instructions BT, BTC, BTR
    and BTS.    

    WARNING - the processor might access more than the one byte necessary to
              reach the requested bit. Depending on operand width, it can
              access 2, 4 or 8 bytes on address that can be calculated as:

                Str + (WB * (BitOffset DIV Wb))

                  WB - width of the operand in bytes (2, 4 or 8)
                  Wb - width of the operand in bits (16, 32 or 64)

    NOTE - InterlockedBitTestAndSetStr without width specification (ie. number
           in the name) will call InterlockedBitTestAndSetStr32 in 32bit
           program and InterlockedBitTestStr64 in 64bit program.


  InterlockedBitTestAndSetStrSec

    This function is not based around a processor instruction (BTS instruction
    does not have byte-operand version), it is merely a wrapper with similar
    functionality.
    Unlike its counterparts, it access only one byte - the one needed to reach
    the requested bit - so it is more secure in this regard, but slower.

-------------------------------------------------------------------------------}

Function InterlockedBitTestAndSetStr16(Str: Pointer; BitOffset: Int16): Boolean; register; assembler;
Function InterlockedBitTestAndSetStr32(Str: Pointer; BitOffset: Int32): Boolean; register; assembler;
{$IFDEF x64}
Function InterlockedBitTestAndSetStr64(Str: Pointer; BitOffset: Int64): Boolean; register; assembler;
{$ENDIF}

Function InterlockedBitTestAndSetStr(Str: Pointer; BitOffset: PtrInt): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTestAndSetStrSec(Str: Pointer; BitOffset: PtrInt): Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                         Interlocked bit test and reset
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedBitTestAndReset

    Resets/clears a bit (changes it to 0) selected by a parameter Bit in
    variable (pointed to by) I and returns original value of this bit.

    Actual bit position in masked so only lowest 3, 4, 5 or 6 bits, depending
    on the width of the variable, of the passed Bit parameter are used,
    effectively taking modulo 8, 16, 32 or 64 of the value.

-------------------------------------------------------------------------------}

Function InterlockedBitTestAndReset8(I: Pointer; Bit: Integer): Boolean; register; assembler;
Function InterlockedBitTestAndReset16(I: Pointer; Bit: Integer): Boolean; register; assembler;
Function InterlockedBitTestAndReset32(I: Pointer; Bit: Integer): Boolean; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedBitTestAndReset64(I: Pointer; Bit: Integer): Boolean; register; assembler;
{$ENDIF}
Function InterlockedBitTestAndResetPtr(I: Pointer; Bit: Integer): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestAndReset(var I: UInt8; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndReset(var I: Int8; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTestAndReset(var I: UInt16; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndReset(var I: Int16; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTestAndReset(var I: UInt32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndReset(var I: Int32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedBitTestAndReset(var I: UInt64; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndReset(var I: Int64; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedBitTestAndReset(var I: Pointer; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                    Bit string interlocked bit test and reset                    
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedBitTestAndResetStr

    Resets a bit selected by paramter BitOffset in a bit string pointed to by
    Str and returns original value of this bit.

    For more information refer to description of InterlockedBitTestAndSetStr.

  InterlockedBitTestAndResetStrSec

    This function is not based around a processor instruction (BTR instruction
    does not have byte-operand version), it is merely a wrapper with similar
    functionality.
    Unlike its counterparts, it access only one byte - the one needed to reach
    the requested bit - so it is more secure in this regard, but slower.

-------------------------------------------------------------------------------}

Function InterlockedBitTestAndResetStr16(Str: Pointer; BitOffset: Int16): Boolean; register; assembler;
Function InterlockedBitTestAndResetStr32(Str: Pointer; BitOffset: Int32): Boolean; register; assembler;
{$IFDEF x64}
Function InterlockedBitTestAndResetStr64(Str: Pointer; BitOffset: Int64): Boolean; register; assembler;
{$ENDIF}

Function InterlockedBitTestAndResetStr(Str: Pointer; BitOffset: PtrInt): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTestAndResetStrSec(Str: Pointer; BitOffset: PtrInt): Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked bit test and complement
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedBitTestAndComplement

    Complements a bit (swiches its value - if it was 0, it will become 1 and
    vice-versa) selected by a parameter Bit in variable (pointed to by) I and
    returns original value of this bit.

    Actual bit position in masked so only lowest 3, 4, 5 or 6 bits, depending
    on the width of the variable, of the passed Bit parameter are used,
    effectively taking modulo 8, 16, 32 or 64 of the value.
        
-------------------------------------------------------------------------------}

Function InterlockedBitTestAndComplement8(I: Pointer; Bit: Integer): Boolean; register; assembler;
Function InterlockedBitTestAndComplement16(I: Pointer; Bit: Integer): Boolean; register; assembler;
Function InterlockedBitTestAndComplement32(I: Pointer; Bit: Integer): Boolean; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedBitTestAndComplement64(I: Pointer; Bit: Integer): Boolean; register; assembler;
{$ENDIF}
Function InterlockedBitTestAndComplementPtr(I: Pointer; Bit: Integer): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestAndComplement(var I: UInt8; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndComplement(var I: Int8; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTestAndComplement(var I: UInt16; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndComplement(var I: Int16; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTestAndComplement(var I: UInt32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndComplement(var I: Int32; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedBitTestAndComplement(var I: UInt64; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedBitTestAndComplement(var I: Int64; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedBitTestAndComplement(var I: Pointer; Bit: Integer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                 Bit string interlocked bit test and complement
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedBitTestAndComplementStr

    Complements a bit selected by paramter BitOffset in a bit string pointed
    to by Str and returns original value of this bit.

    For more information refer to description of InterlockedBitTestAndSetStr.

  InterlockedBitTestAndComplementStrSec

    This function is not based around a processor instruction (BTC instruction
    does not have byte-operand version), it is merely a wrapper with similar
    functionality.
    Unlike its counterparts, it access only one byte - the one needed to reach
    the requested bit - so it is more secure in this regard, but slower.

-------------------------------------------------------------------------------}

Function InterlockedBitTestAndComplementStr16(Str: Pointer; BitOffset: Int16): Boolean; register; assembler;
Function InterlockedBitTestAndComplementStr32(Str: Pointer; BitOffset: Int32): Boolean; register; assembler;
{$IFDEF x64}
Function InterlockedBitTestAndComplementStr64(Str: Pointer; BitOffset: Int64): Boolean; register; assembler;
{$ENDIF}

Function InterlockedBitTestAndComplementStr(Str: Pointer; BitOffset: PtrInt): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedBitTestAndComplementStrSec(Str: Pointer; BitOffset: PtrInt): Boolean;

{===============================================================================
--------------------------------------------------------------------------------
                                Interlocked load
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedLoad

    Atomically obtains value of variable (pointed to by) I and returns it.

-------------------------------------------------------------------------------}

Function InterlockedLoad8(I: Pointer): UInt8; register; assembler;
Function InterlockedLoad16(I: Pointer): UInt16; register; assembler;
Function InterlockedLoad32(I: Pointer): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedLoad64(I: Pointer): UInt64; register; assembler;
{$ENDIF}
Function InterlockedLoadPtr(I: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedLoadBool(I: Pointer): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedLoad(var I: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedLoad(var I: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedLoad(var I: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedLoad(var I: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedLoad(var I: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedLoad(var I: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedLoad(var I: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedLoad(var I: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedLoad(var I: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedLoad(var I: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                               Interlocked store                                                               
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  InterlockedStore

    Sets variable (pointed to by) I to a value passed in parameter NewValue
    and returns original value of I.

-------------------------------------------------------------------------------}

Function InterlockedStore8(I: Pointer; NewValue: UInt8): UInt8; register; assembler;
Function InterlockedStore16(I: Pointer; NewValue: UInt16): UInt16; register; assembler;
Function InterlockedStore32(I: Pointer; NewValue: UInt32): UInt32; register; assembler;
{$IFDEF IncludeVal64}
Function InterlockedStore64(I: Pointer; NewValue: UInt64): UInt64; register; assembler;
{$ENDIF}
Function InterlockedStorePtr(I: Pointer; NewValue: Pointer): Pointer;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedStoreBool(I: Pointer; NewValue: Boolean): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedStore(var I: UInt8; NewValue: UInt8): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedStore(var I: Int8; NewValue: Int8): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedStore(var I: UInt16; NewValue: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedStore(var I: Int16; NewValue: Int16): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedStore(var I: UInt32; NewValue: UInt32): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedStore(var I: Int32; NewValue: Int32): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

{$IFDEF IncludeVal64}
Function InterlockedStore(var I: UInt64; NewValue: UInt64): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function InterlockedStore(var I: Int64; NewValue: Int64): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$ENDIF}

Function InterlockedStore(var I: Pointer; NewValue: Pointer): Pointer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function InterlockedStore(var I: Boolean; NewValue: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                Utility funtions
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------

  ReadBarrier

    Calling this function guarantees that all memory loads issued prior the
    call will be complete before next load operation after the call.

-------------------------------------------------------------------------------}

procedure ReadBarrier; register; assembler;

{-------------------------------------------------------------------------------

  WriteBarrier

    Calling this function guarantees that all memory stores issued prior the
    call will be complete before next store operation after the call.

-------------------------------------------------------------------------------}

procedure WriteBarrier; register; assembler;

{-------------------------------------------------------------------------------

  ReadWriteBarrier

    Calling this function guarantees that all memory loads and stores issued
    prior the call will be complete before next load or store operation after
    the call.

-------------------------------------------------------------------------------}

procedure ReadWriteBarrier; register; assembler;

implementation

{$IFDEF AssertInstructions}
uses
  SimpleCPUID;
{$ENDIF}

// following cannot go any higner because of older FPC (internal error 200501152)
{$IF SizeOf(Pointer) = 8}
  {$DEFINE Ptr64}
{$ELSEIF SizeOf(Pointer) <> 4}
  {$MESSAGE FATAL 'Unsupported size of pointers.'}
{$IFEND}

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W6018:={$WARN 6018 OFF}} // unreachable code
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              Auxiliary functions                                                            
--------------------------------------------------------------------------------
===============================================================================}

Function SAR3(Value: PtrInt): PtrInt; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    SAR   RCX, 3
    MOV   RAX, RCX
  {$ELSE}
    SAR   RDI,3
    MOV   RAX, RDI
  {$ENDIF}
{$ELSE}
    SAR   EAX, 3
{$ENDIF}
end;

{===============================================================================
--------------------------------------------------------------------------------
                             Interlocked increment
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedIncrement8(I: Pointer): UInt8;
asm
          MOV   DL, 1
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  XADD  byte ptr [RCX], DL
  {$ELSE}
    LOCK  XADD  byte ptr [RDI], DL
  {$ENDIF}
{$ELSE}
    LOCK  XADD  byte ptr [EAX], DL
{$ENDIF}
          MOV   AL, DL
          INC   AL
end;

//------------------------------------------------------------------------------

Function InterlockedIncrement16(I: Pointer): UInt16;
asm
          MOV   DX, 1
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  XADD  word ptr [RCX], DX
  {$ELSE}
    LOCK  XADD  word ptr [RDI], DX
  {$ENDIF}
{$ELSE}
    LOCK  XADD  word ptr [EAX], DX
{$ENDIF}
          MOV   AX, DX
          INC   AX
end;

//------------------------------------------------------------------------------

Function InterlockedIncrement32(I: Pointer): UInt32;
asm
          MOV   EDX, 1
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  XADD  dword ptr [RCX], EDX
  {$ELSE}
    LOCK  XADD  dword ptr [RDI], EDX
  {$ENDIF}
{$ELSE}
    LOCK  XADD  dword ptr [EAX], EDX
{$ENDIF}
          MOV   EAX, EDX
          INC   EAX
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedIncrement64(I: Pointer): UInt64;
asm
{$IFDEF x64}

          MOV   RDX, 1
  {$IFDEF Windows}
    LOCK  XADD  qword ptr [RCX], RDX
  {$ELSE}
    LOCK  XADD  qword ptr [RDI], RDX
  {$ENDIF}
          MOV   RAX, RDX
          INC   RAX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

          ADD   EBX, 1
          ADC   ECX, 0

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          MOV   EAX, EBX
          MOV   EDX, ECX

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedIncrementPtr(I: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedIncrement64(I));
{$ELSE}
Result := Pointer(InterlockedIncrement32(I));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

Function InterlockedIncrement(var I: UInt8): UInt8;
begin
Result := InterlockedIncrement8(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedIncrement(var I: Int8): Int8;
begin
Result := Int8(InterlockedIncrement8(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedIncrement(var I: UInt16): UInt16;
begin
Result := InterlockedIncrement16(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedIncrement(var I: Int16): Int16;
begin
Result := Int16(InterlockedIncrement16(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedIncrement(var I: UInt32): UInt32;
begin
Result := InterlockedIncrement32(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedIncrement(var I: Int32): Int32;
begin
Result := Int32(InterlockedIncrement32(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedIncrement(var I: UInt64): UInt64;
begin
Result := InterlockedIncrement64(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedIncrement(var I: Int64): Int64;
begin
Result := Int64(InterlockedIncrement64(@I));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedIncrement(var I: Pointer): Pointer;
begin
Result := InterlockedIncrementPtr(@I);
end;


{===============================================================================
--------------------------------------------------------------------------------
                             Interlocked decrement                              
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedDecrement8(I: Pointer): UInt8;
asm
          MOV   DL, byte(-1)
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  XADD  byte ptr [RCX], DL
  {$ELSE}
    LOCK  XADD  byte ptr [RDI], DL
  {$ENDIF}
{$ELSE}
    LOCK  XADD  byte ptr [EAX], DL
{$ENDIF}
          MOV   AL, DL
          DEC   AL
end;

//------------------------------------------------------------------------------

Function InterlockedDecrement16(I: Pointer): UInt16;
asm
          MOV   DX, word(-1)
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  XADD  word ptr [RCX], DX
  {$ELSE}
    LOCK  XADD  word ptr [RDI], DX
  {$ENDIF}
{$ELSE}
    LOCK  XADD  word ptr [EAX], DX
{$ENDIF}
          MOV   AX, DX
          DEC   AX
end;

//------------------------------------------------------------------------------

Function InterlockedDecrement32(I: Pointer): UInt32;
asm
          MOV   EDX, dword(-1)
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  XADD  dword ptr [RCX], EDX
  {$ELSE}
    LOCK  XADD  dword ptr [RDI], EDX
  {$ENDIF}
{$ELSE}
    LOCK  XADD  dword ptr [EAX], EDX
{$ENDIF}
          MOV   EAX, EDX
          DEC   EAX
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedDecrement64(I: Pointer): UInt64;
asm
{$IFDEF x64}

          MOV   RDX, qword(-1)
  {$IFDEF Windows}
    LOCK  XADD  qword ptr [RCX], RDX
  {$ELSE}
    LOCK  XADD  qword ptr [RDI], RDX
  {$ENDIF}
          MOV   RAX, RDX
          DEC   RAX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

          SUB   EBX, 1
          SBB   ECX, 0

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          MOV   EAX, EBX
          MOV   EDX, ECX

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedDecrementPtr(I: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedDecrement64(I));
{$ELSE}
Result := Pointer(InterlockedDecrement32(I));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

Function InterlockedDecrement(var I: UInt8): UInt8;
begin
Result := InterlockedDecrement8(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrement(var I: Int8): Int8;
begin
Result := Int8(InterlockedDecrement8(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrement(var I: UInt16): UInt16;
begin
Result := InterlockedDecrement16(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrement(var I: Int16): Int16;
begin
Result := Int16(InterlockedDecrement16(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrement(var I: UInt32): UInt32;
begin
Result := InterlockedDecrement32(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrement(var I: Int32): Int32;
begin
Result := Int32(InterlockedDecrement32(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedDecrement(var I: UInt64): UInt64;
begin
Result := InterlockedDecrement64(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrement(var I: Int64): Int64;
begin
Result := Int64(InterlockedDecrement64(@I));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrement(var I: Pointer): Pointer;
begin
Result := InterlockedDecrementPtr(@I);
end;

{===============================================================================
--------------------------------------------------------------------------------
                        Interlocked decrement if positive
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedDecrementIfPositive8(I: Pointer): UInt8;
begin
Result := InterlockedDecrementIfPositive(UInt8(I^));
end;

//------------------------------------------------------------------------------

Function InterlockedDecrementIfPositive16(I: Pointer): UInt16;
begin
Result := InterlockedDecrementIfPositive(UInt16(I^));
end;

//------------------------------------------------------------------------------

Function InterlockedDecrementIfPositive32(I: Pointer): UInt32;
begin
Result := InterlockedDecrementIfPositive(UInt32(I^));
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedDecrementIfPositive64(I: Pointer): UInt64;
begin
Result := InterlockedDecrementIfPositive(UInt64(I^));
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedDecrementIfPositivePtr(I: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedDecrementIfPositive64(I));
{$ELSE}
Result := Pointer(InterlockedDecrementIfPositive32(I));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

Function InterlockedDecrementIfPositive(var I: UInt8): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}
          MOV   DL, AL

          TEST  DL, DL
          JZ    @CompareEx

          DEC   DL

    @CompareEx:

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], DL
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], DL
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]
          MOV   DL, AL

          TEST  DL, DL
          JZ    @CompareEx

          DEC   DL

    @CompareEx:

    LOCK  CMPXCHG byte ptr [ECX], DL

          JNZ   @TryOutStart

{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrementIfPositive(var I: Int8): Int8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}
          MOV   DL, AL

          CMP   DL, 0
          JLE   @CompareEx

          DEC   DL

    @CompareEx:

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], DL
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], DL
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]
          MOV   DL, AL

          CMP   DL, 0
          JLE   @CompareEx

          DEC   DL

    @CompareEx:

    LOCK  CMPXCHG byte ptr [ECX], DL

          JNZ   @TryOutStart

{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrementIfPositive(var I: UInt16): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}
          MOV   DX, AX

          TEST  DX, DX
          JZ    @CompareEx

          DEC   DX

    @CompareEx:

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], DX
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], DX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]
          MOV   DX, AX

          TEST  DX, DX
          JZ    @CompareEx

          DEC   DX

    @CompareEx:

    LOCK  CMPXCHG word ptr [ECX], DX

          JNZ   @TryOutStart

{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrementIfPositive(var I: Int16): Int16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}
          MOV   DX, AX

          CMP   DX, 0
          JLE   @CompareEx

          DEC   DX

    @CompareEx:

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], DX
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], DX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]
          MOV   DX, AX

          CMP   DX, 0
          JLE   @CompareEx

          DEC   DX

    @CompareEx:

    LOCK  CMPXCHG word ptr [ECX], DX

          JNZ   @TryOutStart

{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrementIfPositive(var I: UInt32): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}
          MOV   EDX, EAX

          TEST  EDX, EDX
          JZ    @CompareEx

          DEC   EDX

    @CompareEx:

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], EDX
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], EDX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]
          MOV   EDX, EAX

          TEST  EDX, EDX
          JZ    @CompareEx

          DEC   EDX

    @CompareEx:

    LOCK  CMPXCHG dword ptr [ECX], EDX

          JNZ   @TryOutStart

{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrementIfPositive(var I: Int32): Int32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}
          MOV   EDX, EAX

          CMP   EDX, 0
          JLE   @CompareEx

          DEC   EDX

    @CompareEx:

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], EDX
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], EDX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]
          MOV   EDX, EAX

          CMP   EDX, 0
          JLE   @CompareEx

          DEC   EDX

    @CompareEx:

    LOCK  CMPXCHG dword ptr [ECX], EDX

          JNZ   @TryOutStart

{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedDecrementIfPositive(var I: UInt64): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}
          MOV   RDX, RAX

          TEST  RDX, RDX
          JZ    @CompareEx

          DEC   RDX

    @CompareEx:

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], RDX
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], RDX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]
          MOV   EBX, EAX
          MOV   ECX, EDX

          TEST  EBX, EBX
          JNZ   @Decrement
          TEST  ECX, ECX
          JZ    @CompareEx

    @Decrement:

          SUB   EBX, 1
          SBB   ECX, 0

    @CompareEx:

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrementIfPositive(var I: Int64): Int64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}
          MOV   RDX, RAX

          CMP   RDX, 0
          JLE   @CompareEx

          DEC   RDX

    @CompareEx:

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], RDX
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], RDX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]
          MOV   EBX, EAX
          MOV   ECX, EDX

        {
          First compare high 32bit signed integer to 0.

          If it is negative, then entire number is negative and we can skip the
          subtraction. If it is positive, do the decrement right avay because
          entire number is positive. If it is zero, we have to also check the
          lower integer (it is enough to test for zero).

          If the lower integer is zero, then entire number is zero and skip the
          decrement, otherwise perform it.
        }
          CMP   ECX, 0
          JL    @CompareEx
          JG    @Decrement

          // higher 32bits are zero
          TEST  EBX, EBX
          JZ    @CompareEx

    @Decrement:

          SUB   EBX, 1
          SBB   ECX, 0

    @CompareEx:

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedDecrementIfPositive(var I: Pointer): Pointer;
begin
Result := InterlockedDecrementIfPositivePtr(@I);
end;

{===============================================================================
--------------------------------------------------------------------------------
                              Interlocked addition
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedAdd8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   AL, DL
    LOCK  XADD  byte ptr [RCX], AL
          ADD   AL, DL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   AL, SIL
    LOCK  XADD  byte ptr [RDI], AL
          ADD   AL, SIL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   CL, DL
    LOCK  XADD  byte ptr [EAX], DL
          MOV   AL, DL
          ADD   AL, CL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedAdd16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   AX, DX
    LOCK  XADD  word ptr [RCX], AX
          ADD   AX, DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   AX, SI
    LOCK  XADD  word ptr [RDI], AX
          ADD   AX, SI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   CX, DX
    LOCK  XADD  word ptr [EAX], DX
          MOV   AX, DX
          ADD   AX, CX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedAdd32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   EAX, EDX
    LOCK  XADD  dword ptr [RCX], EAX
          ADD   EAX, EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   EAX, ESI
    LOCK  XADD  dword ptr [RDI], EAX
          ADD   EAX, ESI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EDX
    LOCK  XADD  dword ptr [EAX], EDX
          MOV   EAX, EDX
          ADD   EAX, ECX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedAdd64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   RAX, RDX
    LOCK  XADD  qword ptr [RCX], RAX
          ADD   RAX, RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   RAX, RSI
    LOCK  XADD  qword ptr [RDI], RAX
          ADD   RAX, RSI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EBX, dword ptr [B]
          MOV   ECX, dword ptr [B + 4]

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          ADD   EBX, EAX
          ADC   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          MOV   EAX, EBX
          MOV   EDX, ECX

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedAddPtr(A: Pointer; B: PtrInt): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedAdd64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedAdd32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAddPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedAdd64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedAdd32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

Function InterlockedAdd(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedAdd8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAdd(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedAdd8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAdd(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedAdd16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAdd(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedAdd16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAdd(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedAdd32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAdd(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedAdd32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedAdd(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedAdd64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAdd(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedAdd64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAdd(var A: Pointer; B: PtrInt): Pointer;
begin
Result := InterlockedAddPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAdd(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedAddPtr(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                            Interlocked subtraction
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedSub8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   AL, DL
          NEG   AL
    LOCK  XADD  byte ptr [RCX], AL
          SUB   AL, DL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   AL, SIL
          NEG   AL
    LOCK  XADD  byte ptr [RDI], AL
          SUB   AL, SIL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   CL, DL
          NEG   CL
    LOCK  XADD  byte ptr [EAX], CL
          MOV   AL, CL
          SUB   AL, DL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedSub16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   AX, DX
          NEG   AX
    LOCK  XADD  word ptr [RCX], AX
          SUB   AX, DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   AX, SI
          NEG   AX
    LOCK  XADD  word ptr [RDI], AX
          SUB   AX, SI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   CX, DX
          NEG   CX
    LOCK  XADD  word ptr [EAX], CX
          MOV   AX, CX
          SUB   AX, DX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedSub32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   EAX, EDX
          NEG   EAX
    LOCK  XADD  dword ptr [RCX], EAX
          SUB   EAX, EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   EAX, ESI
          NEG   EAX
    LOCK  XADD  dword ptr [RDI], EAX
          SUB   EAX, ESI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EDX
          NEG   ECX
    LOCK  XADD  dword ptr [EAX], ECX
          MOV   EAX, ECX
          SUB   EAX, EDX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedSub64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   RAX, RDX
          NEG   RAX
    LOCK  XADD  qword ptr [RCX], RAX
          SUB   RAX, RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   RAX, RSI
          NEG   RAX
    LOCK  XADD  qword ptr [RDI], RAX
          SUB   RAX, RSI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

          SUB   EBX, dword ptr [B]
          SBB   ECX, dword ptr [B + 4]

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          MOV   EAX, EBX
          MOV   EDX, ECX

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedSubPtr(A: Pointer; B: PtrInt): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedSub64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedSub32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedSubPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedSub64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedSub32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

Function InterlockedSub(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedSub8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedSub(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedSub8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedSub(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedSub16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedSub(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedSub16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedSub(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedSub32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedSub(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedSub32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedSub(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedSub64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedSub(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedSub64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedSub(var A: Pointer; B: PtrInt): Pointer;
begin
Result := InterlockedSubPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedSub(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedSubPtr(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Interlocked negation
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedNeg8(I: Pointer): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   DL, AL
          NEG   DL

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], DL
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], DL
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AL, DL

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   DL, AL
          NEG   DL

    LOCK  CMPXCHG byte ptr [ECX], DL

          JNZ   @TryOutStart

          MOV   AL, DL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedNeg16(I: Pointer): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   DX, AX
          NEG   DX

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], DX
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], DX
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AX, DX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   DX, AX
          NEG   DX

    LOCK  CMPXCHG word ptr [ECX], DX

          JNZ   @TryOutStart

          MOV   AX, DX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedNeg32(I: Pointer): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   EDX, EAX
          NEG   EDX

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], EDX
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], EDX
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   EAX, EDX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EDX, EAX
          NEG   EDX

    LOCK  CMPXCHG dword ptr [ECX], EDX

          JNZ   @TryOutStart

          MOV   EAX, EDX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedNeg64(I: Pointer): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   RDX, RAX
          NEG   RDX

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], RDX
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], RDX
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   RAX, RDX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          XOR   EBX, EBX
          XOR   ECX, ECX

          SUB   EBX, EAX
          SBB   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          MOV   EAX, EBX
          MOV   EDX, ECX

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedNegPtr(I: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedNeg64(I));
{$ELSE}
Result := Pointer(InterlockedNeg32(I));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

Function InterlockedNeg(var I: UInt8): UInt8;
begin
Result := InterlockedNeg8(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNeg(var I: Int8): Int8;
begin
Result := Int8(InterlockedNeg8(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNeg(var I: UInt16): UInt16;
begin
Result := InterlockedNeg16(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNeg(var I: Int16): Int16;
begin
Result := Int16(InterlockedNeg16(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNeg(var I: UInt32): UInt32;
begin
Result := InterlockedNeg32(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNeg(var I: Int32): Int32;
begin
Result := Int32(InterlockedNeg32(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedNeg(var I: UInt64): UInt64;
begin
Result := InterlockedNeg64(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNeg(var I: Int64): Int64;
begin
Result := Int64(InterlockedNeg64(@I));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNeg(var I: Pointer): Pointer;
begin
Result := InterlockedNegPtr(@I);
end;  


{===============================================================================
--------------------------------------------------------------------------------
                            Interlocked logical not
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedNot8(I: Pointer): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   DL, AL
          NOT   DL

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], DL
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], DL
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AL, DL

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   DL, AL
          NOT   DL

    LOCK  CMPXCHG byte ptr [ECX], DL

          JNZ   @TryOutStart

          MOV   AL, DL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedNot16(I: Pointer): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   DX, AX
          NOT   DX

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], DX
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], DX
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AX, DX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   DX, AX
          NOT   DX

    LOCK  CMPXCHG word ptr [ECX], DX

          JNZ   @TryOutStart

          MOV   AX, DX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedNot32(I: Pointer): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   EDX, EAX
          NOT   EDX

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], EDX
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], EDX
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   EAX, EDX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EDX, EAX
          NOT   EDX

    LOCK  CMPXCHG dword ptr [ECX], EDX

          JNZ   @TryOutStart

          MOV   EAX, EDX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedNot64(I: Pointer): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   RDX, RAX
          NOT   RDX

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], RDX
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], RDX
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   RAX, RDX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

          NOT   EBX
          NOT   ECX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          MOV   EAX, EBX
          MOV   EDX, ECX

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedNotPtr(I: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedNot64(I));
{$ELSE}
Result := Pointer(InterlockedNot32(I));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedNotBool(I: Pointer): Boolean;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   DL, AL
          NOT   DL
          AND   DL, 1

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], DL
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], DL
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AL, DL

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   DL, AL
          NOT   DL
          AND   DL, 1

    LOCK  CMPXCHG byte ptr [ECX], DL

          JNZ   @TryOutStart

          MOV   AL, DL

{$ENDIF}
end;

//==============================================================================

Function InterlockedNot(var I: UInt8): UInt8;
begin
Result := InterlockedNot8(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNot(var I: Int8): Int8;
begin
Result := Int8(InterlockedNot8(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNot(var I: UInt16): UInt16;
begin
Result := InterlockedNot16(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNot(var I: Int16): Int16;
begin
Result := Int16(InterlockedNot16(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNot(var I: UInt32): UInt32;
begin
Result := InterlockedNot32(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNot(var I: Int32): Int32;
begin
Result := Int32(InterlockedNot32(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedNot(var I: UInt64): UInt64;
begin
Result := InterlockedNot64(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNot(var I: Int64): Int64;
begin
Result := Int64(InterlockedNot64(@I));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNot(var I: Pointer): Pointer;
begin
Result := InterlockedNotPtr(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedNot(var I: Boolean): Boolean;
begin
Result := InterlockedNotBool(@I);
end;


{===============================================================================
--------------------------------------------------------------------------------
                            Interlocked logical and
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedAnd8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          AND   R8B, B

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AL, R8B

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          AND   BL, DL

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          MOV   AL, BL
          
          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedAnd16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   R8W, AX
          AND   R8W, B

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], R8W
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], R8W
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AX, R8W

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   BX, AX
          AND   BX, DX

    LOCK  CMPXCHG word ptr [ECX], BX

          JNZ   @TryOutStart

          MOV   AX, BX
          
          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedAnd32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   R8D, EAX
          AND   R8D, B

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], R8D
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], R8D
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   EAX, R8D

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EBX, EAX
          AND   EBX, EDX

    LOCK  CMPXCHG dword ptr [ECX], EBX

          JNZ   @TryOutStart

          MOV   EAX, EBX

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedAnd64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   R8, RAX
          AND   R8, B

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], R8
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], R8
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   RAX, R8

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, dword ptr [B]
          MOV   ECX, dword ptr [B + 4]

          AND   EBX, EAX
          AND   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          MOV   EAX, EBX
          MOV   EDX, ECX

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedAndPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedAnd64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedAnd32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedAndBool(A: Pointer; B: Boolean): Boolean;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          AND   R8B, B
          AND   R8B, 1

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AL, R8B

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          AND   BL, DL
          AND   BL, 1

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          MOV   AL, BL
          
          POP   EBX

{$ENDIF}
end;

//==============================================================================

Function InterlockedAnd(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedAnd8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAnd(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedAnd8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAnd(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedAnd16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAnd(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedAnd16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAnd(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedAnd32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAnd(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedAnd32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedAnd(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedAnd64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAnd(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedAnd64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAnd(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedAndPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedAnd(var A: Boolean; B: Boolean): Boolean;
begin
Result := InterlockedAndBool(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                             Interlocked logical or
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedOr8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          OR    R8B, B

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AL, R8B

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          OR    BL, DL

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          MOV   AL, BL
          
          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedOr16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   R8W, AX
          OR    R8W, B

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], R8W
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], R8W
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AX, R8W

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   BX, AX
          OR    BX, DX

    LOCK  CMPXCHG word ptr [ECX], BX

          JNZ   @TryOutStart

          MOV   AX, BX
          
          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedOr32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   R8D, EAX
          OR    R8D, B

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], R8D
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], R8D
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   EAX, R8D

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EBX, EAX
          OR    EBX, EDX

    LOCK  CMPXCHG dword ptr [ECX], EBX

          JNZ   @TryOutStart

          MOV   EAX, EBX

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedOr64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   R8, RAX
          OR    R8, B

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], R8
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], R8
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   RAX, R8

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, dword ptr [B]
          MOV   ECX, dword ptr [B + 4]

          OR    EBX, EAX
          OR    ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          MOV   EAX, EBX
          MOV   EDX, ECX

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedOrPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedOr64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedOr32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedOrBool(A: Pointer; B: Boolean): Boolean;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          OR    R8B, B
          AND   R8B, 1

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AL, R8B

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          OR    BL, DL
          AND   BL, 1

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          MOV   AL, BL
          
          POP   EBX

{$ENDIF}
end;

//==============================================================================

Function InterlockedOr(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedOr8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedOr(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedOr8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedOr(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedOr16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedOr(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedOr16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedOr(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedOr32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedOr(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedOr32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedOr(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedOr64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedOr(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedOr64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedOr(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedOrPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedOr(var A: Boolean; B: Boolean): Boolean;
begin
Result := InterlockedOrBool(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                             Interlocked logical xor
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedXor8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          XOR   R8B, B

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AL, R8B

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          XOR   BL, DL

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          MOV   AL, BL
          
          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedXor16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   R8W, AX
          XOR   R8W, B

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], R8W
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], R8W
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AX, R8W

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   BX, AX
          XOR   BX, DX

    LOCK  CMPXCHG word ptr [ECX], BX

          JNZ   @TryOutStart

          MOV   AX, BX
          
          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedXor32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   R8D, EAX
          XOR   R8D, B

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], R8D
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], R8D
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   EAX, R8D

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EBX, EAX
          XOR   EBX, EDX

    LOCK  CMPXCHG dword ptr [ECX], EBX

          JNZ   @TryOutStart

          MOV   EAX, EBX

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedXor64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   R8, RAX
          XOR   R8, B

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], R8
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], R8
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   RAX, R8

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, dword ptr [B]
          MOV   ECX, dword ptr [B + 4]

          XOR   EBX, EAX
          XOR   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          MOV   EAX, EBX
          MOV   EDX, ECX

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedXorPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedXor64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedXor32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedXorBool(A: Pointer; B: Boolean): Boolean;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          XOR   R8B, B
          AND   R8B, 1

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

          MOV   AL, R8B

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          XOR   BL, DL
          AND   BL, 1

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          MOV   AL, BL
          
          POP   EBX

{$ENDIF}
end;

//==============================================================================

Function InterlockedXor(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedXor8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedXor(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedXor8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedXor(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedXor16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedXor(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedXor16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedXor(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedXor32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedXor(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedXor32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedXor(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedXor64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedXor(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedXor64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedXor(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedXorPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedXor(var A: Boolean; B: Boolean): Boolean;
begin
Result := InterlockedXorBool(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Interlocked exchange
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedExchange8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XCHG  byte ptr [RCX], DL
          MOV   AL, DL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  byte ptr [RDI], SIL
          MOV   AL, SIL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  byte ptr [EAX], DL
          MOV   AL, DL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchange16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XCHG  word ptr [RCX], DX
          MOV   AX, DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  word ptr [RDI], SI
          MOV   AX, SI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  word ptr [EAX], DX
          MOV   AX, DX
          
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchange32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XCHG  dword ptr [RCX], EDX
          MOV   EAX, EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  dword ptr [RDI], ESI
          MOV   EAX, ESI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  dword ptr [EAX], EDX
          MOV   EAX, EDX
          
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedExchange64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XCHG  qword ptr [RCX], RDX
          MOV   RAX, RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  qword ptr [RDI], RSI
          MOV   RAX, RSI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EBX, dword ptr [B]
          MOV   ECX, dword ptr [B + 4]

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]          

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangePtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchange64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedExchange32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeBool(A: Pointer; B: Boolean): Boolean;
begin
Result := Boolean(InterlockedExchange8(A,UInt8(B)));
end;

//==============================================================================

Function InterlockedExchange(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedExchange8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchange(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedExchange8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchange(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedExchange16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchange(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedExchange16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchange(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedExchange32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchange(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedExchange32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedExchange(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedExchange64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchange(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedExchange64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchange(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedExchangePtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchange(var A: Boolean; B: Boolean): Boolean;
begin
Result := InterlockedExchangeBool(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                          Interlocked exchange and add
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedExchangeAdd8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XADD  byte ptr [RCX], DL
          MOV   AL, DL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XADD  byte ptr [RDI], SIL
          MOV   AL, SIL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XADD  byte ptr [EAX], DL
          MOV   AL, DL
          
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeAdd16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XADD  word ptr [RCX], DX
          MOV   AX, DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XADD  word ptr [RDI], SI
          MOV   AX, SI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XADD  word ptr [EAX], DX
          MOV   AX, DX
          
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeAdd32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XADD  dword ptr [RCX], EDX
          MOV   EAX, EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XADD  dword ptr [RDI], ESI
          MOV   EAX, ESI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XADD  dword ptr [EAX], EDX
          MOV   EAX, EDX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedExchangeAdd64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XADD  qword ptr [RCX], RDX
          MOV   RAX, RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XADD  qword ptr [RDI], RSI
          MOV   RAX, RSI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EBX, dword ptr [B]
          MOV   ECX, dword ptr [B + 4]

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          ADD   EBX, EAX
          ADC   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeAddPtr(A: Pointer; B: PtrInt): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchangeAdd64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedExchangeAdd32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAddPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchangeAdd64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedExchangeAdd32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

Function InterlockedExchangeAdd(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedExchangeAdd8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAdd(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedExchangeAdd8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAdd(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedExchangeAdd16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAdd(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedExchangeAdd16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAdd(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedExchangeAdd32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAdd(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedExchangeAdd32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedExchangeAdd(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedExchangeAdd64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAdd(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedExchangeAdd64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAdd(var A: Pointer; B: PtrInt): Pointer;
begin
Result := InterlockedExchangeAddPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAdd(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedExchangeAddPtr(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                       Interlocked exchange and subtract
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedExchangeSub8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          NEG   DL
    LOCK  XADD  byte ptr [RCX], DL
          MOV   AL, DL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          NEG   SIL
    LOCK  XADD  byte ptr [RDI], SIL
          MOV   AL, SIL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          NEG   DL
    LOCK  XADD  byte ptr [EAX], DL
          MOV   AL, DL
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeSub16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          NEG   DX
    LOCK  XADD  word ptr [RCX], DX
          MOV   AX, DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          NEG   SI
    LOCK  XADD  word ptr [RDI], SI
          MOV   AX, SI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          NEG   DX
    LOCK  XADD  word ptr [EAX], DX
          MOV   AX, DX
          
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeSub32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          NEG   EDX
    LOCK  XADD  dword ptr [RCX], EDX
          MOV   EAX, EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          NEG   ESI
    LOCK  XADD  dword ptr [RDI], ESI
          MOV   EAX, ESI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          NEG   EDX
    LOCK  XADD  dword ptr [EAX], EDX
          MOV   EAX, EDX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedExchangeSub64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          NEG   RDX
    LOCK  XADD  qword ptr [RCX], RDX
          MOV   RAX, RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          NEG   RSI
    LOCK  XADD  qword ptr [RDI], RSI
          MOV   RAX, RSI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

          SUB   EBX, dword ptr [B]
          SBB   ECX, dword ptr [B + 4]

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeSubPtr(A: Pointer; B: PtrInt): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchangeSub64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedExchangeSub32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeSubPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchangeSub64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedExchangeSub32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

Function InterlockedExchangeSub(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedExchangeSub8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeSub(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedExchangeSub8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeSub(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedExchangeSub16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeSub(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedExchangeSub16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeSub(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedExchangeSub32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeSub(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedExchangeSub32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedExchangeSub(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedExchangeSub64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeSub(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedExchangeSub64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeSub(var A: Pointer; B: PtrInt): Pointer;
begin
Result := InterlockedExchangeSubPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeSub(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedExchangeSubPtr(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                       Interlocked exchange and negation
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedExchangeNeg8(I: Pointer): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   DL, AL
          NEG   DL

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], DL
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], DL
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   DL, AL
          NEG   DL

    LOCK  CMPXCHG byte ptr [ECX], DL

          JNZ   @TryOutStart

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeNeg16(I: Pointer): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   DX, AX
          NEG   DX

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], DX
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], DX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   DX, AX
          NEG   DX

    LOCK  CMPXCHG word ptr [ECX], DX

          JNZ   @TryOutStart

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeNeg32(I: Pointer): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   EDX, EAX
          NEG   EDX

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], EDX
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], EDX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EDX, EAX
          NEG   EDX

    LOCK  CMPXCHG dword ptr [ECX], EDX

          JNZ   @TryOutStart

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedExchangeNeg64(I: Pointer): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   RDX, RAX
          NEG   RDX

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], RDX
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], RDX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          XOR   EBX, EBX
          XOR   ECX, ECX

          SUB   EBX, EAX
          SBB   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeNegPtr(I: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchangeNeg64(I));
{$ELSE}
Result := Pointer(InterlockedExchangeNeg32(I));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//==============================================================================

Function InterlockedExchangeNeg(var I: UInt8): UInt8;
begin
Result := InterlockedExchangeNeg8(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNeg(var I: Int8): Int8;
begin
Result := Int8(InterlockedExchangeNeg8(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNeg(var I: UInt16): UInt16;
begin
Result := InterlockedExchangeNeg16(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNeg(var I: Int16): Int16;
begin
Result := Int16(InterlockedExchangeNeg16(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNeg(var I: UInt32): UInt32;
begin
Result := InterlockedExchangeNeg32(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNeg(var I: Int32): Int32;
begin
Result := Int32(InterlockedExchangeNeg32(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedExchangeNeg(var I: UInt64): UInt64;
begin
Result := InterlockedExchangeNeg64(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNeg(var I: Int64): Int64;
begin
Result := Int64(InterlockedExchangeNeg64(@I));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNeg(var I: Pointer): Pointer;
begin
Result := InterlockedExchangeNegPtr(@I);
end;


{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked exchange and logical not
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedExchangeNot8(I: Pointer): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   DL, AL
          NOT   DL

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], DL
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], DL
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   DL, AL
          NOT   DL

    LOCK  CMPXCHG byte ptr [ECX], DL

          JNZ   @TryOutStart

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeNot16(I: Pointer): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   DX, AX
          NOT   DX

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], DX
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], DX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   DX, AX
          NOT   DX

    LOCK  CMPXCHG word ptr [ECX], DX

          JNZ   @TryOutStart

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeNot32(I: Pointer): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   EDX, EAX
          NOT   EDX

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], EDX
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], EDX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EDX, EAX
          NOT   EDX

    LOCK  CMPXCHG dword ptr [ECX], EDX

          JNZ   @TryOutStart

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedExchangeNot64(I: Pointer): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   RDX, RAX
          NOT   RDX

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], RDX
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], RDX
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

          NOT   EBX
          NOT   ECX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeNotPtr(I: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchangeNot64(I));
{$ELSE}
Result := Pointer(InterlockedExchangeNot32(I));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeNotBool(I: Pointer): Boolean;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   DL, AL
          NOT   DL
          AND   DL, 1

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], DL
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], DL
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   DL, AL
          NOT   DL
          AND   DL, 1

    LOCK  CMPXCHG byte ptr [ECX], DL

          JNZ   @TryOutStart

{$ENDIF}
end;

//==============================================================================

Function InterlockedExchangeNot(var I: UInt8): UInt8;
begin
Result := InterlockedExchangeNot8(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNot(var I: Int8): Int8;
begin
Result := Int8(InterlockedExchangeNot8(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNot(var I: UInt16): UInt16;
begin
Result := InterlockedExchangeNot16(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNot(var I: Int16): Int16;
begin
Result := Int16(InterlockedExchangeNot16(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNot(var I: UInt32): UInt32;
begin
Result := InterlockedExchangeNot32(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNot(var I: Int32): Int32;
begin
Result := Int32(InterlockedExchangeNot32(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedExchangeNot(var I: UInt64): UInt64;
begin
Result := InterlockedExchangeNot64(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNot(var I: Int64): Int64;
begin
Result := Int64(InterlockedExchangeNot64(@I));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNot(var I: Pointer): Pointer;
begin
Result := InterlockedExchangeNotPtr(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeNot(var I: Boolean): Boolean;
begin
Result := InterlockedExchangeNotBool(@I);
end;


{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked exchange and logical and
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedExchangeAnd8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          AND   R8B, B

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          AND   BL, DL

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeAnd16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   R8W, AX
          AND   R8W, B

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], R8W
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], R8W
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   BX, AX
          AND   BX, DX

    LOCK  CMPXCHG word ptr [ECX], BX

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeAnd32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   R8D, EAX
          AND   R8D, B

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], R8D
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], R8D
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EBX, EAX
          AND   EBX, EDX

    LOCK  CMPXCHG dword ptr [ECX], EBX

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedExchangeAnd64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   R8, RAX
          AND   R8, B

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], R8
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], R8
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, dword ptr [B]
          MOV   ECX, dword ptr [B + 4]

          AND   EBX, EAX
          AND   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeAndPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchangeAnd64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedExchangeAnd32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeAndBool(A: Pointer; B: Boolean): Boolean;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          AND   R8B, B
          AND   R8B, 1

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          AND   BL, DL
          AND   BL, 1

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//==============================================================================

Function InterlockedExchangeAnd(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedExchangeAnd8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAnd(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedExchangeAnd8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAnd(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedExchangeAnd16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAnd(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedExchangeAnd16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAnd(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedExchangeAnd32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAnd(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedExchangeAnd32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedExchangeAnd(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedExchangeAnd64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAnd(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedExchangeAnd64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAnd(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedExchangeAndPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeAnd(var A: Boolean; B: Boolean): Boolean;
begin
Result := InterlockedExchangeAndBool(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked exchange and logical or
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedExchangeOr8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          OR    R8B, B

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          OR    BL, DL

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeOr16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   R8W, AX
          OR    R8W, B

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], R8W
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], R8W
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   BX, AX
          OR    BX, DX

    LOCK  CMPXCHG word ptr [ECX], BX

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeOr32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   R8D, EAX
          OR    R8D, B

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], R8D
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], R8D
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EBX, EAX
          OR    EBX, EDX

    LOCK  CMPXCHG dword ptr [ECX], EBX

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedExchangeOr64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   R8, RAX
          OR    R8, B

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], R8
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], R8
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, dword ptr [B]
          MOV   ECX, dword ptr [B + 4]

          OR    EBX, EAX
          OR    ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeOrPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchangeOr64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedExchangeOr32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeOrBool(A: Pointer; B: Boolean): Boolean;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          OR    R8B, B
          AND   R8B, 1

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          OR    BL, DL
          AND   BL, 1

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//==============================================================================

Function InterlockedExchangeOr(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedExchangeOr8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOr(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedExchangeOr8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOr(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedExchangeOr16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOr(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedExchangeOr16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOr(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedExchangeOr32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOr(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedExchangeOr32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedExchangeOr(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedExchangeOr64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOr(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedExchangeOr64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOr(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedExchangeOrPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOr(var A: Boolean; B: Boolean): Boolean;
begin
Result := InterlockedExchangeOrBool(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked exchange and logical xor
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedExchangeXor8(A: Pointer; B: UInt8): UInt8;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          XOR   R8B, B

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          XOR   BL, DL

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeXor16(A: Pointer; B: UInt16): UInt16;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AX, word ptr [RCX]
  {$ELSE}
          MOV   AX, word ptr [RDI]
  {$ENDIF}

          MOV   R8W, AX
          XOR   R8W, B

  {$IFDEF Windows}
    LOCK  CMPXCHG word ptr [RCX], R8W
  {$ELSE}
    LOCK  CMPXCHG word ptr [RDI], R8W
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AX, word ptr [ECX]

          MOV   BX, AX
          XOR   BX, DX

    LOCK  CMPXCHG word ptr [ECX], BX

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeXor32(A: Pointer; B: UInt32): UInt32;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   EAX, dword ptr [RCX]
  {$ELSE}
          MOV   EAX, dword ptr [RDI]
  {$ENDIF}

          MOV   R8D, EAX
          XOR   R8D, B

  {$IFDEF Windows}
    LOCK  CMPXCHG dword ptr [RCX], R8D
  {$ELSE}
    LOCK  CMPXCHG dword ptr [RDI], R8D
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [ECX]

          MOV   EBX, EAX
          XOR   EBX, EDX

    LOCK  CMPXCHG dword ptr [ECX], EBX

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedExchangeXor64(A: Pointer; B: UInt64): UInt64;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   RAX, qword ptr [RCX]
  {$ELSE}
          MOV   RAX, qword ptr [RDI]
  {$ENDIF}

          MOV   R8, RAX
          XOR   R8, B

  {$IFDEF Windows}
    LOCK  CMPXCHG qword ptr [RCX], R8
  {$ELSE}
    LOCK  CMPXCHG qword ptr [RDI], R8
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, dword ptr [B]
          MOV   ECX, dword ptr [B + 4]

          XOR   EBX, EAX
          XOR   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeXorPtr(A: Pointer; B: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedExchangeXor64(A,UInt64(B)));
{$ELSE}
Result := Pointer(InterlockedExchangeXor32(A,UInt32(B)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeXorBool(A: Pointer; B: Boolean): Boolean;
asm
{$IFDEF x64}

    @TryOutStart:

  {$IFDEF Windows}
          MOV   AL, byte ptr [RCX]
  {$ELSE}
          MOV   AL, byte ptr [RDI]
  {$ENDIF}

          MOV   R8B, AL
          XOR   R8B, B
          AND   R8B, 1

  {$IFDEF Windows}
    LOCK  CMPXCHG byte ptr [RCX], R8B
  {$ELSE}
    LOCK  CMPXCHG byte ptr [RDI], R8B
  {$ENDIF}

          JNZ   @TryOutStart

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          XOR   BL, DL
          AND   BL, 1

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          POP   EBX

{$ENDIF}
end;

//==============================================================================

Function InterlockedExchangeXor(var A: UInt8; B: UInt8): UInt8;
begin
Result := InterlockedExchangeXor8(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeXor(var A: Int8; B: Int8): Int8;
begin
Result := Int8(InterlockedExchangeXor8(@A,UInt8(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeXor(var A: UInt16; B: UInt16): UInt16;
begin
Result := InterlockedExchangeXor16(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeXor(var A: Int16; B: Int16): Int16;
begin
Result := Int16(InterlockedExchangeXor16(@A,UInt16(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeXor(var A: UInt32; B: UInt32): UInt32;
begin
Result := InterlockedExchangeXor32(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeXor(var A: Int32; B: Int32): Int32;
begin
Result := Int32(InterlockedExchangeXor32(@A,UInt32(B)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedExchangeXor(var A: UInt64; B: UInt64): UInt64;
begin
Result := InterlockedExchangeXor64(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeXor(var A: Int64; B: Int64): Int64;
begin
Result := Int64(InterlockedExchangeXor64(@A,UInt64(B)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeXor(var A: Pointer; B: Pointer): Pointer;
begin
Result := InterlockedExchangeXorPtr(@A,B);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeXor(var A: Boolean; B: Boolean): Boolean;
begin
Result := InterlockedExchangeXorBool(@A,B);
end;


{===============================================================================
--------------------------------------------------------------------------------
                       Interlocked exchange and operation
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedExchangeOp8(A: Pointer; B: UInt8; Op: TInterlockedOp8u): UInt8;
begin
Result := InterlockedExchangeOp(UInt8(A^),B,Op);
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeOp16(A: Pointer; B: UInt16; Op: TInterlockedOp16u): UInt16;
begin
Result := InterlockedExchangeOp(UInt16(A^),B,Op);
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeOp32(A: Pointer; B: UInt32; Op: TInterlockedOp32u): UInt32;
begin
Result := InterlockedExchangeOp(UInt32(A^),B,Op);
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedExchangeOp64(A: Pointer; B: UInt64; Op: TInterlockedOp64u): UInt64;
begin
Result := InterlockedExchangeOp(UInt64(A^),B,Op);
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedExchangeOpPtr(A: Pointer; B: Pointer; Op: TInterlockedOpPtr): Pointer;
begin
Result := InterlockedExchangeOp(Pointer(A^),B,Op);
end;

//------------------------------------------------------------------------------

Function InterlockedExchangeOpBool(A: Pointer; B: Boolean; Op: TInterlockedOpBool): Boolean;
begin
Result := InterlockedExchangeOp(Boolean(A^),B,Op);
end;

//==============================================================================

Function InterlockedExchangeOp(var A: UInt8; B: UInt8; Op: TInterlockedOp8u): UInt8;
var
  NewValue: UInt8;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
until InterlockedCompareExchange8(@A,NewValue,Result) = Result;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOp(var A: Int8; B: Int8; Op: TInterlockedOp8s): Int8;
var
  NewValue: Int8;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
until InterlockedCompareExchange8(@A,UInt8(NewValue),UInt8(Result)) = UInt8(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOp(var A: UInt16; B: UInt16; Op: TInterlockedOp16u): UInt16;
var
  NewValue: UInt16;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
until InterlockedCompareExchange16(@A,NewValue,Result) = Result;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOp(var A: Int16; B: Int16; Op: TInterlockedOp16s): Int16;
var
  NewValue: Int16;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
until InterlockedCompareExchange16(@A,UInt16(NewValue),UInt16(Result)) = UInt16(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOp(var A: UInt32; B: UInt32; Op: TInterlockedOp32u): UInt32;
var
  NewValue: UInt32;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
until InterlockedCompareExchange32(@A,NewValue,Result) = Result;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOp(var A: Int32; B: Int32; Op: TInterlockedOp32s): Int32;
var
  NewValue: Int32;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
until InterlockedCompareExchange32(@A,UInt32(NewValue),UInt32(Result)) = UInt32(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedExchangeOp(var A: UInt64; B: UInt64; Op: TInterlockedOp64u): UInt64;
var
  NewValue: UInt64;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
until InterlockedCompareExchange64(@A,NewValue,Result) = Result;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOp(var A: Int64; B: Int64; Op: TInterlockedOp64s): Int64;
var
  NewValue: Int64;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
until InterlockedCompareExchange64(@A,UInt64(NewValue),UInt64(Result)) = UInt64(Result);
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOp(var A: Pointer; B: Pointer; Op: TInterlockedOpPtr): Pointer;
var
  NewValue: Pointer;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
until Pointer(InterlockedCompareExchange64(@A,UInt64(NewValue),UInt64(Result))) = Result;
{$ELSE}
until Pointer(InterlockedCompareExchange32(@A,UInt32(NewValue),UInt32(Result))) = Result;
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedExchangeOp(var A: Boolean; B: Boolean; Op: TInterlockedOpBool): Boolean;
var
  NewValue: Boolean;
begin
repeat
  Result := A;
  NewValue := Op(Result,B);
until InterlockedCompareExchange8(@A,UInt8(NewValue),UInt8(Result)) = UInt8(Result);
end;


{===============================================================================
--------------------------------------------------------------------------------
                        Interlocked compare and exchange
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedCompareExchange8(Destination: Pointer; Exchange,Comparand: UInt8; out Exchanged: Boolean): UInt8;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   AL, R8B

    LOCK  CMPXCHG byte ptr [RCX], DL

          SETZ  byte ptr [R9]

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   AL, DL

    LOCK  CMPXCHG byte ptr [RDI], SIL

          SETZ  byte ptr [RCX]

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XCHG  EAX, ECX

    LOCK  CMPXCHG byte ptr [ECX], DL

          MOV   EDX, Exchanged
          SETZ  byte ptr [EDX]

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedCompareExchange16(Destination: Pointer; Exchange,Comparand: UInt16; out Exchanged: Boolean): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   AX, R8W

    LOCK  CMPXCHG word ptr [RCX], DX

          SETZ  byte ptr [R9]

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   AX, DX

    LOCK  CMPXCHG word ptr [RDI], SI

          SETZ  byte ptr [RCX]

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XCHG  EAX, ECX

    LOCK  CMPXCHG word ptr [ECX], DX

          MOV   EDX, Exchanged
          SETZ  byte ptr [EDX]

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedCompareExchange32(Destination: Pointer; Exchange,Comparand: UInt32; out Exchanged: Boolean): UInt32;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   EAX, R8D

    LOCK  CMPXCHG dword ptr [RCX], EDX

          SETZ  byte ptr [R9]

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   EAX, EDX

    LOCK  CMPXCHG dword ptr [RDI], ESI

          SETZ  byte ptr [RCX]

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XCHG  EAX, ECX

    LOCK  CMPXCHG dword ptr [ECX], EDX

          MOV   EDX, Exchanged
          SETZ  byte ptr [EDX]

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedCompareExchange64(Destination: Pointer; Exchange,Comparand: UInt64; out Exchanged: Boolean): UInt64;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   RAX, R8

    LOCK  CMPXCHG qword ptr [RCX], RDX

          SETZ  byte ptr [R9]

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   RAX, RDX

    LOCK  CMPXCHG qword ptr [RDI], RSI

          SETZ  byte ptr [RCX]

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI
          PUSH  EDX

          MOV   EDI, EAX

          MOV   EAX, dword ptr [Comparand]
          MOV   EDX, dword ptr [Comparand + 4]

          MOV   EBX, dword ptr [Exchange]
          MOV   ECX, dword ptr [Exchange + 4]

    LOCK  CMPXCHG8B qword ptr [EDI]

          POP   ECX
          SETZ  byte ptr [ECX]

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF IncludeVal128}

Function InterlockedCompareExchange128(Destination: Pointer; Exchange,Comparand: UInt128; out Exchanged: Boolean): UInt128;
asm
{$IFDEF Windows}
{
  Parameters on enter:

    RCX - pointer to a memory allocated for result
    RDX - pointer to Destination parameter
    R8  - pointer to Exchange parameter
    R9  - pointer to Comparand parameter

    Pointer to Exchanged is passed on stack.

  Result is copied into location passed in RCX and this address is also copied
  into RAX.
}
          PUSH  RBX

          MOV   R10, RCX
          MOV   R11, RDX

          MOV   RBX, qword ptr [R8]
          MOV   RCX, qword ptr [R8 + 8]

          MOV   RAX, qword ptr [R9]
          MOV   RDX, qword ptr [R9 + 8]

    LOCK  CMPXCHG16B dqword ptr [R11]

          MOV   RBX, qword ptr [Exchanged]
          SETZ  byte ptr [RBX]

          MOV   qword ptr [R10], RAX
          MOV   qword ptr [R10 + 8], RDX
          MOV   RAX, R10

          POP   RBX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
{
  Parameters on enter:

    RDI - pointer to Destination parameter
    RSI - lower 8 bytes of Exchange parameter
    RDX - higher 8 bytes of Exchange parameter
    RCX - lower 8 bytes of Comparand parameter
    R8  - higher 8 bytes of Comparand parameter
    R9  - pointer to Exchanged parameter

  Lower 8 bytes of result are returned in RAX, higher 8 bytes in RDX.
}
          PUSH  RBX

          MOV   RBX, RSI  {Exchange.Low}
          XCHG  RCX, RDX  {Exchange.High}

          MOV   RAX, RDX  {Comparand.Low}
          MOV   RDX, R8   {Comparand.High}

    LOCK  CMPXCHG16B dqword ptr [RDI]

          SETZ  byte ptr [R9]

          POP   RBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedCompareExchangePtr(Destination: Pointer; Exchange,Comparand: Pointer; out Exchanged: Boolean): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedCompareExchange64(Destination,UInt64(Exchange),UInt64(Comparand),Exchanged));
{$ELSE}
Result := Pointer(InterlockedCompareExchange32(Destination,UInt32(Exchange),UInt32(Comparand),Exchanged));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedCompareExchangeBool(Destination: Pointer; Exchange,Comparand: Boolean; out Exchanged: Boolean): Boolean;
begin
Result := Boolean(InterlockedCompareExchange8(Destination,UInt8(Exchange),UInt8(Comparand),Exchanged));
end;

//==============================================================================

Function InterlockedCompareExchange(var Destination: UInt8; Exchange,Comparand: UInt8; out Exchanged: Boolean): UInt8;
begin
Result := InterlockedCompareExchange8(@Destination,Exchange,Comparand,Exchanged);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Int8; Exchange,Comparand: Int8; out Exchanged: Boolean): Int8;
begin
Result := Int8(InterlockedCompareExchange8(@Destination,UInt8(Exchange),UInt8(Comparand),Exchanged));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: UInt16; Exchange,Comparand: UInt16; out Exchanged: Boolean): UInt16;
begin
Result := InterlockedCompareExchange16(@Destination,Exchange,Comparand,Exchanged);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Int16; Exchange,Comparand: Int16; out Exchanged: Boolean): Int16;
begin
Result := Int16(InterlockedCompareExchange16(@Destination,UInt16(Exchange),UInt16(Comparand),Exchanged));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: UInt32; Exchange,Comparand: UInt32; out Exchanged: Boolean): UInt32;
begin
Result := InterlockedCompareExchange32(@Destination,Exchange,Comparand,Exchanged);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Int32; Exchange,Comparand: Int32; out Exchanged: Boolean): Int32;
begin
Result := Int32(InterlockedCompareExchange32(@Destination,UInt32(Exchange),UInt32(Comparand),Exchanged));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedCompareExchange(var Destination: UInt64; Exchange,Comparand: UInt64; out Exchanged: Boolean): UInt64;
begin
Result := InterlockedCompareExchange64(@Destination,Exchange,Comparand,Exchanged);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Int64; Exchange,Comparand: Int64; out Exchanged: Boolean): Int64;
begin
Result := Int64(InterlockedCompareExchange64(@Destination,UInt64(Exchange),UInt64(Comparand),Exchanged));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal128}

Function InterlockedCompareExchange(var Destination: UInt128; Exchange,Comparand: UInt128; out Exchanged: Boolean): UInt128;
begin
Result := InterlockedCompareExchange128(@Destination,Exchange,Comparand,Exchanged);
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Pointer; Exchange,Comparand: Pointer; out Exchanged: Boolean): Pointer;
begin
Result := InterlockedCompareExchangePtr(@Destination,Exchange,Comparand,Exchanged);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Boolean; Exchange,Comparand: Boolean; out Exchanged: Boolean): Boolean;
begin
Result := InterlockedCompareExchangeBool(@Destination,Exchange,Comparand,Exchanged);
end;

//==============================================================================

Function InterlockedCompareExchange8(Destination: Pointer; Exchange,Comparand: UInt8): UInt8;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   AL, R8B

    LOCK  CMPXCHG byte ptr [RCX], DL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   AL, DL

    LOCK  CMPXCHG byte ptr [RDI], SIL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XCHG  EAX, ECX

    LOCK  CMPXCHG byte ptr [ECX], DL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedCompareExchange16(Destination: Pointer; Exchange,Comparand: UInt16): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   AX, R8W

    LOCK  CMPXCHG word ptr [RCX], DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   AX, DX

    LOCK  CMPXCHG word ptr [RDI], SI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XCHG  EAX, ECX

    LOCK  CMPXCHG word ptr [ECX], DX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedCompareExchange32(Destination: Pointer; Exchange,Comparand: UInt32): UInt32;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   EAX, R8D

    LOCK  CMPXCHG dword ptr [RCX], EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   EAX, EDX

    LOCK  CMPXCHG dword ptr [RDI], ESI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XCHG  EAX, ECX

    LOCK  CMPXCHG dword ptr [ECX], EDX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedCompareExchange64(Destination: Pointer; Exchange,Comparand: UInt64): UInt64;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          MOV   RAX, R8

    LOCK  CMPXCHG qword ptr [RCX], EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          MOV   RAX, RDX

    LOCK  CMPXCHG qword ptr [RDI], RSI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

          MOV   EAX, dword ptr [Comparand]
          MOV   EDX, dword ptr [Comparand + 4]

          MOV   EBX, dword ptr [Exchange]
          MOV   ECX, dword ptr [Exchange + 4]

    LOCK  CMPXCHG8B qword ptr [EDI]

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF IncludeVal128}

Function InterlockedCompareExchange128(Destination: Pointer; Exchange,Comparand: UInt128): UInt128;
asm
{$IFDEF Windows}

          PUSH  RBX

          MOV   R10, RCX
          MOV   R11, RDX

          MOV   RBX, qword ptr [R8]
          MOV   RCX, qword ptr [R8 + 8]

          MOV   RAX, qword ptr [R9]
          MOV   RDX, qword ptr [R9 + 8]

    LOCK  CMPXCHG16B dqword ptr [R11]

          MOV   qword ptr [R10], RAX
          MOV   qword ptr [R10 + 8], RDX
          MOV   RAX, R10

          POP   RBX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  RBX

          MOV   RBX, RSI  {Exchange.Low}
          XCHG  RCX, RDX  {Exchange.High}

          MOV   RAX, RDX  {Comparand.Low}
          MOV   RDX, R8   {Comparand.High}

    LOCK  CMPXCHG16B dqword ptr [RDI]

          POP   RBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedCompareExchangePtr(Destination: Pointer; Exchange,Comparand: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedCompareExchange64(Destination,UInt64(Exchange),UInt64(Comparand)));
{$ELSE}
Result := Pointer(InterlockedCompareExchange32(Destination,UInt32(Exchange),UInt32(Comparand)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedCompareExchangeBool(Destination: Pointer; Exchange,Comparand: Boolean): Boolean;
begin
Result := Boolean(InterlockedCompareExchange8(Destination,UInt8(Exchange),UInt8(Comparand)));
end;

//==============================================================================

Function InterlockedCompareExchange(var Destination: UInt8; Exchange,Comparand: UInt8): UInt8;
begin
Result := InterlockedCompareExchange8(@Destination,Exchange,Comparand);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Int8; Exchange,Comparand: Int8): Int8;
begin
Result := Int8(InterlockedCompareExchange8(@Destination,UInt8(Exchange),UInt8(Comparand)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: UInt16; Exchange,Comparand: UInt16): UInt16;
begin
Result := InterlockedCompareExchange16(@Destination,Exchange,Comparand);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Int16; Exchange,Comparand: Int16): Int16;
begin
Result := Int16(InterlockedCompareExchange16(@Destination,UInt16(Exchange),UInt16(Comparand)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: UInt32; Exchange,Comparand: UInt32): UInt32;
begin
Result := InterlockedCompareExchange32(@Destination,Exchange,Comparand);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Int32; Exchange,Comparand: Int32): Int32;
begin
Result := Int32(InterlockedCompareExchange32(@Destination,UInt32(Exchange),UInt32(Comparand)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedCompareExchange(var Destination: UInt64; Exchange,Comparand: UInt64): UInt64;
begin
Result := InterlockedCompareExchange64(@Destination,Exchange,Comparand);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Int64; Exchange,Comparand: Int64): Int64;
begin
Result := Int64(InterlockedCompareExchange64(@Destination,UInt64(Exchange),UInt64(Comparand)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal128}

Function InterlockedCompareExchange(var Destination: UInt128; Exchange,Comparand: UInt128): UInt128;
begin
Result := InterlockedCompareExchange128(@Destination,Exchange,Comparand);
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Pointer; Exchange,Comparand: Pointer): Pointer;
begin
Result := InterlockedCompareExchangePtr(@Destination,Exchange,Comparand);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedCompareExchange(var Destination: Boolean; Exchange,Comparand: Boolean): Boolean;
begin
Result := InterlockedCompareExchangeBool(@Destination,Exchange,Comparand);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Interlocked bit test
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedBitTest8(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          XOR   AL, AL
    LOCK  XADD  byte ptr [RCX], AL

          AND   DX, 7
          BT    AX, DX

          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XOR   AL, AL
    LOCK  XADD  byte ptr [RDI], AL

          AND   SI, 7
          BT    AX, SI

          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XOR   CL, CL
    LOCK  XADD  byte ptr [EAX], CL

          AND   DX, 7
          BT    CX, DX

          SETC  AL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedBitTest16(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          XOR   AX, AX
    LOCK  XADD  word ptr [RCX], AX

          BT    AX, DX

          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XOR   AX, AX
    LOCK  XADD  word ptr [RDI], AX

          BT    AX, SI

          SETC  AL
          
  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XOR   CX, CX
    LOCK  XADD  word ptr [EAX], CX

          BT    CX, DX

          SETC  AL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedBitTest32(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          XOR   EAX, EAX
    LOCK  XADD  dword ptr [RCX], EAX

          BT    EAX, EDX

          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XOR   EAX, EAX
    LOCK  XADD  dword ptr [RDI], EAX

          BT    EAX, ESI

          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XOR   ECX, ECX
    LOCK  XADD  dword ptr [EAX], ECX

          BT    ECX, DX

          SETC  AL

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedBitTest64(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          XOR   RAX, RAX
    LOCK  XADD  qword ptr [RCX], RAX

          BT    RAX, RDX

          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          XOR   RAX, RAX
    LOCK  XADD  qword ptr [RDI], RAX

          BT    RAX, RSI

          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI
          PUSH  EDX   // push Bit

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   ECX   // pop Bit
          CMP   ECX, 31
          CMOVA EAX, EDX

          AND   ECX, 31
          BT    EAX, ECX

          SETC  AL

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestPtr(I: Pointer; Bit: Integer): Boolean;
begin
{$IFDEF Ptr64}
Result := InterlockedBitTest64(I,Bit);
{$ELSE}
Result := InterlockedBitTest32(I,Bit);
{$ENDIF}
end;

//==============================================================================

Function InterlockedBitTest(var I: UInt8; Bit: Integer): Boolean;
begin
Result := InterlockedBitTest8(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTest(var I: Int8; Bit: Integer): Boolean;
begin
Result := InterlockedBitTest8(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTest(var I: UInt16; Bit: Integer): Boolean;
begin
Result := InterlockedBitTest16(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTest(var I: Int16; Bit: Integer): Boolean;
begin
Result := InterlockedBitTest16(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTest(var I: UInt32; Bit: Integer): Boolean;
begin
Result := InterlockedBitTest32(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTest(var I: Int32; Bit: Integer): Boolean;
begin
Result := InterlockedBitTest32(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedBitTest(var I: UInt64; Bit: Integer): Boolean;
begin
Result := InterlockedBitTest64(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTest(var I: Int64; Bit: Integer): Boolean;
begin
Result := InterlockedBitTest64(@I,Bit);
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTest(var I: Pointer; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestPtr(@I,Bit);
end;

{===============================================================================
--------------------------------------------------------------------------------
                         Bit string interlocked bit test                         
--------------------------------------------------------------------------------
===============================================================================}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function InterlockedBitTestStr(Str: Pointer; BitOffset: PtrInt): Boolean;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := InterlockedBitTest8(Pointer(PtrUInt(Str) + PtrUInt(SAR3(BitOffset))),Integer(BitOffset and 7));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                          Interlocked bit test and set
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedBitTestAndSet8(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   DX, 7

    @TryOutStart:

          MOV   AL, byte ptr [RCX]

          MOV   R8B, AL
          BTS   R8W, DX

    LOCK  CMPXCHG byte ptr [RCX], R8B

          JNZ   @TryOutStart

          BT    AX, DX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   SI, 7

    @TryOutStart:

          MOV   AL, byte ptr [RDI]

          MOV   R8B, AL
          BTS   R8W, SI

    LOCK  CMPXCHG byte ptr [RDI], R8B

          JNZ   @TryOutStart

          BT    AX, SI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

          AND   DX, 7

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          BTS   BX, DX

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          BT    AX, DX
          SETC  AL

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedBitTestAndSet16(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   DX, 15
    LOCK  BTS   word ptr [RCX], DX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   SI, 15
    LOCK  BTS   word ptr [RDI], SI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   DX, 15
    LOCK  BTS   word ptr [EAX], DX
          SETC  AL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedBitTestAndSet32(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   EDX, 31
    LOCK  BTS   dword ptr [RCX], EDX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   ESI, 31
    LOCK  BTS   dword ptr [RDI], ESI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   EDX, 31
    LOCK  BTS   dword ptr [EAX], EDX
          SETC  AL

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedBitTestAndSet64(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   RDX, 63
    LOCK  BTS   qword ptr [RCX], RDX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   RSI, 63
    LOCK  BTS   qword ptr [RDI], RSI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI
          PUSH  ESI

          MOV   EDI, EAX
          MOV   ESI, EDX
          AND   ESI, 63

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

          CMP   ESI, 31
          JA    @BitTestHigh

          BTS   EBX, ESI
          JMP   @BitTestEnd

    @BitTestHigh:

          BTS   ECX, ESI

    @BitTestEnd:

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          SETC  AL

          POP   ESI
          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestAndSetPtr(I: Pointer; Bit: Integer): Boolean;
begin
{$IFDEF Ptr64}
Result := InterlockedBitTestAndSet64(I,Bit);
{$ELSE}
Result := InterlockedBitTestAndSet32(I,Bit);
{$ENDIF}
end;

//==============================================================================

Function InterlockedBitTestAndSet(var I: UInt8; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndSet8(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndSet(var I: Int8; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndSet8(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndSet(var I: UInt16; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndSet16(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndSet(var I: Int16; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndSet16(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndSet(var I: UInt32; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndSet32(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndSet(var I: Int32; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndSet32(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedBitTestAndSet(var I: UInt64; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndSet64(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndSet(var I: Int64; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndSet64(@I,Bit);
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndSet(var I: Pointer; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndSetPtr(@I,Bit);
end;


{===============================================================================
--------------------------------------------------------------------------------
                     Bit string interlocked bit test and set
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedBitTestAndSetStr16(Str: Pointer; BitOffset: Int16): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  BTS   word ptr [RCX], DX
  {$ELSE}
    LOCK  BTS   word ptr [RDI], SI
  {$ENDIF}
{$ELSE}
    LOCK  BTS   word ptr [EAX], DX
{$ENDIF}
          SETC  AL
end;

//------------------------------------------------------------------------------

Function InterlockedBitTestAndSetStr32(Str: Pointer; BitOffset: Int32): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  BTS   dword ptr [RCX], EDX
  {$ELSE}
    LOCK  BTS   dword ptr [RDI], ESI
  {$ENDIF}
{$ELSE}
    LOCK  BTS   dword ptr [EAX], EDX
{$ENDIF}
          SETC  AL
end;

//------------------------------------------------------------------------------

{$IFDEF x64}

Function InterlockedBitTestAndSetStr64(Str: Pointer; BitOffset: Int64): Boolean;
asm
  {$IFDEF Windows}
    LOCK  BTS   qword ptr [RCX], RDX
  {$ELSE}
    LOCK  BTS   qword ptr [RDI], RSI
  {$ENDIF}
          SETC  AL
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestAndSetStr(Str: Pointer; BitOffset: PtrInt): Boolean;
begin
{$IFDEF x64}
Result := InterlockedBitTestAndSetStr64(Str,Int64(BitOffset));
{$ELSE}
Result := InterlockedBitTestAndSetStr32(Str,Int32(BitOffset));
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function InterlockedBitTestAndSetStrSec(Str: Pointer; BitOffset: PtrInt): Boolean;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := InterlockedBitTestAndSet8(Pointer(PtrUInt(Str) + PtrUInt(SAR3(BitOffset))),Integer(BitOffset and 7));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                         Interlocked bit test and reset
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedBitTestAndReset8(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   DX, 7

    @TryOutStart:

          MOV   AL, byte ptr [RCX]

          MOV   R8B, AL
          BTR   R8W, DX

    LOCK  CMPXCHG byte ptr [RCX], R8B

          JNZ   @TryOutStart

          BT    AX, DX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   SI, 7

    @TryOutStart:

          MOV   AL, byte ptr [RDI]

          MOV   R8B, AL
          BTR   R8W, SI

    LOCK  CMPXCHG byte ptr [RDI], R8B

          JNZ   @TryOutStart

          BT    AX, SI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

          AND   DX, 7

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          BTR   BX, DX

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          BT    AX, DX
          SETC  AL

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedBitTestAndReset16(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   DX, 15
    LOCK  BTR   word ptr [RCX], DX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   SI, 15
    LOCK  BTR   word ptr [RDI], SI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   DX, 15
    LOCK  BTR   word ptr [EAX], DX
          SETC  AL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedBitTestAndReset32(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   EDX, 31
    LOCK  BTR   dword ptr [RCX], EDX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   ESI, 31
    LOCK  BTR   dword ptr [RDI], ESI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   EDX, 31
    LOCK  BTR   dword ptr [EAX], EDX
          SETC  AL
          
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedBitTestAndReset64(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   RDX, 63
    LOCK  BTR   qword ptr [RCX], RDX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   RSI, 63
    LOCK  BTR   qword ptr [RDI], RSI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI
          PUSH  ESI

          MOV   EDI, EAX
          MOV   ESI, EDX
          AND   ESI, 63

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

          CMP   ESI, 31
          JA    @BitTestHigh

          BTR   EBX, ESI
          JMP   @BitTestEnd

    @BitTestHigh:

          BTR   ECX, ESI

    @BitTestEnd:

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          SETC  AL

          POP   ESI
          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestAndResetPtr(I: Pointer; Bit: Integer): Boolean;
begin
{$IFDEF Ptr64}
Result := InterlockedBitTestAndReset64(I,Bit);
{$ELSE}
Result := InterlockedBitTestAndReset32(I,Bit);
{$ENDIF}
end;

//==============================================================================

Function InterlockedBitTestAndReset(var I: UInt8; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndReset8(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndReset(var I: Int8; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndReset8(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndReset(var I: UInt16; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndReset16(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndReset(var I: Int16; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndReset16(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndReset(var I: UInt32; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndReset32(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndReset(var I: Int32; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndReset32(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedBitTestAndReset(var I: UInt64; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndReset64(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndReset(var I: Int64; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndReset64(@I,Bit);
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndReset(var I: Pointer; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndResetPtr(@I,Bit);
end;


{===============================================================================
--------------------------------------------------------------------------------
                    Bit string interlocked bit test and reset                    
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedBitTestAndResetStr16(Str: Pointer; BitOffset: Int16): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  BTR   word ptr [RCX], DX
  {$ELSE}
    LOCK  BTR   word ptr [RDI], SI
  {$ENDIF}
{$ELSE}
    LOCK  BTR   word ptr [EAX], DX
{$ENDIF}
          SETC  AL
end;

//------------------------------------------------------------------------------

Function InterlockedBitTestAndResetStr32(Str: Pointer; BitOffset: Int32): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  BTR   dword ptr [RCX], EDX
  {$ELSE}
    LOCK  BTR   dword ptr [RDI], ESI
  {$ENDIF}
{$ELSE}
    LOCK  BTR   dword ptr [EAX], EDX
{$ENDIF}
          SETC  AL
end;

//------------------------------------------------------------------------------

{$IFDEF x64}

Function InterlockedBitTestAndResetStr64(Str: Pointer; BitOffset: Int64): Boolean;
asm
  {$IFDEF Windows}
    LOCK  BTR   qword ptr [RCX], RDX
  {$ELSE}
    LOCK  BTR   qword ptr [RDI], RSI
  {$ENDIF}
          SETC  AL
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestAndResetStr(Str: Pointer; BitOffset: PtrInt): Boolean;
begin
{$IFDEF x64}
Result := InterlockedBitTestAndResetStr64(Str,Int64(BitOffset));
{$ELSE}
Result := InterlockedBitTestAndResetStr32(Str,Int32(BitOffset));
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function InterlockedBitTestAndResetStrSec(Str: Pointer; BitOffset: PtrInt): Boolean;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := InterlockedBitTestAndReset8(Pointer(PtrUInt(Str) + PtrUInt(SAR3(BitOffset))),Integer(BitOffset and 7));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                      Interlocked bit test and complement
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedBitTestAndComplement8(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   DX, 7

    @TryOutStart:

          MOV   AL, byte ptr [RCX]

          MOV   R8B, AL
          BTC   R8W, DX

    LOCK  CMPXCHG byte ptr [RCX], R8B

          JNZ   @TryOutStart

          BT    AX, DX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   SI, 7

    @TryOutStart:

          MOV   AL, byte ptr [RDI]

          MOV   R8B, AL
          BTC   R8W, SI

    LOCK  CMPXCHG byte ptr [RDI], R8B

          JNZ   @TryOutStart

          BT    AX, SI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX

          MOV   ECX, EAX

          AND   DX, 7

    @TryOutStart:

          MOV   AL, byte ptr [ECX]

          MOV   BL, AL
          BTC   BX, DX

    LOCK  CMPXCHG byte ptr [ECX], BL

          JNZ   @TryOutStart

          BT    AX, DX
          SETC  AL

          POP   EBX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedBitTestAndComplement16(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   DX, 15
    LOCK  BTC   word ptr [RCX], DX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   SI, 15
    LOCK  BTC   word ptr [RDI], SI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   DX, 15
    LOCK  BTC   word ptr [EAX], DX
          SETC  AL

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedBitTestAndComplement32(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   EDX, 31
    LOCK  BTC   dword ptr [RCX], EDX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   ESI, 31
    LOCK  BTC   dword ptr [RDI], ESI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   EDX, 31
    LOCK  BTC   dword ptr [EAX], EDX
          SETC  AL

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedBitTestAndComplement64(I: Pointer; Bit: Integer): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}

          AND   RDX, 63
    LOCK  BTC   qword ptr [RCX], RDX
          SETC  AL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          AND   RSI, 63
    LOCK  BTC   qword ptr [RDI], RSI
          SETC  AL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI
          PUSH  ESI

          MOV   EDI, EAX
          MOV   ESI, EDX
          AND   ESI, 63

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

          CMP   ESI, 31
          JA    @BitTestHigh

          BTC   EBX, ESI
          JMP   @BitTestEnd

    @BitTestHigh:

          BTC   ECX, ESI

    @BitTestEnd:

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          SETC  AL

          POP   ESI
          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestAndComplementPtr(I: Pointer; Bit: Integer): Boolean;
begin
{$IFDEF Ptr64}
Result := InterlockedBitTestAndComplement64(I,Bit);
{$ELSE}
Result := InterlockedBitTestAndComplement32(I,Bit);
{$ENDIF}
end;

//==============================================================================

Function InterlockedBitTestAndComplement(var I: UInt8; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndComplement8(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndComplement(var I: Int8; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndComplement8(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndComplement(var I: UInt16; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndComplement16(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndComplement(var I: Int16; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndComplement16(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndComplement(var I: UInt32; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndComplement32(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndComplement(var I: Int32; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndComplement32(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedBitTestAndComplement(var I: UInt64; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndComplement64(@I,Bit);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndComplement(var I: Int64; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndComplement64(@I,Bit);
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedBitTestAndComplement(var I: Pointer; Bit: Integer): Boolean;
begin
Result := InterlockedBitTestAndComplementPtr(@I,Bit);
end;


{===============================================================================
--------------------------------------------------------------------------------
                 Bit string interlocked bit test and complement
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedBitTestAndComplementStr16(Str: Pointer; BitOffset: Int16): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  BTC   word ptr [RCX], DX
  {$ELSE}
    LOCK  BTC   word ptr [RDI], SI
  {$ENDIF}
{$ELSE}
    LOCK  BTC   word ptr [EAX], DX
{$ENDIF}
          SETC  AL
end;

//------------------------------------------------------------------------------

Function InterlockedBitTestAndComplementStr32(Str: Pointer; BitOffset: Int32): Boolean;
asm
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  BTC   dword ptr [RCX], EDX
  {$ELSE}
    LOCK  BTC   dword ptr [RDI], ESI
  {$ENDIF}
{$ELSE}
    LOCK  BTC   dword ptr [EAX], EDX
{$ENDIF}
          SETC  AL
end;

//------------------------------------------------------------------------------

{$IFDEF x64}

Function InterlockedBitTestAndComplementStr64(Str: Pointer; BitOffset: Int64): Boolean;
asm
  {$IFDEF Windows}
    LOCK  BTC   qword ptr [RCX], RDX
  {$ELSE}
    LOCK  BTC   qword ptr [RDI], RSI
  {$ENDIF}
          SETC  AL
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedBitTestAndComplementStr(Str: Pointer; BitOffset: PtrInt): Boolean;
begin
{$IFDEF x64}
Result := InterlockedBitTestAndComplementStr64(Str,Int64(BitOffset));
{$ELSE}
Result := InterlockedBitTestAndComplementStr32(Str,Int32(BitOffset));
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function InterlockedBitTestAndComplementStrSec(Str: Pointer; BitOffset: PtrInt): Boolean;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := InterlockedBitTestAndComplement8(Pointer(PtrUInt(Str) + PtrUInt(SAR3(BitOffset))),Integer(BitOffset and 7));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                Interlocked load
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedLoad8(I: Pointer): UInt8;
asm
          XOR   DL, DL
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  XADD  byte ptr [RCX], DL
  {$ELSE}
    LOCK  XADD  byte ptr [RDI], DL
  {$ENDIF}
{$ELSE}
    LOCK  XADD  byte ptr [EAX], DL
{$ENDIF}
          MOV   AL, DL
end;

//------------------------------------------------------------------------------

Function InterlockedLoad16(I: Pointer): UInt16;
asm
          XOR   DX, DX
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  XADD  word ptr [RCX], DX
  {$ELSE}
    LOCK  XADD  word ptr [RDI], DX
  {$ENDIF}
{$ELSE}
    LOCK  XADD  word ptr [EAX], DX
{$ENDIF}
          MOV   AX, DX
end;

//------------------------------------------------------------------------------

Function InterlockedLoad32(I: Pointer): UInt32;
asm
          XOR   EDX, EDX
{$IFDEF x64}
  {$IFDEF Windows}
    LOCK  XADD  dword ptr [RCX], EDX
  {$ELSE}
    LOCK  XADD  dword ptr [RDI], EDX
  {$ENDIF}
{$ELSE}
    LOCK  XADD  dword ptr [EAX], EDX
{$ENDIF}
          MOV   EAX, EDX
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedLoad64(I: Pointer): UInt64;
asm
{$IFDEF x64}

          XOR   RDX, RDX
  {$IFDEF Windows}
    LOCK  XADD  qword ptr [RCX], RDX
  {$ELSE}
    LOCK  XADD  qword ptr [RDI], RDX
  {$ENDIF}
          MOV   RAX, RDX

{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]

          MOV   EBX, EAX
          MOV   ECX, EDX

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedLoadPtr(I: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedLoad64(I));
{$ELSE}
Result := Pointer(InterlockedLoad32(I));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedLoadBool(I: Pointer): Boolean;
begin
Result := Boolean(InterlockedLoad8(I));
end;

//==============================================================================

Function InterlockedLoad(var I: UInt8): UInt8;
begin
Result := InterlockedLoad8(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedLoad(var I: Int8): Int8;
begin
Result := Int8(InterlockedLoad8(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedLoad(var I: UInt16): UInt16;
begin
Result := InterlockedLoad16(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedLoad(var I: Int16): Int16;
begin
Result := Int16(InterlockedLoad16(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedLoad(var I: UInt32): UInt32;
begin
Result := InterlockedLoad32(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedLoad(var I: Int32): Int32;
begin
Result := Int32(InterlockedLoad32(@I));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedLoad(var I: UInt64): UInt64;
begin
Result := InterlockedLoad64(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedLoad(var I: Int64): Int64;
begin
Result := Int64(InterlockedLoad64(@I));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedLoad(var I: Pointer): Pointer;
begin
Result := InterlockedLoadPtr(@I);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedLoad(var I: Boolean): Boolean;
begin
Result := InterlockedLoadBool(@I);
end;


{===============================================================================
--------------------------------------------------------------------------------
                               Interlocked store                                                               
--------------------------------------------------------------------------------
===============================================================================}

Function InterlockedStore8(I: Pointer; NewValue: UInt8): UInt8;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XCHG  byte ptr [RCX], DL
          MOV   AL, DL

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  byte ptr [RDI], SIL
          MOV   AL, SIL

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  byte ptr [EAX], DL
          MOV   AL, DL
          
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedStore16(I: Pointer; NewValue: UInt16): UInt16;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XCHG  word ptr [RCX], DX
          MOV   AX, DX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  word ptr [RDI], SI
          MOV   AX, SI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  word ptr [EAX], DX
          MOV   AX, DX

{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedStore32(I: Pointer; NewValue: UInt32): UInt32;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XCHG  dword ptr [RCX], EDX
          MOV   EAX, EDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  dword ptr [RDI], ESI
          MOV   EAX, ESI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  dword ptr [EAX], EDX
          MOV   EAX, EDX

{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF IncludeVal64}

Function InterlockedStore64(I: Pointer; NewValue: UInt64): UInt64;
asm
{$IFDEF x64}
  {$IFDEF Windows}

    LOCK  XCHG  qword ptr [RCX], RDX
          MOV   RAX, RDX

  {$ELSE}//  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

    LOCK  XCHG  qword ptr [RDI], RSI
          MOV   RAX, RSI

  {$ENDIF}
{$ELSE}// -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -

          PUSH  EBX
          PUSH  EDI

          MOV   EDI, EAX

    @TryOutStart:

          MOV   EBX, dword ptr [NewValue]
          MOV   ECX, dword ptr [NewValue + 4]

          MOV   EAX, dword ptr [EDI]
          MOV   EDX, dword ptr [EDI + 4]          

    LOCK  CMPXCHG8B qword ptr [EDI]

          JNZ   @TryOutStart

          POP   EDI
          POP   EBX

{$ENDIF}
end;

{$ENDIF}

//------------------------------------------------------------------------------

Function InterlockedStorePtr(I: Pointer; NewValue: Pointer): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
{$IFDEF Ptr64}
Result := Pointer(InterlockedStore64(I,UInt64(NewValue)));
{$ELSE}
Result := Pointer(InterlockedStore32(I,UInt32(NewValue)));
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function InterlockedStoreBool(I: Pointer; NewValue: Boolean): Boolean;
begin
Result := Boolean(InterlockedStore8(I,UInt8(NewValue)));
end;

//==============================================================================

Function InterlockedStore(var I: UInt8; NewValue: UInt8): UInt8;
begin
Result := InterlockedStore8(@I,NewValue);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedStore(var I: Int8; NewValue: Int8): Int8;
begin
Result := Int8(InterlockedStore8(@I,UInt8(NewValue)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedStore(var I: UInt16; NewValue: UInt16): UInt16;
begin
Result := InterlockedStore16(@I,NewValue);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedStore(var I: Int16; NewValue: Int16): Int16;
begin
Result := Int16(InterlockedStore16(@I,UInt16(NewValue)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedStore(var I: UInt32; NewValue: UInt32): UInt32;
begin
Result := InterlockedStore32(@I,NewValue);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedStore(var I: Int32; NewValue: Int32): Int32;
begin
Result := Int32(InterlockedStore32(@I,UInt32(NewValue)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IFDEF IncludeVal64}

Function InterlockedStore(var I: UInt64; NewValue: UInt64): UInt64;
begin
Result := InterlockedStore64(@I,NewValue);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedStore(var I: Int64; NewValue: Int64): Int64;
begin
Result := Int64(InterlockedStore64(@I,UInt64(NewValue)));
end;

{$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedStore(var I: Pointer; NewValue: Pointer): Pointer;
begin
Result := InterlockedStorePtr(@I,NewValue);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function InterlockedStore(var I: Boolean; NewValue: Boolean): Boolean;
begin
Result := InterlockedStoreBool(@I,NewValue);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                Utility funtions
--------------------------------------------------------------------------------
===============================================================================}

{$IFOPT W+}
  {$DEFINE StackFramesOn}
{$ELSE}
  {$UNDEF StackFramesOn}
{$ENDIF}

{$STACKFRAMES OFF}

procedure ReadBarrier; assembler; {$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IFDEF x64}{$IFNDEF FPC}.NOFRAME{$ENDIF}{$ENDIF}
  LFENCE
end;

//------------------------------------------------------------------------------

procedure WriteBarrier; assembler; {$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IFDEF x64}{$IFNDEF FPC}.NOFRAME{$ENDIF}{$ENDIF}
  SFENCE
end;

//------------------------------------------------------------------------------

procedure ReadWriteBarrier; assembler;{$IFDEF FPC} nostackframe; {$ENDIF}
asm
{$IFDEF x64}{$IFNDEF FPC}.NOFRAME{$ENDIF}{$ENDIF}
  MFENCE
end;

{$IFDEF StackFramesOn}
  {$STACKFRAMES ON}
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              Unit initialization
--------------------------------------------------------------------------------
===============================================================================}

procedure Initialize;
begin
{$IFDEF FPCDWM}{$PUSH}W6018{$ENDIF}
If (Ord(False) <> 0) or (Ord(True) <> 1) or (SizeOf(Boolean) <> 1) then
  raise EILOUnsupportedImplementation.Create('Unsupported implementation of type Boolean.');
{$IFDEF FPCDWM}{$POP}{$ENDIF}
{$IFDEF AssertInstructions}
with TSimpleCPUID.Create do
try
{$IF Defined(IncludeVal64) and not Defined(x64)}
  If not Info.ProcessorFeatures.CX8 then
    raise EILOUnsupportedInstruction.Create('Instruction CMPXCHG8B is not supported by the CPU.');
  If not Info.ProcessorFeatures.CMOV then
    raise EILOUnsupportedInstruction.Create('Instruction CMOVcc is not supported by the CPU.');
{$IFEND}
{$IFDEF IncludeVal128}
  If not Info.ProcessorFeatures.CMPXCHG16B then
    raise EILOUnsupportedInstruction.Create('Instruction CMPXCHG16B is not supported by the CPU.');
{$ENDIF}
  If not Info.ProcessorFeatures.SSE2 then
    raise EILOUnsupportedInstruction.Create('Instructions LFENCE and MFENCE are not supported by the CPU.');
  If not Info.ProcessorFeatures.SSE then
    raise EILOUnsupportedInstruction.Create('Instruction SFENCE is not supported by the CPU.');
finally
  Free;
end;
{$ENDIF}
end;

//------------------------------------------------------------------------------

initialization
  Initialize;

end.

