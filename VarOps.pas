{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Variable Operations

    This unit provides a small set of functions intended to help with selected
    operations performed on variables and function arguments/parameters.

  version 1.0.1 (2026-05-03)

  Last change 2026-05-03

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.VarOps

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol VarOps_UseAuxExceptions for details).

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit VarOps;
{
  VarOps_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  VarOps_UseAuxExceptions to achieve this.
}
{$IF Defined(VarOps_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
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

interface

uses
  SysUtils,
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EVOException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EVOConversionError = class(EVOException);

{===============================================================================
--------------------------------------------------------------------------------
                              Argument(s) consuming
--------------------------------------------------------------------------------
===============================================================================}
{
  ConsumeArgs

  Sometimes there is a need to declare function or method prototype accepting
  arguments/parameters that are not used in actual implementation (eg. methods
  that are meant to be fully implemented only in derived classes, but which
  need small part of code to be already in place, so they cannot be abstract).
  Compiler usually issues a hint or warning about unused parameters when this
  happens - these warnings can be very distracting and unwanted.

    WARNING - compiler is issuing these warnings correctly, so think twice
              before you suppress them and make sure you know what you are
              doing!

  This function is here axactly for such situations - by passing the unused
  arguments to it, you can fool the compiler into thinking that they are being
  used, removing a need for compiler directives to suppress the warnings (which
  should be avoided whenever possible).

    For example, let's say you have following funtion declaration:

        procedure Foo(A,B,C: Integer; const Str: String);

    ...where only the parameter B is used in its implementation. Just add
    following line to the implementation and you are done:

        ConsumeArgs([A,C,Str]);

  The function does not change or even reads the paramaters, but it can incur
  some performance overhead because of array building - be aware of that.
  If performance is important, use single-argument overloads provided further.

  You can pass any imaginable argument - simple types like integers, floats,
  characters, strings, pointers, even variants can be passed directly. Some
  types might not be so clear, but usually they are also directly accepted
  (eg. objects, interfaces or classes).
  More complex or specific types cannot be used directly though, but you can
  always pass a reference (pointer) to them (btw. this is applicable to all
  types) - arrays, records, untyped buffers, even open arrays can be passed
  this way.

  Returns number of arguments passed - you can just ignore the result.
}
Function ConsumeArgs(const Args: array of const): Integer;

//------------------------------------------------------------------------------
{
  ConsumeArg

  Following overloads are accepting single argument of prescribed type.
  They are meant for situations where only one argument is to be consumed or
  where performance overhead caused by array creation needs to be avoided.

  They are declared only for the most common types, but with typecasting or
  by passing a reference (pointer) you can use them for all types.

  Return value is not important and differs from type to type (usually it is
  a product of some token operation performed on the passed argument) - result
  is meant to be ignored anyway, so do not use it in any capacity.
}
Function ConsumeArg(Num: Int32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function ConsumeArg(Num: Int64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function ConsumeArg(Num: UInt32): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF Declared(NativeUInt64E)}
Function ConsumeArg(Num: UInt64): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}

Function ConsumeArg(Num: Extended): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function ConsumeArg(const Str: String): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function ConsumeArg(Ptr: Pointer): Integer; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                          TVarRec to Variant conversion                                                                                                                                                             
--------------------------------------------------------------------------------
===============================================================================}
{
  VarRecToVariant

  Variant open array arguments (ie. those declared as "[const] array of const")
  are, by the compiler, implemented as arrays of TVarRec. TVarRec itself has
  field to store values of type Variant, but if you want to convert the record
  directly to a variant, there is no readily provided function for that (that
  I am aware of) - this function is designed to do it.

  If the record stores a pointer value (vtPointer, vtPChar, vtObject, vtClass,
  vtPWideChar and vtInterface), then the pointer address is converted (casted)
  to signed 64bit integer (Int64) and this number is then used to build the
  variant result (meaning it will be of type varInt64).
}
Function VarRecToVariant(const Value: TVarRec): Variant;

{===============================================================================
--------------------------------------------------------------------------------
                                 Values swapping
--------------------------------------------------------------------------------
===============================================================================}
{
  SwapValues

  All these functions are simply swapping (exchanging) values of tvo given
  variables. Of course, swapping values of two variables is very trivial task,
  but it requires use of temporary storage (unless xor-swap is used), which
  needs to be declared. These functions are here just to simplify the task by
  wrapping it into a single call.

  To minimize memory footprint, overload swapping buffers is doing it without
  use of temporary storage, the buffers are streamed and swapped in small
  parts. That being said, passing even partially overlapping buffers is
  undefined operation and most probably will create bogus data in both buffers.
}
procedure SwapValues(var A,B: Boolean); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure SwapValues(var A,B: Int8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: Int16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: Int32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: Int64); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure SwapValues(var A,B: UInt8); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: UInt16); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: UInt32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: UInt64); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure SwapValues(var A,B: Float32); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: Float64); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: Float80); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure SwapValues(var A,B: Pointer); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: TObject); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: TClass); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: IInterface); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure SwapValues(var A,B: Variant); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure SwapValues(var A,B: AnsiChar); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: WideChar); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: UCS4Char); overload;{$IFDEF CanInline} inline;{$ENDIF}

procedure SwapValues(var A,B: ShortString); overload;{$IF Defined(FPC) and Defined(CanInline)} inline;{$IFEND}
procedure SwapValues(var A,B: AnsiString); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: UTF8String); overload;{$IFDEF CanInline} inline;{$ENDIF}
procedure SwapValues(var A,B: WideString); overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IF not Declared(UnicodeIsWideE)}
procedure SwapValues(var A,B: UnicodeString); overload;{$IFDEF CanInline} inline;{$ENDIF}
{$IFEND}
procedure SwapValues(var A,B: UCS4String); overload;{$IFDEF CanInline} inline;{$ENDIF}

{
  Following overload can be used not only to swap untyped buffers, but also
  to exchange values of matching structured types (records, arrays, ...).
}
procedure SwapValues(var A,B; Size: TMemSize); overload;

implementation

uses
  Variants;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$PUSH}{$WARN 2005 OFF}             // Comment level $1 found
  {$IF Defined(FPC) and (FPC_FULLVERSION >= 30200)}
    {$DEFINE W6058:={$WARN 6058 OFF}} // Call to subroutine "$1" marked as inline is not inlined
  {$ELSE}
    {$DEFINE W6058:=}
  {$IFEND}
  {$POP}
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              Argument(s) consuming
--------------------------------------------------------------------------------
===============================================================================}

Function ConsumeArgs(const Args: array of const): Integer;
begin
Result := Length(Args);
end;

//==============================================================================

Function ConsumeArg(Num: Int32): Integer;
begin
Result := Integer(Num);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ConsumeArg(Num: Int64): Integer;
begin
Result := Integer(Num);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ConsumeArg(Num: UInt32): Integer;
begin
Result := Integer(Num);
end;

{$IF Declared(NativeUInt64E)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ConsumeArg(Num: UInt64): Integer;
begin
Result := Integer(Num);
end;

{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ConsumeArg(Num: Extended): Integer;
begin
Result := Ord(Num <> 0.0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ConsumeArg(const Str: String): Integer;
begin
Result := Length(Str);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ConsumeArg(Ptr: Pointer): Integer;
begin
Result := Ord(Assigned(Ptr));
end;


{===============================================================================
--------------------------------------------------------------------------------
                          TVarRec to Variant conversion                                                                                                                                                             
--------------------------------------------------------------------------------
===============================================================================}
{
  FPC complains about >>>RTL<<< functions not being inlined... ;-/

  This time its overloaded assignment operators used for implicit conversions
  to and from variants, which is completely demented because there is no way
  of doing it explicitly and avoid this mess.
}
{$IFDEF FPCDWM}{$PUSH}W6058{$ENDIF}
Function VarRecToVariant(const Value: TVarRec): Variant;

  Function PtrToInt(const Ptr: Pointer): PtrInt;
  var
    Overlay:  PtrInt absolute Ptr;
  begin
    Result := Overlay;
  end;

begin
case Value.VType of
  vtInteger:        Result := Value.VInteger;
  vtBoolean:        Result := Value.VBoolean;
  vtChar:           Result := Value.VChar;
  vtExtended:       Result := Value.VExtended^;
{$IF Declared(vtString)}
  vtString:         Result := ShortString(Value.VString^);
{$IFEND}
  vtPointer:        Result := Int64(PtrToInt(Value.VPointer));
  vtPChar:          Result := Int64(PtrToInt(Value.VPChar));
  vtObject:         Result := Int64(PtrToInt(Pointer(Value.VObject)));
  vtClass:          Result := Int64(PtrToInt(Pointer(Value.VClass)));
  vtWideChar:       Result := WideString(Value.VWideChar);
  vtPWideChar:      Result := Int64(PtrToInt(Value.VPWideChar));
  vtAnsiString:     Result := AnsiString(Value.VAnsiString);
  vtCurrency:       Result := Value.VCurrency^;
  vtVariant:        Result := Value.VVariant^;
  vtInterface:      Result := Int64(PtrToInt(Value.VInterface));
  vtWideString:     Result := WideString(Value.VWideString);
  vtInt64:          Result := Value.VInt64^;
{$IF Declared(vtQWord)}
  vtQWord:          Result := Value.VQWord^;
{$IFEND}
{$IF Declared(vtUnicodeString)}
  vtUnicodeString:  Result := UnicodeString(Value.VUnicodeString);
{$IFEND}
else
  raise EVOConversionError.CreateFmt('VarRecToVariant: Cannot convert value of type %d.',[Value.VType]);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                 Values swapping
--------------------------------------------------------------------------------
===============================================================================}
{
  NOTE - I know about XOR-swapping (or whatever following algorithm is called:
         B := B xor A -> A := A xor B -> B := B xor A). I have not tested it,
         but I seriously doubt it would bring better performance.
}

procedure SwapValues(var A,B: Boolean);
var
  Temp: Boolean;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: Int8);
var
  Temp: Int8;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: Int16); 
var
  Temp: Int16;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: Int32);
var
  Temp: Int32;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: Int64);
var
  Temp: Int64;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: UInt8);
var
  Temp: UInt8;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: UInt16);
var
  Temp: UInt16;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: UInt32);
var
  Temp: UInt32;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: UInt64);
var
  Temp: UInt64;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: Float32);
var
  Temp: Float32;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: Float64);
var
  Temp: Float64;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: Float80);
var
  Temp: Float80;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: Pointer);
var
  Temp: Pointer;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: TObject);
var
  Temp: TObject;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: TClass);
var
  Temp: TClass;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: IInterface);
var
  Temp: IInterface;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: Variant);
var
  Temp: Variant;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: AnsiChar);
var
  Temp: AnsiChar;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: WideChar);
var
  Temp: WideChar;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: UCS4Char);
var
  Temp: UCS4Char;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: ShortString);
var
  Temp: ShortString;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: AnsiString);
var
  Temp: AnsiString;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: UTF8String);
var
  Temp: UTF8String;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: WideString);
var
  Temp: WideString;
begin
Temp := A;
A := B;
B := Temp
end;

{$IF not Declared(UnicodeIsWideE)}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: UnicodeString);
var
  Temp: UnicodeString;
begin
Temp := A;
A := B;
B := Temp
end;

{$IFEND}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B: UCS4String);
var
  Temp: UCS4String;
begin
Temp := A;
A := B;
B := Temp
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure SwapValues(var A,B; Size: TMemSize);
var
  APtr: PByte;
  BPtr: PByte;
  Temp: NativeUInt;
begin
// tried implementing this in asm, there was no measurable performance gain
APtr := @A;
BPtr := @B;
while Size > SizeOf(NativeUInt) do
  begin
    Temp := PNativeUInt(APtr)^;
    PNativeUInt(APtr)^ := PNativeUInt(BPtr)^;
    PNativeUInt(BPtr)^ := Temp;
    Inc(APtr,SizeOf(NativeUInt));
    Inc(BPtr,SizeOf(NativeUInt));
    Dec(Size,SizeOf(NativeUInt));
  end;
while Size > 0 do
  begin
    Temp := NativeUInt(APtr^);
    APtr^ := BPtr^;
    BPtr^ := Byte(Temp);
    Inc(APtr);
    Inc(BPtr);
    Dec(Size);
  end;
end;

end.
