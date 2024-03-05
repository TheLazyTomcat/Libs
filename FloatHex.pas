{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  FloatHex

    Small set of functions for conversions of floating point numbers to and
    from hexadecimal strings and functions for direct casting between integers
    and floats.

    Note that when converting to and from type Extended, the strings will be
    20 characters long - as if the type is 80bits (10bytes) in size,
    irrespective of how it is declared (in 64bit windows programs, it is
    usually declared as a simple alias for type double (64bits)).

  Version 2.1 (2024-02-14)

  Last change 2024-03-05

  ©2015-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.FloatHex

  Dependencies:
    AuxTypes     - github.com/TheLazyTomcat/Lib.AuxTypes
    Float80Utils - github.com/TheLazyTomcat/Lib.Float80Utils

===============================================================================}
unit FloatHex;

{$IFDEF ENDIAN_BIG}
  {$MESSAGE FATAL 'Big-endian system not supported'}
{$ENDIF}

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
  SysUtils,
  AuxTypes, Float80Utils;

type
  // library-specific exceptions, unused atm.
  EFHException = class(Exception);

{===============================================================================
--------------------------------------------------------------------------------
                        Float <-> HexString conversions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Float <-> HexString conversions - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float16
-------------------------------------------------------------------------------}

Function Float16ToHex(Value: Float16): String;
Function HexToFloat16(const HexString: String): Float16;
Function TryHexToFloat16(const HexString: String; out Value: Float16): Boolean;
Function HexToFloat16Def(const HexString: String; const DefaultValue: Float16): Float16;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Half
-------------------------------------------------------------------------------}

Function HalfToHex(Value: Half): String;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToHalf(const HexString: String): Half;{$IFDEF CanInline} inline;{$ENDIF}
Function TryHexToHalf(const HexString: String; out Value: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToHalfDef(const HexString: String; const DefaultValue: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float32
-------------------------------------------------------------------------------}

Function Float32ToHex(Value: Float32): String;
Function HexToFloat32(const HexString: String): Float32;
Function TryHexToFloat32(const HexString: String; out Value: Float32): Boolean;
Function HexToFloat32Def(const HexString: String; const DefaultValue: Float32): Float32;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Single
-------------------------------------------------------------------------------}

Function SingleToHex(Value: Single): String;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToSingle(const HexString: String): Single;{$IFDEF CanInline} inline;{$ENDIF}
Function TryHexToSingle(const HexString: String; out Value: Single): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToSingleDef(const HexString: String; const DefaultValue: Single): Single;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float64
-------------------------------------------------------------------------------}

Function Float64ToHex(Value: Float64): String;
Function HexToFloat64(const HexString: String): Float64;
Function TryHexToFloat64(const HexString: String; out Value: Float64): Boolean;
Function HexToFloat64Def(const HexString: String; const DefaultValue: Float64): Float64;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Double
-------------------------------------------------------------------------------}

Function DoubleToHex(Value: Double): String;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToDouble(const HexString: String): Double;{$IFDEF CanInline} inline;{$ENDIF}
Function TryHexToDouble(const HexString: String; out Value: Double): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToDoubleDef(const HexString: String; const DefaultValue: Double): Double;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float80
-------------------------------------------------------------------------------}

Function Float80ToHex(Value: Float80): String;
Function HexToFloat80(const HexString: String): Float80;
Function TryHexToFloat80(const HexString: String; out Value: Float80): Boolean;
Function HexToFloat80Def(const HexString: String; const DefaultValue: Float80): Float80;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Extended
-------------------------------------------------------------------------------}

Function ExtendedToHex(Value: Extended): String;
Function HexToExtended(const HexString: String): Extended;
Function TryHexToExtended(const HexString: String; out Value: Extended): Boolean;
Function HexToExtendedDef(const HexString: String; const DefaultValue: Extended): Extended;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - default float type
-------------------------------------------------------------------------------}

Function FloatToHex(Value: Double): String;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToFloat(const HexString: String): Double;{$IFDEF CanInline} inline;{$ENDIF}
Function TryHexToFloat(const HexString: String; out Value: Double): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToFloatDef(const HexString: String; const DefaultValue: Double): Double;{$IFDEF CanInline} inline;{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                            Float <-> Integer casting
--------------------------------------------------------------------------------
===============================================================================}
{
  Following functions are casting the integers to floats and back as if they
  would occupy the same memory location, they are NOT converting them.
  The result will have the same bit pattern as the input parameter, which might
  lead to exceptions if you happen to produce SNaN from an integer - be aware
  of that and do not blindly pass some random integers.

  Note that unsigned integers are used to reduce potential problems with
  negative values (don't ask, accept it >:/).
}
{===============================================================================
    Float <-> Integer casting - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Float16
-------------------------------------------------------------------------------}

Function CastToFloat16(Value: UInt16): Float16;
Function CastFromFloat16(Value: Float16): UInt16;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Half
-------------------------------------------------------------------------------}

Function CastToHalf(Value: UInt16): Half;{$IFDEF CanInline} inline;{$ENDIF}
Function CastFromHalf(Value: Half): UInt16;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Float32
-------------------------------------------------------------------------------}

Function CastToFloat32(Value: UInt32): Float32;
Function CastFromFloat32(Value: Float32): UInt32;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Single
-------------------------------------------------------------------------------}

Function CastToSingle(Value: UInt32): Single;{$IFDEF CanInline} inline;{$ENDIF}
Function CastFromSingle(Value: Single): UInt32;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Float64
-------------------------------------------------------------------------------}

Function CastToFloat64(Value: UInt64): Float64;
Function CastFromFloat64(Value: Float64): UInt64;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Double
-------------------------------------------------------------------------------}

Function CastToDouble(Value: UInt64): Double;{$IFDEF CanInline} inline;{$ENDIF}
Function CastFromDouble(Value: Double): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Float80
-------------------------------------------------------------------------------}

Function CastToFloat80(const Value): Float80; overload;
Function CastToFloat80(ExponentWithSign: UInt16; Mantissa: UInt64): Float80; overload;
procedure CastFromFloat80(Value: Float80; out Result); overload;
procedure CastFromFloat80(Value: Float80; out ExponentWithSign: UInt16; out Mantissa: UInt64); overload;
Function CastFromFloat80(Value: Float80): TFloat80Overlay; overload;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Extended
-------------------------------------------------------------------------------}
{
  WARNING - in following functions, the integer input/output is ALWAYS 80bits
            (10 bytes) wide, irrespective of how is the type Extended declared
            (conversion to/from 80bit to 64bit float is done automatically when
            needed).
            If you want to have integer in/out to be the true size of Extended,
            use functions NativeCastToExtended and NativeCastFromExtended.
}
Function CastToExtended(const Value): Extended; overload;
Function CastToExtended(ExponentWithSign: UInt16; Mantissa: UInt64): Extended; overload;
procedure CastFromExtended(Value: Extended; out Result); overload;
procedure CastFromExtended(Value: Extended; out ExponentWithSign: UInt16; out Mantissa: UInt64); overload;
Function CastFromExtended(Value: Extended): TFloat80Overlay; overload;

{
  In following two functions, the size of input/output buffer equals to actual
  size of type Extended, whatever its declaration may be.
}
Function NativeCastToExtended(const Value): Extended; overload;
procedure NativeCastFromExtended(Value: Extended; out Result); overload;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - default float type
-------------------------------------------------------------------------------}

Function CastToFloat(Value: UInt64): Double;{$IFDEF CanInline} inline;{$ENDIF}
Function CastFromFloat(Value: Double): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

implementation

// do not place higher (FPC does not like it)
{$IF SizeOf(Extended) = 8}
  {$DEFINE Extended64}
{$ELSEIF SizeOf(Extended) = 10}
  {$UNDEF Extended64}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported platform, type extended must be 8 or 10 bytes.'}
{$IFEND}

{===============================================================================
--------------------------------------------------------------------------------
                        Float <-> HexString conversions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Float <-> HexString conversions - auxiliary routines
===============================================================================}

Function RectifyHexString(const Str: String; RequiredLength: Integer): String;

  Function StartsWithHexMark: Boolean;
  begin
    If Length(Str) > 0 then
      Result := Str[1] = '$'
    else
      Result := False;
  end;

begin
If not StartsWithHexMark then
  Result := '$' + Str
else
  Result := Str;
Inc(RequiredLength);
If Length(Result) <> RequiredLength then
  begin
    If Length(Result) < RequiredLength then
      Result := Result + StringOfChar('0',RequiredLength - Length(Result))
    else
      Result := Copy(Result,1,RequiredLength);
  end;
end;

{===============================================================================
    Float <-> HexString conversions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float16
-------------------------------------------------------------------------------}

Function Float16ToHex(Value: Float16): String;
var
  Overlay:  UInt16 absolute Value;
begin
Result := IntToHex(Overlay,4);
end;

//------------------------------------------------------------------------------

Function HexToFloat16(const HexString: String): Float16;
var
  Overlay:  UInt16 absolute Result;
begin
Overlay := UInt16(StrToInt(RectifyHexString(HexString,4)));
end;

//------------------------------------------------------------------------------

Function TryHexToFloat16(const HexString: String; out Value: Float16): Boolean;
begin
try
  Value := HexToFloat16(HexString);
  Result := True;
except
  Result := False;
end;
end;
 
//------------------------------------------------------------------------------

Function HexToFloat16Def(const HexString: String; const DefaultValue: Float16): Float16;
begin
If not TryHexToFloat16(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Half
-------------------------------------------------------------------------------}

Function HalfToHex(Value: Half): String;
begin
Result := Float16ToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToHalf(const HexString: String): Half;
begin
Result := HexToFloat16(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToHalf(const HexString: String; out Value: Half): Boolean;
begin
Result := TryHexToFloat16(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToHalfDef(const HexString: String; const DefaultValue: Half): Half;
begin
Result := HexToFloat16Def(HexString,DefaultValue);
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float32
-------------------------------------------------------------------------------}

Function Float32ToHex(Value: Float32): String;
var
  Overlay:  UInt32 absolute Value;
begin
Result := IntToHex(Overlay,8);
end;
 
//------------------------------------------------------------------------------

Function HexToFloat32(const HexString: String): Float32;
var
  Overlay:  UInt32 absolute Result;
begin
Overlay := UInt32(StrToInt(RectifyHexString(HexString,8)));
end;
 
//------------------------------------------------------------------------------

Function TryHexToFloat32(const HexString: String; out Value: Float32): Boolean;
begin
try
  Value := HexToFloat32(HexString);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function HexToFloat32Def(const HexString: String; const DefaultValue: Float32): Float32;
begin
If not TryHexToFloat32(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Single
-------------------------------------------------------------------------------}

Function SingleToHex(Value: Single): String;
begin
Result := Float32ToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToSingle(const HexString: String): Single;
begin
Result := HexToFloat32(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToSingle(const HexString: String; out Value: Single): Boolean;
begin
Result := TryHexToFloat32(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToSingleDef(const HexString: String; const DefaultValue: Single): Single;
begin
Result := HexToFloat32Def(HexString,DefaultValue);
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float64
-------------------------------------------------------------------------------}

Function Float64ToHex(Value: Float64): String;
var
  Overlay:  UInt64 absolute Value;
begin
Result := IntToHex(Overlay,16);
end;

//------------------------------------------------------------------------------

Function HexToFloat64(const HexString: String): Float64;
var
  Overlay:  Int64 absolute Result;
begin
Overlay := StrToInt64(RectifyHexString(HexString,16));
end;

//------------------------------------------------------------------------------

Function TryHexToFloat64(const HexString: String; out Value: Float64): Boolean;
begin
try
  Value := HexToFloat64(HexString);
  Result := True;
except
  Result := False;
end;
end;
 
//------------------------------------------------------------------------------

Function HexToFloat64Def(const HexString: String; const DefaultValue: Float64): Float64;
begin
If not TryHexToFloat64(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Double
-------------------------------------------------------------------------------}

Function DoubleToHex(Value: Double): String;
begin
Result := Float64ToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToDouble(const HexString: String): Double;
begin
Result := HexToFloat64(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToDouble(const HexString: String; out Value: Double): Boolean;
begin
Result := TryHexToFloat64(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToDoubleDef(const HexString: String; const DefaultValue: Double): Double;
begin
Result := HexToFloat64Def(HexString,DefaultValue);
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float80
-------------------------------------------------------------------------------}

Function Float80ToHex(Value: Float80): String;
var
  Overlay:  TFloat80Overlay absolute Value;
begin
Result := IntToHex(Overlay.Part_16,4) + IntToHex(Overlay.Part_64,16);
end;

//------------------------------------------------------------------------------

Function HexToFloat80(const HexString: String): Float80;
var
  Temp:     String;
  Overlay:  TFloat80Overlay absolute Result;
begin
Temp := RectifyHexString(HexString,20);
Overlay.Part_16 := UInt16(StrToInt(Copy(Temp,1,5)));
Overlay.Part_64 := UInt64(StrToInt64('$' + Copy(Temp,6,16)));
end;
 
//------------------------------------------------------------------------------

Function TryHexToFloat80(const HexString: String; out Value: Float80): Boolean;
begin
try
  Value := HexToFloat80(HexString);
  Result := True;
except
  Result := False;
end;
end;
 
//------------------------------------------------------------------------------

Function HexToFloat80Def(const HexString: String; const DefaultValue: Float80): Float80;
begin
If not TryHexToFloat80(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Extended
-------------------------------------------------------------------------------}

Function ExtendedToHex(Value: Extended): String;
var
  Overlay:  TFloat80Overlay {$IFNDEF Extended64}absolute Value{$ENDIF};
begin
{$IFDEF Extended64}
Float64ToFloat80(@Value,@Overlay);
{$ENDIF}
Result := IntToHex(Overlay.Part_16,4) + IntToHex(Overlay.Part_64,16);
end;

//------------------------------------------------------------------------------

Function HexToExtended(const HexString: String): Extended;
var
  Temp:     String;
  Overlay:  TFloat80Overlay {$IFNDEF Extended64}absolute Result{$ENDIF};
begin
Temp := RectifyHexString(HexString,20);
Overlay.Part_16 := UInt16(StrToInt(Copy(Temp,1,5)));
Overlay.Part_64 := UInt64(StrToInt64('$' + Copy(Temp,6,16)));
{$IFDEF Extended64}
Float80ToFloat64(@Overlay,@Result);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TryHexToExtended(const HexString: String; out Value: Extended): Boolean;
begin
try
  Value := HexToExtended(HexString);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function HexToExtendedDef(const HexString: String; const DefaultValue: Extended): Extended;
begin
If not TryHexToExtended(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - default float type
-------------------------------------------------------------------------------}

Function FloatToHex(Value: Double): String;
begin
Result := DoubleToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToFloat(const HexString: String): Double;
begin
Result := HexToDouble(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToFloat(const HexString: String; out Value: Double): Boolean;
begin
Result := TryHexToDouble(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToFloatDef(const HexString: String; const DefaultValue: Double): Double;
begin
Result := HexToDoubleDef(HexString,DefaultValue);
end;


{===============================================================================
--------------------------------------------------------------------------------
                            Float <-> Integer casting
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Float <-> Integer casting - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Float16
-------------------------------------------------------------------------------}

Function CastToFloat16(Value: UInt16): Float16;
var
  Temp: UInt16 absolute Result;
begin
Temp := Value;
end;

//------------------------------------------------------------------------------

Function CastFromFloat16(Value: Float16): UInt16;
var
  Temp: Float16 absolute Result;
begin
Temp := Value;
end;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Half
-------------------------------------------------------------------------------}

Function CastToHalf(Value: UInt16): Half;
begin
Result := CastToFloat16(Value);
end;

//------------------------------------------------------------------------------

Function CastFromHalf(Value: Half): UInt16;
begin
Result := CastFromFloat16(Value);
end;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Float32
-------------------------------------------------------------------------------}

Function CastToFloat32(Value: UInt32): Float32;
var
  Temp: UInt32 absolute Result;
begin
Temp := Value;
end;

//------------------------------------------------------------------------------

Function CastFromFloat32(Value: Float32): UInt32;
var
  Temp: Float32 absolute Result;
begin
Temp := Value;
end;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Single
-------------------------------------------------------------------------------}

Function CastToSingle(Value: UInt32): Single;
begin
Result := CastToFloat32(Value);
end;

//------------------------------------------------------------------------------

Function CastFromSingle(Value: Single): UInt32;
begin
Result := CastFromFloat32(Value);
end;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Float64
-------------------------------------------------------------------------------}

Function CastToFloat64(Value: UInt64): Float64;
var
  Temp: UInt64 absolute Result;
begin
Temp := Value;
end;

//------------------------------------------------------------------------------

Function CastFromFloat64(Value: Float64): UInt64; 
var
  Temp: Float64 absolute Result;
begin
Temp := Value;
end;


{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Double
-------------------------------------------------------------------------------}

Function CastToDouble(Value: UInt64): Double;
begin
Result := CastToFloat64(Value);
end;

//------------------------------------------------------------------------------

Function CastFromDouble(Value: Double): UInt64;
begin
Result := CastFromFloat64(Value);
end;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Float80
-------------------------------------------------------------------------------}

Function CastToFloat80(const Value): Float80;
begin
Result := Float80(Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CastToFloat80(ExponentWithSign: UInt16; Mantissa: UInt64): Float80;
begin
TFloat80Overlay(Result).SignExponent := ExponentWithSign;
TFloat80Overlay(Result).Mantissa := Mantissa;
end;

//------------------------------------------------------------------------------

procedure CastFromFloat80(Value: Float80; out Result);
begin
Float80(Result) := Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure CastFromFloat80(Value: Float80; out ExponentWithSign: UInt16; out Mantissa: UInt64);
begin
ExponentWithSign := TFloat80Overlay(Value).SignExponent;
Mantissa := TFloat80Overlay(Value).Mantissa;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CastFromFloat80(Value: Float80): TFloat80Overlay;
begin
Result := TFloat80Overlay(Value);
end;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - type Extended
-------------------------------------------------------------------------------}

Function CastToExtended(const Value): Extended;
begin
{$IFDEF Extended64}
Float80ToFloat64(@Value,@Result);
{$ELSE}
Result := Extended(Value);
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CastToExtended(ExponentWithSign: UInt16; Mantissa: UInt64): Extended;
var
  Temp: TFloat80Overlay;
begin
Temp.SignExponent := ExponentWithSign;
Temp.Mantissa := Mantissa;
{$IFDEF Extended64}
Float80ToFloat64(@Temp,@Result);
{$ELSE}
Result := Extended(Temp);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure CastFromExtended(Value: Extended; out Result);
begin
{$IFDEF Extended64}
Float64ToFloat80(@Value,@Result);
{$ELSE}
Extended(Result) := Value;
{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure CastFromExtended(Value: Extended; out ExponentWithSign: UInt16; out Mantissa: UInt64);
var
  Temp: TFloat80Overlay;
begin
{$IFDEF Extended64}
Float64ToFloat80(@Value,@Temp);
{$ELSE}
Temp := TFloat80Overlay(Value);
{$ENDIF}
ExponentWithSign := Temp.SignExponent;
Mantissa := Temp.Mantissa;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CastFromExtended(Value: Extended): TFloat80Overlay;
begin
{$IFDEF Extended64}
Float64ToFloat80(@Value,@Result);
{$ELSE}
Result := TFloat80Overlay(Value);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function NativeCastToExtended(const Value): Extended;
begin
Result := Extended(Value);
end;

//------------------------------------------------------------------------------

procedure NativeCastFromExtended(Value: Extended; out Result);
begin
Extended(Result) := Value;
end;

{-------------------------------------------------------------------------------
    Float <-> Integer casting - default float type
-------------------------------------------------------------------------------}

Function CastToFloat(Value: UInt64): Double;
begin
Result := CastToFloat64(Value);
end;

//------------------------------------------------------------------------------

Function CastFromFloat(Value: Double): UInt64;
begin
Result := CastFromFloat64(Value);
end;


end.
