{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  UInt64Utils

    Utility functions for 64bit unsigned integers. Meant mainly for compilers
    that do not have full native support for this type (eg. Delphi 7).

  Version 1.0.5 (2024-02-17)

  Last change 2024-03-05

  ©2018-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.UInt64Utils

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes

===============================================================================}
unit UInt64Utils;

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
  AuxTypes;

type
  EUI64UException = class(Exception);

  EUI64UConvertError = class(EUI64UException);

//------------------------------------------------------------------------------  
type
  UInt64Rec = packed record
    case Integer of
      0: (Lo, Hi: UInt32);
      1: (Cardinals: array [0..1] of UInt32);
      2: (Words: array [0..3] of UInt16);
      3: (Bytes: array [0..7] of UInt8);
  end;

//------------------------------------------------------------------------------

Function UInt64Low: UInt64;{$IFDEF CanInline} inline;{$ENDIF}
Function LowUInt64: UInt64;{$IFDEF CanInline} inline;{$ENDIF}

Function UInt64High: UInt64;{$IFDEF CanInline} inline;{$ENDIF}
Function HighUInt64: UInt64;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function UInt64Get(Hi,Lo: UInt32): UInt64; overload;
Function UInt64Get(Lo: UInt32): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function GetUInt64(Hi,Lo: UInt32): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function GetUInt64(Lo: UInt32): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function UInt64ToStr(Value: UInt64): String;

Function StrToUInt64(const Str: String): UInt64;

Function UInt64ToHex(Value: UInt64): String;

Function HexToUInt64(const Str: String): UInt64;

Function TryStrToUInt64(const Str: String; out Value: UInt64): Boolean;

Function StrToUInt64Def(const Str: String; Default: UInt64): UInt64;

//------------------------------------------------------------------------------ 
{
  Returns negative number if A is less than B, positive number when A is larger
  than B, zero when they equals.
}
Function CompareUInt64(A,B: UInt64): Integer;

Function SameUInt64(A,B: UInt64): Boolean;

Function IsEqualUInt64(A,B: UInt64): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

Function IsLessUInt64(A,B: UInt64): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

Function IsLessOrEqualUInt64(A,B: UInt64): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

Function IsGreaterUInt64(A,B: UInt64): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

Function IsGreaterOrEqualUInt64(A,B: UInt64): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

implementation

Function UInt64Low: UInt64;
begin
Result := UInt64(0);
end;

//------------------------------------------------------------------------------

Function LowUInt64: UInt64;
begin
Result := UInt64(0);
end;

//------------------------------------------------------------------------------

Function UInt64High: UInt64;
begin
Result := UInt64($FFFFFFFFFFFFFFFF);
end;

//------------------------------------------------------------------------------

Function HighUInt64: UInt64;
begin
Result := UInt64($FFFFFFFFFFFFFFFF);
end;

//==============================================================================

Function UInt64Get(Hi,Lo: UInt32): UInt64;
begin
UInt64Rec(Result).Hi := Hi;
UInt64Rec(Result).Lo := Lo;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function UInt64Get(Lo: UInt32): UInt64;
begin
Result := UInt64Get(0,Lo);
end;

//------------------------------------------------------------------------------

Function GetUInt64(Hi,Lo: UInt32): UInt64;
begin
Result := UInt64Get(Hi,Lo);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetUInt64(Lo: UInt32): UInt64;
begin
Result := UInt64Get(0,Lo);
end;

//==============================================================================

const
  UInt64NumTable: array[0..63] of String = (
    '00000000000000000001','00000000000000000002','00000000000000000004','00000000000000000008',
    '00000000000000000016','00000000000000000032','00000000000000000064','00000000000000000128',
    '00000000000000000256','00000000000000000512','00000000000000001024','00000000000000002048',
    '00000000000000004096','00000000000000008192','00000000000000016384','00000000000000032768',
    '00000000000000065536','00000000000000131072','00000000000000262144','00000000000000524288',
    '00000000000001048576','00000000000002097152','00000000000004194304','00000000000008388608',
    '00000000000016777216','00000000000033554432','00000000000067108864','00000000000134217728',
    '00000000000268435456','00000000000536870912','00000000001073741824','00000000002147483648',
    '00000000004294967296','00000000008589934592','00000000017179869184','00000000034359738368',
    '00000000068719476736','00000000137438953472','00000000274877906944','00000000549755813888',
    '00000001099511627776','00000002199023255552','00000004398046511104','00000008796093022208',
    '00000017592186044416','00000035184372088832','00000070368744177664','00000140737488355328',
    '00000281474976710656','00000562949953421312','00001125899906842624','00002251799813685248',
    '00004503599627370496','00009007199254740992','00018014398509481984','00036028797018963968',
    '00072057594037927936','00144115188075855872','00288230376151711744','00576460752303423488',
    '01152921504606846976','02305843009213693952','04611686018427387904','09223372036854775808');

//------------------------------------------------------------------------------

Function UInt64ToStr(Value: UInt64): String;
var
  i,j:      Integer;
  CharOrd:  Integer;
  Carry:    Integer;
begin
// prepare resulting string
Result := (StringOfChar('0',Length(UInt64NumTable[0])));
Carry := 0;
// calculations
For i := 0 to 63 do
  If ((Value shr i) and 1) <> 0 then
    For j := Length(Result) downto 1 do
      begin
        CharOrd := (Ord(Result[j]) - Ord('0')) + (Ord(UInt64NumTable[i][j]) - Ord('0')) + Carry;
        Carry := CharOrd div 10;
        Result[j] := Char(Ord('0') + CharOrd mod 10);
      end;
// remove leading zeroes
i := 0;
repeat
  Inc(i);
until (Result[i] <> '0') or (i >= Length(Result));
Result := Copy(Result,i,Length(Result));
end;

//------------------------------------------------------------------------------

Function StrToUInt64(const Str: String): UInt64;
var
  TempStr:  String;
  ResStr:   String;
  i:        Integer;

  Function CompareValStr(const S1,S2: String): Integer;
  var
    ii: Integer;
  begin
    Result := 0;
    For ii := 1 to Length(S1) do
      If Ord(S1[ii]) < Ord(S2[ii]) then
        begin
          Result := -1;
          Break{For ii};
        end
      else If Ord(S1[ii]) > Ord(S2[ii]) then
        begin
          Result := +1;
          Break{For ii};
        end      
  end;

  Function SubtractValStr(const S1,S2: String; out Res: String): Integer;
  var
    ii:       Integer;
    CharVal:  Integer;
  begin
    Res := '';
    SetLength(Res,Length(S1));
    Result := 0;
    For ii := Length(S1) downto 1 do
      begin
        CharVal := Ord(S1[ii]) - Ord(S2[ii]) + Result;
        If CharVal < 0 then
          begin
            CharVal := CharVal + 10;
            Result := -1;
          end
        else Result := 0;
        Res[ii] := Char(Abs(CharVal) + Ord('0'));
      end;
    If Result < 0 then
      Res := S1;  
  end;

begin
Result := 0;
// look whether the string is hexadecimal
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      begin
        Result := HexToUInt64(Str);
        Exit;
      end
    else If Length(Str) >= 2 then
      If (Str[1] = '0') and ((Str[2] = 'x') or (Str[2] = 'X')) then
        begin
          Result := HexToUInt64(Str);
          Exit;
        end;
    // rectify string
    If Length(Str) < Length(UInt64NumTable[0]) then
      TempStr := StringOfChar('0',Length(UInt64NumTable[0]) - Length(Str)) + Str
    else If Length(Str) > Length(UInt64NumTable[0]) then
      raise EUI64UConvertError.CreateFmt('StrToUInt64: "%s" is not a valid integer string.',[Str])
    else
      TempStr := Str;
    // check if string contains only numbers
    For i := 1 to Length(TempStr) do
      If not(Ord(TempStr[i]) in [Ord('0')..Ord('9')]) then
        raise EUI64UConvertError.CreateFmt('StrToUInt64: "%s" is not a valid integer string.',[Str]);
    // do the calculations
    For i := 63 downto 0 do
      If SubtractValStr(TempStr,UInt64NumTable[i],ResStr) >= 0 then
        begin
          If CompareValStr(ResStr,UInt64NumTable[i]) < 0 then
            begin
              Result := Result or (UInt64(1) shl i);
              TempStr := ResStr;
            end
          else raise EUI64UConvertError.CreateFmt('StrToUInt64: "%s" is not a valid integer string.',[Str]);
        end;
  end
else raise EUI64UConvertError.CreateFmt('StrToUInt64: "%s" is not a valid integer string.',[Str]);
end;

//------------------------------------------------------------------------------

Function UInt64ToHex(Value: UInt64): String;
const
  UInt64HexTable: array[0..15] of Char = (
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  i:  Integer;
begin
Result := StringOfChar('0',16);
For i := 0 to 15 do
  begin
    Result[16 - i] := UInt64HexTable[Value and 15];
    Value := Value shr 4;
  end;
end;

//------------------------------------------------------------------------------

Function HexToUInt64(const Str: String): UInt64;
var
  i:      Integer;
  Start:  Integer;
begin
Result := 0;
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      begin
        If Length(Str) <= 1 then
          raise EUI64UConvertError.CreateFmt('HexToUInt64: "%s" is not a valid hexadecimal string.',[Str]);
        Start := 2;
      end
    else If Length(Str) >= 2 then
      begin
        If (Str[1] = '0') and ((Str[2] = 'x') or (Str[2] = 'X')) then
          begin
            If Length(Str) <= 2 then
              raise EUI64UConvertError.CreateFmt('HexToUInt64: "%s" is not a valid hexadecimal string.',[Str]);
            Start := 3;
          end
        else Start := 1;
      end
    else Start := 1;
    If (Length(Str) - Start) <= 15 then
      begin
        For i := Start to Length(Str) do
          begin
            Result := UInt64(Result shl 4);
            case Str[i] of
              '0'..'9': Result := Result or UInt64(Ord(Str[i]) - Ord('0'));
              'a'..'f': Result := Result or UInt64(Ord(Str[i]) - Ord('a') + 10);
              'A'..'F': Result := Result or UInt64(Ord(Str[i]) - Ord('A') + 10);
            else
              raise EUI64UConvertError.CreateFmt('HexToUInt64: "%s" is not a valid hexadecimal string.',[Str]);
            end;
          end;
      end
    else raise EUI64UConvertError.CreateFmt('HexToUInt64: "%s" is not a valid hexadecimal string.',[Str]);
  end
else raise EUI64UConvertError.CreateFmt('HexToUInt64: "%s" is not a valid hexadecimal string.',[Str]);
end;

//------------------------------------------------------------------------------

Function TryStrToUInt64(const Str: String; out Value: UInt64): Boolean;
begin
try
  Value := StrToUInt64(Str);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToUInt64Def(const Str: String; Default: UInt64): UInt64;
begin
If not TryStrToUInt64(Str,Result) then
  Result := Default;
end;

//==============================================================================

Function CompareUInt64(A,B: UInt64): Integer;
begin
If UInt64Rec(A).Hi > UInt64Rec(B).Hi then
  Result := +1
else If UInt64Rec(A).Hi < UInt64Rec(B).Hi then
  Result := -1
else
  begin
    // higher 32bits are the same, compare lower 32 bits
    If UInt64Rec(A).Lo > UInt64Rec(B).Lo then
      Result := +1
    else If UInt64Rec(A).Lo < UInt64Rec(B).Lo then
      Result := -1
    else
      Result := 0;
  end;
end;

//------------------------------------------------------------------------------

Function SameUInt64(A,B: UInt64): Boolean;
begin
Result := (UInt64Rec(A).Hi = UInt64Rec(B).Hi) and
          (UInt64Rec(A).Lo = UInt64Rec(B).Lo);
end;

//------------------------------------------------------------------------------

Function IsEqualUInt64(A,B: UInt64): Boolean;
begin
Result := SameUInt64(A,B);
end;

//------------------------------------------------------------------------------

Function IsLessUInt64(A,B: UInt64): Boolean;
begin
Result := CompareUInt64(A,B) < 0;
end;

//------------------------------------------------------------------------------

Function IsLessOrEqualUInt64(A,B: UInt64): Boolean;
begin
Result := CompareUInt64(A,B) <= 0;
end;

//------------------------------------------------------------------------------

Function IsGreaterUInt64(A,B: UInt64): Boolean;
begin
Result := CompareUInt64(A,B) > 0;
end;

//------------------------------------------------------------------------------

Function IsGreaterOrEqualUInt64(A,B: UInt64): Boolean;
begin
Result := CompareUInt64(A,B) >= 0;
end;

end.

