{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SuperFast Hash

    Note that currently implemented algorithm uses zero as initial value, but
    I have seen code where length of hashed buffer/key was used for init value.
    As such behavior can be emulated (using LastBufferSuperFast in procedural
    interface or by creating TSuperFastHash instance using CreateAndInitFrom
    constructor), I have decided to not implement it explicitly.

    Author of this algorithm is Paul Hsieh, sometimes this hash is refered to
    as Paul Hsieh's Hash.

  Version 1.0.1 (2026-07-14)

  Last change 2026-07-14

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SuperFastHash

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
unit SuperFastHash;

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
  ESFHException = class(EHashException);

  ESFHIncompatibleClass = class(ESFHException);
  ESFHProcessingError   = class(ESFHException);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TSuperFast are, in memory, always ordered from least significant
  byte to most significant byte (little endian).

  Type TSuperFastSys has no such guarantee and its endianness is system and
  implementation dependent.

  To convert the hash from default ordering to a required specific ordering,
  use method SuperFastToLE for little endian and SuperFastToBE for big endian.
}
type
  TSuperFast = packed array[0..3] of UInt8;
  PSuperFast = ^TSuperFast;

  TSuperFastSys = UInt32;
  PSuperFastSys = ^TSuperFastSys;

  // only for internal use
  TSFHRemainderBuffer = packed array[0..3] of UInt8;

const
  InitialSuperFast: TSuperFast = ($00,$00,$00,$00);

  ZeroSuperFast: TSuperFast = (0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                 TSuperFastHash
--------------------------------------------------------------------------------
===============================================================================}
{
  Technically, this should be declared as a block hash, but since the block is
  only 4 bytes, the overhead would be too much - therefore it is a descendant
  of stream hash and remainders are managed locally.
}
{===============================================================================
    TSuperFastHash - class declaration
===============================================================================}
type
  TSuperFastHash = class(TStreamHash)
  protected
    fSuperFastValue:  TSuperFastSys;
    fRemainder:       TSFHRemainderBuffer;
    fRemainderBytes:  Integer;
    Function GetSuperFast: TSuperFast; virtual;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function SuperFastToSys(Hash: TSuperFast): TSuperFastSys; virtual;
    class Function SuperFastFromSys(Hash: TSuperFastSys): TSuperFast; virtual;
    class Function SuperFastToLE(Hash: TSuperFast): TSuperFast; virtual;
    class Function SuperFastToBE(Hash: TSuperFast): TSuperFast; virtual;
    class Function SuperFastFromLE(Hash: TSuperFast): TSuperFast; virtual;
    class Function SuperFastFromBE(Hash: TSuperFast): TSuperFast; virtual;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSuperFast); overload; virtual;
    procedure Init; override;
    procedure Final; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSuperFast); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property SuperFast: TSuperFast read GetSuperFast;
    property SuperFastSys: TSuperFastSys read fSuperFastValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - declaration
===============================================================================}

Function SuperFastToStr(const Hash: TSuperFast): String;
Function StrToSuperFast(const Str: String): TSuperFast;
Function TryStrToSuperFast(const Str: String; out Hash: TSuperFast): Boolean;
Function StrToSuperFastDef(const Str: String; Default: TSuperFast): TSuperFast;

Function CompareSuperFast(const A,B: TSuperFast): Integer;
Function SameSuperFast(const A,B: TSuperFast): Boolean;

//------------------------------------------------------------------------------

Function BufferSuperFast(const Hash: TSuperFast; const Buffer; Size: TMemSize): TSuperFast; overload;
Function LastBufferSuperFast(const Hash: TSuperFast; const Buffer; Size: TMemSize): TSuperFast;

Function BufferSuperFast(const Buffer; Size: TMemSize): TSuperFast; overload;

Function AnsiStringSuperFast(const Str: AnsiString): TSuperFast;
Function WideStringSuperFast(const Str: WideString): TSuperFast;
Function StringSuperFast(const Str: String): TSuperFast;

Function StreamSuperFast(Stream: TStream; Count: Int64 = -1): TSuperFast;
Function FileSuperFast(const FileName: String): TSuperFast;

//------------------------------------------------------------------------------
type
  TSuperFastContext = type Pointer;

Function SuperFast_Init: TSuperFastContext;
procedure SuperFast_Update(var Context: TSuperFastContext; const Buffer; Size: TMemSize);
Function SuperFast_Final(var Context: TSuperFastContext; const Buffer; Size: TMemSize): TSuperFast; overload;
Function SuperFast_Final(var Context: TSuperFastContext): TSuperFast; overload;
Function SuperFast_Hash(const Buffer; Size: TMemSize): TSuperFast;

implementation

uses
  SysUtils;

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ELSE}
  {$UNDEF OverflowChecks}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE RangeChecks}
{$ELSE}
  {$UNDEF RangeChecks}
{$ENDIF}

{===============================================================================
    Auxiliary routines
===============================================================================}

Function SwapEndian(Hash: UInt16): UInt16; overload;
begin
Result := UInt16(((Hash and $00FF) shl 8) or ((Hash and $FF00) shr 8));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: UInt32): UInt32; overload;
begin
Result := UInt32(((Hash and $000000FF) shl 24) or ((Hash and $0000FF00) shl 8) or
                 ((Hash and $00FF0000) shr 8) or ((Hash and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TSuperFast): TSuperFast; overload;
begin
Result := TSuperFast(SwapEndian(TSuperFastSys(Hash)));
end;

//------------------------------------------------------------------------------

Function Get16Bits(Ptr: Pointer): UInt16;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(PUInt16(Ptr)^);
end;

{===============================================================================
    Main implementation
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function SuperFastProcess(const SuperFast: TSuperFastSys; var Remainder: TSFHRemainderBuffer; var RemainderBytes: Integer; const Buffer; Size: TMemSize): TSuperFastSys;
var
  CurrentData:  PUInt8;
  Temp:         TSuperFastSys;
begin
Result := SuperFast;
If Size > 0 then
  begin
    CurrentData := @Buffer;
    // first process remainder, if any
    RemainderBytes := RemainderBytes and 3;
    If RemainderBytes > 0 then
      begin
        while (Size > 0) and (RemainderBytes < 4) do begin
          Remainder[RemainderBytes] := CurrentData^;
          Inc(RemainderBytes);
          Inc(CurrentData);
          Dec(Size);
        end;
        If RemainderBytes >= 4 then
          begin
            RemainderBytes := 0;
            // use recursion to deal with the full remainder
            Result := SuperFastProcess(Result,Remainder,RemainderBytes{0},Remainder,4);
          end
        else Exit;  // we have only added to remainder
      end;
    // process data in 4-byte blocks
    while Size >= 4 do begin
      Result := Result + TSuperFastSys(Get16Bits(CurrentData));
      Inc(CurrentData,2);
      Temp := TSuperFastSys(TSuperFastSys(Get16Bits(CurrentData)) shl 11) xor Result;
      Result := TSuperFastSys(Result shl 16) xor Temp;
      Result := Result + (Result shr 11);
      Inc(CurrentData,2);
      Dec(Size,4);
    end;
    // store remainder, if any
    If Size <> 0 then
      begin
        RemainderBytes := Integer(Size and 3);
        Move(CurrentData^,Remainder,RemainderBytes);
      end;
  end;
end;

//------------------------------------------------------------------------------

Function SuperFastFinal(const SuperFast: TSuperFastSys; Remainder: TSFHRemainderBuffer; RemainderBytes: Integer): TSuperFastSys;
begin
Result := SuperFast;
case RemainderBytes of
  3:  begin
    Result := Result + TSuperFastSys(Get16Bits(@Remainder[0]));
    Result := Result xor TSuperFastSys(Result shl 16);
    // in the original source, the data are SIGNED CHARS!
    Result := Result xor TSuperFastSys(Int32(Int8(Remainder[2])) shl 18);
    Result := Result + (Result shr 11);
  end;
  2:  begin
    Result := Result + TSuperFastSys(Get16Bits(@Remainder[0]));
    Result := Result xor TSuperFastSys(Result shl 11);
    Result := Result + (Result shr 17);
  end;
  1:  begin
    Result := Result + TSuperFastSys({sign-extension}Int32(Int8(Remainder[0])));
    Result := Result xor TSuperFastSys(Result shl 10);
    Result := Result + (Result shr 1);
  end;
end;
Result := Result xor TSuperFastSys(Result shl 3);
Result := Result + (Result shr 5);
Result := Result xor TSuperFastSys(Result shl 4);
Result := Result + (Result shr 17);
Result := Result xor TSuperFastSys(Result shl 25);
Result := Result + (Result shr 6);
end;

{$IFDEF RangeChecks}{$R+}{$ENDIF}
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
//==============================================================================

Function SuperFastCompare(const A,B: TSuperFastSys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function SuperFastSame(const A,B: TSuperFastSys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function SuperFastAsString(const SuperFast: TSuperFastSys): String;
begin
Result := IntToHex(SuperFast,8);
end;

//------------------------------------------------------------------------------

Function SuperFastFromString(const Str: String): TSuperFastSys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TSuperFastSys(StrToInt(Str))
    else
      Result := TSuperFastSys(StrToInt('$' + Str));
  end
else Result := TSuperFastHash.SuperFastToSys(ZeroSuperFast);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TSuperFastHash
--------------------------------------------------------------------------------
===============================================================================}
const
  EmptyRemainder: TSFHRemainderBuffer = (0,0,0,0);

{===============================================================================
    TSuperFastHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSuperFastHash - protected methods
-------------------------------------------------------------------------------}

Function TSuperFastHash.GetSuperFast: TSuperFast;
begin
Result := SuperFastFromSys(fSuperFastValue);
end;

//------------------------------------------------------------------------------

procedure TSuperFastHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fSuperFastValue := SuperFastProcess(fSuperFastValue,fRemainder,fRemainderBytes,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TSuperFastHash.Initialize;
begin
inherited;
fSuperFastValue := SuperFastToSys(InitialSuperFast);
end;

{-------------------------------------------------------------------------------
    TSuperFastHash - public methods
-------------------------------------------------------------------------------}

class Function TSuperFastHash.SuperFastToSys(Hash: TSuperFast): TSuperFastSys;
begin
Result := TSuperFastSys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.SuperFastFromSys(Hash: TSuperFastSys): TSuperFast;
begin
Result := TSuperFast({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.SuperFastToLE(Hash: TSuperFast): TSuperFast;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.SuperFastToBE(Hash: TSuperFast): TSuperFast;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.SuperFastFromLE(Hash: TSuperFast): TSuperFast;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.SuperFastFromBE(Hash: TSuperFast): TSuperFast;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.HashSize: TMemSize;
begin
Result := SizeOf(TSuperFast);
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

class Function TSuperFastHash.HashName: String;
begin
Result := 'SuperFast';
end;

//------------------------------------------------------------------------------

constructor TSuperFastHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSuperFastHash then
  begin
    fSuperFastValue := TSuperFastHash(Hash).SuperFastSys;
    fRemainder := TSuperFastHash(Hash).fRemainder;
    fRemainderBytes := TSuperFastHash(Hash).fRemainderBytes;
  end
else raise ESFHIncompatibleClass.CreateFmt('TSuperFastHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSuperFastHash.CreateAndInitFrom(Hash: TSuperFast);
begin
CreateAndInit;
fSuperFastValue := SuperFastToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TSuperFastHash.Init;
begin
inherited;
fSuperFastValue := SuperFastToSys(InitialSuperFast);
fRemainder := EmptyRemainder;
fRemainderBytes := 0;
end;

//------------------------------------------------------------------------------

procedure TSuperFastHash.Final;
begin
fSuperFastValue := SuperFastFinal(fSuperFastValue,fRemainder,fRemainderBytes);
inherited;
end;

//------------------------------------------------------------------------------

Function TSuperFastHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TSuperFastHash then
  Result := SuperFastCompare(fSuperFastValue,TSuperFastHash(Hash).SuperFastSys)
else
  raise ESFHIncompatibleClass.CreateFmt('TSuperFastHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSuperFastHash.Same(Hash: THashBase): Boolean;
begin
If Hash is TSuperFastHash then
  Result := SuperFastSame(fSuperFastValue,TSuperFastHash(Hash).SuperFastSys)
else
  raise ESFHIncompatibleClass.CreateFmt('TSuperFastHash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;


//------------------------------------------------------------------------------

Function TSuperFastHash.AsString: String;
begin
Result := SuperFastAsString(fSuperFastValue);
end;

//------------------------------------------------------------------------------

procedure TSuperFastHash.FromString(const Str: String);
begin
fSuperFastValue := SuperFastFromString(Str);
end;

//------------------------------------------------------------------------------

procedure TSuperFastHash.FromStringDef(const Str: String; const Default: TSuperFast);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSuperFastValue := SuperFastToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TSuperFastHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSuperFast;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}SuperFastToBE{$ELSE}SuperFastToLE{$ENDIF}(SuperFastFromSys(fSuperFastValue));
  heLittle: Temp := SuperFastToLE(SuperFastFromSys(fSuperFastValue));
  heBig:    Temp := SuperFastToBE(SuperFastFromSys(fSuperFastValue));
else
 {heDefault}
  Temp := SuperFastFromSys(fSuperFastValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TSuperFast));
end;

//------------------------------------------------------------------------------

procedure TSuperFastHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSuperFast;
begin
Temp := ZeroSuperFast;
Stream.ReadBuffer(Temp,SizeOf(TSuperFast));
case Endianness of
  heSystem: fSuperFastValue := SuperFastToSys({$IFDEF ENDIAN_BIG}SuperFastFromBE{$ELSE}SuperFastFromLE{$ENDIF}(Temp));
  heLittle: fSuperFastValue := SuperFastToSys(SuperFastFromLE(Temp));
  heBig:    fSuperFastValue := SuperFastToSys(SuperFastFromBE(Temp));
else
 {heDefault}
  fSuperFastValue := SuperFastToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Procedural interface - utility functions
-------------------------------------------------------------------------------}

Function SuperFastToStr(const Hash: TSuperFast): String;
begin
Result := SuperFastAsString(TSuperFastHash.SuperFastToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToSuperFast(const Str: String): TSuperFast;
begin
Result := TSuperFastHash.SuperFastFromSys(SuperFastFromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToSuperFast(const Str: String; out Hash: TSuperFast): Boolean;
begin
try
  Hash := TSuperFastHash.SuperFastFromSys(SuperFastFromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToSuperFastDef(const Str: String; Default: TSuperFast): TSuperFast;
begin
If not TryStrToSuperFast(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareSuperFast(const A,B: TSuperFast): Integer;
begin
Result := SuperFastCompare(TSuperFastHash.SuperFastToSys(A),TSuperFastHash.SuperFastToSys(B));
end;

//------------------------------------------------------------------------------

Function SameSuperFast(const A,B: TSuperFast): Boolean;
begin
Result := SuperFastSame(TSuperFastHash.SuperFastToSys(A),TSuperFastHash.SuperFastToSys(B));
end;

{-------------------------------------------------------------------------------
    Procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferSuperFast(const Hash: TSuperFast; const Buffer; Size: TMemSize): TSuperFast;
var
  Remainder:      TSFHRemainderBuffer;
  RemainderBytes: Integer;
begin
If Size > 0 then
  begin
    If (Size and 3) = 0 then
      begin
        Remainder := EmptyRemainder;
        RemainderBytes := 0;
        Result := TSuperFastHash.SuperFastFromSys(SuperFastProcess(
          TSuperFastHash.SuperFastToSys(Hash),Remainder,RemainderBytes,Buffer,Size));
      end
    else raise ESFHProcessingError.CreateFmt('BufferSuperFast: Buffer size (%u) is not divisible by 4.',[Size]);
  end
else Result := Hash;
end;

//------------------------------------------------------------------------------

Function LastBufferSuperFast(const Hash: TSuperFast; const Buffer; Size: TMemSize): TSuperFast;
var
  Remainder:      TSFHRemainderBuffer;
  RemainderBytes: Integer;
  TempHash:       TSuperFastSys;
begin
Remainder := EmptyRemainder;
RemainderBytes := 0;
TempHash := SuperFastProcess(TSuperFastHash.SuperFastToSys(Hash),Remainder,RemainderBytes,Buffer,Size);
Result := TSuperFastHash.SuperFastFromSys(SuperFastFinal(TempHash,Remainder,RemainderBytes));
end;

//------------------------------------------------------------------------------

Function BufferSuperFast(const Buffer; Size: TMemSize): TSuperFast;
begin
Result := LastBufferSuperFast(InitialSuperFast,Buffer,Size);
end;

//------------------------------------------------------------------------------

Function AnsiStringSuperFast(const Str: AnsiString): TSuperFast;
begin
Result := BufferSuperFast(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringSuperFast(const Str: WideString): TSuperFast;
begin
Result := BufferSuperFast(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringSuperFast(const Str: String): TSuperFast;
begin
Result := BufferSuperFast(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamSuperFast(Stream: TStream; Count: Int64 = -1): TSuperFast;
var
  Hasher: TSuperFastHash;
begin
Hasher := TSuperFastHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.SuperFast;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSuperFast(const FileName: String): TSuperFast;
var
  Hasher: TSuperFastHash;
begin
Hasher := TSuperFastHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.SuperFast;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - context functions
-------------------------------------------------------------------------------}

Function SuperFast_Init: TSuperFastContext;
begin
Result := TSuperFastContext(TSuperFastHash.CreateAndInit);
end;

//------------------------------------------------------------------------------

procedure SuperFast_Update(var Context: TSuperFastContext; const Buffer; Size: TMemSize);
begin
TSuperFastHash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SuperFast_Final(var Context: TSuperFastContext; const Buffer; Size: TMemSize): TSuperFast;
begin
SuperFast_Update(Context,Buffer,Size);
Result := SuperFast_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SuperFast_Final(var Context: TSuperFastContext): TSuperFast;
begin
TSuperFastHash(Context).Final;
Result := TSuperFastHash(Context).SuperFast;
FreeAndNil(TSuperFastHash(Context))
end;

//------------------------------------------------------------------------------

Function SuperFast_Hash(const Buffer; Size: TMemSize): TSuperFast;
begin
Result := BufferSuperFast(Buffer,Size);
end;

end.
