{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SDBM Hash

    Provides means of calculating a simple (some may even say primitive)
    SDBM hash.

    I have little to no information regarding the hash (eg. its authors
    or quality), but it is supposed to be used in sdbm (hence the name) -
    a public-domain reimplementation of ndbm (database library).

  Version 1.1 (2026-07-04)

  Last change 2026-07-04

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SDBMHash

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
unit SDBMHash;

{$IF Defined(CPU386) or Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE CPU_x86x}
{$ELSE}
  {$UNDEF CPU_x86x}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

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

interface

uses
  AuxTypes, HashBase,
  Classes;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESDBMException = class(EHashException);

  ESDBMIncompatibleClass = class(ESDBMException);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TSDBM are, in memory, always ordered from least significant byte to
  most significant byte (little endian).

  Type TSDBMSys has no such guarantee and its endianness is system-dependent.

  To convert the checksum in default ordering to a required specific ordering,
  use method SDBMToLE for little endian and SDBMToBE for big endian. Note that
  these methods are expecting the input value to be in default ordering, if it
  is not, the result will be wrong. Be careful when using them.
}
type
  TSDBM = packed array[0..3] of UInt8;
  PSDBM = ^TSDBM;

  TSDBMSys = UInt32;
  PSDBMSys = ^TSDBMSys;

const
  InitialSDBM: TSDBM = ($00,$00,$00,$00);

  ZeroSDBM: TSDBM = (0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                    TSDBMHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSDBMHash - class declaration
===============================================================================}
type
  TSDBMHash = class(TStreamHash)
  protected
    fSDBMValue: TSDBMSys;
    Function GetSDBM: TSDBM; virtual;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function SDBMToSys(Hash: TSDBM): TSDBMSys; virtual;
    class Function SDBMFromSys(Hash: TSDBMSys): TSDBM; virtual;
    class Function SDBMToLE(Hash: TSDBM): TSDBM; virtual;
    class Function SDBMToBE(Hash: TSDBM): TSDBM; virtual;
    class Function SDBMFromLE(Hash: TSDBM): TSDBM; virtual;
    class Function SDBMFromBE(Hash: TSDBM): TSDBM; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSDBM); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSDBM); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property SDBM: TSDBM read GetSDBM;
    property SDBMSys: TSDBMSys read fSDBMValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{
  Most functions provided here are using direct implementation, but some are
  only wrappers around TSDBMHash class and its methods - namely StreamSDBM and
  FileSDBM.
}
{===============================================================================
    Procedural interface - declaration
===============================================================================}

Function SDBMToStr(Hash: TSDBM): String;
Function StrToSDBM(const Str: String): TSDBM;
Function TryStrToSDBM(const Str: String; out Hash: TSDBM): Boolean;
Function StrToSDBMDef(const Str: String; Default: TSDBM): TSDBM;

Function CompareSDBM(A,B: TSDBM): Integer;
Function SameSDBM(A,B: TSDBM): Boolean;

//------------------------------------------------------------------------------

Function BufferSDBM(Hash: TSDBM; const Buffer; Size: TMemSize): TSDBM; overload;
Function BufferSDBM(const Buffer; Size: TMemSize): TSDBM; overload;

Function AnsiStringSDBM(const Str: AnsiString): TSDBM;
Function WideStringSDBM(const Str: WideString): TSDBM;
Function StringSDBM(const Str: String): TSDBM;

Function StreamSDBM(Stream: TStream; Count: Int64 = -1): TSDBM;
Function FileSDBM(const FileName: String): TSDBM;

//------------------------------------------------------------------------------
type
  TSDBMContext = type TSDBMSys;

Function SDBM_Init: TSDBMContext;
procedure SDBM_Update(var Context: TSDBMContext; const Buffer; Size: TMemSize);
Function SDBM_Final(var Context: TSDBMContext; const Buffer; Size: TMemSize): TSDBM; overload;
Function SDBM_Final(var Context: TSDBMContext): TSDBM; overload;
Function SDBM_Hash(const Buffer; Size: TMemSize): TSDBM;

implementation

uses
  SysUtils;

{===============================================================================
    Main implementation
===============================================================================}

Function SwapEndian(Hash: TSDBMSys): TSDBMSys; overload;
begin
Result := TSDBMSys(((Hash and $000000FF) shl 24) or ((Hash and $0000FF00) shl 8) or
                   ((Hash and $00FF0000) shr 8) or ((Hash and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TSDBM): TSDBM; overload;
begin
Result := TSDBM(SwapEndian(TSDBMSys(Hash)));
end;

//==============================================================================

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function SDBMProcess(const Hash: TSDBMSys; const Buffer; Size: TMemSize): TSDBMSys;
var
  WorkHash:     TSDBMSys;
  CurrentData:  PUInt8;
  i:            TMemSize;
begin
WorkHash := Hash;
If Size > 0 then
  begin
    CurrentData := @Buffer;
    For i := 1 to Size do
      begin
      {$IFDEF CPU_x86x}
        // multiplication is faster on x86 cpu
        WorkHash := (WorkHash * TSDBMSys(65599)) + TSDBMSys(CurrentData^);
      {$ELSE}
        WorkHash := TSDBMSys(WorkHash shl 16) + (TSDBMSys(WorkHash shl 6) - WorkHash) + TSDBMSys(CurrentData^);
      {$ENDIF}
        Inc(CurrentData);
      end;
  end;
Result := WorkHash;
end;

{$IFDEF RangeChecks}{$R+}{$ENDIF}
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

Function SDBMCompare(const A,B: TSDBMSys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function SDBMSame(const A,B: TSDBMSys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function SDBMAsString(const SDBM: TSDBMSys): String;
begin
Result := IntToHex(SDBM,8);
end;

//------------------------------------------------------------------------------

Function SDBMFromString(const Str: String): TSDBMSys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TSDBMSys(StrToInt(Str))
    else
      Result := TSDBMSys(StrToInt('$' + Str));
  end
else Result := TSDBMHash.SDBMToSys(ZeroSDBM);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TSDBMHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSDBMHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSDBMHash - protected methods
-------------------------------------------------------------------------------}

Function TSDBMHash.GetSDBM: TSDBM;
begin
Result := SDBMFromSys(fSDBMValue);
end;

//------------------------------------------------------------------------------

procedure TSDBMHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fSDBMValue := SDBMProcess(fSDBMValue,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TSDBMHash.Initialize;
begin
inherited;
fSDBMValue := SDBMToSys(InitialSDBM);
end;

{-------------------------------------------------------------------------------
    TSDBMHash - public methods
-------------------------------------------------------------------------------}

class Function TSDBMHash.SDBMToSys(Hash: TSDBM): TSDBMSys;
begin
Result := TSDBMSys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSDBMHash.SDBMFromSys(Hash: TSDBMSys): TSDBM;
begin
Result := TSDBM({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TSDBMHash.SDBMToLE(Hash: TSDBM): TSDBM;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSDBMHash.SDBMToBE(Hash: TSDBM): TSDBM;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSDBMHash.SDBMFromLE(Hash: TSDBM): TSDBM;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TSDBMHash.SDBMFromBE(Hash: TSDBM): TSDBM;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TSDBMHash.HashSize: TMemSize;
begin
Result := SizeOf(TSDBM);
end;

//------------------------------------------------------------------------------

class Function TSDBMHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TSDBMHash.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

class Function TSDBMHash.HashName: String;
begin
Result := 'SDBM';
end;

//------------------------------------------------------------------------------

constructor TSDBMHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSDBMHash then
  fSDBMValue := TSDBMHash(Hash).SDBMSys
else
  raise ESDBMIncompatibleClass.CreateFmt('TSDBMHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSDBMHash.CreateAndInitFrom(Hash: TSDBM);
begin
CreateAndInit;
fSDBMValue := SDBMToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TSDBMHash.Init;
begin
inherited;
fSDBMValue := SDBMToSys(InitialSDBM);
end;

//------------------------------------------------------------------------------

Function TSDBMHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TSDBMHash then
  Result := SDBMCompare(fSDBMValue,TSDBMHash(Hash).SDBMSys)
else
  raise ESDBMIncompatibleClass.CreateFmt('TSDBMHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSDBMHash.Same(Hash: THashBase): Boolean;
begin
If Hash is TSDBMHash then
  Result := SDBMSame(fSDBMValue,TSDBMHash(Hash).SDBMSys)
else
  raise ESDBMIncompatibleClass.CreateFmt('TSDBMHash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSDBMHash.AsString: String;
begin
Result := SDBMAsString(fSDBMValue);
end;

//------------------------------------------------------------------------------

procedure TSDBMHash.FromString(const Str: String);
begin
fSDBMValue := SDBMFromString(Str);
end;

//------------------------------------------------------------------------------

procedure TSDBMHash.FromStringDef(const Str: String; const Default: TSDBM);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSDBMValue := SDBMToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TSDBMHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSDBM;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}SDBMToBE{$ELSE}SDBMToLE{$ENDIF}(SDBMFromSys(fSDBMValue));
  heLittle: Temp := SDBMToLE(SDBMFromSys(fSDBMValue));
  heBig:    Temp := SDBMToBE(SDBMFromSys(fSDBMValue));
else
 {heDefault}
  Temp := SDBMFromSys(fSDBMValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TSDBM));
end;

//------------------------------------------------------------------------------

procedure TSDBMHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSDBM;
begin
Temp := ZeroSDBM;
Stream.ReadBuffer(Temp,SizeOf(TSDBM));
case Endianness of
  heSystem: fSDBMValue := SDBMToSys({$IFDEF ENDIAN_BIG}SDBMFromBE{$ELSE}SDBMFromLE{$ENDIF}(Temp));
  heLittle: fSDBMValue := SDBMToSys(SDBMFromLE(Temp));
  heBig:    fSDBMValue := SDBMToSys(SDBMFromBE(Temp));
else
 {heDefault}
  fSDBMValue := SDBMToSys(Temp);
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

Function SDBMToStr(Hash: TSDBM): String;
begin
Result := SDBMAsString(TSDBMHash.SDBMToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToSDBM(const Str: String): TSDBM;
begin
Result := TSDBMHash.SDBMFromSys(SDBMFromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToSDBM(const Str: String; out Hash: TSDBM): Boolean;
begin
try
  Hash := TSDBMHash.SDBMFromSys(SDBMFromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToSDBMDef(const Str: String; Default: TSDBM): TSDBM;
begin
If not TryStrToSDBM(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareSDBM(A,B: TSDBM): Integer;
begin
Result := SDBMCompare(TSDBMHash.SDBMToSys(A),TSDBMHash.SDBMToSys(B));
end;

//------------------------------------------------------------------------------

Function SameSDBM(A,B: TSDBM): Boolean;
begin
Result := SDBMSame(TSDBMHash.SDBMToSys(A),TSDBMHash.SDBMToSys(B));
end;

{-------------------------------------------------------------------------------
    Procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferSDBM(Hash: TSDBM; const Buffer; Size: TMemSize): TSDBM;
begin
Result := TSDBMHash.SDBMFromSys(SDBMProcess(TSDBMHash.SDBMToSys(Hash),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferSDBM(const Buffer; Size: TMemSize): TSDBM;
begin
Result := TSDBMHash.SDBMFromSys(SDBMProcess(TSDBMHash.SDBMToSys(InitialSDBM),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringSDBM(const Str: AnsiString): TSDBM;
begin
Result := TSDBMHash.SDBMFromSys(SDBMProcess(TSDBMHash.SDBMToSys(InitialSDBM),PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar)));
end;

//------------------------------------------------------------------------------

Function WideStringSDBM(const Str: WideString): TSDBM;
begin
Result := TSDBMHash.SDBMFromSys(SDBMProcess(TSDBMHash.SDBMToSys(InitialSDBM),PWideChar(Str)^,Length(Str) * SizeOf(WideChar)));
end;

//------------------------------------------------------------------------------

Function StringSDBM(const Str: String): TSDBM;
begin
Result := TSDBMHash.SDBMFromSys(SDBMProcess(TSDBMHash.SDBMToSys(InitialSDBM),PChar(Str)^,Length(Str) * SizeOf(Char)));
end;

//------------------------------------------------------------------------------

Function StreamSDBM(Stream: TStream; Count: Int64 = -1): TSDBM;
var
  Hasher: TSDBMHash;
begin
Hasher := TSDBMHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.SDBM;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSDBM(const FileName: String): TSDBM;
var
  Hasher: TSDBMHash;
begin
Hasher := TSDBMHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.SDBM;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - context functions
-------------------------------------------------------------------------------}

Function SDBM_Init: TSDBMContext;
begin
TSDBMSys(Result) := TSDBMHash.SDBMToSys(InitialSDBM);
end;

//------------------------------------------------------------------------------

procedure SDBM_Update(var Context: TSDBMContext; const Buffer; Size: TMemSize);
begin
Context := TSDBMContext(SDBMProcess(TSDBMSys(Context),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function SDBM_Final(var Context: TSDBMContext; const Buffer; Size: TMemSize): TSDBM;
begin
SDBM_Update(Context,Buffer,Size);
Result := SDBM_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SDBM_Final(var Context: TSDBMContext): TSDBM;
begin
Result := TSDBMHash.SDBMFromSys(TSDBMSys(Context));
TSDBMSys(Context) := TSDBMHash.SDBMToSys(ZeroSDBM);
end;

//------------------------------------------------------------------------------

Function SDBM_Hash(const Buffer; Size: TMemSize): TSDBM;
begin
Result := TSDBMHash.SDBMFromSys(SDBMProcess(TSDBMHash.SDBMToSys(InitialSDBM),Buffer,Size));
end;

end.
