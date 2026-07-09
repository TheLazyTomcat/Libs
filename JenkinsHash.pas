{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Jenkins one-at-a-time (JOAAT) Hash

    Algorithm author: Robert John Jenkins

  Version 1.0 (2026-07-07)

  Last change 2026-07-08

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.JenkinsHash

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
unit JenkinsHash;

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

interface

uses
  AuxTypes, HashBase,
  Classes;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EJHException = class(EHashException);

  EJHIncompatibleClass = class(EJHException);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TJenkins are, in memory, always ordered from least significant
  byte to most significant byte (little endian).

  Type TJenkinsSys has no such guarantee and its endianness is system and
  implementation dependent.

  To convert the hash from default ordering to a required specific ordering,
  use method JenkinsToLE for little endian and JenkinsToBE for big endian.
}
type
  TJenkins = packed array[0..3] of UInt8;
  PJenkins = ^TJenkins;

  TJenkinsSys = UInt32;
  PJenkinsSys = ^TJenkinsSys;

const
  InitialJenkins: TJenkins = ($00,$00,$00,$00);

  ZeroJenkins: TJenkins = (0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                  TJenkinsHash                                  
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TJenkinsHash - class declaration
===============================================================================}
type
  TJenkinsHash = class(TStreamHash)
  protected
    fJenkinsValue: TJenkinsSys;
    Function GetJenkins: TJenkins; virtual;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function JenkinsToSys(Hash: TJenkins): TJenkinsSys; virtual;
    class Function JenkinsFromSys(Hash: TJenkinsSys): TJenkins; virtual;
    class Function JenkinsToLE(Hash: TJenkins): TJenkins; virtual;
    class Function JenkinsToBE(Hash: TJenkins): TJenkins; virtual;
    class Function JenkinsFromLE(Hash: TJenkins): TJenkins; virtual;
    class Function JenkinsFromBE(Hash: TJenkins): TJenkins; virtual;
    class Function HashType: THashType; override;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TJenkins); overload; virtual;
    procedure Init; override;
    procedure Final; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TJenkins); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Jenkins: TJenkins read GetJenkins;
    property JenkinsSys: TJenkinsSys read fJenkinsValue;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - declaration
===============================================================================}

Function JenkinsToStr(const Hash: TJenkins): String;
Function StrToJenkins(const Str: String): TJenkins;
Function TryStrToJenkins(const Str: String; out Hash: TJenkins): Boolean;
Function StrToJenkinsDef(const Str: String; Default: TJenkins): TJenkins;

Function CompareJenkins(const A,B: TJenkins): Integer;
Function SameJenkins(const A,B: TJenkins): Boolean;

//------------------------------------------------------------------------------

Function BufferJenkins(const Hash: TJenkins; const Buffer; Size: TMemSize): TJenkins; overload;
Function LastBufferJenkins(const Hash: TJenkins; const Buffer; Size: TMemSize): TJenkins;

Function BufferJenkins(const Buffer; Size: TMemSize): TJenkins; overload;

Function AnsiStringJenkins(const Str: AnsiString): TJenkins;
Function WideStringJenkins(const Str: WideString): TJenkins;
Function StringJenkins(const Str: String): TJenkins;

Function StreamJenkins(Stream: TStream; Count: Int64 = -1): TJenkins;
Function FileJenkins(const FileName: String): TJenkins;

//------------------------------------------------------------------------------
type
  TJenkinsContext = type TJenkinsSys;

Function Jenkins_Init: TJenkinsContext;
procedure Jenkins_Update(var Context: TJenkinsContext; const Buffer; Size: TMemSize);
Function Jenkins_Final(var Context: TJenkinsContext; const Buffer; Size: TMemSize): TJenkins; overload;
Function Jenkins_Final(var Context: TJenkinsContext): TJenkins; overload;
Function Jenkins_Hash(const Buffer; Size: TMemSize): TJenkins;

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
    Main implementation
===============================================================================}
{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function JenkinsProcess(const Jenkins: TJenkinsSys; const Buffer; Size: TMemSize): TJenkinsSys;
var
  CurrentData:  PUInt8;
  i:            TMemSize;
begin
Result := Jenkins;
If Size > 0 then
  begin
    CurrentData := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Result := Result + TJenkinsSys(CurrentData^);
        Result := Result + TJenkinsSys(Result shl 10);
        Result := Result xor (Result shr 6);
        Inc(CurrentData);
      end;
  end;
end;

//------------------------------------------------------------------------------

Function JenkinsFinal(const Jenkins: TJenkinsSys): TJenkinsSys;
begin
Result := Jenkins + TJenkinsSys(Jenkins shl 3);
Result := Result xor (Result shr 11);
Result := Result + TJenkinsSys(Result shl 15);
end;

{$IFDEF RangeChecks}{$R+}{$ENDIF}
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
//==============================================================================

Function JenkinsCompare(const A,B: TJenkinsSys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function JenkinsSame(const A,B: TJenkinsSys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function JenkinsAsString(const Jenkins: TJenkinsSys): String;
begin
Result := IntToHex(Jenkins,8);
end;

//------------------------------------------------------------------------------

Function JenkinsFromString(const Str: String): TJenkinsSys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TJenkinsSys(StrToInt(Str))
    else
      Result := TJenkinsSys(StrToInt('$' + Str));
  end
else Result := TJenkinsHash.JenkinsToSys(ZeroJenkins);
end;

{===============================================================================
    Auxiliary routines
===============================================================================}

Function SwapEndian(Hash: TJenkinsSys): TJenkinsSys; overload;
begin
Result := TJenkinsSys(((Hash and $000000FF) shl 24) or ((Hash and $0000FF00) shl 8) or
                   ((Hash and $00FF0000) shr 8) or ((Hash and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TJenkins): TJenkins; overload;
begin
Result := TJenkins(SwapEndian(TJenkinsSys(Hash)));
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TJenkinsHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TJenkinsHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TJenkinsHash - protected methods
-------------------------------------------------------------------------------}

Function TJenkinsHash.GetJenkins: TJenkins;
begin
Result := JenkinsFromSys(fJenkinsValue);
end;

//------------------------------------------------------------------------------

procedure TJenkinsHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fJenkinsValue := JenkinsProcess(fJenkinsValue,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TJenkinsHash.Initialize;
begin
inherited;
fJenkinsValue := JenkinsToSys(InitialJenkins);
end;

{-------------------------------------------------------------------------------
    TJenkinsHash - public methods
-------------------------------------------------------------------------------}

class Function TJenkinsHash.JenkinsToSys(Hash: TJenkins): TJenkinsSys;
begin
Result := TJenkinsSys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.JenkinsFromSys(Hash: TJenkinsSys): TJenkins;
begin
Result := TJenkins({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.JenkinsToLE(Hash: TJenkins): TJenkins;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.JenkinsToBE(Hash: TJenkins): TJenkins;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.JenkinsFromLE(Hash: TJenkins): TJenkins;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.JenkinsFromBE(Hash: TJenkins): TJenkins;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.HashType: THashType;
begin
Result := htHash;
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.HashSize: TMemSize;
begin
Result := SizeOf(TJenkins);
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

class Function TJenkinsHash.HashName: String;
begin
Result := 'Jenkins(JOAAT)';
end;

//------------------------------------------------------------------------------

constructor TJenkinsHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TJenkinsHash then
  fJenkinsValue := TJenkinsHash(Hash).JenkinsSys
else
  raise EJHIncompatibleClass.CreateFmt('TJenkinsHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TJenkinsHash.CreateAndInitFrom(Hash: TJenkins);
begin
CreateAndInit;
fJenkinsValue := JenkinsToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TJenkinsHash.Init;
begin
inherited;
fJenkinsValue := JenkinsToSys(InitialJenkins);
end;

//------------------------------------------------------------------------------

procedure TJenkinsHash.Final;
begin
fJenkinsValue := JenkinsFinal(fJenkinsValue);
inherited;
end;

//------------------------------------------------------------------------------

Function TJenkinsHash.Compare(Hash: THashBase): Integer;
begin
If Hash is TJenkinsHash then
  Result := JenkinsCompare(fJenkinsValue,TJenkinsHash(Hash).JenkinsSys)
else
  raise EJHIncompatibleClass.CreateFmt('TJenkinsHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TJenkinsHash.Same(Hash: THashBase): Boolean;
begin
If Hash is TJenkinsHash then
  Result := JenkinsSame(fJenkinsValue,TJenkinsHash(Hash).JenkinsSys)
else
  raise EJHIncompatibleClass.CreateFmt('TJenkinsHash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;


//------------------------------------------------------------------------------

Function TJenkinsHash.AsString: String;
begin
Result := JenkinsAsString(fJenkinsValue);
end;

//------------------------------------------------------------------------------

procedure TJenkinsHash.FromString(const Str: String);
begin
fJenkinsValue := JenkinsFromString(Str);
end;

//------------------------------------------------------------------------------

procedure TJenkinsHash.FromStringDef(const Str: String; const Default: TJenkins);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fJenkinsValue := JenkinsToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TJenkinsHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TJenkins;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}JenkinsToBE{$ELSE}JenkinsToLE{$ENDIF}(JenkinsFromSys(fJenkinsValue));
  heLittle: Temp := JenkinsToLE(JenkinsFromSys(fJenkinsValue));
  heBig:    Temp := JenkinsToBE(JenkinsFromSys(fJenkinsValue));
else
 {heDefault}
  Temp := JenkinsFromSys(fJenkinsValue);
end;
Stream.WriteBuffer(Temp,SizeOf(TJenkins));
end;

//------------------------------------------------------------------------------

procedure TJenkinsHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TJenkins;
begin
Temp := ZeroJenkins;
Stream.ReadBuffer(Temp,SizeOf(TJenkins));
case Endianness of
  heSystem: fJenkinsValue := JenkinsToSys({$IFDEF ENDIAN_BIG}JenkinsFromBE{$ELSE}JenkinsFromLE{$ENDIF}(Temp));
  heLittle: fJenkinsValue := JenkinsToSys(JenkinsFromLE(Temp));
  heBig:    fJenkinsValue := JenkinsToSys(JenkinsFromBE(Temp));
else
 {heDefault}
  fJenkinsValue := JenkinsToSys(Temp);
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

Function JenkinsToStr(const Hash: TJenkins): String;
begin
Result := JenkinsAsString(TJenkinsHash.JenkinsToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToJenkins(const Str: String): TJenkins;
begin
Result := TJenkinsHash.JenkinsFromSys(JenkinsFromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToJenkins(const Str: String; out Hash: TJenkins): Boolean;
begin
try
  Hash := TJenkinsHash.JenkinsFromSys(JenkinsFromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToJenkinsDef(const Str: String; Default: TJenkins): TJenkins;
begin
If not TryStrToJenkins(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareJenkins(const A,B: TJenkins): Integer;
begin
Result := JenkinsCompare(TJenkinsHash.JenkinsToSys(A),TJenkinsHash.JenkinsToSys(B));
end;

//------------------------------------------------------------------------------

Function SameJenkins(const A,B: TJenkins): Boolean;
begin
Result := JenkinsSame(TJenkinsHash.JenkinsToSys(A),TJenkinsHash.JenkinsToSys(B));
end;

{-------------------------------------------------------------------------------
    Procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferJenkins(const Hash: TJenkins; const Buffer; Size: TMemSize): TJenkins;
begin
Result := TJenkinsHash.JenkinsFromSys(JenkinsProcess(TJenkinsHash.JenkinsToSys(Hash),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function LastBufferJenkins(const Hash: TJenkins; const Buffer; Size: TMemSize): TJenkins;
begin
Result := TJenkinsHash.JenkinsFromSys(JenkinsFinal(JenkinsProcess(TJenkinsHash.JenkinsToSys(Hash),Buffer,Size)));
end;

//------------------------------------------------------------------------------

Function BufferJenkins(const Buffer; Size: TMemSize): TJenkins;
begin
Result := TJenkinsHash.JenkinsFromSys(JenkinsFinal(JenkinsProcess(TJenkinsHash.JenkinsToSys(InitialJenkins),Buffer,Size)));
end;

//------------------------------------------------------------------------------

Function AnsiStringJenkins(const Str: AnsiString): TJenkins;
begin
Result := BufferJenkins(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringJenkins(const Str: WideString): TJenkins;
begin
Result := BufferJenkins(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringJenkins(const Str: String): TJenkins;
begin
Result := BufferJenkins(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamJenkins(Stream: TStream; Count: Int64 = -1): TJenkins;
var
  Hasher: TJenkinsHash;
begin
Hasher := TJenkinsHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.Jenkins;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileJenkins(const FileName: String): TJenkins;
var
  Hasher: TJenkinsHash;
begin
Hasher := TJenkinsHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.Jenkins;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - context functions
-------------------------------------------------------------------------------}

Function Jenkins_Init: TJenkinsContext;
begin
TJenkinsSys(Result) := TJenkinsHash.JenkinsToSys(InitialJenkins);
end;

//------------------------------------------------------------------------------

procedure Jenkins_Update(var Context: TJenkinsContext; const Buffer; Size: TMemSize);
begin
Context := TJenkinsContext(JenkinsProcess(TJenkinsSys(Context),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function Jenkins_Final(var Context: TJenkinsContext; const Buffer; Size: TMemSize): TJenkins;
begin
Jenkins_Update(Context,Buffer,Size);
Result := Jenkins_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Jenkins_Final(var Context: TJenkinsContext): TJenkins;
begin
Result := TJenkinsHash.JenkinsFromSys(JenkinsFinal(TJenkinsSys(Context)));
TJenkinsSys(Context) := TJenkinsHash.JenkinsToSys(ZeroJenkins);
end;

//------------------------------------------------------------------------------

Function Jenkins_Hash(const Buffer; Size: TMemSize): TJenkins;
begin
Result := BufferJenkins(Buffer,Size);
end;

end.
