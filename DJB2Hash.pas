{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  DJB2 Hash (also known as Bernstein's hash)

    This simple library offers calculation of DJB2, which is rather old (1991)
    and very simple, yet still usefull, hash.

    Author of this algorithm is Daniel J. Bernstein, hence the name DJB.

    Along with the original form, this unit also provides alternative version
    that is internally using different operations (here reffered to as DJB2X).

  Version 1.1.1 (2026-07-04)

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

      github.com/TheLazyTomcat/Lib.DJB2Hash

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
unit DJB2Hash;

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
  EDJB2Exception = class(EHashException);

  EDJB2IncompatibleClass = class(EDJB2Exception);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TDJB2 are, in memory, always ordered from least significant byte to
  most significant byte (little endian).

  Type TDJB2Sys has no such guarantee and its endianness is system-dependent.

  To convert the checksum in default ordering to a required specific ordering,
  use method DJB2ToLE for little endian and DJB2ToBE for big endian. Note that
  these methods are expecting the input value to be in default ordering, if it
  is not, the result will be wrong. Be careful when using them.
}
type
  TDJB2 = packed array[0..3] of UInt8;
  PDJB2 = ^TDJB2;

  TDJB2Sys = UInt32;
  PDJB2Sys = ^TDJB2Sys;

const
  InitialDJB2: TDJB2 = ($05,$15,$00,$00); // 5381 in decimal

  ZeroDJB2: TDJB2 = (0,0,0,0);

{===============================================================================
--------------------------------------------------------------------------------
                                    TDJB2Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TDJB2Hash - class declaration
===============================================================================}
type
  TDJB2Hash = class(TStreamHash)
  protected
    fDJB2Value: TDJB2Sys;
    Function GetDJB2: TDJB2; virtual;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function DJB2ToSys(Hash: TDJB2): TDJB2Sys; virtual;
    class Function DJB2FromSys(Hash: TDJB2Sys): TDJB2; virtual;
    class Function DJB2ToLE(Hash: TDJB2): TDJB2; virtual;
    class Function DJB2ToBE(Hash: TDJB2): TDJB2; virtual;
    class Function DJB2FromLE(Hash: TDJB2): TDJB2; virtual;
    class Function DJB2FromBE(Hash: TDJB2): TDJB2; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TDJB2); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TDJB2); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property DJB2: TDJB2 read GetDJB2;
    property DJB2Sys: TDJB2Sys read fDJB2Value;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TDJB2XHash                                    
--------------------------------------------------------------------------------
===============================================================================}
{
  This class uses slightly different algorithm to calculate the hash - addition
  of currently processed byte is replaced by exclusive-or (XOR) operation (see
  source code).  
}
{===============================================================================
    TDJB2XHash - class declaration
===============================================================================}
type
  TDJB2XHash = class(TDJB2Hash)
  protected
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
  public
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{
  Most functions provided here are just wrappers around TDJB2Hash class and
  its methods, but functions performing calculations  data stored in memory
  (eg. StringDJB2) are calling direct implementation and are not instantiating
  TDJB2Hash objects.
}
{===============================================================================
    Procedural interface - declaration
===============================================================================}

Function DJB2ToStr(const Hash: TDJB2): String;
Function StrToDJB2(const Str: String): TDJB2;
Function TryStrToDJB2(const Str: String; out Hash: TDJB2): Boolean;
Function StrToDJB2Def(const Str: String; Default: TDJB2): TDJB2;

Function CompareDJB2(const A,B: TDJB2): Integer;
Function SameDJB2(const A,B: TDJB2): Boolean;

//------------------------------------------------------------------------------

Function BufferDJB2(const Hash: TDJB2; const Buffer; Size: TMemSize): TDJB2; overload;
Function BufferDJB2(const Buffer; Size: TMemSize): TDJB2; overload;

Function AnsiStringDJB2(const Str: AnsiString): TDJB2;
Function WideStringDJB2(const Str: WideString): TDJB2;
Function StringDJB2(const Str: String): TDJB2;

Function StreamDJB2(Stream: TStream; Count: Int64 = -1): TDJB2;
Function FileDJB2(const FileName: String): TDJB2;

//------------------------------------------------------------------------------
type
  TDJB2Context = type TDJB2Sys;

Function DJB2_Init: TDJB2Context;
procedure DJB2_Update(var Context: TDJB2Context; const Buffer; Size: TMemSize);
Function DJB2_Final(var Context: TDJB2Context; const Buffer; Size: TMemSize): TDJB2; overload;
Function DJB2_Final(var Context: TDJB2Context): TDJB2; overload;
Function DJB2_Hash(const Buffer; Size: TMemSize): TDJB2;

{===============================================================================
    Procedural interface (XOR variant) - declaration
===============================================================================}
{
  Following functions are using alternative algorithm (see class TDJB2XHash)
  for hash computation. Since the hash type itself does not differ, utility
  functions (eg. DJB2XToStr) are not provided because you can use the ones
  declared previously.
}

Function BufferDJB2X(const Hash: TDJB2; const Buffer; Size: TMemSize): TDJB2; overload;
Function BufferDJB2X(const Buffer; Size: TMemSize): TDJB2; overload;

Function AnsiStringDJB2X(const Str: AnsiString): TDJB2;
Function WideStringDJB2X(const Str: WideString): TDJB2;
Function StringDJB2X(const Str: String): TDJB2;

Function StreamDJB2X(Stream: TStream; Count: Int64 = -1): TDJB2;
Function FileDJB2X(const FileName: String): TDJB2;

//------------------------------------------------------------------------------

Function DJB2X_Init: TDJB2Context;
procedure DJB2X_Update(var Context: TDJB2Context; const Buffer; Size: TMemSize);
Function DJB2X_Final(var Context: TDJB2Context; const Buffer; Size: TMemSize): TDJB2; overload;
Function DJB2X_Final(var Context: TDJB2Context): TDJB2; overload;
Function DJB2X_Hash(const Buffer; Size: TMemSize): TDJB2;

implementation

uses
  SysUtils;

{===============================================================================
    Main implementation
===============================================================================}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

Function DJB2Process(const Hash: TDJB2Sys; const Buffer; Size: TMemSize): TDJB2Sys;
var
  WorkHash:     TDJB2Sys;
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
        // multiplication seems to be marginally faster on x86 cpu
        WorkHash := (WorkHash * TDJB2Sys(33)) + TDJB2Sys(CurrentData^);
      {$ELSE}
        WorkHash := TDJB2Sys(WorkHash shl 5) + WorkHash + TDJB2Sys(CurrentData^);
      {$ENDIF}
        Inc(CurrentData);
      end;
  end;
Result := WorkHash;
end;

//------------------------------------------------------------------------------

Function DJB2XProcess(const Hash: TDJB2Sys; const Buffer; Size: TMemSize): TDJB2Sys;
var
  WorkHash:     TDJB2Sys;
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
        WorkHash := (WorkHash * TDJB2Sys(33)) xor TDJB2Sys(CurrentData^);
      {$ELSE}
        WorkHash := (TDJB2Sys(WorkHash shl 5) + WorkHash) xor TDJB2Sys(CurrentData^);
      {$ENDIF}
        Inc(CurrentData);
      end;
  end;
Result := WorkHash;
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}
{$IFDEF RangeChecks}{$R+}{$ENDIF}

//------------------------------------------------------------------------------

Function DJB2Compare(const A,B: TDJB2Sys): Integer;
begin
If A > B then
  Result := +1
else If A < B then
  Result := -1
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function DJB2Same(const A,B: TDJB2Sys): Boolean;
begin
Result := A = B;
end;

//------------------------------------------------------------------------------

Function DJB2AsString(const DJB2: TDJB2Sys): String;
begin
Result := IntToHex(DJB2,8);
end;

//------------------------------------------------------------------------------

Function DJB2FromString(const Str: String): TDJB2Sys;
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      Result := TDJB2Sys(StrToInt(Str))
    else
      Result := TDJB2Sys(StrToInt('$' + Str));
  end
else Result := TDJB2Hash.DJB2ToSys(ZeroDJB2);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                    TDJB2Hash
--------------------------------------------------------------------------------
===============================================================================}

Function SwapEndian(Hash: TDJB2Sys): TDJB2Sys; overload;
begin
Result := TDJB2Sys(((Hash and $000000FF) shl 24) or ((Hash and $0000FF00) shl 8) or
                   ((Hash and $00FF0000) shr 8) or ((Hash and $FF000000) shr 24));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Hash: TDJB2): TDJB2; overload;
begin
Result := TDJB2(SwapEndian(TDJB2Sys(Hash)));
end;

{===============================================================================
    TDJB2Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TDJB2Hash - protected methods
-------------------------------------------------------------------------------}

Function TDJB2Hash.GetDJB2: TDJB2;
begin
Result := DJB2FromSys(fDJB2Value);
end;

//------------------------------------------------------------------------------

procedure TDJB2Hash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fDJB2Value := DJB2Process(fDJB2Value,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TDJB2Hash.Initialize;
begin
inherited;
fDJB2Value := DJB2ToSys(InitialDJB2);
end;

{-------------------------------------------------------------------------------
    TDJB2Hash - public methods
-------------------------------------------------------------------------------}

class Function TDJB2Hash.DJB2ToSys(Hash: TDJB2): TDJB2Sys;
begin
Result := TDJB2Sys({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TDJB2Hash.DJB2FromSys(Hash: TDJB2Sys): TDJB2;
begin
Result := TDJB2({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(Hash));
end;

//------------------------------------------------------------------------------

class Function TDJB2Hash.DJB2ToLE(Hash: TDJB2): TDJB2;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TDJB2Hash.DJB2ToBE(Hash: TDJB2): TDJB2;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TDJB2Hash.DJB2FromLE(Hash: TDJB2): TDJB2;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TDJB2Hash.DJB2FromBE(Hash: TDJB2): TDJB2;
begin
Result := SwapEndian(Hash);
end;

//------------------------------------------------------------------------------

class Function TDJB2Hash.HashSize: TMemSize;
begin
Result := SizeOf(TDJB2);
end;

//------------------------------------------------------------------------------

class Function TDJB2Hash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TDJB2Hash.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

class Function TDJB2Hash.HashName: String;
begin
Result := 'DJB2';
end;

//------------------------------------------------------------------------------

constructor TDJB2Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TDJB2Hash then
  fDJB2Value := TDJB2Hash(Hash).DJB2Sys
else
  raise EDJB2IncompatibleClass.CreateFmt('TDJB2Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TDJB2Hash.CreateAndInitFrom(Hash: TDJB2);
begin
CreateAndInit;
fDJB2Value := DJB2ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TDJB2Hash.Init;
begin
inherited;
fDJB2Value := DJB2ToSys(InitialDJB2);
end;

//------------------------------------------------------------------------------

Function TDJB2Hash.Compare(Hash: THashBase): Integer;
begin
If Hash is TDJB2Hash then
  Result := DJB2Compare(fDJB2Value,TDJB2Hash(Hash).DJB2Sys)
else
  raise EDJB2IncompatibleClass.CreateFmt('TDJB2Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TDJB2Hash.Same(Hash: THashBase): Boolean;
begin
If Hash is TDJB2Hash then
  Result := DJB2Same(fDJB2Value,TDJB2Hash(Hash).DJB2Sys)
else
  raise EDJB2IncompatibleClass.CreateFmt('TDJB2Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;


//------------------------------------------------------------------------------

Function TDJB2Hash.AsString: String;
begin
Result := DJB2AsString(fDJB2Value);
end;

//------------------------------------------------------------------------------

procedure TDJB2Hash.FromString(const Str: String);
begin
fDJB2Value := DJB2FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TDJB2Hash.FromStringDef(const Str: String; const Default: TDJB2);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fDJB2Value := DJB2ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TDJB2Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TDJB2;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}DJB2ToBE{$ELSE}DJB2ToLE{$ENDIF}(DJB2FromSys(fDJB2Value));
  heLittle: Temp := DJB2ToLE(DJB2FromSys(fDJB2Value));
  heBig:    Temp := DJB2ToBE(DJB2FromSys(fDJB2Value));
else
 {heDefault}
  Temp := DJB2FromSys(fDJB2Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TDJB2));
end;

//------------------------------------------------------------------------------

procedure TDJB2Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TDJB2;
begin
Temp := ZeroDJB2;
Stream.ReadBuffer(Temp,SizeOf(TDJB2));
case Endianness of
  heSystem: fDJB2Value := DJB2ToSys({$IFDEF ENDIAN_BIG}DJB2FromBE{$ELSE}DJB2FromLE{$ENDIF}(Temp));
  heLittle: fDJB2Value := DJB2ToSys(DJB2FromLE(Temp));
  heBig:    fDJB2Value := DJB2ToSys(DJB2FromBE(Temp));
else
 {heDefault}
  fDJB2Value := DJB2ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TDJB2XHash                                    
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TDJB2XHash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TDJB2XHash - protected methods
-------------------------------------------------------------------------------}

procedure TDJB2XHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fDJB2Value := DJB2XProcess(fDJB2Value,Buffer,Size);
end;

{-------------------------------------------------------------------------------
    TDJB2XHash - public methods
-------------------------------------------------------------------------------}

class Function TDJB2XHash.HashName: String;
begin
Result := 'DJB2X';
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

Function DJB2ToStr(const Hash: TDJB2): String;
begin
Result := DJB2AsString(TDJB2Hash.DJB2ToSys(Hash));
end;

//------------------------------------------------------------------------------

Function StrToDJB2(const Str: String): TDJB2;
begin
Result := TDJB2Hash.DJB2FromSys(DJB2FromString(Str));
end;

//------------------------------------------------------------------------------

Function TryStrToDJB2(const Str: String; out Hash: TDJB2): Boolean;
begin
try
  Hash := TDJB2Hash.DJB2FromSys(DJB2FromString(Str));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToDJB2Def(const Str: String; Default: TDJB2): TDJB2;
begin
If not TryStrToDJB2(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareDJB2(const A,B: TDJB2): Integer;
begin
Result := DJB2Compare(TDJB2Hash.DJB2ToSys(A),TDJB2Hash.DJB2ToSys(B));
end;

//------------------------------------------------------------------------------

Function SameDJB2(const A,B: TDJB2): Boolean;
begin
Result := DJB2Same(TDJB2Hash.DJB2ToSys(A),TDJB2Hash.DJB2ToSys(B));
end;

{-------------------------------------------------------------------------------
    Procedural interface - processing functions
-------------------------------------------------------------------------------}

Function BufferDJB2(const Hash: TDJB2; const Buffer; Size: TMemSize): TDJB2;
begin
Result := TDJB2Hash.DJB2FromSys(DJB2Process(TDJB2Hash.DJB2ToSys(Hash),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferDJB2(const Buffer; Size: TMemSize): TDJB2;
begin
Result := TDJB2Hash.DJB2FromSys(DJB2Process(TDJB2Hash.DJB2ToSys(InitialDJB2),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringDJB2(const Str: AnsiString): TDJB2;
begin
Result := BufferDJB2(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringDJB2(const Str: WideString): TDJB2;
begin
Result := BufferDJB2(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringDJB2(const Str: String): TDJB2;
begin
Result := BufferDJB2(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamDJB2(Stream: TStream; Count: Int64 = -1): TDJB2;
var
  Hasher: TDJB2Hash;
begin
Hasher := TDJB2Hash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.DJB2;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileDJB2(const FileName: String): TDJB2;
var
  Hasher: TDJB2Hash;
begin
Hasher := TDJB2Hash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.DJB2;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - context functions
-------------------------------------------------------------------------------}

Function DJB2_Init: TDJB2Context;
begin
TDJB2Sys(Result) := TDJB2Hash.DJB2ToSys(InitialDJB2);
end;

//------------------------------------------------------------------------------

procedure DJB2_Update(var Context: TDJB2Context; const Buffer; Size: TMemSize);
begin
Context := TDJB2Context(DJB2Process(TDJB2Sys(Context),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function DJB2_Final(var Context: TDJB2Context; const Buffer; Size: TMemSize): TDJB2;
begin
DJB2_Update(Context,Buffer,Size);
Result := DJB2_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DJB2_Final(var Context: TDJB2Context): TDJB2;
begin
Result := TDJB2Hash.DJB2FromSys(TDJB2Sys(Context));
TDJB2Sys(Context) := TDJB2Hash.DJB2ToSys(ZeroDJB2);
end;

//------------------------------------------------------------------------------

Function DJB2_Hash(const Buffer; Size: TMemSize): TDJB2;
begin
Result := BufferDJB2(Buffer,Size);
end;

{===============================================================================
    Procedural interface (XOR variant) - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Procedural interface (XOR variant) - processing functions
-------------------------------------------------------------------------------}

Function BufferDJB2X(const Hash: TDJB2; const Buffer; Size: TMemSize): TDJB2;
begin
Result := TDJB2Hash.DJB2FromSys(DJB2XProcess(TDJB2Hash.DJB2ToSys(Hash),Buffer,Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferDJB2X(const Buffer; Size: TMemSize): TDJB2;
begin
Result := TDJB2Hash.DJB2FromSys(DJB2XProcess(TDJB2Hash.DJB2ToSys(InitialDJB2),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function AnsiStringDJB2X(const Str: AnsiString): TDJB2;
begin
Result := BufferDJB2X(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function WideStringDJB2X(const Str: WideString): TDJB2;
begin
Result := BufferDJB2X(PWideChar(Str)^,Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StringDJB2X(const Str: String): TDJB2;
begin
Result := BufferDJB2X(PChar(Str)^,Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function StreamDJB2X(Stream: TStream; Count: Int64 = -1): TDJB2;
var
  Hasher: TDJB2XHash;
begin
Hasher := TDJB2XHash.Create;
try
  Hasher.HashStream(Stream,Count);
  Result := Hasher.DJB2;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileDJB2X(const FileName: String): TDJB2;
var
  Hasher: TDJB2XHash;
begin
Hasher := TDJB2XHash.Create;
try
  Hasher.HashFile(FileName);
  Result := Hasher.DJB2;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface (XOR variant) - context functions
-------------------------------------------------------------------------------}

Function DJB2X_Init: TDJB2Context;
begin
TDJB2Sys(Result) := TDJB2Hash.DJB2ToSys(InitialDJB2);
end;

//------------------------------------------------------------------------------

procedure DJB2X_Update(var Context: TDJB2Context; const Buffer; Size: TMemSize);
begin
Context := TDJB2Context(DJB2XProcess(TDJB2Sys(Context),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function DJB2X_Final(var Context: TDJB2Context; const Buffer; Size: TMemSize): TDJB2;
begin
DJB2X_Update(Context,Buffer,Size);
Result := DJB2_Final(Context);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function DJB2X_Final(var Context: TDJB2Context): TDJB2;
begin
Result := TDJB2Hash.DJB2FromSys(TDJB2Sys(Context));
TDJB2Sys(Context) := TDJB2Hash.DJB2ToSys(ZeroDJB2);
end;

//------------------------------------------------------------------------------

Function DJB2X_Hash(const Buffer; Size: TMemSize): TDJB2;
begin
Result := BufferDJB2X(Buffer,Size);
end;

end.
