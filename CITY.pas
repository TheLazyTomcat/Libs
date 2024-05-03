{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CITY hash calculation - main unit

    This library is only a naive reimplementation of reference code that can be
    found in this repository:

      https://github.com/google/cityhash

    Because individual historical versions can produce different hashes for the
    same message, all versions were implemented, each in its own unit (for
    example version 1.0.3 of the hash is implemented in unit CITY_1_0_3.pas).
    This allows you to choose which version to use, if you happen to need a
    specific older implementation.

    Currently implemented versions are:

        1.0.0 ... in CITY_1_0_0.pas
        1.0.1 ... in CITY_1_0_1.pas, CITY_1_0_1_Test.pas
        1.0.2 ... in CITY_1_0_2.pas, CITY_1_0_2_Test.pas
        1.0.3 ... in CITY_1_0_3.pas, CITY_1_0_3_Test.pas
        1.1.0 ... in CITY_1_1_0.pas, CITY_1_1_0_Test.pas
        1.1.1 ... in CITY_1_1_1.pas, CITY_1_1_1_Test.pas

    Functions provided in this unit are only redirecting to latest version,
    which is currently 1.1.1.

    WARNING - version of this library does not correlate with version of
              implemented and used version of the CITY hash!

  Version 2.1.1 (2023-04-15)

  Last change 2024-04-28

  ©2016-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.CityHash

  Dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    BasicUIM    - github.com/TheLazyTomcat/Lib.BasicUIM
    BitOps      - github.com/TheLazyTomcat/Lib.BitOps
    HashBase    - github.com/TheLazyTomcat/Lib.HashBase
  * SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils

  SimpleCPUID is required only when PurePascal symbol is not defined.

  Library SimpleCPUID might also be required as an indirect dependency.

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit CITY;

{$INCLUDE 'CITY_defs.inc'}

interface

uses
  Classes,
  AuxTypes, HashBase,
  CITY_Common,
  CITY_1_0_0,  
  CITY_1_0_1,
  CITY_1_0_2,
  CITY_1_0_3,
  CITY_1_1_0,
  CITY_1_1_1;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ECITYIncompatibleClass = class(ECITYException);

  ECITYUnsupportedVersion = class(ECITYException);
  ECITYUnsupportedVariant = class(ECITYException);

  ECITYInvalidImplementation = class(ECITYException);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in TCITY* types are always ordered from least significant byte to most
  significant byte (little endian).

  Types TCITY*Sys have no such guarantee and their endianness is
  system-dependent.

  To convert the checksum in default ordering to a required specific ordering,
  use methods CITY*ToLE for little endian and CITY*ToBE for big endian.
  Note that these methods are expecting the input value to be in default
  ordering, if it is not, the result will be wrong. Be carefull when using them.
}
type
  TCITY32 = array[0..3] of UInt8;
  PCITY32 = ^TCITY32;

  TCITY64 = array[0..7] of UInt8;
  PCITY64 = ^TCITY64;

  TCITY128 = array[0..15] of UInt8;
  PCITY128 = ^TCITY128;

  TCITY256 = array[0..31] of UInt8;
  PCITY256 = ^TCITY256;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type
  TCITY32Sys = UInt32;
  PCITY32Sys = ^TCITY32Sys;

  TCITY64Sys = UInt64;
  PCITY64Sys = ^TCITY64Sys;

  TCITY128Sys = UInt128;
  PCITY128Sys = ^TCITY128Sys;
  
  TCITY256Sys = UInt256;
  PCITY256Sys = ^TCITY256Sys;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type
{
  TCITYVersion can be used to select which version of the city hash to use for
  calculation - it is here because different historical versions can and will
  produce different results.

  verDefault corresponds to verCITY111 - this will, unlike in case of verLatest,
  never change in the future.
  varLatest will always point to the latest implemented version, and therefore
  might change in the future.

    WARNING - not all widths (32bit, 64bit, 128bit, ...) of the city hash
              were implemented from the first version!
}
  TCITYVersion = (verDefault,verLatest,verCITY100,verCITY101,verCITY102,
                  verCITY103,verCITY110,verCITY111);

  TCITYVersions = set of TCITYVersion;

{
  TCITYVariant can be used to select which variant (with no seed, one seed or
  two seeds) of the hash to use for calculation.

    WARNING - not all varians are supported by different widths and versions
              of the city hash.
}
  TCITYVariant = (varPlain,varSeed,varSeeds);

  TCITYVariants = set of TCITYVariant;

{===============================================================================
--------------------------------------------------------------------------------
                                   TCityHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCityHash - class declaration
===============================================================================}
type
  TCityHash = class(TBufferHash)
  protected
    fCityVersion: TCITYVersion;
    fCityVariant: TCITYVariant;
    procedure SetCityVersion(Value: TCITYVersion); virtual;
    procedure SetCityVariant(Value: TCITYVariant); virtual;
    procedure Initialize; override;
    procedure CheckVersionAndVariant; virtual;
  public
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    class Function CityVersionsSupported: TCITYVersions; virtual; abstract;
    class Function CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants; virtual; abstract;
    constructor CreateAndInitFrom(Hash: THashBase); override;
    property CityVersion: TCITYVersion read fCityVersion write SetCityVersion;
    property CityVariant: TCityVariant read fCityVariant write SetCityVariant;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TCity32Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity32Hash - class declaration
===============================================================================}
type
  TCity32Hash = class(TCityHash)
  protected
    fCity32:  TCity32Sys;
    Function GetCity32: TCity32; virtual;
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
    procedure Initialize; override;
  public
    class Function CityVersionsSupported: TCITYVersions; override;
    class Function CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants; override;
    class Function City32ToSys(City32: TCity32): TCity32Sys; virtual;
    class Function City32FromSys(City32: TCity32Sys): TCity32; virtual;
    class Function City32ToLE(City32: TCity32): TCity32; virtual;
    class Function City32ToBE(City32: TCity32): TCity32; virtual;
    class Function City32FromLE(City32: TCity32): TCity32; virtual;
    class Function City32FromBE(City32: TCity32): TCity32; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TCity32); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TCity32); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property City32: TCity32 read GetCity32;
    property City32Sys: TCity32Sys read fCity32;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TCity64Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity64Hash - class declaration
===============================================================================}
type
  TCity64Hash = class(TCityHash)
  protected
    fCity64:  TCity64Sys;
    fSeed0:   UInt64;
    fSeed1:   UInt64;
    Function GetCity64: TCity64; virtual;
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
    procedure Initialize; override;
  public
    class Function CityVersionsSupported: TCITYVersions; override;
    class Function CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants; override;
    class Function City64ToSys(City64: TCity64): TCity64Sys; virtual;
    class Function City64FromSys(City64: TCity64Sys): TCity64; virtual;
    class Function City64ToLE(City64: TCity64): TCity64; virtual;
    class Function City64ToBE(City64: TCity64): TCity64; virtual;
    class Function City64FromLE(City64: TCity64): TCity64; virtual;
    class Function City64FromBE(City64: TCity64): TCity64; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TCity64); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TCity64); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property City64: TCity64 read GetCity64;
    property City64Sys: TCity64Sys read fCity64;
    property Seed: UInt64 read fSeed0 write fSeed0;
    property Seed0: UInt64 read fSeed0 write fSeed0;
    property Seed1: UInt64 read fSeed1 write fSeed1;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TCity128HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity128HashBase - class declaration
===============================================================================}
type
  TCity128HashBase = class(TCityHash)
  protected
    fCity128: TCity128Sys;
    fSeed:    UInt128;
    Function GetCity128: TCity128; virtual;
    procedure Initialize; override;
  public
    class Function City128ToSys(City128: TCity128): TCity128Sys; virtual;
    class Function City128FromSys(City128: TCity128Sys): TCity128; virtual;
    class Function City128ToLE(City128: TCity128): TCity128; virtual;
    class Function City128ToBE(City128: TCity128): TCity128; virtual;
    class Function City128FromLE(City128: TCity128): TCity128; virtual;
    class Function City128FromBE(City128: TCity128): TCity128; virtual;
    class Function HashSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TCity128); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TCity128); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property City128: TCity128 read GetCity128;
    property City128Sys: TCity128Sys read fCity128;
    property Seed: UInt128 read fSeed write fSeed;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TCity128Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity128Hash - class declaration
===============================================================================}
type
  TCity128Hash = class(TCity128HashBase)
  protected
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
  public
    class Function CityVersionsSupported: TCITYVersions; override;
    class Function CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants; override;
    class Function HashName: String; override;    
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TCityCRC128Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCityCRC128Hash - class declaration
===============================================================================}
type
  TCityCRC128Hash = class(TCity128HashBase)
  protected
    Function GetHashImplementation: THashImplementation; override;
    procedure SetHashImplementation(Value: THashImplementation); override;
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
  public
    class Function CityVersionsSupported: TCITYVersions; override;
    class Function CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants; override;
    class Function HashImplementationsAvailable: THashImplementations; override;
    class Function HashImplementationsSupported: THashImplementations; override;
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TCity256HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity256HashBase - class declaration
===============================================================================}
type
  TCity256HashBase = class(TCityHash)
  protected
    fCity256: TCity256Sys;
    Function GetCity256: TCity256; virtual;
    procedure Initialize; override;
  public
    class Function City256ToSys(City256: TCity256): TCity256Sys; virtual;
    class Function City256FromSys(City256: TCity256Sys): TCity256; virtual;
    class Function City256ToLE(City256: TCity256): TCity256; virtual;
    class Function City256ToBE(City256: TCity256): TCity256; virtual;
    class Function City256FromLE(City256: TCity256): TCity256; virtual;
    class Function City256FromBE(City256: TCity256): TCity256; virtual;
    class Function HashSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TCity256); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TCity256); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property City256: TCity256 read GetCity256;
    property City256Sys: TCity256Sys read fCity256;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TCityCRC256Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCityCRC256Hash - class declaration
===============================================================================}
type
  TCityCRC256Hash = class(TCity256HashBase)
  protected
    Function GetHashImplementation: THashImplementation; override;
    procedure SetHashImplementation(Value: THashImplementation); override;
    procedure CalculateHash(Memory: Pointer; Count: TMemSize); override;
  public
    class Function CityVersionsSupported: TCITYVersions; override;
    class Function CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants; override;
    class Function HashImplementationsAvailable: THashImplementations; override;
    class Function HashImplementationsSupported: THashImplementations; override;
    class Function HashName: String; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                        Backward compatibility functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Utility functions - declaration
===============================================================================}

const
  CITY_VersionMajor   = 1;
  CITY_VersionMinor   = 1;
  CITY_VersionRelease = 1;
  CITY_VersionFull    = UInt64($0001000100010000);
  CITY_VersionStr     = '1.1.1';

Function Hash128to64(x: UInt128): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
    Main hash functions - declaration
===============================================================================}

Function CityHash32(s: Pointer; len: TMemSize): UInt32;{$IFDEF CanInline} inline;{$ENDIF}

Function CityHash64(s: Pointer; len: TMemSize): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

Function CityHash64WithSeed(s: Pointer; len: TMemSize; seed: UInt64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}
Function CityHash64WithSeeds(s: Pointer; len: TMemSize; seed0,seed1: UInt64): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

Function CityHash128(s: Pointer; len: TMemSize): UInt128;{$IFDEF CanInline} inline;{$ENDIF}
Function CityHash128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
    CRC hash functions - declaration
===============================================================================}

procedure CityHashCrc256(s: Pointer; len: TMemSize; out result: UInt256);{$IFDEF CanInline} inline;{$ENDIF}

Function CityHashCrc128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;{$IFDEF CanInline} inline;{$ENDIF}
Function CityHashCrc128(s: Pointer; len: TMemSize): UInt128;{$IFDEF CanInline} inline;{$ENDIF}

implementation

uses
  SysUtils, Math,
  UInt64Utils;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5057:={$WARN 5057 OFF}} // Local variable "$1" does not seem to be initialized
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TCityHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCityHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCityHash - protected methods
-------------------------------------------------------------------------------}

procedure TCityHash.SetCityVersion(Value: TCITYVersion);
begin
If Value in CityVersionsSupported then
  fCityVersion := Value
else
  raise ECITYUnsupportedVersion.CreateFmt('TCityHash.SetCityVersion: Unsupported version (%d).',[Ord(Value)]);
end;

//------------------------------------------------------------------------------

procedure TCityHash.SetCityVariant(Value: TCITYVariant);
begin
If Value in CityVariantsSupported(fCityVersion) then
  fCityVariant := Value
else
  raise ECITYUnsupportedVariant.CreateFmt('TCityHash.SetCityVariant: Unsupported variant (%d).',[Ord(Value)]);
end;

//------------------------------------------------------------------------------

procedure TCityHash.Initialize;
begin
inherited;
fCityVersion := verDefault;
fCityVariant := varPlain;
end;

//------------------------------------------------------------------------------

procedure TCityHash.CheckVersionAndVariant;
begin
If not(fCityVersion in CityVersionsSupported) then
  raise ECITYUnsupportedVersion.CreateFmt('%s.CheckVersionAndVariant: Unsupported version (%d).',
    [Self.ClassName,Ord(fCityVersion)]);
If not(fCityVariant in CityVariantsSupported(fCityVersion)) then
  raise ECITYUnsupportedVariant.CreateFmt('%s.CheckVersionAndVariant: Unsupported variant (%d) for version %d.',
    [Self.ClassName,Ord(fCityVariant),Ord(fCityVersion)]);   
end;

{-------------------------------------------------------------------------------
    TCityHash - public methods
-------------------------------------------------------------------------------}

class Function TCityHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TCityHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TCityHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCityHash then
  begin
    CityVersion := TCityHash(Hash).CityVersion;
    CityVariant := TCityHash(Hash).CityVariant;
  end
else raise ECITYIncompatibleClass.CreateFmt('TCityHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TCity32Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity32Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCity32Hash - utility functions
-------------------------------------------------------------------------------}

Function SwapEndian(Value: TCITY32Sys): TCITY32Sys; overload;
begin
Result := TCITY32Sys(EndianSwap(UInt32(Value)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TCITY32): TCITY32; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := TCITY32(SwapEndian(TCITY32Sys(Value)));
end;

{-------------------------------------------------------------------------------
    TCity32Hash - protected methods
-------------------------------------------------------------------------------}

Function TCity32Hash.GetCity32: TCity32;
begin
Result := City32FromSys(fCity32);
end;

//------------------------------------------------------------------------------

procedure TCity32Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
CheckVersionAndVariant;
case fCityVersion of
  verCITY110: fCity32 := CITY_1_1_0.CityHash32(Memory,Count);
  verDefault,
  verLatest,
  verCITY111: fCity32 := CITY_1_1_1.CityHash32(Memory,Count);
end;
end;

//------------------------------------------------------------------------------

procedure TCity32Hash.Initialize;
begin
inherited;
fCity32 := 0;
end;

{-------------------------------------------------------------------------------
    TCity32Hash - public methods
-------------------------------------------------------------------------------}

class Function TCity32Hash.CityVersionsSupported: TCITYVersions;
begin
Result := [verCITY110,verDefault,verLatest,verCITY111];
end;

//------------------------------------------------------------------------------

class Function TCity32Hash.CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants;
begin
case CityVersion of
  verDefault,
  verLatest,
  verCITY110,
  verCITY111: Result := [varPlain];
else
  Result := [];
end;
end;

//------------------------------------------------------------------------------

class Function TCity32Hash.City32ToSys(City32: TCity32): TCity32Sys;
begin
Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TCity32Sys(City32));
end;

//------------------------------------------------------------------------------

class Function TCity32Hash.City32FromSys(City32: TCity32Sys): TCity32;
begin
Result := TCity32({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(City32));
end;

//------------------------------------------------------------------------------

class Function TCity32Hash.City32ToLE(City32: TCity32): TCity32;
begin
Result := City32;
end;

//------------------------------------------------------------------------------

class Function TCity32Hash.City32ToBE(City32: TCity32): TCity32;
begin
Result := SwapEndian(City32);
end;

//------------------------------------------------------------------------------

class Function TCity32Hash.City32FromLE(City32: TCity32): TCity32;
begin
Result := City32;
end;
 
//------------------------------------------------------------------------------

class Function TCity32Hash.City32FromBE(City32: TCity32): TCity32;
begin
Result := SwapEndian(City32);
end;

//------------------------------------------------------------------------------

class Function TCity32Hash.HashSize: TMemSize;
begin
Result := SizeOf(TCITY32);
end;

//------------------------------------------------------------------------------

class Function TCity32Hash.HashName: String;
begin
Result := 'CITY-32';
end;

//------------------------------------------------------------------------------

constructor TCity32Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCity32Hash then
  fCity32 := TCity32Hash(Hash).City32Sys
else
  raise ECITYIncompatibleClass.CreateFmt('TCity32Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TCity32Hash.CreateAndInitFrom(Hash: TCity32);
begin
CreateAndInit;
fCity32 := City32ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TCity32Hash.Init;
begin
inherited;
fCity32 := 0;
end;

//------------------------------------------------------------------------------

Function TCity32Hash.Compare(Hash: THashBase): Integer;
begin
If Hash is TCity32Hash then
  begin
    If fCity32 > TCity32Hash(Hash).City32Sys then
      Result := +1
    else If fCity32 < TCity32Hash(Hash).City32Sys then
      Result := -1
    else
      Result := 0;
  end
else raise ECITYIncompatibleClass.CreateFmt('TCity32Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCity32Hash.AsString: String;
begin
Result := IntToHex(fCity32,8);
end;

//------------------------------------------------------------------------------

procedure TCity32Hash.FromString(const Str: String);
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      fCity32 := TCity32Sys(StrToInt(Str))
    else
      fCity32 := TCity32Sys(StrToInt('$' + Str));
  end
else fCity32 := 0;
end;

//------------------------------------------------------------------------------

procedure TCity32Hash.FromStringDef(const Str: String; const Default: TCity32);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fCity32 := City32ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TCity32Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCity32;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}City32ToBE{$ELSE}City32ToLE{$ENDIF}(City32FromSys(fCity32));
  heLittle: Temp := City32ToLE(City32FromSys(fCity32));
  heBig:    Temp := City32ToBE(City32FromSys(fCity32));
else
 {heDefault}
  Temp := City32FromSys(fCity32);
end;
Stream.WriteBuffer(Temp,SizeOf(TCity32));
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TCity32Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCity32;
begin
Stream.ReadBuffer(Temp,SizeOf(TCity32));
case Endianness of
  heSystem: fCity32 := City32ToSys({$IFDEF ENDIAN_BIG}City32FromBE{$ELSE}City32FromLE{$ENDIF}(Temp));
  heLittle: fCity32 := City32ToSys(City32FromLE(Temp));
  heBig:    fCity32 := City32ToSys(City32FromBE(Temp));
else
 {heDefault}
  fCity32 := City32ToSys(Temp);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                  TCity64Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity64Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCity64Hash - utility functions
-------------------------------------------------------------------------------}

Function SwapEndian(Value: TCITY64Sys): TCITY64Sys; overload;
begin
Result := TCITY64Sys(EndianSwap(UInt64(Value)));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TCITY64): TCITY64; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := TCITY64(SwapEndian(TCITY64Sys(Value)));
end;

{-------------------------------------------------------------------------------
    TCity64Hash - protected methods
-------------------------------------------------------------------------------}

Function TCity64Hash.GetCity64: TCity64;
begin
Result := City64FromSys(fCity64);
end;

//------------------------------------------------------------------------------

procedure TCity64Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
CheckVersionAndVariant;  
case fCityVersion of
  verCITY100: case fCityVariant of
                varPlain: fCity64 := CITY_1_0_0.CityHash64(Memory,Count);
                varSeed:  fCity64 := CITY_1_0_0.CityHash64WithSeed(Memory,Count,fSeed0);
                varSeeds: fCity64 := CITY_1_0_0.CityHash64WithSeeds(Memory,Count,fSeed0,fSeed1);
              end;
  verCITY101: case fCityVariant of
                varPlain: fCity64 := CITY_1_0_1.CityHash64(Memory,Count);
                varSeed:  fCity64 := CITY_1_0_1.CityHash64WithSeed(Memory,Count,fSeed0);
                varSeeds: fCity64 := CITY_1_0_1.CityHash64WithSeeds(Memory,Count,fSeed0,fSeed1);
              end;
  verCITY102: case fCityVariant of
                varPlain: fCity64 := CITY_1_0_2.CityHash64(Memory,Count);
                varSeed:  fCity64 := CITY_1_0_2.CityHash64WithSeed(Memory,Count,fSeed0);
                varSeeds: fCity64 := CITY_1_0_2.CityHash64WithSeeds(Memory,Count,fSeed0,fSeed1);
              end;
  verCITY103: case fCityVariant of
                varPlain: fCity64 := CITY_1_0_3.CityHash64(Memory,Count);
                varSeed:  fCity64 := CITY_1_0_3.CityHash64WithSeed(Memory,Count,fSeed0);
                varSeeds: fCity64 := CITY_1_0_3.CityHash64WithSeeds(Memory,Count,fSeed0,fSeed1);
              end;
  verCITY110: case fCityVariant of
                varPlain: fCity64 := CITY_1_1_0.CityHash64(Memory,Count);
                varSeed:  fCity64 := CITY_1_1_0.CityHash64WithSeed(Memory,Count,fSeed0);
                varSeeds: fCity64 := CITY_1_1_0.CityHash64WithSeeds(Memory,Count,fSeed0,fSeed1);
              end;
  verDefault,
  verLatest,
  verCITY111: case fCityVariant of
                varPlain: fCity64 := CITY_1_1_1.CityHash64(Memory,Count);
                varSeed:  fCity64 := CITY_1_1_1.CityHash64WithSeed(Memory,Count,fSeed0);
                varSeeds: fCity64 := CITY_1_1_1.CityHash64WithSeeds(Memory,Count,fSeed0,fSeed1);
              end;
end;
end;

//------------------------------------------------------------------------------

procedure TCity64Hash.Initialize;
begin
inherited;
fCity64 := 0;
end;

{-------------------------------------------------------------------------------
    TCity64Hash - public methods
-------------------------------------------------------------------------------}

class Function TCity64Hash.CityVersionsSupported: TCITYVersions;
begin
Result := [verDefault,verLatest,verCITY100,verCITY101,verCITY102,verCITY103,verCITY110,verCITY111];
end;

//------------------------------------------------------------------------------

class Function TCity64Hash.CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants;
begin
case CityVersion of
  verDefault,
  verLatest,
  verCITY100,
  verCITY101,
  verCITY102,
  verCITY103,
  verCITY110,
  verCITY111: Result := [varPlain,varSeed,varSeeds];
else
  Result := [];
end;
end;

//------------------------------------------------------------------------------

class Function TCity64Hash.City64ToSys(City64: TCity64): TCity64Sys;
begin
Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TCity64Sys(City64));
end;

//------------------------------------------------------------------------------

class Function TCity64Hash.City64FromSys(City64: TCity64Sys): TCity64;
begin
Result := TCity64({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(City64));
end;

//------------------------------------------------------------------------------

class Function TCity64Hash.City64ToLE(City64: TCity64): TCity64;
begin
Result := City64;
end;

//------------------------------------------------------------------------------

class Function TCity64Hash.City64ToBE(City64: TCity64): TCity64;
begin
Result := SwapEndian(City64);
end;

//------------------------------------------------------------------------------

class Function TCity64Hash.City64FromLE(City64: TCity64): TCity64;
begin
Result := City64;
end;
 
//------------------------------------------------------------------------------

class Function TCity64Hash.City64FromBE(City64: TCity64): TCity64;
begin
Result := SwapEndian(City64);
end;

//------------------------------------------------------------------------------

class Function TCity64Hash.HashSize: TMemSize;
begin
Result := SizeOf(TCITY64);
end;

//------------------------------------------------------------------------------

class Function TCity64Hash.HashName: String;
begin
Result := 'CITY-64';
end;

//------------------------------------------------------------------------------

constructor TCity64Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCity64Hash then
  fCity64 := TCity64Hash(Hash).City64Sys
else
  raise ECITYIncompatibleClass.CreateFmt('TCity64Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TCity64Hash.CreateAndInitFrom(Hash: TCity64);
begin
CreateAndInit;
fCity64 := City64ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TCity64Hash.Init;
begin
inherited;
fCity64 := 0;
end;

//------------------------------------------------------------------------------

Function TCity64Hash.Compare(Hash: THashBase): Integer;
begin
If Hash is TCity64Hash then
  Result := CompareUInt64(fCity64,TCity64Hash(Hash).City64Sys)
else
  raise ECITYIncompatibleClass.CreateFmt('TCity64Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCity64Hash.AsString: String;
begin
Result := IntToHex(fCity64,16);
end;

//------------------------------------------------------------------------------

procedure TCity64Hash.FromString(const Str: String);
begin
If Length(Str) > 0 then
  begin
    If Str[1] = '$' then
      fCity64 := TCity64Sys(StrToUInt64(Str))
    else
      fCity64 := TCity64Sys(StrToUInt64('$' + Str));
  end
else fCity64 := 0;
end;

//------------------------------------------------------------------------------

procedure TCity64Hash.FromStringDef(const Str: String; const Default: TCity64);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fCity64 := City64ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TCity64Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCity64;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}City64ToBE{$ELSE}City64ToLE{$ENDIF}(City64FromSys(fCity64));
  heLittle: Temp := City64ToLE(City64FromSys(fCity64));
  heBig:    Temp := City64ToBE(City64FromSys(fCity64));
else
 {heDefault}
  Temp := City64FromSys(fCity64);
end;
Stream.WriteBuffer(Temp,SizeOf(TCity64));
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TCity64Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCity64;
begin
Stream.ReadBuffer(Temp,SizeOf(TCity64));
case Endianness of
  heSystem: fCity64 := City64ToSys({$IFDEF ENDIAN_BIG}City64FromBE{$ELSE}City64FromLE{$ENDIF}(Temp));
  heLittle: fCity64 := City64ToSys(City64FromLE(Temp));
  heBig:    fCity64 := City64ToSys(City64FromBE(Temp));
else
 {heDefault}
  fCity64 := City64ToSys(Temp);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                TCity128HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity128HashBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCity128HashBase - utility functions
-------------------------------------------------------------------------------}

Function SwapEndian(Value: TCITY128Sys): TCITY128Sys; overload;
begin
Result.Low := EndianSwap(Value.High);
Result.High := EndianSwap(Value.Low);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TCITY128): TCITY128; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := TCITY128(SwapEndian(TCITY128Sys(Value)));
end;

{-------------------------------------------------------------------------------
    TCity128HashBase - protected methods
-------------------------------------------------------------------------------}

Function TCity128HashBase.GetCity128: TCity128;
begin
Result := City128FromSys(fCity128);
end;

//------------------------------------------------------------------------------

procedure TCity128HashBase.Initialize;
begin
inherited;
FillChar(fCity128,SizeOf(TCITY128Sys),0);
end;

{-------------------------------------------------------------------------------
    TCity128HashBase - public methods
-------------------------------------------------------------------------------}

class Function TCity128HashBase.City128ToSys(City128: TCity128): TCity128Sys;
begin
Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TCity128Sys(City128));
end;

//------------------------------------------------------------------------------

class Function TCity128HashBase.City128FromSys(City128: TCity128Sys): TCity128;
begin
Result := TCity128({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(City128));
end;

//------------------------------------------------------------------------------

class Function TCity128HashBase.City128ToLE(City128: TCity128): TCity128;
begin
Result := City128;
end;

//------------------------------------------------------------------------------

class Function TCity128HashBase.City128ToBE(City128: TCity128): TCity128;
begin
Result := SwapEndian(City128);
end;

//------------------------------------------------------------------------------

class Function TCity128HashBase.City128FromLE(City128: TCity128): TCity128;
begin
Result := City128;
end;
 
//------------------------------------------------------------------------------

class Function TCity128HashBase.City128FromBE(City128: TCity128): TCity128;
begin
Result := SwapEndian(City128);
end;

//------------------------------------------------------------------------------

class Function TCity128HashBase.HashSize: TMemSize;
begin
Result := SizeOf(TCITY128);
end;

//------------------------------------------------------------------------------

constructor TCity128HashBase.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCity128HashBase then
  fCity128 := TCity128HashBase(Hash).City128Sys
else
  raise ECITYIncompatibleClass.CreateFmt('TCity128HashBase.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TCity128HashBase.CreateAndInitFrom(Hash: TCity128);
begin
CreateAndInit;
fCity128 := City128ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TCity128HashBase.Init;
begin
inherited;
FillChar(fCity128,SizeOf(TCITY128Sys),0);
end;

//------------------------------------------------------------------------------

Function TCity128HashBase.Compare(Hash: THashBase): Integer;
begin
If Hash is TCity128HashBase then
  begin
    Result := CompareUInt64(fCity128.High,TCity128HashBase(Hash).City128Sys.High);
    If Result = 0 then
      Result := CompareUInt64(fCity128.Low,TCity128HashBase(Hash).City128Sys.Low);
  end
else raise ECITYIncompatibleClass.CreateFmt('TCity128HashBase.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCity128HashBase.AsString: String;
begin
Result := IntToHex(fCity128.High,16) + IntToHex(fCity128.Low,16);
end;

//------------------------------------------------------------------------------

procedure TCity128HashBase.FromString(const Str: String);
begin
If Length(Str) > 0 then
  begin
    If Str[1] <> '$' then
      begin
        If Length(Str) > 16 then
          begin
            fCity128.High := StrToUInt64('$' + Copy(Str,1,Length(Str) - 16));
            fCity128.Low := StrToUInt64('$' + Copy(Str,Succ(Length(Str) - 16),16));
          end
        else
          begin
            fCity128.High := 0;
            fCity128.Low := StrToUInt64('$' + Str);
          end;
      end
    else FromString(Copy(Str,2,Length(Str)));
  end
else FillChar(fCity128,SizeOf(TCITY128Sys),0);
end;

//------------------------------------------------------------------------------

procedure TCity128HashBase.FromStringDef(const Str: String; const Default: TCity128);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fCity128 := City128ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TCity128HashBase.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCity128;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}City128ToBE{$ELSE}City128ToLE{$ENDIF}(City128FromSys(fCity128));
  heLittle: Temp := City128ToLE(City128FromSys(fCity128));
  heBig:    Temp := City128ToBE(City128FromSys(fCity128));
else
 {heDefault}
  Temp := City128FromSys(fCity128);
end;
Stream.WriteBuffer(Temp,SizeOf(TCity128));
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TCity128HashBase.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCity128;
begin
Stream.ReadBuffer(Temp,SizeOf(TCity128));
case Endianness of
  heSystem: fCity128 := City128ToSys({$IFDEF ENDIAN_BIG}City128FromBE{$ELSE}City128FromLE{$ENDIF}(Temp));
  heLittle: fCity128 := City128ToSys(City128FromLE(Temp));
  heBig:    fCity128 := City128ToSys(City128FromBE(Temp));
else
 {heDefault}
  fCity128 := City128ToSys(Temp);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                  TCity128Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity128Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCity128Hash - protected methods
-------------------------------------------------------------------------------}

procedure TCity128Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
CheckVersionAndVariant;
case fCityVersion of
  verCITY100: case fCityVariant of
                varPlain: fCity128 := CITY_1_0_0.CityHash128(Memory,Count);
                varSeed:  fCity128 := CITY_1_0_0.CityHash128WithSeed(Memory,Count,fSeed);
              end;
  verCITY101: case fCityVariant of
                varPlain: fCity128 := CITY_1_0_1.CityHash128(Memory,Count);
                varSeed:  fCity128 := CITY_1_0_1.CityHash128WithSeed(Memory,Count,fSeed);
              end;
  verCITY102: case fCityVariant of
                varPlain: fCity128 := CITY_1_0_2.CityHash128(Memory,Count);
                varSeed:  fCity128 := CITY_1_0_2.CityHash128WithSeed(Memory,Count,fSeed);
              end;
  verCITY103: case fCityVariant of
                varPlain: fCity128 := CITY_1_0_3.CityHash128(Memory,Count);
                varSeed:  fCity128 := CITY_1_0_3.CityHash128WithSeed(Memory,Count,fSeed);
              end;
  verCITY110: case fCityVariant of
                varPlain: fCity128 := CITY_1_1_0.CityHash128(Memory,Count);
                varSeed:  fCity128 := CITY_1_1_0.CityHash128WithSeed(Memory,Count,fSeed);
              end;
  verDefault,
  verLatest,
  verCITY111: case fCityVariant of
                varPlain: fCity128 := CITY_1_1_1.CityHash128(Memory,Count);
                varSeed:  fCity128 := CITY_1_1_1.CityHash128WithSeed(Memory,Count,fSeed);
              end;
end;
end;

{-------------------------------------------------------------------------------
    TCity128Hash - public methods
-------------------------------------------------------------------------------}

class Function TCity128Hash.CityVersionsSupported: TCITYVersions;
begin
Result := [verDefault,verLatest,verCITY100,verCITY101,verCITY102,verCITY103,verCITY110,verCITY111];
end;

//------------------------------------------------------------------------------

class Function TCity128Hash.CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants;
begin
case CityVersion of
  verDefault,
  verLatest,
  verCITY100,
  verCITY101,
  verCITY102,
  verCITY103,
  verCITY110,
  verCITY111: Result := [varPlain,varSeed];
else
  Result := [];
end;
end;

//------------------------------------------------------------------------------

class Function TCity128Hash.HashName: String;
begin
Result := 'CITY-128';
end;

{===============================================================================
--------------------------------------------------------------------------------
                                TCityCRC128Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCityCRC128Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCityCRC128Hash - protected methods
-------------------------------------------------------------------------------}

Function TCityCRC128Hash.GetHashImplementation: THashImplementation;
begin
// do not call inherited
If UIM_CityHash_GetFuncImpl(fnCRC32Intrinsic) in [CITY_Common.imAssembly,CITY_Common.imAccelerated]  then
  Result := hiAccelerated
else If UIM_CityHash_GetFuncImpl(fnCRC32Intrinsic) = CITY_Common.imPascal then
  Result := hiPascal
else
  raise ECITYInvalidImplementation.CreateFmt('TCityCRC128Hash.GetHashImplementation: Invalid implementation (%d).',
                                             [Ord(UIM_CityHash_GetFuncImpl(fnCRC32Intrinsic))]);
end;

//------------------------------------------------------------------------------

procedure TCityCRC128Hash.SetHashImplementation(Value: THashImplementation);
begin
// do not call inherited
case Value of
  hiAssembly,
  hiAccelerated:
    UIM_CityHash_SetFuncImpl(fnCRC32Intrinsic,CITY_Common.imAccelerated);
else
 {hiPascal}
  UIM_CityHash_SetFuncImpl(fnCRC32Intrinsic,CITY_Common.imPascal);
end;
end;

//------------------------------------------------------------------------------

procedure TCityCRC128Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
CheckVersionAndVariant;
case fCityVersion of
  verCITY101: case fCityVariant of
                varPlain: fCity128 := CITY_1_0_1.CityHashCrc128(Memory,Count);
                varSeed:  fCity128 := CITY_1_0_1.CityHashCrc128WithSeed(Memory,Count,fSeed);
              end;
  verCITY102: case fCityVariant of
                varPlain: fCity128 := CITY_1_0_2.CityHashCrc128(Memory,Count);
                varSeed:  fCity128 := CITY_1_0_2.CityHashCrc128WithSeed(Memory,Count,fSeed);
              end;
  verCITY103: case fCityVariant of
                varPlain: fCity128 := CITY_1_0_3.CityHashCrc128(Memory,Count);
                varSeed:  fCity128 := CITY_1_0_3.CityHashCrc128WithSeed(Memory,Count,fSeed);
              end;
  verCITY110: case fCityVariant of
                varPlain: fCity128 := CITY_1_1_0.CityHashCrc128(Memory,Count);
                varSeed:  fCity128 := CITY_1_1_0.CityHashCrc128WithSeed(Memory,Count,fSeed);
              end;
  verDefault,
  verLatest,
  verCITY111: case fCityVariant of
                varPlain: fCity128 := CITY_1_1_1.CityHashCrc128(Memory,Count);
                varSeed:  fCity128 := CITY_1_1_1.CityHashCrc128WithSeed(Memory,Count,fSeed);
              end;
end;
end;

{-------------------------------------------------------------------------------
    TCityCRC128Hash - public methods
-------------------------------------------------------------------------------}

class Function TCityCRC128Hash.CityVersionsSupported: TCITYVersions;
begin
Result := [verDefault,verLatest,verCITY101,verCITY102,verCITY103,verCITY110,verCITY111];
end;

//------------------------------------------------------------------------------

class Function TCityCRC128Hash.CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants;
begin
case CityVersion of
  verDefault,
  verLatest,
  verCITY101,
  verCITY102,
  verCITY103,
  verCITY110,
  verCITY111: Result := [varPlain,varSeed];
else
  Result := [];
end;
end;

//------------------------------------------------------------------------------

class Function TCityCRC128Hash.HashImplementationsAvailable: THashImplementations;
var
  Temp: TUIM_CityHash_Implementations;
begin
Temp := UIM_CityHash_AvailableFuncImpl(fnCRC32Intrinsic);
Result := [];
If CITY_Common.imPascal in Temp then
  Include(Result,hiPascal);
If CITY_Common.imAssembly in Temp then
  Include(Result,hiAssembly);
If CITY_Common.imAccelerated in Temp then
  Include(Result,hiAccelerated);
end;

//------------------------------------------------------------------------------

class Function TCityCRC128Hash.HashImplementationsSupported: THashImplementations;
var
  Temp: TUIM_CityHash_Implementations;
begin
Temp := UIM_CityHash_SupportedFuncImpl(fnCRC32Intrinsic);
Result := [];
If CITY_Common.imPascal in Temp then
  Include(Result,hiPascal);
If CITY_Common.imAssembly in Temp then
  Include(Result,hiAssembly);
If CITY_Common.imAccelerated in Temp then
  Include(Result,hiAccelerated);
end;

//------------------------------------------------------------------------------

class Function TCityCRC128Hash.HashName: String;
begin
Result := 'CITY-128(CRC)';
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TCity256HashBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCity256HashBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCity256HashBase - utility functions
-------------------------------------------------------------------------------}

Function SwapEndian(Value: TCITY256Sys): TCITY256Sys; overload;
begin
Result[0] := EndianSwap(Value[3]);
Result[1] := EndianSwap(Value[2]);
Result[2] := EndianSwap(Value[1]);
Result[3] := EndianSwap(Value[0]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TCITY256): TCITY256; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := TCITY256(SwapEndian(TCITY256Sys(Value)));
end;

{-------------------------------------------------------------------------------
    TCity256HashBase - protected methods
-------------------------------------------------------------------------------}

Function TCity256HashBase.GetCity256: TCity256;
begin
Result := City256FromSys(fCity256);
end;

//------------------------------------------------------------------------------

procedure TCity256HashBase.Initialize;
begin
inherited;
FillChar(fCity256,SizeOf(TCITY256Sys),0);
end;

{-------------------------------------------------------------------------------
    TCity256HashBase - public methods
-------------------------------------------------------------------------------}

class Function TCity256HashBase.City256ToSys(City256: TCity256): TCity256Sys;
begin
Result := {$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(TCity256Sys(City256));
end;

//------------------------------------------------------------------------------

class Function TCity256HashBase.City256FromSys(City256: TCity256Sys): TCity256;
begin
Result := TCity256({$IFDEF ENDIAN_BIG}SwapEndian{$ENDIF}(City256));
end;

//------------------------------------------------------------------------------

class Function TCity256HashBase.City256ToLE(City256: TCity256): TCity256;
begin
Result := City256;
end;

//------------------------------------------------------------------------------

class Function TCity256HashBase.City256ToBE(City256: TCity256): TCity256;
begin
Result := SwapEndian(City256);
end;

//------------------------------------------------------------------------------

class Function TCity256HashBase.City256FromLE(City256: TCity256): TCity256;
begin
Result := City256;
end;
 
//------------------------------------------------------------------------------

class Function TCity256HashBase.City256FromBE(City256: TCity256): TCity256;
begin
Result := SwapEndian(City256);
end;

//------------------------------------------------------------------------------

class Function TCity256HashBase.HashSize: TMemSize;
begin
Result := SizeOf(TCITY256);
end;

//------------------------------------------------------------------------------

constructor TCity256HashBase.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TCity256HashBase then
  fCity256 := TCity256HashBase(Hash).City256Sys
else
  raise ECITYIncompatibleClass.CreateFmt('TCity256HashBase.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TCity256HashBase.CreateAndInitFrom(Hash: TCity256);
begin
CreateAndInit;
fCity256 := City256ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TCity256HashBase.Init;
begin
inherited;
FillChar(fCity256,SizeOf(TCITY256Sys),0);
end;

//------------------------------------------------------------------------------

Function TCity256HashBase.Compare(Hash: THashBase): Integer;
var
  i:  Integer;
begin
If Hash is TCity256HashBase then
  begin
    For i := High(TCITY256Sys) downto Low(TCITY256Sys) do
      begin
        Result := CompareUInt64(fCity256[i],TCity256HashBase(Hash).City256Sys[i]);
        If Result <> 0 then
          Break{For i};
      end;
  end
else raise ECITYIncompatibleClass.CreateFmt('TCity256HashBase.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCity256HashBase.AsString: String;
begin
Result := IntToHex(fCity256[3],16) + IntToHex(fCity256[2],16) +
          IntToHex(fCity256[1],16) + IntToHex(fCity256[0],16);
end;

//------------------------------------------------------------------------------

procedure TCity256HashBase.FromString(const Str: String);
begin
If Length(Str) > 0 then
  begin
    If Str[1] <> '$' then
      begin
        FillChar(fCity256,SizeOf(TCITY256Sys),0);
        If Length(Str) > 48 then
          // if the string is too long (65+), following will throw an exception (which is wanted!)
          fCity256[3] := StrToUInt64('$' + Copy(Str,1,Length(Str) - 48));
        If Length(Str) > 32 then
          fCity256[2] := StrToUInt64('$' + Copy(Str,Max(1,Succ(Length(Str) - 48)),Min(16,Length(Str) - 32)));
        If Length(Str) > 16 then
          fCity256[1] := StrToUInt64('$' + Copy(Str,Max(1,Succ(Length(Str) - 32)),Min(16,Length(Str) - 16)));
        fCity256[0] := StrToUInt64('$' + Copy(Str,Max(1,Succ(Length(Str) - 16)),16));
      end
    else FromString(Copy(Str,2,Length(Str)));
  end
else FillChar(fCity256,SizeOf(TCITY256Sys),0);

end;

//------------------------------------------------------------------------------

procedure TCity256HashBase.FromStringDef(const Str: String; const Default: TCity256);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fCity256 := City256ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TCity256HashBase.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCity256;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}City256ToBE{$ELSE}City256ToLE{$ENDIF}(City256FromSys(fCity256));
  heLittle: Temp := City256ToLE(City256FromSys(fCity256));
  heBig:    Temp := City256ToBE(City256FromSys(fCity256));
else
 {heDefault}
  Temp := City256FromSys(fCity256);
end;
Stream.WriteBuffer(Temp,SizeOf(TCity256));
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TCity256HashBase.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCity256;
begin
Stream.ReadBuffer(Temp,SizeOf(TCity256));
case Endianness of
  heSystem: fCity256 := City256ToSys({$IFDEF ENDIAN_BIG}City256FromBE{$ELSE}City256FromLE{$ENDIF}(Temp));
  heLittle: fCity256 := City256ToSys(City256FromLE(Temp));
  heBig:    fCity256 := City256ToSys(City256FromBE(Temp));
else
 {heDefault}
  fCity256 := City256ToSys(Temp);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                TCityCRC256Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCityCRC256Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCityCRC256Hash - protected methods
-------------------------------------------------------------------------------}

Function TCityCRC256Hash.GetHashImplementation: THashImplementation;
begin
// do not call inherited
If UIM_CityHash_GetFuncImpl(fnCRC32Intrinsic) in [CITY_Common.imAssembly,CITY_Common.imAccelerated]  then
  Result := hiAccelerated
else If UIM_CityHash_GetFuncImpl(fnCRC32Intrinsic) = CITY_Common.imPascal then
  Result := hiPascal
else
  raise ECITYInvalidImplementation.CreateFmt('TCityCRC256Hash.GetHashImplementation: Invalid implementation (%d).',
                                             [Ord(UIM_CityHash_GetFuncImpl(fnCRC32Intrinsic))]);
end;

//------------------------------------------------------------------------------

procedure TCityCRC256Hash.SetHashImplementation(Value: THashImplementation);
begin
// do not call inherited
case Value of
  hiAssembly,
  hiAccelerated:
    UIM_CityHash_SetFuncImpl(fnCRC32Intrinsic,CITY_Common.imAccelerated);
else
 {hiPascal}
  UIM_CityHash_SetFuncImpl(fnCRC32Intrinsic,CITY_Common.imPascal);
end;
end;

//------------------------------------------------------------------------------

procedure TCityCRC256Hash.CalculateHash(Memory: Pointer; Count: TMemSize);
begin
CheckVersionAndVariant;
case fCityVersion of
  verCITY101: CITY_1_0_1.CityHashCrc256(Memory,Count,fCity256);
  verCITY102: CITY_1_0_2.CityHashCrc256(Memory,Count,fCity256);
  verCITY103: CITY_1_0_3.CityHashCrc256(Memory,Count,fCity256);
  verCITY110: CITY_1_1_0.CityHashCrc256(Memory,Count,fCity256);
  verDefault,
  verLatest,
  verCITY111: CITY_1_1_1.CityHashCrc256(Memory,Count,fCity256);
end;
end;

{-------------------------------------------------------------------------------
    TCityCRC256Hash - public methods
-------------------------------------------------------------------------------}

class Function TCityCRC256Hash.CityVersionsSupported: TCITYVersions;
begin
Result := [verDefault,verLatest,verCITY101,verCITY102,verCITY103,verCITY110,verCITY111];
end;

//------------------------------------------------------------------------------

class Function TCityCRC256Hash.CityVariantsSupported(CityVersion: TCITYVersion): TCITYVariants;
begin
case CityVersion of
  verDefault,
  verLatest,
  verCITY101,
  verCITY102,
  verCITY103,
  verCITY110,
  verCITY111: Result := [varPlain];
else
  Result := [];
end;
end;

//------------------------------------------------------------------------------

class Function TCityCRC256Hash.HashImplementationsAvailable: THashImplementations;
var
  Temp: TUIM_CityHash_Implementations;
begin
Temp := UIM_CityHash_AvailableFuncImpl(fnCRC32Intrinsic);
Result := [];
If CITY_Common.imPascal in Temp then
  Include(Result,hiPascal);
If CITY_Common.imAssembly in Temp then
  Include(Result,hiAssembly);
If CITY_Common.imAccelerated in Temp then
  Include(Result,hiAccelerated);
end;

//------------------------------------------------------------------------------

class Function TCityCRC256Hash.HashImplementationsSupported: THashImplementations;
var
  Temp: TUIM_CityHash_Implementations;
begin
Temp := UIM_CityHash_SupportedFuncImpl(fnCRC32Intrinsic);
Result := [];
If CITY_Common.imPascal in Temp then
  Include(Result,hiPascal);
If CITY_Common.imAssembly in Temp then
  Include(Result,hiAssembly);
If CITY_Common.imAccelerated in Temp then
  Include(Result,hiAccelerated);
end;

//------------------------------------------------------------------------------

class Function TCityCRC256Hash.HashName: String;
begin
Result := 'CITY-256(CRC)';
end;


{===============================================================================
--------------------------------------------------------------------------------
                        Backward compatibility functions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Utility functions - implementation
===============================================================================}

Function Hash128to64(x: UInt128): UInt64;
begin
Result := CITY_1_1_1.Hash128to64(x);
end;

{===============================================================================
    Main hash functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Main hash functions - public functions
-------------------------------------------------------------------------------}

Function CityHash32(s: Pointer; len: TMemSize): UInt32;
begin
Result := CITY_1_1_1.CityHash32(s,len);
end;

//------------------------------------------------------------------------------

Function CityHash64(s: Pointer; len: TMemSize): UInt64;
begin
Result := CITY_1_1_1.CityHash64(s,len);
end;

//------------------------------------------------------------------------------

Function CityHash64WithSeed(s: Pointer; len: TMemSize; seed: UInt64): UInt64;
begin
Result := CITY_1_1_1.CityHash64WithSeed(s,len,seed);
end;

//------------------------------------------------------------------------------

Function CityHash64WithSeeds(s: Pointer; len: TMemSize; seed0,seed1: UInt64): UInt64;
begin
Result := CITY_1_1_1.CityHash64WithSeeds(s,len,seed0,seed1);
end;

//------------------------------------------------------------------------------

Function CityHash128(s: Pointer; len: TMemSize): UInt128;
begin
Result := CITY_1_1_1.CityHash128(s,len);
end;

//------------------------------------------------------------------------------

Function CityHash128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;
begin
Result := CITY_1_1_1.CityHash128WithSeed(s,len,seed);
end;

{===============================================================================
    CRC hash functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    CRC hash functions - public functions
-------------------------------------------------------------------------------}

procedure CityHashCrc256(s: Pointer; len: TMemSize; out result: UInt256);
begin
CITY_1_1_1.CityHashCrc256(s,len,result);
end;

//------------------------------------------------------------------------------

Function CityHashCrc128WithSeed(s: Pointer; len: TMemSize; seed: UInt128): UInt128;
begin
Result := CITY_1_1_1.CityHashCrc128WithSeed(s,len,seed);
end;

//------------------------------------------------------------------------------

Function CityHashCrc128(s: Pointer; len: TMemSize): UInt128;
begin
Result := CITY_1_1_1.CityHashCrc128(s,len);
end;

end.
