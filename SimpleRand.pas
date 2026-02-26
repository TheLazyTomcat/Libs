{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple pseudo-random number generator

    This library is designed to provide a pseudo-random number generator that
    is fully deterministic, meaning it will produce exactly the same numbers
    given the same seed and sequence of subsequent generating calls.

    The default RNG provided by RTL is also deterministic, but it is explicitly
    stated in documentation that its implementation can change, and therefore
    should not be used for purposes where invariant behavior is required
    (encryption is specifically mentioned).

    To use this library, just create an instance of TSimpleRand class and use
    its methods to generate required numbers or data.

    You can also select which algorithm will be used for data generation - the
    TSimpleRand does not in itself do any generation, it internally manages a
    generator object (a descendant of class TSimpleRandGenerator) which does
    these calculations, and you can choose in the TSimpleRand constructor which
    class will be used as this generator object.

    Currently this library provides generators utilizing MD5 hash, Keccak/SHA-3
    hash and also one emulating random number generator present in Delphi 7 RTL.
    You can also implement your own generators if necessary (see the source or
    contact the author if you need more information on how to implement a
    generator).

    If you do not choose any class, then generator using Keccak/SHA-3 hash (or,
    more precisely, a sponge function that computes these hashes) will be used.

      WARNING - I have absolutely no idea how good this generator is in terms
                of "randomness" or values distribution. You should consider
                this fact before using it.

    The class TSimpleRand might be somewhat slow, especially in comparison with
    default RNG provided by the RTL. This is mainly because complete calculation
    round/cycle is perfomed for every single generated object/number.

    For situations where better performance is required, or for general usage
    where presence of next generated number in memory is of no concern, use
    class TSimpleRandBuffered. This class buffers maximum number of bytes that
    can be obtained from the generator before next round calculation, and
    requested random numbers are copied from this buffer. The calculations are
    then executed only when really needed. This can mainly speed-up generation
    of large number of small objects.
    
      NOTE - TSimpleRandBuffered returns DIFFERENT numbers than TSimpleRand for
             the same seed and call sequence.

  Version 1.1 (2025-02-01)

  Last change 2026-02-25

  ©2023-2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleRand

  Dependencies:
    AuxClasses    - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
  * AuxMath       - github.com/TheLazyTomcat/Lib.AuxMath
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps        - github.com/TheLazyTomcat/Lib.BitOps
    MD5           - github.com/TheLazyTomcat/Lib.MD5
    SHA3          - github.com/TheLazyTomcat/Lib.SHA3
    StrRect       - github.com/TheLazyTomcat/Lib.StrRect
  * UInt64Utils   - github.com/TheLazyTomcat/Lib.UInt64Utils

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol SimpleRand_UseAuxExceptions for details).

  AuxMath and UInt64Utils are required only in compilers that do not have full
  support for unsigned 64 bits wide integers (UInt64, QWord).

  Libraries AuxExceptions and UInt64Utils might also be required as an indirect
  dependencies.

  Indirect dependencies:
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    ListUtils          - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit SimpleRand;
{
  SimpleRand_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  SimpleRand_UseAuxExceptions to achieve this.
}
{$IF Defined(SimpleRand_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}  
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------
// do not touch following defines...  
{$UNDEF SR_OverflowChecks}
{$UNDEF SR_UInt64Supported}

interface

uses
  SysUtils,
  AuxTypes, AuxClasses, SHA3, MD5{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESRException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ESRInvalidValue    = class(ESRException);
  ESRSeedError       = class(ESRException);
  ESRConversionError = class(ESRException);

{===============================================================================
--------------------------------------------------------------------------------
                              TSimpleRandGenerator
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRandGenerator - class declaration
===============================================================================}
type
  TSimpleRandGenerator = class(TCustomObject)
  protected
  {
    (f)RoundOutputSize

    Set this to a number of bytes the generator can produce in one round - ie.
    after seeding and before any subsequent calculations are required. This is
    usually size of hash for hashing algorithms or size of block in block
    ciphers.

    This variable must be set in method Initialize and must NOT change during
    the entire lifetime of the generator object.

    It is used in TSimpleRandBuffered to establish the internal buffer. Values
    below 16 are disabling the buffering.
  }
    fRoundOutputSize: TMemSize;
    procedure Initialize; virtual; abstract;
    procedure Finalize; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Generate(out Buff; Size: TMemSize); virtual; abstract;
    procedure Seed(const Buff; Size: TMemSize); virtual; abstract;
    property RoundOutputSize: TMemSize read fRoundOutputSize;
  end;

  TSimpleRandGeneratorClass = class of TSimpleRandGenerator;

{===============================================================================
--------------------------------------------------------------------------------
                                   TSimpleRand
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRand - class declaration
===============================================================================}
type
  TSimpleRand = class(TCustomObject)
  protected
    fGenerator: TSimpleRandGenerator;
    procedure Initialize(GeneratorClass: TSimpleRandGeneratorClass); virtual;
    procedure Finalize; virtual;
    procedure SetSeed(const Buff; Size: TMemSize); virtual;
    procedure GetRand(out Buff; Size: TMemSize); virtual;
  public
  {
    Create

    Note that the generator is seeded during creation using method Seed.
  }
    constructor Create(GeneratorClass: TSimpleRandGeneratorClass); overload;
    constructor Create; overload;
    destructor Destroy; override;
  {
    Seed

    Uses local time sources (high precision timer if available) to obtain a
    seed.
  }
    procedure Seed; virtual;
    procedure SeedInt(Value: Int32); virtual;
    procedure SeedInt64(Value: Int64); virtual;
    procedure SeedFloat(Value: Double); virtual;
    procedure SeedString(const Value: String); virtual;
    procedure SeedBuffer(const Buffer; Size: TMemSize); virtual;
  {
    SeedFromEntropy

    Will use system-provided cryptographically random sources to obtain a seed
    of selected length.

    It uses function CryptGenRandom in Windows, and file /dev/urandom in Linux.

    Note that in Linux, the Length is limited to 256. If you use higher number,
    it will be ignored and 256 will be used instead.
  }
    procedure SeedFromEntropy(Length: TMemSize = 32); virtual;
    Function RandomBool: Boolean; virtual;
    Function RandomInt: Int32; overload; virtual;
    Function RandomInt64: Int64; overload; virtual;
    Function RandomUInt64: UInt64; overload; virtual;
  {
    RandomFloat

    When parameter Normalized is se to true (default), then the function
    returns a floating point number from interval [0,1), with at most 32 bits
    of precision.
    Otherwise a completely random number is returned. But note that this number
    will be valid and internally normalized - ie. no NaN, infinity or denormal.
  }
    Function RandomFloat(Normalized: Boolean = True): Double; overload; virtual;
  {
    RandomBuffer

    Fills provided memory buffer with Size number of random bytes.
  }
    procedure RandomBuffer(out Buffer; Size: TMemSize); virtual;
  {
    RandomGaussian

    Returns random numbers with gaussian distribution, mean of 0 and standard
    deviation 1.
  }
    Function RandomGaussian: Double; overload; virtual;
  {
    Random*(Range)

    For positive values of parameter Range, the returned number will be from
    interval [0,Range).
    For negative values, it will be from interval (Range,0].
    If range is set to zero, then zero is returned.
  }
    Function RandomInt(Range: Int32): Int32; overload; virtual;
    Function RandomInt64(Range: Int64): Int64; overload; virtual;
    Function RandomUInt64(Range: UInt64): UInt64; overload; virtual;
    Function RandomFloat(Range: Double): Double; overload; virtual;
    Function RandomGaussian(Mean,StdDev: Double): Double; overload; virtual;
  {
    RandomRange*

    These functions will always return a number from interval [from,to),
    irrespective of sign or relative magnitude of limits.

    For example, for limits 10 (from) and -50 (to), numbers between 10 and -49
    can be returned.
  }
    Function RandomRangeInt(aFrom,aTo: Int32): Int32; virtual;
    Function RandomRangeInt64(aFrom,aTo: Int64): Int64; virtual;
    Function RandomRangeUInt64(aFrom,aTo: UInt64): UInt64; virtual;
    Function RandomRangeFloat(aFrom,aTo: Double): Double; virtual;
    Function RandomFromBool(const Value: array of Boolean): Boolean; virtual;
    Function RandomFromInt(const Value: array of Int32): Int32; virtual;
    Function RandomFromInt64(const Value: array of Int64): Int64; virtual;
    Function RandomFromUInt64(const Value: array of UInt64): UInt64; virtual;
    Function RandomFromFloat(const Value: array of Double): Double; virtual;
    Function RandomFromString(const Value: array of String): String; virtual;
  {
    RandomFrom

    For all allowed pointer types (vtPointer, vtPChar, vtObject, vtClass,
    vtPWideChar and vtInterface), the pointer address is converted (casted)
    to Int64 and this number is then returned.
  }
    Function RandomFrom(const Value: array of const): Variant; virtual;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleRandBuffered
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRandBuffered - class declaration
===============================================================================}
type
  TSimpleRandBuffered = class(TSimpleRand)
  protected
    fBufferSize:    TMemSize;
    fBufferEnabled: Boolean;
    fBuffer:        Pointer;
    fBufferCount:   TMemSize; // number of unconsumed bytes in the buffer
    procedure Initialize(GeneratorClass: TSimpleRandGeneratorClass); override;
    procedure Finalize; override;
    procedure SetSeed(const Buff; Size: TMemSize); override;
    procedure GetRand(out Buff; Size: TMemSize); override;
    property BufferingEnabled: Boolean read fBufferEnabled;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                            TSimpleRandGenerator_SHA3
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRandGenerator_SHA3 - class declaration
===============================================================================}
type
  TSimpleRandGenerator_SHA3 = class(TSimpleRandGenerator)
  protected
    fGenerator: TKeccak0Hash;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    procedure Generate(out Buff; Size: TMemSize); override;
    procedure Seed(const Buff; Size: TMemSize); override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                            TSimpleRandGenerator_MD5                             
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRandGenerator_MD5 - class declaration
===============================================================================}
type
  TSimpleRandGenerator_MD5 = class(TSimpleRandGenerator)
  protected
    fGenerator: TMD5Hash;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    procedure Generate(out Buff; Size: TMemSize); override;
    procedure Seed(const Buff; Size: TMemSize); override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                             TSimpleRandGenerator_D7
--------------------------------------------------------------------------------
===============================================================================}
{
  TSimpleRandGenerator_D7

  This class implements the same algorithm generating pseudo-random data as
  generator provided in Delphi 7 RTL (System unit), but note that this does not
  mean TSimpleRand will always generate the same numbers.

  That being said, the TSimpleRand will provide the same results as RTL
  generator if you seed it using method SeedInt while providing the same
  integer seed that is loaded in System.RandSeed global variable, but only
  when you then limit generation to routines TSimpleRand.RandomInt(Range)
  (System.Random(Range)) and TSimpleRand.RandomFloat(True) (System.Random).
}
{===============================================================================
    TSimpleRandGenerator_D7 - class declaration
===============================================================================}
type
  TSimpleRandGenerator_D7 = class(TSimpleRandGenerator)
  protected
    fRngState:  Int32;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    procedure Generate(out Buff; Size: TMemSize); override;
    procedure Seed(const Buff; Size: TMemSize); override;
  end;

implementation

{$IF NativeUInt64}
  {$DEFINE SR_UInt64Supported}
{$ELSE}
  {$UNDEF SR_UInt64Supported}
{$IFEND}

uses
  {$IFDEF Windows}Windows,{$ELSE}baseunix, linux,{$ENDIF} Variants,
  BitOps, StrRect {$IFNDEF SR_UInt64Supported}, AuxMath, UInt64Utils{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$PUSH}{$WARN 2005 OFF}           // Comment level $1 found
  {$IF Defined(FPC) and (FPC_FULLVERSION >= 30200)}
    {$DEFINE W6058:={$WARN 6058 OFF}} // Call to subroutine "$1" marked as inline is not inlined
  {$ELSE}
    {$DEFINE W6058:=}
  {$IFEND}
  {$POP}
{$ENDIF}

{===============================================================================
    External and auxiliary functions
===============================================================================}
{$IFDEF Windows}

type
  HCRYPTPROV  = THandle;
  PHCRYPTPROV = ^HCRYPTPROV;

Function CryptAcquireContextA(phProv: PHCRYPTPROV; pszContainer: PAnsiChar; pszProvider: PAnsiChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall; external advapi32;
Function CryptAcquireContextW(phProv: PHCRYPTPROV; pszContainer: PWideChar; pszProvider: PWideChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall; external advapi32;
Function CryptAcquireContext(phProv: PHCRYPTPROV; pszContainer: PChar; pszProvider: PChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall; external advapi32 name
  {$IFDEF Unicode}'CryptAcquireContextW'{$ELSE}'CryptAcquireContextA'{$ENDIF};

Function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: DWORD): BOOL; stdcall; external advapi32;

Function CryptGenRandom(hProv: HCRYPTPROV; dwLen: DWORD; pbBuffer: PByte): BOOL; stdcall; external advapi32;

{$ELSE}

{$IFDEF FPCDWM}{$PUSH}W6058{$ENDIF}
procedure GetEntropy(out Buff; Count: TMemSize);
var
  FileHandle: cint;
begin
FileHandle := FpOpen(PChar('/dev/urandom'),O_RDONLY);
If FileHandle <> -1 then
  try
    If FpRead(FileHandle,Addr(Buff)^,tsize(Count)) <> tssize(Count) then
      raise ESRSeedError.Create('GetEntropy: Failed to read file.');
  finally
    If FpClose(FileHandle) = -1 then
      raise ESRSeedError.CreateFmt('GetEntropy: Failed to close file (%d).',[errno]);
  end
else raise ESRSeedError.CreateFmt('GetEntropy: Failed to open file (%d).',[errno]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                              TSimpleRandGenerator
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRandGenerator - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleRandGenerator - public methods
-------------------------------------------------------------------------------}

constructor TSimpleRandGenerator.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TSimpleRandGenerator.Destroy;
begin
Finalize;
inherited;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TSimpleRand                                   
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRand - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleRand - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleRand.Initialize(GeneratorClass: TSimpleRandGeneratorClass);
begin
fGenerator := GeneratorClass.Create;
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.Finalize;
begin
FreeAndNil(fGenerator);
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.SetSeed(const Buff; Size: TMemSize);
begin
fGenerator.Seed(Buff,Size);
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.GetRand(out Buff; Size: TMemSize);
begin
fGenerator.Generate(Buff,Size);
end;

{-------------------------------------------------------------------------------
    TSimpleRand - public methods
-------------------------------------------------------------------------------}

constructor TSimpleRand.Create(GeneratorClass: TSimpleRandGeneratorClass);
begin
inherited Create;
Initialize(GeneratorClass);
Seed;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSimpleRand.Create;
begin
Create(TSimpleRandGenerator_SHA3);
end;

//------------------------------------------------------------------------------

destructor TSimpleRand.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.Seed;
var
{$IFDEF Windows}
  PerfCounter:  Int64;
{$ELSE}
  TimeSpec:     TTimeSpec;
  Time:         time_t;
{$ENDIF}
begin
{
  As this is supposed to be out of control of the user, and therefore does not
  need to be fully deterministic, there is no need for endianess processing.
}
{$IFDEF Windows}
If not QueryPerformanceCounter(Int64(Addr(PerfCounter)^)) then
  PerfCounter := Int64(GetTickCount{returns DWORD});
SetSeed(PerfCounter,SizeOf(PerfCounter));
{$ELSE}
If clock_gettime(CLOCK_MONOTONIC_RAW,@TimeSpec) <> 0 then
  begin
    Time := FpTime;
    SetSeed(Time,SizeOf(Time))
  end
else SetSeed(TimeSpec,SizeOf(TimeSpec))
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.SeedInt(Value: Int32);
begin
{
  For the sake of deterministic behavior, make sure the same buffer is always
  hashed, irrespective of current system endianess.
}
{$IFNDEF ENDIAN_BIG}
Value := Int32(SwapEndian(UInt32(Value)));
{$ENDIF}
SetSeed(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.SeedInt64(Value: Int64);
begin
{$IFNDEF ENDIAN_BIG}
Value := Int64(SwapEndian(UInt64(Value)));
{$ENDIF}
SetSeed(Value,SizeOf(Value));
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.SeedFloat(Value: Double);
{$IFNDEF ENDIAN_BIG}
var
  Temp: UInt64;
begin
Temp := SwapEndian(PUInt64(Addr(Value))^);
SetSeed(Temp,SizeOf(Temp));
{$ELSE}
SetSeed(Value,SizeOf(Value));
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.SeedString(const Value: String);
var
  Temp: UTF8String;
begin
// make sure the same byte stream is hashed, no maater how the type String is declared
Temp := StrToUTF8(Value);
SetSeed(PUTF8Char(Temp)^,Length(Temp) * SizeOf(UTF8Char));
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.SeedBuffer(const Buffer; Size: TMemSize);
begin
SetSeed(Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.SeedFromEntropy(Length: TMemSize = 32);
var
  Buff:           Pointer;
{$IFDEF Windows}
  CryptoProvider: HCRYPTPROV;
{$ENDIF}
begin
If Length > 0 then
  begin
  {$IFNDEF Windows}
    If Length > 256 then
      Length := 256;
  {$ENDIF}
    Buff := AllocMem(Length);
    try
    {$IFDEF Windows}
      If CryptAcquireContext(@CryptoProvider,nil,nil,1{PROV_RSA_FULL},0) then
        try
          If CryptGenRandom(CryptoProvider,DWORD(Length),Buff) then
            SetSeed(Buff^,Length)
          else
            raise ESRSeedError.CreateFmt('TSimpleRand.SeedFromEntropy: Failed to generate entropy (%d).',[GetLastError]);
        finally
          If not CryptReleaseContext(CryptoProvider,0) then
            raise ESRSeedError.CreateFmt('TSimpleRand.SeedFromEntropy: Failed to release crypto provider (%d).',[GetLastError]);
        end
      else raise ESRSeedError.CreateFmt('TSimpleRand.SeedFromEntropy: Failed to acquire crypto provider (%d).',[GetLastError]);
    {$ELSE}
      GetEntropy(Buff^,Length);
      SetSeed(Buff^,Length)
    {$ENDIF}
    finally
      FreeMem(Buff,Length);
    end;
  end
else raise ESRInvalidValue.CreateFmt('TSimpleRand.SeedFromEntropy: Invalid seed length (%u).',[Length]);
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomBool: Boolean;
var
  Temp: UInt8;
begin
GetRand(Temp,1);
Result := (Temp and 1) <> 0;
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomInt: Int32;
begin
GetRand(Result,SizeOf(Result));
{
  Make sure the same value is returned, irrespective of current system
  endianess.
}
{$IFNDEF ENDIAN_BIG}
Result := Int32(SwapEndian(UInt32(Result)));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomInt64: Int64;
begin
GetRand(Result,SizeOf(Result));
{$IFNDEF ENDIAN_BIG}
Result := Int64(SwapEndian(UInt64(Result)));
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomUInt64: UInt64;
begin
GetRand(Result,SizeOf(Result));
{$IFNDEF ENDIAN_BIG}
Result := SwapEndian(Result);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomFloat(Normalized: Boolean = True): Double;
const
  F64_MASK_SIGN = UInt64($8000000000000000);  // sign bit
  F64_MASK_FRAC = UInt64($000FFFFFFFFFFFFF);  // fraction/mantissa
  Coef: Extended = 1.0 / Int64($100000000);
var
  Temp: UInt64 absolute Result;
  Exp:  Integer;
begin
If not Normalized then
  begin
  {
    First get exponent - if exponent is zero, then mantissa must be zero too,
    otherwise we would create a denormal number.
    Also, highest possible exponent (0x7FF) is reserved for NaN and infinity.

    Then combine mantisaa (which can be completely random) with the obtained
    exponent.

    And at the end, select sign of the number.
  }
    Exp := RandomInt($7FF);
    If Exp <> 0 then
      Temp := (RandomUInt64() and F64_MASK_FRAC) or (UInt64(Exp) shl 52)
    else
      Temp := UInt64(Exp) shl 52;
    If RandomBool then
      Temp := Temp or F64_MASK_SIGN;
  end
else Result := UInt32(RandomInt()) * Coef;
end;

//------------------------------------------------------------------------------

procedure TSimpleRand.RandomBuffer(out Buffer; Size: TMemSize);
begin
GetRand(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomGaussian: Double;
var
  U1, S2: Extended;
begin
  repeat
    U1 := 2 * RandomFloat(True) - 1;
    S2 := Sqr(U1) + Sqr(2 * RandomFloat(True) - 1);
  until S2 < 1;
  Result := Sqrt(-2 * Ln(S2) / S2) * U1;
end;

//------------------------------------------------------------------------------

// there can be overflow in multiplication
{$IFOPT Q+}{$DEFINE SR_OverflowChecks}{$Q-}{$ENDIF}

Function TSimpleRand.RandomInt(Range: Int32): Int32;
begin
Result := Int32((Int64(UInt32(RandomInt())) * Range) shr 32);
If Range < 0 then
  Inc(Result);
end;

{$IFDEF SR_OverflowChecks}{$Q+}{$UNDEF SR_OverflowChecks}{$ENDIF}

//------------------------------------------------------------------------------

Function TSimpleRand.RandomInt64(Range: Int64): Int64;
begin
If Range > 0 then
  Result := (RandomInt64() and not(Int64(1) shl 63)) mod Range
else If Range < 0 then
  Result := (RandomInt64() or (Int64(1) shl 63)) mod Range
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomUInt64(Range: UInt64): UInt64;
{$IFDEF SR_UInt64Supported}
begin
If Range <> 0 then
  Result := RandomUInt64() mod Range
else
  Result := 0;
{$ELSE}
var
  Quotient: UInt64;
begin
If Range <> 0 then
  uDivMod(RandomUInt64(),Range,Quotient,Result)
else
  Result := 0;
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomFloat(Range: Double): Double;
begin
Result := RandomFloat(True) * Range;
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomGaussian(Mean,StdDev: Double): Double;
begin
Result := RandomGaussian() * StdDev + Mean;
end;

//------------------------------------------------------------------------------

{$IFOPT Q+}{$DEFINE SR_OverflowChecks}{$Q-}{$ENDIF}

Function TSimpleRand.RandomRangeInt(aFrom,aTo: Int32): Int32;
begin
If aFrom > aTo then
  Result := Int32(RandomInt64(UInt32(aFrom - aTo)) + aTo + 1)
else
  Result := Int32(RandomInt64(UInt32(aTo - aFrom)) + aFrom);
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomRangeInt64(aFrom,aTo: Int64): Int64;
begin
If aFrom > aTo then
  Result := Int64(RandomUInt64(UInt64(aFrom - aTo))) + aTo + Int64(1)
else
  Result := Int64(RandomUInt64(UInt64(aTo - aFrom))) + aFrom;
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomRangeUInt64(aFrom,aTo: UInt64): UInt64;
begin
{$IFDEF SR_UInt64Supported}
If aFrom > aTo then
  Result := RandomUInt64(aFrom - aTo) + aTo + UInt64(1)
else
  Result := RandomUInt64(aTo - aFrom) + aFrom;
{$ELSE}
If CompareUInt64(aFrom,aTo) > 0 then
  Result := UInt64(Int64(RandomUInt64(UInt64(Int64(aFrom) - Int64(aTo)))) + Int64(aTo) + Int64(1))
else
  Result := UInt64(Int64(RandomUInt64(UInt64(Int64(aTo) - Int64(aFrom)))) + Int64(aFrom));
{$ENDIF}
end;

{$IFDEF SR_OverflowChecks}{$Q+}{$UNDEF SR_OverflowChecks}{$ENDIF}

//------------------------------------------------------------------------------

Function TSimpleRand.RandomRangeFloat(aFrom,aTo: Double): Double;
begin
Result := ((aTo - aFrom) * RandomFloat(True)) + aFrom;
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomFromBool(const Value: array of Boolean): Boolean;
begin
If Length(Value) > 0 then
  Result := Value[RandomInt(Length(Value))]
else
  raise ESRInvalidValue.Create('TSimpleRand.RandomFromBool: Empty values array.');
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomFromInt(const Value: array of Int32): Int32;
begin
If Length(Value) > 0 then
  Result := Value[RandomInt(Length(Value))]
else
  raise ESRInvalidValue.Create('TSimpleRand.RandomFromInt: Empty values array.');
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomFromInt64(const Value: array of Int64): Int64;
begin
If Length(Value) > 0 then
  Result := Value[RandomInt(Length(Value))]
else
  raise ESRInvalidValue.Create('TSimpleRand.RandomFromInt64: Empty values array.');
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomFromUInt64(const Value: array of UInt64): UInt64;
begin
If Length(Value) > 0 then
  Result := Value[RandomInt(Length(Value))]
else
  raise ESRInvalidValue.Create('TSimpleRand.RandomFromUInt64: Empty values array.');
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomFromFloat(const Value: array of Double): Double;
begin
If Length(Value) > 0 then
  Result := Value[RandomInt(Length(Value))]
else
  raise ESRInvalidValue.Create('TSimpleRand.RandomFromFloat: Empty values array.');
end;

//------------------------------------------------------------------------------

Function TSimpleRand.RandomFromString(const Value: array of String): String;
begin
If Length(Value) > 0 then
  Result := Value[RandomInt(Length(Value))]
else
  raise ESRInvalidValue.Create('TSimpleRand.RandomFromString: Empty values array.');
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W6058{$ENDIF}
Function TSimpleRand.RandomFrom(const Value: array of const): Variant;
var
  Index:  Integer;
begin
If Length(Value) > 0 then
  begin
    Index := RandomInt(Length(Value));
    {
      Afaik there is no built-in (compiler, RTL, anything) conversion from
      TVarRec to a Variant...
    }
    case Value[Index].VType of
      vtInteger:        Result := TVarRec(Value[Index]).VInteger;
      vtBoolean:        Result := TVarRec(Value[Index]).VBoolean;
      vtChar:           Result := TVarRec(Value[Index]).VChar;
      vtExtended:       Result := TVarRec(Value[Index]).VExtended^;
    {$IF Declared(vtString)}
      vtString:         Result := ShortString(TVarRec(Value[Index]).VString^);
    {$IFEND}
    {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
      vtPointer:        Result := Int64(PtrUInt(TVarRec(Value[Index]).VPointer));
      vtPChar:          Result := Int64(PtrUInt(TVarRec(Value[Index]).VPChar));
      vtObject:         Result := Int64(PtrUInt(Pointer(TVarRec(Value[Index]).VObject)));
      vtClass:          Result := Int64(PtrUInt(Pointer(TVarRec(Value[Index]).VClass)));
      vtWideChar:       Result := WideString(TVarRec(Value[Index]).VWideChar);
      vtPWideChar:      Result := Int64(PtrUInt(TVarRec(Value[Index]).VPWideChar));
      vtAnsiString:     Result := AnsiString(TVarRec(Value[Index]).VAnsiString);
      vtCurrency:       Result := TVarRec(Value[Index]).VCurrency^;
      vtVariant:        Result := TVarRec(Value[Index]).VVariant^;
      vtInterface:      Result := Int64(PtrUInt(TVarRec(Value[Index]).VInterface));
    {$IFDEF FPCDWM}{$POP}{$ENDIF}
      vtWideString:     Result := WideString(TVarRec(Value[Index]).VWideString);
      vtInt64:          Result := TVarRec(Value[Index]).VInt64^;
    {$IF Declared(vtQWord)}
      vtQWord:          Result := TVarRec(Value[Index]).VQWord^;
    {$IFEND}
    {$IF Declared(vtUnicodeString)}
      vtUnicodeString:  Result := UnicodeString(TVarRec(Value[Index]).VUnicodeString);
    {$IFEND}
    else
      raise ESRConversionError.CreateFmt('TSimpleRand.RandomFrom: Cannot convert value of type %d.',[Value[Index].VType]);
    end;
  end
else raise ESRInvalidValue.Create('TSimpleRand.RandomFrom: Empty values array.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleRandBuffered
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRandBuffered - class declaration
===============================================================================}

procedure TSimpleRandBuffered.Initialize(GeneratorClass: TSimpleRandGeneratorClass);
begin
inherited;  // fGenerator is created here
fBufferSize := fGenerator.RoundOutputSize;
fBufferEnabled := fBufferSize >= (2 * SizeOf(UInt64){16 bytes});
If fBufferEnabled then
  begin
    fBuffer := AllocMem(fBufferSize);
    fBufferCount := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleRandBuffered.Finalize;
begin
If fBufferEnabled then
  FreeMem(fBuffer,fBufferSize);
inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleRandBuffered.SetSeed(const Buff; Size: TMemSize);
begin
inherited SetSeed(Buff,Size);
fBufferCount := 0;
end;

//------------------------------------------------------------------------------

procedure TSimpleRandBuffered.GetRand(out Buff; Size: TMemSize);
var
  Ptr:      PByte;
  Transfer: TMemSize;

  Function Min(A,B: TMemSize): TMemSize;
  begin
    If A < B then Result := A
      else Result := B;
  end;

begin
If fBufferEnabled then
  begin
    If Size > fBufferCount then
      begin
        Ptr := @Buff;
        // consume what is left in the buffer
        If fBufferCount > 0 then
          begin
            Move(PtrAdvance(fBuffer,PtrInt(fBufferSize - fBufferCount))^,Ptr^,fBufferCount);
            Inc(Ptr,fBufferCount);
            Dec(Size,fBufferCount);
            fBufferCount := 0;
          end;
        // generate and consume new random data
        while Size > 0 do
          begin
            fGenerator.Generate(fBuffer^,fBufferSize);
            fBufferCount := fBufferSize;
            Transfer := Min(Size,fBufferCount);
            Move(fBuffer^,Ptr^,Transfer);
            Inc(Ptr,Transfer);
            Dec(fBufferCount,Transfer);
            Dec(Size,Transfer);
          end;
      end
    else
      begin
        Move(PtrAdvance(fBuffer,PtrInt(fBufferSize - fBufferCount))^,Addr(Buff)^,Size);
        Dec(fBufferCount,Size);
      end;
  end
else inherited GetRand(Buff,Size);
end;


{===============================================================================
--------------------------------------------------------------------------------
                            TSimpleRandGenerator_SHA3
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRandGenerator_SHA3 - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleRandGenerator_SHA3 - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleRandGenerator_SHA3.Initialize;
begin
fGenerator := TKeccak0Hash.Create;
fRoundOutputSize := fGenerator.BlockSize; // this should be 128 bytes
end;

//------------------------------------------------------------------------------

procedure TSimpleRandGenerator_SHA3.Finalize;
begin
FreeAndNil(fGenerator);
end;

{-------------------------------------------------------------------------------
    TSimpleRandGenerator_SHA3 - public methods
-------------------------------------------------------------------------------}

procedure TSimpleRandGenerator_SHA3.Generate(out Buff; Size: TMemSize);
begin
{
  Following two lines should be reversed, but for the sake of backward
  compatibility let's leave it this way.
}
fGenerator.Permute;
fGenerator.Squeeze(Addr(Buff)^,Size);
end;

//------------------------------------------------------------------------------

procedure TSimpleRandGenerator_SHA3.Seed(const Buff; Size: TMemSize);
begin
fGenerator.HashBuffer(Buff,Size);
end;


{===============================================================================
--------------------------------------------------------------------------------
                            TSimpleRandGenerator_MD5                            
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRandGenerator_MD5 - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleRandGenerator_MD5 - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleRandGenerator_MD5.Initialize;
begin
fGenerator := TMD5Hash.Create;
fRoundOutputSize := SizeOf(TMD5); // 16 bytes (128bits)
end;

//------------------------------------------------------------------------------

procedure TSimpleRandGenerator_MD5.Finalize;
begin
FreeAndNil(fGenerator);
end;

{-------------------------------------------------------------------------------
    TSimpleRandGenerator_MD5 - public methods
-------------------------------------------------------------------------------}

procedure TSimpleRandGenerator_MD5.Generate(out Buff; Size: TMemSize);
var
  WorkPtr:  PMD5;
  TempMD5:  TMD5;
begin
WorkPtr := @Buff;
while Size >= TMemSize(SizeOf(TMD5)) do
  begin
    TempMD5 := fGenerator.MD5;
    fGenerator.HashBuffer(TempMD5,SizeOf(TMD5));
    WorkPtr^ := TempMD5;
    Inc(WorkPtr);
    Dec(Size,TMemSize(SizeOf(TMD5)));
   end;
If Size > 0 then
  begin
    TempMD5 := fGenerator.MD5;
    fGenerator.HashBuffer(TempMD5,SizeOf(TMD5));
    WorkPtr^ := TempMD5;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleRandGenerator_MD5.Seed(const Buff; Size: TMemSize);
begin
fGenerator.HashBuffer(Buff,Size);
end;


{===============================================================================
--------------------------------------------------------------------------------
                             TSimpleRandGenerator_D7
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleRandGenerator_D7 - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleRandGenerator_D7 - protected methods
-------------------------------------------------------------------------------}

procedure TSimpleRandGenerator_D7.Initialize;
begin
fRngState := 0;
fRoundOutputSize := SizeOf(Int32);
end;

//------------------------------------------------------------------------------

procedure TSimpleRandGenerator_D7.Finalize;
begin
// nothing to do here
end;

{-------------------------------------------------------------------------------
    TSimpleRandGenerator_D7 - public methods
-------------------------------------------------------------------------------}

procedure TSimpleRandGenerator_D7.Generate(out Buff; Size: TMemSize);
var
  WorkPtr:  PInt32;
  TempWord: Int32;
begin
WorkPtr := @Buff;
while Size >= TMemSize(SizeOf(Int32)) do
  begin
    fRngState := Int32((Int64(fRngState) * $08088405) + 1);
    WorkPtr^ := Int32({$IFNDEF ENDIAN_BIG}SwapEndian{$ENDIF}(UInt32(fRngState)));
    Inc(WorkPtr);
    Dec(Size,SizeOf(Int32));
   end;
If Size > 0 then
  begin
    fRngState := Int32((Int64(fRngState) * $08088405) + 1);
    TempWord := fRngState;
  {$IFNDEF ENDIAN_BIG}
    SwapEndianValue(UInt32(TempWord));
  {$ENDIF}
    Move(TempWord,WorkPtr^,Size);    
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleRandGenerator_D7.Seed(const Buff; Size: TMemSize);
var
  WorkPtr:  PInt32;
  TempWord: Int32;
begin
fRngState := 0;
WorkPtr := @Buff;
while Size >= TMemSize(SizeOf(Int32)) do
  begin
    fRngState := fRngState xor Int32({$IFNDEF ENDIAN_BIG}SwapEndian{$ENDIF}(UInt32(WorkPtr^)));
    Inc(WorkPtr);
    Dec(Size,SizeOf(Int32));
  end;
If Size > 0 then
  begin
    TempWord := 0;
    Move(WorkPtr^,TempWord,Size);
  {$IFNDEF ENDIAN_BIG}
    SwapEndianValue(UInt32(TempWord));
  {$ENDIF}
    fRngState := fRngState xor TempWord;
  end;
end;

end.

