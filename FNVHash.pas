{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Fowler–Noll–Vo (FNV) hash

    This unit provides a mean of computing FNV hash of any provided data. Hash
    widths of 32, 64, 128, 256, 512 and 1024 bits are all provided. Also,
    algorithms of version 0, 1 and 1a are implemented and can be selected for
    each and every width variant.

                               --- DISCLAIMER ---                                                                                       

    Implementation is somewhat naive and the entire thing was written without
    access to any relevant documentation (ie. no internet, completely offline).
    Only source of information was a Wikipedia page - which is not ideal, to
    say the least.
    This all means that some things might be wrong or missing. If you find any
    problem, please let me know.

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

      github.com/TheLazyTomcat/Lib.FNVHash

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
unit FNVHash;

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
  {$MODESWITCH DuplicateLocals+}
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
  EFNVException = class(EHashException);

  EFNVIncompatibleClass = class(EFNVException);
  EFNVInvalidValue      = class(EFNVException);
  EFNVInvalidState      = class(EFNVException);

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in types TFNVb (where b is bit width) are in memory always ordered from
  least significant byte to most significant byte (little endian).

  Types TFNVbSys have no such guarantee and their endianness is undefiend (note
  they do not necessarily have system endianness, and byte ordering can even be
  mixed).

  To convert the checksum in default ordering to a required specific ordering,
  use methods FNVbToLE for little endian and FNVbToBE for big endian. Note that
  these methods are expecting the input value to be in default ordering, if it
  is not, the result will be wrong. Be careful when using them.
}  
type
  TFNV32 = packed array[0..3] of UInt8;
  PFNV32 = ^TFNV32;

  TFNV64 = packed array[0..7] of UInt8;
  PFNV64 = ^TFNV64;

  TFNV128 = packed array[0..15] of UInt8;
  PFNV128 = ^TFNV128;

  TFNV256 = packed array[0..31] of UInt8;
  PFNV256 = ^TFNV256;

  TFNV512 = packed array[0..63] of UInt8;
  PFNV512 = ^TFNV512;

  TFNV1024 = packed array[0..127] of UInt8;
  PFNV1024 = ^TFNV32;

//------------------------------------------------------------------------------
type  
  TFNV32Sys = UInt32;
  PFNV32Sys = ^TFNV32Sys;

  TFNV64Sys = UInt64;
  PFNV64Sys = ^TFNV64Sys;
{
  There is no point in declaring completely new types for larger hashes, they
  would end-up as an array of something anyway.
}
  TFNV128Sys = type TFNV128;
  PFNV128Sys = ^TFNV128Sys;

  TFNV256Sys = type TFNV256;
  PFNV256Sys = ^TFNV256Sys;

  TFNV512Sys = type TFNV512;
  PFNV512Sys = ^TFNV512Sys;

  TFNV1024Sys = type TFNV1024;
  PFNV1024Sys = ^TFNV1024Sys;

//------------------------------------------------------------------------------
const
  ZeroFNV32: TFNV32 = (0,0,0,0);

  ZeroFNV64: TFNV64 = (0,0,0,0,0,0,0,0);

  ZeroFNV128: TFNV128 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

  ZeroFNV256: TFNV256 = (
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

  ZeroFNV512: TFNV512 = (
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

  ZeroFNV1024: TFNV1024 = (
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

//------------------------------------------------------------------------------
{
  All following initial values can also be obtained by hashing ASCII-encoded
  string "chongo <Landon Curt Noll> /\../\" (without the quotes) using FNV-0
  algorithm and corresponding size.
}
const      
  InitialFNV32: TFNV32 = ($C5,$9D,$1C,$81);

  InitialFNV64: TFNV64 = ($25,$23,$22,$84,$E4,$9C,$F2,$CB);

  InitialFNV128: TFNV128 = (
      $8D,$C5,$95,$62,$75,$21,$B8,$62,$42,$01,$BB,$07,$2E,$27,$62,$6C);

  InitialFNV256: TFNV256 = (
      $35,$05,$EE,$CA,$C8,$B4,$23,$10,$B3,$BB,$B6,$47,$68,$53,$B1,$C8,
      $CC,$76,$E5,$C4,$84,$C3,$98,$2D,$36,$50,$C5,$AA,$BC,$8D,$26,$DD);

  InitialFNV512: TFNV512 = (
      $D9,$9F,$FE,$4A,$AC,$2A,$98,$AC,$4B,$E3,$56,$5F,$41,$36,$20,$18,
      $CE,$E7,$DB,$42,$C9,$9B,$A7,$2E,$F6,$92,$C1,$34,$8A,$F6,$48,$E9,
      $21,$0D,$00,$00,$00,$00,$00,$00,$00,$00,$00,$C9,$59,$D0,$87,$AC,
      $AC,$90,$99,$30,$0F,$E5,$A1,$DC,$16,$44,$1F,$17,$B1,$B0,$6D,$B8);

  InitialFNV1024: TFNV1024 = (
      $B3,$90,$EE,$71,$6C,$B1,$F4,$AF,$21,$3B,$A9,$C6,$C9,$8C,$DE,$6B,
      $55,$AE,$05,$C0,$6C,$25,$5F,$55,$0A,$51,$34,$27,$80,$73,$6E,$EB,
      $D7,$C6,$04,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$D9,$21,$9A,$DA,$74,$36,$DA,$4E,$F3,$3B,$6C,
      $A1,$AD,$FD,$23,$42,$FC,$29,$4B,$B7,$28,$10,$59,$5A,$6D,$E5,$32,
      $4D,$CC,$8E,$75,$76,$7A,$5F,$00,$00,$00,$00,$00,$00,$00,$00,$00);

{===============================================================================
--------------------------------------------------------------------------------
                                  TFNVBaseHash
--------------------------------------------------------------------------------
===============================================================================}
type
  TFNVHashAlgorithm = (algFNV0,algFNV1,algFNV1a);

{===============================================================================
    TFNVBaseHash - class declaration
===============================================================================}
type
  TFNVBaseHash = class(TStreamHash)
  protected
    fHashAlgorithm: TFNVHashAlgorithm;
    fProcessBuffer: procedure(const Buffer; Size: TMemSize) of object;
    procedure ForceHashAlgorithm(NewValue: TFNVHashAlgorithm); virtual;
    procedure SetHashAlgorithm(NewValue: TFNVHashAlgorithm); virtual;
    procedure ProcessBuffer_FNV_0(const Buffer; Size: TMemSize); virtual; abstract;
  {
    Algorithm 1 is the same as 0, only the initial value differs - this is
    managed in derived classes by method Init.
  }
    procedure ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize); virtual; abstract;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    Function HashName: String; reintroduce; virtual;  // must not be class method here
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    property HashAlgorithm: TFNVHashAlgorithm read fHashAlgorithm write SetHashAlgorithm;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV32Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFNV32Hash - class declaration
===============================================================================}
type
  TFNV32Hash = class(TFNVBaseHash)
  protected
    fFNV32Value:  TFNV32Sys;
    Function GetFNV32: TFNV32; virtual;
    procedure ProcessBuffer_FNV_0(const Buffer; Size: TMemSize); override;
    procedure ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function FNV32ToSys(Hash: TFNV32): TFNV32Sys; virtual;
    class Function FNV32FromSys(Hash: TFNV32Sys): TFNV32; virtual;
    class Function FNV32ToLE(Hash: TFNV32): TFNV32; virtual;
    class Function FNV32ToBE(Hash: TFNV32): TFNV32; virtual;
    class Function FNV32FromLE(Hash: TFNV32): TFNV32; virtual;
    class Function FNV32FromBE(Hash: TFNV32): TFNV32; virtual;
    class Function HashSize: TMemSize; override;
    Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
  {
    CreateAndInitFrom

    Parameter Algorithm is here for situations where one wants to continue
    processing from given Hash, but with non-default algorithm - HashAlgorithm
    property cannot be changed (would raise an EFNVInvalidState exception)
    after Init, which is implicitly called by this constructor. So to allow
    for non-default algorithm, it must be selected here.
  }
    constructor CreateAndInitFrom(Hash: TFNV32; HashAlgorithm: TFNVHashAlgorithm = algFNV1a); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TFNV32); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property FNV32: TFNV32 read GetFNV32;
    property FNV32Sys: TFNV32Sys read fFNV32Value;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV64Hash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFNV64Hash - class declaration
===============================================================================}
type
  TFNV64Hash = class(TFNVBaseHash)
  protected
    fFNV64Value:  TFNV64Sys;
    Function GetFNV64: TFNV64; virtual;
    procedure ProcessBuffer_FNV_0(const Buffer; Size: TMemSize); override;
    procedure ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function FNV64ToSys(Hash: TFNV64): TFNV64Sys; virtual;
    class Function FNV64FromSys(Hash: TFNV64Sys): TFNV64; virtual;
    class Function FNV64ToLE(Hash: TFNV64): TFNV64; virtual;
    class Function FNV64ToBE(Hash: TFNV64): TFNV64; virtual;
    class Function FNV64FromLE(Hash: TFNV64): TFNV64; virtual;
    class Function FNV64FromBE(Hash: TFNV64): TFNV64; virtual;
    class Function HashSize: TMemSize; override;
    Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TFNV64; HashAlgorithm: TFNVHashAlgorithm = algFNV1a); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TFNV64); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property FNV64: TFNV64 read GetFNV64;
    property FNV64Sys: TFNV64Sys read fFNV64Value;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV128Hash                                   
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFNV128Hash - class declaration
===============================================================================}
type
  TFNV128Hash = class(TFNVBaseHash)
  protected
    fFNV128Value:  TFNV128Sys;
    Function GetFNV128: TFNV128; virtual;
    procedure ProcessBuffer_FNV_0(const Buffer; Size: TMemSize); override;
    procedure ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function FNV128ToSys(Hash: TFNV128): TFNV128Sys; virtual;
    class Function FNV128FromSys(Hash: TFNV128Sys): TFNV128; virtual;
    class Function FNV128ToLE(Hash: TFNV128): TFNV128; virtual;
    class Function FNV128ToBE(Hash: TFNV128): TFNV128; virtual;
    class Function FNV128FromLE(Hash: TFNV128): TFNV128; virtual;
    class Function FNV128FromBE(Hash: TFNV128): TFNV128; virtual;
    class Function HashSize: TMemSize; override;
    Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TFNV128; HashAlgorithm: TFNVHashAlgorithm = algFNV1a); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TFNV128); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property FNV128: TFNV128 read GetFNV128;
    property FNV128Sys: TFNV128Sys read fFNV128Value;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV256Hash                                   
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFNV256Hash - class declaration
===============================================================================}
type
  TFNV256Hash = class(TFNVBaseHash)
  protected
    fFNV256Value:  TFNV256Sys;
    Function GetFNV256: TFNV256; virtual;
    procedure ProcessBuffer_FNV_0(const Buffer; Size: TMemSize); override;
    procedure ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function FNV256ToSys(Hash: TFNV256): TFNV256Sys; virtual;
    class Function FNV256FromSys(Hash: TFNV256Sys): TFNV256; virtual;
    class Function FNV256ToLE(Hash: TFNV256): TFNV256; virtual;
    class Function FNV256ToBE(Hash: TFNV256): TFNV256; virtual;
    class Function FNV256FromLE(Hash: TFNV256): TFNV256; virtual;
    class Function FNV256FromBE(Hash: TFNV256): TFNV256; virtual;
    class Function HashSize: TMemSize; override;
    Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TFNV256; HashAlgorithm: TFNVHashAlgorithm = algFNV1a); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TFNV256); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property FNV256: TFNV256 read GetFNV256;
    property FNV256Sys: TFNV256Sys read fFNV256Value;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV512Hash                                   
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFNV512Hash - class declaration
===============================================================================}
type
  TFNV512Hash = class(TFNVBaseHash)
  protected
    fFNV512Value:  TFNV512Sys;
    Function GetFNV512: TFNV512; virtual;
    procedure ProcessBuffer_FNV_0(const Buffer; Size: TMemSize); override;
    procedure ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function FNV512ToSys(Hash: TFNV512): TFNV512Sys; virtual;
    class Function FNV512FromSys(Hash: TFNV512Sys): TFNV512; virtual;
    class Function FNV512ToLE(Hash: TFNV512): TFNV512; virtual;
    class Function FNV512ToBE(Hash: TFNV512): TFNV512; virtual;
    class Function FNV512FromLE(Hash: TFNV512): TFNV512; virtual;
    class Function FNV512FromBE(Hash: TFNV512): TFNV512; virtual;
    class Function HashSize: TMemSize; override;
    Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TFNV512; HashAlgorithm: TFNVHashAlgorithm = algFNV1a); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TFNV512); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property FNV512: TFNV512 read GetFNV512;
    property FNV512Sys: TFNV512Sys read fFNV512Value;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TFNV1024Hash                                                                     
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFNV1024Hash - class declaration
===============================================================================}
type
  TFNV1024Hash = class(TFNVBaseHash)
  protected
    fFNV1024Value:  TFNV1024Sys;
    Function GetFNV1024: TFNV1024; virtual;
    procedure ProcessBuffer_FNV_0(const Buffer; Size: TMemSize); override;
    procedure ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize); override;
    procedure Initialize; override;
  public
    class Function FNV1024ToSys(Hash: TFNV1024): TFNV1024Sys; virtual;
    class Function FNV1024FromSys(Hash: TFNV1024Sys): TFNV1024; virtual;
    class Function FNV1024ToLE(Hash: TFNV1024): TFNV1024; virtual;
    class Function FNV1024ToBE(Hash: TFNV1024): TFNV1024; virtual;
    class Function FNV1024FromLE(Hash: TFNV1024): TFNV1024; virtual;
    class Function FNV1024FromBE(Hash: TFNV1024): TFNV1024; virtual;
    class Function HashSize: TMemSize; override;
    Function HashName: String; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TFNV1024; HashAlgorithm: TFNVHashAlgorithm = algFNV1a); overload; virtual;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TFNV1024); reintroduce;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property FNV1024: TFNV1024 read GetFNV1024;
    property FNV1024Sys: TFNV1024Sys read fFNV1024Value;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{
  Most of the following functions are, for the sake of performance, calling
  direct implementation, but some (context functions and stream and file
  processing) are using TFNV[b]Hash objects and their methods to perform more
  complex tasks.

  Only functions for 32bit and 64bit FNV variants are provided - simply because
  I do not expect anyone to use larger variants (or this unit at all, when we
  are at it :/). But if any demand arises, I can easily add them.
}
{===============================================================================
    Procedural interface - 32bit hash declaration
===============================================================================}

Function FNV32ToStr(const Hash: TFNV32): String;
Function StrToFNV32(const Str: String): TFNV32;
Function TryStrToFNV32(const Str: String; out Hash: TFNV32): Boolean;
Function StrToFNV32Def(const Str: String; Default: TFNV32): TFNV32;

Function CompareFNV32(const A,B: TFNV32): Integer;
Function SameFNV32(const A,B: TFNV32): Boolean;

//------------------------------------------------------------------------------

Function BufferFNV32(const Hash: TFNV32; const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32; overload;
Function BufferFNV32(const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32; overload;

Function AnsiStringFNV32(const Str: AnsiString; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
Function WideStringFNV32(const Str: WideString; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
Function StringFNV32(const Str: String; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;

Function StreamFNV32(Stream: TStream; Count: Int32 = -1; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
Function FileFNV32(const FileName: String; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;

//------------------------------------------------------------------------------
type
  TFNV32Context = type Pointer;

Function FNV32_Init(HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32Context;
procedure FNV32_Update(Context: TFNV32Context; const Buffer; Size: TMemSize);
Function FNV32_Final(var Context: TFNV32Context; const Buffer; Size: TMemSize): TFNV32; overload;
Function FNV32_Final(var Context: TFNV32Context): TFNV32; overload;
Function FNV32_Hash(const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;

{===============================================================================
    Procedural interface - 64bit hash declaration
===============================================================================}

Function FNV64ToStr(const Hash: TFNV64): String;
Function StrToFNV64(const Str: String): TFNV64;
Function TryStrToFNV64(const Str: String; out Hash: TFNV64): Boolean;
Function StrToFNV64Def(const Str: String; Default: TFNV64): TFNV64;

Function CompareFNV64(const A,B: TFNV64): Integer;
Function SameFNV64(const A,B: TFNV64): Boolean;

//------------------------------------------------------------------------------

Function BufferFNV64(const Hash: TFNV64; const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64; overload;
Function BufferFNV64(const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64; overload;

Function AnsiStringFNV64(const Str: AnsiString; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
Function WideStringFNV64(const Str: WideString; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
Function StringFNV64(const Str: String; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;

Function StreamFNV64(Stream: TStream; Count: Int64 = -1; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
Function FileFNV64(const FileName: String; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;

//------------------------------------------------------------------------------
type
  TFNV64Context = type Pointer;

Function FNV64_Init(HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64Context;
procedure FNV64_Update(Context: TFNV64Context; const Buffer; Size: TMemSize);
Function FNV64_Final(var Context: TFNV64Context; const Buffer; Size: TMemSize): TFNV64; overload;
Function FNV64_Final(var Context: TFNV64Context): TFNV64; overload;
Function FNV64_Hash(const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;

implementation

uses
  SysUtils;

{$IFOPT Q+}
  {$DEFINE OveflowChecks}
{$ELSE}
  {$UNDEF OveflowChecks}
{$ENDIF}
{$IFOPT R+}
  {$DEFINE RangeChecks}
{$ELSE}
  {$UNDEF RangeChecks}
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                    Internals
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Arbitrary-length truncated multiplication
===============================================================================}  
{
  Following function takes two untyped arguments (A and B) and treats them as
  unsigned integers of given size (must be multiple of two for 32bit code and
  four for 64bit code) with system endianness, multiplies them and then returns
  low-order part of result with given size in output parameter R, truncating
  overflow into high-order places.
}

procedure FNVTruncatedMul(const A,B; out R; Size: TMemSize);
type
  TCompWord = {$IFDEF CPU64bit}UInt32{$ELSE}UInt16{$ENDIF};
  TCompLong = {$IFDEF CPU64bit}UInt64{$ELSE}UInt32{$ENDIF};
  TCWArrayOverlay = array[0..Pred(SizeOf(TFNV1024Sys) div SizeOf(TCompWord))] of TCompWord;
var
  ArrR:       TCWArrayOverlay absolute R;
  HighIndex:  Integer;

  procedure AddSubproduct(Subproduct: TCompLong; WordIndex: Integer);
  const
    HalfMask = TCompLong(TCompWord(-1));
  var
    Carry:  TCompLong;
  begin
    Carry := 0;
    If Subproduct <> 0 then
      repeat
        Carry := TCompLong(ArrR[WordIndex]) + (Subproduct and HalfMask) + (Carry and HalfMask);
        ArrR[WordIndex] := TCompWord(Carry);
        Subproduct := Subproduct shr (SizeOf(TCompWord) * 8);
        Carry := Carry shr (SizeOf(TCompWord) * 8);
    {$IFDEF ENDIAN_BIG}
        Dec(WordIndex);
      until ((Carry <= 0) and (Subproduct <= 0)) or (WordIndex < 0);
    {$ELSE}
        Inc(WordIndex);
      until ((Carry <= 0) and (Subproduct <= 0)) or (WordIndex > HighIndex);
    {$ENDIF}
  end;

var
  ArrA:   TCWArrayOverlay absolute A;
  ArrB:   TCWArrayOverlay absolute B;
  Index:  Integer;
  i:      Integer;
begin
If (Size < SizeOf(TCompWord)) or (Size > SizeOf(TCWArrayOverlay)) or ((Size and Pred(SizeOf(TCompWord))) <> 0) then
  raise EFNVInvalidValue.CreateFmt('FNVTruncatedMul: Invalid size (%d)',[Size]);
HighIndex := Pred(Integer(Size div SizeOf(TCompWord)));
FillChar(Addr(R)^,Size,0);
{$IFDEF ENDIAN_BIG}
For Index := HighIndex downto 0 do
  For i := HighIndex downto Index do
    AddSubproduct(TCompLong(ArrA[i]) * TCompLong(ArrB[Index + (HighIndex - i)]),Index);
{$ELSE}
For Index := 0 to HighIndex do
  For i := 0 to Index do
    AddSubproduct(TCompLong(ArrA[i]) * TCompLong(ArrB[Index - i]),Index);
{$ENDIF}
end;

{===============================================================================
    Auxiliary functions
===============================================================================}
{
  All following functions expect the hash(es) to be in an interchangeable
  form (ie. not "sys" form).
}
type
  TFNVHashOverlay = packed array[0..Pred(SizeOf(TFNV1024))] of UInt8;

//------------------------------------------------------------------------------

procedure FNVSwapEndian(var Hash; HashSize: TMemSize);
var
  HashOverlay:  TFNVHashOverlay absolute Hash;
  LashLength:   Integer;
  i:            Integer;
  Temp:         UInt8;
begin
LashLength := Integer(HashSize);
For i := 0 to Pred(LashLength div 2) do
  begin
    Temp := HashOverlay[i];
    HashOverlay[i] := HashOverlay[Pred(LashLength) - i];
    HashOverlay[Pred(LashLength) - i] := Temp;
  end;
end;

//------------------------------------------------------------------------------

Function FNVCompare(const A,B; HashSize: TMemSize): Integer;
var
  AOverlay:   TFNVHashOverlay absolute A;
  BOverlay:   TFNVHashOverlay absolute B;
  LashLength: Integer;
  i:          Integer;
begin
LashLength := Integer(HashSize);
Result := 0;
{
  FNV hashes are stored with little endianness, meaning first byte in memory
  is the least significant. But when comparing, we must compare the most
  significant bytes first, therefore going backwards.
}
For i := Pred(LashLength) downto 0 do
  If AOverlay[i] <> BOverlay[i] then
    begin
      If AOverlay[i] > BOverlay[i] then
        Result := +1
      else
        Result := -1;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function FNVSame(const A,B; HashSize: TMemSize): Boolean;
var
  AOverlay:   TFNVHashOverlay absolute A;
  BOverlay:   TFNVHashOverlay absolute B;
  LashLength: Integer;
  i:          Integer;
begin
LashLength := Integer(HashSize);
Result := True;
// order of processed bytes does not matter here
For i := 0 to Pred(LashLength) do
  If AOverlay[i] <> BOverlay[i] then
    begin
      Result := False;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function FNVAsString(const Hash; HashSize: TMemSize): String;
var
  HashOverlay:  TFNVHashOverlay absolute Hash;
  LashLength:   Integer;
  i:            Integer;
begin
LashLength := Integer(HashSize);
Result := StringOfChar('0',LashLength * 2);
For i := 0 to Pred(LashLength) do
  begin
    Result[(i * 2) + 2] := IntToHex(HashOverlay[Pred(LashLength) - i] and $0F,1)[1];
    Result[(i * 2) + 1] := IntToHex(HashOverlay[Pred(LashLength) - i] shr 4,1)[1];
  end;
end;

//------------------------------------------------------------------------------

procedure FNVFromString(const Str: String; out Hash; HashSize: TMemSize);
var
  HashOverlay:  TFNVHashOverlay absolute Hash;
  LashLength:   Integer;
  WorkStr:      String;
  i:            Integer;
begin
LashLength := Integer(HashSize);
If Length(Str) < (LashLength * 2) then
  WorkStr := StringOfChar('0',(LashLength * 2) - Length(Str)) + Str
else If Length(Str) > (LashLength * 2) then
  WorkStr := Copy(Str,Length(Str) - Pred(LashLength * 2),LashLength * 2)
else
  WorkStr := Str;
For i := 0 to Pred(LashLength) do
  HashOverlay[Pred(LashLength) - i] := UInt8(StrToInt('$' + Copy(WorkStr,(i * 2) + 1,2)));
end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TFNVBaseHash
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TFNVBaseHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TFNVBaseHash - protected methods
-------------------------------------------------------------------------------}

procedure TFNVBaseHash.ForceHashAlgorithm(NewValue: TFNVHashAlgorithm);
begin
fHashAlgorithm := NewValue;
If fHashAlgorithm = algFNV1a then
  fProcessBuffer := ProcessBuffer_FNV_1a
else
  fProcessBuffer := ProcessBuffer_FNV_0;
end;

//------------------------------------------------------------------------------

procedure TFNVBaseHash.SetHashAlgorithm(NewValue: TFNVHashAlgorithm);
begin
If not Initialized or Finalized then
  ForceHashAlgorithm(NewValue)
else
  raise EFNVInvalidState.Create('TFNVBaseHash.SetHashAlgorithm: Cannot change algorithm during processing.');
end;

//------------------------------------------------------------------------------

procedure TFNVBaseHash.ProcessBuffer(const Buffer; Size: TMemSize);
begin
fProcessBuffer(Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TFNVBaseHash.Initialize;
begin
inherited;
fHashAlgorithm := algFNV1a;
fProcessBuffer := ProcessBuffer_FNV_1a
end;

{-------------------------------------------------------------------------------
    TFNVBaseHash - public methods
-------------------------------------------------------------------------------}

class Function TFNVBaseHash.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TFNVBaseHash.HashFinalization: Boolean;
begin
Result := False;
end;

//------------------------------------------------------------------------------

Function TFNVBaseHash.HashName: String;
begin
case fHashAlgorithm of
  algFNV0:  Result := 'FNV-0';
  algFNV1:  Result := 'FNV-1';
  algFNV1a: Result := 'FNV-1a';
else
  raise EFNVInvalidValue.CreateFmt('TFNVBaseHash.HashName: Unknown hash algorithm (%d).',[Ord(fHashAlgorithm)]);
end;
end;

//------------------------------------------------------------------------------

constructor TFNVBaseHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TFNVBaseHash then
  ForceHashAlgorithm(TFNVBaseHash(Hash).HashAlgorithm)
else
  raise EFNVIncompatibleClass.CreateFmt('TFNVBaseHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV32Hash
--------------------------------------------------------------------------------
===============================================================================}
const
  FNV32Prime = TFNV32Sys($01000193);

//------------------------------------------------------------------------------
{$IFDEF OveflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

// overflows and range checks are disabled because of the multiplication
Function FNV32Process_0(FNV32: TFNV32Sys; const Buffer; Size: TMemSize): TFNV32Sys;
var
  Buff: PByte;
  i:    Integer;
begin
Result := FNV32;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Result := TFNV32Sys(Result * FNV32Prime) xor TFNV32Sys(Buff^);
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

Function FNV32Process_1a(FNV32: TFNV32Sys; const Buffer; Size: TMemSize): TFNV32Sys;
var
  Buff: PByte;
  i:    Integer;
begin
Result := FNV32;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Result := TFNV32Sys((Result xor TFNV32Sys(Buff^)) * FNV32Prime);
        Inc(Buff);
      end;
  end;
end;

{$IFDEF RangeChecks}{$R+}{$ENDIF}
{$IFDEF OveflowChecks}{$Q+}{$ENDIF}

{===============================================================================
    TFNV32Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TFNV32Hash - protected methods
-------------------------------------------------------------------------------}

Function TFNV32Hash.GetFNV32: TFNV32;
begin
Result := FNV32FromSys(fFNV32Value);
end;

//------------------------------------------------------------------------------

procedure TFNV32Hash.ProcessBuffer_FNV_0(const Buffer; Size: TMemSize);
begin
fFNV32Value := FNV32Process_0(fFNV32Value,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TFNV32Hash.ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize);
begin
fFNV32Value := FNV32Process_1a(fFNV32Value,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TFNV32Hash.Initialize;
begin
inherited;
fFNV32Value := FNV32ToSys(ZeroFNV32);
end;

{-------------------------------------------------------------------------------
    TFNV32Hash - public methods
-------------------------------------------------------------------------------}

class Function TFNV32Hash.FNV32ToSys(Hash: TFNV32): TFNV32Sys;
begin
Result := TFNV32Sys(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV32Hash.FNV32FromSys(Hash: TFNV32Sys): TFNV32;
begin
Result := TFNV32(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV32Hash.FNV32ToLE(Hash: TFNV32): TFNV32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV32Hash.FNV32ToBE(Hash: TFNV32): TFNV32;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV32Hash.FNV32FromLE(Hash: TFNV32): TFNV32;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV32Hash.FNV32FromBE(Hash: TFNV32): TFNV32;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV32Hash.HashSize: TMemSize;
begin
Result := SizeOf(TFNV32);
end;

//------------------------------------------------------------------------------

Function TFNV32Hash.HashName: String;
begin
Result := inherited HashName + '(32)';
end;

//------------------------------------------------------------------------------

constructor TFNV32Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TFNV32Hash then
  fFNV32Value := TFNV32Hash(Hash).FNV32Sys
else
  raise EFNVIncompatibleClass.CreateFmt('TFNV32Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFNV32Hash.CreateAndInitFrom(Hash: TFNV32; HashAlgorithm: TFNVHashAlgorithm = algFNV1a);
begin
CreateAndInit;
ForceHashAlgorithm(HashAlgorithm);
fFNV32Value := FNV32ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TFNV32Hash.Init;
begin
inherited;
If fHashAlgorithm = algFNV0 then
  fFNV32Value := FNV32ToSys(ZeroFNV32)
else
  fFNV32Value := FNV32ToSys(InitialFNV32);
end;

//------------------------------------------------------------------------------

Function TFNV32Hash.Compare(Hash: THashBase): Integer;
var
  Local:  TFNV32;
  Remote: TFNV32;
begin
If Hash is TFNV32Hash then
  begin
    Local := FNV32FromSys(fFNV32Value);
    Remote := TFNV32Hash(Hash).FNV32;
    Result := FNVCompare(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV32Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV32Hash.Same(Hash: THashBase): Boolean;
var
  Local:  TFNV32;
  Remote: TFNV32;
begin
If Hash is TFNV32Hash then
  begin
    Local := FNV32FromSys(fFNV32Value);
    Remote := TFNV32Hash(Hash).FNV32;
    Result := FNVSame(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV32Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV32Hash.AsString: String;
var
  Temp: TFNV32;
begin
Temp := FNV32FromSys(fFNV32Value);
Result := FNVAsString(Temp,HashSize);
end;

//------------------------------------------------------------------------------

procedure TFNV32Hash.FromString(const Str: String);
var
  Temp: TFNV32;
begin
FNVFromString(Str,Temp,HashSize);
fFNV32Value := FNV32ToSys(Temp);
end;

//------------------------------------------------------------------------------

procedure TFNV32Hash.FromStringDef(const Str: String; const Default: TFNV32);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fFNV32Value := FNV32ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TFNV32Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV32;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}FNV32ToBE{$ELSE}FNV32ToLE{$ENDIF}(FNV32FromSys(fFNV32Value));
  heLittle: Temp := FNV32ToLE(FNV32FromSys(fFNV32Value));
  heBig:    Temp := FNV32ToBE(FNV32FromSys(fFNV32Value));
else
 {heDefault}
  Temp := FNV32FromSys(fFNV32Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TFNV32));
end;

//------------------------------------------------------------------------------

procedure TFNV32Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV32;
begin
Temp := ZeroFNV32;
Stream.ReadBuffer(Temp,SizeOf(TFNV32));
case Endianness of
  heSystem: fFNV32Value := FNV32ToSys({$IFDEF ENDIAN_BIG}FNV32FromBE{$ELSE}FNV32FromLE{$ENDIF}(Temp));
  heLittle: fFNV32Value := FNV32ToSys(FNV32FromLE(Temp));
  heBig:    fFNV32Value := FNV32ToSys(FNV32FromBE(Temp));
else
 {heDefault}
  fFNV32Value := FNV32ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV64Hash
--------------------------------------------------------------------------------
===============================================================================}
const
  FNV64Prime = TFNV64Sys($00000100000001B3);

//------------------------------------------------------------------------------
{$IFDEF OveflowChecks}{$Q-}{$ENDIF}
{$IFDEF RangeChecks}{$R-}{$ENDIF}

// overflows and range checks are disabled because of the multiplication
Function FNV64Process_0(FNV64: TFNV64Sys; const Buffer; Size: TMemSize): TFNV64Sys;
var
  Buff:   PByte;
  i:      Integer;
{$IF not Declared(NativeUInt64E)}
  Prime:  TFNV64Sys;
  Temp:   TFNV64Sys;
begin
Prime := FNV64Prime;
{$ELSE}
begin
{$IFEND}
Result := FNV64;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
      {$IF Declared(NativeUInt64E)}
        Result := TFNV64Sys(Result * FNV64Prime) xor TFNV64Sys(Buff^);
      {$ELSE}
        FNVTruncatedMul(Result,Prime,Temp,SizeOf(TFNV64Sys));
        Result := Temp xor TFNV64Sys(Buff^);
      {$IFEND}
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

Function FNV64Process_1a(FNV64: TFNV64Sys; const Buffer; Size: TMemSize): TFNV64Sys;
var
  Buff:   PByte;
  i:      Integer;
{$IF not Declared(NativeUInt64E)}
  Prime:  TFNV64Sys;
  Temp:   TFNV64Sys;
begin
Prime := FNV64Prime;
{$ELSE}
begin
{$IFEND}
Result := FNV64;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
      {$IF Declared(NativeUInt64E)}
        Result := TFNV64Sys((Result xor TFNV64Sys(Buff^)) * FNV64Prime);
      {$ELSE}
        Temp := Result xor TFNV64Sys(Buff^);
        FNVTruncatedMul(Temp,Prime,Result,SizeOf(TFNV64Sys));
      {$IFEND}
        Inc(Buff);
      end;
  end;
end;

{$IFDEF RangeChecks}{$R+}{$ENDIF}
{$IFDEF OveflowChecks}{$Q+}{$ENDIF}
  
{===============================================================================
    TFNV64Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TFNV64Hash - protected methods
-------------------------------------------------------------------------------}

Function TFNV64Hash.GetFNV64: TFNV64;
begin
Result := FNV64FromSys(fFNV64Value);
end;

//------------------------------------------------------------------------------

procedure TFNV64Hash.ProcessBuffer_FNV_0(const Buffer; Size: TMemSize);
begin
fFNV64Value := FNV64Process_0(fFNV64Value,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TFNV64Hash.ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize);
begin
fFNV64Value := FNV64Process_1a(fFNV64Value,Buffer,Size);
end;

//------------------------------------------------------------------------------

procedure TFNV64Hash.Initialize;
begin
inherited;
fFNV64Value := FNV64ToSys(ZeroFNV64);
end;

{-------------------------------------------------------------------------------
    TFNV64Hash - public methods
-------------------------------------------------------------------------------}

class Function TFNV64Hash.FNV64ToSys(Hash: TFNV64): TFNV64Sys;
begin
Result := TFNV64Sys(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV64Hash.FNV64FromSys(Hash: TFNV64Sys): TFNV64;
begin
Result := TFNV64(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV64Hash.FNV64ToLE(Hash: TFNV64): TFNV64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV64Hash.FNV64ToBE(Hash: TFNV64): TFNV64;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV64Hash.FNV64FromLE(Hash: TFNV64): TFNV64;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV64Hash.FNV64FromBE(Hash: TFNV64): TFNV64;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV64Hash.HashSize: TMemSize;
begin
Result := SizeOf(TFNV64);
end;

//------------------------------------------------------------------------------

Function TFNV64Hash.HashName: String;
begin
Result := inherited HashName + '(64)';
end;

//------------------------------------------------------------------------------

constructor TFNV64Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TFNV64Hash then
  fFNV64Value := TFNV64Hash(Hash).FNV64Sys
else
  raise EFNVIncompatibleClass.CreateFmt('TFNV64Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFNV64Hash.CreateAndInitFrom(Hash: TFNV64; HashAlgorithm: TFNVHashAlgorithm = algFNV1a);
begin
CreateAndInit;
ForceHashAlgorithm(HashAlgorithm);
fFNV64Value := FNV64ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TFNV64Hash.Init;
begin
inherited;
If fHashAlgorithm = algFNV0 then
  fFNV64Value := FNV64ToSys(ZeroFNV64)
else
  fFNV64Value := FNV64ToSys(InitialFNV64);
end;

//------------------------------------------------------------------------------

Function TFNV64Hash.Compare(Hash: THashBase): Integer;
var
  Local:  TFNV64;
  Remote: TFNV64;
begin
If Hash is TFNV64Hash then
  begin
    Local := FNV64FromSys(fFNV64Value);
    Remote := TFNV64Hash(Hash).FNV64;
    Result := FNVCompare(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV64Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV64Hash.Same(Hash: THashBase): Boolean;
var
  Local:  TFNV64;
  Remote: TFNV64;
begin
If Hash is TFNV64Hash then
  begin
    Local := FNV64FromSys(fFNV64Value);
    Remote := TFNV64Hash(Hash).FNV64;
    Result := FNVSame(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV64Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV64Hash.AsString: String;
var
  Temp: TFNV64;
begin
Temp := FNV64FromSys(fFNV64Value);
Result := FNVAsString(Temp,HashSize);
end;

//------------------------------------------------------------------------------

procedure TFNV64Hash.FromString(const Str: String);
var
  Temp: TFNV64;
begin
FNVFromString(Str,Temp,HashSize);
fFNV64Value := FNV64ToSys(Temp);
end;

//------------------------------------------------------------------------------

procedure TFNV64Hash.FromStringDef(const Str: String; const Default: TFNV64);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fFNV64Value := FNV64ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TFNV64Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV64;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}FNV64ToBE{$ELSE}FNV64ToLE{$ENDIF}(FNV64FromSys(fFNV64Value));
  heLittle: Temp := FNV64ToLE(FNV64FromSys(fFNV64Value));
  heBig:    Temp := FNV64ToBE(FNV64FromSys(fFNV64Value));
else
 {heDefault}
  Temp := FNV64FromSys(fFNV64Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TFNV64));
end;

//------------------------------------------------------------------------------

procedure TFNV64Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV64;
begin
Temp := ZeroFNV64;
Stream.ReadBuffer(Temp,SizeOf(TFNV64));
case Endianness of
  heSystem: fFNV64Value := FNV64ToSys({$IFDEF ENDIAN_BIG}FNV64FromBE{$ELSE}FNV64FromLE{$ENDIF}(Temp));
  heLittle: fFNV64Value := FNV64ToSys(FNV64FromLE(Temp));
  heBig:    fFNV64Value := FNV64ToSys(FNV64FromBE(Temp));
else
 {heDefault}
  fFNV64Value := FNV64ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV128Hash
--------------------------------------------------------------------------------
===============================================================================}
const
  FNV128Prime: TFNV128Sys = (
  {$IFDEF ENDIAN_BIG}
    0,0,0,0,$01,0,0,0,0,0,0,0,0,0,$01,$3B
  {$ELSE}
    $3B,$01,0,0,0,0,0,0,0,0,0,$01,0,0,0,0
  {$ENDIF});

{===============================================================================
    TFNV128Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TFNV128Hash - protected methods
-------------------------------------------------------------------------------}

Function TFNV128Hash.GetFNV128: TFNV128;
begin
Result := FNV128FromSys(fFNV128Value);
end;

//------------------------------------------------------------------------------

procedure TFNV128Hash.ProcessBuffer_FNV_0(const Buffer; Size: TMemSize);
var
  Buff:   PByte;
  i:      Integer;
  Prime:  TFNV128Sys;
  Temp:   TFNV128Sys;
begin
Prime := FNV128Prime;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        FNVTruncatedMul(fFNV128Value,Prime,Temp,HashSize);
        fFNV128Value := Temp;
        fFNV128Value[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV128Sys)] :=
          fFNV128Value[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV128Sys)] xor Buff^;
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFNV128Hash.ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize);
var
  Buff:   PByte;
  i:      Integer;
  Prime:  TFNV128Sys;
  Temp:   TFNV128Sys;
begin
Prime := FNV128Prime;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Temp := fFNV128Value;
        Temp[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV128Sys)] :=
          Temp[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV128Sys)] xor Buff^;
        FNVTruncatedMul(Temp,Prime,fFNV128Value,HashSize);
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFNV128Hash.Initialize;
begin
inherited;
fFNV128Value := FNV128ToSys(ZeroFNV128);
end;

{-------------------------------------------------------------------------------
    TFNV128Hash - public methods
-------------------------------------------------------------------------------}

class Function TFNV128Hash.FNV128ToSys(Hash: TFNV128): TFNV128Sys;
begin
Result := TFNV128Sys(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV128Hash.FNV128FromSys(Hash: TFNV128Sys): TFNV128;
begin
Result := TFNV128(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV128Hash.FNV128ToLE(Hash: TFNV128): TFNV128;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV128Hash.FNV128ToBE(Hash: TFNV128): TFNV128;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV128Hash.FNV128FromLE(Hash: TFNV128): TFNV128;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV128Hash.FNV128FromBE(Hash: TFNV128): TFNV128;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV128Hash.HashSize: TMemSize;
begin
Result := SizeOf(TFNV128);
end;

//------------------------------------------------------------------------------

Function TFNV128Hash.HashName: String;
begin
Result := inherited HashName + '(128)';
end;

//------------------------------------------------------------------------------

constructor TFNV128Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TFNV128Hash then
  fFNV128Value := TFNV128Hash(Hash).FNV128Sys
else
  raise EFNVIncompatibleClass.CreateFmt('TFNV128Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFNV128Hash.CreateAndInitFrom(Hash: TFNV128; HashAlgorithm: TFNVHashAlgorithm = algFNV1a);
begin
CreateAndInit;
ForceHashAlgorithm(HashAlgorithm);
fFNV128Value := FNV128ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TFNV128Hash.Init;
begin
inherited;
If fHashAlgorithm = algFNV0 then
  fFNV128Value := FNV128ToSys(ZeroFNV128)
else
  fFNV128Value := FNV128ToSys(InitialFNV128);
end;

//------------------------------------------------------------------------------

Function TFNV128Hash.Compare(Hash: THashBase): Integer;
var
  Local:  TFNV128;
  Remote: TFNV128;
begin
If Hash is TFNV128Hash then
  begin
    Local := FNV128FromSys(fFNV128Value);
    Remote := TFNV128Hash(Hash).FNV128;
    Result := FNVCompare(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV128Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV128Hash.Same(Hash: THashBase): Boolean;
var
  Local:  TFNV128;
  Remote: TFNV128;
begin
If Hash is TFNV128Hash then
  begin
    Local := FNV128FromSys(fFNV128Value);
    Remote := TFNV128Hash(Hash).FNV128;
    Result := FNVSame(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV128Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV128Hash.AsString: String;
var
  Temp: TFNV128;
begin
Temp := FNV128FromSys(fFNV128Value);
Result := FNVAsString(Temp,HashSize);
end;

//------------------------------------------------------------------------------

procedure TFNV128Hash.FromString(const Str: String);
var
  Temp: TFNV128;
begin
FNVFromString(Str,Temp,HashSize);
fFNV128Value := FNV128ToSys(Temp);
end;

//------------------------------------------------------------------------------

procedure TFNV128Hash.FromStringDef(const Str: String; const Default: TFNV128);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fFNV128Value := FNV128ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TFNV128Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV128;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}FNV128ToBE{$ELSE}FNV128ToLE{$ENDIF}(FNV128FromSys(fFNV128Value));
  heLittle: Temp := FNV128ToLE(FNV128FromSys(fFNV128Value));
  heBig:    Temp := FNV128ToBE(FNV128FromSys(fFNV128Value));
else
 {heDefault}
  Temp := FNV128FromSys(fFNV128Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TFNV128));
end;

//------------------------------------------------------------------------------

procedure TFNV128Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV128;
begin
Temp := ZeroFNV128;
Stream.ReadBuffer(Temp,SizeOf(TFNV128));
case Endianness of
  heSystem: fFNV128Value := FNV128ToSys({$IFDEF ENDIAN_BIG}FNV128FromBE{$ELSE}FNV128FromLE{$ENDIF}(Temp));
  heLittle: fFNV128Value := FNV128ToSys(FNV128FromLE(Temp));
  heBig:    fFNV128Value := FNV128ToSys(FNV128FromBE(Temp));
else
 {heDefault}
  fFNV128Value := FNV128ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV256Hash
--------------------------------------------------------------------------------
===============================================================================}
const
  FNV256Prime: TFNV256Sys = (
  {$IFDEF ENDIAN_BIG}
    0,0,0,0,0,0,0,0,0,0,$01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$01,$63
  {$ELSE}
    $63,$01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$01,0,0,0,0,0,0,0,0,0,0
  {$ENDIF});

{===============================================================================
    TFNV256Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TFNV256Hash - protected methods
-------------------------------------------------------------------------------}

Function TFNV256Hash.GetFNV256: TFNV256;
begin
Result := FNV256FromSys(fFNV256Value);
end;

//------------------------------------------------------------------------------

procedure TFNV256Hash.ProcessBuffer_FNV_0(const Buffer; Size: TMemSize);
var
  Buff:   PByte;
  i:      Integer;
  Prime:  TFNV256Sys;
  Temp:   TFNV256Sys;
begin
Prime := FNV256Prime;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        FNVTruncatedMul(fFNV256Value,Prime,Temp,HashSize);
        fFNV256Value := Temp;
        fFNV256Value[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV256Sys)] :=
          fFNV256Value[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV256Sys)] xor Buff^;
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFNV256Hash.ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize);
var
  Buff:   PByte;
  i:      Integer;
  Prime:  TFNV256Sys;
  Temp:   TFNV256Sys;
begin
Prime := FNV256Prime;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Temp := fFNV256Value;
        Temp[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV256Sys)] :=
          Temp[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV256Sys)] xor Buff^;
        FNVTruncatedMul(Temp,Prime,fFNV256Value,HashSize);
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFNV256Hash.Initialize;
begin
inherited;
fFNV256Value := FNV256ToSys(ZeroFNV256);
end;

{-------------------------------------------------------------------------------
    TFNV256Hash - public methods
-------------------------------------------------------------------------------}

class Function TFNV256Hash.FNV256ToSys(Hash: TFNV256): TFNV256Sys;
begin
Result := TFNV256Sys(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV256Hash.FNV256FromSys(Hash: TFNV256Sys): TFNV256;
begin
Result := TFNV256(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV256Hash.FNV256ToLE(Hash: TFNV256): TFNV256;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV256Hash.FNV256ToBE(Hash: TFNV256): TFNV256;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV256Hash.FNV256FromLE(Hash: TFNV256): TFNV256;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV256Hash.FNV256FromBE(Hash: TFNV256): TFNV256;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV256Hash.HashSize: TMemSize;
begin
Result := SizeOf(TFNV256);
end;

//------------------------------------------------------------------------------

Function TFNV256Hash.HashName: String;
begin
Result := inherited HashName + '(256)';
end;

//------------------------------------------------------------------------------

constructor TFNV256Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TFNV256Hash then
  fFNV256Value := TFNV256Hash(Hash).FNV256Sys
else
  raise EFNVIncompatibleClass.CreateFmt('TFNV256Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFNV256Hash.CreateAndInitFrom(Hash: TFNV256; HashAlgorithm: TFNVHashAlgorithm = algFNV1a);
begin
CreateAndInit;
ForceHashAlgorithm(HashAlgorithm);
fFNV256Value := FNV256ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TFNV256Hash.Init;
begin
inherited;
If fHashAlgorithm = algFNV0 then
  fFNV256Value := FNV256ToSys(ZeroFNV256)
else
  fFNV256Value := FNV256ToSys(InitialFNV256);
end;

//------------------------------------------------------------------------------

Function TFNV256Hash.Compare(Hash: THashBase): Integer;
var
  Local:  TFNV256;
  Remote: TFNV256;
begin
If Hash is TFNV256Hash then
  begin
    Local := FNV256FromSys(fFNV256Value);
    Remote := TFNV256Hash(Hash).FNV256;
    Result := FNVCompare(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV256Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV256Hash.Same(Hash: THashBase): Boolean;
var
  Local:  TFNV256;
  Remote: TFNV256;
begin
If Hash is TFNV256Hash then
  begin
    Local := FNV256FromSys(fFNV256Value);
    Remote := TFNV256Hash(Hash).FNV256;
    Result := FNVSame(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV256Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV256Hash.AsString: String;
var
  Temp: TFNV256;
begin
Temp := FNV256FromSys(fFNV256Value);
Result := FNVAsString(Temp,HashSize);
end;

//------------------------------------------------------------------------------

procedure TFNV256Hash.FromString(const Str: String);
var
  Temp: TFNV256;
begin
FNVFromString(Str,Temp,HashSize);
fFNV256Value := FNV256ToSys(Temp);
end;

//------------------------------------------------------------------------------

procedure TFNV256Hash.FromStringDef(const Str: String; const Default: TFNV256);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fFNV256Value := FNV256ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TFNV256Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV256;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}FNV256ToBE{$ELSE}FNV256ToLE{$ENDIF}(FNV256FromSys(fFNV256Value));
  heLittle: Temp := FNV256ToLE(FNV256FromSys(fFNV256Value));
  heBig:    Temp := FNV256ToBE(FNV256FromSys(fFNV256Value));
else
 {heDefault}
  Temp := FNV256FromSys(fFNV256Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TFNV256));
end;

//------------------------------------------------------------------------------

procedure TFNV256Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV256;
begin
Temp := ZeroFNV256;
Stream.ReadBuffer(Temp,SizeOf(TFNV256));
case Endianness of
  heSystem: fFNV256Value := FNV256ToSys({$IFDEF ENDIAN_BIG}FNV256FromBE{$ELSE}FNV256FromLE{$ENDIF}(Temp));
  heLittle: fFNV256Value := FNV256ToSys(FNV256FromLE(Temp));
  heBig:    fFNV256Value := FNV256ToSys(FNV256FromBE(Temp));
else
 {heDefault}
  fFNV256Value := FNV256ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TFNV512Hash
--------------------------------------------------------------------------------
===============================================================================}
const
  FNV512Prime: TFNV512Sys = (
  {$IFDEF ENDIAN_BIG}
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$01,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$01,$57
  {$ELSE}
    $57,$01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,$01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  {$ENDIF});

{===============================================================================
    TFNV512Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TFNV512Hash - protected methods
-------------------------------------------------------------------------------}

Function TFNV512Hash.GetFNV512: TFNV512;
begin
Result := FNV512FromSys(fFNV512Value);
end;

//------------------------------------------------------------------------------

procedure TFNV512Hash.ProcessBuffer_FNV_0(const Buffer; Size: TMemSize);
var
  Buff:   PByte;
  i:      Integer;
  Prime:  TFNV512Sys;
  Temp:   TFNV512Sys;
begin
Prime := FNV512Prime;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        FNVTruncatedMul(fFNV512Value,Prime,Temp,HashSize);
        fFNV512Value := Temp;
        fFNV512Value[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV512Sys)] :=
          fFNV512Value[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV512Sys)] xor Buff^;
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFNV512Hash.ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize);
var
  Buff:   PByte;
  i:      Integer;
  Prime:  TFNV512Sys;
  Temp:   TFNV512Sys;
begin
Prime := FNV512Prime;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Temp := fFNV512Value;
        Temp[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV512Sys)] :=
          Temp[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV512Sys)] xor Buff^;
        FNVTruncatedMul(Temp,Prime,fFNV512Value,HashSize);
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFNV512Hash.Initialize;
begin
inherited;
fFNV512Value := FNV512ToSys(ZeroFNV512);
end;

{-------------------------------------------------------------------------------
    TFNV512Hash - public methods
-------------------------------------------------------------------------------}

class Function TFNV512Hash.FNV512ToSys(Hash: TFNV512): TFNV512Sys;
begin
Result := TFNV512Sys(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV512Hash.FNV512FromSys(Hash: TFNV512Sys): TFNV512;
begin
Result := TFNV512(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV512Hash.FNV512ToLE(Hash: TFNV512): TFNV512;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV512Hash.FNV512ToBE(Hash: TFNV512): TFNV512;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV512Hash.FNV512FromLE(Hash: TFNV512): TFNV512;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV512Hash.FNV512FromBE(Hash: TFNV512): TFNV512;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV512Hash.HashSize: TMemSize;
begin
Result := SizeOf(TFNV512);
end;

//------------------------------------------------------------------------------

Function TFNV512Hash.HashName: String;
begin
Result := inherited HashName + '(512)';
end;

//------------------------------------------------------------------------------

constructor TFNV512Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TFNV512Hash then
  fFNV512Value := TFNV512Hash(Hash).FNV512Sys
else
  raise EFNVIncompatibleClass.CreateFmt('TFNV512Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFNV512Hash.CreateAndInitFrom(Hash: TFNV512; HashAlgorithm: TFNVHashAlgorithm = algFNV1a);
begin
CreateAndInit;
ForceHashAlgorithm(HashAlgorithm);
fFNV512Value := FNV512ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TFNV512Hash.Init;
begin
inherited;
If fHashAlgorithm = algFNV0 then
  fFNV512Value := FNV512ToSys(ZeroFNV512)
else
  fFNV512Value := FNV512ToSys(InitialFNV512);
end;

//------------------------------------------------------------------------------

Function TFNV512Hash.Compare(Hash: THashBase): Integer;
var
  Local:  TFNV512;
  Remote: TFNV512;
begin
If Hash is TFNV512Hash then
  begin
    Local := FNV512FromSys(fFNV512Value);
    Remote := TFNV512Hash(Hash).FNV512;
    Result := FNVCompare(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV512Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV512Hash.Same(Hash: THashBase): Boolean;
var
  Local:  TFNV512;
  Remote: TFNV512;
begin
If Hash is TFNV512Hash then
  begin
    Local := FNV512FromSys(fFNV512Value);
    Remote := TFNV512Hash(Hash).FNV512;
    Result := FNVSame(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV512Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV512Hash.AsString: String;
var
  Temp: TFNV512;
begin
Temp := FNV512FromSys(fFNV512Value);
Result := FNVAsString(Temp,HashSize);
end;

//------------------------------------------------------------------------------

procedure TFNV512Hash.FromString(const Str: String);
var
  Temp: TFNV512;
begin
FNVFromString(Str,Temp,HashSize);
fFNV512Value := FNV512ToSys(Temp);
end;

//------------------------------------------------------------------------------

procedure TFNV512Hash.FromStringDef(const Str: String; const Default: TFNV512);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fFNV512Value := FNV512ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TFNV512Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV512;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}FNV512ToBE{$ELSE}FNV512ToLE{$ENDIF}(FNV512FromSys(fFNV512Value));
  heLittle: Temp := FNV512ToLE(FNV512FromSys(fFNV512Value));
  heBig:    Temp := FNV512ToBE(FNV512FromSys(fFNV512Value));
else
 {heDefault}
  Temp := FNV512FromSys(fFNV512Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TFNV512));
end;

//------------------------------------------------------------------------------

procedure TFNV512Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV512;
begin
Temp := ZeroFNV512;
Stream.ReadBuffer(Temp,SizeOf(TFNV512));
case Endianness of
  heSystem: fFNV512Value := FNV512ToSys({$IFDEF ENDIAN_BIG}FNV512FromBE{$ELSE}FNV512FromLE{$ENDIF}(Temp));
  heLittle: fFNV512Value := FNV512ToSys(FNV512FromLE(Temp));
  heBig:    fFNV512Value := FNV512ToSys(FNV512FromBE(Temp));
else
 {heDefault}
  fFNV512Value := FNV512ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TFNV1024Hash
--------------------------------------------------------------------------------
===============================================================================}
const
  FNV1024Prime: TFNV1024Sys = (
  {$IFDEF ENDIAN_BIG}
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,$01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$01,$8D,
  {$ELSE}
    $8D,$01,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$01,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  {$ENDIF});

{===============================================================================
    TFNV1024Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TFNV1024Hash - protected methods
-------------------------------------------------------------------------------}

Function TFNV1024Hash.GetFNV1024: TFNV1024;
begin
Result := FNV1024FromSys(fFNV1024Value);
end;

//------------------------------------------------------------------------------

procedure TFNV1024Hash.ProcessBuffer_FNV_0(const Buffer; Size: TMemSize);
var
  Buff:   PByte;
  i:      Integer;
  Prime:  TFNV1024Sys;
  Temp:   TFNV1024Sys;
begin
Prime := FNV1024Prime;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        FNVTruncatedMul(fFNV1024Value,Prime,Temp,HashSize);
        fFNV1024Value := Temp;
        fFNV1024Value[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV1024Sys)] :=
          fFNV1024Value[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV1024Sys)] xor Buff^;
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFNV1024Hash.ProcessBuffer_FNV_1a(const Buffer; Size: TMemSize);
var
  Buff:   PByte;
  i:      Integer;
  Prime:  TFNV1024Sys;
  Temp:   TFNV1024Sys;
begin
Prime := FNV1024Prime;
If Size > 0 then
  begin
    Buff := @Buffer;
    For i := 0 to Pred(Size) do
      begin
        Temp := fFNV1024Value;
        Temp[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV1024Sys)] :=
          Temp[{$IFDEF ENDIAN_BIG}High{$ELSE}Low{$ENDIF}(TFNV1024Sys)] xor Buff^;
        FNVTruncatedMul(Temp,Prime,fFNV1024Value,HashSize);
        Inc(Buff);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFNV1024Hash.Initialize;
begin
inherited;
fFNV1024Value := FNV1024ToSys(ZeroFNV1024);
end;

{-------------------------------------------------------------------------------
    TFNV1024Hash - public methods
-------------------------------------------------------------------------------}

class Function TFNV1024Hash.FNV1024ToSys(Hash: TFNV1024): TFNV1024Sys;
begin
Result := TFNV1024Sys(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV1024Hash.FNV1024FromSys(Hash: TFNV1024Sys): TFNV1024;
begin
Result := TFNV1024(Hash);
{$IFDEF ENDIAN_BIG}FNVSwapEndian(Result,HashSize);{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TFNV1024Hash.FNV1024ToLE(Hash: TFNV1024): TFNV1024;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV1024Hash.FNV1024ToBE(Hash: TFNV1024): TFNV1024;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV1024Hash.FNV1024FromLE(Hash: TFNV1024): TFNV1024;
begin
Result := Hash;
end;

//------------------------------------------------------------------------------

class Function TFNV1024Hash.FNV1024FromBE(Hash: TFNV1024): TFNV1024;
begin
Result := Hash;
FNVSwapEndian(Result,HashSize);
end;

//------------------------------------------------------------------------------

class Function TFNV1024Hash.HashSize: TMemSize;
begin
Result := SizeOf(TFNV1024);
end;

//------------------------------------------------------------------------------

Function TFNV1024Hash.HashName: String;
begin
Result := inherited HashName + '(1024)';
end;

//------------------------------------------------------------------------------

constructor TFNV1024Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TFNV1024Hash then
  fFNV1024Value := TFNV1024Hash(Hash).FNV1024Sys
else
  raise EFNVIncompatibleClass.CreateFmt('TFNV1024Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TFNV1024Hash.CreateAndInitFrom(Hash: TFNV1024; HashAlgorithm: TFNVHashAlgorithm = algFNV1a);
begin
CreateAndInit;
ForceHashAlgorithm(HashAlgorithm);
fFNV1024Value := FNV1024ToSys(Hash);
end;

//------------------------------------------------------------------------------

procedure TFNV1024Hash.Init;
begin
inherited;
If fHashAlgorithm = algFNV0 then
  fFNV1024Value := FNV1024ToSys(ZeroFNV1024)
else
  fFNV1024Value := FNV1024ToSys(InitialFNV1024);
end;

//------------------------------------------------------------------------------

Function TFNV1024Hash.Compare(Hash: THashBase): Integer;
var
  Local:  TFNV1024;
  Remote: TFNV1024;
begin
If Hash is TFNV1024Hash then
  begin
    Local := FNV1024FromSys(fFNV1024Value);
    Remote := TFNV1024Hash(Hash).FNV1024;
    Result := FNVCompare(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV1024Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV1024Hash.Same(Hash: THashBase): Boolean;
var
  Local:  TFNV1024;
  Remote: TFNV1024;
begin
If Hash is TFNV1024Hash then
  begin
    Local := FNV1024FromSys(fFNV1024Value);
    Remote := TFNV1024Hash(Hash).FNV1024;
    Result := FNVSame(Local,Remote,HashSize);
  end
else raise EFNVIncompatibleClass.CreateFmt('TFNV1024Hash.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TFNV1024Hash.AsString: String;
var
  Temp: TFNV1024;
begin
Temp := FNV1024FromSys(fFNV1024Value);
Result := FNVAsString(Temp,HashSize);
end;

//------------------------------------------------------------------------------

procedure TFNV1024Hash.FromString(const Str: String);
var
  Temp: TFNV1024;
begin
FNVFromString(Str,Temp,HashSize);
fFNV1024Value := FNV1024ToSys(Temp);
end;

//------------------------------------------------------------------------------

procedure TFNV1024Hash.FromStringDef(const Str: String; const Default: TFNV1024);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fFNV1024Value := FNV1024ToSys(Default);
end;

//------------------------------------------------------------------------------

procedure TFNV1024Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV1024;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}FNV1024ToBE{$ELSE}FNV1024ToLE{$ENDIF}(FNV1024FromSys(fFNV1024Value));
  heLittle: Temp := FNV1024ToLE(FNV1024FromSys(fFNV1024Value));
  heBig:    Temp := FNV1024ToBE(FNV1024FromSys(fFNV1024Value));
else
 {heDefault}
  Temp := FNV1024FromSys(fFNV1024Value);
end;
Stream.WriteBuffer(Temp,SizeOf(TFNV1024));
end;

//------------------------------------------------------------------------------

procedure TFNV1024Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TFNV1024;
begin
Temp := ZeroFNV1024;
Stream.ReadBuffer(Temp,SizeOf(TFNV1024));
case Endianness of
  heSystem: fFNV1024Value := FNV1024ToSys({$IFDEF ENDIAN_BIG}FNV1024FromBE{$ELSE}FNV1024FromLE{$ENDIF}(Temp));
  heLittle: fFNV1024Value := FNV1024ToSys(FNV1024FromLE(Temp));
  heBig:    fFNV1024Value := FNV1024ToSys(FNV1024FromBE(Temp));
else
 {heDefault}
  fFNV1024Value := FNV1024ToSys(Temp);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - 32bit hash implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Procedural interface - 32bit hash utility functions
-------------------------------------------------------------------------------}

Function FNV32ToStr(const Hash: TFNV32): String;
begin
Result := FNVAsString(Hash,SizeOf(TFNV32));
end;

//------------------------------------------------------------------------------

Function StrToFNV32(const Str: String): TFNV32;
begin
FNVFromString(Str,Result,SizeOf(TFNV32));
end;

//------------------------------------------------------------------------------

Function TryStrToFNV32(const Str: String; out Hash: TFNV32): Boolean;
begin
try
  FNVFromString(Str,Hash,SizeOf(TFNV32));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToFNV32Def(const Str: String; Default: TFNV32): TFNV32;
begin
If not TryStrToFNV32(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareFNV32(const A,B: TFNV32): Integer;
begin
Result := FNVCompare(A,B,SizeOf(TFNV32));
end;

//------------------------------------------------------------------------------

Function SameFNV32(const A,B: TFNV32): Boolean;
begin
Result := FNVSame(A,B,SizeOf(TFNV32));
end;

{-------------------------------------------------------------------------------
    Procedural interface - 32bit hash processing functions
-------------------------------------------------------------------------------}

Function BufferFNV32(const Hash: TFNV32; const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
begin
If HashAlgorithm = algFNV1a then
  Result := TFNV32Hash.FNV32FromSys(FNV32Process_1a(TFNV32Hash.FNV32ToSys(Hash),Buffer,Size))
else
  Result := TFNV32Hash.FNV32FromSys(FNV32Process_0(TFNV32Hash.FNV32ToSys(Hash),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function BufferFNV32(const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
begin
case HashAlgorithm of
  algFNV0:  Result := TFNV32Hash.FNV32FromSys(FNV32Process_0(TFNV32Hash.FNV32ToSys(ZeroFNV32),Buffer,Size));
  algFNV1:  Result := TFNV32Hash.FNV32FromSys(FNV32Process_0(TFNV32Hash.FNV32ToSys(InitialFNV32),Buffer,Size));
  algFNV1a: Result := TFNV32Hash.FNV32FromSys(FNV32Process_1a(TFNV32Hash.FNV32ToSys(InitialFNV32),Buffer,Size));
else
  raise EFNVInvalidValue.CreateFmt('BufferFNV32: Unknown hash algorithm (%d).',[Ord(HashAlgorithm)]);
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringFNV32(const Str: AnsiString; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
begin
Result := BufferFNV32(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar),HashAlgorithm);
end;

//------------------------------------------------------------------------------

Function WideStringFNV32(const Str: WideString; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
begin
Result := BufferFNV32(PWideChar(Str)^,Length(Str) * SizeOf(WideChar),HashAlgorithm);
end;

//------------------------------------------------------------------------------

Function StringFNV32(const Str: String; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
begin
Result := BufferFNV32(PChar(Str)^,Length(Str) * SizeOf(Char),HashAlgorithm);
end;

//------------------------------------------------------------------------------

Function StreamFNV32(Stream: TStream; Count: Int32 = -1; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
var
  Hasher: TFNV32Hash;
begin
Hasher := TFNV32Hash.Create;
try
  Hasher.HashAlgorithm := HashAlgorithm;
  Hasher.HashStream(Stream,Count);
  Result := Hasher.FNV32;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileFNV32(const FileName: String; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
var
  Hasher: TFNV32Hash;
begin
Hasher := TFNV32Hash.Create;
try
  Hasher.HashAlgorithm := HashAlgorithm;
  Hasher.HashFile(FileName);
  Result := Hasher.FNV32;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - 32bit hash context functions
-------------------------------------------------------------------------------}

Function FNV32_Init(HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32Context;
var
  Hasher: TFNV32Hash;
begin
Hasher := TFNV32Hash.Create;
Hasher.HashAlgorithm := HashAlgorithm;
Hasher.Init;
Result := TFNV32Context(Hasher);
end;

//------------------------------------------------------------------------------

procedure FNV32_Update(Context: TFNV32Context; const Buffer; Size: TMemSize);
begin
TFNV32Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function FNV32_Final(var Context: TFNV32Context; const Buffer; Size: TMemSize): TFNV32;
begin
FNV32_Update(Context,Buffer,Size);
Result := FNV32_Final(Context);
end;

//------------------------------------------------------------------------------

Function FNV32_Final(var Context: TFNV32Context): TFNV32;
begin
TFNV32Hash(Context).Final;
Result := TFNV32Hash(Context).FNV32;
FreeAndNil(TFNV32Hash(Context));
end;

//------------------------------------------------------------------------------

Function FNV32_Hash(const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV32;
begin
Result := BufferFNV32(Buffer,Size,HashAlgorithm);
end;

{===============================================================================
    Procedural interface - 64bit hash implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Procedural interface - 64bit hash utility functions
-------------------------------------------------------------------------------}

Function FNV64ToStr(const Hash: TFNV64): String;
begin
Result := FNVAsString(Hash,SizeOf(TFNV64));
end;

//------------------------------------------------------------------------------

Function StrToFNV64(const Str: String): TFNV64;
begin
FNVFromString(Str,Result,SizeOf(TFNV64));
end;

//------------------------------------------------------------------------------

Function TryStrToFNV64(const Str: String; out Hash: TFNV64): Boolean;
begin
try
  FNVFromString(Str,Hash,SizeOf(TFNV64));
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function StrToFNV64Def(const Str: String; Default: TFNV64): TFNV64;
begin
If not TryStrToFNV64(Str,Result) then
  Result := Default;
end;

//------------------------------------------------------------------------------

Function CompareFNV64(const A,B: TFNV64): Integer;
begin
Result := FNVCompare(A,B,SizeOf(TFNV64));
end;

//------------------------------------------------------------------------------

Function SameFNV64(const A,B: TFNV64): Boolean;
begin
Result := FNVSame(A,B,SizeOf(TFNV64));
end;

{-------------------------------------------------------------------------------
    Procedural interface - 64bit hash processing functions
-------------------------------------------------------------------------------}

Function BufferFNV64(const Hash: TFNV64; const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
begin
If HashAlgorithm = algFNV1a then
  Result := TFNV64Hash.FNV64FromSys(FNV64Process_1a(TFNV64Hash.FNV64ToSys(Hash),Buffer,Size))
else
  Result := TFNV64Hash.FNV64FromSys(FNV64Process_0(TFNV64Hash.FNV64ToSys(Hash),Buffer,Size));
end;

//------------------------------------------------------------------------------

Function BufferFNV64(const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
begin
case HashAlgorithm of
  algFNV0:  Result := TFNV64Hash.FNV64FromSys(FNV64Process_0(TFNV64Hash.FNV64ToSys(ZeroFNV64),Buffer,Size));
  algFNV1:  Result := TFNV64Hash.FNV64FromSys(FNV64Process_0(TFNV64Hash.FNV64ToSys(InitialFNV64),Buffer,Size));
  algFNV1a: Result := TFNV64Hash.FNV64FromSys(FNV64Process_1a(TFNV64Hash.FNV64ToSys(InitialFNV64),Buffer,Size));
else
  raise EFNVInvalidValue.CreateFmt('BufferFNV64: Unknown hash algorithm (%d).',[Ord(HashAlgorithm)]);
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringFNV64(const Str: AnsiString; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
begin
Result := BufferFNV64(PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar),HashAlgorithm);
end;

//------------------------------------------------------------------------------

Function WideStringFNV64(const Str: WideString; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
begin
Result := BufferFNV64(PWideChar(Str)^,Length(Str) * SizeOf(WideChar),HashAlgorithm);
end;

//------------------------------------------------------------------------------

Function StringFNV64(const Str: String; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
begin
Result := BufferFNV64(PChar(Str)^,Length(Str) * SizeOf(Char),HashAlgorithm);
end;

//------------------------------------------------------------------------------

Function StreamFNV64(Stream: TStream; Count: Int64 = -1; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
var
  Hasher: TFNV64Hash;
begin
Hasher := TFNV64Hash.Create;
try
  Hasher.HashAlgorithm := HashAlgorithm;
  Hasher.HashStream(Stream,Count);
  Result := Hasher.FNV64;
finally
  Hasher.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileFNV64(const FileName: String; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
var
  Hasher: TFNV64Hash;
begin
Hasher := TFNV64Hash.Create;
try
  Hasher.HashAlgorithm := HashAlgorithm;
  Hasher.HashFile(FileName);
  Result := Hasher.FNV64;
finally
  Hasher.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - 64bit hash context functions
-------------------------------------------------------------------------------}

Function FNV64_Init(HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64Context;
var
  Hasher: TFNV64Hash;
begin
Hasher := TFNV64Hash.Create;
Hasher.HashAlgorithm := HashAlgorithm;
Hasher.Init;
Result := TFNV64Context(Hasher);
end;

//------------------------------------------------------------------------------

procedure FNV64_Update(Context: TFNV64Context; const Buffer; Size: TMemSize);
begin
TFNV64Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function FNV64_Final(var Context: TFNV64Context; const Buffer; Size: TMemSize): TFNV64;
begin
FNV64_Update(Context,Buffer,Size);
Result := FNV64_Final(Context);
end;

//------------------------------------------------------------------------------

Function FNV64_Final(var Context: TFNV64Context): TFNV64;
begin
TFNV64Hash(Context).Final;
Result := TFNV64Hash(Context).FNV64;
FreeAndNil(TFNV64Hash(Context));
end;

//------------------------------------------------------------------------------

Function FNV64_Hash(const Buffer; Size: TMemSize; HashAlgorithm: TFNVHashAlgorithm = algFNV1a): TFNV64;
begin
Result := BufferFNV64(Buffer,Size,HashAlgorithm);
end;

end.
