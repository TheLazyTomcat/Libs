{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SHA-2 calculation

    SHA-2 consists of two groups of hashes. First group is calculated using
    32 bit long words and contains SHA-224 and SHA-256. The other group is
    calculated using 64 bit long words and contains all other hashes.

    Both groups are completely implemented, therefore following hashes are
    implemented and supported by this library:

        SHA-224
        SHA-256
        SHA-384
        SHA-512
        SHA-512/224
        SHA-512/256

    Inheritance tree of hash classes looks like this (star marks an abstract
    class that must not be directy instantiated):

       *TBlockHash --- *TSHA2Hash --- *TSHA2Hash_32 --- TSHA224Hash
                                   |                 |- TSHA256Hash
                                   |
                                   |- *TSHA2Hash_64 --- TSHA384Hash
                                                     |- TSHA512Hash
                                                     |- TSHA512_224Hash
                                                     |- TSHA512_256Hash

    Given the nature of calculation, 64bit-based hashes performs poorly on
    32bit systems. On the other hand, they seem to give better performance on
    64bit systems than 32bit-based hashes.

  Version 1.1.3 (2020-07-13)

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

      github.com/TheLazyTomcat/Lib.SHA2

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes
    BitOps   - github.com/TheLazyTomcat/Lib.BitOps
    HashBase - github.com/TheLazyTomcat/Lib.HashBase
    StrRect  - github.com/TheLazyTomcat/Lib.StrRect

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit SHA2;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
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

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

interface

uses
  Classes,
  AuxTypes, HashBase;

{===============================================================================
    Auxiliary types, constants and functions
===============================================================================}
type
  UInt128 = packed record
    case Integer of
      0:(Lo,Hi:   UInt64);
      1:(Bytes:   packed array[0..15] of UInt8);
      2:(Words:   packed array[0..7] of UInt16);
      3:(DWords:  packed array[0..3] of UInt32);
      4:(QWords:  packed array[0..1] of UInt64);
  end;
  PUInt128 = ^UInt128;

  OctaWord = UInt128;
  POctaWord = ^OctaWord;

  OWord = OctaWord;
  POWord = ^OWord;

const
  ZeroUInt128: UInt128 = (Lo: 0; Hi: 0);

Function BuildOctaWord(Lo,Hi: UInt64): UInt128; overload;
Function BuildOctaWord(Lo: UInt64): UInt128; overload;{$IFDEF CanInline} inline; {$ENDIF}

procedure IncrementOctaWord(var Value: UInt128; Increment: UInt128);

Function SizeToMessageLength(Size: UInt64): UInt128;
Function MessageLengthToSize(MessageLength: UInt128): UInt64;

Function EndianSwap(Value: UInt128): UInt128; overload;
procedure EndianSwapValue(var Value: UInt128); overload;{$IFDEF CanInline} inline; {$ENDIF}

{===============================================================================
    Common types and constants
===============================================================================}
{
  Bytes in types TSHA224 trough TSHA512_256 are always ordered from the most
  significant byte to least significant byte (big endian).
  
  Types TSHA*Sys has no such guarantee and their internal structure depends on
  current implementation.

  SHA-2 does not differ in little and big endian form, as it is not a single
  quantity, therefore methods like SHA*ToLE or SHA*ToBE do nothing and are
  present only for the sake of completeness.
}
type
  TSHA2_32 = packed array[0..31] of UInt8;  PSHA2_32 = ^TSHA2_32;

  TSHA224 = type TSHA2_32;                  PSHA224 = ^TSHA224;
  TSHA256 = type TSHA2_32;                  PSHA256 = ^TSHA256;

  TSHA2_64 = packed array[0..63] of UInt8;  PSHA2_64 = ^TSHA2_64;

  TSHA384 = type TSHA2_64;                  PSHA384 = ^TSHA384;
  TSHA512 = type TSHA2_64;                  PSHA512 = ^TSHA512;

  TSHA512_224 = type TSHA2_64;              PSHA512_224 = ^TSHA512_224;
  TSHA512_256 = type TSHA2_64;              PSHA512_256 = ^TSHA512_256;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  TSHA2Function = (fnSHA224,fnSHA256,fnSHA384,fnSHA512,fnSHA512_224,fnSHA512_256);

  TSHA2 = record
    case HashFunction: TSHA2Function of
      fnSHA224:     (SHA224:     TSHA224);
      fnSHA256:     (SHA256:     TSHA256);
      fnSHA384:     (SHA384:     TSHA384);
      fnSHA512:     (SHA512:     TSHA512);
      fnSHA512_224: (SHA512_224: TSHA512_224);
      fnSHA512_256: (SHA512_256: TSHA512_256);
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

  TSHA2Sys_32 = packed record
    PartA:  UInt32;
    PartB:  UInt32;
    PartC:  UInt32;
    PartD:  UInt32;
    PartE:  UInt32;
    PartF:  UInt32;
    PartG:  UInt32;
    PartH:  UInt32;
  end;
  PSHA2Sys_32 = ^TSHA2Sys_32;

  TSHA224Sys = type TSHA2Sys_32;      PSHA224Sys = ^TSHA224Sys;
  TSHA256Sys = type TSHA2Sys_32;      PSHA256Sys = ^TSHA256Sys;

  TSHA2Sys_64 = packed record
    PartA:  UInt64;
    PartB:  UInt64;
    PartC:  UInt64;
    PartD:  UInt64;
    PartE:  UInt64;
    PartF:  UInt64;
    PartG:  UInt64;
    PartH:  UInt64;
  end;
  PSHA2Sys_64 = ^TSHA2Sys_64;

  TSHA384Sys = type TSHA2Sys_64;      PSHA384Sys = ^TSHA384Sys;
  TSHA512Sys = type TSHA2Sys_64;      PSHA512Sys = ^TSHA512Sys;

  TSHA512_224Sys = type TSHA2Sys_64;  PSHA512_224Sys = ^TSHA512_224Sys;
  TSHA512_256Sys = type TSHA2Sys_64;  PSHA512_256Sys = ^TSHA512_256Sys;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

const
  InitialSHA224: TSHA224 =
   ($C1,$05,$9E,$D8,$36,$7C,$D5,$07,$30,$70,$DD,$17,$F7,$0E,$59,$39,
    $FF,$C0,$0B,$31,$68,$58,$15,$11,$64,$F9,$8F,$A7,$BE,$FA,$4F,$A4);

  InitialSHA256: TSHA256 =
    ($6A,$09,$E6,$67,$BB,$67,$AE,$85,$3C,$6E,$f3,$72,$A5,$4f,$f5,$3A,
     $51,$0E,$52,$7f,$9B,$05,$68,$8C,$1F,$83,$d9,$AB,$5B,$E0,$CD,$19);

  InitialSHA384: TSHA384 =
    ($CB,$BB,$9D,$5D,$C1,$05,$9E,$D8,$62,$9A,$29,$2A,$36,$7C,$D5,$07,
     $91,$59,$01,$5A,$30,$70,$DD,$17,$15,$2F,$EC,$D8,$F7,$0E,$59,$39,
     $67,$33,$26,$67,$FF,$C0,$0B,$31,$8E,$B4,$4A,$87,$68,$58,$15,$11,
     $DB,$0C,$2E,$0D,$64,$F9,$8F,$A7,$47,$B5,$48,$1D,$BE,$FA,$4F,$A4);

  InitialSHA512: TSHA512 =
    ($6A,$09,$E6,$67,$F3,$BC,$C9,$08,$BB,$67,$AE,$85,$84,$CA,$A7,$3B,
     $3C,$6E,$F3,$72,$FE,$94,$F8,$2B,$A5,$4F,$F5,$3A,$5F,$1D,$36,$F1,
     $51,$0E,$52,$7F,$AD,$E6,$82,$D1,$9B,$05,$68,$8C,$2B,$3E,$6C,$1F,
     $1F,$83,$D9,$AB,$FB,$41,$BD,$6B,$5B,$E0,$CD,$19,$13,$7E,$21,$79);

  InitialSHA512Mod: TSHA512 =
    ($CF,$AC,$43,$C2,$56,$19,$6C,$AD,$1E,$C2,$0B,$20,$21,$6F,$02,$9E,
     $99,$CB,$56,$D7,$5B,$31,$5D,$8E,$00,$EA,$50,$9F,$FA,$B8,$93,$54,
     $F4,$AB,$F7,$DA,$08,$43,$27,$74,$3E,$A0,$CD,$29,$8E,$9B,$C9,$BA,
     $BA,$26,$7C,$0E,$5E,$E4,$18,$CE,$FE,$45,$68,$BC,$B6,$DB,$84,$DC);

{
  InitialSHA512_224 is calculated as a SHA512 of ASCII string 'SHA-512/224'
  (without quotes) with initial value being the InitialSHA512Mod.
}
  InitialSHA512_224: TSHA512_224 =
    ($8C,$3D,$37,$C8,$19,$54,$4D,$A2,$73,$E1,$99,$66,$89,$DC,$D4,$D6,
     $1D,$FA,$B7,$AE,$32,$FF,$9C,$82,$67,$9D,$D5,$14,$58,$2F,$9F,$CF,
     $0F,$6D,$2B,$69,$7B,$D4,$4D,$A8,$77,$E3,$6F,$73,$04,$C4,$89,$42,
     $3F,$9D,$85,$A8,$6A,$1D,$36,$C8,$11,$12,$E6,$AD,$91,$D6,$92,$A1);

{
  InitialSHA512_256 is calculated as a SHA512 of ASCII string 'SHA-512/256'
  (without quotes) with initial value being the InitialSHA512Mod.
}
  InitialSHA512_256: TSHA512_256 =
    ($22,$31,$21,$94,$FC,$2B,$F7,$2C,$9F,$55,$5F,$A3,$C8,$4C,$64,$C2,
     $23,$93,$B8,$6B,$6F,$53,$B1,$51,$96,$38,$77,$19,$59,$40,$EA,$BD,
     $96,$28,$3E,$E2,$A8,$8E,$FF,$E3,$BE,$5E,$1E,$25,$53,$86,$39,$92,
     $2B,$01,$99,$FC,$2C,$85,$B8,$AA,$0E,$B7,$2D,$DC,$81,$C5,$2C,$A2);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  

  ZeroSHA224: TSHA224 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroSHA256: TSHA256 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

  ZeroSHA384: TSHA384 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroSHA512: TSHA512 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

  ZeroSHA512_224: TSHA512_224 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroSHA512_256: TSHA512_256 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type
  ESHA2Exception = class(EHashException);

  ESHA2IncompatibleClass    = class(ESHA2Exception);
  ESHA2IncompatibleFunction = class(ESHA2Exception);
  ESHA2ProcessingError      = class(ESHA2Exception);
  ESHA2InvalidFunction      = class(ESHA2Exception);

{-------------------------------------------------------------------------------
================================================================================
                                    TSHA2Hash
================================================================================
-------------------------------------------------------------------------------}
type
  TSHA2HashBuffer = packed array[0..64] of UInt8;

{===============================================================================
    TSHA2Hash - class declaration
===============================================================================}
type
  TSHA2Hash = class(TBlockHash)
  protected
    class Function HashBufferToLE(HashBuffer: TSHA2HashBuffer): TSHA2HashBuffer; virtual;
    class Function HashBufferToBE(HashBuffer: TSHA2HashBuffer): TSHA2HashBuffer; virtual;
    class Function HashBufferFromLE(HashBuffer: TSHA2HashBuffer): TSHA2HashBuffer; virtual;
    class Function HashBufferFromBE(HashBuffer: TSHA2HashBuffer): TSHA2HashBuffer; virtual;
    Function GetHashBuffer: TSHA2HashBuffer; virtual; abstract;
    procedure SetHashBuffer(HashBuffer: TSHA2HashBuffer); virtual; abstract;
    Function GetSHA2: TSHA2; virtual;
    procedure ProcessFirst(const Block); override;
  public
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    class Function HashFunction: TSHA2Function; virtual; abstract;
  {
    Since HashSize returns technical size of the hash, which is not always the
    same as is indicated by its name, observed size was introduced. It returns
    number of bytes that are observed in the hash (they are accessed when
    comparing and converting to/from string).
  }
    class Function HashObservedSize: TMemSize; virtual; abstract;
    constructor CreateAndInitFrom(Hash: TSHA2); overload; virtual; abstract;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TSHA2); reintroduce; overload; virtual;
  {
    Note that, by default, the hash is streamed in its entirety, not only
    observed bytes. How many bytes will be written/read is equal to HashSize.

    If you want to only stream observed bytes, use methods SaveObservedToStream
    and LoadObservedFromStream.
    Note that since setting endianness does not change the streamed data, it
    is omitted from observed streaming. 
  }
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure SaveObservedToStream(Stream: TStream); virtual;
    procedure LoadObservedFromStream(Stream: TStream); virtual;
    property SHA2: TSHA2 read GetSHA2;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                  TSHA2Hash_32
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA2Hash_32 - class declaration
===============================================================================}
type
  TSHA2Hash_32 = class(TSHA2Hash)
  protected
    fSHA2:  TSHA2Sys_32;
    procedure ProcessBlock(const Block); override;
    procedure ProcessLast; override;
    procedure Initialize; override;
  public
    property SHA2Sys: TSHA2Sys_32 read fSHA2;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                  TSHA2Hash_64
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA2Hash_64 - class declaration
===============================================================================}
type
  TSHA2Hash_64 = class(TSHA2Hash)
  protected
    fSHA2:  TSHA2Sys_64;
    procedure ProcessBlock(const Block); override;
    procedure ProcessLast; override;
    procedure Initialize; override;
  public
    property SHA2Sys: TSHA2Sys_64 read fSHA2;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                   TSHA224Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA224Hash - class declaration
===============================================================================}
type
  TSHA224Hash = class(TSHA2Hash_32)
  protected
    Function GetHashBuffer: TSHA2HashBuffer; override;
    procedure SetHashBuffer(HashBuffer: TSHA2HashBuffer); override;
    Function GetSHA224: TSHA224; virtual;
    Function GetSHA224Sys: TSHA224Sys; virtual;
    procedure Initialize; override;
  public
    class Function SHA224ToSys(SHA224: TSHA224): TSHA224Sys; virtual;
    class Function SHA224FromSys(SHA224: TSHA224Sys): TSHA224; virtual;
    class Function SHA224ToLE(SHA224: TSHA224): TSHA224; virtual;
    class Function SHA224ToBE(SHA224: TSHA224): TSHA224; virtual;
    class Function SHA224FromLE(SHA224: TSHA224): TSHA224; virtual;
    class Function SHA224FromBE(SHA224: TSHA224): TSHA224; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashFunction: TSHA2Function; override;
    class Function HashObservedSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA2); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA224); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA2); overload; override;
    procedure FromStringDef(const Str: String; const Default: TSHA224); overload; virtual;
    property SHA224: TSHA224 read GetSHA224;
    property SHA224Sys: TSHA224Sys read GetSHA224Sys;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                   TSHA256Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA256Hash - class declaration
===============================================================================}
type
  TSHA256Hash = class(TSHA2Hash_32)
  protected
    Function GetHashBuffer: TSHA2HashBuffer; override;
    procedure SetHashBuffer(HashBuffer: TSHA2HashBuffer); override;
    Function GetSHA256: TSHA256; virtual;
    Function GetSHA256Sys: TSHA256Sys; virtual;
    procedure Initialize; override;
  public
    class Function SHA256ToSys(SHA256: TSHA256): TSHA256Sys; virtual;
    class Function SHA256FromSys(SHA256: TSHA256Sys): TSHA256; virtual;
    class Function SHA256ToLE(SHA256: TSHA256): TSHA256; virtual;
    class Function SHA256ToBE(SHA256: TSHA256): TSHA256; virtual;
    class Function SHA256FromLE(SHA256: TSHA256): TSHA256; virtual;
    class Function SHA256FromBE(SHA256: TSHA256): TSHA256; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashFunction: TSHA2Function; override;
    class Function HashObservedSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA2); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA256); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA2); overload; override;
    procedure FromStringDef(const Str: String; const Default: TSHA256); overload; virtual;
    property SHA256: TSHA256 read GetSHA256;
    property SHA256Sys: TSHA256Sys read GetSHA256Sys;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                   TSHA384Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA384Hash - class declaration
===============================================================================}
type
  TSHA384Hash = class(TSHA2Hash_64)
  protected
    Function GetHashBuffer: TSHA2HashBuffer; override;
    procedure SetHashBuffer(HashBuffer: TSHA2HashBuffer); override;
    Function GetSHA384: TSHA384; virtual;
    Function GetSHA384Sys: TSHA384Sys; virtual;
    procedure Initialize; override;
  public
    class Function SHA384ToSys(SHA384: TSHA384): TSHA384Sys; virtual;
    class Function SHA384FromSys(SHA384: TSHA384Sys): TSHA384; virtual;
    class Function SHA384ToLE(SHA384: TSHA384): TSHA384; virtual;
    class Function SHA384ToBE(SHA384: TSHA384): TSHA384; virtual;
    class Function SHA384FromLE(SHA384: TSHA384): TSHA384; virtual;
    class Function SHA384FromBE(SHA384: TSHA384): TSHA384; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashFunction: TSHA2Function; override;
    class Function HashObservedSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA2); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA384); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA2); overload; override;
    procedure FromStringDef(const Str: String; const Default: TSHA384); overload; virtual;
    property SHA384: TSHA384 read GetSHA384;
    property SHA384Sys: TSHA384Sys read GetSHA384Sys;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                   TSHA512Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA512Hash - class declaration
===============================================================================}
type
  TSHA512Hash = class(TSHA2Hash_64)
  protected
    Function GetHashBuffer: TSHA2HashBuffer; override;
    procedure SetHashBuffer(HashBuffer: TSHA2HashBuffer); override;
    Function GetSHA512: TSHA512; virtual;
    Function GetSHA512Sys: TSHA512Sys; virtual;
    procedure Initialize; override;
  public
    class Function SHA512ToSys(SHA512: TSHA512): TSHA512Sys; virtual;
    class Function SHA512FromSys(SHA512: TSHA512Sys): TSHA512; virtual;
    class Function SHA512ToLE(SHA512: TSHA512): TSHA512; virtual;
    class Function SHA512ToBE(SHA512: TSHA512): TSHA512; virtual;
    class Function SHA512FromLE(SHA512: TSHA512): TSHA512; virtual;
    class Function SHA512FromBE(SHA512: TSHA512): TSHA512; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashFunction: TSHA2Function; override;
    class Function HashObservedSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA2); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA512); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA2); overload; override;
    procedure FromStringDef(const Str: String; const Default: TSHA512); overload; virtual;
    property SHA512: TSHA512 read GetSHA512;
    property SHA512Sys: TSHA512Sys read GetSHA512Sys;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                   TSHA512_224Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA512_224Hash - class declaration
===============================================================================}
type
  TSHA512_224Hash = class(TSHA2Hash_64)
  protected
    Function GetHashBuffer: TSHA2HashBuffer; override;
    procedure SetHashBuffer(HashBuffer: TSHA2HashBuffer); override;
    Function GetSHA512_224: TSHA512_224; virtual;
    Function GetSHA512_224Sys: TSHA512_224Sys; virtual;
    procedure Initialize; override;
  public
    class Function SHA512_224ToSys(SHA512_224: TSHA512_224): TSHA512_224Sys; virtual;
    class Function SHA512_224FromSys(SHA512_224: TSHA512_224Sys): TSHA512_224; virtual;
    class Function SHA512_224ToLE(SHA512_224: TSHA512_224): TSHA512_224; virtual;
    class Function SHA512_224ToBE(SHA512_224: TSHA512_224): TSHA512_224; virtual;
    class Function SHA512_224FromLE(SHA512_224: TSHA512_224): TSHA512_224; virtual;
    class Function SHA512_224FromBE(SHA512_224: TSHA512_224): TSHA512_224; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashFunction: TSHA2Function; override;
    class Function HashObservedSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA2); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA512_224); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA2); overload; override;
    procedure FromStringDef(const Str: String; const Default: TSHA512_224); overload; virtual;
    property SHA512_224: TSHA512_224 read GetSHA512_224;
    property SHA512_224Sys: TSHA512_224Sys read GetSHA512_224Sys;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                   TSHA512_256Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA512_256Hash - class declaration
===============================================================================}
type
  TSHA512_256Hash = class(TSHA2Hash_64)
  protected
    Function GetHashBuffer: TSHA2HashBuffer; override;
    procedure SetHashBuffer(HashBuffer: TSHA2HashBuffer); override;
    Function GetSHA512_256: TSHA512_256; virtual;
    Function GetSHA512_256Sys: TSHA512_256Sys; virtual;
    procedure Initialize; override;
  public
    class Function SHA512_256ToSys(SHA512_256: TSHA512_256): TSHA512_256Sys; virtual;
    class Function SHA512_256FromSys(SHA512_256: TSHA512_256Sys): TSHA512_256; virtual;
    class Function SHA512_256ToLE(SHA512_256: TSHA512_256): TSHA512_256; virtual;
    class Function SHA512_256ToBE(SHA512_256: TSHA512_256): TSHA512_256; virtual;
    class Function SHA512_256FromLE(SHA512_256: TSHA512_256): TSHA512_256; virtual;
    class Function SHA512_256FromBE(SHA512_256: TSHA512_256): TSHA512_256; virtual;
    class Function HashSize: TMemSize; override;
    class Function HashName: String; override;
    class Function HashFunction: TSHA2Function; override;
    class Function HashObservedSize: TMemSize; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA2); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA512_256); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA2); overload; override;
    procedure FromStringDef(const Str: String; const Default: TSHA512_256); overload; virtual;
    property SHA512_256: TSHA512_256 read GetSHA512_256;
    property SHA512_256Sys: TSHA512_256Sys read GetSHA512_256Sys;
  end;

{===============================================================================
    Auxiliary functions
===============================================================================}

Function InitialSHA2(HashFunction: TSHA2Function): TSHA2;

Function CreateByFunction(HashFunction: TSHA2Function): TSHA2Hash;

Function CreateFromByFunction(HashFunction: TSHA2Function; Hash: TSHA2Hash): TSHA2Hash; overload;
Function CreateFromByFunction(HashFunction: TSHA2Function; Hash: TSHA2): TSHA2Hash; overload;
Function CreateFromByFunction(SHA2: TSHA2): TSHA2Hash; overload;{$IFDEF CanInline} inline; {$ENDIF}

{===============================================================================
    Backward compatibility functions
===============================================================================}
{
  Following two functions could be implemented to only return precomputed
  constants, but for the sake of backward compatibility, they are actually
  calculating the result.
}
Function InitialSHA2_512_224: TSHA512_224;
Function InitialSHA2_512_256: TSHA512_256;

//------------------------------------------------------------------------------

Function SHA2ToStr(SHA224: TSHA224): String; overload;
Function SHA2ToStr(SHA256: TSHA256): String; overload;
Function SHA2ToStr(SHA384: TSHA384): String; overload;
Function SHA2ToStr(SHA512: TSHA512): String; overload;
Function SHA2ToStr(SHA512_224: TSHA512_224): String; overload;
Function SHA2ToStr(SHA512_256: TSHA512_256): String; overload;
Function SHA2ToStr(SHA2: TSHA2): String; overload;

{
  Delphi cannot overload based on result type, welp...
}
Function StrToSHA2_224(Str: String): TSHA224;
Function StrToSHA2_256(Str: String): TSHA256;
Function StrToSHA2_384(Str: String): TSHA384;
Function StrToSHA2_512(Str: String): TSHA512;
Function StrToSHA2_512_224(Str: String): TSHA512_224;
Function StrToSHA2_512_256(Str: String): TSHA512_256;
Function StrToSHA2(HashFunction: TSHA2Function; Str: String): TSHA2;

Function TryStrToSHA2(const Str: String; out SHA224: TSHA224): Boolean; overload;
Function TryStrToSHA2(const Str: String; out SHA256: TSHA256): Boolean; overload;
Function TryStrToSHA2(const Str: String; out SHA384: TSHA384): Boolean; overload;
Function TryStrToSHA2(const Str: String; out SHA512: TSHA512): Boolean; overload;
Function TryStrToSHA2(const Str: String; out SHA512_224: TSHA512_224): Boolean; overload;
Function TryStrToSHA2(const Str: String; out SHA512_256: TSHA512_256): Boolean; overload;
Function TryStrToSHA2(HashFunction: TSHA2Function; const Str: String; out SHA2: TSHA2): Boolean; overload;

Function StrToSHA2Def(const Str: String; Default: TSHA224): TSHA224; overload;
Function StrToSHA2Def(const Str: String; Default: TSHA256): TSHA256; overload;
Function StrToSHA2Def(const Str: String; Default: TSHA384): TSHA384; overload;
Function StrToSHA2Def(const Str: String; Default: TSHA512): TSHA512; overload;
Function StrToSHA2Def(const Str: String; Default: TSHA512_224): TSHA512_224; overload;
Function StrToSHA2Def(const Str: String; Default: TSHA512_256): TSHA512_256; overload;
Function StrToSHA2Def(HashFunction: TSHA2Function; const Str: String; Default: TSHA2): TSHA2; overload;

Function CompareSHA2(A,B: TSHA224): Integer; overload;
Function CompareSHA2(A,B: TSHA256): Integer; overload;
Function CompareSHA2(A,B: TSHA384): Integer; overload;
Function CompareSHA2(A,B: TSHA512): Integer; overload;
Function CompareSHA2(A,B: TSHA512_224): Integer; overload;
Function CompareSHA2(A,B: TSHA512_256): Integer; overload;
Function CompareSHA2(A,B: TSHA2): Integer; overload;

Function SameSHA2(A,B: TSHA224): Boolean; overload;
Function SameSHA2(A,B: TSHA256): Boolean; overload;
Function SameSHA2(A,B: TSHA384): Boolean; overload;
Function SameSHA2(A,B: TSHA512): Boolean; overload;
Function SameSHA2(A,B: TSHA512_224): Boolean; overload;
Function SameSHA2(A,B: TSHA512_256): Boolean; overload;
Function SameSHA2(A,B: TSHA2): Boolean; overload;

Function BinaryCorrectSHA2(SHA224: TSHA224): TSHA224; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function BinaryCorrectSHA2(SHA256: TSHA256): TSHA256; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function BinaryCorrectSHA2(SHA384: TSHA384): TSHA384; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function BinaryCorrectSHA2(SHA512: TSHA512): TSHA512; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function BinaryCorrectSHA2(SHA512_224: TSHA512_224): TSHA512_224; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function BinaryCorrectSHA2(SHA512_256: TSHA512_256): TSHA512_256; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function BinaryCorrectSHA2(SHA2: TSHA2): TSHA2; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

procedure BufferSHA2(var SHA224: TSHA224; const Buffer; Size: TMemSize); overload;
procedure BufferSHA2(var SHA256: TSHA256; const Buffer; Size: TMemSize); overload;
procedure BufferSHA2(var SHA384: TSHA384; const Buffer; Size: TMemSize); overload;
procedure BufferSHA2(var SHA512: TSHA512; const Buffer; Size: TMemSize); overload;
procedure BufferSHA2(var SHA512_224: TSHA512_224; const Buffer; Size: TMemSize); overload;
procedure BufferSHA2(var SHA512_256: TSHA512_256; const Buffer; Size: TMemSize); overload;
procedure BufferSHA2(var SHA2: TSHA2; const Buffer; Size: TMemSize); overload;

Function LastBufferSHA2(SHA224: TSHA224; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA224; overload;
Function LastBufferSHA2(SHA256: TSHA256; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA256; overload;
Function LastBufferSHA2(SHA384: TSHA384; const Buffer; Size: TMemSize; MessageLength: UInt128): TSHA384; overload;
Function LastBufferSHA2(SHA512: TSHA512; const Buffer; Size: TMemSize; MessageLength: UInt128): TSHA512; overload;
Function LastBufferSHA2(SHA512_224: TSHA512_224; const Buffer; Size: TMemSize; MessageLength: UInt128): TSHA512_224; overload;
Function LastBufferSHA2(SHA512_256: TSHA512_256; const Buffer; Size: TMemSize; MessageLength: UInt128): TSHA512_256; overload;

{
  Inlining note - FPC at this point (05-2020) does not support untyped (formal)
  parameter in inlined functions.
}
Function LastBufferSHA2(SHA384: TSHA384; const Buffer; Size: TMemSize; MessageLengthLo: UInt64): TSHA384; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA512: TSHA512; const Buffer; Size: TMemSize; MessageLengthLo: UInt64): TSHA512; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA512_224: TSHA512_224; const Buffer; Size: TMemSize; MessageLengthLo: UInt64): TSHA512_224; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA512_256: TSHA512_256; const Buffer; Size: TMemSize; MessageLengthLo: UInt64): TSHA512_256; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}

Function LastBufferSHA2(SHA384: TSHA384; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA384; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA512: TSHA512; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA512; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA512_224: TSHA512_224; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA512_224; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA512_256: TSHA512_256; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA512_256; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}

Function LastBufferSHA2(SHA2: TSHA2; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA2; overload;
Function LastBufferSHA2(SHA2: TSHA2; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA2; overload;
Function LastBufferSHA2(SHA2: TSHA2; const Buffer; Size: TMemSize; MessageLength: OctaWord): TSHA2; overload;

Function LastBufferSHA2(SHA224: TSHA224; const Buffer; Size: TMemSize): TSHA224; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA256: TSHA256; const Buffer; Size: TMemSize): TSHA256; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA384: TSHA384; const Buffer; Size: TMemSize): TSHA384; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA512: TSHA512; const Buffer; Size: TMemSize): TSHA512; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA512_224: TSHA512_224; const Buffer; Size: TMemSize): TSHA512_224; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA512_256: TSHA512_256; const Buffer; Size: TMemSize): TSHA512_256; overload;{$IF defined(CanInline) and not defined(FPC)} inline; {$IFEND}
Function LastBufferSHA2(SHA2: TSHA2; const Buffer; Size: TMemSize): TSHA2; overload;

//------------------------------------------------------------------------------

Function BufferSHA2(HashFunction: TSHA2Function; const Buffer; Size: TMemSize): TSHA2; overload;

Function AnsiStringSHA2(HashFunction: TSHA2Function; const Str: AnsiString): TSHA2;
Function WideStringSHA2(HashFunction: TSHA2Function; const Str: WideString): TSHA2;
Function StringSHA2(HashFunction: TSHA2Function; const Str: String): TSHA2;

Function StreamSHA2(HashFunction: TSHA2Function; Stream: TStream; Count: Int64 = -1): TSHA2;
Function FileSHA2(HashFunction: TSHA2Function; const FileName: String): TSHA2;

//------------------------------------------------------------------------------

type
  TSHA2Context = type Pointer;

Function SHA2_Init(HashFunction: TSHA2Function): TSHA2Context;
procedure SHA2_Update(Context: TSHA2Context; const Buffer; Size: TMemSize);
Function SHA2_Final(var Context: TSHA2Context; const Buffer; Size: TMemSize): TSHA2; overload;
Function SHA2_Final(var Context: TSHA2Context): TSHA2; overload;
Function SHA2_Hash(HashFunction: TSHA2Function; const Buffer; Size: TMemSize): TSHA2;

implementation

uses
  SysUtils,
  BitOps, StrRect;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W4056:={$WARN 4056 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5057:={$WARN 5057 OFF}} // Local variable "$1" does not seem to be initialized
{$ENDIF}

{===============================================================================
    Auxiliary functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary functions - public functions
-------------------------------------------------------------------------------}

Function BuildOctaWord(Lo,Hi: UInt64): UInt128;
begin
Result.Lo := Lo;
Result.Hi := Hi;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BuildOctaWord(Lo: UInt64): UInt128;
begin
Result := BuildOctaWord(Lo,0);
end;

//------------------------------------------------------------------------------

procedure IncrementOctaWord(var Value: UInt128; Increment: UInt128);
var
  Result: UInt64;
  Carry:  UInt32;
  i:      Integer;
begin
Carry := 0;
For i := Low(Value.DWords) to High(Value.DWords) do
  begin
    Result := UInt64(Carry) + Value.DWords[i] + Increment.DWords[i];
    Value.DWords[i] := Int64Rec(Result).Lo;
    Carry := Int64Rec(Result).Hi;
  end;
end;

//------------------------------------------------------------------------------

Function SizeToMessageLength(Size: UInt64): UInt128;
begin
Result.Hi := UInt64(Size shr 61);
Result.Lo := UInt64(Size shl 3);
end;

//------------------------------------------------------------------------------

Function MessageLengthToSize(MessageLength: UInt128): UInt64;
begin
Result := UInt64(MessageLength.Lo shr 3) or
          UInt64(MessageLength.Hi shl 61);
end;

//------------------------------------------------------------------------------

Function EndianSwap(Value: UInt128): UInt128;
begin
Result.Hi := EndianSwap(Value.Lo);
Result.Lo := EndianSwap(Value.Hi);
end;

//------------------------------------------------------------------------------

procedure EndianSwapValue(var Value: UInt128);
begin
Value := EndianSwap(Value);
end;

//------------------------------------------------------------------------------

Function InitialSHA2(HashFunction: TSHA2Function): TSHA2;
begin
Result.HashFunction := HashFunction;
case HashFunction of
  fnSHA224:     Result.SHA224 := InitialSHA224;
  fnSHA256:     Result.SHA256 := InitialSHA256;
  fnSHA384:     Result.SHA384 := InitialSHA384;
  fnSHA512:     Result.SHA512 := InitialSHA512;
  fnSHA512_224: Result.SHA512_224 := InitialSHA512_224;
  fnSHA512_256: Result.SHA512_256 := InitialSHA512_256;
else
  raise ESHA2InvalidFunction.CreateFmt('InitialSHA2: Invalid hash function (%d)',[Ord(HashFunction)]);
end;
end;

//------------------------------------------------------------------------------

Function CreateByFunction(HashFunction: TSHA2Function): TSHA2Hash;
begin
case HashFunction of
  fnSHA224:     Result := TSHA224Hash.Create;
  fnSHA256:     Result := TSHA256Hash.Create;
  fnSHA384:     Result := TSHA384Hash.Create;
  fnSHA512:     Result := TSHA512Hash.Create;
  fnSHA512_224: Result := TSHA512_224Hash.Create;
  fnSHA512_256: Result := TSHA512_256Hash.Create;
else
  raise ESHA2InvalidFunction.CreateFmt('CreateByFunction: Invalid hash function (%d)',[Ord(HashFunction)]);
end;
end;

//------------------------------------------------------------------------------

Function CreateFromByFunction(HashFunction: TSHA2Function; Hash: TSHA2Hash): TSHA2Hash;
begin
case HashFunction of
  fnSHA224:     Result := TSHA224Hash.CreateAndInitFrom(Hash);
  fnSHA256:     Result := TSHA256Hash.CreateAndInitFrom(Hash);
  fnSHA384:     Result := TSHA384Hash.CreateAndInitFrom(Hash);
  fnSHA512:     Result := TSHA512Hash.CreateAndInitFrom(Hash);
  fnSHA512_224: Result := TSHA512_224Hash.CreateAndInitFrom(Hash);
  fnSHA512_256: Result := TSHA512_256Hash.CreateAndInitFrom(Hash);
else
  raise ESHA2InvalidFunction.CreateFmt('CreateFromByFunction(TSHA2Hash): Invalid hash function (%d)',[Ord(HashFunction)]);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CreateFromByFunction(HashFunction: TSHA2Function; Hash: TSHA2): TSHA2Hash;
begin
case HashFunction of
  fnSHA224:     Result := TSHA224Hash.CreateAndInitFrom(Hash);
  fnSHA256:     Result := TSHA256Hash.CreateAndInitFrom(Hash);
  fnSHA384:     Result := TSHA384Hash.CreateAndInitFrom(Hash);
  fnSHA512:     Result := TSHA512Hash.CreateAndInitFrom(Hash);
  fnSHA512_224: Result := TSHA512_224Hash.CreateAndInitFrom(Hash);
  fnSHA512_256: Result := TSHA512_256Hash.CreateAndInitFrom(Hash);
else
  raise ESHA2InvalidFunction.CreateFmt('CreateFromByFunction(TSHA2): Invalid hash function (%d)',[Ord(HashFunction)]);
end;
end;

//------------------------------------------------------------------------------

Function CreateFromByFunction(SHA2: TSHA2): TSHA2Hash;
begin
Result := CreateFromByFunction(SHA2.HashFunction,SHA2);
end;

{-------------------------------------------------------------------------------
    Auxiliary functions - private functions
-------------------------------------------------------------------------------}

procedure EndianSwapValue(var Value: TSHA2Sys_32); overload;
begin
EndianSwapValue(Value.PartA);
EndianSwapValue(Value.PartB);
EndianSwapValue(Value.PartC);
EndianSwapValue(Value.PartD);
EndianSwapValue(Value.PartE);
EndianSwapValue(Value.PartF);
EndianSwapValue(Value.PartG);
EndianSwapValue(Value.PartH);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure EndianSwapValue(var Value: TSHA2Sys_64); overload;
begin
EndianSwapValue(Value.PartA);
EndianSwapValue(Value.PartB);
EndianSwapValue(Value.PartC);
EndianSwapValue(Value.PartD);
EndianSwapValue(Value.PartE);
EndianSwapValue(Value.PartF);
EndianSwapValue(Value.PartG);
EndianSwapValue(Value.PartH);
end;


{-------------------------------------------------------------------------------
================================================================================
                                    TSHA2Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA2Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA2Hash - protected methods
-------------------------------------------------------------------------------}

class Function TSHA2Hash.HashBufferToLE(HashBuffer: TSHA2HashBuffer): TSHA2HashBuffer;
begin
Result := HashBuffer;
end;

//------------------------------------------------------------------------------

class Function TSHA2Hash.HashBufferToBE(HashBuffer: TSHA2HashBuffer): TSHA2HashBuffer;
begin
Result := HashBuffer;
end;

//------------------------------------------------------------------------------

class Function TSHA2Hash.HashBufferFromLE(HashBuffer: TSHA2HashBuffer): TSHA2HashBuffer;
begin
Result := HashBuffer;
end;

//------------------------------------------------------------------------------

class Function TSHA2Hash.HashBufferFromBE(HashBuffer: TSHA2HashBuffer): TSHA2HashBuffer;
begin
Result := HashBuffer;
end;

//------------------------------------------------------------------------------

Function TSHA2Hash.GetSHA2: TSHA2;
var
  Temp: TSHA2HashBuffer;
begin
Result.HashFunction := HashFunction;
Temp := GetHashBuffer;
Move(Temp,Result.SHA224,HashSize);
end;

//------------------------------------------------------------------------------

procedure TSHA2Hash.ProcessFirst(const Block);
begin
inherited;
ProcessBlock(Block);
end;

{-------------------------------------------------------------------------------
    TSHA2Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA2Hash.HashEndianness: THashEndianness;
begin
// first byte is most significant
Result := heBig;
end;

//------------------------------------------------------------------------------

class Function TSHA2Hash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

Function TSHA2Hash.Compare(Hash: THashBase): Integer;
var
  A,B:  TSHA2HashBuffer;
  i:    Integer;
begin
If Hash is Self.ClassType then
  begin
    Result := 0;
    A := GetHashBuffer;
    B := TSHA2Hash(Hash).GetHashBuffer;
    For i := 0 to Pred(HashObservedSize) do
      If A[i] > B[i] then
        begin
          Result := +1;
          Break;
        end
      else If A[i] < B[i] then
        begin
          Result := -1;
          Break;
        end;
  end
else raise ESHA2IncompatibleClass.CreateFmt('TSHA2Hash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TSHA2Hash.AsString: String;
var
  Temp: TSHA2HashBuffer;
  i:    Integer;
begin
Temp := GetHashBuffer;
Result := StringOfChar('0',HashObservedSize * 2);
For i := 0 to Pred(HashObservedSize) do
  begin
    Result[(i * 2) + 2] := IntToHex(Temp[i] and $0F,1)[1];
    Result[(i * 2) + 1] := IntToHex(Temp[i] shr 4,1)[1];
  end;
end;

//------------------------------------------------------------------------------

procedure TSHA2Hash.FromString(const Str: String);
var
  TempStr:        String;
  i:              Integer;
  TempHashBuffer: TSHA2HashBuffer;
begin
If Length(Str) < Integer(HashObservedSize * 2) then
  TempStr := StringOfChar('0',Integer(HashObservedSize * 2) - Length(Str)) + Str
else If Length(Str) > Integer(HashObservedSize * 2) then
  TempStr := Copy(Str,Length(Str) - Pred(Integer(HashObservedSize * 2)),Integer(HashObservedSize * 2))
else
  TempStr := Str;
FillChar(Addr(TempHashBuffer)^,SizeOf(TSHA2HashBuffer),0);
For i := 0 to Pred(HashObservedSize) do
  TempHashBuffer[i] := UInt8(StrToInt('$' + Copy(TempStr,(i * 2) + 1,2)));
SetHashBuffer(TempHashBuffer);
end;

//------------------------------------------------------------------------------

procedure TSHA2Hash.FromStringDef(const Str: String; const Default: TSHA2);
begin
If Default.HashFunction = HashFunction then
  inherited FromStringDef(Str,Default)
else
  raise ESHA2IncompatibleFunction.CreateFmt('TSHA2Hash.FromStringDef: Incompatible function (%d).',[Ord(Default.HashFunction)]);
end;

//------------------------------------------------------------------------------

procedure TSHA2Hash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSHA2HashBuffer;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}HashBufferToBE{$ELSE}HashBufferToLE{$ENDIF}(GetHashBuffer);
  heLittle: Temp := HashBufferToLE(GetHashBuffer);
  heBig:    Temp := HashBufferToBE(GetHashBuffer);
else
 {heDefault}
  Temp := GetHashBuffer;
end;
Stream.WriteBuffer(Temp,HashSize);  // do not use observed size
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA2Hash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TSHA2HashBuffer;
begin
Stream.ReadBuffer(Temp,HashSize);
case Endianness of
  heSystem: SetHashBuffer({$IFDEF ENDIAN_BIG}HashBufferFromBE{$ELSE}HashBufferFromLE{$ENDIF}(Temp));
  heLittle: SetHashBuffer(HashBufferFromLE(Temp));
  heBig:    SetHashBuffer(HashBufferFromBE(Temp));
else
 {heDefault}
  SetHashBuffer(Temp);
end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSHA2Hash.SaveObservedToStream(Stream: TStream);
var
  Temp: TSHA2HashBuffer;
begin
Temp := GetHashBuffer;
Stream.WriteBuffer(Temp,HashObservedSize);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA2Hash.LoadObservedFromStream(Stream: TStream);
var
  Temp: TSHA2HashBuffer;
begin
Stream.ReadBuffer(Temp,HashObservedSize);
SetHashBuffer(Temp);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{-------------------------------------------------------------------------------
================================================================================
                                  TSHA2Hash_32
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA2Hash_32 - calculation constants
===============================================================================}
const
  SHA2_32_ROUND_CONSTS: array[0..63] of UInt32 = (
    $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5, $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
    $D807AA98, $12835B01, $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
    $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
    $983E5152, $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E, $92722C85,
    $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624, $F40E3585, $106AA070,
    $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
    $748F82EE, $78A5636F, $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2);
    
{===============================================================================
    TSHA2Hash_32 - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA2Hash_32 - protected methods
-------------------------------------------------------------------------------}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
procedure TSHA2Hash_32.ProcessBlock(const Block);
var
  Hash:         TSHA2Sys_32;
  i:            Integer;
  Temp1,Temp2:  UInt32;
  Schedule:     array[0..63] of UInt32;
  BlockWords:   packed array[0..15] of UInt32 absolute Block;
begin
Hash := fSHA2;
For i := 0 to 15 do
  Schedule[i] := {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(BlockWords[i]);
For i := 16 to 63 do
  Schedule[i] := UInt32(Schedule[i - 16] + (ROR(Schedule[i - 15],7) xor ROR(Schedule[i - 15],18) xor (Schedule[i - 15] shr 3)) +
                        Schedule[i - 7] + (ROR(Schedule[i - 2],17) xor ROR(Schedule[i - 2],19) xor (Schedule[i - 2] shr 10)));
For i := 0 to 63 do
  begin
    Temp1 := UInt32(Hash.PartH + (ROR(Hash.PartE,6) xor ROR(Hash.PartE,11) xor ROR(Hash.PartE,25)) +
                  ((Hash.PartE and Hash.PartF) xor ((not Hash.PartE) and Hash.PartG)) + SHA2_32_ROUND_CONSTS[i] + Schedule[i]);
    Temp2 := UInt32((ROR(Hash.PartA,2) xor ROR(Hash.PartA,13) xor ROR(Hash.PartA,22)) +
                   ((Hash.PartA and Hash.PartB) xor (Hash.PartA and Hash.PartC) xor (Hash.PartB and Hash.PartC)));
    Hash.PartH := Hash.PartG;
    Hash.PartG := Hash.PartF;
    Hash.PartF := Hash.PartE;
    Hash.PartE := UInt32(Hash.PartD + Temp1);
    Hash.PartD := Hash.PartC;
    Hash.PartC := Hash.PartB;
    Hash.PartB := Hash.PartA;
    Hash.PartA := UInt32(Temp1 + Temp2);
  end;
fSHA2.PartA := UInt32(fSHA2.PartA + Hash.PartA);
fSHA2.PartB := UInt32(fSHA2.PartB + Hash.PartB);
fSHA2.PartC := UInt32(fSHA2.PartC + Hash.PartC);
fSHA2.PartD := UInt32(fSHA2.PartD + Hash.PartD);
fSHA2.PartE := UInt32(fSHA2.PartE + Hash.PartE);
fSHA2.PartF := UInt32(fSHA2.PartF + Hash.PartF);
fSHA2.PartG := UInt32(fSHA2.PartG + Hash.PartG);
fSHA2.PartH := UInt32(fSHA2.PartH + Hash.PartH);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSHA2Hash_32.ProcessLast;
begin
If (fBlockSize - fTransCount) >= (SizeOf(UInt64) + 1) then
  begin
    // padding and length can fit
  {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
    FillChar(Pointer(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^,fBlockSize - fTransCount,0);
    PUInt8(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^ := $80;
    PUInt64(PtrUInt(fTransBlock) + (PtrUInt(fBlockSize) - SizeOf(UInt64)))^ :=
      {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(UInt64(fProcessedBytes) * 8);
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    ProcessBlock(fTransBlock^);
  end
else
  begin
    // padding and length cannot fit  
    If fBlockSize > fTransCount then
      begin
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        FillChar(Pointer(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^,fBlockSize - fTransCount,0);
        PUInt8(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^ := $80;
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        ProcessBlock(fTransBlock^);
        FillChar(fTransBlock^,fBlockSize,0);
      {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
        PUInt64(PtrUInt(fTransBlock) + (PtrUInt(fBlockSize) - SizeOf(UInt64)))^ :=
          {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(UInt64(fProcessedBytes) * 8);
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        ProcessBlock(fTransBlock^);        
      end
    else raise ESHA2ProcessingError.CreateFmt('TSHA2Hash_32.ProcessLast: Invalid data transfer (%d).',[fTransCount]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSHA2Hash_32.Initialize;
begin
fBlockSize := 64; // 512 bits
inherited;
end;


{-------------------------------------------------------------------------------
================================================================================
                                  TSHA2Hash_64
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA2Hash_64 - calculation constants
===============================================================================}
const
  SHA2_64_ROUND_CONSTS: array[0..79] of UInt64 = (
    UInt64($428A2F98D728AE22), UInt64($7137449123EF65CD), UInt64($B5C0FBCFEC4D3B2F), UInt64($E9B5DBA58189DBBC),
    UInt64($3956C25BF348B538), UInt64($59F111F1B605D019), UInt64($923F82A4AF194F9B), UInt64($AB1C5ED5DA6D8118),
    UInt64($D807AA98A3030242), UInt64($12835B0145706FBE), UInt64($243185BE4EE4B28C), UInt64($550C7DC3D5FFB4E2),
    UInt64($72BE5D74F27B896F), UInt64($80DEB1FE3B1696B1), UInt64($9BDC06A725C71235), UInt64($C19BF174CF692694),
    UInt64($E49B69C19EF14AD2), UInt64($EFBE4786384F25E3), UInt64($0FC19DC68B8CD5B5), UInt64($240CA1CC77AC9C65),
    UInt64($2DE92C6F592B0275), UInt64($4A7484AA6EA6E483), UInt64($5CB0A9DCBD41FBD4), UInt64($76F988DA831153B5),
    UInt64($983E5152EE66DFAB), UInt64($A831C66D2DB43210), UInt64($B00327C898FB213F), UInt64($BF597FC7BEEF0EE4),
    UInt64($C6E00BF33DA88FC2), UInt64($D5A79147930AA725), UInt64($06CA6351E003826F), UInt64($142929670A0E6E70),
    UInt64($27B70A8546D22FFC), UInt64($2E1B21385C26C926), UInt64($4D2C6DFC5AC42AED), UInt64($53380D139D95B3DF),
    UInt64($650A73548BAF63DE), UInt64($766A0ABB3C77B2A8), UInt64($81C2C92E47EDAEE6), UInt64($92722C851482353B),
    UInt64($A2BFE8A14CF10364), UInt64($A81A664BBC423001), UInt64($C24B8B70D0F89791), UInt64($C76C51A30654BE30),
    UInt64($D192E819D6EF5218), UInt64($D69906245565A910), UInt64($F40E35855771202A), UInt64($106AA07032BBD1B8),
    UInt64($19A4C116B8D2D0C8), UInt64($1E376C085141AB53), UInt64($2748774CDF8EEB99), UInt64($34B0BCB5E19B48A8),
    UInt64($391C0CB3C5C95A63), UInt64($4ED8AA4AE3418ACB), UInt64($5B9CCA4F7763E373), UInt64($682E6FF3D6B2B8A3),
    UInt64($748F82EE5DEFB2FC), UInt64($78A5636F43172F60), UInt64($84C87814A1F0AB72), UInt64($8CC702081A6439EC),
    UInt64($90BEFFFA23631E28), UInt64($A4506CEBDE82BDE9), UInt64($BEF9A3F7B2C67915), UInt64($C67178F2E372532B),
    UInt64($CA273ECEEA26619C), UInt64($D186B8C721C0C207), UInt64($EADA7DD6CDE0EB1E), UInt64($F57D4F7FEE6ED178),
    UInt64($06F067AA72176FBA), UInt64($0A637DC5A2C898A6), UInt64($113F9804BEF90DAE), UInt64($1B710B35131C471B),
    UInt64($28DB77F523047D84), UInt64($32CAAB7B40C72493), UInt64($3C9EBE0A15C9BEBC), UInt64($431D67C49C100D4C),
    UInt64($4CC5D4BECB3E42B6), UInt64($597F299CFC657E2A), UInt64($5FCB6FAB3AD6FAEC), UInt64($6C44198C4A475817));

{===============================================================================
    TSHA2Hash_64 - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA2Hash_64 - protected methods
-------------------------------------------------------------------------------}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
procedure TSHA2Hash_64.ProcessBlock(const Block);
var
  Hash:         TSHA2Sys_64;
  i:            Integer;
  Temp1,Temp2:  UInt64;
  Schedule:     array[0..79] of UInt64;
  BlockWords:   packed array[0..15] of UInt64 absolute Block;
begin
Hash := fSHA2;
For i := 0 to 15 do
  Schedule[i] := {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(BlockWords[i]);
For i := 16 to 79 do
  Schedule[i] := UInt64(Schedule[i - 16] + (ROR(Schedule[i - 15],1) xor ROR(Schedule[i - 15],8) xor (Schedule[i - 15] shr 7)) +
                        Schedule[i - 7] + (ROR(Schedule[i - 2],19) xor ROR(Schedule[i - 2],61) xor (Schedule[i - 2] shr 6)));
For i := 0 to 79 do
  begin
    Temp1 := UInt64(Hash.PartH + (ROR(Hash.PartE,14) xor ROR(Hash.PartE,18) xor ROR(Hash.PartE,41)) +
                  ((Hash.PartE and Hash.PartF) xor ((not Hash.PartE) and Hash.PartG)) + SHA2_64_ROUND_CONSTS[i] + Schedule[i]);
    Temp2 := UInt64((ROR(Hash.PartA,28) xor ROR(Hash.PartA,34) xor ROR(Hash.PartA,39)) +
                   ((Hash.PartA and Hash.PartB) xor (Hash.PartA and Hash.PartC) xor (Hash.PartB and Hash.PartC)));
    Hash.PartH := Hash.PartG;
    Hash.PartG := Hash.PartF;
    Hash.PartF := Hash.PartE;
    Hash.PartE := UInt64(Hash.PartD + Temp1);
    Hash.PartD := Hash.PartC;
    Hash.PartC := Hash.PartB;
    Hash.PartB := Hash.PartA;
    Hash.PartA := UInt64(Temp1 + Temp2);
  end;
fSHA2.PartA := UInt64(fSHA2.PartA + Hash.PartA);
fSHA2.PartB := UInt64(fSHA2.PartB + Hash.PartB);
fSHA2.PartC := UInt64(fSHA2.PartC + Hash.PartC);
fSHA2.PartD := UInt64(fSHA2.PartD + Hash.PartD);
fSHA2.PartE := UInt64(fSHA2.PartE + Hash.PartE);
fSHA2.PartF := UInt64(fSHA2.PartF + Hash.PartF);
fSHA2.PartG := UInt64(fSHA2.PartG + Hash.PartG);
fSHA2.PartH := UInt64(fSHA2.PartH + Hash.PartH);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSHA2Hash_64.ProcessLast;
begin
If (fBlockSize - fTransCount) >= (SizeOf(UInt128) + 1) then
  begin
    // padding and length can fit
  {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
    FillChar(Pointer(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^,fBlockSize - fTransCount,0);
    PUInt8(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^ := $80;
    PUInt128(PtrUInt(fTransBlock) + (PtrUInt(fBlockSize) - SizeOf(UInt128)))^ :=
      {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(SizeToMessageLength(fProcessedBytes));
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    ProcessBlock(fTransBlock^);
  end
else
  begin
    // padding and length cannot fit  
    If fBlockSize > fTransCount then
      begin
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        FillChar(Pointer(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^,fBlockSize - fTransCount,0);
        PUInt8(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^ := $80;
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        ProcessBlock(fTransBlock^);
        FillChar(fTransBlock^,fBlockSize,0);
      {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
        PUInt128(PtrUInt(fTransBlock) + (PtrUInt(fBlockSize) - SizeOf(UInt128)))^ :=
          {$IFNDEF ENDIAN_BIG}EndianSwap{$ENDIF}(SizeToMessageLength(fProcessedBytes));
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        ProcessBlock(fTransBlock^);        
      end
    else raise ESHA2ProcessingError.CreateFmt('TSHA2Hash_64.ProcessLast: Invalid data transfer (%d).',[fTransCount]);
  end;
end;

//------------------------------------------------------------------------------

procedure TSHA2Hash_64.Initialize;
begin
fBlockSize := 128;  // 1024 bits
inherited;
end;


{-------------------------------------------------------------------------------
================================================================================
                                   TSHA224Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA224Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA224Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA224Hash.GetHashBuffer: TSHA2HashBuffer;
var
  Temp: TSHA224;
begin
Temp := SHA224FromSys(TSHA224Sys(fSHA2));
FillChar(Addr(Result)^,SizeOf(TSHA2HashBuffer),0);
Move(Temp,Result,HashSize);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA224Hash.SetHashBuffer(HashBuffer: TSHA2HashBuffer);
var
  Temp: TSHA224;
begin
Move(HashBuffer,Temp,HashSize);
fSHA2 := TSHA2Sys_32(SHA224ToSys(Temp));
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TSHA224Hash.GetSHA224: TSHA224;
begin
Result := SHA224FromSys(TSHA224Sys(fSHA2));
end;

//------------------------------------------------------------------------------

Function TSHA224Hash.GetSHA224Sys: TSHA224Sys;
begin
Result := TSHA224Sys(fSHA2);
end;

//------------------------------------------------------------------------------

procedure TSHA224Hash.Initialize;
begin
inherited;
fSHA2 := TSHA2Sys_32(SHA224ToSys(ZeroSHA224));
end;

{-------------------------------------------------------------------------------
    TSHA224Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA224Hash.SHA224ToSys(SHA224: TSHA224): TSHA224Sys;
var
  Temp: TSHA224Sys absolute SHA224;
begin
Result := Temp;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_32(Result));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA224Hash.SHA224FromSys(SHA224: TSHA224Sys): TSHA224;
var
  Temp: TSHA224Sys absolute Result;
begin
Temp := SHA224;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_32(Temp));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA224Hash.SHA224ToLE(SHA224: TSHA224): TSHA224;
begin
Result := SHA224;
end; 

//------------------------------------------------------------------------------

class Function TSHA224Hash.SHA224ToBE(SHA224: TSHA224): TSHA224;
begin
Result := SHA224;
end;

//------------------------------------------------------------------------------

class Function TSHA224Hash.SHA224FromLE(SHA224: TSHA224): TSHA224;
begin
Result := SHA224;
end;

//------------------------------------------------------------------------------

class Function TSHA224Hash.SHA224FromBE(SHA224: TSHA224): TSHA224;
begin
Result := SHA224;
end;

//------------------------------------------------------------------------------

class Function TSHA224Hash.HashSize: TMemSize;
begin
Result := SizeOf(TSHA224);
end;
 
//------------------------------------------------------------------------------

class Function TSHA224Hash.HashName: String;
begin
Result := 'SHA-224';
end;
 
//------------------------------------------------------------------------------

class Function TSHA224Hash.HashFunction: TSHA2Function;
begin
Result := fnSHA224;
end;
 
//------------------------------------------------------------------------------

class Function TSHA224Hash.HashObservedSize: TMemSize;
begin
Result := 28; // 224 bits
end;

//------------------------------------------------------------------------------

constructor TSHA224Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA224Hash then
  fSHA2 := TSHA2Sys_32(TSHA224Hash(Hash).SHA224Sys)
else
  raise ESHA2IncompatibleClass.CreateFmt('TSHA224Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TSHA224Hash.CreateAndInitFrom(Hash: TSHA2);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  fSHA2 := TSHA2Sys_32(SHA224ToSys(Hash.SHA224))
else
  raise ESHA2IncompatibleFunction.CreateFmt('TSHA224Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA224Hash.CreateAndInitFrom(Hash: TSHA224);
begin
CreateAndInit;
fSHA2 := TSHA2Sys_32(SHA224ToSys(Hash));
end;

//------------------------------------------------------------------------------

procedure TSHA224Hash.Init;
begin
inherited;
fSHA2 := TSHA2Sys_32(SHA224ToSys(InitialSHA224));
end;

//------------------------------------------------------------------------------

procedure TSHA224Hash.FromStringDef(const Str: String; const Default: TSHA2);
begin
inherited FromStringDef(Str,Default);
FromStringDef(Str,Default.SHA224);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSHA224Hash.FromStringDef(const Str: String; const Default: TSHA224);
begin
If not TryFromString(Str) then
  fSHA2 := TSHA2Sys_32(SHA224ToSys(Default));
end;


{-------------------------------------------------------------------------------
================================================================================
                                   TSHA256Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA256Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA256Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA256Hash.GetHashBuffer: TSHA2HashBuffer;
var
  Temp: TSHA256;
begin
Temp := SHA256FromSys(TSHA256Sys(fSHA2));
FillChar(Addr(Result)^,SizeOf(TSHA2HashBuffer),0);
Move(Temp,Result,HashSize);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA256Hash.SetHashBuffer(HashBuffer: TSHA2HashBuffer);
var
  Temp: TSHA256;
begin
Move(HashBuffer,Temp,HashSize);
fSHA2 := TSHA2Sys_32(SHA256ToSys(Temp));
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TSHA256Hash.GetSHA256: TSHA256;
begin
Result := SHA256FromSys(TSHA256Sys(fSHA2));
end;

//------------------------------------------------------------------------------

Function TSHA256Hash.GetSHA256Sys: TSHA256Sys;
begin
Result := TSHA256Sys(fSHA2);
end;

//------------------------------------------------------------------------------

procedure TSHA256Hash.Initialize;
begin
inherited;
fSHA2 := TSHA2Sys_32(SHA256ToSys(ZeroSHA256));
end;

{-------------------------------------------------------------------------------
    TSHA256Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA256Hash.SHA256ToSys(SHA256: TSHA256): TSHA256Sys;
var
  Temp: TSHA256Sys absolute SHA256;
begin
Result := Temp;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_32(Result));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA256Hash.SHA256FromSys(SHA256: TSHA256Sys): TSHA256;
var
  Temp: TSHA256Sys absolute Result;
begin
Temp := SHA256;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_32(Temp));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA256Hash.SHA256ToLE(SHA256: TSHA256): TSHA256;
begin
Result := SHA256;
end; 

//------------------------------------------------------------------------------

class Function TSHA256Hash.SHA256ToBE(SHA256: TSHA256): TSHA256;
begin
Result := SHA256;
end;

//------------------------------------------------------------------------------

class Function TSHA256Hash.SHA256FromLE(SHA256: TSHA256): TSHA256;
begin
Result := SHA256;
end;

//------------------------------------------------------------------------------

class Function TSHA256Hash.SHA256FromBE(SHA256: TSHA256): TSHA256;
begin
Result := SHA256;
end;

//------------------------------------------------------------------------------

class Function TSHA256Hash.HashSize: TMemSize;
begin
Result := SizeOf(TSHA256);
end;
 
//------------------------------------------------------------------------------

class Function TSHA256Hash.HashName: String;
begin
Result := 'SHA-256';
end;
 
//------------------------------------------------------------------------------

class Function TSHA256Hash.HashFunction: TSHA2Function;
begin
Result := fnSHA256;
end;
 
//------------------------------------------------------------------------------

class Function TSHA256Hash.HashObservedSize: TMemSize;
begin
Result := 32; // 256 bits
end;

//------------------------------------------------------------------------------

constructor TSHA256Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA256Hash then
  fSHA2 := TSHA2Sys_32(TSHA256Hash(Hash).SHA256Sys)
else
  raise ESHA2IncompatibleClass.CreateFmt('TSHA256Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TSHA256Hash.CreateAndInitFrom(Hash: TSHA2);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  fSHA2 := TSHA2Sys_32(SHA256ToSys(Hash.SHA256))
else
  raise ESHA2IncompatibleFunction.CreateFmt('TSHA256Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA256Hash.CreateAndInitFrom(Hash: TSHA256);
begin
CreateAndInit;
fSHA2 := TSHA2Sys_32(SHA256ToSys(Hash));
end;

//------------------------------------------------------------------------------

procedure TSHA256Hash.Init;
begin
inherited;
fSHA2 := TSHA2Sys_32(SHA256ToSys(InitialSHA256));
end;

//------------------------------------------------------------------------------

procedure TSHA256Hash.FromStringDef(const Str: String; const Default: TSHA2);
begin
inherited FromStringDef(Str,Default);
FromStringDef(Str,Default.SHA256);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSHA256Hash.FromStringDef(const Str: String; const Default: TSHA256);
begin
If not TryFromString(Str) then
  fSHA2 := TSHA2Sys_32(SHA256ToSys(Default));
end;


{-------------------------------------------------------------------------------
================================================================================
                                   TSHA384Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA384Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA384Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA384Hash.GetHashBuffer: TSHA2HashBuffer;
var
  Temp: TSHA384;
begin
Temp := SHA384FromSys(TSHA384Sys(fSHA2));
FillChar(Addr(Result)^,SizeOf(TSHA2HashBuffer),0);
Move(Temp,Result,HashSize);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA384Hash.SetHashBuffer(HashBuffer: TSHA2HashBuffer);
var
  Temp: TSHA384;
begin
Move(HashBuffer,Temp,HashSize);
fSHA2 := TSHA2Sys_64(SHA384ToSys(Temp));
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TSHA384Hash.GetSHA384: TSHA384;
begin
Result := SHA384FromSys(TSHA384Sys(fSHA2));
end;

//------------------------------------------------------------------------------

Function TSHA384Hash.GetSHA384Sys: TSHA384Sys;
begin
Result := TSHA384Sys(fSHA2);
end;

//------------------------------------------------------------------------------

procedure TSHA384Hash.Initialize;
begin
inherited;
fSHA2 := TSHA2Sys_64(SHA384ToSys(ZeroSHA384));
end;

{-------------------------------------------------------------------------------
    TSHA384Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA384Hash.SHA384ToSys(SHA384: TSHA384): TSHA384Sys;
var
  Temp: TSHA384Sys absolute SHA384;
begin
Result := Temp;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_64(Result));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA384Hash.SHA384FromSys(SHA384: TSHA384Sys): TSHA384;
var
  Temp: TSHA384Sys absolute Result;
begin
Temp := SHA384;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_64(Temp));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA384Hash.SHA384ToLE(SHA384: TSHA384): TSHA384;
begin
Result := SHA384;
end; 

//------------------------------------------------------------------------------

class Function TSHA384Hash.SHA384ToBE(SHA384: TSHA384): TSHA384;
begin
Result := SHA384;
end;

//------------------------------------------------------------------------------

class Function TSHA384Hash.SHA384FromLE(SHA384: TSHA384): TSHA384;
begin
Result := SHA384;
end;

//------------------------------------------------------------------------------

class Function TSHA384Hash.SHA384FromBE(SHA384: TSHA384): TSHA384;
begin
Result := SHA384;
end;

//------------------------------------------------------------------------------

class Function TSHA384Hash.HashSize: TMemSize;
begin
Result := SizeOf(TSHA384);
end;
 
//------------------------------------------------------------------------------

class Function TSHA384Hash.HashName: String;
begin
Result := 'SHA-384';
end;
 
//------------------------------------------------------------------------------

class Function TSHA384Hash.HashFunction: TSHA2Function;
begin
Result := fnSHA384;
end;
 
//------------------------------------------------------------------------------

class Function TSHA384Hash.HashObservedSize: TMemSize;
begin
Result := 48; // 384 bits
end;

//------------------------------------------------------------------------------

constructor TSHA384Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA384Hash then
  fSHA2 := TSHA2Sys_64(TSHA384Hash(Hash).SHA384Sys)
else
  raise ESHA2IncompatibleClass.CreateFmt('TSHA384Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TSHA384Hash.CreateAndInitFrom(Hash: TSHA2);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  fSHA2 := TSHA2Sys_64(SHA384ToSys(Hash.SHA384))
else
  raise ESHA2IncompatibleFunction.CreateFmt('TSHA384Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA384Hash.CreateAndInitFrom(Hash: TSHA384);
begin
CreateAndInit;
fSHA2 := TSHA2Sys_64(SHA384ToSys(Hash));
end;

//------------------------------------------------------------------------------

procedure TSHA384Hash.Init;
begin
inherited;
fSHA2 := TSHA2Sys_64(SHA384ToSys(InitialSHA384));
end;

//------------------------------------------------------------------------------

procedure TSHA384Hash.FromStringDef(const Str: String; const Default: TSHA2);
begin
inherited FromStringDef(Str,Default);
FromStringDef(Str,Default.SHA384);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSHA384Hash.FromStringDef(const Str: String; const Default: TSHA384);
begin
If not TryFromString(Str) then
  fSHA2 := TSHA2Sys_64(SHA384ToSys(Default));
end;


{-------------------------------------------------------------------------------
================================================================================
                                   TSHA512Hash                                  
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA512Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA512Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA512Hash.GetHashBuffer: TSHA2HashBuffer;
var
  Temp: TSHA512;
begin
Temp := SHA512FromSys(TSHA512Sys(fSHA2));
FillChar(Addr(Result)^,SizeOf(TSHA2HashBuffer),0);
Move(Temp,Result,HashSize);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA512Hash.SetHashBuffer(HashBuffer: TSHA2HashBuffer);
var
  Temp: TSHA512;
begin
Move(HashBuffer,Temp,HashSize);
fSHA2 := TSHA2Sys_64(SHA512ToSys(Temp));
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TSHA512Hash.GetSHA512: TSHA512;
begin
Result := SHA512FromSys(TSHA512Sys(fSHA2));
end;

//------------------------------------------------------------------------------

Function TSHA512Hash.GetSHA512Sys: TSHA512Sys;
begin
Result := TSHA512Sys(fSHA2);
end;

//------------------------------------------------------------------------------

procedure TSHA512Hash.Initialize;
begin
inherited;
fSHA2 := TSHA2Sys_64(SHA512ToSys(ZeroSHA512));
end;

{-------------------------------------------------------------------------------
    TSHA512Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA512Hash.SHA512ToSys(SHA512: TSHA512): TSHA512Sys;
var
  Temp: TSHA512Sys absolute SHA512;
begin
Result := Temp;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_64(Result));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA512Hash.SHA512FromSys(SHA512: TSHA512Sys): TSHA512;
var
  Temp: TSHA512Sys absolute Result;
begin
Temp := SHA512;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_64(Temp));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA512Hash.SHA512ToLE(SHA512: TSHA512): TSHA512;
begin
Result := SHA512;
end; 

//------------------------------------------------------------------------------

class Function TSHA512Hash.SHA512ToBE(SHA512: TSHA512): TSHA512;
begin
Result := SHA512;
end;

//------------------------------------------------------------------------------

class Function TSHA512Hash.SHA512FromLE(SHA512: TSHA512): TSHA512;
begin
Result := SHA512;
end;

//------------------------------------------------------------------------------

class Function TSHA512Hash.SHA512FromBE(SHA512: TSHA512): TSHA512;
begin
Result := SHA512;
end;

//------------------------------------------------------------------------------

class Function TSHA512Hash.HashSize: TMemSize;
begin
Result := SizeOf(TSHA512);
end;
 
//------------------------------------------------------------------------------

class Function TSHA512Hash.HashName: String;
begin
Result := 'SHA-512';
end;
 
//------------------------------------------------------------------------------

class Function TSHA512Hash.HashFunction: TSHA2Function;
begin
Result := fnSHA512;
end;
 
//------------------------------------------------------------------------------

class Function TSHA512Hash.HashObservedSize: TMemSize;
begin
Result := 64; // 512 bits
end;

//------------------------------------------------------------------------------

constructor TSHA512Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA512Hash then
  fSHA2 := TSHA2Sys_64(TSHA512Hash(Hash).SHA512Sys)
else
  raise ESHA2IncompatibleClass.CreateFmt('TSHA512Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TSHA512Hash.CreateAndInitFrom(Hash: TSHA2);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  fSHA2 := TSHA2Sys_64(SHA512ToSys(Hash.SHA512))
else
  raise ESHA2IncompatibleFunction.CreateFmt('TSHA512Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA512Hash.CreateAndInitFrom(Hash: TSHA512);
begin
CreateAndInit;
fSHA2 := TSHA2Sys_64(SHA512ToSys(Hash));
end;

//------------------------------------------------------------------------------

procedure TSHA512Hash.Init;
begin
inherited;
fSHA2 := TSHA2Sys_64(SHA512ToSys(InitialSHA512));
end;

//------------------------------------------------------------------------------

procedure TSHA512Hash.FromStringDef(const Str: String; const Default: TSHA2);
begin
inherited FromStringDef(Str,Default);
FromStringDef(Str,Default.SHA512);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSHA512Hash.FromStringDef(const Str: String; const Default: TSHA512);
begin
If not TryFromString(Str) then
  fSHA2 := TSHA2Sys_64(SHA512ToSys(Default));
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TSHA512_224Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA512_224Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA512_224Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA512_224Hash.GetHashBuffer: TSHA2HashBuffer;
var
  Temp: TSHA512_224;
begin
Temp := SHA512_224FromSys(TSHA512_224Sys(fSHA2));
FillChar(Addr(Result)^,SizeOf(TSHA2HashBuffer),0);
Move(Temp,Result,HashSize);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA512_224Hash.SetHashBuffer(HashBuffer: TSHA2HashBuffer);
var
  Temp: TSHA512_224;
begin
Move(HashBuffer,Temp,HashSize);
fSHA2 := TSHA2Sys_64(SHA512_224ToSys(Temp));
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TSHA512_224Hash.GetSHA512_224: TSHA512_224;
begin
Result := SHA512_224FromSys(TSHA512_224Sys(fSHA2));
end;

//------------------------------------------------------------------------------

Function TSHA512_224Hash.GetSHA512_224Sys: TSHA512_224Sys;
begin
Result := TSHA512_224Sys(fSHA2);
end;

//------------------------------------------------------------------------------

procedure TSHA512_224Hash.Initialize;
begin
inherited;
fSHA2 := TSHA2Sys_64(SHA512_224ToSys(ZeroSHA512_224));
end;

{-------------------------------------------------------------------------------
    TSHA512_224Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA512_224Hash.SHA512_224ToSys(SHA512_224: TSHA512_224): TSHA512_224Sys;
var
  Temp: TSHA512_224Sys absolute SHA512_224;
begin
Result := Temp;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_64(Result));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA512_224Hash.SHA512_224FromSys(SHA512_224: TSHA512_224Sys): TSHA512_224;
var
  Temp: TSHA512_224Sys absolute Result;
begin
Temp := SHA512_224;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_64(Temp));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA512_224Hash.SHA512_224ToLE(SHA512_224: TSHA512_224): TSHA512_224;
begin
Result := SHA512_224;
end; 

//------------------------------------------------------------------------------

class Function TSHA512_224Hash.SHA512_224ToBE(SHA512_224: TSHA512_224): TSHA512_224;
begin
Result := SHA512_224;
end;

//------------------------------------------------------------------------------

class Function TSHA512_224Hash.SHA512_224FromLE(SHA512_224: TSHA512_224): TSHA512_224;
begin
Result := SHA512_224;
end;

//------------------------------------------------------------------------------

class Function TSHA512_224Hash.SHA512_224FromBE(SHA512_224: TSHA512_224): TSHA512_224;
begin
Result := SHA512_224;
end;

//------------------------------------------------------------------------------

class Function TSHA512_224Hash.HashSize: TMemSize;
begin
Result := SizeOf(TSHA512_224);
end;
 
//------------------------------------------------------------------------------

class Function TSHA512_224Hash.HashName: String;
begin
Result := 'SHA-512/224';
end;
 
//------------------------------------------------------------------------------

class Function TSHA512_224Hash.HashFunction: TSHA2Function;
begin
Result := fnSHA512_224;
end;
 
//------------------------------------------------------------------------------

class Function TSHA512_224Hash.HashObservedSize: TMemSize;
begin
Result := 28; // 224 bits
end;

//------------------------------------------------------------------------------

constructor TSHA512_224Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA512_224Hash then
  fSHA2 := TSHA2Sys_64(TSHA512_224Hash(Hash).SHA512_224Sys)
else
  raise ESHA2IncompatibleClass.CreateFmt('TSHA512_224Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TSHA512_224Hash.CreateAndInitFrom(Hash: TSHA2);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  fSHA2 := TSHA2Sys_64(SHA512_224ToSys(Hash.SHA512_224))
else
  raise ESHA2IncompatibleFunction.CreateFmt('TSHA512_224Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA512_224Hash.CreateAndInitFrom(Hash: TSHA512_224);
begin
CreateAndInit;
fSHA2 := TSHA2Sys_64(SHA512_224ToSys(Hash));
end;

//------------------------------------------------------------------------------

procedure TSHA512_224Hash.Init;
begin
inherited;
fSHA2 := TSHA2Sys_64(SHA512_224ToSys(InitialSHA512_224));
end;

//------------------------------------------------------------------------------

procedure TSHA512_224Hash.FromStringDef(const Str: String; const Default: TSHA2);
begin
inherited FromStringDef(Str,Default);
FromStringDef(Str,Default.SHA512_224);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSHA512_224Hash.FromStringDef(const Str: String; const Default: TSHA512_224);
begin
If not TryFromString(Str) then
  fSHA2 := TSHA2Sys_64(SHA512_224ToSys(Default));
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TSHA512_256Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA512_256Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA512_256Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA512_256Hash.GetHashBuffer: TSHA2HashBuffer;
var
  Temp: TSHA512_256;
begin
Temp := SHA512_256FromSys(TSHA512_256Sys(fSHA2));
FillChar(Addr(Result)^,SizeOf(TSHA2HashBuffer),0);
Move(Temp,Result,HashSize);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
procedure TSHA512_256Hash.SetHashBuffer(HashBuffer: TSHA2HashBuffer);
var
  Temp: TSHA512_256;
begin
Move(HashBuffer,Temp,HashSize);
fSHA2 := TSHA2Sys_64(SHA512_256ToSys(Temp));
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TSHA512_256Hash.GetSHA512_256: TSHA512_256;
begin
Result := SHA512_256FromSys(TSHA512_256Sys(fSHA2));
end;

//------------------------------------------------------------------------------

Function TSHA512_256Hash.GetSHA512_256Sys: TSHA512_256Sys;
begin
Result := TSHA512_256Sys(fSHA2);
end;

//------------------------------------------------------------------------------

procedure TSHA512_256Hash.Initialize;
begin
inherited;
fSHA2 := TSHA2Sys_64(SHA512_256ToSys(ZeroSHA512_256));
end;

{-------------------------------------------------------------------------------
    TSHA512_256Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA512_256Hash.SHA512_256ToSys(SHA512_256: TSHA512_256): TSHA512_256Sys;
var
  Temp: TSHA512_256Sys absolute SHA512_256;
begin
Result := Temp;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_64(Result));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA512_256Hash.SHA512_256FromSys(SHA512_256: TSHA512_256Sys): TSHA512_256;
var
  Temp: TSHA512_256Sys absolute Result;
begin
Temp := SHA512_256;
{$IFNDEF ENDIAN_BIG}
EndianSwapValue(TSHA2Sys_64(Temp));
{$ENDIF}
end;

//------------------------------------------------------------------------------

class Function TSHA512_256Hash.SHA512_256ToLE(SHA512_256: TSHA512_256): TSHA512_256;
begin
Result := SHA512_256;
end; 

//------------------------------------------------------------------------------

class Function TSHA512_256Hash.SHA512_256ToBE(SHA512_256: TSHA512_256): TSHA512_256;
begin
Result := SHA512_256;
end;

//------------------------------------------------------------------------------

class Function TSHA512_256Hash.SHA512_256FromLE(SHA512_256: TSHA512_256): TSHA512_256;
begin
Result := SHA512_256;
end;

//------------------------------------------------------------------------------

class Function TSHA512_256Hash.SHA512_256FromBE(SHA512_256: TSHA512_256): TSHA512_256;
begin
Result := SHA512_256;
end;

//------------------------------------------------------------------------------

class Function TSHA512_256Hash.HashSize: TMemSize;
begin
Result := SizeOf(TSHA512_256);
end;
 
//------------------------------------------------------------------------------

class Function TSHA512_256Hash.HashName: String;
begin
Result := 'SHA-512/256';
end;
 
//------------------------------------------------------------------------------

class Function TSHA512_256Hash.HashFunction: TSHA2Function;
begin
Result := fnSHA512_256;
end;
 
//------------------------------------------------------------------------------

class Function TSHA512_256Hash.HashObservedSize: TMemSize;
begin
Result := 32; // 256 bits
end;

//------------------------------------------------------------------------------

constructor TSHA512_256Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA512_256Hash then
  fSHA2 := TSHA2Sys_64(TSHA512_256Hash(Hash).SHA512_256Sys)
else
  raise ESHA2IncompatibleClass.CreateFmt('TSHA512_256Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TSHA512_256Hash.CreateAndInitFrom(Hash: TSHA2);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  fSHA2 := TSHA2Sys_64(SHA512_256ToSys(Hash.SHA512_256))
else
  raise ESHA2IncompatibleFunction.CreateFmt('TSHA512_256Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA512_256Hash.CreateAndInitFrom(Hash: TSHA512_256);
begin
CreateAndInit;
fSHA2 := TSHA2Sys_64(SHA512_256ToSys(Hash));
end;

//------------------------------------------------------------------------------

procedure TSHA512_256Hash.Init;
begin
inherited;
fSHA2 := TSHA2Sys_64(SHA512_256ToSys(InitialSHA512_256));
end;

//------------------------------------------------------------------------------

procedure TSHA512_256Hash.FromStringDef(const Str: String; const Default: TSHA2);
begin
inherited FromStringDef(Str,Default);
FromStringDef(Str,Default.SHA512_256);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TSHA512_256Hash.FromStringDef(const Str: String; const Default: TSHA512_256);
begin
If not TryFromString(Str) then
  fSHA2 := TSHA2Sys_64(SHA512_256ToSys(Default));
end;


{===============================================================================
    Backward compatibility functions
===============================================================================}
{-------------------------------------------------------------------------------
    Backward compatibility functions - auxiliary functions
-------------------------------------------------------------------------------}

Function InitialSHA2_512_224: TSHA512_224;
var
  Hash:     TSHA512Hash;
  EvalStr:  AnsiString;
begin
Hash := TSHA512Hash.CreateAndInitFrom(InitialSHA512mod);
try
  EvalStr := StrToAnsi('SHA-512/224');
  Hash.Final(PAnsiChar(EvalStr)^,Length(EvalStr) * SizeOf(AnsiChar));
  Result := Hash.SHA2.SHA512_224;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function InitialSHA2_512_256: TSHA512_256;
var
  Hash:     TSHA512Hash;
  EvalStr:  AnsiString;
begin
Hash := TSHA512Hash.CreateAndInitFrom(InitialSHA512mod);
try
  EvalStr := StrToAnsi('SHA-512/256');
  Hash.Final(PAnsiChar(EvalStr)^,Length(EvalStr) * SizeOf(AnsiChar));
  Result := Hash.SHA2.SHA512_256;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - utility functions
-------------------------------------------------------------------------------}

Function SHA2ToStr(SHA224: TSHA224): String;
var
  Hash: TSHA224Hash;
begin
Hash := TSHA224Hash.CreateAndInitFrom(SHA224);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SHA2ToStr(SHA256: TSHA256): String;
var
  Hash: TSHA256Hash;
begin
Hash := TSHA256Hash.CreateAndInitFrom(SHA256);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SHA2ToStr(SHA384: TSHA384): String;
var
  Hash: TSHA384Hash;
begin
Hash := TSHA384Hash.CreateAndInitFrom(SHA384);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SHA2ToStr(SHA512: TSHA512): String;
var
  Hash: TSHA512Hash;
begin
Hash := TSHA512Hash.CreateAndInitFrom(SHA512);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SHA2ToStr(SHA512_224: TSHA512_224): String;
var
  Hash: TSHA512_224Hash;
begin
Hash := TSHA512_224Hash.CreateAndInitFrom(SHA512_224);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SHA2ToStr(SHA512_256: TSHA512_256): String;
var
  Hash: TSHA512_256Hash;
begin
Hash := TSHA512_256Hash.CreateAndInitFrom(SHA512_256);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SHA2ToStr(SHA2: TSHA2): String;
begin
case SHA2.HashFunction of
  fnSHA224:     Result := SHA2ToStr(SHA2.SHA224);
  fnSHA256:     Result := SHA2ToStr(SHA2.SHA256);
  fnSHA384:     Result := SHA2ToStr(SHA2.SHA384);
  fnSHA512:     Result := SHA2ToStr(SHA2.SHA512);
  fnSHA512_224: Result := SHA2ToStr(SHA2.SHA512_224);
  fnSHA512_256: Result := SHA2ToStr(SHA2.SHA512_256);
else
  raise ESHA2InvalidFunction.CreateFmt('SHA2ToStr: Invalid hash function (%d)',[Ord(SHA2.HashFunction)]);
end;
end;

//------------------------------------------------------------------------------

Function StrToSHA2_224(Str: String): TSHA224;
var
  Hash: TSHA224Hash;
begin
Hash := TSHA224Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.SHA224;
finally
  Hash.Free;
end;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2_256(Str: String): TSHA256;
var
  Hash: TSHA256Hash;
begin
Hash := TSHA256Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.SHA256;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2_384(Str: String): TSHA384;
var
  Hash: TSHA384Hash;
begin
Hash := TSHA384Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.SHA384;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2_512(Str: String): TSHA512;
var
  Hash: TSHA512Hash;
begin
Hash := TSHA512Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.SHA512;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2_512_224(Str: String): TSHA512_224;
var
  Hash: TSHA512_224Hash;
begin
Hash := TSHA512_224Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.SHA512_224;
finally
  Hash.Free;
end;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2_512_256(Str: String): TSHA512_256;
var
  Hash: TSHA512_256Hash;
begin
Hash := TSHA512_256Hash.Create;
try
  Hash.FromString(Str);
  Result := Hash.SHA512_256;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2(HashFunction: TSHA2Function; Str: String): TSHA2;
begin
Result.HashFunction := HashFunction;
case HashFunction of
  fnSHA224:     Result.SHA224 := StrToSHA2_224(Str);
  fnSHA256:     Result.SHA256 := StrToSHA2_256(Str);
  fnSHA384:     Result.SHA384 := StrToSHA2_384(Str);
  fnSHA512:     Result.SHA512 := StrToSHA2_512(Str);
  fnSHA512_224: Result.SHA512_224 := StrToSHA2_512_224(Str);
  fnSHA512_256: Result.SHA512_256 := StrToSHA2_512_256(Str);
else
  raise ESHA2InvalidFunction.CreateFmt('StrToSHA2: Invalid hash function (%d)',[Ord(HashFunction)]);
end;
end;

//------------------------------------------------------------------------------

Function TryStrToSHA2(const Str: String; out SHA224: TSHA224): Boolean;
var
  Hash: TSHA224Hash;
begin
Hash := TSHA224Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    SHA224 := Hash.SHA224;
finally
  Hash.Free;
end;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryStrToSHA2(const Str: String; out SHA256: TSHA256): Boolean;
var
  Hash: TSHA256Hash;
begin
Hash := TSHA256Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    SHA256 := Hash.SHA256;
finally
  Hash.Free;
end;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryStrToSHA2(const Str: String; out SHA384: TSHA384): Boolean;
var
  Hash: TSHA384Hash;
begin
Hash := TSHA384Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    SHA384 := Hash.SHA384;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryStrToSHA2(const Str: String; out SHA512: TSHA512): Boolean;
var
  Hash: TSHA512Hash;
begin
Hash := TSHA512Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    SHA512 := Hash.SHA512;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryStrToSHA2(const Str: String; out SHA512_224: TSHA512_224): Boolean;
var
  Hash: TSHA512_224Hash;
begin
Hash := TSHA512_224Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    SHA512_224 := Hash.SHA512_224;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryStrToSHA2(const Str: String; out SHA512_256: TSHA512_256): Boolean;
var
  Hash: TSHA512_256Hash;
begin
Hash := TSHA512_256Hash.Create;
try
  Result := Hash.TryFromString(Str);
  If Result then
    SHA512_256 := Hash.SHA512_256;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TryStrToSHA2(HashFunction: TSHA2Function; const Str: String; out SHA2: TSHA2): Boolean;
begin
case HashFunction of
  fnSHA224:     Result := TryStrToSHA2(Str,SHA2.SHA224);
  fnSHA256:     Result := TryStrToSHA2(Str,SHA2.SHA256);
  fnSHA384:     Result := TryStrToSHA2(Str,SHA2.SHA384);
  fnSHA512:     Result := TryStrToSHA2(Str,SHA2.SHA512);
  fnSHA512_224: Result := TryStrToSHA2(Str,SHA2.SHA512_224);
  fnSHA512_256: Result := TryStrToSHA2(Str,SHA2.SHA512_256);
else
  raise ESHA2InvalidFunction.CreateFmt('TryStrToSHA2: Invalid hash function (%d)',[Ord(HashFunction)]);
end;
end;

//------------------------------------------------------------------------------

Function StrToSHA2Def(const Str: String; Default: TSHA224): TSHA224;
var
  Hash: TSHA224Hash;
begin
Hash := TSHA224Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.SHA224;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2Def(const Str: String; Default: TSHA256): TSHA256;
var
  Hash: TSHA256Hash;
begin
Hash := TSHA256Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.SHA256;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2Def(const Str: String; Default: TSHA384): TSHA384;
var
  Hash: TSHA384Hash;
begin
Hash := TSHA384Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.SHA384;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2Def(const Str: String; Default: TSHA512): TSHA512;
var
  Hash: TSHA512Hash;
begin
Hash := TSHA512Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.SHA512;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2Def(const Str: String; Default: TSHA512_224): TSHA512_224;
var
  Hash: TSHA512_224Hash;
begin
Hash := TSHA512_224Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.SHA512_224;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2Def(const Str: String; Default: TSHA512_256): TSHA512_256;
var
  Hash: TSHA512_256Hash;
begin
Hash := TSHA512_256Hash.Create;
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.SHA512_256;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function StrToSHA2Def(HashFunction: TSHA2Function; const Str: String; Default: TSHA2): TSHA2;
begin
If HashFunction = Default.HashFunction then
  begin
    Result.HashFunction := HashFunction;
    case HashFunction of
      fnSHA224:     Result.SHA224 := StrToSHA2Def(Str,Default.SHA224);
      fnSHA256:     Result.SHA256 := StrToSHA2Def(Str,Default.SHA256);
      fnSHA384:     Result.SHA384 := StrToSHA2Def(Str,Default.SHA384);
      fnSHA512:     Result.SHA512 := StrToSHA2Def(Str,Default.SHA512);
      fnSHA512_224: Result.SHA512_224 := StrToSHA2Def(Str,Default.SHA512_224);
      fnSHA512_256: Result.SHA512_256 := StrToSHA2Def(Str,Default.SHA512_256);
    else
      raise ESHA2Invalidfunction.CreateFmt('StrToSHA2Def: Invalid hash function (%d)',[Ord(HashFunction)]);
    end;
  end
else raise ESHA2Invalidfunction.CreateFmt(
  'StrToSHA2Def: Requested hash function differs from hash function of default value (%d,%d)',
  [Ord(HashFunction),Ord(Default.HashFunction)]);
end;

//------------------------------------------------------------------------------

Function CompareSHA2(A,B: TSHA224): Integer;
var
  HashA:  TSHA224Hash;
  HashB:  TSHA224Hash;
begin
HashA := TSHA224Hash.CreateAndInitFrom(A);
try
  HashB := TSHA224Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Compare(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareSHA2(A,B: TSHA256): Integer;
var
  HashA:  TSHA256Hash;
  HashB:  TSHA256Hash;
begin
HashA := TSHA256Hash.CreateAndInitFrom(A);
try
  HashB := TSHA256Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Compare(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareSHA2(A,B: TSHA384): Integer;
var
  HashA:  TSHA384Hash;
  HashB:  TSHA384Hash;
begin
HashA := TSHA384Hash.CreateAndInitFrom(A);
try
  HashB := TSHA384Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Compare(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareSHA2(A,B: TSHA512): Integer;
var
  HashA:  TSHA512Hash;
  HashB:  TSHA512Hash;
begin
HashA := TSHA512Hash.CreateAndInitFrom(A);
try
  HashB := TSHA512Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Compare(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareSHA2(A,B: TSHA512_224): Integer;
var
  HashA:  TSHA512_224Hash;
  HashB:  TSHA512_224Hash;
begin
HashA := TSHA512_224Hash.CreateAndInitFrom(A);
try
  HashB := TSHA512_224Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Compare(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareSHA2(A,B: TSHA512_256): Integer;
var
  HashA:  TSHA512_256Hash;
  HashB:  TSHA512_256Hash;
begin
HashA := TSHA512_256Hash.CreateAndInitFrom(A);
try
  HashB := TSHA512_256Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Compare(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CompareSHA2(A,B: TSHA2): Integer;
begin
If A.HashFunction = B.HashFunction then
  begin
    case A.HashFunction of
      fnSHA224:     Result := CompareSHA2(A.SHA224,B.SHA224);
      fnSHA256:     Result := CompareSHA2(A.SHA256,B.SHA256);
      fnSHA384:     Result := CompareSHA2(A.SHA384,B.SHA384);
      fnSHA512:     Result := CompareSHA2(A.SHA512,B.SHA512);
      fnSHA512_224: Result := CompareSHA2(A.SHA512_224,B.SHA512_224);
      fnSHA512_256: Result := CompareSHA2(A.SHA512_256,B.SHA512_256);
    else
      raise ESHA2InvalidFunction.CreateFmt('CompareSHA2: Invalid hash function (%d)',[Ord(A.HashFunction)]);
    end;
  end
else raise ESHA2InvalidFunction.CreateFmt('CompareSHA2: Functions do not match (%d,%d)',[Ord(A.HashFunction),Ord(B.HashFunction)]);
end;

//------------------------------------------------------------------------------

Function SameSHA2(A,B: TSHA224): Boolean;
var
  HashA:  TSHA224Hash;
  HashB:  TSHA224Hash;
begin
HashA := TSHA224Hash.CreateAndInitFrom(A);
try
  HashB := TSHA224Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Same(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameSHA2(A,B: TSHA256): Boolean;
var
  HashA:  TSHA256Hash;
  HashB:  TSHA256Hash;
begin
HashA := TSHA256Hash.CreateAndInitFrom(A);
try
  HashB := TSHA256Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Same(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameSHA2(A,B: TSHA384): Boolean;
var
  HashA:  TSHA384Hash;
  HashB:  TSHA384Hash;
begin
HashA := TSHA384Hash.CreateAndInitFrom(A);
try
  HashB := TSHA384Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Same(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameSHA2(A,B: TSHA512): Boolean;
var
  HashA:  TSHA512Hash;
  HashB:  TSHA512Hash;
begin
HashA := TSHA512Hash.CreateAndInitFrom(A);
try
  HashB := TSHA512Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Same(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameSHA2(A,B: TSHA512_224): Boolean;
var
  HashA:  TSHA512_224Hash;
  HashB:  TSHA512_224Hash;
begin
HashA := TSHA512_224Hash.CreateAndInitFrom(A);
try
  HashB := TSHA512_224Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Same(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;
   
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameSHA2(A,B: TSHA512_256): Boolean;
var
  HashA:  TSHA512_256Hash;
  HashB:  TSHA512_256Hash;
begin
HashA := TSHA512_256Hash.CreateAndInitFrom(A);
try
  HashB := TSHA512_256Hash.CreateAndInitFrom(B);
  try
    Result := HashA.Same(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SameSHA2(A,B: TSHA2): Boolean;
begin
If A.HashFunction = B.HashFunction then
  begin
    case A.HashFunction of
      fnSHA224:     Result := SameSHA2(A.SHA224,B.SHA224);
      fnSHA256:     Result := SameSHA2(A.SHA256,B.SHA256);
      fnSHA384:     Result := SameSHA2(A.SHA384,B.SHA384);
      fnSHA512:     Result := SameSHA2(A.SHA512,B.SHA512);
      fnSHA512_224: Result := SameSHA2(A.SHA512_224,B.SHA512_224);
      fnSHA512_256: Result := SameSHA2(A.SHA512_256,B.SHA512_256);
    else
      raise ESHA2InvalidFunction.CreateFmt('SameSHA2: Invalid hash function (%d)',[Ord(A.HashFunction)]);
    end;
  end
else raise ESHA2InvalidFunction.CreateFmt('SameSHA2: Functions do not match (%d,%d)',[Ord(A.HashFunction),Ord(B.HashFunction)]);
end;

//------------------------------------------------------------------------------

Function BinaryCorrectSHA2(SHA224: TSHA224): TSHA224;
begin
Result := SHA224;
end; 
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BinaryCorrectSHA2(SHA256: TSHA256): TSHA256;
begin
Result := SHA256;
end; 
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BinaryCorrectSHA2(SHA384: TSHA384): TSHA384;
begin
Result := SHA384;
end; 
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BinaryCorrectSHA2(SHA512: TSHA512): TSHA512;
begin
Result := SHA512;
end; 
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BinaryCorrectSHA2(SHA512_224: TSHA512_224): TSHA512_224;
begin
Result := SHA512_224;
end;  
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BinaryCorrectSHA2(SHA512_256: TSHA512_256): TSHA512_256;
begin
Result := SHA512_256;
end; 
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BinaryCorrectSHA2(SHA2: TSHA2): TSHA2;
begin
Result := SHA2;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - low-level processing functions
-------------------------------------------------------------------------------}

procedure BufferSHA2(var SHA224: TSHA224; const Buffer; Size: TMemSize);
var
  Hash: TSHA224Hash;
begin
Hash := TSHA224Hash.CreateAndInitFrom(SHA224);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          SHA224 := Hash.SHA224;
        end
      else raise ESHA2ProcessingError.CreateFmt('BufferSHA2: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure BufferSHA2(var SHA256: TSHA256; const Buffer; Size: TMemSize);
var
  Hash: TSHA256Hash;
begin
Hash := TSHA256Hash.CreateAndInitFrom(SHA256);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          SHA256 := Hash.SHA256;
        end
      else raise ESHA2ProcessingError.CreateFmt('BufferSHA2: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;
   
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure BufferSHA2(var SHA384: TSHA384; const Buffer; Size: TMemSize);
var
  Hash: TSHA384Hash;
begin
Hash := TSHA384Hash.CreateAndInitFrom(SHA384);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          SHA384 := Hash.SHA384;
        end
      else raise ESHA2ProcessingError.CreateFmt('BufferSHA2: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;
  
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure BufferSHA2(var SHA512: TSHA512; const Buffer; Size: TMemSize);
var
  Hash: TSHA512Hash;
begin
Hash := TSHA512Hash.CreateAndInitFrom(SHA512);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          SHA512 := Hash.SHA512;
        end
      else raise ESHA2ProcessingError.CreateFmt('BufferSHA2: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;
   
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure BufferSHA2(var SHA512_224: TSHA512_224; const Buffer; Size: TMemSize);
var
  Hash: TSHA512_224Hash;
begin
Hash := TSHA512_224Hash.CreateAndInitFrom(SHA512_224);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          SHA512_224 := Hash.SHA512_224;
        end
      else raise ESHA2ProcessingError.CreateFmt('BufferSHA2: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;
    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure BufferSHA2(var SHA512_256: TSHA512_256; const Buffer; Size: TMemSize);
var
  Hash: TSHA512_256Hash;
begin
Hash := TSHA512_256Hash.CreateAndInitFrom(SHA512_256);
try
  If Size > 0 then
    begin
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Update(Buffer,Size);
          SHA512_256 := Hash.SHA512_256;
        end
      else raise ESHA2ProcessingError.CreateFmt('BufferSHA2: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    end;
finally
  Hash.Free;
end;
end;
   
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure BufferSHA2(var SHA2: TSHA2; const Buffer; Size: TMemSize);
begin
case SHA2.HashFunction of
  fnSHA224:     BufferSHA2(SHA2.SHA224,Buffer,Size);
  fnSHA256:     BufferSHA2(SHA2.SHA256,Buffer,Size);
  fnSHA384:     BufferSHA2(SHA2.SHA384,Buffer,Size);
  fnSHA512:     BufferSHA2(SHA2.SHA512,Buffer,Size);
  fnSHA512_224: BufferSHA2(SHA2.SHA512_224,Buffer,Size);
  fnSHA512_256: BufferSHA2(SHA2.SHA512_256,Buffer,Size);
else
  raise ESHA2InvalidFunction.CreateFmt('BufferSHA2: Invalid hash function (%d)',[Ord(SHA2.HashFunction)]);
end;
end;

//------------------------------------------------------------------------------

Function LastBufferSHA2(SHA224: TSHA224; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA224;
var
  Hash: TSHA224Hash;
begin
Hash := TSHA224Hash.CreateAndInitFrom(SHA224);
try
  Hash.ProcessedBytes := (MessageLength shr 3) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.SHA224;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA256: TSHA256; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA256;
var
  Hash: TSHA256Hash;
begin
Hash := TSHA256Hash.CreateAndInitFrom(SHA256);
try
  Hash.ProcessedBytes := (MessageLength shr 3) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.SHA256;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA384: TSHA384; const Buffer; Size: TMemSize; MessageLength: UInt128): TSHA384;
var
  Hash: TSHA384Hash;
begin
Hash := TSHA384Hash.CreateAndInitFrom(SHA384);
try
  Hash.ProcessedBytes := MessageLengthToSize(MessageLength) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.SHA384;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512: TSHA512; const Buffer; Size: TMemSize; MessageLength: UInt128): TSHA512;
var
  Hash: TSHA512Hash;
begin
Hash := TSHA512Hash.CreateAndInitFrom(SHA512);
try
  Hash.ProcessedBytes := MessageLengthToSize(MessageLength) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.SHA512;
finally
  Hash.Free;
end;
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512_224: TSHA512_224; const Buffer; Size: TMemSize; MessageLength: UInt128): TSHA512_224;
var
  Hash: TSHA512_224Hash;
begin
Hash := TSHA512_224Hash.CreateAndInitFrom(SHA512_224);
try
  Hash.ProcessedBytes := MessageLengthToSize(MessageLength) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.SHA512_224;
finally
  Hash.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512_256: TSHA512_256; const Buffer; Size: TMemSize; MessageLength: UInt128): TSHA512_256;
var
  Hash: TSHA512_256Hash;
begin
Hash := TSHA512_256Hash.CreateAndInitFrom(SHA512_256);
try
  Hash.ProcessedBytes := MessageLengthToSize(MessageLength) - Size;
  Hash.Final(Buffer,Size);
  Result := Hash.SHA512_256;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function LastBufferSHA2(SHA384: TSHA384; const Buffer; Size: TMemSize; MessageLengthLo: UInt64): TSHA384;
begin
Result := LastBufferSHA2(SHA384,Buffer,Size,BuildOctaWord(MessageLengthLo));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512: TSHA512; const Buffer; Size: TMemSize; MessageLengthLo: UInt64): TSHA512;
begin
Result := LastBufferSHA2(SHA512,Buffer,Size,BuildOctaWord(MessageLengthLo));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512_224: TSHA512_224; const Buffer; Size: TMemSize; MessageLengthLo: UInt64): TSHA512_224;
begin
Result := LastBufferSHA2(SHA512_224,Buffer,Size,BuildOctaWord(MessageLengthLo));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512_256: TSHA512_256; const Buffer; Size: TMemSize; MessageLengthLo: UInt64): TSHA512_256;
begin
Result := LastBufferSHA2(SHA512_256,Buffer,Size,BuildOctaWord(MessageLengthLo));
end;

//------------------------------------------------------------------------------

Function LastBufferSHA2(SHA384: TSHA384; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA384;
begin
Result := LastBufferSHA2(SHA384,Buffer,Size,BuildOctaWord(MessageLengthLo,MessageLengthHi));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512: TSHA512; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA512;
begin
Result := LastBufferSHA2(SHA512,Buffer,Size,BuildOctaWord(MessageLengthLo,MessageLengthHi));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512_224: TSHA512_224; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA512_224;
begin
Result := LastBufferSHA2(SHA512_224,Buffer,Size,BuildOctaWord(MessageLengthLo,MessageLengthHi));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512_256: TSHA512_256; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA512_256;
begin
Result := LastBufferSHA2(SHA512_256,Buffer,Size,BuildOctaWord(MessageLengthLo,MessageLengthHi));
end;

//------------------------------------------------------------------------------

Function LastBufferSHA2(SHA2: TSHA2; const Buffer; Size: TMemSize; MessageLength: UInt64): TSHA2; overload;
begin
Result.HashFunction := SHA2.HashFunction;
case SHA2.HashFunction of
  fnSHA224:     Result.SHA224 := LastBufferSHA2(SHA2.SHA224,Buffer,Size,MessageLength);
  fnSHA256:     Result.SHA256 := LastBufferSHA2(SHA2.SHA256,Buffer,Size,MessageLength);
  fnSHA384:     Result.SHA384 := LastBufferSHA2(SHA2.SHA384,Buffer,Size,MessageLength);
  fnSHA512:     Result.SHA512 := LastBufferSHA2(SHA2.SHA512,Buffer,Size,MessageLength);
  fnSHA512_224: Result.SHA512_224 := LastBufferSHA2(SHA2.SHA512_224,Buffer,Size,MessageLength);
  fnSHA512_256: Result.SHA512_256 := LastBufferSHA2(SHA2.SHA512_256,Buffer,Size,MessageLength);
else
  raise ESHA2InvalidFunction.CreateFmt('LastBufferSHA2: Invalid hash function (%d)',[Ord(SHA2.HashFunction)]);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA2: TSHA2; const Buffer; Size: TMemSize; MessageLengthLo, MessageLengthHi: UInt64): TSHA2; overload;
begin
Result.HashFunction := SHA2.HashFunction;
case SHA2.HashFunction of
  fnSHA224:     Result.SHA224 := LastBufferSHA2(SHA2.SHA224,Buffer,Size,MessageLengthLo);
  fnSHA256:     Result.SHA256 := LastBufferSHA2(SHA2.SHA256,Buffer,Size,MessageLengthLo);
  fnSHA384:     Result.SHA384 := LastBufferSHA2(SHA2.SHA384,Buffer,Size,MessageLengthLo,MessageLengthHi);
  fnSHA512:     Result.SHA512 := LastBufferSHA2(SHA2.SHA512,Buffer,Size,MessageLengthLo,MessageLengthHi);
  fnSHA512_224: Result.SHA512_224 := LastBufferSHA2(SHA2.SHA512_224,Buffer,Size,MessageLengthLo,MessageLengthHi);
  fnSHA512_256: Result.SHA512_256 := LastBufferSHA2(SHA2.SHA512_256,Buffer,Size,MessageLengthLo,MessageLengthHi);
else
  raise ESHA2InvalidFunction.CreateFmt('LastBufferSHA2: Invalid hash function (%d)',[Ord(SHA2.HashFunction)]);
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA2: TSHA2; const Buffer; Size: TMemSize; MessageLength: OctaWord): TSHA2; overload;
begin
Result.HashFunction := SHA2.HashFunction;
case SHA2.HashFunction of
  fnSHA224:     Result.SHA224 := LastBufferSHA2(SHA2.SHA224,Buffer,Size,MessageLength.Lo);
  fnSHA256:     Result.SHA256 := LastBufferSHA2(SHA2.SHA256,Buffer,Size,MessageLength.Lo);
  fnSHA384:     Result.SHA384 := LastBufferSHA2(SHA2.SHA384,Buffer,Size,MessageLength);
  fnSHA512:     Result.SHA512 := LastBufferSHA2(SHA2.SHA512,Buffer,Size,MessageLength);
  fnSHA512_224: Result.SHA512_224 := LastBufferSHA2(SHA2.SHA512_224,Buffer,Size,MessageLength);
  fnSHA512_256: Result.SHA512_256 := LastBufferSHA2(SHA2.SHA512_256,Buffer,Size,MessageLength);
else
  raise ESHA2InvalidFunction.CreateFmt('LastBufferSHA2: Invalid hash function (%d)',[Ord(SHA2.HashFunction)]);
end;
end;

//------------------------------------------------------------------------------

Function LastBufferSHA2(SHA224: TSHA224; const Buffer; Size: TMemSize): TSHA224;
begin
Result := LastBufferSHA2(SHA224,Buffer,Size,UInt64(Size) shl 3);
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA256: TSHA256; const Buffer; Size: TMemSize): TSHA256;
begin
Result := LastBufferSHA2(SHA256,Buffer,Size,UInt64(Size) shl 3);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA384: TSHA384; const Buffer; Size: TMemSize): TSHA384;
begin
Result := LastBufferSHA2(SHA384,Buffer,Size,SizeToMessageLength(Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512: TSHA512; const Buffer; Size: TMemSize): TSHA512;
begin
Result := LastBufferSHA2(SHA512,Buffer,Size,SizeToMessageLength(Size));
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512_224: TSHA512_224; const Buffer; Size: TMemSize): TSHA512_224;
begin
Result := LastBufferSHA2(SHA512_224,Buffer,Size,SizeToMessageLength(Size));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA512_256: TSHA512_256; const Buffer; Size: TMemSize): TSHA512_256;
begin
Result := LastBufferSHA2(SHA512_256,Buffer,Size,SizeToMessageLength(Size));
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function LastBufferSHA2(SHA2: TSHA2; const Buffer; Size: TMemSize): TSHA2;
begin
Result.HashFunction := SHA2.HashFunction;
case SHA2.HashFunction of
  fnSHA224:     Result.SHA224 := LastBufferSHA2(SHA2.SHA224,Buffer,Size);
  fnSHA256:     Result.SHA256 := LastBufferSHA2(SHA2.SHA256,Buffer,Size);
  fnSHA384:     Result.SHA384 := LastBufferSHA2(SHA2.SHA384,Buffer,Size);
  fnSHA512:     Result.SHA512 := LastBufferSHA2(SHA2.SHA512,Buffer,Size);
  fnSHA512_224: Result.SHA512_224 := LastBufferSHA2(SHA2.SHA512_224,Buffer,Size);
  fnSHA512_256: Result.SHA512_256 := LastBufferSHA2(SHA2.SHA512_256,Buffer,Size);
else
  raise ESHA2InvalidFunction.CreateFmt('LastBufferSHA2: Invalid hash function (%d)',[Ord(SHA2.HashFunction)]);
end;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - processing functions
-------------------------------------------------------------------------------}

Function BufferSHA2(HashFunction: TSHA2Function; const Buffer; Size: TMemSize): TSHA2;
var
  Hash: TSHA2Hash;
begin
Hash := CreateByFunction(HashFunction);
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.SHA2;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringSHA2(HashFunction: TSHA2Function; const Str: AnsiString): TSHA2;
var
  Hash: TSHA2Hash;
begin
Hash := CreateByFunction(HashFunction);
try
  Hash.HashAnsiString(Str);
  Result := Hash.SHA2;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function WideStringSHA2(HashFunction: TSHA2Function; const Str: WideString): TSHA2;
var
  Hash: TSHA2Hash;
begin
Hash := CreateByFunction(HashFunction);
try
  Hash.HashWideString(Str);
  Result := Hash.SHA2;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function StringSHA2(HashFunction: TSHA2Function; const Str: String): TSHA2;
var
  Hash: TSHA2Hash;
begin
Hash := CreateByFunction(HashFunction);
try
  Hash.HashString(Str);
  Result := Hash.SHA2;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function StreamSHA2(HashFunction: TSHA2Function; Stream: TStream; Count: Int64 = -1): TSHA2;
var
  Hash: TSHA2Hash;
begin
Hash := CreateByFunction(HashFunction);
try
  Hash.HashStream(Stream,Count);
  Result := Hash.SHA2;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSHA2(HashFunction: TSHA2Function; const FileName: String): TSHA2;
var
  Hash: TSHA2Hash;
begin
Hash := CreateByFunction(HashFunction);
try
  Hash.HashFile(FileName);
  Result := Hash.SHA2;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - context functions
-------------------------------------------------------------------------------}

Function SHA2_Init(HashFunction: TSHA2Function): TSHA2Context;
var
  Temp: TSHA2Hash;
begin
Temp := CreateByFunction(HashFunction);
Temp.Init;
Result := TSHA2Context(Temp);
end;

//------------------------------------------------------------------------------

procedure SHA2_Update(Context: TSHA2Context; const Buffer; Size: TMemSize);
begin
TSHA2Hash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SHA2_Final(var Context: TSHA2Context; const Buffer; Size: TMemSize): TSHA2;
begin
SHA2_Update(Context,Buffer,Size);
Result := SHA2_Final(Context);
end;
 
//------------------------------------------------------------------------------

Function SHA2_Final(var Context: TSHA2Context): TSHA2;
begin
TSHA2Hash(Context).Final;
Result := TSHA2Hash(Context).SHA2;
FreeAndNil(TSHA2Hash(Context));
end;

//------------------------------------------------------------------------------

Function SHA2_Hash(HashFunction: TSHA2Function; const Buffer; Size: TMemSize): TSHA2;
var
  Hash: TSHA2Hash;
begin
Hash := CreateByFunction(HashFunction);
try
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.SHA2;
finally
  Hash.Free;
end;
end;

end.
