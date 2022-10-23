{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SHA-3/Keccak calculation

    This unit provides a mean of calculating SHA-3 hash of pretty much any data.
    The only limiting factor is, that the data must consist of whole bytes, ie.
    arbitrary bitstrings are not supported. SHA-3 is based on Keccak, so it is
    provided in limited form too.

    As SHA-3 is derived from Keccak-f[1600] (permutation width of 1600 bits,
    64bit lanes/words), all hashes provided here are also based on this
    permutation. To be more specific, everything is based on Keccak-p[1600,24],
    a generalization of Keccak-f[1600] with 24 rounds per permutation (number
    of rounds Nr = 12 + 2L, where L = log2(w), w is width of a keccak word, or,
    in keccak terminology, a lane size).

    Let's have following sponge function:

        SPONGE[f,pad,r]

            f   - underlying sponge function (permutation function)
            pad - padding rule or algorithm for input (padding to block boundary)
            r   - (bit)rate of the sponge function (defines size of input block)

    Then we can define Keccak[c] as this:

        Keccak[c] = SPONGE[Keccak-p[1600.24],pad10*1,1600-c]

            c - capacity of the sponge function

    pad10*1 (multi-rate padding) pretty much adds one 1 bit at the end of the
    message, followed by none or more zeroes. The block is then closed by 1 bit.

    Using all this, any of the implemented function can be described by
    following formula:

        Function(M) = Keccak[c](M||pad,d)

            M   - input message (data) that will be hashed
            pad - optional padding bits beyond original pad10*1 padding (added
                  before the original padding bits)
            d   - number of bits in the output digest (hash)

    Following twelve functions, or hash variants, are implemented and therefore
    supported by this library:

        Keccak[]            SHA3-224
        Keccak224           SHA3-256
        Keccak256           SHA3-384
        Keccak384           SHA3-512
        Keccak512           SHAKE128
        Keccak[c]           SHAKE256

    Each variant is implemeted in its own class. They all share the same
    interface inherited from TBlockHash, but each provides some specialized
    methods and properties.
    Inheritance tree of all present class looks like this (star marks an
    abstract class, these must not be directly instantiated):

      *TKeccakHash --- TKeccak0Hash
                    |
                    |- *TKeccakDefHash --- *TKeccakFixHash --- TKeccak224Hash
                                        |                   |- TKeccak256Hash
                                        |                   |- TKeccak384Hash
                                        |                   |- TKeccak512Hash
                                        |                   |
                                        |                   |- *TSHA3Hash ---- TSHA3_224Hash
                                        |                                   |- TSHA3_256Hash
                                        |                                   |- TSHA3_384Hash
                                        |                                   |- TSHA3_512Hash
                                        |
                                        |- *TKeccakVarHash --- TKeccakCHash
                                                            |
                                                            |- *TSHAKEHash --- TSHAKE128Hash
                                                                            |- TSHAKE256Hash

    Keccak[], in this library denoted as Keccak0, is a special case as it does
    not produce any hash. Instead, it has public methods Permute and Squeeze
    which can be used to produce output. It is intended to be used for other
    purposes than hashing, for example as a base for a pseudo-random number
    generator. It is defined as (note 576 is a default capacity for Keccak):

        Keccak[](M) = Keccak[576](M,0)

    Keccak224 trough Keccak512 are defined as:

        Keccak224(M) = Keccak[448](M,224)
        Keccak256(M) = Keccak[512](M,256)
        Keccak384(M) = Keccak[768](M,384)
        Keccak512(M) = Keccak[1024](M,512)

    Keccak[c] is, again, a special case. Both capacity and hash length can be
    selected independetly. Capacity can be anything from 8 to 1592. Hash bits
    must be larger than zero, other than that they are limited only by available
    memory. Thanks to this, Keccak[c] can be treated as an XOF - an
    eXtendable-Output Function.
    Defining this function from previous formula has no meaning, as it is fully
    parametrized. It can be viewed as a direct implementation of this
    generalization.

    SHA-3 family of functions differ only slightly from the original Keccak.
    They append few more bits to the message and sets capacity to double of the
    hash length in bits. The functions are defined:

        SHA3-224(M) = Keccak[448](M||01,224)
        SHA3-256(M) = Keccak[512](M||01,256)
        SHA3-384(M) = Keccak[768](M||01,384)
        SHA3-512(M) = Keccak[1024](M||01,512)

    SHAKE functions are extendable-output functions, meaning they produce
    variable length hash, of which length can be selected before hashing.
    They can be defined:

        SHAKE128(M,d) = KECCAK[256](M||1111,d)
        SHAKE256(M,d) = KECCAK[512](M||1111,d)

  Version 1.2.1 (2020-07-13)

  Last change 2022-09-24

  ©2015-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SHA3

  Dependencies:
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    HashBase           - github.com/TheLazyTomcat/Lib.HashBase
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID

  SimpleCPUID might not be needed, see BitOps library for details.

===============================================================================}
unit SHA3;

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
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
    Common types and constants
===============================================================================}
{
  Bytes in all Keccak, SHA-3 and SHAKE hashes are always ordered from the most
  significant byte to the least significant byte (big endian).

  Keccak/SHA-3/SHAKE does not differ in little and big endian forms, as it is
  not a single quantity, therefore methods like SHA3_*ToLE or SHA3_*ToBE do
  nothing and are present only for the sake of completeness.
}
type
  // fixed length hashes
  TKeccak224 = packed array[0..27] of UInt8;    PKeccak224 = ^TKeccak224;
  TKeccak256 = packed array[0..31] of UInt8;    PKeccak256 = ^TKeccak256;
  TKeccak384 = packed array[0..47] of UInt8;    PKeccak384 = ^TKeccak384;
  TKeccak512 = packed array[0..63] of UInt8;    PKeccak512 = ^TKeccak512;

  TSHA3_224 = type TKeccak224;    PSHA3_224 = ^TSHA3_224;
  TSHA3_256 = type TKeccak256;    PSHA3_256 = ^TSHA3_256;
  TSHA3_384 = type TKeccak384;    PSHA3_384 = ^TSHA3_384;
  TSHA3_512 = type TKeccak512;    PSHA3_512 = ^TSHA3_512;

  // variable length hashes
  TKeccakVar = packed array of UInt8; // also used internally as a buffer

  TKeccakC = type TKeccakVar;     PKeccakC = ^TKeccakC;

  TSHAKE128 = type TKeccakVar;    PSHAKE128 = ^TSHAKE128;
  TSHAKE256 = type TKeccakVar;    PSHAKE256 = ^TSHAKE256;

  TKeccakFunction = (fnKeccak0,fnKeccak224,fnKeccak256,fnKeccak384,fnKeccak512,fnKeccakC,
                     fnSHA3_224,fnSHA3_256,fnSHA3_384,fnSHA3_512,fnSHAKE128,fnSHAKE256);

  TKeccak = record
    HashFunction: TKeccakFunction;
    HashBits:     UInt32;
    HashData:     TKeccakVar;
  end;

  // some aliases
  TSHA3Function = TKeccakFunction;
  
  TSHA3 = TKeccak;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const
  ZeroKeccak224: TKeccak224 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroKeccak256: TKeccak256 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroKeccak384: TKeccak384 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroKeccak512: TKeccak512 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

  ZeroSHA3_224: TSHA3_224 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroSHA3_256: TSHA3_256 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroSHA3_384: TSHA3_384 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  ZeroSHA3_512: TSHA3_512 = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
type
  TKeccakWord = UInt64;

  TKeccakSponge = packed array[0..4,0..4] of TKeccakWord;

  TKeccakSpongeWordOverlay = packed array[0..24] of TKeccakWord;
  TKeccakSpongeByteOverlay = packed array[0..Pred(25 * SizeOf(TKeccakWord))] of UInt8;  

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type
  ESHA3Exception = class(EHashException);

  ESHA3IncompatibleClass    = class(ESHA3Exception);
  ESHA3IncompatibleFunction = class(ESHA3Exception);
  ESHA3IncompatibleHashBits = class(ESHA3Exception);
  ESHA3IncompatibleSize     = class(ESHA3Exception);
  ESHA3ProcessingError      = class(ESHA3Exception);
  ESHA3InvalidHashFunction  = class(ESHA3Exception);
  ESHA3InvalidHashBits      = class(ESHA3Exception);
  ESHA3InvalidSize          = class(ESHA3Exception);
  ESHA3InvalidCapacity      = class(ESHA3Exception);

{-------------------------------------------------------------------------------
================================================================================
                                   TKeccakHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakHash - class declaration
===============================================================================}
type
  TKeccakHash = class(TBlockHash)
  protected
    fSponge:    TKeccakSponge;
    fHashBits:  UInt32;
    fCapacity:  UInt32;
    // getters, setters
    Function GetBitrate: UInt32; virtual;
    Function GetHashBuffer: TKeccakVar; virtual;              // override in descendants
    procedure SetHashBuffer(HashBuffer: TKeccakVar); virtual; // -//-
    // endianness conversion
    class Function HashBufferToLE(HashBuffer: TKeccakVar): TKeccakVar; virtual;
    class Function HashBufferToBE(HashBuffer: TKeccakVar): TKeccakVar; virtual;
    class Function HashBufferFromLE(HashBuffer: TKeccakVar): TKeccakVar; virtual;
    class Function HashBufferFromBE(HashBuffer: TKeccakVar): TKeccakVar; virtual;
    // hash function specific stuff
    class Function CapacityFromHashBits(HashBits: UInt32): UInt32; virtual;
    class Function PaddingByte: UInt8; virtual;
    // processing
    procedure Permute; virtual;
    procedure Squeeze; overload; virtual;
    procedure SqueezeTo(var Buffer; Size: TMemSize); virtual;
    procedure ProcessFirst(const Block); override;
    procedure ProcessBlock(const Block); override;
    procedure ProcessLast; override;
    // initialization
    procedure InitHashBits(Value: UInt32); virtual;   // must be called before Initialize indescendants
    procedure Initialize; override;
  public
    class Function LaneSize: UInt32; virtual;         // in bits, also width of the keccak word
    class Function PermutationWidth: UInt32; virtual; // in bits  
    Function HashSize: TMemSize; reintroduce;         // hides class functions
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    class Function HashFunction: TKeccakFunction; virtual; abstract;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak); overload; virtual; abstract;
    procedure Init; override;
    Function Compare(Hash: THashBase): Integer; override;
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TKeccak); reintroduce; overload; virtual;
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    property Sponge: TKeccakSponge read fSponge write fSponge;
    property HashBits: UInt32 read fHashBits;
    property Capacity: UInt32 read fCapacity;
    property Bitrate: UInt32 read GetBitrate;
  end;

  TKeccakHashClass = class of TKeccakHash;  

{-------------------------------------------------------------------------------
================================================================================
                                  TKeccak0Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak0Hash - class declaration
===============================================================================}
type
  TKeccak0Hash = class(TKeccakHash)
  protected
    procedure Initialize; override;
  public
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: TKeccak); overload; override;
    procedure Permute; override;
    procedure Squeeze(var Buffer; Size: TMemSize); overload; virtual;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TKeccakDefHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakDefHash - class declaration
===============================================================================}
type
  TKeccakDefHash = class(TKeccakHash)
  protected
    Function GetKeccak: TKeccak; virtual;
  public
    property Keccak: TKeccak read GetKeccak;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TKeccakFixHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakFixHash - class declaration
===============================================================================}
type
  TKeccakFixHash = class(TKeccakDefHash)
  protected
    class Function CapacityFromHashBits(HashBits: UInt32): UInt32; override;
  public
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TKeccak); overload; override;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TKeccak224Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak224Hash - class declaration
===============================================================================}
type
  TKeccak224Hash = class(TKeccakFixHash)
  protected
    fKeccak224: TKeccak224;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    procedure Initialize; override;
  public
    class Function Keccak224ToLE(Keccak224: TKeccak224): TKeccak224; virtual;
    class Function Keccak224ToBE(Keccak224: TKeccak224): TKeccak224; virtual;
    class Function Keccak224FromLE(Keccak224: TKeccak224): TKeccak224; virtual;
    class Function Keccak224FromBE(Keccak224: TKeccak224): TKeccak224; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak224); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TKeccak224); overload; virtual;
    property Keccak224: TKeccak224 read fKeccak224;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TKeccak256Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak256Hash - class declaration
===============================================================================}
type
  TKeccak256Hash = class(TKeccakFixHash)
  protected
    fKeccak256: TKeccak256;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    procedure Initialize; override;
  public
    class Function Keccak256ToLE(Keccak256: TKeccak256): TKeccak256; virtual;
    class Function Keccak256ToBE(Keccak256: TKeccak256): TKeccak256; virtual;
    class Function Keccak256FromLE(Keccak256: TKeccak256): TKeccak256; virtual;
    class Function Keccak256FromBE(Keccak256: TKeccak256): TKeccak256; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak256); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TKeccak256); overload; virtual;
    property Keccak256: TKeccak256 read fKeccak256;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TKeccak384Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak384Hash - class declaration
===============================================================================}
type
  TKeccak384Hash = class(TKeccakFixHash)
  protected
    fKeccak384: TKeccak384;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    procedure Initialize; override;
  public
    class Function Keccak384ToLE(Keccak384: TKeccak384): TKeccak384; virtual;
    class Function Keccak384ToBE(Keccak384: TKeccak384): TKeccak384; virtual;
    class Function Keccak384FromLE(Keccak384: TKeccak384): TKeccak384; virtual;
    class Function Keccak384FromBE(Keccak384: TKeccak384): TKeccak384; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak384); overload; virtual; 
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TKeccak384); overload; virtual;
    property Keccak384: TKeccak384 read fKeccak384;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TKeccak512Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak512Hash - class declaration
===============================================================================}
type
  TKeccak512Hash = class(TKeccakFixHash)
  protected
    fKeccak512: TKeccak512;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    procedure Initialize; override;
  public
    class Function Keccak512ToLE(Keccak512: TKeccak512): TKeccak512; virtual;
    class Function Keccak512ToBE(Keccak512: TKeccak512): TKeccak512; virtual;
    class Function Keccak512FromLE(Keccak512: TKeccak512): TKeccak512; virtual;
    class Function Keccak512FromBE(Keccak512: TKeccak512): TKeccak512; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak512); overload; virtual;  
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TKeccak512); overload; virtual;
    property Keccak512: TKeccak512 read fKeccak512;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                    TSHA3Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3Hash - class declaration
===============================================================================}
type
  TSHA3Hash = class(TKeccakFixHash)
  protected
    class Function CapacityFromHashBits(HashBits: UInt32): UInt32; override;
    class Function PaddingByte: UInt8; override;
  public
    property SHA3: TSHA3 read GetKeccak;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TSHA3_224Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3_224Hash - class declaration
===============================================================================}
type
  TSHA3_224Hash = class(TSHA3Hash)
  protected
    fSHA3_224: TSHA3_224;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    procedure Initialize; override;
  public
    class Function SHA3_224ToLE(SHA3_224: TSHA3_224): TSHA3_224; virtual;
    class Function SHA3_224ToBE(SHA3_224: TSHA3_224): TSHA3_224; virtual;
    class Function SHA3_224FromLE(SHA3_224: TSHA3_224): TSHA3_224; virtual;
    class Function SHA3_224FromBE(SHA3_224: TSHA3_224): TSHA3_224; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA3); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA3_224); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA3_224); overload; virtual;
    property SHA3_224: TSHA3_224 read fSHA3_224;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TSHA3_256Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3_256Hash - class declaration
===============================================================================}
type
  TSHA3_256Hash = class(TSHA3Hash)
  protected
    fSHA3_256: TSHA3_256;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    procedure Initialize; override;
  public
    class Function SHA3_256ToLE(SHA3_256: TSHA3_256): TSHA3_256; virtual;
    class Function SHA3_256ToBE(SHA3_256: TSHA3_256): TSHA3_256; virtual;
    class Function SHA3_256FromLE(SHA3_256: TSHA3_256): TSHA3_256; virtual;
    class Function SHA3_256FromBE(SHA3_256: TSHA3_256): TSHA3_256; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA3); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA3_256); overload; virtual; 
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA3_256); overload; virtual;
    property SHA3_256: TSHA3_256 read fSHA3_256;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TSHA3_384Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3_384Hash - class declaration
===============================================================================}
type
  TSHA3_384Hash = class(TSHA3Hash)
  protected
    fSHA3_384: TSHA3_384;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    procedure Initialize; override;
  public
    class Function SHA3_384ToLE(SHA3_384: TSHA3_384): TSHA3_384; virtual;
    class Function SHA3_384ToBE(SHA3_384: TSHA3_384): TSHA3_384; virtual;
    class Function SHA3_384FromLE(SHA3_384: TSHA3_384): TSHA3_384; virtual;
    class Function SHA3_384FromBE(SHA3_384: TSHA3_384): TSHA3_384; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA3); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA3_384); overload; virtual; 
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA3_384); overload; virtual;
    property SHA3_384: TSHA3_384 read fSHA3_384;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TSHA3_512Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3_512Hash - class declaration
===============================================================================}
type
  TSHA3_512Hash = class(TSHA3Hash)
  protected
    fSHA3_512: TSHA3_512;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    procedure Initialize; override;
  public
    class Function SHA3_512ToLE(SHA3_512: TSHA3_512): TSHA3_512; virtual;
    class Function SHA3_512ToBE(SHA3_512: TSHA3_512): TSHA3_512; virtual;
    class Function SHA3_512FromLE(SHA3_512: TSHA3_512): TSHA3_512; virtual;
    class Function SHA3_512FromBE(SHA3_512: TSHA3_512): TSHA3_512; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA3); overload; override;
    constructor CreateAndInitFrom(Hash: TSHA3_512); overload; virtual;  
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHA3_512); overload; virtual;
    property SHA3_512: TSHA3_512 read fSHA3_512;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TKeccakVarHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakVarHash - class declaration
===============================================================================}
type
  TKeccakVarHash = class(TKeccakDefHash)
  protected
    procedure SetHashBits(Value: UInt32); virtual; abstract;
    class Function CapacityFromHashBits(HashBits: UInt32): UInt32; override;
  public
    procedure FromStringDef(const Str: String; const Default: TKeccak); overload; override;
    property HashBits: UInt32 read fHashBits write SetHashBits;    
  end;

{-------------------------------------------------------------------------------
================================================================================
                                  TKeccakCHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakCHash - class declaration
===============================================================================}
type
  TKeccakCHash = class(TKeccakVarHash)
  protected
    fKeccakC: TKeccakC;
    procedure SetHashBits(Value: UInt32); override;
    procedure SetCapacity(Value: UInt32); virtual;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    Function GetKeccakC: TKeccakC; virtual;
    procedure Initialize; override;
  public
    class Function KeccakCToLE(KeccakC: TKeccakC): TKeccakC; virtual;
    class Function KeccakCToBE(KeccakC: TKeccakC): TKeccakC; virtual;
    class Function KeccakCFromLE(KeccakC: TKeccakC): TKeccakC; virtual;
    class Function KeccakCFromBE(KeccakC: TKeccakC): TKeccakC; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    class Function MaxCapacity: UInt32; virtual;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccakC); overload; virtual;
    procedure Init; override;    
    procedure FromStringDef(const Str: String; const Default: TKeccakC); overload; virtual;
    property Capacity: UInt32 read fCapacity write SetCapacity;
    property KeccakC: TKeccakC read GetKeccakC;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                   TSHAKEHash                                   
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHAKEHash - class declaration
===============================================================================}
type
  TSHAKEHash = class(TKeccakVarHash)
  protected
    class Function PaddingByte: UInt8; override;    
  end;

{-------------------------------------------------------------------------------
================================================================================
                                  TSHAKE128Hash                                 
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHAKE128Hash - class declaration
===============================================================================}
type
  TSHAKE128Hash = class(TSHAKEHash)
  protected
    fSHAKE128:  TSHAKE128;
    procedure SetHashBits(Value: UInt32); override;
    class Function CapacityFromHashBits(HashBits: UInt32): UInt32; override;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    Function GetSHAKE128: TSHAKE128; virtual;
    procedure Initialize; override;
  public
    class Function SHAKE128ToLE(SHAKE128: TSHAKE128): TSHAKE128; virtual;
    class Function SHAKE128ToBE(SHAKE128: TSHAKE128): TSHAKE128; virtual;
    class Function SHAKE128FromLE(SHAKE128: TSHAKE128): TSHAKE128; virtual;
    class Function SHAKE128FromBE(SHAKE128: TSHAKE128): TSHAKE128; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak); overload; override;
    constructor CreateAndInitFrom(Hash: TSHAKE128); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHAKE128); overload; virtual;
    property HashBits: UInt32 read fHashBits write SetHashBits;
    property SHAKE128: TSHAKE128 read GetSHAKE128;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                  TSHAKE256Hash                                 
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHAKE256Hash - class declaration
===============================================================================}
type
  TSHAKE256Hash = class(TSHAKEHash)
  protected
    fSHAKE256:  TSHAKE256;
    procedure SetHashBits(Value: UInt32); override;
    class Function CapacityFromHashBits(HashBits: UInt32): UInt32; override;
    Function GetHashBuffer: TKeccakVar; override;
    procedure SetHashBuffer(HashBuffer: TKeccakVar); override;
    Function GetSHAKE256: TSHAKE256; virtual;
    procedure Initialize; override;
  public
    class Function SHAKE256ToLE(SHAKE256: TSHAKE256): TSHAKE256; virtual;
    class Function SHAKE256ToBE(SHAKE256: TSHAKE256): TSHAKE256; virtual;
    class Function SHAKE256FromLE(SHAKE256: TSHAKE256): TSHAKE256; virtual;
    class Function SHAKE256FromBE(SHAKE256: TSHAKE256): TSHAKE256; virtual;
    class Function HashName: String; override;
    class Function HashFunction: TKeccakFunction; override;
    constructor CreateAndInitFrom(Hash: THashBase); overload; override;
    constructor CreateAndInitFrom(Hash: TKeccak); overload; override;
    constructor CreateAndInitFrom(Hash: TSHAKE256); overload; virtual;
    procedure Init; override;
    procedure FromStringDef(const Str: String; const Default: TSHAKE256); overload; virtual;
    property HashBits: UInt32 read fHashBits write SetHashBits;
    property SHAKE256: TSHAKE256 read GetSHAKE256;
  end;

{===============================================================================
    Auxiliary functions
===============================================================================}

Function ClassByFunction(HashFunction: TKeccakFunction): TKeccakHashClass;

Function CreateByFunction(HashFunction: TKeccakFunction): TKeccakHash;{$IFDEF CanInline} inline; {$ENDIF}

Function CreateFromByFunction(HashFunction: TKeccakFunction; Hash: TKeccakHash): TKeccakHash; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function CreateFromByFunction(HashFunction: TKeccakFunction; Hash: TKeccak): TKeccakHash; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function CreateFromByFunction(Keccak: TKeccak): TKeccakHash; overload;{$IFDEF CanInline} inline; {$ENDIF}

{===============================================================================
    Backward compatibility functions
===============================================================================}
{
  For Keccak/SHA3/SHAKE, it is not enough to pass hash from previous step when
  doing continuous hashing (BufferSHA3 > LastBufferSHA3). TKecakState type is
  introduced for this purpose.
}
type
  TKeccakState = record
    HashFunction: TKeccakFunction;
    HashBits:     UInt32;
    Sponge:       TKeccakSponge;
  end;
  PKeccakState = ^TKeccakState;

  TSHA3State = TKeccakState;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetBlockSize(HashFunction: TSHA3Function): UInt32;

Function InitialSHA3State(HashFunction: TSHA3Function; HashBits: UInt32 = 0): TSHA3State;

Function SHA3ToStr(SHA3: TSHA3): String;
Function StrToSHA3(HashFunction: TSHA3Function; Str: String): TSHA3;
Function TryStrToSHA3(HashFunction: TSHA3Function; const Str: String; out SHA3: TSHA3): Boolean;
Function StrToSHA3Def(HashFunction: TSHA3Function; const Str: String; Default: TSHA3): TSHA3;

Function CompareSHA3(A,B: TSHA3): Integer;
Function SameSHA3(A,B: TSHA3): Boolean;

Function BinaryCorrectSHA3(Hash: TSHA3): TSHA3;{$IFDEF CanInline} inline; {$ENDIF}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure BufferSHA3(var State: TSHA3State; const Buffer; Size: TMemSize); overload;
Function LastBufferSHA3(State: TSHA3State; const Buffer; Size: TMemSize): TSHA3;

Function BufferSHA3(HashFunction: TSHA3Function; const Buffer; Size: TMemSize; HashBits: UInt32 = 0): TSHA3; overload;

Function AnsiStringSHA3(HashFunction: TSHA3Function; const Str: AnsiString; HashBits: UInt32 = 0): TSHA3;
Function WideStringSHA3(HashFunction: TSHA3Function; const Str: WideString; HashBits: UInt32 = 0): TSHA3;
Function StringSHA3(HashFunction: TSHA3Function; const Str: String; HashBits: UInt32 = 0): TSHA3;

Function StreamSHA3(HashFunction: TSHA3Function; Stream: TStream; Count: Int64 = -1; HashBits: UInt32 = 0): TSHA3;
Function FileSHA3(HashFunction: TSHA3Function; const FileName: String; HashBits: UInt32 = 0): TSHA3;

//------------------------------------------------------------------------------

type
  TSHA3Context = type Pointer;

Function SHA3_Init(HashFunction: TSHA3Function; HashBits: UInt32 = 0): TSHA3Context;
procedure SHA3_Update(Context: TSHA3Context; const Buffer; Size: TMemSize);
Function SHA3_Final(var Context: TSHA3Context; const Buffer; Size: TMemSize): TSHA3; overload;
Function SHA3_Final(var Context: TSHA3Context): TSHA3; overload;
Function SHA3_Hash(HashFunction: TSHA3Function; const Buffer; Size: TMemSize; HashBits: UInt32 = 0): TSHA3;

implementation

uses
  SysUtils,
  BitOps;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W4056:={$WARN 4056 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
  {$DEFINE W6018:={$WARN 6018 OFF}} // unreachable code
{$ENDIF}

{===============================================================================
    Auxiliary functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Auxiliary functions - private functions
-------------------------------------------------------------------------------}

Function EndianSwap(Sponge: TKeccakSponge): TKeccakSponge; overload;
var
  i:  Integer;
begin
For i := Low(TKeccakSpongeWordOverlay) to High(TKeccakSpongeWordOverlay) do
  TKeccakSpongeWordOverlay(Result)[i] := EndianSwap(TKeccakSpongeWordOverlay(Sponge)[i]);
end;

//------------------------------------------------------------------------------
{
  BCF = Backward Compatibility Functions, yup ;-)
}
Function BCF_CreateByFunction(HashFunction: TKeccakFunction): TKeccakDefHash;
begin
If HashFunction <> fnKeccak0 then
  Result := TKeccakDefHash(CreateByFunction(HashFunction))
else
  raise ESHA3InvalidHashFunction.Create('BCF_CreateByFunction: Keccak0 not allowed here.');
end;

//------------------------------------------------------------------------------

Function BCF_CreateFromByFunction(HashFunction: TKeccakFunction; Hash: TKeccak): TKeccakDefHash; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
If HashFunction <> fnKeccak0 then
  Result := TKeccakDefHash(CreateFromByFunction(HashFunction,Hash))
else
  raise ESHA3InvalidHashFunction.Create('BCF_CreateFromByFunction: Keccak0 not allowed here.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BCF_CreateFromByFunction(Keccak: TKeccak): TKeccakDefHash; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := BCF_CreateFromByFunction(Keccak.HashFunction,Keccak);
end;

{-------------------------------------------------------------------------------
    Auxiliary functions - public functions
-------------------------------------------------------------------------------}

Function ClassByFunction(HashFunction: TKeccakFunction): TKeccakHashClass;
begin
case HashFunction of
  fnKeccak0:    Result := TKeccak0Hash;
  fnKeccak224:  Result := TKeccak224Hash;
  fnKeccak256:  Result := TKeccak256Hash;
  fnKeccak384:  Result := TKeccak384Hash;
  fnKeccak512:  Result := TKeccak512Hash;
  fnKeccakC:    Result := TKeccakCHash;
  fnSHA3_224:   Result := TSHA3_224Hash;
  fnSHA3_256:   Result := TSHA3_256Hash;
  fnSHA3_384:   Result := TSHA3_384Hash;
  fnSHA3_512:   Result := TSHA3_512Hash;
  fnSHAKE128:   Result := TSHAKE128Hash;
  fnSHAKE256:   Result := TSHAKE256Hash;
else
  raise ESHA3InvalidHashFunction.CreateFmt('ClassByFunction: Invalid hash function (%d)',[Ord(HashFunction)]);
end;
end;

//------------------------------------------------------------------------------

Function CreateByFunction(HashFunction: TKeccakFunction): TKeccakHash;
begin
Result := ClassByFunction(HashFunction).Create;
end;

//------------------------------------------------------------------------------

Function CreateFromByFunction(HashFunction: TKeccakFunction; Hash: TKeccakHash): TKeccakHash;
begin
Result := ClassByFunction(HashFunction).CreateAndInitFrom(Hash);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CreateFromByFunction(HashFunction: TKeccakFunction; Hash: TKeccak): TKeccakHash;
begin
Result := ClassByFunction(HashFunction).CreateAndInitFrom(Hash);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function CreateFromByFunction(Keccak: TKeccak): TKeccakHash;
begin
Result := CreateFromByFunction(Keccak.HashFunction,Keccak);
end;

{-------------------------------------------------------------------------------
================================================================================
                                   TKeccakHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakHash - calculation constants
===============================================================================}

const
  KECCAK_ROUND_CONSTS: array[0..23] of UInt64 = (
    UInt64($0000000000000001), UInt64($0000000000008082), UInt64($800000000000808A),
    UInt64($8000000080008000), UInt64($000000000000808B), UInt64($0000000080000001),
    UInt64($8000000080008081), UInt64($8000000000008009), UInt64($000000000000008A),
    UInt64($0000000000000088), UInt64($0000000080008009), UInt64($000000008000000A),
    UInt64($000000008000808B), UInt64($800000000000008B), UInt64($8000000000008089),
    UInt64($8000000000008003), UInt64($8000000000008002), UInt64($8000000000000080),
    UInt64($000000000000800A), UInt64($800000008000000A), UInt64($8000000080008081),
    UInt64($8000000000008080), UInt64($0000000080000001), UInt64($8000000080008008));

  KECCAK_ROT_COEFS: array[0..4,0..4] of UInt8 = ( // first index is X, second Y
    {X = 0} ( 0,36, 3,41,18),
    {X = 1} ( 1,44,10,45, 2),
    {X = 2} (62, 6,43,15,61),
    {X = 3} (28,55,25,21,56),
    {X = 4} (27,20,39, 8,14));

  KECCAK_DEFAULT_CAPACITY = 9 * 8 * SizeOf(TKeccakWord);  // 576 bits

{===============================================================================
    TKeccakHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TKeccakHash - protected methods
-------------------------------------------------------------------------------}

Function TKeccakHash.GetBitrate: UInt32;
begin
Result := PermutationWidth - fCapacity;
end;

//------------------------------------------------------------------------------

Function TKeccakHash.GetHashBuffer: TKeccakVar;
begin
SetLength(Result,0);
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If Length(HashBuffer) <> 0 then
  raise ESHA3IncompatibleSize.CreateFmt('TKeccakHash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;

//------------------------------------------------------------------------------

class Function TKeccakHash.HashBufferToLE(HashBuffer: TKeccakVar): TKeccakVar;
begin
Result := HashBuffer;
end;

//------------------------------------------------------------------------------

class Function TKeccakHash.HashBufferToBE(HashBuffer: TKeccakVar): TKeccakVar;
begin
Result := HashBuffer;
end;
 
//------------------------------------------------------------------------------

class Function TKeccakHash.HashBufferFromLE(HashBuffer: TKeccakVar): TKeccakVar;
begin
Result := HashBuffer;
end;
 
//------------------------------------------------------------------------------

class Function TKeccakHash.HashBufferFromBE(HashBuffer: TKeccakVar): TKeccakVar;
begin
Result := HashBuffer;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
class Function TKeccakHash.CapacityFromHashBits(HashBits: UInt32): UInt32;
begin
Result := KECCAK_DEFAULT_CAPACITY;
end;
{$IFDEF FPCDWM}{$POP}W5024{$ENDIF}

//------------------------------------------------------------------------------

class Function TKeccakHash.PaddingByte: UInt8;
begin
Result := $01;  // keccak padding (pad10*1)
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.Permute;
var
  i:    Integer;
  B:    TKeccakSponge;
  C,D:  array[0..4] of TKeccakWord;
begin
// 24 rounds (12 + 2L; where L = log2(64) = 6; 64 is length of sponge word in bits)
For i := 0 to 23 do
  begin
    C[0] := fSponge[0,0] xor fSponge[1,0] xor fSponge[2,0] xor fSponge[3,0] xor fSponge[4,0];
    C[1] := fSponge[0,1] xor fSponge[1,1] xor fSponge[2,1] xor fSponge[3,1] xor fSponge[4,1];
    C[2] := fSponge[0,2] xor fSponge[1,2] xor fSponge[2,2] xor fSponge[3,2] xor fSponge[4,2];
    C[3] := fSponge[0,3] xor fSponge[1,3] xor fSponge[2,3] xor fSponge[3,3] xor fSponge[4,3];
    C[4] := fSponge[0,4] xor fSponge[1,4] xor fSponge[2,4] xor fSponge[3,4] xor fSponge[4,4];

    D[0] := C[4] xor ROL(C[1],1);
    D[1] := C[0] xor ROL(C[2],1);
    D[2] := C[1] xor ROL(C[3],1);
    D[3] := C[2] xor ROL(C[4],1);
    D[4] := C[3] xor ROL(C[0],1);

    fSponge[0,0] := fSponge[0,0] xor D[0];
    fSponge[0,1] := fSponge[0,1] xor D[1];
    fSponge[0,2] := fSponge[0,2] xor D[2];
    fSponge[0,3] := fSponge[0,3] xor D[3];
    fSponge[0,4] := fSponge[0,4] xor D[4];
    fSponge[1,0] := fSponge[1,0] xor D[0];
    fSponge[1,1] := fSponge[1,1] xor D[1];
    fSponge[1,2] := fSponge[1,2] xor D[2];
    fSponge[1,3] := fSponge[1,3] xor D[3];
    fSponge[1,4] := fSponge[1,4] xor D[4];
    fSponge[2,0] := fSponge[2,0] xor D[0];
    fSponge[2,1] := fSponge[2,1] xor D[1];
    fSponge[2,2] := fSponge[2,2] xor D[2];
    fSponge[2,3] := fSponge[2,3] xor D[3];
    fSponge[2,4] := fSponge[2,4] xor D[4];
    fSponge[3,0] := fSponge[3,0] xor D[0];
    fSponge[3,1] := fSponge[3,1] xor D[1];
    fSponge[3,2] := fSponge[3,2] xor D[2];
    fSponge[3,3] := fSponge[3,3] xor D[3];
    fSponge[3,4] := fSponge[3,4] xor D[4];
    fSponge[4,0] := fSponge[4,0] xor D[0];
    fSponge[4,1] := fSponge[4,1] xor D[1];
    fSponge[4,2] := fSponge[4,2] xor D[2];
    fSponge[4,3] := fSponge[4,3] xor D[3];
    fSponge[4,4] := fSponge[4,4] xor D[4];

    B[0,0] := ROL(fSponge[0,0],KECCAK_ROT_COEFS[0,0]);
    B[2,0] := ROL(fSponge[0,1],KECCAK_ROT_COEFS[1,0]);
    B[4,0] := ROL(fSponge[0,2],KECCAK_ROT_COEFS[2,0]);
    B[1,0] := ROL(fSponge[0,3],KECCAK_ROT_COEFS[3,0]);
    B[3,0] := ROL(fSponge[0,4],KECCAK_ROT_COEFS[4,0]);
    B[3,1] := ROL(fSponge[1,0],KECCAK_ROT_COEFS[0,1]);
    B[0,1] := ROL(fSponge[1,1],KECCAK_ROT_COEFS[1,1]);
    B[2,1] := ROL(fSponge[1,2],KECCAK_ROT_COEFS[2,1]);
    B[4,1] := ROL(fSponge[1,3],KECCAK_ROT_COEFS[3,1]);
    B[1,1] := ROL(fSponge[1,4],KECCAK_ROT_COEFS[4,1]);
    B[1,2] := ROL(fSponge[2,0],KECCAK_ROT_COEFS[0,2]);
    B[3,2] := ROL(fSponge[2,1],KECCAK_ROT_COEFS[1,2]);
    B[0,2] := ROL(fSponge[2,2],KECCAK_ROT_COEFS[2,2]);
    B[2,2] := ROL(fSponge[2,3],KECCAK_ROT_COEFS[3,2]);
    B[4,2] := ROL(fSponge[2,4],KECCAK_ROT_COEFS[4,2]);
    B[4,3] := ROL(fSponge[3,0],KECCAK_ROT_COEFS[0,3]);
    B[1,3] := ROL(fSponge[3,1],KECCAK_ROT_COEFS[1,3]);
    B[3,3] := ROL(fSponge[3,2],KECCAK_ROT_COEFS[2,3]);
    B[0,3] := ROL(fSponge[3,3],KECCAK_ROT_COEFS[3,3]);
    B[2,3] := ROL(fSponge[3,4],KECCAK_ROT_COEFS[4,3]);
    B[2,4] := ROL(fSponge[4,0],KECCAK_ROT_COEFS[0,4]);
    B[4,4] := ROL(fSponge[4,1],KECCAK_ROT_COEFS[1,4]);
    B[1,4] := ROL(fSponge[4,2],KECCAK_ROT_COEFS[2,4]);
    B[3,4] := ROL(fSponge[4,3],KECCAK_ROT_COEFS[3,4]);
    B[0,4] := ROL(fSponge[4,4],KECCAK_ROT_COEFS[4,4]);

    fSponge[0,0] := B[0,0] xor ((not B[0,1]) and B[0,2]);
    fSponge[0,1] := B[0,1] xor ((not B[0,2]) and B[0,3]);
    fSponge[0,2] := B[0,2] xor ((not B[0,3]) and B[0,4]);
    fSponge[0,3] := B[0,3] xor ((not B[0,4]) and B[0,0]);
    fSponge[0,4] := B[0,4] xor ((not B[0,0]) and B[0,1]);
    fSponge[1,0] := B[1,0] xor ((not B[1,1]) and B[1,2]);
    fSponge[1,1] := B[1,1] xor ((not B[1,2]) and B[1,3]);
    fSponge[1,2] := B[1,2] xor ((not B[1,3]) and B[1,4]);
    fSponge[1,3] := B[1,3] xor ((not B[1,4]) and B[1,0]);
    fSponge[1,4] := B[1,4] xor ((not B[1,0]) and B[1,1]);
    fSponge[2,0] := B[2,0] xor ((not B[2,1]) and B[2,2]);
    fSponge[2,1] := B[2,1] xor ((not B[2,2]) and B[2,3]);
    fSponge[2,2] := B[2,2] xor ((not B[2,3]) and B[2,4]);
    fSponge[2,3] := B[2,3] xor ((not B[2,4]) and B[2,0]);
    fSponge[2,4] := B[2,4] xor ((not B[2,0]) and B[2,1]);
    fSponge[3,0] := B[3,0] xor ((not B[3,1]) and B[3,2]);
    fSponge[3,1] := B[3,1] xor ((not B[3,2]) and B[3,3]);
    fSponge[3,2] := B[3,2] xor ((not B[3,3]) and B[3,4]);
    fSponge[3,3] := B[3,3] xor ((not B[3,4]) and B[3,0]);
    fSponge[3,4] := B[3,4] xor ((not B[3,0]) and B[3,1]);
    fSponge[4,0] := B[4,0] xor ((not B[4,1]) and B[4,2]);
    fSponge[4,1] := B[4,1] xor ((not B[4,2]) and B[4,3]);
    fSponge[4,2] := B[4,2] xor ((not B[4,3]) and B[4,4]);
    fSponge[4,3] := B[4,3] xor ((not B[4,4]) and B[4,0]);
    fSponge[4,4] := B[4,4] xor ((not B[4,0]) and B[4,1]);

    fSponge[0,0] := fSponge[0,0] xor KECCAK_ROUND_CONSTS[i];
  end;
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.Squeeze;
var
  Temp: TKeccakVar;
begin
If HashSize > 0 then
  begin
    SetLength(Temp,HashSize);
    SqueezeTo(Temp[0],HashSize);
    SetHashBuffer(Temp);
  end;
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.SqueezeTo(var Buffer; Size: TMemSize);
var
  Offset: PtrUInt;

  procedure SqueezeSponge(var Dest; Count: TMemSize);
  {$IFDEF ENDIAN_BIG}
  var
    Temp: TKeccakSponge;
    i:    Integer;
  {$ENDIF}
  begin
  {$IFDEF ENDIAN_BIG}
    Temp := EndianSwap(fSponge);
    Move(Temp,Dest,Count);
  {$ELSE}
    Move(fSponge,Dest,Count);
  {$ENDIF}
  end;

  {$IFDEF FPCDWM}{$PUSH}W6018{$ENDIF}
  Function Min(A,B: TMemSize): TMemSize;
  begin
  {$IFDEF CPU64bit}
    If not AuxTypes.NativeUInt64 then
      begin
        If Int64Rec(A).Hi < Int64Rec(B).Hi then
          Result := A
        else If Int64Rec(A).Hi > Int64Rec(B).Hi then
          Result := B
        else
          begin
            If Int64Rec(A).Lo < Int64Rec(B).Lo then
              Result := A
            else
              Result := B;
          end;
      end
    else
      begin
        If A < B then Result := A
          else Result := B;
      end
  {$ELSE}
    If A < B then Result := A
      else Result := B;
  {$ENDIF}
  end;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}

begin
If Size > 0 then
  begin
    Offset := 0;
    If Size > fBlockSize then
      while Size > 0 do
        begin
        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
          SqueezeSponge(Pointer(PtrUInt(@Buffer) + Offset)^,Min(Size,fBlockSize));
        {$IFDEF FPCDWM}{$POP}{$ENDIF}
          Inc(Offset,Min(Size,fBlockSize));
          Dec(Size,Min(Size,fBlockSize));
          Permute;
        end
    else SqueezeSponge(Buffer,Size);
  end;
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.ProcessFirst(const Block);
begin
inherited;
ProcessBlock(Block);
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.ProcessBlock(const Block);
var
  WBuff:      TKeccakSpongeWordOverlay absolute Block;
  BBuff:      TKeccakSpongeByteOverlay absolute Block;
  i:          Integer;
{$IFDEF ENDIAN_BIG}
  TempSponge: TKeccakSponge;
{$ENDIF}
begin
If (fBlockSize mod SizeOf(TKeccakWord)) = 0 then
  begin
    For i := 0 to Pred(fBlockSize div SizeOf(TKeccakWord)) do
      TKeccakSpongeWordOverlay(fSponge)[i] := TKeccakSpongeWordOverlay(fSponge)[i] xor
    {$IFDEF ENDIAN_BIG}EndianSwap{$ENDIF}(WBuff[i]);
  end
else  
  begin
  {$IFDEF ENDIAN_BIG}
    TempSponge := EndianSwap(fSponge);
    For i := 0 to Pred(fBlockSize) do
      TKeccakSpongeByteOverlay(TempSponge)[i] :=
        TKeccakSpongeByteOverlay(TempSponge)[i] xor BBuff[i];
    fSponge := EndianSwap(TempSponge);
  {$ELSE}
    For i := 0 to Pred(fBlockSize) do
      TKeccakSpongeByteOverlay(fSponge)[i] :=
        TKeccakSpongeByteOverlay(fSponge)[i] xor BBuff[i];
  {$ENDIF}
  end;
Permute;
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.ProcessLast;
begin
If fTransCount < fBlockSize then
  begin
    // padding can fit
  {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
    FillChar(Pointer(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^,fBlockSize - fTransCount,0);
    PUInt8(PtrUInt(fTransBlock) + PtrUInt(fTransCount))^ := PaddingByte;
    PUInt8(PtrUInt(fTransBlock) - 1 + PtrUInt(fBlockSize))^ :=
      PUInt8(PtrUInt(fTransBlock) - 1 + PtrUInt(fBlockSize))^ or $80;
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    ProcessBlock(fTransBlock^);
    Squeeze;
  end
else
  begin
    // padding cannot fit
    If fTransCount = fBlockSize then
      begin
        ProcessBlock(fTransBlock^);
      {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
        FillChar(fTransBlock^,fBlockSize,0);
        PUInt8(fTransBlock)^ := PaddingByte;
        PUInt8(PtrUInt(fTransBlock) - 1 + PtrUInt(fBlockSize))^ := $80;
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        ProcessBlock(fTransBlock^);
        Squeeze;
      end
    else raise ESHA3ProcessingError.CreateFmt('TKeccakHash.ProcessLast: Invalid data transfer (%d).',[fTransCount]);
  end;
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.InitHashBits(Value: UInt32);
begin
If (Value mod 8) = 0 then
  begin
    fHashBits := Value;
    fCapacity := CapacityFromHashBits(fHashBits);
    fBlockSize := Bitrate div 8;
  end
else raise ESHA3InvalidHashBits.CreateFmt('TKeccakHash.InitHashBits: Invalid hash bits (%d).',[Value]);
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.Initialize;
begin
inherited;
FillChar(fSponge,SizeOf(TKeccakSponge),0);
end;

{-------------------------------------------------------------------------------
    TKeccakHash - public methods
-------------------------------------------------------------------------------}

class Function TKeccakHash.LaneSize: UInt32;
begin
Result := SizeOf(TKeccakWord) * 8;  // 64, with of the keccak word
end;

//------------------------------------------------------------------------------

class Function TKeccakHash.PermutationWidth: UInt32;
begin
Result := 25 * LaneSize;  // 1600, size of the sponge in bits
end;

//------------------------------------------------------------------------------

Function TKeccakHash.HashSize: TMemSize;
begin
Result := fHashBits div 8;
end;

//------------------------------------------------------------------------------

class Function TKeccakHash.HashEndianness: THashEndianness;
begin
Result := heBig;
end;

//------------------------------------------------------------------------------

class Function TKeccakHash.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

constructor TKeccakHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TKeccakHash then
  begin
    fSponge := TKeccakHash(Hash).Sponge;
    fHashBits := TKeccakHash(Hash).HashBits;
    fCapacity := TKeccakHash(Hash).Capacity;
  end
else
  raise ESHA3IncompatibleClass.CreateFmt('TKeccakHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.Init;
begin
inherited;
FillChar(fSponge,SizeOf(TKeccakSponge),0);
end;

//------------------------------------------------------------------------------

Function TKeccakHash.Compare(Hash: THashBase): Integer;
var
  A,B:  TKeccakVar;
  i:    Integer;
begin
If Hash is Self.ClassType then
  begin
    Result := 0;
    A := GetHashBuffer;
    B := TKeccakHash(Hash).GetHashBuffer; // calling protected method, but meh...
    If Length(A) = Length(B) then
      begin
        For i := Low(A) to High(A) do
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
    else raise ESHA3IncompatibleSize.CreateFmt('TKeccakHash.Compare: Incompatible size (%d,%d).',[Length(A),Length(B)]);
  end
else raise ESHA3IncompatibleClass.CreateFmt('TKeccakHash.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TKeccakHash.AsString: String;
var
  Temp: TKeccakVar;
  i:    Integer;
begin
Temp := GetHashBuffer;
If Length(Temp) > 0 then
  begin
    Result := StringOfChar('0',Length(Temp) * 2);
    For i := Low(Temp) to High(Temp) do
      begin
        Result[(i * 2) + 2] := IntToHex(Temp[i] and $0F,1)[1];
        Result[(i * 2) + 1] := IntToHex(Temp[i] shr 4,1)[1];
      end;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.FromString(const Str: String);
var
  Temp: TKeccakVar;
  i:    Integer;  
begin
SetLength(Temp,Length(Str) div 2);
For i := Low(Temp) to High(Temp) do
  Temp[i] := UInt8(StrToInt('$' + Copy(Str,(i * 2) + 1,2)));
SetHashBuffer(Temp);
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.FromStringDef(const Str: String; const Default: TKeccak);
begin
If (Default.HashFunction = HashFunction) then
  inherited FromStringDef(Str,Default)
else
  raise ESHA3IncompatibleFunction.CreateFmt('TKeccakHash.FromStringDef: Incompatible function (%d).',[Ord(Default.HashFunction)]);
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TKeccakVar;
begin
case Endianness of
  heSystem: Temp := {$IFDEF ENDIAN_BIG}HashBufferToBE{$ELSE}HashBufferToLE{$ENDIF}(GetHashBuffer);
  heLittle: Temp := HashBufferToLE(GetHashBuffer);
  heBig:    Temp := HashBufferToBE(GetHashBuffer);
else
 {heDefault}
  Temp := GetHashBuffer;
end;
If Length(Temp) > 0 then
  Stream.WriteBuffer(Temp[0],Length(Temp));
end;

//------------------------------------------------------------------------------

procedure TKeccakHash.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TKeccakVar;
begin
SetLength(Temp,HashSize);
If Length(Temp) > 0 then
  begin
    Stream.ReadBuffer(Temp[0],Length(Temp));
    case Endianness of
      heSystem: SetHashBuffer({$IFDEF ENDIAN_BIG}HashBufferFromBE{$ELSE}HashBufferFromLE{$ENDIF}(Temp));
      heLittle: SetHashBuffer(HashBufferFromLE(Temp));
      heBig:    SetHashBuffer(HashBufferFromBE(Temp));
    else
     {heDefault}
      SetHashBuffer(Temp);
    end;
  end;
end;

{-------------------------------------------------------------------------------
================================================================================
                                  TKeccak0Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak0Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TKeccak0Hash - protected methods
-------------------------------------------------------------------------------}

procedure TKeccak0Hash.Initialize;
begin
InitHashBits(0);
inherited;
end;

{-------------------------------------------------------------------------------
    TKeccak0Hash - public methods
-------------------------------------------------------------------------------}

class Function TKeccak0Hash.HashName: String;
begin
Result := 'Keccak[]';
end;

//------------------------------------------------------------------------------

class Function TKeccak0Hash.HashFunction: TKeccakFunction;
begin
Result := fnKeccak0;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
constructor TKeccak0Hash.CreateAndInitFrom(Hash: TKeccak);
begin
CreateAndInit;
// this clas does not have a true hash, drop the Hash parameter
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TKeccak0Hash.Permute;
begin
inherited Permute;
end;

//------------------------------------------------------------------------------

procedure TKeccak0Hash.Squeeze(var Buffer; Size: TMemSize);
begin
SqueezeTo(Buffer,Size);
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TKeccakDefHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakDefHash - class declaration
===============================================================================}

Function TKeccakDefHash.GetKeccak: TKeccak;
begin
Result.HashFunction := HashFunction;
Result.HashBits := fHashBits;
If HashSize > 0 then
  Result.HashData := Copy(GetHashBuffer)
else
  SetLength(Result.HashData,0);
end;

{-------------------------------------------------------------------------------
================================================================================
                                 TKeccakFixHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakFixHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TKeccakFixHash - protected methods
-------------------------------------------------------------------------------}

class Function TKeccakFixHash.CapacityFromHashBits(HashBits: UInt32): UInt32;
begin
If (HashBits > 0) and ((HashBits * 2) < PermutationWidth) then
  Result := HashBits * 2
else
  raise ESHA3InvalidHashBits.CreateFmt('TKeccakFixHash.CapacityFromHashBits: Invalid hash bits (%d).',[HashBits]);
end;

{-------------------------------------------------------------------------------
    TKeccakFixHash - public methods
-------------------------------------------------------------------------------}

procedure TKeccakFixHash.FromString(const Str: String);
var
  TempStr:  String;
begin
If Length(Str) < Integer(HashSize * 2) then
  TempStr := StringOfChar('0',Integer(HashSize * 2) - Length(Str)) + Str
else If Length(Str) > Integer(HashSize * 2) then
  TempStr := Copy(Str,Length(Str) - Pred(Integer(HashSize * 2)),Integer(HashSize * 2))
else
  TempStr := Str;
inherited FromString(TempStr);
end;

//------------------------------------------------------------------------------

procedure TKeccakFixHash.FromStringDef(const Str: String; const Default: TKeccak);
begin
If Default.HashBits = fHashBits then 
  begin
    If UInt32(Length(Default.HashData)) = HashSize then
      begin
        inherited FromStringDef(Str,Default); // also checks the hash function
        If not TryFromString(Str) then
          SetHashBuffer(Default.HashData);
      end
    else raise ESHA3IncompatibleSize.CreateFmt('TKeccakFixHash.FromStringDef: Incompatible size (%d).',[Length(Default.HashData)]);
  end
else raise ESHA3IncompatibleHashBits.CreateFmt('TKeccakFixHash.FromStringDef: Incompatible hash bits (%d).',[Default.HashBits]);
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TKeccak224Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak224Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TKeccak224Hash - protected methods
-------------------------------------------------------------------------------}

Function TKeccak224Hash.GetHashBuffer: TKeccakVar;
begin
SetLength(Result,HashSize);
Move(fKeccak224,Result[0],HashSize);
end;

//------------------------------------------------------------------------------

procedure TKeccak224Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If UInt32(Length(HashBuffer)) = HashSize then
  Move(HashBuffer[0],fKeccak224,HashSize)
else
  raise ESHA3IncompatibleSize.CreateFmt('TKeccak224Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;

//------------------------------------------------------------------------------

procedure TKeccak224Hash.Initialize;
begin
InitHashBits(224);
inherited;
end;

{-------------------------------------------------------------------------------
    TKeccak224Hash - public methods
-------------------------------------------------------------------------------}

class Function TKeccak224Hash.Keccak224ToLE(Keccak224: TKeccak224): TKeccak224;
begin
Result := Keccak224;
end;

//------------------------------------------------------------------------------

class Function TKeccak224Hash.Keccak224ToBE(Keccak224: TKeccak224): TKeccak224;
begin
Result := Keccak224;
end;

//------------------------------------------------------------------------------

class Function TKeccak224Hash.Keccak224FromLE(Keccak224: TKeccak224): TKeccak224;
begin
Result := Keccak224;
end;

//------------------------------------------------------------------------------

class Function TKeccak224Hash.Keccak224FromBE(Keccak224: TKeccak224): TKeccak224;
begin
Result := Keccak224;
end;

//------------------------------------------------------------------------------

class Function TKeccak224Hash.HashName: String;
begin
Result := 'Keccak224';
end;

//------------------------------------------------------------------------------

class Function TKeccak224Hash.HashFunction: TKeccakFunction;
begin
Result := fnKeccak224;
end;

//------------------------------------------------------------------------------

constructor TKeccak224Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TKeccak224Hash then
  fKeccak224 := TKeccak224Hash(Hash).Keccak224
else
  raise ESHA3IncompatibleClass.CreateFmt('TKeccak224Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TKeccak224Hash.CreateAndInitFrom(Hash: TKeccak);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    If Hash.HashBits = fHashBits then
      begin
        If (UInt32(Length(Hash.HashData)) = HashSize) then
          Move(Hash.HashData[0],fKeccak224,HashSize)
        else
          raise ESHA3IncompatibleSize.CreateFmt('TKeccak224Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
      end
    else raise ESHA3IncompatibleHashBits.CreateFmt('TKeccak224Hash.CreateAndInitFrom: Incompatible hash bits (%d).',[Hash.HashBits]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TKeccak224Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;
  
//------------------------------------------------------------------------------

constructor TKeccak224Hash.CreateAndInitFrom(Hash: TKeccak224);
begin
CreateAndInit;
fKeccak224 := Hash;
end;

//------------------------------------------------------------------------------

procedure TKeccak224Hash.Init;
begin
inherited;
FillChar(fKeccak224,SizeOf(TKeccak224),0);
end;

//------------------------------------------------------------------------------

procedure TKeccak224Hash.FromStringDef(const Str: String; const Default: TKeccak224);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fKeccak224 := Default;
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TKeccak256Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak256Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TKeccak256Hash - protected methods
-------------------------------------------------------------------------------}

Function TKeccak256Hash.GetHashBuffer: TKeccakVar;
begin
SetLength(Result,HashSize);
Move(fKeccak256,Result[0],HashSize);
end;

//------------------------------------------------------------------------------

procedure TKeccak256Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If UInt32(Length(HashBuffer)) = HashSize then
  Move(HashBuffer[0],fKeccak256,HashSize)
else
  raise ESHA3IncompatibleSize.CreateFmt('TKeccak256Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;

//------------------------------------------------------------------------------

procedure TKeccak256Hash.Initialize;
begin
InitHashBits(256);
inherited;
end;

{-------------------------------------------------------------------------------
    TKeccak256Hash - public methods
-------------------------------------------------------------------------------}

class Function TKeccak256Hash.Keccak256ToLE(Keccak256: TKeccak256): TKeccak256;
begin
Result := Keccak256;
end;

//------------------------------------------------------------------------------

class Function TKeccak256Hash.Keccak256ToBE(Keccak256: TKeccak256): TKeccak256;
begin
Result := Keccak256;
end;

//------------------------------------------------------------------------------

class Function TKeccak256Hash.Keccak256FromLE(Keccak256: TKeccak256): TKeccak256;
begin
Result := Keccak256;
end;

//------------------------------------------------------------------------------

class Function TKeccak256Hash.Keccak256FromBE(Keccak256: TKeccak256): TKeccak256;
begin
Result := Keccak256;
end;

//------------------------------------------------------------------------------

class Function TKeccak256Hash.HashName: String;
begin
Result := 'Keccak256';
end;

//------------------------------------------------------------------------------

class Function TKeccak256Hash.HashFunction: TKeccakFunction;
begin
Result := fnKeccak256;
end;

//------------------------------------------------------------------------------

constructor TKeccak256Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TKeccak256Hash then
  fKeccak256 := TKeccak256Hash(Hash).Keccak256
else
  raise ESHA3IncompatibleClass.CreateFmt('TKeccak256Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TKeccak256Hash.CreateAndInitFrom(Hash: TKeccak);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    If Hash.HashBits = fHashBits then
      begin
        If (UInt32(Length(Hash.HashData)) = HashSize) then
          Move(Hash.HashData[0],fKeccak256,HashSize)
        else
          raise ESHA3IncompatibleSize.CreateFmt('TKeccak256Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
      end
    else raise ESHA3IncompatibleHashBits.CreateFmt('TKeccak256Hash.CreateAndInitFrom: Incompatible hash bits (%d).',[Hash.HashBits]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TKeccak256Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;
  
//------------------------------------------------------------------------------

constructor TKeccak256Hash.CreateAndInitFrom(Hash: TKeccak256);
begin
CreateAndInit;
fKeccak256 := Hash;
end;

//------------------------------------------------------------------------------

procedure TKeccak256Hash.Init;
begin
inherited;
FillChar(fKeccak256,SizeOf(TKeccak256),0);
end;

//------------------------------------------------------------------------------

procedure TKeccak256Hash.FromStringDef(const Str: String; const Default: TKeccak256);
begin
If not TryFromString(Str) then
  fKeccak256 := Default;
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TKeccak384Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak384Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TKeccak384Hash - protected methods
-------------------------------------------------------------------------------}

Function TKeccak384Hash.GetHashBuffer: TKeccakVar;
begin
SetLength(Result,HashSize);
Move(fKeccak384,Result[0],HashSize);
end;

//------------------------------------------------------------------------------

procedure TKeccak384Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If UInt32(Length(HashBuffer)) = HashSize then
  Move(HashBuffer[0],fKeccak384,HashSize)
else
  raise ESHA3IncompatibleSize.CreateFmt('TKeccak384Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;

//------------------------------------------------------------------------------

procedure TKeccak384Hash.Initialize;
begin
InitHashBits(384);
inherited;
end;

{-------------------------------------------------------------------------------
    TKeccak384Hash - public methods
-------------------------------------------------------------------------------}

class Function TKeccak384Hash.Keccak384ToLE(Keccak384: TKeccak384): TKeccak384;
begin
Result := Keccak384;
end;

//------------------------------------------------------------------------------

class Function TKeccak384Hash.Keccak384ToBE(Keccak384: TKeccak384): TKeccak384;
begin
Result := Keccak384;
end;

//------------------------------------------------------------------------------

class Function TKeccak384Hash.Keccak384FromLE(Keccak384: TKeccak384): TKeccak384;
begin
Result := Keccak384;
end;

//------------------------------------------------------------------------------

class Function TKeccak384Hash.Keccak384FromBE(Keccak384: TKeccak384): TKeccak384;
begin
Result := Keccak384;
end;

//------------------------------------------------------------------------------

class Function TKeccak384Hash.HashName: String;
begin
Result := 'Keccak384';
end;

//------------------------------------------------------------------------------

class Function TKeccak384Hash.HashFunction: TKeccakFunction;
begin
Result := fnKeccak384;
end;

//------------------------------------------------------------------------------

constructor TKeccak384Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TKeccak384Hash then
  fKeccak384 := TKeccak384Hash(Hash).Keccak384
else
  raise ESHA3IncompatibleClass.CreateFmt('TKeccak384Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TKeccak384Hash.CreateAndInitFrom(Hash: TKeccak);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    If Hash.HashBits = fHashBits then
      begin
        If (UInt32(Length(Hash.HashData)) = HashSize) then
          Move(Hash.HashData[0],fKeccak384,HashSize)
        else
          raise ESHA3IncompatibleSize.CreateFmt('TKeccak384Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
      end
    else raise ESHA3IncompatibleHashBits.CreateFmt('TKeccak384Hash.CreateAndInitFrom: Incompatible hash bits (%d).',[Hash.HashBits]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TKeccak384Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;
  
//------------------------------------------------------------------------------

constructor TKeccak384Hash.CreateAndInitFrom(Hash: TKeccak384);
begin
CreateAndInit;
fKeccak384 := Hash;
end;

//------------------------------------------------------------------------------

procedure TKeccak384Hash.Init;
begin
inherited;
FillChar(fKeccak384,SizeOf(TKeccak384),0);
end;

//------------------------------------------------------------------------------

procedure TKeccak384Hash.FromStringDef(const Str: String; const Default: TKeccak384);
begin
If not TryFromString(Str) then
  fKeccak384 := Default;
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TKeccak512Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccak512Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TKeccak512Hash - protected methods
-------------------------------------------------------------------------------}

Function TKeccak512Hash.GetHashBuffer: TKeccakVar;
begin
SetLength(Result,HashSize);
Move(fKeccak512,Result[0],HashSize);
end;

//------------------------------------------------------------------------------

procedure TKeccak512Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If UInt32(Length(HashBuffer)) = HashSize then
  Move(HashBuffer[0],fKeccak512,HashSize)
else
  raise ESHA3IncompatibleSize.CreateFmt('TKeccak512Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;

//------------------------------------------------------------------------------

procedure TKeccak512Hash.Initialize;
begin
InitHashBits(512);
inherited;
end;

{-------------------------------------------------------------------------------
    TKeccak512Hash - public methods
-------------------------------------------------------------------------------}

class Function TKeccak512Hash.Keccak512ToLE(Keccak512: TKeccak512): TKeccak512;
begin
Result := Keccak512;
end;

//------------------------------------------------------------------------------

class Function TKeccak512Hash.Keccak512ToBE(Keccak512: TKeccak512): TKeccak512;
begin
Result := Keccak512;
end;

//------------------------------------------------------------------------------

class Function TKeccak512Hash.Keccak512FromLE(Keccak512: TKeccak512): TKeccak512;
begin
Result := Keccak512;
end;

//------------------------------------------------------------------------------

class Function TKeccak512Hash.Keccak512FromBE(Keccak512: TKeccak512): TKeccak512;
begin
Result := Keccak512;
end;

//------------------------------------------------------------------------------

class Function TKeccak512Hash.HashName: String;
begin
Result := 'Keccak512';
end;

//------------------------------------------------------------------------------

class Function TKeccak512Hash.HashFunction: TKeccakFunction;
begin
Result := fnKeccak512;
end;

//------------------------------------------------------------------------------

constructor TKeccak512Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TKeccak512Hash then
  fKeccak512 := TKeccak512Hash(Hash).Keccak512
else
  raise ESHA3IncompatibleClass.CreateFmt('TKeccak512Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TKeccak512Hash.CreateAndInitFrom(Hash: TKeccak);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    If Hash.HashBits = fHashBits then
      begin
        If (UInt32(Length(Hash.HashData)) = HashSize) then
          Move(Hash.HashData[0],fKeccak512,HashSize)
        else
          raise ESHA3IncompatibleSize.CreateFmt('TKeccak512Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
      end
    else raise ESHA3IncompatibleHashBits.CreateFmt('TKeccak512Hash.CreateAndInitFrom: Incompatible hash bits (%d).',[Hash.HashBits]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TKeccak512Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;
  
//------------------------------------------------------------------------------

constructor TKeccak512Hash.CreateAndInitFrom(Hash: TKeccak512);
begin
CreateAndInit;
fKeccak512 := Hash;
end;

//------------------------------------------------------------------------------

procedure TKeccak512Hash.Init;
begin
inherited;
FillChar(fKeccak512,SizeOf(TKeccak512),0);
end;

//------------------------------------------------------------------------------

procedure TKeccak512Hash.FromStringDef(const Str: String; const Default: TKeccak512);
begin
If not TryFromString(Str) then
  fKeccak512 := Default;
end;


{-------------------------------------------------------------------------------
================================================================================
                                    TSHA3Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3Hash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA3Hash - protected methods
-------------------------------------------------------------------------------}

class Function TSHA3Hash.CapacityFromHashBits(HashBits: UInt32): UInt32;
begin
If (HashBits > 0) and ((HashBits * 2) < PermutationWidth) then
  Result := HashBits * 2
else
  raise ESHA3InvalidHashBits.CreateFmt('TSHA3Hash.CapacityFromHashBits: Invalid hash bits (%d).',[HashBits]);
end;
   
//------------------------------------------------------------------------------
 
class Function TSHA3Hash.PaddingByte: UInt8;
begin
Result := $06;  // SHA3 padding (M || 01 || pad10*1)
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TSHA3_224Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3_224Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA3_224Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA3_224Hash.GetHashBuffer: TKeccakVar;
begin
SetLength(Result,HashSize);
Move(fSHA3_224,Result[0],HashSize);
end;
   
//------------------------------------------------------------------------------

procedure TSHA3_224Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If UInt32(Length(HashBuffer)) = HashSize then
  Move(HashBuffer[0],fSHA3_224,HashSize)
else
  raise ESHA3IncompatibleSize.CreateFmt('TSHA3_224Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;
   
//------------------------------------------------------------------------------

procedure TSHA3_224Hash.Initialize;
begin
InitHashBits(224);
inherited;
end;

{-------------------------------------------------------------------------------
    TSHA3_224Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA3_224Hash.SHA3_224ToLE(SHA3_224: TSHA3_224): TSHA3_224;
begin
Result := SHA3_224;
end;
    
//------------------------------------------------------------------------------

class Function TSHA3_224Hash.SHA3_224ToBE(SHA3_224: TSHA3_224): TSHA3_224;
begin
Result := SHA3_224;
end;
    
//------------------------------------------------------------------------------

class Function TSHA3_224Hash.SHA3_224FromLE(SHA3_224: TSHA3_224): TSHA3_224;
begin
Result := SHA3_224;
end;
      
//------------------------------------------------------------------------------

class Function TSHA3_224Hash.SHA3_224FromBE(SHA3_224: TSHA3_224): TSHA3_224;
begin
Result := SHA3_224;
end;
     
//------------------------------------------------------------------------------

class Function TSHA3_224Hash.HashName: String;
begin
Result := 'SHA3-224';
end;
     
//------------------------------------------------------------------------------

class Function TSHA3_224Hash.HashFunction: TKeccakFunction;
begin
Result := fnSHA3_224;
end;
     
//------------------------------------------------------------------------------

constructor TSHA3_224Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA3_224Hash then
  fSHA3_224 := TSHA3_224Hash(Hash).SHA3_224
else
  raise ESHA3IncompatibleClass.CreateFmt('TSHA3_224Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;
   
//------------------------------------------------------------------------------

constructor TSHA3_224Hash.CreateAndInitFrom(Hash: TSHA3);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    If Hash.HashBits = fHashBits then
      begin
        If (UInt32(Length(Hash.HashData)) = HashSize) then
          Move(Hash.HashData[0],fSHA3_224,HashSize)
        else
          raise ESHA3IncompatibleSize.CreateFmt('TSHA3_224Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
      end
    else raise ESHA3IncompatibleHashBits.CreateFmt('TSHA3_224Hash.CreateAndInitFrom: Incompatible hash bits (%d).',[Hash.HashBits]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TSHA3_224Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA3_224Hash.CreateAndInitFrom(Hash: TSHA3_224);
begin
CreateAndInit;
fSHA3_224 := Hash;
end;

//------------------------------------------------------------------------------

procedure TSHA3_224Hash.Init;
begin
inherited;
FillChar(fSHA3_224,SizeOf(TSHA3_224),0);
end;

//------------------------------------------------------------------------------

procedure TSHA3_224Hash.FromStringDef(const Str: String; const Default: TSHA3_224);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSHA3_224 := Default;
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TSHA3_256Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3_256Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA3_256Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA3_256Hash.GetHashBuffer: TKeccakVar;
begin
SetLength(Result,HashSize);
Move(fSHA3_256,Result[0],HashSize);
end;
   
//------------------------------------------------------------------------------

procedure TSHA3_256Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If UInt32(Length(HashBuffer)) = HashSize then
  Move(HashBuffer[0],fSHA3_256,HashSize)
else
  raise ESHA3IncompatibleSize.CreateFmt('TSHA3_256Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;
   
//------------------------------------------------------------------------------

procedure TSHA3_256Hash.Initialize;
begin
InitHashBits(256);
inherited;
end;

{-------------------------------------------------------------------------------
    TSHA3_256Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA3_256Hash.SHA3_256ToLE(SHA3_256: TSHA3_256): TSHA3_256;
begin
Result := SHA3_256;
end;
    
//------------------------------------------------------------------------------

class Function TSHA3_256Hash.SHA3_256ToBE(SHA3_256: TSHA3_256): TSHA3_256;
begin
Result := SHA3_256;
end;
    
//------------------------------------------------------------------------------

class Function TSHA3_256Hash.SHA3_256FromLE(SHA3_256: TSHA3_256): TSHA3_256;
begin
Result := SHA3_256;
end;
      
//------------------------------------------------------------------------------

class Function TSHA3_256Hash.SHA3_256FromBE(SHA3_256: TSHA3_256): TSHA3_256;
begin
Result := SHA3_256;
end;
     
//------------------------------------------------------------------------------

class Function TSHA3_256Hash.HashName: String;
begin
Result := 'SHA3-256';
end;
     
//------------------------------------------------------------------------------

class Function TSHA3_256Hash.HashFunction: TKeccakFunction;
begin
Result := fnSHA3_256;
end;
     
//------------------------------------------------------------------------------

constructor TSHA3_256Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA3_256Hash then
  fSHA3_256 := TSHA3_256Hash(Hash).SHA3_256
else
  raise ESHA3IncompatibleClass.CreateFmt('TSHA3_256Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;
   
//------------------------------------------------------------------------------

constructor TSHA3_256Hash.CreateAndInitFrom(Hash: TSHA3);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    If Hash.HashBits = fHashBits then
      begin
        If (UInt32(Length(Hash.HashData)) = HashSize) then
          Move(Hash.HashData[0],fSHA3_256,HashSize)
        else
          raise ESHA3IncompatibleSize.CreateFmt('TSHA3_256Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
      end
    else raise ESHA3IncompatibleHashBits.CreateFmt('TSHA3_256Hash.CreateAndInitFrom: Incompatible hash bits (%d).',[Hash.HashBits]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TSHA3_256Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA3_256Hash.CreateAndInitFrom(Hash: TSHA3_256);
begin
CreateAndInit;
fSHA3_256 := Hash;
end;

//------------------------------------------------------------------------------

procedure TSHA3_256Hash.Init;
begin
inherited;
FillChar(fSHA3_256,SizeOf(TSHA3_256),0);
end;

//------------------------------------------------------------------------------

procedure TSHA3_256Hash.FromStringDef(const Str: String; const Default: TSHA3_256);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSHA3_256 := Default;
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TSHA3_384Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3_384Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA3_384Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA3_384Hash.GetHashBuffer: TKeccakVar;
begin
SetLength(Result,HashSize);
Move(fSHA3_384,Result[0],HashSize);
end;
   
//------------------------------------------------------------------------------

procedure TSHA3_384Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If UInt32(Length(HashBuffer)) = HashSize then
  Move(HashBuffer[0],fSHA3_384,HashSize)
else
  raise ESHA3IncompatibleSize.CreateFmt('TSHA3_384Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;
   
//------------------------------------------------------------------------------

procedure TSHA3_384Hash.Initialize;
begin
InitHashBits(384);
inherited;
end;

{-------------------------------------------------------------------------------
    TSHA3_384Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA3_384Hash.SHA3_384ToLE(SHA3_384: TSHA3_384): TSHA3_384;
begin
Result := SHA3_384;
end;
    
//------------------------------------------------------------------------------

class Function TSHA3_384Hash.SHA3_384ToBE(SHA3_384: TSHA3_384): TSHA3_384;
begin
Result := SHA3_384;
end;
    
//------------------------------------------------------------------------------

class Function TSHA3_384Hash.SHA3_384FromLE(SHA3_384: TSHA3_384): TSHA3_384;
begin
Result := SHA3_384;
end;
      
//------------------------------------------------------------------------------

class Function TSHA3_384Hash.SHA3_384FromBE(SHA3_384: TSHA3_384): TSHA3_384;
begin
Result := SHA3_384;
end;
     
//------------------------------------------------------------------------------

class Function TSHA3_384Hash.HashName: String;
begin
Result := 'SHA3-384';
end;
     
//------------------------------------------------------------------------------

class Function TSHA3_384Hash.HashFunction: TKeccakFunction;
begin
Result := fnSHA3_384;
end;
     
//------------------------------------------------------------------------------

constructor TSHA3_384Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA3_384Hash then
  fSHA3_384 := TSHA3_384Hash(Hash).SHA3_384
else
  raise ESHA3IncompatibleClass.CreateFmt('TSHA3_384Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;
   
//------------------------------------------------------------------------------

constructor TSHA3_384Hash.CreateAndInitFrom(Hash: TSHA3);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    If Hash.HashBits = fHashBits then
      begin
        If (UInt32(Length(Hash.HashData)) = HashSize) then
          Move(Hash.HashData[0],fSHA3_384,HashSize)
        else
          raise ESHA3IncompatibleSize.CreateFmt('TSHA3_384Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
      end
    else raise ESHA3IncompatibleHashBits.CreateFmt('TSHA3_384Hash.CreateAndInitFrom: Incompatible hash bits (%d).',[Hash.HashBits]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TSHA3_384Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA3_384Hash.CreateAndInitFrom(Hash: TSHA3_384);
begin
CreateAndInit;
fSHA3_384 := Hash;
end;

//------------------------------------------------------------------------------

procedure TSHA3_384Hash.Init;
begin
inherited;
FillChar(fSHA3_384,SizeOf(TSHA3_384),0);
end;

//------------------------------------------------------------------------------

procedure TSHA3_384Hash.FromStringDef(const Str: String; const Default: TSHA3_384);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSHA3_384 := Default;
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TSHA3_512Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHA3_512Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHA3_512Hash - protected methods
-------------------------------------------------------------------------------}

Function TSHA3_512Hash.GetHashBuffer: TKeccakVar;
begin
SetLength(Result,HashSize);
Move(fSHA3_512,Result[0],HashSize);
end;
   
//------------------------------------------------------------------------------

procedure TSHA3_512Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If UInt32(Length(HashBuffer)) = HashSize then
  Move(HashBuffer[0],fSHA3_512,HashSize)
else
  raise ESHA3IncompatibleSize.CreateFmt('TSHA3_512Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;
   
//------------------------------------------------------------------------------

procedure TSHA3_512Hash.Initialize;
begin
InitHashBits(512);
inherited;
end;

{-------------------------------------------------------------------------------
    TSHA3_512Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHA3_512Hash.SHA3_512ToLE(SHA3_512: TSHA3_512): TSHA3_512;
begin
Result := SHA3_512;
end;
    
//------------------------------------------------------------------------------

class Function TSHA3_512Hash.SHA3_512ToBE(SHA3_512: TSHA3_512): TSHA3_512;
begin
Result := SHA3_512;
end;
    
//------------------------------------------------------------------------------

class Function TSHA3_512Hash.SHA3_512FromLE(SHA3_512: TSHA3_512): TSHA3_512;
begin
Result := SHA3_512;
end;
      
//------------------------------------------------------------------------------

class Function TSHA3_512Hash.SHA3_512FromBE(SHA3_512: TSHA3_512): TSHA3_512;
begin
Result := SHA3_512;
end;
     
//------------------------------------------------------------------------------

class Function TSHA3_512Hash.HashName: String;
begin
Result := 'SHA3-512';
end;
     
//------------------------------------------------------------------------------

class Function TSHA3_512Hash.HashFunction: TKeccakFunction;
begin
Result := fnSHA3_512;
end;
     
//------------------------------------------------------------------------------

constructor TSHA3_512Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHA3_512Hash then
  fSHA3_512 := TSHA3_512Hash(Hash).SHA3_512
else
  raise ESHA3IncompatibleClass.CreateFmt('TSHA3_512Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;
   
//------------------------------------------------------------------------------

constructor TSHA3_512Hash.CreateAndInitFrom(Hash: TSHA3);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    If Hash.HashBits = fHashBits then
      begin
        If (UInt32(Length(Hash.HashData)) = HashSize) then
          Move(Hash.HashData[0],fSHA3_512,HashSize)
        else
          raise ESHA3IncompatibleSize.CreateFmt('TSHA3_512Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
      end
    else raise ESHA3IncompatibleHashBits.CreateFmt('TSHA3_512Hash.CreateAndInitFrom: Incompatible hash bits (%d).',[Hash.HashBits]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TSHA3_512Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHA3_512Hash.CreateAndInitFrom(Hash: TSHA3_512);
begin
CreateAndInit;
fSHA3_512 := Hash;
end;

//------------------------------------------------------------------------------

procedure TSHA3_512Hash.Init;
begin
inherited;
FillChar(fSHA3_512,SizeOf(TSHA3_512),0);
end;

//------------------------------------------------------------------------------

procedure TSHA3_512Hash.FromStringDef(const Str: String; const Default: TSHA3_512);
begin
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  fSHA3_512 := Default;
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TKeccakVarHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakVarHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TKeccakVarHash - protected methods
-------------------------------------------------------------------------------}

class Function TKeccakVarHash.CapacityFromHashBits(HashBits: UInt32): UInt32;
begin
If HashBits > 0 then
  Result := KECCAK_DEFAULT_CAPACITY
else
  raise ESHA3InvalidHashBits.CreateFmt('TKeccakVarHash.CapacityFromHashBits: Invalid hash bits (%d).',[HashBits]);
end;

{-------------------------------------------------------------------------------
    TKeccakVarHash - public methods
-------------------------------------------------------------------------------}

procedure TKeccakVarHash.FromStringDef(const Str: String; const Default: TKeccak);
begin
If (Length(Str) div 2) = Length(Default.HashData) then
  begin
    inherited FromStringDef(Str,Default);
    If not TryFromString(Str) then
      SetHashBuffer(Default.HashData);
  end
else raise ESHA3InvalidSize.CreateFmt('TKeccakVarHash.FromStringDef: Size mismatch (%d, %d).',[Length(Str) div 2,Length(Default.HashData)]);
end;

{-------------------------------------------------------------------------------
================================================================================
                                  TKeccakCHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TKeccakCHash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TKeccakCHash - protected methods
-------------------------------------------------------------------------------}

procedure TKeccakCHash.SetHashBits(Value: UInt32);
begin
If ((Value mod 8) = 0) and (Value > 0) then
  begin
    fHashBits := Value;
    SetLength(fKeccakC,HashSize);
    FillChar(fKeccakC[0],Length(fKeccakC),0);
  end
else raise ESHA3InvalidHashBits.CreateFmt('TKeccakCHash.SetHashBits: Invalid hash bits (%d).',[Value]);
end;

//------------------------------------------------------------------------------

procedure TKeccakCHash.SetCapacity(Value: UInt32);
begin
If ((Value mod 8) = 0) and (Value > 0) and (Value < PermutationWidth) then
  begin
    fCapacity := Value;
    fBlockSize := Bitrate div 8;
    ReallocMem(fTransBlock,fBlockSize);
  end
else raise ESHA3InvalidCapacity.CreateFmt('TKeccakCHash.SetCapacity: Invalid capacity (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TKeccakCHash.GetHashBuffer: TKeccakVar;
begin
Result := Copy(TKeccakVar(fKeccakC));
end;

//------------------------------------------------------------------------------

procedure TKeccakCHash.SetHashBuffer(HashBuffer: TKeccakVar);
begin                                                  
If (Length(HashBuffer) > 0) then
  begin
    SetHashBits(Length(HashBuffer) * 8);
    fKeccakC := Copy(TKeccakC(HashBuffer));
  end
else raise ESHA3IncompatibleSize.CreateFmt('TKeccakCHash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;

//------------------------------------------------------------------------------

Function TKeccakCHash.GetKeccakC: TKeccakC;
begin
Result := Copy(fKeccakC);
end;

//------------------------------------------------------------------------------

procedure TKeccakCHash.Initialize;
begin
InitHashBits(KECCAK_DEFAULT_CAPACITY);  // capacity is also set to default value
inherited;
SetLength(fKeccakC,HashSize);
end;

{-------------------------------------------------------------------------------
    TKeccakCHash - public methods
-------------------------------------------------------------------------------}

class Function TKeccakCHash.KeccakCToLE(KeccakC: TKeccakC): TKeccakC;
begin
Result := Copy(KeccakC);
end;
  
//------------------------------------------------------------------------------

class Function TKeccakCHash.KeccakCToBE(KeccakC: TKeccakC): TKeccakC;
begin
Result := Copy(KeccakC);
end;
  
//------------------------------------------------------------------------------

class Function TKeccakCHash.KeccakCFromLE(KeccakC: TKeccakC): TKeccakC;
begin
Result := Copy(KeccakC);
end;
 
//------------------------------------------------------------------------------

class Function TKeccakCHash.KeccakCFromBE(KeccakC: TKeccakC): TKeccakC;
begin
Result := Copy(KeccakC);
end;

//------------------------------------------------------------------------------

class Function TKeccakCHash.HashName: String;
begin
Result := 'Keccak[c]'
end;

//------------------------------------------------------------------------------

class Function TKeccakCHash.HashFunction: TKeccakFunction;
begin
Result := fnKeccakC;
end;

//------------------------------------------------------------------------------

class Function TKeccakCHash.MaxCapacity: UInt32;
begin
Result := PermutationWidth - 8;
end;
 
//------------------------------------------------------------------------------

constructor TKeccakCHash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TKeccakCHash then
  begin
    SetHashBits(TKeccakCHash(Hash).HashBits);
    SetCapacity(TKeccakCHash(Hash).Capacity);
    fKeccakC := TKeccakCHash(Hash).KeccakC;   // no need to call copy
  end
else raise ESHA3IncompatibleClass.CreateFmt('TKeccakCHash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;
 
//------------------------------------------------------------------------------

constructor TKeccakCHash.CreateAndInitFrom(Hash: TKeccak);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    SetHashBits(Hash.HashBits); // capacity is left untouched
    If (UInt32(Length(Hash.HashData)) = HashSize) then
      fKeccakC := Copy(TKeccakC(Hash.HashData))
    else
      raise ESHA3IncompatibleSize.CreateFmt('TKeccakCHash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TKeccakCHash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;
  
//------------------------------------------------------------------------------

constructor TKeccakCHash.CreateAndInitFrom(Hash: TKeccakC);
begin
CreateAndInit;
SetHashBuffer(TKeccakVar(Hash));  // also sets bits and others
end;
   
//------------------------------------------------------------------------------

procedure TKeccakCHash.Init;
begin
inherited;
If Length(fKeccakC) > 0 then
  FillChar(fKeccakC[0],Length(fKeccakC),0);
end;

//------------------------------------------------------------------------------

procedure TKeccakCHash.FromStringDef(const Str: String; const Default: TKeccakC);
begin
If (Length(Str) div 2) = Length(Default) then
  begin
    If not TryFromString(Str) then
      SetHashBuffer(TKeccakVar(Default));
  end
else raise ESHA3InvalidSize.CreateFmt('TKeccakCHash.FromStringDef: Size mismatch (%d, %d).',[Length(Str) div 2,Length(Default)]);
end;


{-------------------------------------------------------------------------------
================================================================================
                                   TSHAKEHash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHAKEHash - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TSHAKEHash - protected methods
-------------------------------------------------------------------------------}

class Function TSHAKEHash.PaddingByte: UInt8;
begin
Result := $1F;  // RawSHAKE + SHAKE padding (M || 11 || 11 || pad10*1)
end;


{-------------------------------------------------------------------------------
================================================================================
                                  TSHAKE128Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHAKE128Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHAKE128Hash - protected methods
-------------------------------------------------------------------------------}

procedure TSHAKE128Hash.SetHashBits(Value: UInt32);
begin
InitHashBits(Value);
// capacity does not change, no need to reallocate temp block
SetLength(fSHAKE128,HashSize);
FillChar(fSHAKE128[0],Length(fSHAKE128),0);
end;
//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
class Function TSHAKE128Hash.CapacityFromHashBits(HashBits: UInt32): UInt32;
begin
Result := 256;  // capacity is static
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TSHAKE128Hash.GetHashBuffer: TKeccakVar;
begin
Result := Copy(TKeccakVar(fSHAKE128));
end;

//------------------------------------------------------------------------------

procedure TSHAKE128Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If (Length(HashBuffer) > 0) then
  begin
    SetHashBits(Length(HashBuffer) * 8);
    fSHAKE128 := Copy(TSHAKE128(HashBuffer));
  end
else raise ESHA3IncompatibleSize.CreateFmt('TSHAKE128Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;

//------------------------------------------------------------------------------

Function TSHAKE128Hash.GetSHAKE128: TSHAKE128;
begin
Result := Copy(fSHAKE128);
end;

//------------------------------------------------------------------------------

procedure TSHAKE128Hash.Initialize;
begin
InitHashBits(128);
inherited;
SetLength(fSHAKE128,HashSize);
end;

{-------------------------------------------------------------------------------
    TSHAKE128Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHAKE128Hash.SHAKE128ToLE(SHAKE128: TSHAKE128): TSHAKE128;
begin
Result := Copy(SHAKE128);
end;
     
//------------------------------------------------------------------------------

class Function TSHAKE128Hash.SHAKE128ToBE(SHAKE128: TSHAKE128): TSHAKE128;
begin
Result := Copy(SHAKE128);
end;  
     
//------------------------------------------------------------------------------

class Function TSHAKE128Hash.SHAKE128FromLE(SHAKE128: TSHAKE128): TSHAKE128;
begin
Result := Copy(SHAKE128);
end; 
     
//------------------------------------------------------------------------------

class Function TSHAKE128Hash.SHAKE128FromBE(SHAKE128: TSHAKE128): TSHAKE128;
begin
Result := Copy(SHAKE128);
end; 
     
//------------------------------------------------------------------------------

class Function TSHAKE128Hash.HashName: String;
begin
Result := 'SHAKE128';
end;  
     
//------------------------------------------------------------------------------

class Function TSHAKE128Hash.HashFunction: TKeccakFunction;
begin
Result := fnSHAKE128;
end;  
     
//------------------------------------------------------------------------------

constructor TSHAKE128Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHAKE128Hash then
  begin
    SetHashBits(TSHAKE128Hash(Hash).HashBits);
    fSHAKE128 := TSHAKE128Hash(Hash).SHAKE128;
  end
else raise ESHA3IncompatibleClass.CreateFmt('TSHAKE128Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TSHAKE128Hash.CreateAndInitFrom(Hash: TKeccak);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    SetHashBits(Hash.HashBits);
    If (UInt32(Length(Hash.HashData)) = HashSize) then
      fSHAKE128 := Copy(TSHAKE128(Hash.HashData))
    else
      raise ESHA3IncompatibleSize.CreateFmt('TSHAKE128Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TSHAKE128Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHAKE128Hash.CreateAndInitFrom(Hash: TSHAKE128);
begin
CreateAndInit;
SetHashBuffer(TKeccakVar(Hash));
end;

//------------------------------------------------------------------------------

procedure TSHAKE128Hash.Init;
begin
inherited;
If Length(fSHAKE128) > 0 then
  FillChar(fSHAKE128[0],Length(fSHAKE128),0);
end;
     
//------------------------------------------------------------------------------

procedure TSHAKE128Hash.FromStringDef(const Str: String; const Default: TSHAKE128);
begin
If (Length(Str) div 2) = Length(Default) then
  begin
    If not TryFromString(Str) then
      SetHashBuffer(TKeccakVar(Default));
  end
else raise ESHA3InvalidSize.CreateFmt('TSHAKE128Hash.FromStringDef: Size mismatch (%d, %d).',[Length(Str) div 2,Length(Default)]);
end;


{-------------------------------------------------------------------------------
================================================================================
                                  TSHAKE256Hash
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TSHAKE256Hash - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSHAKE256Hash - protected methods
-------------------------------------------------------------------------------}

procedure TSHAKE256Hash.SetHashBits(Value: UInt32);
begin
InitHashBits(Value);
SetLength(fSHAKE256,HashSize);
FillChar(fSHAKE256[0],Length(fSHAKE256),0);
end;
//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
class Function TSHAKE256Hash.CapacityFromHashBits(HashBits: UInt32): UInt32;
begin
Result := 512;  // capacity is static
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TSHAKE256Hash.GetHashBuffer: TKeccakVar;
begin
Result := Copy(TKeccakVar(fSHAKE256));
end;

//------------------------------------------------------------------------------

procedure TSHAKE256Hash.SetHashBuffer(HashBuffer: TKeccakVar);
begin
If (Length(HashBuffer) > 0) then
  begin
    SetHashBits(Length(HashBuffer) * 8);
    fSHAKE256 := Copy(TSHAKE256(HashBuffer));
  end
else raise ESHA3IncompatibleSize.CreateFmt('TSHAKE256Hash.SetHashBuffer: Incompatible size (%d).',[Length(HashBuffer)]);
end;

//------------------------------------------------------------------------------

Function TSHAKE256Hash.GetSHAKE256: TSHAKE256;
begin
Result := Copy(fSHAKE256);
end;

//------------------------------------------------------------------------------

procedure TSHAKE256Hash.Initialize;
begin
InitHashBits(256);
inherited;
SetLength(fSHAKE256,HashSize);
end;

{-------------------------------------------------------------------------------
    TSHAKE256Hash - public methods
-------------------------------------------------------------------------------}

class Function TSHAKE256Hash.SHAKE256ToLE(SHAKE256: TSHAKE256): TSHAKE256;
begin
Result := Copy(SHAKE256);
end;
     
//------------------------------------------------------------------------------

class Function TSHAKE256Hash.SHAKE256ToBE(SHAKE256: TSHAKE256): TSHAKE256;
begin
Result := Copy(SHAKE256);
end;  
     
//------------------------------------------------------------------------------

class Function TSHAKE256Hash.SHAKE256FromLE(SHAKE256: TSHAKE256): TSHAKE256;
begin
Result := Copy(SHAKE256);
end; 
     
//------------------------------------------------------------------------------

class Function TSHAKE256Hash.SHAKE256FromBE(SHAKE256: TSHAKE256): TSHAKE256;
begin
Result := Copy(SHAKE256);
end; 
     
//------------------------------------------------------------------------------

class Function TSHAKE256Hash.HashName: String;
begin
Result := 'SHAKE256';
end;  
     
//------------------------------------------------------------------------------

class Function TSHAKE256Hash.HashFunction: TKeccakFunction;
begin
Result := fnSHAKE256;
end;  
     
//------------------------------------------------------------------------------

constructor TSHAKE256Hash.CreateAndInitFrom(Hash: THashBase);
begin
inherited CreateAndInitFrom(Hash);
If Hash is TSHAKE256Hash then
  begin
    SetHashBits(TSHAKE256Hash(Hash).HashBits);
    fSHAKE256 := TSHAKE256Hash(Hash).SHAKE256;
  end
else raise ESHA3IncompatibleClass.CreateFmt('TSHAKE256Hash.CreateAndInitFrom: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

constructor TSHAKE256Hash.CreateAndInitFrom(Hash: TKeccak);
begin
CreateAndInit;
If Hash.HashFunction = HashFunction then
  begin
    SetHashBits(Hash.HashBits);
    If (UInt32(Length(Hash.HashData)) = HashSize) then
      fSHAKE256 := Copy(TSHAKE256(Hash.HashData))
    else
      raise ESHA3IncompatibleSize.CreateFmt('TSHAKE256Hash.CreateAndInitFrom: Incompatible size (%d).',[Length(Hash.HashData)]);
  end
else raise ESHA3IncompatibleFunction.CreateFmt('TSHAKE256Hash.CreateAndInitFrom: Incompatible function (%d).',[Ord(Hash.HashFunction)]);
end;

//------------------------------------------------------------------------------

constructor TSHAKE256Hash.CreateAndInitFrom(Hash: TSHAKE256);
begin
CreateAndInit;
SetHashBuffer(TKeccakVar(Hash));
end;

//------------------------------------------------------------------------------

procedure TSHAKE256Hash.Init;
begin
inherited;
If Length(fSHAKE256) > 0 then
  FillChar(fSHAKE256[0],Length(fSHAKE256),0);
end;
     
//------------------------------------------------------------------------------

procedure TSHAKE256Hash.FromStringDef(const Str: String; const Default: TSHAKE256);
begin
If (Length(Str) div 2) = Length(Default) then
  begin
    If not TryFromString(Str) then
      SetHashBuffer(TKeccakVar(Default));
  end
else raise ESHA3InvalidSize.CreateFmt('TSHAKE256Hash.FromStringDef: Size mismatch (%d, %d).',[Length(Str) div 2,Length(Default)]);
end;


{===============================================================================
    Backward compatibility functions
===============================================================================}
{-------------------------------------------------------------------------------
    Backward compatibility functions - auxiliary functions
-------------------------------------------------------------------------------}

Function GetBlockSize(HashFunction: TSHA3Function): UInt32;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  Result := Hash.BlockSize;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function InitialSHA3State(HashFunction: TSHA3Function; HashBits: UInt32 = 0): TSHA3State;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  // following is here to catch invalid values
  If HashBits <> 0 then
    case HashFunction of
      fnKeccakC:  TKeccakCHash(Hash).HashBits := HashBits;
      fnSHAKE128: TSHAKE128Hash(Hash).HashBits := HashBits;
      fnSHAKE256: TSHAKE256Hash(Hash).HashBits := HashBits;
    end;
  Result.HashFunction := HashFunction;
  Result.HashBits := Hash.HashBits;
  Result.Sponge := Hash.Sponge;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function SHA3ToStr(SHA3: TSHA3): String;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateFromByFunction(SHA3);
try
  Result := Hash.AsString;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToSHA3(HashFunction: TSHA3Function; Str: String): TSHA3;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  Hash.FromString(Str);
  Result := Hash.Keccak;
finally
  Hash.Free;
end; 
end;

//------------------------------------------------------------------------------

Function TryStrToSHA3(HashFunction: TSHA3Function; const Str: String; out SHA3: TSHA3): Boolean;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  Result := Hash.TryFromString(Str);
  If Result then
    SHA3 := Hash.Keccak;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StrToSHA3Def(HashFunction: TSHA3Function; const Str: String; Default: TSHA3): TSHA3;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  Hash.FromStringDef(Str,Default);
  Result := Hash.Keccak;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function CompareSHA3(A,B: TSHA3): Integer;
var
  HashA:  TKeccakDefHash;
  HashB:  TKeccakDefHash;
begin
HashA := BCF_CreateFromByFunction(A);
try
  Result := 0;  // don't ask me, ask Delphi...
  HashB := BCF_CreateFromByFunction(B);
  try
    Result := HashA.Compare(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

//------------------------------------------------------------------------------

Function SameSHA3(A,B: TSHA3): Boolean;
var
  HashA:  TKeccakDefHash;
  HashB:  TKeccakDefHash;
begin
HashA := BCF_CreateFromByFunction(A);
try
  Result := False;
  HashB := BCF_CreateFromByFunction(B);
  try
    Result := HashA.Same(HashB);
  finally
    HashB.Free;
  end;
finally
  HashA.Free;
end;
end;

//------------------------------------------------------------------------------

Function BinaryCorrectSHA3(Hash: TSHA3): TSHA3;
begin
Result := Hash;
SetLength(Result.HashData,Length(Result.HashData)); // this will create unique copy
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - processing functions
-------------------------------------------------------------------------------}

procedure BufferSHA3(var State: TSHA3State; const Buffer; Size: TMemSize);
var
  Hash: TKeccakDefHash;
begin
If Size > 0 then
  begin
    Hash := BCF_CreateByFunction(State.HashFunction);
    try
      If (Size mod Hash.BlockSize) = 0 then
        begin
          Hash.Init;
          If (Hash is TKeccakVarHash) and (State.HashBits <> 0) then
            TKeccakVarHash(Hash).HashBits := State.HashBits;
          Hash.Sponge := State.Sponge;
          Hash.Update(Buffer,Size);
          State.Sponge := Hash.Sponge;
        end
      else raise ESHA3ProcessingError.CreateFmt('BufferSHA3: Buffer size (%d) is not divisible by %d.',[Size,Hash.BlockSize]);
    finally
      Hash.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

Function LastBufferSHA3(State: TSHA3State; const Buffer; Size: TMemSize): TSHA3;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(State.HashFunction);
try
  Hash.Init;
  If (Hash is TKeccakVarHash) and (State.HashBits <> 0) then
    TKeccakVarHash(Hash).HashBits := State.HashBits;
  Hash.Sponge := State.Sponge;
  Hash.Final(Buffer,Size);
  Result := Hash.Keccak;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function BufferSHA3(HashFunction: TSHA3Function; const Buffer; Size: TMemSize; HashBits: UInt32 = 0): TSHA3; overload;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  If (Hash is TKeccakVarHash) and (HashBits <> 0) then
    TKeccakVarHash(Hash).HashBits := HashBits;
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.Keccak;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function AnsiStringSHA3(HashFunction: TSHA3Function; const Str: AnsiString; HashBits: UInt32 = 0): TSHA3;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  If (Hash is TKeccakVarHash) and (HashBits <> 0) then
    TKeccakVarHash(Hash).HashBits := HashBits;
  Hash.HashAnsiString(Str);
  Result := Hash.Keccak;
finally
  Hash.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function WideStringSHA3(HashFunction: TSHA3Function; const Str: WideString; HashBits: UInt32 = 0): TSHA3;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  If (Hash is TKeccakVarHash) and (HashBits <> 0) then
    TKeccakVarHash(Hash).HashBits := HashBits;
  Hash.HashWideString(Str);
  Result := Hash.Keccak;
finally
  Hash.Free;
end;
end;
  
//------------------------------------------------------------------------------

Function StringSHA3(HashFunction: TSHA3Function; const Str: String; HashBits: UInt32 = 0): TSHA3;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  If (Hash is TKeccakVarHash) and (HashBits <> 0) then
    TKeccakVarHash(Hash).HashBits := HashBits;
  Hash.HashString(Str);
  Result := Hash.Keccak;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function StreamSHA3(HashFunction: TSHA3Function; Stream: TStream; Count: Int64 = -1; HashBits: UInt32 = 0): TSHA3;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  If (Hash is TKeccakVarHash) and (HashBits <> 0) then
    TKeccakVarHash(Hash).HashBits := HashBits;
  Hash.HashStream(Stream,Count);
  Result := Hash.Keccak;
finally
  Hash.Free;
end;
end;

//------------------------------------------------------------------------------

Function FileSHA3(HashFunction: TSHA3Function; const FileName: String; HashBits: UInt32 = 0): TSHA3;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  If (Hash is TKeccakVarHash) and (HashBits <> 0) then
    TKeccakVarHash(Hash).HashBits := HashBits;
  Hash.HashFile(FileName);
  Result := Hash.Keccak;
finally
  Hash.Free;
end;
end;

{-------------------------------------------------------------------------------
    Backward compatibility functions - context functions
-------------------------------------------------------------------------------}

Function SHA3_Init(HashFunction: TSHA3Function; HashBits: UInt32 = 0): TSHA3Context;
var
  Temp: TKeccakDefHash;
begin
Temp := BCF_CreateByFunction(HashFunction);
If (Temp is TKeccakVarHash) and (HashBits <> 0) then
  TKeccakVarHash(Temp).HashBits := HashBits;
Temp.Init;
Result := TSHA3Context(Temp);
end;

//------------------------------------------------------------------------------

procedure SHA3_Update(Context: TSHA3Context; const Buffer; Size: TMemSize);
begin
TKeccakDefHash(Context).Update(Buffer,Size);
end;

//------------------------------------------------------------------------------

Function SHA3_Final(var Context: TSHA3Context; const Buffer; Size: TMemSize): TSHA3;
begin
SHA3_Update(Context,Buffer,Size);
Result := SHA3_Final(Context);
end;
 
//------------------------------------------------------------------------------

Function SHA3_Final(var Context: TSHA3Context): TSHA3;
begin
TKeccakDefHash(Context).Final;
Result := TKeccakDefHash(Context).Keccak;
FreeAndNil(TKeccakDefHash(Context));
end;
 
//------------------------------------------------------------------------------

Function SHA3_Hash(HashFunction: TSHA3Function; const Buffer; Size: TMemSize; HashBits: UInt32 = 0): TSHA3;
var
  Hash: TKeccakDefHash;
begin
Hash := BCF_CreateByFunction(HashFunction);
try
  If (Hash is TKeccakVarHash) and (HashBits <> 0) then
    TKeccakVarHash(Hash).HashBits := HashBits;
  Hash.HashBuffer(Buffer,Size);
  Result := Hash.Keccak;
finally
  Hash.Free;
end;
end;

end.

