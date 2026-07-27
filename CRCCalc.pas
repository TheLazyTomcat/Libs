{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  CRC Calculator

    This unit provides means of calculating vitually any cyclic redundancy
    check (CRC) value, irrespective of its specification.

    Using instances of TCRCCalculator and its properties, you can configure
    all standard variables of CRC algorithm - width, polynomial, initial
    value, xor-out value and also in- and out-reflections. Minimum width of
    CRC is 1 (0 is not possible as the polynomial must have at least two
    bits), but there is no explicit maximum (there are technical limitations,
    of course). The width is not limited to whole bytes, so arbitrary widths
    are possible.

    Also, the hashed data are not limited to whole bytes - arbitrary
    bitstreams are supported. The only limiting factor is, that they must
    start at byte boundary, bit offsets are not supported at this moment.

    That is all nice, but...

      WARNING - this library is not intended for normal CRC calculations
                (eg. for use in archives), only for "academic" use, simply
                because it is EXTREMELY slow when compared to specialized
                implementations. Depending on compiler and use case, it
                can be slower by several orders of magnitude (yes, more
                than 100x slower)!
                At this point, no optimizations were attempted, only if
                there will be real demand for it I might consider them.

  Version 1.0 (2026-07-26)

  Last change 2026-07-27

  ©2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.CRCCalc

  Dependencies:
    AuxMath  - github.com/TheLazyTomcat/Lib.AuxMath
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes
    HashBase - github.com/TheLazyTomcat/Lib.HashBase
    StrRect  - github.com/TheLazyTomcat/Lib.StrRect

  Indirect dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxExceptions      - github.com/TheLazyTomcat/Lib.AuxExceptions
    ListUtils          - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    UInt64Utils        - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo        - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit CRCCalc;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
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
  ECRCException = class(EHashException);

  ECRCSizeMismatch      = class(ECRCException);
  ECRCInvalidValue      = class(ECRCException);
  ECRCIncompatibleClass = class(ECRCException);
  ECRCIncompatibleValue = class(ECRCException);

{===============================================================================
    Common types
===============================================================================}
type
{
  TCRCBuffer

  This is used as a general mean of binary data storage (instead of using
  pointer + size combo).

  Format of the stored data is not explicitly prescribed and can be arbitrary.
}
  TCRCBuffer = array of UInt8;
{
  TCRCValue

  This type is used to transfer any binary data, but in particular to return
  resulting CRC value after processing.

  Bits of the value are always stored with little endianness, meaning least
  significant bit is stored first (within lowest byte, in its lowest bit) and
  most significant bit at the end.
  In general, bit n of the value is stored within byte [n div 8], in its bit
  [n mod 8].

  Field Data must never contain empty bytes, ie. bytes that do not contain any
  bit belonging to the stored value.

  State of bits after the last bit is undefined, but is strongly recommended
  to be zero.

  Since the stored value might not completely fill all bytes, number of bits
  is explicitly given in BitCount. Make sure this number corresponds to the
  length of Data array (its length must be equal to Ceil(BitCount / 8)),
  othervise an ECRCInvalidValue exception will be raised the moment such an
  invalid TCRCValue is passed to any function or method.
}
  TCRCValue = record
    Data:     TCRCBuffer;
    BitCount: TMemSize;
  end;
  PCRCValue = ^TCRCValue;

//------------------------------------------------------------------------------  
{
  CRCValueInit

  Initializes variable Value - that is, it sets field BitCount to given value
  and properly allocates internal buffer (field Data).

  If the Value already contains anything, its content is discarded.
}
procedure CRCValueInit(BitCount: TMemSize; out Value: TCRCValue);

{
  CRCValueCompare

  Compares two given values and returns result accordingly - if A is larger,
  then a positive integer is returned, when it is considered smalled than B
  then a negative number is returned. When the two given values are equal,
  then zero is returned.

  If any of the two values is invalid (its BitCount does not correspond to
  size of internal buffer), then an ECRCInvalidValue exception is raised.

  Both values must have the same BitCount, otherwise an ECRCSizeMismatch
  exception is raised.

  The values are compared from highest (last) byte to lowest (first) byte,
  be aware of that. This is to reflect the fact that they are stored with
  little-endianness, meaning the most significant bits (those that should
  have the most influence over the result, and therefore must be compared
  first) are at highest positions.
}
Function CRCValueCompare(const A,B: TCRCValue): Integer;

{
  CRCValueSame

  Compares two given values for equality and returns true when they are
  equal, false otherwise.

  If any of the two values is invalid (its BitCount does not correspond to
  size of internal buffer), then an ECRCInvalidValue exception is raised.

  Both values must have the same BitCount, otherwise an ECRCSizeMismatch
  exception is raised.
}
Function CRCValueSame(const A,B: TCRCValue): Boolean;

{
  CRCValueAsString

  Returns hexadecimal representation of given value.

  The returned string is big-endian (highest-order byte to the left) and
  right-aligned (bits of partial byte are towards right). This corresponds
  to classical string representation of integers (eg. produced by IntToHex).
  Also note that string is only as long as required to represent indicated
  number of bits (BitCount), therefore do no assume its length is a multiple
  of two.

  If provided value is invalid (its BitCount does not correspond to size of
  internal buffer), then an ECRCInvalidValue exception is raised.
}
Function CRCValueAsString(const Value: TCRCValue): String;

{
  CRCValueFromString

  Converts given string to a TCRCValue value.

  Returned value will have BitCount number of bits, unless paramater BitCount
  is set to zero (default value). In that case, result's bit count will be set
  to length of given string multiplied by 4.

  If the string is shorter than required for requested bit count, then it is
  left-padded with zeroes before conversion. If longer, then only its right-
  most part of corresponding length is used.

  The string is parsed from right - this corresponds to how CRCValueAsString
  constructs its result, meaning these two functions are complementary.
}
Function CRCValueFromString(const Str: String; BitCount: TMemSize = 0): TCRCValue;

{
  CRCValueSaveToBuffer

  Copies data from internal buffer (field Data) to provided memory buffer.

  Note that whole bytes are copied, even if they are not integral (not fully
  filled with observed bits).

  If provided value is invalid (its BitCount does not correspond to size of
  internal buffer), then an ECRCInvalidValue exception is raised.

  You can use this function eg. to copy data into an integer variable - but
  beware of endianness (swap endianness after copy on bit endian systems).
}
procedure CRCValueSaveToBuffer(const Value: TCRCValue; out Buffer);

{
  CRCValueLoadFromBuffer

  Copies data from provided memory buffer to internal buffer of given value.

  The value must be initialized before it is used here (you can use function
  CRCValueInit), as its BitCount is used to calculate how many bytes will be
  copied.

  If provided value is invalid (its BitCount does not correspond to size of
  internal buffer), then an ECRCInvalidValue exception is raised.

  Note that the value is masked after copying - meaning. irrespective of what
  was copied, only observed bits (BitCount) will be set, others are zored.

  You can use this function eg. to copy integer variable into the data - but
  beware of endianness (swap endianness after copy on bit endian systems).
}
procedure CRCValueLoadFromBuffer(var Value: TCRCValue; const Buffer);

{===============================================================================
--------------------------------------------------------------------------------
                                 TCRCCalculator
--------------------------------------------------------------------------------
===============================================================================}
type
{
  TCRCValueOptions

  This set (its individual values) is used in certain methods to alter how are
  provided data processed and converted for use in the CRC calculation.

    optHighByteFirst

      By default, provided data are expected to be in little-endian byte order,
      meaning least significant byte is at lowest memory address and the most
      significant byte at highest address.

      By selecting this option, you are declaring that the data are stored in
      big-endian byte order, which means that, when loaded, they will be byte-
      reflected before further processing. This usually happens when the data
      are loaded from byte streams instead of integer variables.

      This order is used in internal buffers.

    optLeftAlignedPartial

      If the provided data contains partial byte (ie. BitCount is not a
      multiple of 8), it is expected that the trailing bits are stored at
      lowest places in the last byte (eg. two trailing bits would be in
      bits 0..1).

      Selecting this option informs the method that the trailing bits are
      stored in highest bits of the last byte (ie. to the left). For example,
      let's have data with 3 trailing bits - then the least significant of
      these three bits is at position 5, second at 6 and the most significant
      bit is at bit position 7 in the last byte.

      This scheme is used in internal buffers.

    optPolyExcludesHighBit

      Declares that the value given as polynomial is without its highest-order
      bit.

      This has the effect that the given value can be one bit smaller without
      raising an exception. The missing bit will be automatically added (it is
      always one).

      CRC polynomials are usually presented with the highest bit omitted. So,
      if you need to use such value, include this option.

      Observed only by functions SetPolynomial(FromString).

    optPolyExcludesLowBit

      Declares that the value given as polynomial is without its lowest-order
      bit.

      Given value can then be one bit smaller without raising an exception.
      The missing bit will be automatically added as it is always one.

      CRC polynomials are usually presented with this bit, but there is one
      particular scheme (Koopman) where it is omitted. Use this option for
      such values.

      Observed only by functions SetPolynomial(FromString).

    optStringReadFromLeft

      Normally, the given string is parsed from right (so two right-most
      characters corresponds to first parsed byte). When it is too short,
      it is left-padded with zeroes and when too long, then only its right-
      most part is used.

      When this option is active, then the string is parsed from left, is
      right-padded when too short and its left part is used when too long.
      This can be used then provided string represents byte stream instead
      of an integral value.

      Strings that should be parsed this way are returned by functions
      InternalBufferAs(Bit)String.

      Observed only by functions accepting strings (eg. SetCRCFromString).
}
  TCRCValueOptions = set of (optHighByteFirst,optLeftAlignedPartial,
    optPolyExcludesHighBit,optPolyExcludesLowBit,optStringReadFromLeft);

{
  TCRCInternalBuffer

  This enumeration is used to select which internal buffer should be provided
  in textual representation by methods InternalBufferAs(Bit)String.

  As these are ment only for testing and debugging, there is no point in
  describing individual values - consult the source code if you are interested.
}
  TCRCInternalBuffer = (bufPolynomial,bufInit,bufXorOut,bufCRC,bufResidue,
    bufProcessing,bufPolyTable0,bufPolyTable1,bufPolyTable2,bufPolyTable3,
    bufPolyTable4,bufPolyTable5,bufPolyTable6,bufPolyTable7);

{
  TCRCBufferLoader

  This type is used only internally, yet must be publicly declared.

  Do not use it. as it can be changed or even removed in the future!
}
type
  TCRCBufferLoader = procedure(Data: Pointer; BitCount: TMemSize; ValueOptions: TCRCValueOptions; out Buffer: TCRCBuffer) of object;

{===============================================================================
    TCRCCalculator - class declaration
===============================================================================}
type
  TCRCCalculator = class(TStreamHash)
  protected
  {
    Internally, data in buffers are stored MSB first - first byte contains
    the most significant bits. Within bytes, the bits are in natural order,
    ie. bits with lower significance at lower places (note that the bytes
    are filled from top, not from bottom).
  }  
    fHashBits:          TMemSize;
    fPolynomial:        TCRCBuffer;
    fInitial:           TCRCBuffer;
    fXorOut:            TCRCBuffer;
    fReflectIn:         Boolean;
    fReflectOut:        Boolean;
    fCRCValue:          TCRCBuffer;
    fResidue:           TCRCBuffer;
    // processing fields
    fPolyTable:         array[0..7] of TCRCBuffer;
    fProcBuffer:        TCRCBuffer;
    fProcBits:          TMemSize;
    fProcThreshold:     TMemSize;
    fProcInitApplied:   Boolean;
    fProcessedOddBits:  TMemSize;
    procedure HashBitsSetter(Value: TMemSize); virtual;
    Function PolynomialGetter: TCRCValue; virtual;
    procedure PolynomialSetter(const Value: TCRCValue); virtual;
    Function InitialGetter: TCRCValue; virtual;
    procedure InitialSetter(const Value: TCRCValue); virtual;
    Function XorOutGetter: TCRCValue; virtual;
    procedure XorOutSetter(const Value: TCRCValue); virtual;
    procedure ReflectInSetter(Value: Boolean); virtual;
    procedure ReflectOutSetter(Value: Boolean); virtual;
    Function CRCGetter: TCRCValue; virtual;
    procedure CRCSetter(const Value: TCRCValue); virtual;
    Function ResidueGetter: TCRCValue; virtual;
    procedure Initialize; override;
    // main processing
    procedure ProcessingRound; virtual;
    procedure ProcessBuffer(const Buffer; Size: TMemSize); override;
    procedure ProcessBufferAligned(const Buffer; Size: TMemSize); virtual;
    procedure ProcessBufferBits(const Buffer; BitCount: TMemSize); virtual;
    // buffers loading
    procedure BufferLoaderMemory(Data: Pointer; BitCount: TMemSize; ValueOptions: TCRCValueOptions; out Buffer: TCRCBuffer); virtual;
    procedure BufferLoaderString(Data: Pointer; BitCount: TMemSize; ValueOptions: TCRCValueOptions; out Buffer: TCRCBuffer); virtual;
    procedure BufferRectifyAfterLoad(var Buffer: TCRCBuffer; BitCount: TMemSize; ValueOptions: TCRCValueOptions); virtual;
    procedure SetPolynomialInternal(Data: Pointer; BitCount: TMemSize; ValueOptions: TCRCValueOptions; BufferLoader: TCRCBufferLoader); virtual;
    procedure SetBufferInternal(var Buffer: TCRCBuffer; Data: Pointer; BitCount: TMemSize; ValueOptions: TCRCValueOptions; BufferLoader: TCRCBufferLoader); virtual;
  public
    class Function HashType: THashType; override;
    class Function HashEndianness: THashEndianness; override;
    class Function HashFinalization: Boolean; override;
    Function HashName: String; reintroduce;
    Function HashSize: TMemSize; reintroduce;
    constructor CreateAndInit{$IFNDEF FPC}(Dummy: Integer = 0){$ENDIF}; override;
  {
    CreateAndInitFrom

    Only overload accepting THashBase argument can be used to continue
    processing started in Source object.

    The other overload gets only resulting CRC, which here is not enough to
    continue processing (internal state is much more complex and, what's more,
    no algorithm settings (eg. Polynomial) are provided).

    If provided CRC value is invalid (its BitCount does not correspond to size
    of internal buffer or is empty (BitCount of zero), then an ECRCInvalidValue
    exception is raised.
  }
    constructor CreateAndInitFrom(Source: THashBase); overload; override;
    constructor CreateAndInitFrom(Source: TCRCValue); overload; virtual;
  {
    CreateAndInitFromString

    The instance is created as CRC of width equal to length of given string
    multiplied by four. If you need to create and init an object while giving
    it number of bits not divisible by 4, use following sequence:

      Instance := TCRCCalculator.Create;
      Instance.SetInitialFromString(str,bitcount);
      Instance.Init;

    Given string must not be empty (length of zero), otherwise an exception
    of class ECRCInvalidValue is raised.

    This constructor cannot be used for continuous processing (it does not
    properly set up the algorithm settings).
  }
    constructor CreateAndInitFromString(const Str: String); override;
  {
    SetPolynomial(FromString)
    SetInitial(FromString)
    SetXorOut(FromString)
    SetCRC(FromString)

    These functions provide an alternative way of setting respective values
    to classical property assignment (which is strict in what it can accept).

    By default, rules in effect for propety assignemnt or, in case of strings,
    for FromString methods apply here too, but selecting certain value options
    can change them - see declaration of type TCRCValueOptions for details
    about individual options.
  }
    procedure SetPolynomial(const Polynomial; BitCount: TMemSize; ValueOptions: TCRCValueOptions = []); overload; virtual;
    procedure SetPolynomial(const Polynomial: TCRCValue; ValueOptions: TCRCValueOptions = []); overload; virtual;
    procedure SetPolynomialFromString(const Str: String; BitCount: TMemSize = 0; ValueOptions: TCRCValueOptions = []); virtual;
    procedure SetInitial(const Initial; BitCount: TMemSize; ValueOptions: TCRCValueOptions = []); overload; virtual;
    procedure SetInitial(const Initial: TCRCValue; ValueOptions: TCRCValueOptions = []); overload; virtual;
    procedure SetInitialFromString(const Str: String; BitCount: TMemSize = 0; ValueOptions: TCRCValueOptions = []); virtual;
    procedure SetXorOut(const XorOut; BitCount: TMemSize; ValueOptions: TCRCValueOptions = []); overload; virtual;
    procedure SetXorOut(const XorOut: TCRCValue; ValueOptions: TCRCValueOptions = []); overload; virtual;
    procedure SetXorOutFromString(const Str: String; BitCount: TMemSize = 0; ValueOptions: TCRCValueOptions = []); virtual;
    procedure SetCRC(const CRC; BitCount: TMemSize; ValueOptions: TCRCValueOptions = []); overload; virtual;
    procedure SetCRC(const CRC: TCRCValue; ValueOptions: TCRCValueOptions = []); overload; virtual;
    procedure SetCRCFromString(const Str: String; BitCount: TMemSize = 0; ValueOptions: TCRCValueOptions = []); virtual;
  {
    Init

    If you call Init on instance that is not properly set up (HashBits is
    zero), then it will initialize it to a fallback settings - HashBits 1
    and all values set to zero (polynomial will be 0x3, because both its
    lowest and highest bits must be set).
  }
    procedure Init; override;
    procedure Final; override;
    procedure UpdateBits(const Buffer; BitCount: TMemSize); virtual;
    procedure FinalBits(const Buffer; BitCount: TMemSize); virtual;
  {
    Compare
    Same

    These two methods can only compare CRCs with the same number of bits. If
    you pass instance with differing HashBits, then an ECRCIncompatibleValue
    exception will be raised.
  }
    Function Compare(Hash: THashBase): Integer; override;
    Function Same(Hash: THashBase): Boolean; override;
  {
    AsString
    FromString
   (TryFromString) 
    FromStringDef

    These methods are producing/expecting big-endian right-aligned hexadecimal
    string representation (the same as CRCValueAsString and CRCValueFromString,
    in fact these functions are internally called) of CRC value.

    If current HashBits is zero when calling any FromString function, then
    HashBits is set to length of provided string multiplied by four.

    If conversion from string fails in FromStringDef and current HashBits is
    zero, then HashBits is set to BitCount of Default value.

    If Default parameter in FromStringDef is not valid, then an ECRCInvalidValue
    exception is raised.    
  }
    Function AsString: String; override;
    procedure FromString(const Str: String); override;
    procedure FromStringDef(const Str: String; const Default: TCRCValue); reintroduce;
  {
    SaveToStream
    LoadFromStream

    Stores and loads CRC value, but only its data, not BitCount/HashBits.
    This means, among others, that you need to first properly set HashBits
    BEFORE loading CRC value.

    Loaded data are masked, so only observed bits are kept.

    Parameter Endianness is ignored, the data are always stored with little
    endianness.
  }
    procedure SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
    procedure LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault); override;
  {
    InternalBufferAsString
    InternalBufferAsBitString

    These functions are here only for testing and debugging - they provide
    direct string representation of internal buffers in specific formats
    (big-endian, left-aligned hexadecimal or MS-Byte and MS-Bit first, left-
    aligned, byte-separated bit string).

    If you want to use string returned by InternalBufferAsString as input,
    you have to use methods Set*FromString with value options containing
    optHighByteFirst, optLeftAlignedPartial and optStringReadFromLeft.
  }
    Function InternalBufferAsString(Buffer: TCRCInternalBuffer): String; virtual;
    Function InternalBufferAsBitString(Buffer: TCRCInternalBuffer): String; virtual;
  {
    HashBits

    This number is the width of calculated CRC value (eg. 32 for CRC-32).
    It must always be set to a value above zero (so 1+) to do calculations,
    but can be explicitly set to zero to clear algorithm settings.

    Setting this property always cancels/aborts pending processing/hashing
    (equivalent to calling inherited method AbortHashing).

    Changing this value (setting it to a different value than it currently has)
    will clear binary data properties (namely Polynomial, Initial, XorOut, CRC
    and Residue) and then allocates them to proper size for a given new width.
  }
    property HashBits: TMemSize read fHashBits write HashBitsSetter;
  {
    Polynomial

    This is the polynomial used in computation. As it is returned and accepted
    as TCRCValue type, the byte and bit order corresponds to that type, see its
    description for more details.

    It is always returned with highest bit included, meaning it will be
    HashBits + 1 wide. Also, when settings it, it must be provided with
    highest bit.

    Using an invalid value when setting it will raise an ECRCInvalidValue
    exception. This exception is also raised when value smaller than two
    bits is used.

    Setting this value always aborts pending processing, clears binary data
    properties and sets HashBits to width of provided polynomial minus one
    (HashBits := Polynomial.BitCount - 1).
    Whatever is assigned to this property, first and last bits are
    automatically set to one - this is a requirement for CRC calculation.
  }
    property Polynomial: TCRCValue read PolynomialGetter write PolynomialSetter;
  {
    Initial
    XorOut
    ReflectIn
    ReflectOut
    CRC
    Residue

    Setting any of these properties, with exception being CRC and Resudie, will
    abort pending processing.

    If current HashBits is zero when setting Initial, XorOut, CRC or Residue,
    then HashBits is set to BitCount of provided value. If HashBits is non-zero
    then provided value must have the same bit count as is hash bits, otherwise
    an ECRCIncompatibleValue exception is raised.

    Using an invalid TCRCValue will raise an ECRCInvalidValue exception.
  }
    property Initial: TCRCValue read InitialGetter write InitialSetter;
    property XorOut: TCRCValue read XorOutGetter write XorOutSetter;
    property ReflectIn: Boolean read fReflectIn write ReflectInSetter;
    property ReflectOut: Boolean read fReflectOut write ReflectOutSetter;
  {
    CRC

    Resulting CRC value after computation.

    For more details see above.
  }
    property CRC: TCRCValue read CRCGetter write CRCSetter;
  {
    Residue

    Contains state of internal processing buffer from the last complete
    computation. This corresponds to CRC value after applying optional
    out-reflection but before applying (xor-ing in) final XorOut.

    For more details see above.
  }
    property Residue: TCRCValue read ResidueGetter;
  {
    ProcessedOddBits

    This property does not contain total number of processed bits, only bits
    that do not form a whole byte. Meaning its value can be in range 0 to 7.
    To get total number of processed bits, use following formula:

        TotalProcessedBits = (ProcessedBytes * 8) + ProcessedOddBits
  }
    property ProcessedOddBits: TMemSize read fProcessedOddBits;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              TCRCCalculatorTester
--------------------------------------------------------------------------------
===============================================================================}
{
  TCRCCalculatorTester

  This class is here just to provide a self-testing capability for actual
  implementation.

  It loads a specially prepared text file (this file should be always provided
  with this unit) that contains specifications for over a hundred known CRC
  variants along with two basic tests for each one (control CRC that should be
  obtained after hashing a specific ansi string and a residue). There are also
  test vectors for some specifications, to allow for more thorough testing.

  Assignable callbacks and/or events are also provided - they will be called
  after competion of each test and provided with information about what test
  was performed and how it ended.
}
//------------------------------------------------------------------------------
{
  CRCCALC_TESTFILE

  Default name of the textual file containing self-test data.
}
const
  CRCCALC_TESTFILE = 'CRCCalcTests.txt';

{
  TCRCTestStage

  Used in test callbacks to indicate what kind of test has just finished.
}
type
  TCRCTestStage = (tstCheck,tstResidue,tstCodeword);

{
  TestStage

  Returns textual representation of given test stage, eg. for debugging or
  logging purposes.
}
Function TestStageAsString(TestStage: TCRCTestStage): String;

{
  TCRCTestEvent
  TCRCTestCallback

  Types used for on-test-done callbacks.

  Argument Result indicates how the test went - it is true if the test
  succeeded, false when it failed.

  Index is valid only for tstCodeword stage, it is undefined for other
  stages. If indicates which codeword (starting at zero) of the given
  preset was tested.
}
type
  TCRCTestEvent    = procedure(Sender: TObject; PresetName: String; Stage: TCRCTestStage; Index: Integer; Result: Boolean) of object;
  TCRCTestCallback = procedure(Sender: TObject; PresetName: String; Stage: TCRCTestStage; Index: Integer; Result: Boolean);

{===============================================================================
    TCRCCalculatorTester - class declaration
===============================================================================}
type
  TCRCCalculatorTester = class(TCRCCalculator)
  protected
    fOnTestEvent:         TCRCTestEvent;
    fOnTestCallback:      TCRCTestCallback;
    fCurrentPresetName:   String;
    fCurrentPresetIndex:  Integer;
    procedure Initialize; override;
    procedure DoOnTest(Stage: TCRCTestStage; Index: Integer; Result: Boolean); virtual;
    Function SelfTestLinePreset(const Line: String; BreakOnFailure: Boolean): Boolean; virtual;
    Function SelfTestLineCodeword(Selector: Char; const Line: String): Boolean; virtual;
    Function SelfTestLineCodewordHex(const Line: String): Boolean; virtual;
    Function SelfTestLineCodewordOct(const Line: String): Boolean; virtual;
    Function SelfTestLineCodewordBit(const Line: String): Boolean; virtual;
    Function SelfTestLineCodewordAuto_0(const Line: String): Boolean; virtual;
    Function SelfTestLineCodewordAuto_1(const Line: String): Boolean; virtual;
    Function SelfTestLineCodewordAuto_2(const Line: String): Boolean; virtual;
    Function SelfTestLineCodewordAuto_3(const Line: String): Boolean; virtual;
    Function SelfTestLineCodewordAuto_4(const Line: String): Boolean; virtual;
    Function SelfTestLineCodewordAuto_5(const Line: String): Boolean; virtual;
  public
  {
    SelfTest

    Runs self-testing suite.

    First overload allows you to specify a file containing test specification,
    second overload uses default name (constant CRCCALC_TESTFILE) and current
    directory to load the file (if not present, then expect standard exception
    to be raised).

    If BreakOnFailure is set to true (default value), then the function exits
    after first failed test. If false, then it continues until it finishes all
    tests, irrespective of how many of them fails.

    Returns true if all tests succeeded, false when at least one (potentially
    more) failed. 
  }
    Function SelfTest(const FileName: String; BreakOnFailure: Boolean = True): Boolean; overload; virtual;
    Function SelfTest(BreakOnFailure: Boolean = True): Boolean; overload; virtual;
  {
    OnTest*

    Called when a test is completed. If both event and callback are assigned,
    then only the event is called.
  }
    property OnTest: TCRCTestEvent read fOnTestEvent write fOnTestEvent;
    property OnTestEvent: TCRCTestEvent read fOnTestEvent write fOnTestEvent;
    property OnTestCallback: TCRCTestCallback read fOnTestCallback write fOnTestCallback;
  end;

implementation

uses
  SysUtils,
  AuxMath, StrRect;

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ELSE}
  {$UNDEF OverflowChecks}
{$ENDIF}

{===============================================================================
    Internal utility functions
===============================================================================}

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}
Function PtrAdvance(Address: Pointer; Offset: TMemOff): Pointer;
var
  IntAddress: PtrInt absolute Address;
  IntResult:  PtrInt absolute Result;
begin
IntResult := IntAddress + PtrInt(Offset);
end;
{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

//------------------------------------------------------------------------------

Function ConsumeArg(const Arg: Integer): Integer;
begin
Result := Arg;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Auxiliary processing                              
--------------------------------------------------------------------------------
===============================================================================}
{
  These functions are not directly part of CRC computation, but they provide
  necessary infrastructure so it can be implemented as simly and user-friendly
  as possible.
}
//------------------------------------------------------------------------------
const
  CRC_BYTE_REFLECTED: array[UInt8] of UInt8 = (
    $00, $80, $40, $C0, $20, $A0, $60, $E0, $10, $90, $50, $D0, $30, $B0, $70, $F0,
    $08, $88, $48, $C8, $28, $A8, $68, $E8, $18, $98, $58, $D8, $38, $B8, $78, $F8,
    $04, $84, $44, $C4, $24, $A4, $64, $E4, $14, $94, $54, $D4, $34, $B4, $74, $F4,
    $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
    $02, $82, $42, $C2, $22, $A2, $62, $E2, $12, $92, $52, $D2, $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA, $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
    $06, $86, $46, $C6, $26, $A6, $66, $E6, $16, $96, $56, $D6, $36, $B6, $76, $F6,
    $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE, $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
    $01, $81, $41, $C1, $21, $A1, $61, $E1, $11, $91, $51, $D1, $31, $B1, $71, $F1,
    $09, $89, $49, $C9, $29, $A9, $69, $E9, $19, $99, $59, $D9, $39, $B9, $79, $F9,
    $05, $85, $45, $C5, $25, $A5, $65, $E5, $15, $95, $55, $D5, $35, $B5, $75, $F5,
    $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
    $03, $83, $43, $C3, $23, $A3, $63, $E3, $13, $93, $53, $D3, $33, $B3, $73, $F3,
    $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB, $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
    $07, $87, $47, $C7, $27, $A7, $67, $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF, $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF);

const
  CRC_BYTE_LZCOUNT: array[UInt8] of UInt8 = (
    8,7,6,6,5,5,5,5,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);    

{===============================================================================
    Auxiliary processing - TCRCBufer
===============================================================================}
{
  All CRCBufer functions are assuming that the data they get are correct, do
  not contain bits they should not and are of proper lengths - they are called
  only internally and therefore all data should be sanitized.
}

procedure CRCBufferCheckLength(const Buffer: TCRCBuffer; BitCount: TMemSize);
begin
If TMemSize(Length(Buffer)) <> uDivCeilPow2NC(BitCount,8) then
  raise ECRCInvalidValue.CreateFmt('CRCBufferCheckLength: Length of buffer (%d) does not correspond to bit count (%u).',[Length(Buffer),BitCount]);
end;

//------------------------------------------------------------------------------

procedure CRCBufferMaskBits(var Buffer: TCRCBuffer; BitCount: TMemSize);
var
  RemainingBits:  Integer;
  i:              Integer;
begin
If Length(Buffer) > 0 then
  begin
    If BitCount > 0 then
      begin
        RemainingBits := Integer(BitCount);
        i := Low(Buffer);
        while i <= High(Buffer) do begin
          If RemainingBits <= 0 then
            Buffer[i] := 0
          else If RemainingBits < 8 then
            Buffer[i] := Buffer[i] and UInt8(UInt8(-1) shl (8 - RemainingBits));
          Inc(i);
          Dec(RemainingBits,8);
        end;
      end
    else FillChar(Buffer[Low(Buffer)],Length(Buffer),0);
  end;
end;

{-------------------------------------------------------------------------------
    Auxiliary processing - TCRCBufer - shifts
-------------------------------------------------------------------------------}

procedure CRCBufferShiftBytesRight(var Buffer: TCRCBuffer; ByteShift: Integer);
var
  i:  Integer;
begin
If (Length(Buffer) > 0) and (ByteShift > 0) then
  begin
    If ByteShift < Length(Buffer) then
      begin
        For i := High(Buffer) downto Succ(ByteShift) do
          Buffer[i] := Buffer[i - ByteShift];
        For i := ByteShift downto Low(Buffer) do
          Buffer[i] := 0;
      end
    else FillChar(Buffer[Low(Buffer)],Length(Buffer),0);
  end;
end;

//------------------------------------------------------------------------------

procedure CRCBufferShiftBytesLeft(var Buffer: TCRCBuffer; ByteShift: Integer);
var
  i:  Integer;
begin
If (Length(Buffer) > 0) and (ByteShift > 0) then
  begin
    If ByteShift < Length(Buffer) then
      begin
        For i := Low(Buffer) to Pred(Length(Buffer) - ByteShift) do
          Buffer[i] := Buffer[i + ByteShift];
        For i := (Length(Buffer) - ByteShift) to High(Buffer) do
          Buffer[i] := 0;
      end
    else FillChar(Buffer[Low(Buffer)],Length(Buffer),0);
  end;
end;

//------------------------------------------------------------------------------

procedure CRCBufferShiftBitsRight(var Buffer: TCRCBuffer; BitShift: Integer);
var
  i:  Integer;
begin
If (Length(Buffer) > 0) and (BitShift > 0) then
  begin
    If (BitShift shr 3) < Length(Buffer) then
      begin
        If BitShift >= 8 then
          begin
            CRCBufferShiftBytesRight(Buffer,BitShift shr 3);
            BitShift := BitShift and 7;
          end;
      {
        Now traverse bytes from the right and shift each by BitShift places to
        the right (down) while shifting in bits from a byte one place to the
        left.
      }
        For i := High(Buffer) downto Succ(Low(Buffer)) do
          Buffer[i] := UInt8((UInt16(UInt16(Buffer[Pred(i)]) shl 8) or UInt16(Buffer[i])) shr BitShift);
        Buffer[Low(Buffer)] := Buffer[Low(Buffer)] shr BitShift;
      end
    else FillChar(Buffer[Low(Buffer)],Length(Buffer),0);
  end;
end;

//------------------------------------------------------------------------------

procedure CRCBufferShiftBitsLeft(var Buffer: TCRCBuffer; BitShift: Integer);
var
  i:  Integer;
begin
If (Length(Buffer) > 0) and (BitShift > 0) then
  begin
    If (BitShift shr 3) < Length(Buffer) then
      begin
        If BitShift >= 8 then
          begin
            CRCBufferShiftBytesLeft(Buffer,BitShift shr 3);
            BitShift := BitShift and 7;
          end;
        For i := Low(Buffer) to Pred(High(Buffer)) do
          Buffer[i] := UInt8(((UInt16(UInt16(Buffer[i]) shl 8) or UInt16(Buffer[Succ(i)])) shl BitShift) shr 8);
        Buffer[High(Buffer)] := UInt8(Buffer[High(Buffer)] shl BitShift);
      end
    else FillChar(Buffer[Low(Buffer)],Length(Buffer),0);
  end;
end;

{-------------------------------------------------------------------------------
    Auxiliary processing - TCRCBufer - reflection
-------------------------------------------------------------------------------}

procedure CRCBufferReflectBytes(var Buffer: TCRCBuffer);
var
  i:    Integer;
  Temp: UInt8;
begin
If Length(Buffer) > 1 then
  For i := Low(Buffer) to (High(Buffer) shr 1) do
    begin
      Temp := Buffer[i];
      Buffer[i] := Buffer[High(Buffer) - i];
      Buffer[High(Buffer) - i] := Temp;
    end;
end;

//------------------------------------------------------------------------------

procedure CRCBufferReflectBits(var Buffer: TCRCBuffer; BitCount: TMemSize);
var
  i:  Integer;
begin
If (Length(Buffer) > 0) and (BitCount > 0) then
  begin
    // first reflect all bytes
    CRCBufferReflectBytes(Buffer);
    // now reflect bits withing bytes
    For i := Low(Buffer) to High(Buffer) do
      Buffer[i] := CRC_BYTE_REFLECTED[Buffer[i]];
    // rectify position of left-most bit
    If (BitCount and 7) <> 0 then
      CRCBufferShiftBitsLeft(Buffer,8 - (BitCount and 7));
  end;
end;

{-------------------------------------------------------------------------------
    Auxiliary processing - TCRCBufer - multi-buffer operations
-------------------------------------------------------------------------------}

procedure CRCBufferXOR(var Destination: TCRCBuffer; const Source: TCRCBuffer);
var
  i:  Integer;
begin
If (Length(Destination) > 0) and (Length(Source) > 0) then
  For i := 0 to Pred(iMin(Length(Destination),Length(Source))) do
    Destination[i] := Destination[i] xor Source[i];
end;

{-------------------------------------------------------------------------------
    Auxiliary processing - TCRCBufer - conversions
-------------------------------------------------------------------------------}

procedure CRCBufferConvertToValue(const Buffer: TCRCBuffer; BitCount: TMemSize; out Value: TCRCValue);
begin
Value.BitCount := BitCount;
If Value.BitCount > 0 then
  begin
    Value.Data := Copy(Buffer);
    If (Value.BitCount and 7) <> 0 then
      CRCBufferShiftBitsRight(Value.Data,8 - Integer(Value.BitCount and 7));
    CRCBufferReflectBytes(Value.Data);
  end
else Value.Data := nil;
end;

//------------------------------------------------------------------------------

procedure CRCBufferConvertFromValue(const Value: TCRCValue; out Buffer: TCRCBuffer);
begin
If Value.BitCount > 0 then
  begin
    Buffer := Copy(Value.Data);
    CRCBufferReflectBytes(Buffer);
    If (Value.BitCount and 7) <> 0 then
      CRCBufferShiftBitsLeft(Buffer,8 - Integer(Value.BitCount and 7));
  end
else Buffer := nil;
end;

//------------------------------------------------------------------------------

Function CRCBufferAsBitString(const Buffer: TCRCBuffer): String;
var
  i,j:  Integer;
begin
If Length(Buffer) > 0 then
  begin
    Result := StringOfChar(' ',Pred(Length(Buffer) * 9));
    For i := Low(Buffer) to High(Buffer) do
      begin
        For j := 7 downto 0 do
          If (Buffer[i] shr j) and 1 <> 0 then
            Result[(i * 9) + (8 - j)] := '1'
          else
            Result[(i * 9) + (8 - j)] := '0';
      end;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function CRCBufferAsString(const Buffer: TCRCBuffer; BitCount: TMemSize): String;
var
  FullLastByte: Boolean;
  i:            Integer;
begin
If BitCount > 0 then
  begin
    Result := StringOfChar('0',uDivCeilPow2NC(BitCount,4));
    FullLastByte := ((BitCount and 7) = 0) or ((BitCount and 7) > 4);
    For i := Low(Buffer) to High(Buffer) do
      begin
        Result[1 + (i * 2)] := IntToHex(Buffer[i] shr 4,1)[1];
        If (i < High(Buffer)) or FullLastByte then
          Result[2 + (i * 2)] := IntToHex(Buffer[i] and $F,1)[1];
      end;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function CRCBufferFromString(const Str: String; BitCount: TMemSize): TCRCBuffer;
var
  ExpLength:  Integer;
  TempStr:    String;
  i:          Integer;
begin
If (BitCount <= 0) and (Length(Str) > 0) then
  BitCount := Length(Str) * 4;
Result := nil;
If BitCount > 0 then
  begin
    ExpLength := Integer(uDivCeilPow2NC(BitCount,8) * 2);
    If Length(Str) < ExpLength then
      TempStr := Str + StringOfChar('0',ExpLength - Length(Str))
    else If Length(Str) > ExpLength then
      TempStr := Copy(Str,1,ExpLength)
    else
      TempStr := Str;
    SetLength(Result,uDivCeilPow2NC(BitCount,8));
    For i := Low(Result) to High(Result) do
      Result[i] := UInt8(StrToInt('$' + Copy(TempStr,1 + (i * 2),2)));
  end;
end;

{-------------------------------------------------------------------------------
    Auxiliary processing - TCRCBufer - comparison
-------------------------------------------------------------------------------}

Function CRCBufferCompare(const A,B: TCRCBuffer): Integer;
var
  i:  Integer;
begin
Result := 0;
// buffers are internally big-endian, so we can start low and go up
For i := Low(A) to High(A) do
  If A[i] <> B[i] then
    begin
      If A[i] > B[i] then
        Result := +1
      else
        Result := -1;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function CRCBufferSame(const A,B: TCRCBuffer): Boolean;
var
  i:  Integer;
begin
Result := True;
For i := Low(A) to High(A) do
  If A[i] <> B[i] then
    begin
      Result := False;
      Break{For i};
    end;
end;

{===============================================================================
    Auxiliary processing - CRCValue
===============================================================================}

procedure CRCValueCheckLength(const Value: TCRCValue);
begin
If TMemSize(Length(Value.Data)) <> uDivCeilPow2NC(Value.BitCount,8) then
  raise ECRCInvalidValue.CreateFmt('CRCValueCheckLength: Length of data (%d) does not correspond to bit count (%u).',[Length(Value.Data),Value.BitCount]);
end;

//------------------------------------------------------------------------------

procedure CRCValueMaskBits(var Value: TCRCValue);
var
  RemainingBits:  Integer;
  i:              Integer;
begin
If Length(Value.Data) > 0 then
  begin
    If Value.BitCount > 0 then
      begin
        RemainingBits := Integer(Value.BitCount);
        i := Low(Value.Data);
        while i <= High(Value.Data) do begin
          If RemainingBits <= 0 then
            Value.Data[i] := 0
          else If RemainingBits < 8 then
            Value.Data[i] := Value.Data[i] and (UInt8(-1) shr (8 - RemainingBits));
          Inc(i);
          Dec(RemainingBits,8);
        end;
      end
    else FillChar(Value.Data[Low(Value.Data)],Length(Value.Data),0);
  end;
end;

//------------------------------------------------------------------------------

procedure CRCValueSecureCopy(const Source: TCRCValue; out Destination: TCRCValue);
begin
Destination.Data := Copy(Source.Data);  // ensures unique copy
Destination.BitCount := Source.BitCount;
end;

{-------------------------------------------------------------------------------
    Auxiliary processing - TCRCValue - public utilities
-------------------------------------------------------------------------------}

procedure CRCValueInit(BitCount: TMemSize; out Value: TCRCValue);
begin
Value.Data := nil;
SetLength(Value.Data,uDivCeilPow2NC(BitCount,8));
Value.BitCount := BitCount;
end;

//------------------------------------------------------------------------------

Function CRCValueCompare(const A,B: TCRCValue): Integer;
var
  Mask: UInt8;
  i:    Integer;
begin
Result := 0;
CRCValueCheckLength(A);
CRCValueCheckLength(B);
If A.BitCount = B.BitCount then
  begin
    If (A.BitCount and 7) <> 0 then
      Mask := UInt8(-1) shr (8 - (A.BitCount and 7))
    else
      Mask := $FF; 
    For i := High(A.Data) downto Low(A.Data) do
      begin
        If (A.Data[i] and Mask) <> (B.Data[i] and Mask) then
          begin
            If (A.Data[i] and Mask) > (B.Data[i] and Mask) then
              Result := +1
            else
              Result := -1;
            Break{For i};
          end;
        Mask := $FF;
      end;
  end
else raise ECRCSizeMismatch.CreateFmt('CRCValueCompare: Cannot compare values with different bit count (%u, %u).',[A.BitCount,B.BitCount]);
end;

//------------------------------------------------------------------------------

Function CRCValueSame(const A,B: TCRCValue): Boolean;
var
  Mask: UInt8;
  i:    Integer;
begin
Result := True;
CRCValueCheckLength(A);
CRCValueCheckLength(B);
If A.BitCount = B.BitCount then
  begin
    If (A.BitCount and 7) <> 0 then
      Mask := UInt8(-1) shr (8 - (A.BitCount and 7))
    else
      Mask := $FF;
    // using precalculated mask, so need to start from the last byte  
    For i := High(A.Data) downto Low(A.Data) do
      begin
        If (A.Data[i] and Mask) <> (B.Data[i] and Mask) then
          begin
            Result := False;
            Break{For i};
          end;
        Mask := $FF;
      end;
  end
else raise ECRCSizeMismatch.CreateFmt('CRCValueSame: Cannot compare values with different bit count (%u, %u).',[A.BitCount,B.BitCount]);
end;

//------------------------------------------------------------------------------

Function CRCValueAsString(const Value: TCRCValue): String;
var
  Temp:         TCRCValue;
  FullLastByte: Boolean;
  i,ResPos:     Integer;
begin
CRCValueCheckLength(Value);
CRCValueSecureCopy(Value,Temp);
If Temp.BitCount > 0 then
  begin
    CRCValueMaskBits(Temp);
    Result := StringOfChar('0',uDivCeilPow2NC(Temp.BitCount,4));
    FullLastByte := ((Temp.BitCount and 7) = 0) or ((Temp.BitCount and 7) > 4);
    ResPos := 1;
    For i := High(Temp.Data) downto Low(Temp.Data) do
      begin
        If (i < High(Temp.Data)) or FullLastByte then
          begin
            Result[ResPos] := IntToHex(Temp.Data[i] shr 4,1)[1];
            Result[ResPos + 1] := IntToHex(Temp.Data[i] and $F,1)[1];
            Inc(ResPos);
          end
        else Result[ResPos] := IntToHex(Temp.Data[i] and $F,1)[1];
        Inc(ResPos);
      end;
  end
else Result := '';
end;

//------------------------------------------------------------------------------

Function CRCValueFromString(const Str: String; BitCount: TMemSize = 0): TCRCValue;
var
  ExpLength:  Integer;
  TempStr:    String;
  i:          Integer;
begin
If (BitCount <= 0) and (Length(Str) > 0) then
  BitCount := Length(Str) * 4;
CRCValueInit(BitCount,Result);
If BitCount > 0 then
  begin
    ExpLength := Integer(uDivCeilPow2NC(BitCount,8) * 2);
    If Length(Str) < ExpLength then
      TempStr := StringOfChar('0',ExpLength - Length(Str)) + Str
    else If Length(Str) > ExpLength then
      TempStr := Copy(Str,Length(Str) - Pred(ExpLength),ExpLength)
    else
      TempStr := Str;
    For i := Low(Result.Data) to High(Result.Data) do
      Result.Data[i] := UInt8(StrToInt('$' + Copy(TempStr,Pred(Length(TempStr)) - (i * 2),2)));
    CRCValueMaskBits(Result);
  end;
end;

//------------------------------------------------------------------------------

procedure CRCValueSaveToBuffer(const Value: TCRCValue; out Buffer);
begin
CRCValueCheckLength(Value);
If Value.BitCount > 0 then
  Move(Value.Data[Low(Value.Data)],Addr(Buffer)^,uDivCeilPow2NC(Value.BitCount,8));
end;

//------------------------------------------------------------------------------

procedure CRCValueLoadFromBuffer(var Value: TCRCValue; const Buffer);
begin
CRCValueCheckLength(Value);
If Value.BitCount > 0 then
  begin
    Move(Buffer,Value.Data[Low(Value.Data)],uDivCeilPow2NC(Value.BitCount,8));
    CRCValueMaskBits(Value);
  end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TCRCCalculator
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCRCCalculator - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCRCCalculator - protected methods
-------------------------------------------------------------------------------}

procedure TCRCCalculator.HashBitsSetter(Value: TMemSize);
var
  ByteCount:  TMemSize;
begin
If Value <> fHashBits then
  begin
    fHashBits := Value;
    fPolynomial := nil;
    fInitial := nil;
    fXorOut := nil;
    fCRCValue := nil;
    fResidue := nil;
    If fHashBits > 0 then
      begin
        ByteCount := uDivCeilPow2NC(fHashBits,8);
        SetLength(fPolynomial,uIfThen((fHashBits and 7) = 0,ByteCount + 1,ByteCount));
        // ensure first and last bits are set
        If Length(fPolynomial) > 0 then
          begin
            fPolynomial[Low(fPolynomial)] := $80;
            fPolynomial[High(fPolynomial)] := fPolynomial[High(fPolynomial)] or
              UInt8(UInt8(1) shl ((8 - ((fHashBits + 1) and 7)) and 7));
          end;
        SetLength(fInitial,ByteCount);
        SetLength(fXorOut,ByteCount);
        SetLength(fCRCValue,ByteCount);
        SetLength(fResidue,ByteCount);
      end;
  end;
// if processing is under way, cancel it
AbortHashing;
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.PolynomialGetter: TCRCValue;
begin
CRCBufferConvertToValue(fPolynomial,uIfThen(fHashBits > 0,fHashBits + 1,0),Result)
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.PolynomialSetter(const Value: TCRCValue);
begin
CRCValueCheckLength(Value);
If Value.BitCount < 2 then
  raise ECRCInvalidValue.CreateFmt('TCRCCalculator.PolynomialSetter: Given polynomial is too small (%d).',[Value.BitCount]);
HashBitsSetter(Value.BitCount - 1);
CRCBufferConvertFromValue(Value,fPolynomial);
CRCBufferMaskBits(fPolynomial,Value.BitCount);
// make sure first and last bits are set
If Length(fPolynomial) > 0 then
  begin
    fPolynomial[Low(fPolynomial)] := fPolynomial[Low(fPolynomial)] or $80;
    fPolynomial[High(fPolynomial)] := fPolynomial[High(fPolynomial)] or
      UInt8(UInt8(1) shl ((8 - (Value.BitCount and 7)) and 7))
  end;
// HashBitsSetter already aborted posssible pending processing, no need to do it again
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.InitialGetter: TCRCValue;
begin
CRCBufferConvertToValue(fInitial,fHashBits,Result);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.InitialSetter(const Value: TCRCValue);
begin
CRCValueCheckLength(Value);
If fHashBits > 0 then
  begin
    If Value.BitCount <> fHashBits then
      raise ECRCIncompatibleValue.CreateFmt('TCRCCalculator.InitialSetter: Wrong bit count (%u, expected %u).',[Value.BitCount,fHashBits]);
    AbortHashing;
  end
else HashBitsSetter(Value.BitCount); // aborts pending processing
CRCBufferConvertFromValue(Value,fInitial);
CRCBufferMaskBits(fInitial,Value.BitCount);
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.XorOutGetter: TCRCValue;
begin
CRCBufferConvertToValue(fXorOut,fHashBits,Result);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.XorOutSetter(const Value: TCRCValue);
begin
CRCValueCheckLength(Value);
If fHashBits > 0 then
  begin
    If Value.BitCount <> fHashBits then
      raise ECRCIncompatibleValue.CreateFmt('TCRCCalculator.XorOutSetter: Wrong bit count (%u, expected %u).',[Value.BitCount,fHashBits]);
    AbortHashing;
  end
else HashBitsSetter(Value.BitCount);
CRCBufferConvertFromValue(Value,fXorOut);
CRCBufferMaskBits(fXorOut,Value.BitCount);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.ReflectInSetter(Value: Boolean);
begin
fReflectIn := Value;
AbortHashing;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.ReflectOutSetter(Value: Boolean);
begin
fReflectOut := Value;
AbortHashing;
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.CRCGetter: TCRCValue;
begin
CRCBufferConvertToValue(fCRCValue,fHashBits,Result);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.CRCSetter(const Value: TCRCValue);
begin
CRCValueCheckLength(Value);
If fHashBits > 0 then
  begin
    If Value.BitCount <> fHashBits then
      raise ECRCIncompatibleValue.CreateFmt('TCRCCalculator.CRCSetter: Wrong bit count (%u, expected %u).',[Value.BitCount,fHashBits]);
    // do not abort processing, this value is not used there
  end
else HashBitsSetter(Value.BitCount);
CRCBufferConvertFromValue(Value,fCRCValue);
CRCBufferMaskBits(fCRCValue,Value.BitCount);
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.ResidueGetter: TCRCValue;
begin
CRCBufferConvertToValue(fResidue,fHashBits,Result);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.Initialize;
var
  i:  Integer;
begin
inherited;
fHashBits := 0;
fPolynomial := nil;
fInitial := nil;
fXorOut := nil;
fReflectIn := False;
fReflectOut := False;
fCRCValue := nil;
fResidue := nil;
For i := Low(fPolyTable) to High(fPolyTable) do
  fPolyTable[i] := nil;
fProcBuffer := nil;
fProcBits := 0;
fProcThreshold := 0;
fProcInitApplied := False;
fProcessedOddBits := 0;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.ProcessingRound;
begin
If fProcBits > fProcThreshold then
  begin
    If not fProcInitApplied then
      begin
        CRCBufferXor(fProcBuffer,fCRCValue);
        fProcInitApplied := True;
      end;
    // one full round of processing
    while (fProcBits - fProcThreshold) >= 8 do
      begin
        while fProcBuffer[Low(fProcBuffer)] <> 0 do
          CRCBufferXor(fProcBuffer,fPolyTable[CRC_BYTE_LZCOUNT[fProcBuffer[Low(fProcBuffer)]]]);
        CRCBufferShiftBytesLeft(fProcBuffer,1);
        Dec(fProcBits,8);
      end;
    // partial round
    while fProcBits > fProcThreshold do
      begin
        If (fProcBuffer[Low(fProcBuffer)] and UInt8($80)) <> 0 then
          CRCBufferXor(fProcBuffer,fPolyTable[Low(fPolyTable)]);
        CRCBufferShiftBitsLeft(fProcBuffer,1);
        Dec(fProcBits);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.ProcessBuffer(const Buffer; Size: TMemSize);
var
  CurrentData:  PUInt8;
  Temp:         UInt8;
  TempBits:     TMemSize;
begin
CurrentData := @Buffer;
while Size > 0 do
  begin
    If (fProcBits and 7) <> 0 then
      begin
        If fReflectIn then
          Temp := CRC_BYTE_REFLECTED[CurrentData^]
        else
          Temp := CurrentData^;
        fProcBuffer[fProcBits shr 3] := fProcBuffer[fProcBits shr 3] or (Temp shr (fProcBits and 7));
        TempBits := fProcBits and 7;
        Inc(fProcBits,8 - TempBits);
        ProcessingRound;
        fProcBuffer[fProcBits shr 3] := UInt8(Temp shl (8 - TempBits));
        Inc(fProcBits,TempBits);
      {
        We can call processing again, in case it is now possible to align the
        bits to byte boundary.
      }
        ProcessingRound;
        Inc(CurrentData);
        Dec(Size);
      end
    else
      begin
        ProcessBufferAligned(CurrentData^,Size);
        Break{while};
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.ProcessBufferAligned(const Buffer; Size: TMemSize);
var
  CurrentData:  PUInt8;
begin
CurrentData := @Buffer;
while Size > 0 do
  begin
    If fReflectIn then
      fProcBuffer[fProcBits shr 3] := CRC_BYTE_REFLECTED[CurrentData^]
    else
      fProcBuffer[fProcBits shr 3] := CurrentData^;
    Inc(fProcBits,8);
    ProcessingRound;      
    Inc(CurrentData);
    Dec(Size);
  end;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.ProcessBufferBits(const Buffer; BitCount: TMemSize);
var
  Temp:     UInt8;
  TempBits: TMemSize;
begin
If BitCount >= 8 then
  ProcessBuffer(Buffer,BitCount shr 3);
If (BitCount and 7) <> 0 then
  begin
    // now we have only partial byte...
    TempBits := BitCount and 7;
    // get the partial byte and mask it
    Temp := PUInt8(PtrAdvance(@Buffer,BitCount shr 3))^ and (UInt8(-1) shr (8 - TempBits));
    If fReflectIn then
      Temp := CRC_BYTE_REFLECTED[Temp] shr (8 - TempBits);
    If (fProcBits and 7) <> 0 then
      begin
        If (fProcBits and 7) + TempBits > 8 then
          begin
            TempBits := (TempBits + (fProcBits and 7)) and 7;
            fProcBuffer[fProcBits shr 3] := fProcBuffer[fProcBits shr 3] or (Temp shr TempBits);
            Inc(fProcBits,8 - (fProcBits and 7));
            ProcessingRound;
            fProcBuffer[fProcBits shr 3] := UInt8(Temp shl (8 - TempBits));
            Inc(fProcBits,TempBits);
            ProcessingRound;  // in case we can align
          end
        else
          begin
            // partial byte can fit together with remainder into one byte
            fProcBuffer[fProcBits shr 3] := fProcBuffer[fProcBits shr 3] or
              (Temp shl (8 - Integer(TempBits + fProcBits and 7)));
            Inc(fProcBits,TempBits);
            ProcessingRound;
          end;
      end
    else
      begin
        // bits are at low places, we need to shift them to high places
        fProcBuffer[fProcBits shr 3] := UInt8(Temp shl (8 - TempBits));
        Inc(fProcBits,TempBits);
        ProcessingRound;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.BufferLoaderMemory(Data: Pointer; BitCount: TMemSize; ValueOptions: TCRCValueOptions; out Buffer: TCRCBuffer);
begin
Buffer := nil;
SetLength(Buffer,uDivCeilPow2NC(BitCount,8));
Move(Data^,Buffer[Low(Buffer)],Length(Buffer));
// rectify data to expected bit order
BufferRectifyAfterLoad(Buffer,BitCount,ValueOptions);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.BufferLoaderString(Data: Pointer; BitCount: TMemSize; ValueOptions: TCRCValueOptions; out Buffer: TCRCBuffer);
var
  ExpLength:  Integer;
  TempStr:    String;
  i:          Integer;
begin
Buffer := nil;
SetLength(Buffer,uDivCeilPow2NC(BitCount,8));
ExpLength := Length(Buffer) * 2;
If optStringReadFromLeft in ValueOptions then
  begin
    If Length(String(Data^)) < ExpLength then
      TempStr := String(Data^) + StringOfChar('0',ExpLength - Length(String(Data^)))
    else If Length(String(Data^)) > ExpLength then
      TempStr := Copy(String(Data^),1,ExpLength)
    else
      TempStr := String(Data^);
    For i := Low(Buffer) to High(Buffer) do
      Buffer[i] := StrToInt('$' + Copy(TempStr,1 + (i * 2),2));
  end
else
  begin
    If Length(String(Data^)) < ExpLength then
      TempStr := StringOfChar('0',ExpLength - Length(String(Data^))) + String(Data^)
    else If Length(String(Data^)) > ExpLength then
      TempStr := Copy(String(Data^),Length(String(Data^)) - Pred(ExpLength),ExpLength)
    else
      TempStr := String(Data^);
    For i := Low(Buffer) to High(Buffer) do
      Buffer[i] := StrToInt('$' + Copy(TempStr,Pred(Length(TempStr)) - (i * 2),2));      
  end;
BufferRectifyAfterLoad(Buffer,BitCount,ValueOptions);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.BufferRectifyAfterLoad(var Buffer: TCRCBuffer; BitCount: TMemSize; ValueOptions: TCRCValueOptions);
begin
If not(optHighByteFirst in ValueOptions) and (Length(Buffer) > 1) then
  begin
    CRCBufferReflectBytes(Buffer);
    If (BitCount and 7) <> 0 then
      begin
        If optLeftAlignedPartial in ValueOptions then
          Buffer[High(Buffer)] := Buffer[High(Buffer)] shr (8 - (BitCount and 7));
        CRCBufferShiftBitsLeft(Buffer,8 - (BitCount and 7));
      end;
  end
else If ((BitCount and 7) <> 0) and not(optLeftAlignedPartial in ValueOptions) then
  Buffer[High(Buffer)] := UInt8(Buffer[High(Buffer)] shl (8 - (BitCount and 7)));
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetPolynomialInternal(Data: Pointer; BitCount: TMemSize; ValueOptions: TCRCValueOptions; BufferLoader: TCRCBufferLoader);
var
  PolyBits: TMemSize;
begin
// check that we have enough data
If ((BitCount <= 0) and not([optPolyExcludesHighBit,optPolyExcludesLowBit] <= ValueOptions)) or
   ((BitCount = 1) and (([optPolyExcludesHighBit,optPolyExcludesLowBit] * ValueOptions) = [])) then
  raise ECRCInvalidValue.CreateFmt('TCRCCalculator.SetPolynomialInternal: Given polynomial is too small (%d).',[BitCount]);
// get correct bit count according to given value options
PolyBits := BitCount;
If optPolyExcludesHighBit in ValueOptions then
  Inc(PolyBits);
If optPolyExcludesLowBit in ValueOptions then
  Inc(PolyBits);
// HashBitsSetter will abort pending processing
HashBitsSetter(PolyBits - 1);
// load data into buffer
BufferLoader(Data,BitCount,ValueOptions,fPolynomial);
// add and/or set highest and lowest bits (make sure these bits are always set)
If optPolyExcludesHighBit in ValueOptions then
  begin
    If (BitCount and 7) = 0 then
      SetLength(fPolynomial,Length(fPolynomial) + 1);
    CRCBufferShiftBitsRight(fPolynomial,1);
    Inc(BitCount);
  end;
fPolynomial[Low(fPolynomial)] := fPolynomial[Low(fPolynomial)] or UInt8($80);
If optPolyExcludesLowBit in ValueOptions then
  begin
    If (BitCount and 7) = 0 then
      SetLength(fPolynomial,Length(fPolynomial) + 1);
    Inc(BitCount);
  end;
fPolynomial[High(fPolynomial)] := fPolynomial[High(fPolynomial)] or
  UInt8(UInt8(1) shl ((8 - (BitCount and 7)) and 7));
CRCBufferMaskBits(fPolynomial,BitCount);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetBufferInternal(var Buffer: TCRCBuffer; Data: Pointer; BitCount: TMemSize; ValueOptions: TCRCValueOptions; BufferLoader: TCRCBufferLoader);
begin
If fHashBits > 0 then
  begin
    If BitCount <> fHashBits then
      raise ECRCInvalidValue.CreateFmt('TCRCCalculator.SetBufferInternal: Wrong size of data (%u, expected %u).',[BitCount,fHashBits]);
    If Addr(Buffer) <> Addr(fCRCValue) then
      AbortHashing;
  end
else HashBitsSetter(BitCount);  // BitCount can be zero, but that is ok here
If BitCount > 0 then
  begin
    BufferLoader(Data,BitCount,ValueOptions,Buffer);
    CRCBufferMaskBits(Buffer,BitCount);
  end;
end;

{-------------------------------------------------------------------------------
    TCRCCalculator - public methods
-------------------------------------------------------------------------------}

class Function TCRCCalculator.HashType: THashType;
begin
Result := htCRC;
end;

//------------------------------------------------------------------------------

class Function TCRCCalculator.HashEndianness: THashEndianness;
begin
Result := heLittle;
end;

//------------------------------------------------------------------------------

class Function TCRCCalculator.HashFinalization: Boolean;
begin
Result := True;
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.HashName: String;
begin
If fHashBits > 0 then
  Result := Format('CRC-%d(custom)',[fHashBits])
else
  Result := 'CRC(undefined)';
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.HashSize: TMemSize;
begin
Result := uDivCeilPow2NC(fHashBits,8);
end;

//------------------------------------------------------------------------------
threadvar
  THRVAR_CreateAndInit_BitCount: TMemSize;

constructor TCRCCalculator.CreateAndInit{$IFNDEF FPC}(Dummy: Integer = 0){$ENDIF};
begin
Create;
If THRVAR_CreateAndInit_BitCount > 0 then
  HashBitsSetter(THRVAR_CreateAndInit_BitCount)
else
  HashBitsSetter(1);
THRVAR_CreateAndInit_BitCount := 0;  
Init;
end;

//------------------------------------------------------------------------------

constructor TCRCCalculator.CreateAndInitFrom(Source: THashBase);
var
  i:  Integer;
begin
inherited CreateAndInitFrom(Source);
If Source is TCRCCalculator then
  begin
    // copy the entire state
    fHashBits := TCRCCalculator(Source).fHashBits;
    fPolynomial := Copy(TCRCCalculator(Source).fPolynomial);
    fInitial := Copy(TCRCCalculator(Source).fInitial);
    fXorOut := Copy(TCRCCalculator(Source).fXorOut);
    fReflectIn := TCRCCalculator(Source).fReflectIn;
    fReflectOut := TCRCCalculator(Source).fReflectOut;
    fCRCValue := Copy(TCRCCalculator(Source).fCRCValue);
    fResidue := Copy(TCRCCalculator(Source).fResidue);
    For i := Low(fPolyTable) to High(fPolyTable) do
      fPolyTable[i] := Copy(TCRCCalculator(Source).fPolyTable[i]);
    fProcBuffer := Copy(TCRCCalculator(Source).fProcBuffer);
    fProcBits := TCRCCalculator(Source).fProcBits;
    fProcThreshold := TCRCCalculator(Source).fProcThreshold;
    fProcInitApplied := TCRCCalculator(Source).fProcInitApplied;
    fProcessedOddBits := TCRCCalculator(Source).fProcessedOddBits
  end
else raise ECRCIncompatibleClass.CreateFmt('TCRCCalculator.CreateAndInitFrom: Incompatible class (%s).',[Source.ClassName]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCRCCalculator.CreateAndInitFrom(Source: TCRCValue);
begin
CRCValueCheckLength(Source);
If Source.BitCount <= 0 then
  raise ECRCInvalidValue.Create('TCRCCalculator.CreateAndInitFrom: Empty source not allowed.');
THRVAR_CreateAndInit_BitCount := Source.BitCount;
CreateAndInit;
CRCBufferConvertFromValue(Source,fCRCValue);
end;

//------------------------------------------------------------------------------

constructor TCRCCalculator.CreateAndInitFromString(const Str: String);
begin
If Length(Str) <= 0 then
  raise ECRCInvalidValue.Create('TCRCCalculator.CreateAndInitFromString: Empty source not allowed.');
THRVAR_CreateAndInit_BitCount := Length(Str) * 4;
CreateAndInit;
FromString(Str);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetPolynomial(const Polynomial; BitCount: TMemSize; ValueOptions: TCRCValueOptions = []);
begin
SetPolynomialInternal(@Polynomial,BitCount,ValueOptions,BufferLoaderMemory);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRCCalculator.SetPolynomial(const Polynomial: TCRCValue; ValueOptions: TCRCValueOptions = []);
begin
CRCValueCheckLength(Polynomial);
If Polynomial.BitCount > 0 then
  SetPolynomialInternal(Addr(Polynomial.Data[Low(Polynomial.Data)]),Polynomial.BitCount,ValueOptions,BufferLoaderMemory);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetPolynomialFromString(const Str: String; BitCount: TMemSize = 0; ValueOptions: TCRCValueOptions = []);
begin 
If BitCount <= 0 then
  begin
    If fHashBits > 0 then
      BitCount := fHashBits + 1
    else If Length(Str) > 0 then
      BitCount := Length(Str) * 4;
  end;
If BitCount > 0 then
  SetPolynomialInternal(@Str,BitCount,ValueOptions,BufferLoaderString);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetInitial(const Initial; BitCount: TMemSize; ValueOptions: TCRCValueOptions = []);
begin
SetBufferInternal(fInitial,@Initial,BitCount,ValueOptions,BufferLoaderMemory);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRCCalculator.SetInitial(const Initial: TCRCValue; ValueOptions: TCRCValueOptions = []);
begin
CRCValueCheckLength(Initial);
If Initial.BitCount > 0 then
  SetBufferInternal(fInitial,Addr(Initial.Data[Low(Initial.Data)]),Initial.BitCount,ValueOptions,BufferLoaderMemory);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetInitialFromString(const Str: String; BitCount: TMemSize = 0; ValueOptions: TCRCValueOptions = []);
begin
If BitCount <= 0 then
  begin
    If fHashBits > 0 then
      BitCount := fHashBits
    else If Length(Str) > 0 then
      BitCount := Length(Str) * 4;
  end;
If BitCount > 0 then
  SetBufferInternal(fInitial,@Str,BitCount,ValueOptions,BufferLoaderString);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetXorOut(const XorOut; BitCount: TMemSize; ValueOptions: TCRCValueOptions = []);
begin
SetBufferInternal(fXorOut,@XorOut,BitCount,ValueOptions,BufferLoaderMemory);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRCCalculator.SetXorOut(const XorOut: TCRCValue; ValueOptions: TCRCValueOptions = []);
begin
CRCValueCheckLength(XorOut);
If Initial.BitCount > 0 then
  SetBufferInternal(fXorOut,Addr(XorOut.Data[Low(XorOut.Data)]),XorOut.BitCount,ValueOptions,BufferLoaderMemory);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetXorOutFromString(const Str: String; BitCount: TMemSize = 0; ValueOptions: TCRCValueOptions = []);
begin
If BitCount <= 0 then
  begin
    If fHashBits > 0 then
      BitCount := fHashBits
    else If Length(Str) > 0 then
      BitCount := Length(Str) * 4;
  end;
If BitCount > 0 then
  SetBufferInternal(fXorOut,@Str,BitCount,ValueOptions,BufferLoaderString);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetCRC(const CRC; BitCount: TMemSize; ValueOptions: TCRCValueOptions = []);
begin
SetBufferInternal(fCRCValue,@CRC,BitCount,ValueOptions,BufferLoaderMemory);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCRCCalculator.SetCRC(const CRC: TCRCValue; ValueOptions: TCRCValueOptions = []);
begin
CRCValueCheckLength(XorOut);
If Initial.BitCount > 0 then
  SetBufferInternal(fCRCValue,Addr(CRC.Data[Low(CRC.Data)]),CRC.BitCount,ValueOptions,BufferLoaderMemory)
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SetCRCFromString(const Str: String; BitCount: TMemSize = 0; ValueOptions: TCRCValueOptions = []);
begin
If BitCount <= 0 then
  begin
    If fHashBits > 0 then
      BitCount := fHashBits
    else If Length(Str) > 0 then
      BitCount := Length(Str) * 4;
  end;
If BitCount > 0 then
  SetBufferInternal(fCRCValue,@Str,BitCount,ValueOptions,BufferLoaderString)
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.Init;
var
  i:  Integer;
begin
inherited;
// CRC cannot be 0 bits wide, do some fallback
If fHashBits <= 0 then
  HashBitsSetter(1);
// initialize the state
For i := Low(fPolyTable) to High(fPolyTable) do
  begin
    fPolyTable[i] := Copy(fPolynomial);
    SetLength(fPolyTable[i],Length(fPolyTable[i]) + 1);
    fPolyTable[i][High(fPolyTable[i])] := 0;  // should be zeroed automatically, but...
    CRCBufferShiftBitsRight(fPolyTable[i],i);
  end;
fProcBuffer := nil;
SetLength(fProcBuffer,Length(fPolynomial) + 2);
fProcBits := 0;
fProcThreshold := Pred(Length(fProcBuffer)) * 8;
fProcInitApplied := False;
fProcessedOddBits := 0;
fCRCValue := Copy(fInitial);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.Final;
begin
If not fProcInitApplied then
  begin
    CRCBufferXor(fProcBuffer,fCRCValue);
    fProcInitApplied := True;
  end;
// process remaining full bytes
while fProcBits >= 8 do
  begin
    // run one full round of processing
    while fProcBuffer[Low(fProcBuffer)] <> 0 do
      CRCBufferXor(fProcBuffer,fPolyTable[CRC_BYTE_LZCOUNT[fProcBuffer[Low(fProcBuffer)]]]);
    CRCBufferShiftBytesLeft(fProcBuffer,1);
    Dec(fProcBits,8);
  end;
// and now remaining non-integral bytes (odd bits)
while fProcBits > 0 do
  begin
    If (fProcBuffer[Low(fProcBuffer)] and UInt8($80)) <> 0 then
      CRCBufferXor(fProcBuffer,fPolyTable[Low(fPolyTable)]);
    CRCBufferShiftBitsLeft(fProcBuffer,1);
    Dec(fProcBits);
  end;
// move result into CRC value
Move(fProcBuffer[Low(fProcBuffer)],fCRCValue[Low(fCRCValue)],uDivCeilPow2NC(fHashBits,8));
// apply optional reflection and final xorout
If fReflectOut then
  CRCBufferReflectBits(fCRCValue,fHashBits);
fResidue := Copy(fCRCValue);  
CRCBufferXor(fCRCValue,fXorOut);
CRCBufferMaskBits(fCRCValue,fHashBits);
inherited;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.UpdateBits(const Buffer; BitCount: TMemSize);
begin
If fInitialized then
  begin
    If not fFinalized then
      begin
        ProcessBufferBits(Buffer,BitCount);
        Inc(fProcessedBytes,BitCount shr 3);
        Inc(fProcessedOddBits,BitCount and 7);
        If fProcessedOddBits >= 8 then
          begin
            Inc(fProcessedBytes,fProcessedOddBits shr 3);
            fProcessedOddBits := fProcessedOddBits and 7;
          end;
      end
    else raise EHASHInvalidState.Create('TCRCCalculator.UpdateBits: Hash already finalized.');
  end
else raise EHASHInvalidState.Create('TCRCCalculator.UpdateBits: Hash not initialized.');
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.FinalBits(const Buffer; BitCount: TMemSize);
begin
UpdateBits(Buffer,BitCount);
Final;
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.Compare(Hash: THashBase): Integer;
begin
If Hash is TCRCCalculator then
  begin
    If TCRCCalculator(Hash).fHashBits = fHashBits then
      Result := CRCBufferCompare(fCRCValue,TCRCCalculator(Hash).fCRCValue)
    else
      raise ECRCIncompatibleValue.CreateFmt('TCRCCalculator.Compare: Incompatible values (differing lengths: %u, %u)',
        [TCRCCalculator(Hash).HashBits,fHashBits]);
  end
else raise ECRCIncompatibleClass.CreateFmt('TCRCCalculator.Compare: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.Same(Hash: THashBase): Boolean;
begin
If Hash is TCRCCalculator then
  begin
    If TCRCCalculator(Hash).fHashBits = fHashBits then
      Result := CRCBufferSame(fCRCValue,TCRCCalculator(Hash).fCRCValue)
    else
      raise ECRCIncompatibleValue.CreateFmt('TCRCCalculator.Same: Incompatible values (differing lengths: %u, %u)',
        [TCRCCalculator(Hash).HashBits,fHashBits]);
  end
else raise ECRCIncompatibleClass.CreateFmt('TCRCCalculator.Same: Incompatible class (%s).',[Hash.ClassName]);
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.AsString: String;
var
  Temp: TCRCValue;
begin
CRCBufferConvertToValue(fCRCValue,fHashBits,Temp);
Result := CRCValueAsString(Temp);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.FromString(const Str: String);
var
  Temp: TCRCValue;
begin
Temp := CRCValueFromString(Str,fHashBits);  // masks observed bits
If fHashBits <= 0 then
  HashBitsSetter(Temp.BitCount);
CRCBufferConvertFromValue(Temp,fCRCValue);
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.FromStringDef(const Str: String; const Default: TCRCValue);
begin
CRCValueCheckLength(Default);
inherited FromStringDef(Str,Default);
If not TryFromString(Str) then
  begin
    If fHashBits <= 0 then
      HashBitsSetter(Default.BitCount);
    CRCBufferConvertFromValue(Default,fCRCValue);
    CRCBufferMaskBits(fCRCValue,fHashBits);
  end;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.SaveToStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRCValue;
begin
ConsumeArg(Ord(Endianness));
If fHashBits > 0 then
  begin
    CRCBufferConvertToValue(fCRCValue,fHashBits,Temp);
    Stream.WriteBuffer(Temp.Data[Low(Temp.Data)],Length(Temp.Data));
  end;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculator.LoadFromStream(Stream: TStream; Endianness: THashEndianness = heDefault);
var
  Temp: TCRCValue;
begin
ConsumeArg(Ord(Endianness));
If fHashBits > 0 then
  begin
    CRCValueInit(fHashBits,Temp);
    Stream.ReadBuffer(Temp.Data[Low(Temp.Data)],Length(Temp.Data));
    CRCValueMaskBits(Temp); // make sure only needed bits are there
    CRCBufferConvertFromValue(Temp,fCRCValue);
  end;
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.InternalBufferAsString(Buffer: TCRCInternalBuffer): String;
var
  Index:  TMemSize;
begin
case Buffer of
  bufPolynomial:  Result := CRCBufferAsString(fPolynomial,uIfThen(fHashBits > 0,fHashBits + 1,0));
  bufInit:        Result := CRCBufferAsString(fInitial,fHashBits);
  bufXorOut:      Result := CRCBufferAsString(fXorOut,fHashBits);
  bufCRC:         Result := CRCBufferAsString(fCRCValue,fHashBits);
  bufResidue:     Result := CRCBufferAsString(fResidue,fHashBits);
  bufProcessing:  Result := CRCBufferAsString(fProcBuffer,TMemSize(Length(fProcBuffer) * 8));
  bufPolyTable0..
  bufPolyTable7:  begin
                    Index := TMemSize(Ord(Buffer) - Ord(bufPolyTable0));
                    Result := CRCBufferAsString(fPolyTable[Index],fHashBits + 1 + Index);
                  end;
else
  raise ECRCInvalidValue.CreateFmt('TCRCCalculator.InternalBufferAsString: Unknown buffer (%d).',[Ord(Buffer)]);
end;
end;

//------------------------------------------------------------------------------

Function TCRCCalculator.InternalBufferAsBitString(Buffer: TCRCInternalBuffer): String;
begin
case Buffer of
  bufPolynomial:  Result := CRCBufferAsBitString(fPolynomial);
  bufInit:        Result := CRCBufferAsBitString(fInitial);
  bufXorOut:      Result := CRCBufferAsBitString(fXorOut);
  bufCRC:         Result := CRCBufferAsBitString(fCRCValue);
  bufResidue:     Result := CRCBufferAsBitString(fResidue);
  bufProcessing:  Result := CRCBufferAsBitString(fProcBuffer);
  bufPolyTable0..
  bufPolyTable7:  Result := CRCBufferAsBitString(fPolyTable[ Ord(Buffer) - Ord(bufPolyTable0)]);
else
  raise ECRCInvalidValue.CreateFmt('TCRCCalculator.InternalBufferAsBitString: Unknown buffer (%d).',[Ord(Buffer)]);
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              TCRCCalculatorTester
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCRCCalculatorTester - internal auxiliary functions
===============================================================================}

Function StrInArrIndex(const Str: String; const Arr: array of String): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := Low(Arr) to High(Arr) do
  If AnsiSameText(Arr[i],Str) then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

procedure SplitSpecLine(const Line: String; LineParts: TStrings);
var
  Position:   TStrOff;
  PartStart:  TStrOff;
  PartLength: TStrSize;
begin
LineParts.Clear;
Position := 1;
PartStart := 1;
PartLength := 0;
while Position <= Length(Line) do
  begin
    If Ord(Line[Position]) in [Ord(' '),Ord('=')] then
      begin
        If PartLength > 0 then
          LineParts.Add(Copy(Line,PartStart,PartLength));
        PartStart := Position + 1;
        PartLength := 0;
      end
    else Inc(PartLength);
    Inc(Position);
  end;
If PartLength > 0 then
  LineParts.Add(Copy(Line,PartStart,PartLength));
end;

{===============================================================================
    TCRCCalculatorTester - public auxiliary functions
===============================================================================}

Function TestStageAsString(TestStage: TCRCTestStage): String;
begin
case TestStage of
  tstCheck:     Result := 'check';
  tstResidue:   Result := 'residue';
  tstCodeword:  Result := 'codeword';
else
  raise ECRCInvalidValue.CreateFmt('TestStageAsString: Unknown test stage (%d).',[Ord(TestStage)]);
end;
end;

{===============================================================================
    TCRCCalculatorTester - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCRCCalculatorTester - protected methods
-------------------------------------------------------------------------------}

procedure TCRCCalculatorTester.Initialize;
begin
inherited;
fOnTestEvent := nil;
fOnTestCallback := nil;
fCurrentPresetName := '';
fCurrentPresetIndex := -1;
end;

//------------------------------------------------------------------------------

procedure TCRCCalculatorTester.DoOnTest(Stage: TCRCTestStage; Index: Integer; Result: Boolean);
begin
If Assigned(fOnTestEvent) then
  fOnTestEvent(Self,fCurrentPresetName,Stage,Index,Result)
else If Assigned(fOnTestCallback) then
  fOnTestCallback(Self,fCurrentPresetName,Stage,Index,Result);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLinePreset(const Line: String; BreakOnFailure: Boolean): Boolean;
var
  LineParts:    TStringList;
  i:            Integer;
  TestCheck:    TCRCValue;
  TestResidue:  TCRCValue;
  StrBuff:      AnsiString;
  RefState:     Integer;
  TempBuffer:   TCRCBuffer;
begin
HashBits := 0;
LineParts := TStringList.Create;
try
  // parse the line
  SplitSpecLine(Line,LineParts);
  For i := 0 to Pred(LineParts.Count) do
    case StrInArrIndex(LineParts[i],
    ['width','poly','init','refin','refout','xorout','check','residue','name']) of
      0:  HashBits := StrToInt(LineParts[i + 1]);
      1:  Polynomial := CRCValueFromString(Copy(LineParts[i + 1],3,Length(LineParts[i + 1]) - 2),fHashBits + 1);
      2:  Initial := CRCValueFromString(Copy(LineParts[i + 1],3,Length(LineParts[i + 1]) - 2),fHashBits);
      3:  ReflectIn := StrToBool(LineParts[i + 1]);
      4:  ReflectOut := StrToBool(LineParts[i + 1]);
      5:  XorOut := CRCValueFromString(Copy(LineParts[i + 1],3,Length(LineParts[i + 1]) - 2),fHashBits);
      6:  TestCheck := CRCValueFromString(Copy(LineParts[i + 1],3,Length(LineParts[i + 1]) - 2),fHashBits);
      7:  TestResidue := CRCValueFromString(Copy(LineParts[i + 1],3,Length(LineParts[i + 1]) - 2),fHashBits);
      8:  fCurrentPresetName := Copy(LineParts[i + 1],2,Length(LineParts[i + 1]) - 2)
    end;
  fCurrentPresetIndex := 0;
  // correctness test
  HashAnsiString(AnsiString('123456789'));
  Result := CRCValueSame(CRC,TestCheck);
  DoOnTest(tstCheck,-1,Result);
  If not Result and BreakOnFailure then
    Exit;
  // residue test
  Init;
  StrBuff := AnsiString('123456789');
  Update(PAnsiChar(StrBuff)^,Length(StrBuff) * SizeOf(AnsiChar));
  // we need to prepare the CRC so we can feed it back
  RefState := 0;
  If ReflectIn then
    RefState := 2;
  If ReflectOut then
    Inc(RefState);
  case RefState of
    1:  begin // refin = false, refout = true
          CRCBufferConvertFromValue(TestCheck,Tempbuffer);
          CRCBufferReflectBits(TempBuffer,fHashBits);
          If (fHashBits and 7) <> 0 then
            TempBuffer[High(TempBuffer)] := TempBuffer[High(TempBuffer)] shr (8 - (fHashBits and 7));
          UpdateBits(TempBuffer[Low(TempBuffer)],fHashBits);
        end;
    2:  begin // refin = true, refout = false
          CRCBufferConvertFromValue(TestCheck,Tempbuffer);
          For i := Low(TempBuffer) to High(TempBuffer) do
            TempBuffer[i] := CRC_BYTE_REFLECTED[TempBuffer[i]];
          UpdateBits(TempBuffer[Low(TempBuffer)],fHashBits);
        end;
    3:  begin // refin = true, refout = true
          UpdateBits(TestCheck.Data[Low(TestCheck.Data)],fHashBits);
        end;
  else
   {0, refin = false, refout = false}
    If fHashBits > 8 then
      begin
        CRCBufferConvertFromValue(TestCheck,Tempbuffer);
        If (fHashBits and 7) <> 0 then
          TempBuffer[High(TempBuffer)] := TempBuffer[High(TempBuffer)] shr (8 - (fHashBits and 7));
      end
    else Tempbuffer := Copy(TestCheck.Data);
    UpdateBits(TempBuffer[Low(TempBuffer)],fHashBits);
  end;
  Final;
  Result := CRCValueSame(Residue,TestResidue);
  DoOnTest(tstResidue,-1,Result);
  If not Result and BreakOnFailure then
    Exit;
finally
  LineParts.Free;
end;
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodeword(Selector: Char; const Line: String): Boolean;
begin
case Selector of
  'h','H':  Result := SelfTestLineCodewordHex(Line);
  'o','O':  Result := SelfTestLineCodewordOct(Line);
  'b','B':  Result := SelfTestLineCodewordBit(Line);
  '0':      Result := SelfTestLineCodewordAuto_0(Line);
  '1':      Result := SelfTestLineCodewordAuto_1(Line);
  '2':      Result := SelfTestLineCodewordAuto_2(Line);
  '3':      Result := SelfTestLineCodewordAuto_3(Line);
  '4':      Result := SelfTestLineCodewordAuto_4(Line);
  '5':      Result := SelfTestLineCodewordAuto_5(Line);
else
  raise ECRCInvalidValue.CreateFmt('TCRCCalculatorTester.SelfTestLineCodeword: Unknown selector (%d).',[Selector]);
end;
DoOnTest(tstCodeword,fCurrentPresetIndex,Result);
Inc(fCurrentPresetIndex);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodewordHex(const Line: String): Boolean;
var
  BitsLeft:   Integer;
  StrPos:     Integer;
  Buffer:     UInt8;
  CRCValue:   TCRCValue;
begin
Init;
try
  BitsLeft := (Length(Line) * 4) - Integer(fHashBits);
  StrPos := 1;
  while BitsLeft > 8 do
    begin
      Buffer := UInt8(StrToInt('$' + Copy(Line,StrPos,2)));
      Update(Buffer,1);
      Inc(StrPos,2);
      Dec(BitsLeft,8);
    end;
  If BitsLeft > 0 then
    begin
      Buffer := UInt8(StrToInt('$' + Copy(Line,StrPos,2))) and
        UInt8(UInt8(-1) shl (8 - BitsLeft));
      UpdateBits(Buffer,BitsLeft);
    end;
finally
  Final;
end;
CRCValue := CRCValueFromString(Copy(Line,Length(Line) - Pred(Integer(HashSize) * 2),Integer(HashSize) * 2),fHashBits);
If ((fHashBits and 7) = 0) and fReflectOut then
  CRCBufferReflectBytes(CRCValue.Data);
Result := CRCValueSame(CRCValue,CRC);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodewordOct(const Line: String): Boolean;
var
  BitsLeft:   Integer;
  StrPos:     Integer;
  Buffer:     UInt8;
  CRCBuffer:  TCRCBuffer;
  CRCValue:   TCRCValue;
begin
Init;
try
  BitsLeft := (Length(Line) * 3) - Integer(fHashBits);
  StrPos := 1;
  while BitsLeft > 3 do
    begin
      Buffer := UInt8(StrToInt(Copy(Line,StrPos,1)));
      UpdateBits(Buffer,3);
      Inc(StrPos);
      Dec(BitsLeft,3);
    end;
  If BitsLeft > 0 then
    begin
      Buffer := UInt8(StrToInt(Copy(Line,StrPos,1))) shr (3 - BitsLeft);
      UpdateBits(Buffer,BitsLeft);
    end;
finally
  Final;
end;
CRCBuffer := nil;
SetLength(CRCBuffer,HashSize);
BitsLeft := Integer(fHashBits);
StrPos := Length(Line);
while BitsLeft > 3 do
  begin
    CRCBufferShiftBitsRight(CRCBuffer,3);
    Buffer := UInt8(StrToInt(Copy(Line,StrPos,1)));
    CRCBuffer[Low(CRCBuffer)] := CRCBuffer[Low(CRCBuffer)] or UInt8(Buffer shl 5);
    Dec(StrPos);
    Dec(BitsLeft,3);
  end;
If BitsLeft > 0 then
  begin
    CRCBufferShiftBitsRight(CRCBuffer,BitsLeft);
    Buffer := UInt8(StrToInt(Copy(Line,StrPos,1))) and (UInt8(-1) shr (8 - BitsLeft));
    CRCBuffer[Low(CRCBuffer)] := CRCBuffer[Low(CRCBuffer)] or UInt8(Buffer shl (8 - BitsLeft));
  end;
CRCBufferConvertToValue(CRCBuffer,fHashBits,CRCValue);
Result := CRCValueSame(CRCValue,CRC);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodewordBit(const Line: String): Boolean;
var
  i:          Integer;
  Buffer:     UInt8;
  CRCBuffer:  TCRCBuffer;
  CRCValue:   TCRCValue;
begin
Init;
try
  For i := 1 to (Length(Line) - Integer(fHashBits)) do
    begin
      If Line[i] = '1' then
        Buffer := 1
      else
        Buffer := 0;
      UpdateBits(Buffer,1);    
    end;
finally
  Final;
end;
// convert remaining bits to CRC
CRCBuffer := nil;
SetLength(CRCBuffer,HashSize);
For i := Length(Line) downto (Length(Line) - Pred(Integer(fHashBits)))  do
  begin
    CRCBufferShiftBitsRight(CRCBuffer,1);
    If Line[i] = '1' then
      CRCBuffer[Low(CRCBuffer)] := CRCBuffer[Low(CRCBuffer)] or UInt8($80);
  end;
If fReflectOut then
  CRCBufferReflectBits(CRCBuffer,fHashBits);
CRCBufferConvertToValue(CRCBuffer,fHashBits,CRCValue);
Result := CRCValueSame(CRCValue,CRC);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodewordAuto_0(const Line: String): Boolean;
var
  Buffer: array of UInt8;
  i:      Integer;
begin
Buffer := nil;
SetLength(Buffer,1999);
For i := Low(Buffer) to High(Buffer) do
  Buffer[i] := UInt8(i + i * i);
Init;
try
  Update(Buffer[Low(Buffer)],Length(Buffer));
finally
  Final;
end;
Result := CRCValueSame(CRCValueFromString(Line),CRC);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodewordAuto_1(const Line: String): Boolean;
var
  Buffer: array of UInt8;
  i:      Integer;
begin
Buffer := nil;
SetLength(Buffer,257);
Buffer[Low(Buffer)] := UInt8(StrToInt('$' + Copy(Line,1,2)));
Buffer[Low(Buffer) + 1] := UInt8(StrToInt('$' + Copy(Line,3,2)));
For i := (Low(Buffer) + 2) to High(Buffer) do
  Buffer[i] := Buffer[Low(Buffer) + 1];
Init;
try
  Update(Buffer[Low(Buffer)],Length(Buffer));
finally
  Final;
end;
Result := CRCValueSame(CRCValueFromString(Copy(Line,5,4)),CRC);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodewordAuto_2(const Line: String): Boolean;
var
  Buffer: array of UInt8;
  i:      Integer;
begin
Buffer := nil;
SetLength(Buffer,256);
For i := Low(Buffer) to High(Buffer) do
  Buffer[i] := UInt8(i);
Init;
try
  Update(Buffer[Low(Buffer)],Length(Buffer));
finally
  Final;
end;
Result := CRCValueSame(CRCValueFromString(Line),CRC);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodewordAuto_3(const Line: String): Boolean;
var
  Buffer:   array of UInt8;
  i:        Integer;
  CRCValue: TCRCValue;
begin
Buffer := nil;
SetLength(Buffer,4096);
Buffer[Low(Buffer)] := UInt8(StrToInt('$' + Copy(Line,1,2)));
For i := Succ(Low(Buffer)) to High(Buffer) do
  Buffer[i] := Buffer[Low(Buffer)];
Init;
try
  Update(Buffer[Low(Buffer)],Length(Buffer));
finally
  Final;
end;
CRCValue := CRCValueFromString(Copy(Line,3,16));
If fReflectOut then
  CRCBufferReflectBytes(CRCValue.Data);
Result := CRCValueSame(CRCValue,CRC);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodewordAuto_4(const Line: String): Boolean;
var
  Buffer:   array of UInt8;
  i:        Integer;
  CRCValue: TCRCValue;
begin
Buffer := nil;
SetLength(Buffer,4096);
For i := Low(Buffer) to High(Buffer) do
  Buffer[i] := UInt8(i);
Init;
try
  Update(Buffer[Low(Buffer)],Length(Buffer));
finally
  Final;
end;
CRCValue := CRCValueFromString(Line);
If fReflectOut then
  CRCBufferReflectBytes(CRCValue.Data);
Result := CRCValueSame(CRCValue,CRC);
end;

//------------------------------------------------------------------------------

Function TCRCCalculatorTester.SelfTestLineCodewordAuto_5(const Line: String): Boolean;
var
  Buffer:   array of UInt8;
  i:        Integer;
  CRCValue: TCRCValue;
begin
Buffer := nil;
SetLength(Buffer,4096);
For i := Low(Buffer) to High(Buffer) do
  Buffer[i] := UInt8(4095 - i);
Init;
try
  Update(Buffer[Low(Buffer)],Length(Buffer));
finally
  Final;
end;
CRCValue := CRCValueFromString(Line);
If fReflectOut then
  CRCBufferReflectBytes(CRCValue.Data);
Result := CRCValueSame(CRCValue,CRC);
end;

{-------------------------------------------------------------------------------
    TCRCCalculatorTester - public methods
-------------------------------------------------------------------------------}

Function TCRCCalculatorTester.SelfTest(const FileName: String; BreakOnFailure: Boolean = True): Boolean;
var
  FileLines:    TStringList;
  i:            Integer;
  TempStr:      String;
  LineResult:   Boolean;
  IgnoreBlock:  Boolean;
begin
Result := True;
LineResult := False;
IgnoreBlock := False;
FileLines := TStringList.Create;
try
  FileLines.LoadFromFile(StrToRTL(FileName));
  For i := 0 to Pred(FileLines.Count) do
    begin
      TempStr := Trim(FileLines[i]);
      If Length(TempStr) > 0 then
        begin
          If TempStr[1] = '}' then
            begin
              IgnoreBlock := False;
              Continue;
            end
          else If IgnoreBlock then
            Continue;
          case TempStr[1] of
            ';':      Continue;
            '{':      begin
                        IgnoreBlock := True;
                        Continue;  
                      end;
            'w','W':  LineResult := SelfTestLinePreset(TempStr,BreakOnFailure);
            '§':      If Length(TempStr) >= 2 then
                        LineResult := SelfTestLineCodeword(TempStr[2],Copy(TempStr,3,Length(TempStr) - 2))
                      else
                        raise ECRCInvalidValue.CreateFmt('TCRCCalculatorTester.SelfTest: Line is too short (%d).',[Length(TempStr)]);
          else
            LineResult := SelfTestLineCodeword('H',TempStr);
          end;
          If not LineResult then
            begin
              Result := False;
              If BreakOnFailure then
                Break{For i};
            end;
      end;
    end;
finally
  FileLines.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TCRCCalculatorTester.SelfTest(BreakOnFailure: Boolean = True): Boolean;
begin
Result := SelfTest(ExpandFileName('.' + PathDelim + CRCCALC_TESTFILE),BreakOnFailure);
end;

end.
