{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Binary data to text encoding and decoding

    - Base16, Base32(Hex) and Base64(URL) encodings should be compliant with
      RFC 4648
    - Base85 encoding is using Z85 alphabet with undescore ("_", #95) as an
      all-zero compression character
    - Base16 corresponds to usual hexadecimal encoding
    - Base10 only encodes to three-digit decimal number representing given byte
    - when using Base85 and ASCII85, the decoded data migh end or start (in
      case of reversed data reading) with zeroes that were not part of the
      original data (a result of these encodings being block-based, these
      zeroes are block padding)

  Version 2.1.2 (2023-04-17)

  Last change 2023-04-17

  ©2015-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.BinTextEnc

  Dependencies:
    AuxTypes   - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses - github.com/TheLazyTomcat/Lib.AuxClasses
    StrRect    - github.com/TheLazyTomcat/Lib.StrRect

===============================================================================}
unit BinTextEnc;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

{$IFOPT Q+}
  {$DEFINE OverflowChecks}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EBTEException = class(Exception);

  EBTEInvalidValue = class(EBTEException);
  EBTEInvalidState = class(EBTEException);

  EBTETooMuchData    = class(EBTEException);
  EBTEBufferTooSmall = class(EBTEException);

  EBTEProcessingError = class(EBTEException);

{===============================================================================
--------------------------------------------------------------------------------
                                 TBTETranscoder
--------------------------------------------------------------------------------
===============================================================================}
type
  TBTEEncoding = (encUnknown,encBase2,encBase4,encBase8,encBase10,encBase16,
                  encBase32,encBase32Hex,encBase64,encBase64URL,encBase85,
                  encASCII85);

{
  efReversible            encoding supports reversed data reading
  efPadding               encoding supports padding
  efCompression           encoding supports compression
  efTrim                  encoding supports trimming
  efOutputSize            it is possible to obtain length of encoded string or
                          size of decoded data
  efSlowOutputSize        same as efOutputSize, but the data/string must be
                          scanned to obtain the size, this can be slow
  efInpreciseOutputSize   reported encoded length or decoded size can be
                          inprecise (note that it will never be smaller), it is
                          usually rounded to some boundary (eg. block)
}
  TBTEEncodingFeature = (efReversible,efPadding,efCompression,efTrim,
                         efOutputSize,efSlowOutputSize,efInpreciseOutputSize);

  TBTEEncodingFeatures = set of TBTEEncodingFeature;

  TBTEEncodingTable = array of Char;

  TBTEDecodingTable = array[0..127] of Byte;

{===============================================================================
    TBTETranscoder - class declaration
===============================================================================}
type
  TBTETranscoder = class(TCustomObject)
  protected
    fEncodedString:       String;
    fIsProcessing:        Boolean;
    fBreakProcessing:     Boolean;
    // encoding settings
    fHeader:              Boolean;
    fReversed:            Boolean;
    fPadding:             Boolean;
    fPaddingChar:         Char;
    fCompression:         Boolean;
    fCompressionChar:     Char;
    fTrim:                Boolean;
    // processing variables
    fEncodedStringPos:    TStrOff;
    // events
    fProgressCoef:        Integer;
    fOnProgressEvent:     TFloatEvent;
    fOnProgressCallback:  TFloatCallback;
    // getters/setters
    Function GetEncodedString: String; virtual;
    procedure SetEncodedString(const Value: String); virtual;
    procedure SetPadding(Value: Boolean); virtual;
    procedure SetReversed(Value: Boolean); virtual;
    procedure SetPaddingChar(Value: Char); virtual;
    procedure SetCompression(Value: Boolean); virtual;
    procedure SetCompressionChar(Value: Char); virtual;
    procedure SetTrim(Value: Boolean); virtual;
    procedure SetHeader(Value: Boolean); virtual;
    // events firing
    procedure DoProgress(Progress: Double); virtual;
    // initialization, finalization
    procedure InitializeTable; virtual; abstract;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    // header methods
    class Function HeaderNumberExtract(const EncodedString: String; out HeaderNumber: UInt16): Boolean; virtual;
    // auxiliary
    Function ResolveDataPointer(Ptr: Pointer; Size: TMemSize; RevOffset: UInt32 = 1): Pointer; virtual;
    procedure AdvanceDataPointer(var Ptr: Pointer; Delta: TMemSize = 1); virtual;
  public
  {
    Use following to build encoding table type from other types.
  }
    class Function CreateEncodingTable(const Source: array of Char): TBTEEncodingTable; overload; virtual;
    class Function CreateEncodingTable(const Source: String): TBTEEncodingTable; overload; virtual;
  {
    DataLimit returns maximum number of bytes that can be processed by a
    particular encoder.
    If you try to encode or even get encoded length for more data, an
    EBTETooMuchData exception will be raised.

    This limit is here to prevent potential out-of-memory errors and severe
    rounding errors in some calculations when encoding.

    Note that this limit changes for each encoding, and is selected so that
    encoded string can reach a maximum of about 512M characters in length.
  }
    class Function DataLimit: TMemSize; virtual; abstract;
  {
    CharLimit sets limit on how long a string can be assigned to the property
    EncodedString. If you try to assign longer string, an EBTETooMuchData
    exception will be raised.

    Currently it is always 512M characters.
  }
    class Function CharLimit: TStrSize; virtual;
  {
    Encoding returns what encoding the class is providing.
  }
    class Function Encoding: TBTEEncoding; virtual; abstract;
  {
    EncodingFeatures returns set of fetures the particular encoder or decoder
    is supporting.

    To test for support of specific feature, use the "in" set operator.
    If the feature is supported, then the appropriate enumeration values will
    be included, otherwise the value will not be included in the returned set.

    See decription of type TBTEEncoding for detailed description of indvidual
    features.
  }
    class Function EncodingFeatures: TBTEEncodingFeatures; virtual; abstract;
  {
    EncodingTableLength returns strict length of the encoding table (sometimes
    also termed "alphabet").
  }
    class Function EncodingTableLength: Integer; virtual; abstract;
  {
    HeaderLength returns length, in characters, of the header that can be
    stored at the beginning of encoded strings.

    It will always return 6.
  }
    class Function HeaderLength: TStrSize; virtual;
  {
    HeaderPresent returns true when EncodedString starts with a substring that
    can be an encoded string header. False otherwise.

    When CheckEncoding is set to true, and the string contains a pattern
    compatible with header, the function will also check whether the header
    contains a valid encoding number or not.
  }
    class Function HeaderPresent(const EncodedString: String; CheckEncoding: Boolean = False): Boolean; virtual;
  {
    HeaderEncoding reads header from provided encoded string and returns which
    encoding is stored in this header.

    If the string does not contain a valid header or the stored encoding is not
    known/valid, it will return encUnknown.
  }
    class Function HeaderEncoding(const EncodedString: String): TBTEEncoding; virtual;
  {
    HeaderReversed reads header from provided encoded string and returns whether
    the encoded string was constructed with reversed data read (see description
    of Reversed property for details) or not.
  }
    class Function HeaderReversed(const EncodedString: String): Boolean; virtual;
  {
    EncodingTableIsValid checs validity of passed encoding table.

    It checks length of the table, ordinal values of all characters (must be
    below or equal to 127), and that no character repeats. Note that some
    encodings might require stricter rules, they are then checked too.
  }
    class Function EncodingTableIsValid(const EncodingTable: TBTEEncodingTable): Boolean; virtual;
    constructor Create;
    destructor Destroy; override;
    Function BreakProcessing: Boolean; virtual;
    // properties
    property EncodedString: String read GetEncodedString write SetEncodedString;
    property IsProcessing: Boolean read fIsProcessing;
  {
    Encoder

      When set to true, the encoded string will start with a header sequence
      that stores used encoding scheme and some other encoding options (namely
      reversed flag).

    Decoder

      Read-only property (can be set to a different value, but that is of no
      use).
      Value of this property is set when decoding or when obtaining decoded
      size. It is set to true when the encoded string contained a valid header,
      false otherwise.
  }
    property Header: Boolean read fHeader write SetHeader;
  {
    Encoder

      When set to true, then the data being encoded are read in reverse (from
      the last byte/block to the first). When false, reading is done in normal
      order (from first byte/block to last).

    Decoder

      If the encoded string contains a valid header, then this value is set
      according to a reverse flag in this header (true when the flag is set,
      false otherwise).
      If you use encoded string that does NOT contain a header, then you must
      set this property to a value used during encoding, otherwise the decoded
      data will be completely wrong.
  }
    property Reversed: Boolean read fReversed write SetReversed;
  {
    Encoder

      When set to true, and when the encoding supports padding, then the
      produced encoded string will be padded (missing characters at the end
      replaced by PaddingCharacter) to certain length.

    Decoder

      Read-only property.
      Value is set when decoding or when obtaining decoded size. It is set to
      true when the encoded string ends with one or more PaddingCharacter,
      false otherwise.
      Note that this does not reflect whether padding was active during
      encoding, only whether there is padding character(s) or not.
  }
    property Padding: Boolean read fPadding write SetPadding;
    property PaddingCharacter: Char read fPaddingChar write SetPaddingChar;
  {
    Encoder

      When set to true, and when the encoding supports compression, then some
      of the encoded data can be compressed when encoding.

    Decoder

      Read-only property.
      Value is set when decoding or when obtaining decoded size. It is set to
      true when the encoded string contains one or more CompressionCharacter,
      false otherwise.
      Note that this does not reflect whether compression was active during
      encoding, only whether there is compression character(s) or not.
  }
    property Compression: Boolean read fCompression write SetCompression;
    property CompressionCharacter: Char read fCompressionChar write SetCompressionChar;
  {
    Encoder

      When set to true, and when the encoding supports trimming, then some
      the resulting encoded string might be shortened by characters that are
      not carrying any information about the encoded data.

    Decoder

      Read-only property.
      Value is set when decoding or when obtaining decoded size. It is set to
      true when the encoded string seems to be trimmed (is shorter than is
      expected), false otherwise.
      Note that this does not reflect whether trimming was active during
      encoding, only whether there are some missing characters.
  }
    property Trim: Boolean read fTrim write SetTrim;
    property ProgressCoefficient: Integer read fProgressCoef write fProgressCoef;
    property OnProgressEvent: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
    property OnProgressCallback: TFloatCallback read fOnProgressCallback write fOnProgressCallback;
    property OnProgress: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  TBTEEncoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBTEEncoder - class declaration
===============================================================================}
type
  TBTEEncoder = class(TBTETranscoder)
  protected
    fEncodingTable: TBTEEncodingTable;
    Function GetEncodingTable: TBTEEncodingTable; virtual;
    procedure SetEncodingTable(const Value: TBTEEncodingTable); virtual;
    procedure AssignEncodingTable(const EncodingTable: TBTEEncodingTable); virtual;
    // processing
    procedure WriteHeader; virtual;
    procedure WriteChar(C: Char); virtual;
    procedure WritePadding; virtual;
    procedure Encode(const Buffer; Size: TMemSize); virtual;
    procedure EncodeSpecific(const Buffer; Size: TMemSize); virtual; abstract;
  public
    // resulting string length
    Function EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize; virtual; abstract;
    Function EncodedLengthFromMemory(Mem: Pointer; Size: TMemSize): TStrSize; virtual;
    Function EncodedLengthFromStream(Stream: TStream; Count: Int64 = -1): TStrSize; virtual;
    Function EncodedLengthFromFile(const FileName: String): TStrSize; virtual;
    // processing
    procedure EncodeFromBuffer(const Buffer; Size: TMemSize); virtual;
    procedure EncodeFromMemory(Mem: Pointer; Size: TMemSize); virtual;
    procedure EncodeFromStream(Stream: TStream; Count: Int64 = -1); virtual;
    procedure EncodeFromFile(const FileName: String); virtual;
    property EncodingTable: TBTEEncodingTable read GetEncodingTable write SetEncodingTable;
  end;

  TBTEEncoderClass = class of TBTEEncoder;

{===============================================================================
--------------------------------------------------------------------------------
                                  TBTEDecoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBTEDecoder - class declaration
===============================================================================}
type
  TBTEDecoder = class(TBTETranscoder)
  protected
    fDecodingTable: TBTEDecodingTable;
    // processing
    procedure ReadHeader; virtual;  // sets properties Header and Reversed (if header is present)
    Function ReadChar: Char; virtual;
    procedure RollBack; virtual;
    Function ResolveChar(C: Char): Byte; virtual;
    Function CountPadding: TStrSize; virtual;
    procedure Decode(out Buffer; Size: TMemSize); virtual;
    procedure DecodeSpecific(out Buffer; Size: TMemSize); virtual; abstract;
  public
    procedure ConstructDecodingTable(const EncodingTable: TBTEEncodingTable); virtual;
    // resulting data size
    Function DecodedSize: TMemSize; virtual; abstract;  // sets properties Padding, Compression and Trim
    //processing
    Function DecodeIntoBuffer(out Buffer; Size: TMemSize): TMemSize; virtual;
    Function DecodeIntoMemory(Mem: Pointer; Size: TMemSize): TMemSize; overload; virtual;
    Function DecodeIntoMemory(out Mem: Pointer): TMemSize; overload; virtual;
    Function DecodeIntoStream(Stream: TStream): TMemSize; virtual;
    Function DecodeIntoFile(const FileName: String): TMemSize; virtual;
    property DecodingTable: TBTEDecodingTable read fDecodingTable write fDecodingTable;
  end;

  TBTEDecoderClass = class of TBTEDecoder;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase2Encoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase2Encoder - class declaration
===============================================================================}
type
  TBase2Encoder = class(TBTEEncoder)
  protected
    procedure InitializeTable; override;
    procedure EncodeSpecific(const Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase2Decoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase2Decoder - class declaration
===============================================================================}
type
  TBase2Decoder = class(TBTEDecoder)
  protected
    procedure InitializeTable; override;
    procedure DecodeSpecific(out Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function DecodedSize: TMemSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase4Encoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase4Encoder - class declaration
===============================================================================}
type
  TBase4Encoder = class(TBTEEncoder)
  protected
    procedure InitializeTable; override;
    procedure EncodeSpecific(const Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase4Decoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase4Decoder - class declaration
===============================================================================}
type
  TBase4Decoder = class(TBTEDecoder)
  protected
    procedure InitializeTable; override;
    procedure DecodeSpecific(out Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function DecodedSize: TMemSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase8Encoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase8Encoder - class declaration
===============================================================================}
type
  TBase8Encoder = class(TBTEEncoder)
  protected
    procedure InitializeTable; override;
    procedure EncodeSpecific(const Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase8Decoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase8Decoder - class declaration
===============================================================================} 
type
  TBase8Decoder = class(TBTEDecoder)
  protected
    procedure InitializeTable; override;
    procedure DecodeSpecific(out Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function DecodedSize: TMemSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase10Encoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase10Encoder - class declaration
===============================================================================}
type
  TBase10Encoder = class(TBTEEncoder)
  protected
    procedure InitializeTable; override;
    procedure EncodeSpecific(const Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase10Decoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase10Decoder - class declaration
===============================================================================}
type
  TBase10Decoder = class(TBTEDecoder)
  protected
    procedure InitializeTable; override;
    procedure DecodeSpecific(out Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function DecodedSize: TMemSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase16Encoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase16Encoder - class declaration
===============================================================================}
type
  TBase16Encoder = class(TBTEEncoder)
  protected
    procedure InitializeTable; override;
    procedure EncodeSpecific(const Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase16Decoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase16Decoder - class declaration
===============================================================================}
type
  TBase16Decoder = class(TBTEDecoder)
  protected
    procedure InitializeTable; override;
    procedure DecodeSpecific(out Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function DecodedSize: TMemSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase32Encoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase32Encoder - class declaration
===============================================================================}
type
  TBase32Encoder = class(TBTEEncoder)
  protected
    procedure InitializeTable; override;
    procedure EncodeSpecific(const Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase32Decoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase32Decoder - class declaration
===============================================================================}
type
  TBase32Decoder = class(TBTEDecoder)
  protected
    procedure InitializeTable; override;
    procedure DecodeSpecific(out Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function DecodedSize: TMemSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TBase32HexEncoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase32HexEncoder - class declaration
===============================================================================}
type
  TBase32HexEncoder = class(TBase32Encoder)
  protected
    procedure InitializeTable; override;
  public
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingTableLength: Integer; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TBase32HexDecoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase32HexDecoder - class declaration
===============================================================================}
type
  TBase32HexDecoder = class(TBase32Decoder)
  protected
    procedure InitializeTable; override;
  public
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingTableLength: Integer; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase64Encoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase64Encoder - class declaration
===============================================================================}
type
  TBase64Encoder = class(TBTEEncoder)
  protected
    procedure InitializeTable; override;
    procedure EncodeSpecific(const Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase64Decoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase64Decoder - class declaration
===============================================================================}
type
  TBase64Decoder = class(TBTEDecoder)
  protected
    procedure InitializeTable; override;
    procedure DecodeSpecific(out Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    Function DecodedSize: TMemSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TBase64URLEncoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase64URLEncoder - class declaration
===============================================================================}
type
  TBase64URLEncoder = class(TBase64Encoder)
  protected
    procedure InitializeTable; override;
  public
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingTableLength: Integer; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TBase64URLDecoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase64URLDecoder - class declaration
===============================================================================}
type
  TBase64URLDecoder = class(TBase64Decoder)
  protected
    procedure InitializeTable; override;
  public
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingTableLength: Integer; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase85Encoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase85Encoder - class declaration
===============================================================================}
type
  TBase85Encoder = class(TBTEEncoder)
  protected
    procedure InitializeTable; override;
    procedure EncodeSpecific(const Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    class Function EncodingTableIsValid(const EncodingTable: TBTEEncodingTable): Boolean; override;
    Function EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase85Decoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBase85Decoder - class declaration
===============================================================================}
type
  TBase85Decoder = class(TBTEDecoder)
  protected
    Function ReadChar: Char; override;
    procedure InitializeTable; override;
    procedure DecodeSpecific(out Buffer; Size: TMemSize); override;
  public
    class Function DataLimit: TMemSize; override;
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingFeatures: TBTEEncodingFeatures; override;
    class Function EncodingTableLength: Integer; override;
    class Function EncodingTableIsValid(const EncodingTable: TBTEEncodingTable): Boolean; override;
    Function DecodedSize: TMemSize; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TASCII85Encoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TASCII85Encoder - class declaration
===============================================================================}
type
  TASCII85Encoder = class(TBase85Encoder)
  protected
    procedure InitializeTable; override;
  public
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingTableLength: Integer; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TASCII85Decoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TASCII85Decoder - class declaration
===============================================================================}
type
  TASCII85Decoder = class(TBase85Decoder)
  protected
    procedure InitializeTable; override;
  public
    class Function Encoding: TBTEEncoding; override;
    class Function EncodingTableLength: Integer; override;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - specialized functions declaration
===============================================================================}

Function EncodedLength_Base2(const Buffer; Size: TMemSize; Header: Boolean = False): TStrSize;
Function Encode_Base2(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False): String;

Function DecodedSize_Base2(const EncodedString: String): TMemSize;
Function Decode_Base2(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_Base4(const Buffer; Size: TMemSize; Header: Boolean = False): TStrSize;
Function Encode_Base4(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False): String;

Function DecodedSize_Base4(const EncodedString: String): TMemSize;
Function Decode_Base4(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_Base8(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
Function Encode_Base8(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;

Function DecodedSize_Base8(const EncodedString: String): TMemSize;
Function Decode_Base8(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_Base10(const Buffer; Size: TMemSize; Header: Boolean = False): TStrSize;
Function Encode_Base10(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False): String;

Function DecodedSize_Base10(const EncodedString: String): TMemSize;
Function Decode_Base10(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_Base16(const Buffer; Size: TMemSize; Header: Boolean = False): TStrSize;
Function Encode_Base16(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False): String;

Function DecodedSize_Base16(const EncodedString: String): TMemSize;
Function Decode_Base16(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_Base32(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
Function Encode_Base32(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;

Function DecodedSize_Base32(const EncodedString: String): TMemSize;
Function Decode_Base32(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_Base32Hex(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
Function Encode_Base32Hex(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;

Function DecodedSize_Base32Hex(const EncodedString: String): TMemSize;
Function Decode_Base32Hex(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_Base64(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
Function Encode_Base64(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;

Function DecodedSize_Base64(const EncodedString: String): TMemSize;
Function Decode_Base64(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_Base64URL(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
Function Encode_Base64URL(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;

Function DecodedSize_Base64URL(const EncodedString: String): TMemSize;
Function Decode_Base64URL(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_Base85(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): TStrSize;
Function Encode_Base85(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): String;

Function DecodedSize_Base85(const EncodedString: String): TMemSize;
Function Decode_Base85(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

//------------------------------------------------------------------------------

Function EncodedLength_ASCII85(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): TStrSize;
Function Encode_ASCII85(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): String;

Function DecodedSize_ASCII85(const EncodedString: String): TMemSize;
Function Decode_ASCII85(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;

{===============================================================================
    Procedural interface - universal functions declaration
===============================================================================}

Function EncodedLength(Encoding: TBTEEncoding; const Buffer; Size: TMemSize; Header: Boolean = False;
  Reversed: Boolean = False; Padding: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): TStrSize;
Function Encode(Encoding: TBTEEncoding; const Buffer; Size: TMemSize; Header: Boolean = False;
  Reversed: Boolean = False; Padding: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): String;

Function DecodedSize(Encoding: TBTEEncoding; const EncodedString: String): TMemSize; overload;
{
  Buffer must be already allocated to accommodate all data (pass allocated size
  in Size parameter). To get the required size, use function DecodedSize.
}
Function Decode(Encoding: TBTEEncoding; const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize; overload;

//------------------------------------------------------------------------------

// retuns encUnknown if the string does not contain a valid header
Function HeaderEncoding(const EncodedString: String): TBTEEncoding;

Function DecodedSize(const EncodedString: String): TMemSize; overload;
Function Decode(const EncodedString: String; out Buffer; Size: TMemSize): TMemSize; overload;

//------------------------------------------------------------------------------

Function EncoderFromEncoding(Encoding: TBTEEncoding): TBTEEncoderClass;
Function DecoderFromEncoding(Encoding: TBTEEncoding): TBTEDecoderClass;

implementation

uses
  Math,
  StrRect;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W4056:={$WARN 4056 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
    Implementation constants and functions
===============================================================================}
// constants for encoded string headers
const
  BTE_HEADER_ENCODING_BASE2   = 1;
  BTE_HEADER_ENCODING_BASE4   = 2;
  BTE_HEADER_ENCODING_BASE8   = 3;
  BTE_HEADER_ENCODING_BASE10  = 4;
  BTE_HEADER_ENCODING_BASE16  = 5;
  BTE_HEADER_ENCODING_BASE32  = 6;
  BTE_HEADER_ENCODING_BASE32H = 7;
  BTE_HEADER_ENCODING_BASE64  = 8;
  BTE_HEADER_ENCODING_BASE85  = 9;
  BTE_HEADER_ENCODING_BASE85A = 10; 
  // newly added...
  BTE_HEADER_ENCODING_BASE64U = 11;

  BTE_HEADER_ENCODING_MASK = $00FF;

  BTE_HEADER_FLAG_REVERSED = $0100;

  BTE_CHAR_MASK = $7F;

//------------------------------------------------------------------------------  

Function CharInSet(C: Char; S: TSysCharSet): Boolean;
begin
{$IF SizeOf(Char) <> 1}
If Ord(C) > 255 then
  Result := False
else
{$IFEND}
  Result := AnsiChar(C) in S;
end;

//------------------------------------------------------------------------------

procedure SwapByteOrder(var Value: UInt32);
begin
Value := ((Value shr 24) and $FF) or ((Value shr 8) and $FF00) or
         ((Value and $FF00) shl 8) or ((Value and $FF) shl 24);
end;

//------------------------------------------------------------------------------

Function EncodingToNumber(Encoding: TBTEEncoding): UInt16;
begin
case Encoding of
  encBase2:       Result := BTE_HEADER_ENCODING_BASE2;
  encBase4:       Result := BTE_HEADER_ENCODING_BASE4;
  encBase8:       Result := BTE_HEADER_ENCODING_BASE8;
  encBase10:      Result := BTE_HEADER_ENCODING_BASE10;
  encBase16:      Result := BTE_HEADER_ENCODING_BASE16;
  encBase32:      Result := BTE_HEADER_ENCODING_BASE32;
  encBase32Hex:   Result := BTE_HEADER_ENCODING_BASE32H;
  encBase64:      Result := BTE_HEADER_ENCODING_BASE64;
  encBase64URL:   Result := BTE_HEADER_ENCODING_BASE64U;
  encBase85:      Result := BTE_HEADER_ENCODING_BASE85;
  encAscii85:     Result := BTE_HEADER_ENCODING_BASE85A;
else
  raise EBTEInvalidValue.CreateFmt('EncodingToNumber: Invalid encoding (%d).',[Ord(Encoding)]);
end;
end;

//------------------------------------------------------------------------------

Function NumberToEncoding(Number: UInt16): TBTEEncoding;
begin
case Number and BTE_HEADER_ENCODING_MASK of
  BTE_HEADER_ENCODING_BASE2:    Result := encBase2;
  BTE_HEADER_ENCODING_BASE4:    Result := encBase4;
  BTE_HEADER_ENCODING_BASE8:    Result := encBase8;
  BTE_HEADER_ENCODING_BASE10:   Result := encBase10;
  BTE_HEADER_ENCODING_BASE16:   Result := encBase16;
  BTE_HEADER_ENCODING_BASE32:   Result := encBase32;
  BTE_HEADER_ENCODING_BASE32H:  Result := encBase32Hex;
  BTE_HEADER_ENCODING_BASE64:   Result := encBase64;
  BTE_HEADER_ENCODING_BASE64U:  Result := encBase64URL;
  BTE_HEADER_ENCODING_BASE85:   Result := encBase85;
  BTE_HEADER_ENCODING_BASE85A:  Result := encAscii85;
else
  Result := encUnknown;
end
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBTETranscoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBTETranscoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBTETranscoder - protected methods
-------------------------------------------------------------------------------}

Function TBTETranscoder.GetEncodedString: String;
begin
If not fIsProcessing then
  Result := fEncodedString
else
  raise EBTEInvalidState.Create('TBTETranscoder.GetEncodedString: Cannot get encoded string, transcoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.SetEncodedString(const Value: String);
begin
If not fIsProcessing then
  begin
    If Length(Value) <= CharLimit then
      fEncodedString := Value
    else
      raise EBTETooMuchData.CreateFmt('TBTETranscoder.SetEncodedString: Encoded string too long (%d characters).',[Length(Value)]);
  end
else raise EBTEInvalidState.Create('TBTETranscoder.SetEncodedString: Cannot set encoded string, transcoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.SetHeader(Value: Boolean);
begin
If not fIsProcessing then
  fHeader := Value
else
  raise EBTEInvalidState.Create('TBTETranscoder.SetHeader: Cannot change settings, transcoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.SetReversed(Value: Boolean);
begin
If not fIsProcessing then
  begin
    If efReversible in EncodingFeatures then
      fReversed := Value
    else
      fReversed := False;
  end
else raise EBTEInvalidState.Create('TBTETranscoder.SetReversed: Cannot change settings, transcoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.SetPadding(Value: Boolean);
begin
If not fIsProcessing then
  begin
    If efPadding in EncodingFeatures then
      fPadding := Value
    else
      fPadding := False;
  end
else raise EBTEInvalidState.Create('TBTETranscoder.SetPadding: Cannot change settings, transcoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.SetPaddingChar(Value: Char);
begin
If not fIsProcessing then
  begin
    If efPadding in EncodingFeatures then
      fPaddingChar := Value
    else
      fPaddingChar := #0;
  end
else raise EBTEInvalidState.Create('TBTETranscoder.SetPaddingChar: Cannot change settings, transcoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.SetCompression(Value: Boolean);
begin
If not fIsProcessing then
  begin
    If efCompression in EncodingFeatures then
      fCompression := Value
    else
      fCompression := False;
  end
else raise EBTEInvalidState.Create('TBTETranscoder.SetCompression: Cannot change settings, transcoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.SetCompressionChar(Value: Char);
begin
If not fIsProcessing then
  begin
    If efCompression in EncodingFeatures then
      fCompressionChar := Value
    else
      fCompressionChar := #0;
  end
else raise EBTEInvalidState.Create('TBTETranscoder.SetCompressionChar: Cannot change settings, transcoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.SetTrim(Value: Boolean);
begin
If not fIsProcessing then
  begin
    If efTrim in EncodingFeatures then
      fTrim := Value
    else
      fTrim := False;
  end
else raise EBTEInvalidState.Create('TBTETranscoder.SetTrim: Cannot change settings, transcoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.DoProgress(Progress: Double);
begin
If Assigned(fOnProgressEvent) then
  fOnProgressEvent(Self,Progress)
else If Assigned(fOnProgressCallback) then
  fOnProgressCallback(Self,Progress);
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.Initialize;
begin
fEncodedString := '';
fIsProcessing := False;
fBreakProcessing := False;
fHeader := False;
fReversed := False;
fPadding := False;
fPaddingChar := #0;
fCompression := False;
fCompressionChar := #0;
fTrim := False;
fEncodedStringPos := 0;
fProgressCoef := 1024;
fOnProgressEvent := nil;
fOnProgressCallback := nil;
InitializeTable;
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.Finalize;
begin
fOnProgressEvent := nil;
fOnProgressCallback := nil;
end;

//------------------------------------------------------------------------------

class Function TBTETranscoder.HeaderNumberExtract(const EncodedString: String; out HeaderNumber: UInt16): Boolean;
var
  Temp: Integer;
begin
{
  Header number format

    bits          meaning
  -------------------------------------
     0.. 6        encoding number
         7        reserved
         8        reversed data flag
     9..15        reserved

  BTE_HEADER_FLAG_REVERSED   = $0100;
}
If HeaderPresent(EncodedString) then
  begin
    If TryStrToInt('$' + Copy(EncodedString,2,4),Temp) then
      begin
        HeaderNumber := UInt16(Temp);
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

{$IFDEF OverflowChecks}{$Q-}{$ENDIF}

Function TBTETranscoder.ResolveDataPointer(Ptr: Pointer; Size: TMemSize; RevOffset: UInt32 = 1): Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
If fReversed then
  Result := Pointer(PtrUInt(Ptr) + (PtrUInt(Size) - PtrUInt(RevOffset)))
else
  Result := Ptr;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TBTETranscoder.AdvanceDataPointer(var Ptr: Pointer; Delta: TMemSize = 1);
begin
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
If fReversed then
  Ptr := Pointer(PtrUInt(Ptr) - Delta)
else
  Ptr := Pointer(PtrUInt(Ptr) + Delta);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{$IFDEF OverflowChecks}{$Q+}{$ENDIF}

{-------------------------------------------------------------------------------
    TBTETranscoder - public methods
-------------------------------------------------------------------------------}

class Function TBTETranscoder.CreateEncodingTable(const Source: array of Char): TBTEEncodingTable;
var
  i:  Integer;
begin
SetLength(Result,Length(Source));
For i := Low(Result) to High(Result) do
  Result[i] := Source[i];
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

class Function TBTETranscoder.CreateEncodingTable(const Source: String): TBTEEncodingTable;
var
  i:  Integer;
begin
SetLength(Result,Length(Source));
For i := Low(Result) to High(Result) do
  Result[i] := Source[i + 1];
end;

//------------------------------------------------------------------------------

class Function TBTETranscoder.CharLimit: TStrSize;
begin
Result := 512 * 1024 * 1024;  // about 1GiB for unicode/wide string
end;

//------------------------------------------------------------------------------

class Function TBTETranscoder.HeaderLength: TStrSize;
begin
{
  Header has the following format:

    #xxxx~

  ... where xxxx is a four-digit hexadecimal number containing encoding type
  and some other settings. It is directly followed by the encoded string.

  This means the header has always the same length.
}
Result := 6;
end;

//------------------------------------------------------------------------------

class Function TBTETranscoder.HeaderPresent(const EncodedString: String; CheckEncoding: Boolean = False): Boolean;
var
  i:    TStrOff;
  Temp: Integer;
begin
// first check if the header can actually fit into the string (ie. the string is long enough)
If Length(EncodedString) >= HeaderLength then
  begin
    // check that the first character is a cross (#) and sixth is a vawe (~)
    If (EncodedString[1] = '#') and (EncodedString[6] = '~') then
      begin
        Result := True;
        // check that the four chars between can be a hex number
        For i := 2 to 5 do
          If not CharInSet(EncodedString[i],['0'..'9','a'..'f','A'..'F']) then
            begin
              Result := False;
              Exit;
            end;
        // finally check whether the header contains a valid encoding
        If CheckEncoding and TryStrToInt('$' + Copy(EncodedString,2,4),Temp) then
          Result := NumberToEncoding(UInt16(Temp)) <> encUnknown;
      end
    else Result := False;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

class Function TBTETranscoder.HeaderEncoding(const EncodedString: String): TBTEEncoding;
var
  HeaderNumber: UInt16;
begin
If HeaderNumberExtract(EncodedString,HeaderNumber) then
  Result := NumberToEncoding(HeaderNumber)
else
  Result := encUnknown;
end;

//------------------------------------------------------------------------------

class Function TBTETranscoder.HeaderReversed(const EncodedString: String): Boolean;
var
  HeaderNumber: UInt16;
begin
If HeaderNumberExtract(EncodedString,HeaderNumber) then
  Result := (HeaderNumber and BTE_HEADER_FLAG_REVERSED) <> 0
else
  Result := False;
end;

//------------------------------------------------------------------------------

class Function TBTETranscoder.EncodingTableIsValid(const EncodingTable: TBTEEncodingTable): Boolean;
var
  i,j:  Integer;
begin
{
  Check length of the table, that all chars have ordinal values lower or equal
  to 127 and are not a tilde, and also that there are no repeats.
}
Result := False;
If Length(EncodingTable) = EncodingTableLength then
  begin
    For i := Low(EncodingTable) to High(EncodingTable) do
      If (Ord(EncodingTable[i]) <= 127) and (EncodingTable[i] <> '~') then
        begin
          For j := Succ(i) to High(EncodingTable) do
            If EncodingTable[i] = EncodingTable[j] then
              Exit;
        end
      else Exit;
  end;
Result := True;
end;

//------------------------------------------------------------------------------

constructor TBTETranscoder.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TBTETranscoder.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TBTETranscoder.BreakProcessing: Boolean;
begin
Result := fBreakProcessing;
fBreakProcessing := True;
end;     


{===============================================================================
--------------------------------------------------------------------------------
                                  TBTEEncoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBTEEncoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBTEEncoder - protected methods
-------------------------------------------------------------------------------}

Function TBTEEncoder.GetEncodingTable: TBTEEncodingTable;
begin
Result := Copy(fEncodingTable);
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.SetEncodingTable(const Value: TBTEEncodingTable);
begin
If not fIsProcessing then
  begin
    // check validity of the table
    If EncodingTableIsValid(Value) then
      fEncodingTable := Copy(Value)
    else
      raise EBTEInvalidValue.Create('TBTEEncoder.SetEncodingTable: Invalid encoding table.');
  end
else raise EBTEInvalidState.Create('TBTEEncoder.SetEncodingTable: Cannot set encoding table, encoder is running.');
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.AssignEncodingTable(const EncodingTable: TBTEEncodingTable);
var
  i:  Integer;
begin
SetLength(fEncodingTable,Length(EncodingTable));
For i := Low(EncodingTable) to High(EncodingTable) do
  fEncodingTable[i] := EncodingTable[i];
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.WriteHeader;
begin
If fReversed and (efReversible in EncodingFeatures) then
  fEncodedString := Format('#%.4x~',[EncodingToNumber(Encoding) or BTE_HEADER_FLAG_REVERSED])
else
  fEncodedString := Format('#%.4x~',[EncodingToNumber(Encoding)]);
fEncodedStringPos := Succ(HeaderLength);
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.WriteChar(C: Char);
begin
If fEncodedStringPos <= Length(fEncodedString) then
  begin
    // progress
    If ((fEncodedStringPos mod fProgressCoef) = 0) and (Length(fEncodedString) > 0) then
      DoProgress(Pred(fEncodedStringPos) / Length(fEncodedString));
    fEncodedString[fEncodedStringPos] := C;
    Inc(fEncodedStringPos);
  end
else raise EBTEProcessingError.Create('TBTEEncoder.WriteChar: Invalid string position.');
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.WritePadding;
begin
while fEncodedStringPos <= Length(fEncodedString) do
  WriteChar(fPaddingChar);
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.Encode(const Buffer; Size: TMemSize);
begin
fBreakProcessing := False;
fIsProcessing := True;
try
  fEncodedStringPos := 1;
  // write header
  If fHeader then
    WriteHeader;
  // preallocate encoded string (note that it may already contain the header)
  SetLength(fEncodedString,EncodedLengthFromBuffer(Buffer,Size));
  DoProgress(0.0);
  If not fBreakProcessing then
    begin
      // encoding-specific processing
      EncodeSpecific(Buffer,Size);
      If not fBreakProcessing then
        DoProgress(1.0);
    end;
finally
  fIsProcessing := False;
end;
end;

{-------------------------------------------------------------------------------
    TBTEEncoder - public methods
-------------------------------------------------------------------------------}

Function TBTEEncoder.EncodedLengthFromMemory(Mem: Pointer; Size: TMemSize): TStrSize;
begin
Result := EncodedLengthFromBuffer(Mem^,Size);
end;

//------------------------------------------------------------------------------

Function TBTEEncoder.EncodedLengthFromStream(Stream: TStream; Count: Int64 = -1): TStrSize;
var
  Buffer:     Pointer;
  BytesRead:  Integer;
begin
If Assigned(Stream) then
  begin
    If Count = 0 then
      Count := Stream.Size - Stream.Position;
    If Count < 0 then
      begin
        Stream.Seek(0,soBeginning);
        Count := Stream.Size;
      end;  
    If Stream.Size <= DataLimit then
      begin
        If efSlowOutputSize in EncodingFeatures then
          begin
            // slow output size (the entire data must be scanned)
            If not(Stream is TCustomMemoryStream) then
              begin
                GetMem(Buffer,Count);
                try
                  BytesRead := Stream.Read(Buffer^,Count);
                  Result := EncodedLengthFromBuffer(Buffer^,TMemSize(BytesRead));
                finally
                  FreeMem(Buffer,Count);
                end;              
              end
          {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
            else Result := EncodedLengthFromBuffer(Pointer(PtrUInt(TCustomMemoryStream(Stream).Memory) + PtrUInt(Stream.Position))^,TMemSize(Count));
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
          end
        else Result := EncodedLengthFromBuffer(nil^,TMemSize(Count));
      end
    else raise EBTETooMuchData.CreateFmt('TBTEEncoder.EncodedLengthFromStream: Too much data (%d bytes).',[Stream.Size]);
  end
else raise EBTEInvalidValue.Create('TBTEEncoder.EncodedLengthFromStream: Stream not assigned.');
end;

//------------------------------------------------------------------------------

Function TBTEEncoder.EncodedLengthFromFile(const FileName: String): TStrSize;
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyNone);
try
  Result := EncodedLengthFromStream(FileStream);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.EncodeFromBuffer(const Buffer; Size: TMemSize);
begin
If Size <= DataLimit then
  Encode(Buffer,Size)
else
  raise EBTETooMuchData.CreateFmt('TBTEEncoder.EncodeFromBuffer: Too much data (%d bytes).',[Size]);
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.EncodeFromMemory(Mem: Pointer; Size: TMemSize);
begin
If Size <= DataLimit then
  Encode(Mem^,Size)
else
  raise EBTETooMuchData.CreateFmt('TBTEEncoder.EncodeFromMemory: Too much data (%d bytes).',[Size]);
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.EncodeFromStream(Stream: TStream; Count: Int64 = -1);
var
  Buffer:     Pointer;
  BytesRead:  Integer;
begin
If Assigned(Stream) then
  begin
    If Count = 0 then
      Count := Stream.Size - Stream.Position;
    If Count < 0 then
      begin
        Stream.Seek(0,soBeginning);
        Count := Stream.Size;
      end; 
    If Count <= DataLimit then
      begin
        If not(Stream is TCustomMemoryStream) then
          begin
            // not a memory stream, load all data into buffer and process them there
            GetMem(Buffer,Count);
            try
              BytesRead := Stream.Read(Buffer^,Count);
              Encode(Buffer^,TMemSize(BytesRead));
            finally
              FreeMem(Buffer,Count);
            end;
          end
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        else Encode(Pointer(PtrUInt(TCustomMemoryStream(Stream).Memory) + PtrUInt(Stream.Position))^,TMemSize(Count));
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
      end
    else raise EBTETooMuchData.CreateFmt('TBTEEncoder.EncodeFromStream: Too much data (%d bytes).',[Stream.Size]);
  end
else raise EBTEInvalidValue.Create('TBTEEncoder.EncodeFromStream: Stream not assigned.');
end;

//------------------------------------------------------------------------------

procedure TBTEEncoder.EncodeFromFile(const FileName: String);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  EncodeFromStream(FileStream);
finally
  FileStream.Free;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  TBTEDecoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TBTEDecoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBTEDecoder - protected methods
-------------------------------------------------------------------------------}

procedure TBTEDecoder.ReadHeader;
var
  StrEncoding:  TBTEEncoding;
begin
fHeader := HeaderPresent(fEncodedString);
// if header is not present, leave current encoding settings
If fHeader then
  begin
    StrEncoding := HeaderEncoding(fEncodedString);
    If StrEncoding = Encoding then
      begin
        If efReversible in EncodingFeatures then
          fReversed := HeaderReversed(fEncodedString)
        else
          fReversed := False;
      end
    else raise EBTEProcessingError.CreateFmt('TBTEDecoder.ReadHeader: String has different encoding (%d).',[Ord(StrEncoding)]);
  end;
end;

//------------------------------------------------------------------------------

Function TBTEDecoder.ReadChar: Char;
begin
If fEncodedStringPos <= Length(fEncodedString) then
  begin
    // progress
    If ((fEncodedStringPos mod fProgressCoef) = 0) and (Length(fEncodedString) > 0) then
      DoProgress(Pred(fEncodedStringPos) / Length(fEncodedString));
    Result := fEncodedString[fEncodedStringPos];
    Inc(fEncodedStringPos);
  end
else raise EBTEProcessingError.Create('TBTEDecoder.ReadChar: Invalid string position.');
end;

//------------------------------------------------------------------------------

procedure TBTEDecoder.RollBack;
begin
If fEncodedStringPos > 1 then
  Dec(fEncodedStringPos)
else
  raise EBTEProcessingError.Create('TBTEDecoder.RollBack: Invalid string position.');
end;

//------------------------------------------------------------------------------

Function TBTEDecoder.ResolveChar(C: Char): Byte;
begin
If Ord(C) <= 127 then
  begin
    Result := fDecodingTable[Ord(C) and BTE_CHAR_MASK];
    If Result = Byte(-1) then
      raise EBTEProcessingError.CreateFmt('TBTEDecoder.ResolveChar: Unknown character (#%d).',[Ord(C)]);
  end
else raise EBTEProcessingError.CreateFmt('TBTEDecoder.ResolveChar: Invalid character (#%d).',[Ord(C)]);
end;

//------------------------------------------------------------------------------

Function TBTEDecoder.CountPadding: TStrSize;
var
  i:  TStrOff;
begin
Result := 0;
For i := Length(fEncodedString) downto 1 do
  If fEncodedString[i] = fPaddingChar then
    Inc(Result)
  else
    Break{For i};
end;

//------------------------------------------------------------------------------

procedure TBTEDecoder.Decode(out Buffer; Size: TMemSize);
begin
fBreakProcessing := False;
fIsProcessing := True;
try
  // read header
  ReadHeader;
  If fHeader then
    fEncodedStringPos := Succ(HeaderLength)
  else
    fEncodedStringPos := 1;
  DoProgress(0.0);
  If not fBreakProcessing then
    begin
      // decoding-specific processing
      DecodeSpecific(Buffer,Size);
      If not fBreakProcessing then
        DoProgress(1.0);
    end;
finally
  fIsProcessing := False;
end;
end;

{-------------------------------------------------------------------------------
    TBTEDecoder - public methods
-------------------------------------------------------------------------------}

procedure TBTEDecoder.ConstructDecodingTable(const EncodingTable: TBTEEncodingTable);
var
  i:  Integer;
begin
If EncodingTableIsValid(EncodingTable) then
  begin
    FillChar(fDecodingTable,SizeOf(TBTEDecodingTable),Byte(-1));
    For i := Low(EncodingTable) to High(EncodingTable) do
      fDecodingTable[Ord(EncodingTable[i]) and BTE_CHAR_MASK] := i;
  end
else raise EBTEInvalidValue.Create('TBTEDecoder.ConstructDecodingTable: Invalid encoding table.');
end;

//------------------------------------------------------------------------------

Function TBTEDecoder.DecodeIntoBuffer(out Buffer; Size: TMemSize): TMemSize;
begin
Result := DecodedSize;
If Size >= Result then
  Decode(Buffer,Result)
else
  raise EBTEBufferTooSmall.CreateFmt('TBTEDecoder.DecodeIntoBuffer: Buffer too small (%u/%u).',[Size,Result]);
end;

//------------------------------------------------------------------------------

Function TBTEDecoder.DecodeIntoMemory(Mem: Pointer; Size: TMemSize): TMemSize;
begin
Result := DecodedSize;
If Size >= Result then
  Decode(Mem^,Result)
else
  raise EBTEBufferTooSmall.CreateFmt('TBTEDecoder.DecodeIntoMemory: Buffer too small (%u/%u).',[Size,Result]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TBTEDecoder.DecodeIntoMemory(out Mem: Pointer): TMemSize; 
begin
Result := DecodedSize;
If Result > 0 then
  begin
    GetMem(Mem,Result);
    try
      Decode(Mem^,Result);
    except
      FreeMem(Mem,Result);
      Mem := nil;      
      Result := 0;
    end;
  end
else Mem := nil;
end;

//------------------------------------------------------------------------------

Function TBTEDecoder.DecodeIntoStream(Stream: TStream): TMemSize;
var
  Buffer: Pointer;
begin
Result := DecodeIntoMemory(Buffer);
try
  Stream.WriteBuffer(Buffer^,Result);
finally
  FreeMem(Buffer,Result);
end;
end;

//------------------------------------------------------------------------------

Function TBTEDecoder.DecodeIntoFile(const FileName: String): TMemSize;
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmCreate or fmShareDenyWrite);
try
  Result := DecodeIntoStream(FileStream);
finally
  FileStream.Free;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase2Encoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base2: array[0..1] of Char = ('0','1');

{===============================================================================
    TBase2Encoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase2Encoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase2Encoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base2));
end;

//------------------------------------------------------------------------------

procedure TBase2Encoder.EncodeSpecific(const Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i,j:      Integer;
  Temp:     Byte;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
For i := 1 to Size do
  begin
    Temp := PByte(DataPtr)^;
    For j := 7 downto 0 do
      WriteChar(fEncodingTable[(Temp shr j) and 1]);
    If fBreakProcessing then
      Break{For i};
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase2Encoder - public methods
-------------------------------------------------------------------------------}

class Function TBase2Encoder.DataLimit: TMemSize;
begin
Result := 64 * 1024 * 1024; // 64MiB
end;

//------------------------------------------------------------------------------

class Function TBase2Encoder.Encoding: TBTEEncoding;
begin
Result := encBase2;
end;

//------------------------------------------------------------------------------

class Function TBase2Encoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase2Encoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base2);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TBase2Encoder.EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize;
begin
If Size <= DataLimit then
  begin
    If fHeader then
      Result := TStrSize(Size * 8) + HeaderLength
    else
      Result := TStrSize(Size * 8);
  end
else raise EBTETooMuchData.CreateFmt('TBase2Encoder.EncodedLengthFromBuffer: Too much data (%u bytes).',[Size]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase2Decoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base2: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $00, $01, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

{===============================================================================
    TBase2Decoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase2Decoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase2Decoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base2;
end;

//------------------------------------------------------------------------------

procedure TBase2Decoder.DecodeSpecific(out Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i,j:      Integer;
  Temp:     Byte;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
For i := 1 to Size do
  begin
    Temp := 0;
    For j := 1 to 8 do
      Temp := (Temp shl 1) or ResolveChar(ReadChar);
    If fBreakProcessing then
      Break{For i};
    PByte(DataPtr)^ := Temp;
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase2Decoder - public methods
-------------------------------------------------------------------------------}

class Function TBase2Decoder.DataLimit: TMemSize;
begin
Result := 64 * 1024 * 1024; // 64MiB
end;

//------------------------------------------------------------------------------

class Function TBase2Decoder.Encoding: TBTEEncoding;
begin
Result := encBase2;
end;

//------------------------------------------------------------------------------

class Function TBase2Decoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase2Decoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base2);
end;

//------------------------------------------------------------------------------

Function TBase2Decoder.DecodedSize: TMemSize;
begin
If HeaderPresent(fEncodedString) then
  Result := TMemSize((Length(fEncodedString) - HeaderLength) div 8)
else
  Result := TMemSize(Length(fEncodedString) div 8)
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase4Encoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base4: array[0..3] of Char = ('0','1','2','3');

{===============================================================================
    TBase4Encoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase4Encoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase4Encoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base4));
end;

//------------------------------------------------------------------------------

procedure TBase4Encoder.EncodeSpecific(const Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i,j:      Integer;
  Temp:     Byte;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
For i := 1 to Size do
  begin
    Temp := PByte(DataPtr)^;
    For j := 3 downto 0 do
      WriteChar(fEncodingTable[(Temp shr (j * 2)) and 3]);
    If fBreakProcessing then
      Break{For i};
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase4Encoder - public methods
-------------------------------------------------------------------------------}

class Function TBase4Encoder.DataLimit: TMemSize;
begin
Result := 128 * 1024 * 1024;  // 128MiB
end;

//------------------------------------------------------------------------------

class Function TBase4Encoder.Encoding: TBTEEncoding;
begin
Result := encBase4;
end;

//------------------------------------------------------------------------------

class Function TBase4Encoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase4Encoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base4);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TBase4Encoder.EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize;
begin
If Size <= DataLimit then
  begin
    If fHeader then
      Result := TStrSize(Size * 4) + HeaderLength
    else
      Result := TStrSize(Size * 4);
  end
else raise EBTETooMuchData.CreateFmt('TBase4Encoder.EncodedLengthFromBuffer: Too much data (%u bytes).',[Size]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                 TBase4Decoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base4: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $00, $01, $02, $03, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

{===============================================================================
    TBase4Decoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase4Decoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase4Decoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base4;
end;

//------------------------------------------------------------------------------

procedure TBase4Decoder.DecodeSpecific(out Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i,j:      Integer;
  Temp:     Byte;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
For i := 1 to Size do
  begin
    Temp := 0;
    For j := 1 to 4 do
      Temp := (Temp shl 2) or ResolveChar(ReadChar);
    If fBreakProcessing then
      Break{For i};
    PByte(DataPtr)^ := Temp;
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase4Decoder - public methods
-------------------------------------------------------------------------------}

class Function TBase4Decoder.DataLimit: TMemSize;
begin
Result := 128 * 1024 * 1024;  // 128MiB
end;

//------------------------------------------------------------------------------

class Function TBase4Decoder.Encoding: TBTEEncoding;
begin
Result := encBase4;
end;

//------------------------------------------------------------------------------

class Function TBase4Decoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase4Decoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base4);
end;

//------------------------------------------------------------------------------

Function TBase4Decoder.DecodedSize: TMemSize;
begin
If HeaderPresent(fEncodedString) then
  Result := TMemSize((Length(fEncodedString) - HeaderLength) div 4)
else
  Result := TMemSize(Length(fEncodedString) div 4)
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase8Encoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base8: array[0..7] of Char = ('0','1','2','3','4','5','6','7');

  PaddingChar_Base8: Char = '=';

{===============================================================================
    TBase8Encoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase8Encoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase8Encoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base8));
fPaddingChar := PaddingChar_Base8;
end;

//------------------------------------------------------------------------------

procedure TBase8Encoder.EncodeSpecific(const Buffer; Size: TMemSize);
var
  DataPtr:        Pointer;
  i:              Integer;
  Temp:           Byte;
  Remainder:      Byte;
  RemainderBits:  Integer;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
Remainder := 0;
RemainderBits := 0;
For i := 1 to Size do
  begin
    Temp := PByte(DataPtr)^;
    case RemainderBits of
      0:  begin
            WriteChar(fEncodingTable[(Temp shr 5) and 7]);
            WriteChar(fEncodingTable[(Temp shr 2) and 7]);
            Remainder := Temp and 3;
            RemainderBits := 2;
          end;
      1:  begin
            WriteChar(fEncodingTable[(Remainder shl 2) or ((Temp shr 6) and 3)]);
            WriteChar(fEncodingTable[(Temp shr 3) and 7]);
            WriteChar(fEncodingTable[Temp and 7]);
            Remainder := 0;
            RemainderBits := 0;
          end;
      2:  begin
            WriteChar(fEncodingTable[(Remainder shl 1) or ((Temp shr 7) and 1)]);
            WriteChar(fEncodingTable[(Temp shr 4) and 7]);
            WriteChar(fEncodingTable[(Temp shr 1) and 7]);
            Remainder := Temp and 1;
            RemainderBits := 1;
          end;
    else
      raise EBTEProcessingError.CreateFmt('TBase8Encoder.EncodeSpecific: Invalid RemainderBits (%d).',[RemainderBits]);
    end;
    If fBreakProcessing then
      Break{For i};
    AdvanceDataPointer(DataPtr);
  end;
If not fBreakProcessing then
  begin
    // process possible remainder bits
    case RemainderBits of
      0:  ; // no remainder, do nothing
      1:  WriteChar(fEncodingTable[Remainder shl 2]);
      2:  WriteChar(fEncodingTable[Remainder shl 1]);
    else
      raise EBTEProcessingError.CreateFmt('TBase8Encoder.EncodeSpecific: Invalid RemainderBits (%d).',[RemainderBits]);
    end;
    // fill rest of the string with padding
    If fPadding then
      WritePadding;
  end;
end;

{-------------------------------------------------------------------------------
    TBase8Encoder - public methods
-------------------------------------------------------------------------------}

class Function TBase8Encoder.DataLimit: TMemSize;
begin
Result := 192 * 1024 * 1024;  // 192MiB
end;

//------------------------------------------------------------------------------

class Function TBase8Encoder.Encoding: TBTEEncoding;
begin
Result := encBase8;
end;

//------------------------------------------------------------------------------

class Function TBase8Encoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efPadding,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase8Encoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base8);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TBase8Encoder.EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize;
begin
If Size <= DataLimit then
  begin
    If fPadding then
      Result := TStrSize(Ceil(Size / 3) * 8)
    else
      Result := TStrSize(Ceil((Size * 8) / 3));
    If fHeader then
      Result := Result + HeaderLength;
  end
else raise EBTETooMuchData.CreateFmt('TBase8Encoder.EncodedLengthFromBuffer: Too much data (%u bytes).',[Size]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase8Decoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base8: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $00, $01, $02, $03, $04, $05, $06, $07, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
     
{===============================================================================
    TBase8Decoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase8Decoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase8Decoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base8;
fPaddingChar := PaddingChar_Base8;
end;

//------------------------------------------------------------------------------

procedure TBase8Decoder.DecodeSpecific(out Buffer; Size: TMemSize);
var
  DataPtr:        Pointer;
  i:              Integer;
  Temp:           Byte;
  Remainder:      Byte;
  RemainderBits:  Integer;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
Remainder := 0;
RemainderBits := 0;
For i := 1 to Size do
  begin
    case RemainderBits of
      0:  begin
            Temp := ResolveChar(ReadChar) shl 5;
            Temp := Temp or (ResolveChar(ReadChar) shl 2);
            Remainder := ResolveChar(ReadChar);
            Temp := Temp or (Remainder shr 1);
            Remainder := Remainder and 1;
            RemainderBits := 1;
          end;
      1:  begin
            Temp := (Remainder shl 7) or (ResolveChar(ReadChar) shl 4);
            Temp := Temp or (ResolveChar(ReadChar) shl 1);
            Remainder := ResolveChar(ReadChar);
            Temp := Temp or (Remainder shr 2);
            Remainder := Remainder and 3;
            RemainderBits := 2;
          end;
      2:  begin
            Temp := (Remainder shl 6) or (ResolveChar(ReadChar) shl 3);
            Temp := Temp or ResolveChar(ReadChar);
            Remainder := 0;
            RemainderBits := 0;
          end;
    else
      raise EBTEProcessingError.CreateFmt('TBase8Decoder.DecodeSpecific: Invalid RemainderBits (%d).',[RemainderBits]);
    end;
    If fBreakProcessing then
      Break{For i};
    PByte(DataPtr)^ := Temp;
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase8Decoder - public methods
-------------------------------------------------------------------------------}

class Function TBase8Decoder.DataLimit: TMemSize;
begin
Result := 192 * 1024 * 1024;  // 192MiB
end;

//------------------------------------------------------------------------------

class Function TBase8Decoder.Encoding: TBTEEncoding;
begin
Result := encBase8;
end;

//------------------------------------------------------------------------------

class Function TBase8Decoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efPadding,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase8Decoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base8);
end;

//------------------------------------------------------------------------------

Function TBase8Decoder.DecodedSize: TMemSize;
var
  PaddingCount: TStrSize;
begin
PaddingCount := CountPadding;
If HeaderPresent(fEncodedString) then
  Result := TMemSize(Floor(((Length(fEncodedString) - HeaderLength - PaddingCount) * 3) / 8))
else
  Result := TMemSize(Floor(((Length(fEncodedString) - PaddingCount) * 3) / 8));
fPadding := PaddingCount > 0;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase10Encoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base10: array[0..9] of Char = ('0','1','2','3','4','5','6','7','8','9');

  Coefficients_Base10: array[0..2] of Integer = (100,10,1);

{===============================================================================
    TBase10Encoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase10Encoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase10Encoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base10));
end;

//------------------------------------------------------------------------------

procedure TBase10Encoder.EncodeSpecific(const Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i,j:      Integer;
  Temp:     Byte;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
For i := 1 to Size do
  begin
    Temp := PByte(DataPtr)^;
    For j := 0 to 2 do
      begin
        WriteChar(fEncodingTable[Temp div Coefficients_Base10[j]]);
        Temp := Temp mod Coefficients_Base10[j];
      end;
    If fBreakProcessing then
      Break{For i};
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase10Encoder - public methods
-------------------------------------------------------------------------------}

class Function TBase10Encoder.DataLimit: TMemSize;
begin
Result := 170 * 1024 * 1024;  // 170MiB
end;

//------------------------------------------------------------------------------

class Function TBase10Encoder.Encoding: TBTEEncoding;
begin
Result := encBase10;
end;

//------------------------------------------------------------------------------

class Function TBase10Encoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase10Encoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base10);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TBase10Encoder.EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize;
begin
If Size <= DataLimit then
  begin
    If fHeader then
      Result := TStrSize(Size * 3) + HeaderLength
    else
      Result := TStrSize(Size * 3);
  end
else raise EBTETooMuchData.CreateFmt('TBase10Encoder.EncodedLengthFromBuffer: Too much data (%u bytes).',[Size]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase10Decoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base10: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

{===============================================================================
    TBase10Decoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase10Decoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase10Decoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base10;
end;

//------------------------------------------------------------------------------

procedure TBase10Decoder.DecodeSpecific(out Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i,j:      Integer;
  Temp:     Byte;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
For i := 1 to Size do
  begin
    Temp := 0;
    For j := 0 to 2 do
      Temp := Temp + (ResolveChar(ReadChar) * Coefficients_Base10[j]);
    If fBreakProcessing then
      Break{For i};
    PByte(DataPtr)^ := Temp;
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase10Decoder - public methods
-------------------------------------------------------------------------------}

class Function TBase10Decoder.DataLimit: TMemSize;
begin
Result := 170 * 1024 * 1024;  // 170MiB
end;

//------------------------------------------------------------------------------

class Function TBase10Decoder.Encoding: TBTEEncoding;
begin
Result := encBase10;
end;

//------------------------------------------------------------------------------

class Function TBase10Decoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase10Decoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base10);
end;

//------------------------------------------------------------------------------

Function TBase10Decoder.DecodedSize: TMemSize;
begin
If HeaderPresent(fEncodedString) then
  Result := TMemSize((Length(fEncodedString) - HeaderLength) div 3)
else
  Result := TMemSize(Length(fEncodedString) div 3)
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase16Encoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base16: array[0..15] of Char =
    ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

{===============================================================================
    TBase16Encoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase16Encoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase16Encoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base16));
end;

//------------------------------------------------------------------------------

procedure TBase16Encoder.EncodeSpecific(const Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i:        Integer;
  Temp:     Byte;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
For i := 1 to Size do
  begin
    Temp := PByte(DataPtr)^;
    WriteChar(fEncodingTable[(Temp shr 4) and $F]);
    WriteChar(fEncodingTable[Temp and $F]);
    If fBreakProcessing then
      Break{For i};
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase16Encoder - public methods
-------------------------------------------------------------------------------}

class Function TBase16Encoder.DataLimit: TMemSize;
begin
Result := 256 * 1024 * 1024;  // 256MiB
end;

//------------------------------------------------------------------------------

class Function TBase16Encoder.Encoding: TBTEEncoding;
begin
Result := encBase16;
end;

//------------------------------------------------------------------------------

class Function TBase16Encoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase16Encoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base16);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TBase16Encoder.EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize;
begin
If Size <= DataLimit then
  begin
    If fHeader then
      Result := TStrSize(Size * 2) + HeaderLength
    else
      Result := TStrSize(Size * 2);
  end
else raise EBTETooMuchData.CreateFmt('TBase16Encoder.EncodedLengthFromBuffer: Too much data (%u bytes).',[Size]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase16Decoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base16: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $0A, $0B, $0C, $0D, $0E, $0F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);

{===============================================================================
    TBase16Decoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase16Decoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase16Decoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base16;
end;

//------------------------------------------------------------------------------

procedure TBase16Decoder.DecodeSpecific(out Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i:        Integer;
  Temp:     Byte;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
For i := 1 to Size do
  begin
    Temp := ResolveChar(ReadChar) shl 4;
    Temp := Temp or ResolveChar(ReadChar);
    If fBreakProcessing then
      Break{For i};
    PByte(DataPtr)^ := Temp;
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase16Decoder - public methods
-------------------------------------------------------------------------------}

class Function TBase16Decoder.DataLimit: TMemSize;
begin
Result := 256 * 1024 * 1024;  // 256MiB
end;

//------------------------------------------------------------------------------

class Function TBase16Decoder.Encoding: TBTEEncoding;
begin
Result := encBase16;
end;

//------------------------------------------------------------------------------

class Function TBase16Decoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase16Decoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base16);
end;

//------------------------------------------------------------------------------

Function TBase16Decoder.DecodedSize: TMemSize;
begin
If HeaderPresent(fEncodedString) then
  Result := TMemSize((Length(fEncodedString) - HeaderLength) div 2)
else
  Result := TMemSize(Length(fEncodedString) div 2)
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase32Encoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base32: array[0..31] of Char =
    ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
     'Q','R','S','T','U','V','W','X','Y','Z','2','3','4','5','6','7');

  PaddingChar_Base32: Char = '=';

{===============================================================================
    TBase32Encoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase32Encoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase32Encoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base32));
fPaddingChar := PaddingChar_Base32;
end;

//------------------------------------------------------------------------------

procedure TBase32Encoder.EncodeSpecific(const Buffer; Size: TMemSize);
var
  DataPtr:        Pointer;
  i:              Integer;
  Temp:           Byte;
  Remainder:      Byte;
  RemainderBits:  Integer;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
Remainder := 0;
RemainderBits := 0;
For i := 1 to Size do
  begin
    Temp := PByte(DataPtr)^;
    case RemainderBits of
      0:  begin
            WriteChar(fEncodingTable[(Temp shr 3) and $1F]);
            Remainder := Temp and 7;
            RemainderBits := 3;
          end;
      1:  begin
            WriteChar(fEncodingTable[(Remainder shl 4) or ((Temp shr 4) and $F)]);
            Remainder := Temp and $F;
            RemainderBits := 4;
          end;
      2:  begin
            WriteChar(fEncodingTable[(Remainder shl 3) or ((Temp shr 5) and 7)]);
            WriteChar(fEncodingTable[Temp and $1F]);
            Remainder := 0;
            RemainderBits := 0;
          end;
      3:  begin
            WriteChar(fEncodingTable[(Remainder shl 2) or ((Temp shr 6) and 3)]);
            WriteChar(fEncodingTable[(Temp shr 1) and $1F]);
            Remainder := Temp and 1;
            RemainderBits := 1;
          end;
      4:  begin
            WriteChar(fEncodingTable[(Remainder shl 1) or ((Temp shr 7) and 1)]);
            WriteChar(fEncodingTable[(Temp shr 2) and $1F]);
            Remainder := Temp and 3;
            RemainderBits := 2;
          end;
    else
      raise EBTEProcessingError.CreateFmt('TBase32Encoder.EncodeSpecific: Invalid RemainderBits (%d).',[RemainderBits]);
    end;
    If fBreakProcessing then
      Break{For i};
    AdvanceDataPointer(DataPtr);
  end;
If not fBreakProcessing then
  begin
    case RemainderBits of
      0:  ;
      1:  WriteChar(fEncodingTable[Remainder shl 4]);
      2:  WriteChar(fEncodingTable[Remainder shl 3]);
      3:  WriteChar(fEncodingTable[Remainder shl 2]);
      4:  WriteChar(fEncodingTable[Remainder shl 1]);
    else
      raise EBTEProcessingError.CreateFmt('TBase32Encoder.EncodeSpecific: Invalid RemainderBits (%d).',[RemainderBits]);
    end;
    If fPadding then
      WritePadding;
  end;
end;

{-------------------------------------------------------------------------------
    TBase32Encoder - public methods
-------------------------------------------------------------------------------}

class Function TBase32Encoder.DataLimit: TMemSize;
begin
Result := 320 * 1024 * 1024;  // 320MiB
end;

//------------------------------------------------------------------------------

class Function TBase32Encoder.Encoding: TBTEEncoding;
begin
Result := encBase32;
end;

//------------------------------------------------------------------------------

class Function TBase32Encoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efPadding,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase32Encoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base32);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TBase32Encoder.EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize;
begin
If Size <= DataLimit then
  begin
    If fPadding then
      Result := TStrSize(Ceil(Size / 5) * 8)
    else
      Result := TStrSize(Ceil((Size * 8) / 5));
    If fHeader then
      Result := Result + HeaderLength;
  end
else raise EBTETooMuchData.CreateFmt('TBase32Encoder.EncodedLengthFromBuffer: Too much data (%u bytes).',[Size]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase32Decoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base32: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $1A, $1B, $1C, $1D, $1E, $1F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E,
     $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
     
{===============================================================================
    TBase32Decoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase32Decoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase32Decoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base32;
fPaddingChar := PaddingChar_Base32;
end;

//------------------------------------------------------------------------------

procedure TBase32Decoder.DecodeSpecific(out Buffer; Size: TMemSize);
var
  DataPtr:        Pointer;
  i:              Integer;
  Temp:           Byte;
  Remainder:      Byte;
  RemainderBits:  Integer;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
Remainder := 0;
RemainderBits := 0;
For i := 1 to Size do
  begin
    case RemainderBits of
      0:  begin
            Temp := ResolveChar(ReadChar) shl 3;
            Remainder := ResolveChar(ReadChar);
            Temp := Temp or (Remainder shr 2);
            Remainder := Remainder and 3;
            RemainderBits := 2;
          end;
      1:  begin
            Temp := (Remainder shl 7) or (ResolveChar(ReadChar) shl 2);
            Remainder := ResolveChar(ReadChar);
            Temp := Temp or (Remainder shr 3);
            Remainder := Remainder and 7;
            RemainderBits := 3;
          end;
      2:  begin
            Temp := (Remainder shl 6) or (ResolveChar(ReadChar) shl 1);
            Remainder := ResolveChar(ReadChar);
            Temp := Temp or (Remainder shr 4);
            Remainder := Remainder and $F;
            RemainderBits := 4;
          end;
      3:  begin
            Temp := (Remainder shl 5) or ResolveChar(ReadChar);
            Remainder := 0;
            RemainderBits := 0;
          end;
      4:  begin
            Temp := Remainder shl 4;
            Remainder := ResolveChar(ReadChar);
            Temp := Temp or (Remainder shr 1);
            Remainder := Remainder and 1;
            RemainderBits := 1;
          end;
    else
      raise EBTEProcessingError.CreateFmt('TBase32Decoder.DecodeSpecific: Invalid RemainderBits (%d).',[RemainderBits]);
    end;
    If fBreakProcessing then
      Break{For i};
    PByte(DataPtr)^ := Temp;
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase32Decoder - public methods
-------------------------------------------------------------------------------}

class Function TBase32Decoder.DataLimit: TMemSize;
begin
Result := 320 * 1024 * 1024;  // 320MiB
end;

//------------------------------------------------------------------------------

class Function TBase32Decoder.Encoding: TBTEEncoding;
begin
Result := encBase32;
end;

//------------------------------------------------------------------------------

class Function TBase32Decoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efPadding,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase32Decoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base32);
end;

//------------------------------------------------------------------------------

Function TBase32Decoder.DecodedSize: TMemSize;
var
  PaddingCount: TStrSize;
begin
PaddingCount := CountPadding;
If HeaderPresent(fEncodedString) then
  Result := TMemSize(Floor(((Length(fEncodedString) - HeaderLength - PaddingCount) * 5) / 8))
else
  Result := TMemSize(Floor(((Length(fEncodedString) - PaddingCount) * 5) / 8));
fPadding := PaddingCount > 0;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TBase32HexEncoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base32Hex: array[0..31] of Char =
    ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F',
     'G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V');

  PaddingChar_Base32Hex: Char = '=';

{===============================================================================
    TBase32HexEncoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase32HexEncoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase32HexEncoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base32Hex));
fPaddingChar := PaddingChar_Base32Hex;
end;

{-------------------------------------------------------------------------------
    TBase32HexEncoder - public methods
-------------------------------------------------------------------------------}

class Function TBase32HexEncoder.Encoding: TBTEEncoding;
begin
Result := encBase32Hex;
end;

//------------------------------------------------------------------------------

class Function TBase32HexEncoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base32Hex);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TBase32HexDecoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base32Hex: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18,
     $19, $1A, $1B, $1C, $1D, $1E, $1F, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
     
{===============================================================================
    TBase32HexDecoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase32HexDecoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase32HexDecoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base32Hex;
fPaddingChar := PaddingChar_Base32Hex;
end;

{-------------------------------------------------------------------------------
    TBase32HexDecoder - public methods
-------------------------------------------------------------------------------}

class Function TBase32HexDecoder.Encoding: TBTEEncoding;
begin
Result := encBase32Hex;
end;

//------------------------------------------------------------------------------

class Function TBase32HexDecoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base32Hex);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase64Encoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base64: array[0..63] of Char =
    ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
     'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
     'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
     'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/');

  PaddingChar_Base64: Char = '=';

{===============================================================================
    TBase64Encoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase64Encoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase64Encoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base64));
fPaddingChar := PaddingChar_Base64;
end;

//------------------------------------------------------------------------------

procedure TBase64Encoder.EncodeSpecific(const Buffer; Size: TMemSize);
var
  DataPtr:        Pointer;
  i:              Integer;
  Temp:           Byte;
  Remainder:      Byte;
  RemainderBits:  Integer;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
Remainder := 0;
RemainderBits := 0;
For i := 1 to Size do
  begin
    Temp := PByte(DataPtr)^;
    case RemainderBits of
      0:  begin
            WriteChar(fEncodingTable[(Temp shr 2) and $3F]);
            Remainder := Temp and $03;
            RemainderBits := 2;
          end;
      2:  begin
            WriteChar(fEncodingTable[(Remainder shl 4) or ((Temp shr 4) and $F)]);
            Remainder := Temp and $0F;
            RemainderBits := 4;
          end;
      4:  begin
            WriteChar(fEncodingTable[(Remainder shl 2) or ((Temp shr 6) and $3)]);
            WriteChar(fEncodingTable[Temp and $3F]);
            Remainder := 0;
            RemainderBits := 0;
          end;
    else
      raise EBTEProcessingError.CreateFmt('TBase64Encoder.EncodeSpecific: Invalid RemainderBits (%d).',[RemainderBits]);
    end;
    If fBreakProcessing then
      Break{For i};
    AdvanceDataPointer(DataPtr);
  end;
If not fBreakProcessing then
  begin
    case RemainderBits of
      0:  ;
      2:  WriteChar(fEncodingTable[Remainder shl 4]);
      4:  WriteChar(fEncodingTable[Remainder shl 2]);
    else
      raise EBTEProcessingError.CreateFmt('TBase64Encoder.EncodeSpecific: Invalid RemainderBits (%d).',[RemainderBits]);
    end;
    If fPadding then
      WritePadding;
  end;
end;

{-------------------------------------------------------------------------------
    TBase64Encoder - public methods
-------------------------------------------------------------------------------}

class Function TBase64Encoder.DataLimit: TMemSize;
begin
Result := 384 * 1024 * 1024;  // 384MiB
end;

//------------------------------------------------------------------------------

class Function TBase64Encoder.Encoding: TBTEEncoding;
begin
Result := encBase64;
end;

//------------------------------------------------------------------------------

class Function TBase64Encoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efPadding,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase64Encoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base64);
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TBase64Encoder.EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize;
begin
If Size <= DataLimit then
  begin
    If fPadding then
      Result := TStrSize(Ceil(Size / 3) * 4)
    else
      Result := TStrSize(Ceil((Size * 4) / 3));
    If fHeader then
      Result := Result + HeaderLength;
  end
else raise EBTETooMuchData.CreateFmt('TBase64Encoder.EncodedLengthFromBuffer: Too much data (%u bytes).',[Size]);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase64Decoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base64: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $3E, $FF, $FF, $FF, $3F,
     $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E,
     $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $FF, $FF, $FF, $FF, $FF,
     $FF, $1A, $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23, $24, $25, $26, $27, $28,
     $29, $2A, $2B, $2C, $2D, $2E, $2F, $30, $31, $32, $33, $FF, $FF, $FF, $FF, $FF);
     
{===============================================================================
    TBase64Decoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase64Decoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase64Decoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base64;
fPaddingChar := PaddingChar_Base64;
end;

//------------------------------------------------------------------------------

procedure TBase64Decoder.DecodeSpecific(out Buffer; Size: TMemSize);
var
  DataPtr:        Pointer;
  i:              Integer;
  Temp:           Byte;
  Remainder:      Byte;
  RemainderBits:  Integer;
begin
DataPtr := ResolveDataPointer(@Buffer,Size);
Remainder := 0;
RemainderBits := 0;
For i := 1 to Size do
  begin
    case RemainderBits of
      0:  begin
            Temp := ResolveChar(ReadChar) shl 2;
            Remainder := ResolveChar(ReadChar);
            Temp := Temp or ((Remainder shr 4) and 3);
            Remainder := Remainder and $F;
            RemainderBits := 4;
          end;
      2:  begin
            Temp := (Remainder shl 6) or ResolveChar(ReadChar);
            Remainder := 0;
            RemainderBits := 0;
          end;
      4:  begin
            Temp := Remainder shl 4;
            Remainder := ResolveChar(ReadChar);
            Temp := Temp or ((Remainder shr 2) and $F);
            Remainder := Remainder and 3;
            RemainderBits := 2;
          end;
    else
      raise EBTEProcessingError.CreateFmt('TBase64Decoder.DecodeSpecific: Invalid RemainderBits (%d).',[RemainderBits]);
    end;
    If fBreakProcessing then
      Break{For i};
    PByte(DataPtr)^ := Temp;
    AdvanceDataPointer(DataPtr);
  end;
end;

{-------------------------------------------------------------------------------
    TBase64Decoder - public methods
-------------------------------------------------------------------------------}

class Function TBase64Decoder.DataLimit: TMemSize;
begin
Result := 384 * 1024 * 1024;  // 384MiB
end;

//------------------------------------------------------------------------------

class Function TBase64Decoder.Encoding: TBTEEncoding;
begin
Result := encBase64;
end;

//------------------------------------------------------------------------------

class Function TBase64Decoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efPadding,efOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase64Decoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base64);
end;

//------------------------------------------------------------------------------

Function TBase64Decoder.DecodedSize: TMemSize;
var
  PaddingCount: TStrSize;
begin
PaddingCount := CountPadding;
If HeaderPresent(fEncodedString) then
  Result := TMemSize(Floor(((Length(fEncodedString) - HeaderLength - PaddingCount) * 3) / 4))
else
  Result := TMemSize(Floor(((Length(fEncodedString) - PaddingCount) * 3) / 4));
fPadding := PaddingCount > 0;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TBase64URLEncoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base64URL: array[0..63] of Char =
    ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
     'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
     'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
     'w','x','y','z','0','1','2','3','4','5','6','7','8','9','-','_');

  PaddingChar_Base64URL: Char = '=';

{===============================================================================
    TBase64URLEncoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase64URLEncoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase64URLEncoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base64URL));
fPaddingChar := PaddingChar_Base64URL;
end;

{-------------------------------------------------------------------------------
    TBase64URLEncoder - public methods
-------------------------------------------------------------------------------}

class Function TBase64URLEncoder.Encoding: TBTEEncoding;
begin
Result := encBase64URL;
end;

//------------------------------------------------------------------------------

class Function TBase64URLEncoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base64URL);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TBase64URLDecoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base64URL: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $3E, $FF, $FF,
     $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E,
     $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $FF, $FF, $FF, $FF, $3F,
     $FF, $1A, $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23, $24, $25, $26, $27, $28,
     $29, $2A, $2B, $2C, $2D, $2E, $2F, $30, $31, $32, $33, $FF, $FF, $FF, $FF, $FF);

{===============================================================================
    TBase64URLDecoder - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TBase64URLDecoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase64URLDecoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base64URL;
fPaddingChar := PaddingChar_Base64URL;
end;

{-------------------------------------------------------------------------------
    TBase64URLEncoder - public methods
-------------------------------------------------------------------------------}

class Function TBase64URLDecoder.Encoding: TBTEEncoding;
begin
Result := encBase64URL;
end;

//------------------------------------------------------------------------------

class Function TBase64URLDecoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base64URL);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase85Encoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_Base85: array[0..84] of Char =
    ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f',
     'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
     'w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L',
     'M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','.','-',
     ':','+','=','^','!','/','*','?','&','<','>','(',')','[',']','{',
     '}','@','%','$','#');

  CompressionChar_Base85: Char = '_';

  Coefficients_Base85: array[0..4] of UInt32 = (52200625,614125,7225,85,1);

{===============================================================================
    TBase85Encoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase85Encoder - protected methods
-------------------------------------------------------------------------------}

procedure TBase85Encoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_Base85));
fCompressionChar := CompressionChar_Base85;
end;

//------------------------------------------------------------------------------

procedure TBase85Encoder.EncodeSpecific(const Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i,j:      Integer;
  Temp:     UInt32;
begin
DataPtr := ResolveDataPointer(@Buffer,Size,4);
// trawerse all words, including partial ones
For i := 1 to Ceil(Size / 4) do
  begin
    // load working word (32bits)
    If TMemSize(i * 4) > Size then
      begin
        // partial word
        Temp := 0;
        If fReversed then
        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
          Move(Pointer(PtrUInt(DataPtr) + PtrUInt(4 - (Size mod 4)))^,
               Pointer(PtrUInt(@Temp) + PtrUInt(4 - (Size mod 4)))^,Size mod 4)
        {$IFDEF FPCDWM}{$POP}W4055{$ENDIF}
        else
          Move(DataPtr^,Temp,Size mod 4);
      end
    else Temp := PUInt32(DataPtr)^;
    // swap endianness if necessary (note that default processing is big-endian)
    If {$IFNDEF ENDIAN_BIG}not{$ENDIF} Reversed then
      SwapByteOrder(Temp);
    // do the processing  
    If not((Temp = 0) and fCompression and (TMemSize(i * 4) <= Size)) then
      begin
        // word is not compressible
        For j := 0 to IfThen(fTrim,Min(4,Size - TMemSize(Pred(i) * 4)),4) do
          begin
            WriteChar(fEncodingTable[Temp div Coefficients_Base85[j]]);
            Temp := Temp mod Coefficients_Base85[j];
          end;
      end
    else WriteChar(fCompressionChar);
    If fBreakProcessing then
      Break{For i};
    AdvanceDataPointer(DataPtr,4);
  end;
end;

{-------------------------------------------------------------------------------
    TBase85Encoder - public methods
-------------------------------------------------------------------------------}

class Function TBase85Encoder.DataLimit: TMemSize;
begin
// in worst-case scanario, four bytes are encoded to five characters
Result := 409 * 1024 * 1024;  // 409MiB
end;

//------------------------------------------------------------------------------

class Function TBase85Encoder.Encoding: TBTEEncoding;
begin
Result := encBase85;
end;

//------------------------------------------------------------------------------

class Function TBase85Encoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efCompression,efTrim,efSlowOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase85Encoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base85);
end;

//------------------------------------------------------------------------------

class Function TBase85Encoder.EncodingTableIsValid(const EncodingTable: TBTEEncodingTable): Boolean;
var
  i:  Integer;
begin
If inherited EncodingTableIsValid(EncodingTable) then
  begin
    Result := True;
    For i := Low(EncodingTable) to High(EncodingTable) do
      If (Ord(EncodingTable[i]) <= 32) or (Ord(EncodingTable[i]) >= 127) then
        begin
          Result := False;
          Break{For i};
        end;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TBase85Encoder.EncodedLengthFromBuffer(const Buffer; Size: TMemSize): TStrSize;

  Function CountCompressibleWords: Integer;
  var
    DataPtr:  Pointer;
    i:        Integer;
  begin
    Result := 0;
    DataPtr := ResolveDataPointer(@Buffer,Size,4);
    For i := 1 to (Size div 4) do
      begin
        If PUInt32(DataPtr)^ = 0 then
          Inc(Result);
        AdvanceDataPointer(DataPtr,4);
      end;
  end;

begin
// single character encodes about 6.41 bits from binary data (discounting compression)
If Size <= DataLimit then
  begin
    If fTrim then
      Result := TStrSize(Ceil(Size / 4) + Integer(Size))
    else
      Result := TStrSize(Ceil(Size / 4) * 5);
    If fCompression then
      Result := Result - (CountCompressibleWords * 4);
    If fHeader then
      Result := Result + HeaderLength;
  end
else raise EBTETooMuchData.CreateFmt('TBase85Encoder.EncodedLengthFromBuffer: Too much data (%u bytes).',[Size]);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TBase85Decoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_Base85: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $44, $FF, $54, $53, $52, $48, $FF, $4B, $4C, $46, $41, $FF, $3F, $3E, $45,
     $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $40, $FF, $49, $42, $4A, $47,
     $51, $24, $25, $26, $27, $28, $29, $2A, $2B, $2C, $2D, $2E, $2F, $30, $31, $32,
     $33, $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $4D, $FF, $4E, $43, $FF,
     $FF, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18,
     $19, $1A, $1B, $1C, $1D, $1E, $1F, $20, $21, $22, $23, $4F, $FF, $50, $FF, $FF);
     
{===============================================================================
    TBase85Decoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TBase85Decoder - protected methods
-------------------------------------------------------------------------------}

Function TBase85Decoder.ReadChar: Char;
begin
// ignore control chars and chars out of alloved range
repeat
  Result := inherited ReadChar;
until (Ord(Result) > 32) and (Ord(Result) < 127);
end;

//------------------------------------------------------------------------------

procedure TBase85Decoder.InitializeTable;
begin
fDecodingTable := DecodingTable_Base85;
fCompressionChar := CompressionChar_Base85;
end;

//------------------------------------------------------------------------------

procedure TBase85Decoder.DecodeSpecific(out Buffer; Size: TMemSize);
var
  DataPtr:  Pointer;
  i,j:      Integer;
  C:        Char;
  Temp:     UInt32;
  Temp64:   Int64;
begin
DataPtr := ResolveDataPointer(@Buffer,Size,4);
For i := 1 to Ceil(Size / 4) do
  begin
    C := ReadChar;
    If C <> fCompressionChar then
      begin
        Temp64 := 0;
        RollBack;
        If TMemSize(i * 4) > Size then
          begin
            // > potentially < trimmed word
            For j := 0 to (Size mod 4) do
              begin
                C := ReadChar;
                Temp64 := Temp64 + (Int64(ResolveChar(C)) * Coefficients_Base85[j]);
              end;
            For j := Succ(Size mod 4) to 4 do
              Temp64 := Temp64 + (Int64(84) * Coefficients_Base85[j]);
          end
        else
          begin
            // untrimmed word (ie. there is all five characters)
            For j := 0 to 4 do
              begin
                C := ReadChar;
                Temp64 := Temp64 + (Int64(ResolveChar(C)) * Coefficients_Base85[j]);
              end;
          end;
        // check validity of the encoded word
        If Temp64 <= High(UInt32) then
          Temp := UInt32(Temp64)
        else
          raise EBTEProcessingError.Create('TBase85Decoder.DecodeSpecific: Encoded value too large.');
      end
    else Temp := 0;
    // swap endianness if necessary
    If {$IFNDEF ENDIAN_BIG}not{$ENDIF} Reversed then
      SwapByteOrder(Temp);
    // do output
    If TMemSize(i * 4) > Size then
      begin
        If fReversed then
        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
          Move(Pointer(PtrUInt(@Temp) + PtrUInt(4 - (Size mod 4)))^,
               Pointer(PtrUInt(DataPtr) + PtrUInt(4 - (Size mod 4)))^,Size mod 4)
        {$IFDEF FPCDWM}{$POP}{$ENDIF}
        else
          Move(Temp,DataPtr^,Size mod 4);
      end
    else PUInt32(DataPtr)^ := Temp;
    If fBreakProcessing then
      Break{For i};    
    AdvanceDataPointer(DataPtr,4);
  end;
end;

{-------------------------------------------------------------------------------
    TBase85Decoder - public methods
-------------------------------------------------------------------------------}

class Function TBase85Decoder.DataLimit: TMemSize;
begin
Result := 409 * 1024 * 1024;  // 409MiB
end;

//------------------------------------------------------------------------------

class Function TBase85Decoder.Encoding: TBTEEncoding;
begin
Result := encBase85;
end;

//------------------------------------------------------------------------------

class Function TBase85Decoder.EncodingFeatures: TBTEEncodingFeatures;
begin
Result := [efReversible,efCompression,efTrim,efSlowOutputSize,efInpreciseOutputSize];
end;

//------------------------------------------------------------------------------

class Function TBase85Decoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_Base85);
end;

//------------------------------------------------------------------------------

class Function TBase85Decoder.EncodingTableIsValid(const EncodingTable: TBTEEncodingTable): Boolean;
var
  i:  Integer;
begin
If inherited EncodingTableIsValid(EncodingTable) then
  begin
    Result := True;
    For i := Low(EncodingTable) to High(EncodingTable) do
      If (Ord(EncodingTable[i]) <= 32) or (Ord(EncodingTable[i]) >= 127) then
        begin
          Result := False;
          Break{For i};
        end;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function TBase85Decoder.DecodedSize: TMemSize;

  Function Correction: TStrSize;
  var
    i:  TStrOff;
  begin
    Result := 0;
    fCompression := False;
    For i := 1 to Length(fEncodedString) do
      begin
        If fEncodedString[i] = fCompressionChar then
          begin
            fCompression := True;
            Inc(Result,4);
          end
        else If (Ord(fEncodedString[i]) <= 32) or (Ord(fEncodedString[i]) >= 127) then
          Dec(Result);
      end;
  end;

var
  FullLength: TStrSize;
begin
If HeaderPresent(fEncodedString) then
  FullLength := Length(fEncodedString) - HeaderLength + Correction
else
  FullLength := Length(fEncodedString) + Correction;
Result := TMemSize(FullLength - Ceil(FullLength / 5));
fTrim := FullLength mod 5 > 1;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TASCII85Encoder
--------------------------------------------------------------------------------
===============================================================================}
const
  EncodingTable_ASCII85: array[0..84] of Char =
    ('!','"','#','$','%','&','''','(',')','*','+',',','-','.','/','0',
     '1','2','3','4','5','6','7','8','9',':',';','<','=','>','?','@',
     'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
     'Q','R','S','T','U','V','W','X','Y','Z','[','\',']','^','_','`',
     'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p',
     'q','r','s','t','u');

  CompressionChar_ASCII85: Char = 'z';

{===============================================================================
    TASCII85Encoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TASCII85Encoder - protected methods
-------------------------------------------------------------------------------}

procedure TASCII85Encoder.InitializeTable;
begin
AssignEncodingTable(CreateEncodingTable(EncodingTable_ASCII85));
fCompressionChar := CompressionChar_ASCII85;
end;

{-------------------------------------------------------------------------------
    TASCII85Encoder - public methods
-------------------------------------------------------------------------------}

class Function TASCII85Encoder.Encoding: TBTEEncoding;
begin
Result := encASCII85;
end;

//------------------------------------------------------------------------------

class Function TASCII85Encoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_ASCII85);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TASCII85Decoder
--------------------------------------------------------------------------------
===============================================================================}
const
  DecodingTable_ASCII85: TBTEDecodingTable =
    ($FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
     $FF, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E,
     $0F, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1A, $1B, $1C, $1D, $1E,
     $1F, $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2A, $2B, $2C, $2D, $2E,
     $2F, $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E,
     $3F, $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4A, $4B, $4C, $4D, $4E,
     $4F, $50, $51, $52, $53, $54, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF);
     
{===============================================================================
    TASCII85Decoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TASCII85Decoder - protected methods
-------------------------------------------------------------------------------}

procedure TASCII85Decoder.InitializeTable;
begin
fDecodingTable := DecodingTable_ASCII85;
fCompressionChar := CompressionChar_ASCII85;
end;

{-------------------------------------------------------------------------------
    TASCII85Decoder - public methods
-------------------------------------------------------------------------------}

class Function TASCII85Decoder.Encoding: TBTEEncoding;
begin
Result := encASCII85;
end;

//------------------------------------------------------------------------------

class Function TASCII85Decoder.EncodingTableLength: Integer;
begin
Result := Length(EncodingTable_ASCII85);
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Procedural interface
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Procedural interface - specialized functions implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Procedural interface - Base2 encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base2(const Buffer; Size: TMemSize; Header: Boolean = False): TStrSize;
var
  Encoder:  TBase2Encoder;
begin
Encoder := TBase2Encoder.Create;
try
  Encoder.Header := Header;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Encode_Base2(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False): String;
var
  Encoder:  TBase2Encoder;
begin
Encoder := TBase2Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_Base2(const EncodedString: String): TMemSize;
var
  Decoder:  TBase2Decoder;
begin
Decoder := TBase2Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Decode_Base2(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase2Decoder;
begin
Decoder := TBase2Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - Base4 encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base4(const Buffer; Size: TMemSize; Header: Boolean = False): TStrSize;
var
  Encoder:  TBase4Encoder;
begin
Encoder := TBase4Encoder.Create;
try
  Encoder.Header := Header;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Encode_Base4(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False): String;
var
  Encoder:  TBase4Encoder;
begin
Encoder := TBase4Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_Base4(const EncodedString: String): TMemSize;
var
  Decoder:  TBase4Decoder;
begin
Decoder := TBase4Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Decode_Base4(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase4Decoder;
begin
Decoder := TBase4Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - Base8 encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base8(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
var
  Encoder:  TBase8Encoder;
begin
Encoder := TBase8Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Padding := Padding;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Encode_Base8(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;
var
  Encoder:  TBase8Encoder;
begin
Encoder := TBase8Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.Padding := Padding;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_Base8(const EncodedString: String): TMemSize;
var
  Decoder:  TBase8Decoder;
begin
Decoder := TBase8Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function Decode_Base8(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase8Decoder;
begin
Decoder := TBase8Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - Base10 encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base10(const Buffer; Size: TMemSize; Header: Boolean = False): TStrSize;
var
  Encoder:  TBase10Encoder;
begin
Encoder := TBase10Encoder.Create;
try
  Encoder.Header := Header;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Encode_Base10(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False): String;
var
  Encoder:  TBase10Encoder;
begin
Encoder := TBase10Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_Base10(const EncodedString: String): TMemSize;
var
  Decoder:  TBase10Decoder;
begin
Decoder := TBase10Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Decode_Base10(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase10Decoder;
begin
Decoder := TBase10Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - Base16 encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base16(const Buffer; Size: TMemSize; Header: Boolean = False): TStrSize;
var
  Encoder:  TBase16Encoder;
begin
Encoder := TBase16Encoder.Create;
try
  Encoder.Header := Header;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Encode_Base16(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False): String;
var
  Encoder:  TBase16Encoder;
begin
Encoder := TBase16Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_Base16(const EncodedString: String): TMemSize;
var
  Decoder:  TBase16Decoder;
begin
Decoder := TBase16Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Decode_Base16(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase16Decoder;
begin
Decoder := TBase16Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - Base32 encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base32(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
var
  Encoder:  TBase32Encoder;
begin
Encoder := TBase32Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Padding := Padding;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Encode_Base32(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;
var
  Encoder:  TBase32Encoder;
begin
Encoder := TBase32Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.Padding := Padding;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function DecodedSize_Base32(const EncodedString: String): TMemSize;
var
  Decoder:  TBase32Decoder;
begin
Decoder := TBase32Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function Decode_Base32(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase32Decoder;
begin
Decoder := TBase32Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - Base32Hex encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base32Hex(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
var
  Encoder:  TBase32HexEncoder;
begin
Encoder := TBase32HexEncoder.Create;
try
  Encoder.Header := Header;
  Encoder.Padding := Padding;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Encode_Base32Hex(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;
var
  Encoder:  TBase32HexEncoder;
begin
Encoder := TBase32HexEncoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.Padding := Padding;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_Base32Hex(const EncodedString: String): TMemSize;
var
  Decoder:  TBase32HexDecoder;
begin
Decoder := TBase32HexDecoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Decode_Base32Hex(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase32HexDecoder;
begin
Decoder := TBase32HexDecoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - Base64 encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base64(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
var
  Encoder:  TBase64Encoder;
begin
Encoder := TBase64Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Padding := Padding;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Encode_Base64(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;
var
  Encoder:  TBase64Encoder;
begin
Encoder := TBase64Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.Padding := Padding;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_Base64(const EncodedString: String): TMemSize;
var
  Decoder:  TBase64Decoder;
begin
Decoder := TBase64Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Decode_Base64(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase64Decoder;
begin
Decoder := TBase64Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - Base64URL encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base64URL(const Buffer; Size: TMemSize; Header: Boolean = False; Padding: Boolean = False): TStrSize;
var
  Encoder:  TBase64URLEncoder;
begin
Encoder := TBase64URLEncoder.Create;
try
  Encoder.Header := Header;
  Encoder.Padding := Padding;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Encode_Base64URL(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Padding: Boolean = False): String;
var
  Encoder:  TBase64URLEncoder;
begin
Encoder := TBase64URLEncoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.Padding := Padding;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_Base64URL(const EncodedString: String): TMemSize;
var
  Decoder:  TBase64URLDecoder;
begin
Decoder := TBase64URLDecoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Decode_Base64URL(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase64URLDecoder;
begin
Decoder := TBase64URLDecoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - Base85 encoding
-------------------------------------------------------------------------------}

Function EncodedLength_Base85(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): TStrSize;
var
  Encoder:  TBase85Encoder;
begin
Encoder := TBase85Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.Compression := Compression;
  Encoder.Trim := Trim;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function Encode_Base85(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): String;
var
  Encoder:  TBase85Encoder;
begin
Encoder := TBase85Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.Compression := Compression;
  Encoder.Trim := Trim;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_Base85(const EncodedString: String): TMemSize;
var
  Decoder:  TBase85Decoder;
begin
Decoder := TBase85Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Decode_Base85(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TBase85Decoder;
begin
Decoder := TBase85Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{-------------------------------------------------------------------------------
    Procedural interface - ASCII85 encoding
-------------------------------------------------------------------------------}

Function EncodedLength_ASCII85(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): TStrSize;
var
  Encoder:  TASCII85Encoder;
begin
Encoder := TASCII85Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.Compression := Compression;
  Encoder.Trim := Trim;
  Result := Encoder.EncodedLengthFromBuffer(Buffer,Size);
finally
  Encoder.Free;
end;
end;
 
//------------------------------------------------------------------------------

Function Encode_ASCII85(const Buffer; Size: TMemSize; Header: Boolean = False; Reversed: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): String;
var
  Encoder:  TASCII85Encoder;
begin
Encoder := TASCII85Encoder.Create;
try
  Encoder.Header := Header;
  Encoder.Reversed := Reversed;
  Encoder.Compression := Compression;
  Encoder.Trim := Trim;
  Encoder.EncodeFromBuffer(Buffer,Size);
  Result := Encoder.EncodedString;
finally
  Encoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize_ASCII85(const EncodedString: String): TMemSize;
var
  Decoder:  TASCII85Decoder;
begin
Decoder := TASCII85Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Result := Decoder.DecodedSize;
finally
  Decoder.Free;
end;
end;

//------------------------------------------------------------------------------

Function Decode_ASCII85(const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
var
  Decoder:  TASCII85Decoder;
begin
Decoder := TASCII85Decoder.Create;
try
  Decoder.EncodedString := EncodedString;
  Decoder.Reversed := Reversed;
  Result := Decoder.DecodeIntoBuffer(Buffer,Size);
finally
  Decoder.Free;
end;
end;

{===============================================================================
    Procedural interface - universal functions implementation
===============================================================================}

Function EncodedLength(Encoding: TBTEEncoding; const Buffer; Size: TMemSize; Header: Boolean = False;
  Reversed: Boolean = False; Padding: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): TStrSize;
begin
case Encoding of
  encBase2:     Result := EncodedLength_Base2(Buffer,Size,Header);
  encBase4:     Result := EncodedLength_Base4(Buffer,Size,Header);
  encBase8:     Result := EncodedLength_Base8(Buffer,Size,Header,Padding);
  encBase10:    Result := EncodedLength_Base10(Buffer,Size,Header);
  encBase16:    Result := EncodedLength_Base16(Buffer,Size,Header);
  encBase32:    Result := EncodedLength_Base32(Buffer,Size,Header,Padding);
  encBase32Hex: Result := EncodedLength_Base32Hex(Buffer,Size,Header,Padding);
  encBase64:    Result := EncodedLength_Base64(Buffer,Size,Header,Padding);
  encBase64URL: Result := EncodedLength_Base64URL(Buffer,Size,Header,Padding);
  encBase85:    Result := EncodedLength_Base85(Buffer,Size,Header,Reversed,Compression,Trim);
  encASCII85:   Result := EncodedLength_ASCII85(Buffer,Size,Header,Reversed,Compression,Trim);
else
  raise EBTEInvalidValue.CreateFmt('EncodedLength: Unknown encoding (%d).',[Ord(Encoding)]);
end;
end;

//------------------------------------------------------------------------------

Function Encode(Encoding: TBTEEncoding; const Buffer; Size: TMemSize; Header: Boolean = False;
  Reversed: Boolean = False; Padding: Boolean = False; Compression: Boolean = False; Trim: Boolean = False): String;
begin
case Encoding of
  encBase2:     Result := Encode_Base2(Buffer,Size,Header,Reversed);
  encBase4:     Result := Encode_Base4(Buffer,Size,Header,Reversed);
  encBase8:     Result := Encode_Base8(Buffer,Size,Header,Reversed,Padding);
  encBase10:    Result := Encode_Base10(Buffer,Size,Header,Reversed);
  encBase16:    Result := Encode_Base16(Buffer,Size,Header,Reversed);
  encBase32:    Result := Encode_Base32(Buffer,Size,Header,Reversed,Padding);
  encBase32Hex: Result := Encode_Base32Hex(Buffer,Size,Header,Reversed,Padding);
  encBase64:    Result := Encode_Base64(Buffer,Size,Header,Reversed,Padding);
  encBase64URL: Result := Encode_Base64URL(Buffer,Size,Header,Reversed,Padding);
  encBase85:    Result := Encode_Base85(Buffer,Size,Header,Reversed,Compression,Trim);
  encASCII85:   Result := Encode_ASCII85(Buffer,Size,Header,Reversed,Compression,Trim);
else
  raise EBTEInvalidValue.CreateFmt('Encode: Unknown encoding (%d).',[Ord(Encoding)]);
end;
end;

//------------------------------------------------------------------------------

Function DecodedSize(Encoding: TBTEEncoding; const EncodedString: String): TMemSize;
begin
case Encoding of
  encBase2:     Result := DecodedSize_Base2(EncodedString);
  encBase4:     Result := DecodedSize_Base4(EncodedString);
  encBase8:     Result := DecodedSize_Base8(EncodedString);
  encBase10:    Result := DecodedSize_Base10(EncodedString);
  encBase16:    Result := DecodedSize_Base16(EncodedString);
  encBase32:    Result := DecodedSize_Base32(EncodedString);
  encBase32Hex: Result := DecodedSize_Base32Hex(EncodedString);
  encBase64:    Result := DecodedSize_Base64(EncodedString);
  encBase64URL: Result := DecodedSize_Base64URL(EncodedString);
  encBase85:    Result := DecodedSize_Base85(EncodedString);
  encASCII85:   Result := DecodedSize_ASCII85(EncodedString);
else
  raise EBTEInvalidValue.CreateFmt('DecodedSize: Unknown encoding (%d).',[Ord(Encoding)]);
end;
end;

//------------------------------------------------------------------------------

Function Decode(Encoding: TBTEEncoding; const EncodedString: String; out Buffer; Size: TMemSize; Reversed: Boolean = False): TMemSize;
begin
case Encoding of
  encBase2:     Result := Decode_Base2(EncodedString,Buffer,Size,Reversed);
  encBase4:     Result := Decode_Base4(EncodedString,Buffer,Size,Reversed);
  encBase8:     Result := Decode_Base8(EncodedString,Buffer,Size,Reversed);
  encBase10:    Result := Decode_Base10(EncodedString,Buffer,Size,Reversed);
  encBase16:    Result := Decode_Base16(EncodedString,Buffer,Size,Reversed);
  encBase32:    Result := Decode_Base32(EncodedString,Buffer,Size,Reversed);
  encBase32Hex: Result := Decode_Base32Hex(EncodedString,Buffer,Size,Reversed);
  encBase64:    Result := Decode_Base64(EncodedString,Buffer,Size,Reversed);
  encBase64URL: Result := Decode_Base64URL(EncodedString,Buffer,Size,Reversed);
  encBase85:    Result := Decode_Base85(EncodedString,Buffer,Size,Reversed);
  encASCII85:   Result := Decode_ASCII85(EncodedString,Buffer,Size,Reversed);
else
  raise EBTEInvalidValue.CreateFmt('Decode: Unknown encoding (%d).',[Ord(Encoding)]);
end;
end;

//------------------------------------------------------------------------------

Function HeaderEncoding(const EncodedString: String): TBTEEncoding;
begin
Result := TBTETranscoder.HeaderEncoding(EncodedString);
end;

//------------------------------------------------------------------------------

Function DecodedSize(const EncodedString: String): TMemSize;
begin
If TBTETranscoder.HeaderPresent(EncodedString) then
  Result := DecodedSize(TBTETranscoder.HeaderEncoding(EncodedString),EncodedString)
else
  raise EBTEInvalidValue.Create('DecodedSize: Encoded string does not contain a valid header.');
end;

//------------------------------------------------------------------------------

Function Decode(const EncodedString: String; out Buffer; Size: TMemSize): TMemSize;
begin
If TBTETranscoder.HeaderPresent(EncodedString) then
  Result := Decode(TBTETranscoder.HeaderEncoding(EncodedString),EncodedString,Buffer,Size)
else
  raise EBTEInvalidValue.Create('Decode: Encoded string does not contain a valid header.');
end;

//------------------------------------------------------------------------------

Function EncoderFromEncoding(Encoding: TBTEEncoding): TBTEEncoderClass;
begin
case Encoding of
  encBase2:     Result := TBase2Encoder;
  encBase4:     Result := TBase4Encoder;
  encBase8:     Result := TBase8Encoder;
  encBase10:    Result := TBase10Encoder;
  encBase16:    Result := TBase16Encoder;
  encBase32:    Result := TBase32Encoder;
  encBase32Hex: Result := TBase32HexEncoder;
  encBase64:    Result := TBase64Encoder;
  encBase64URL: Result := TBase64URLEncoder;
  encBase85:    Result := TBase85Encoder;
  encASCII85:   Result := TASCII85Encoder;
else
  raise EBTEInvalidValue.CreateFmt('EncoderFromEncoding: Unknown encoding (%d).',[Ord(Encoding)]);
end;
end;

//------------------------------------------------------------------------------

Function DecoderFromEncoding(Encoding: TBTEEncoding): TBTEDecoderClass;
begin
case Encoding of
  encBase2:     Result := TBase2Decoder;
  encBase4:     Result := TBase4Decoder;
  encBase8:     Result := TBase8Decoder;
  encBase10:    Result := TBase10Decoder;
  encBase16:    Result := TBase16Decoder;
  encBase32:    Result := TBase32Decoder;
  encBase32Hex: Result := TBase32HexDecoder;
  encBase64:    Result := TBase64Decoder;
  encBase64URL: Result := TBase64URLDecoder;
  encBase85:    Result := TBase85Decoder;
  encASCII85:   Result := TASCII85Decoder;
else
  raise EBTEInvalidValue.CreateFmt('DecoderFromEncoding: Unknown encoding (%d).',[Ord(Encoding)]);
end;
end;

end.
