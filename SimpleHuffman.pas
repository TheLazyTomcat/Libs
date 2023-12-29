{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  SimpleHuffman

    Provides classes for encoding and decoding of data using a Huffman tree,
    as a method of loss-less data compression.

    Note that this entire library was written "in the blind" - that is, with no
    access to internet or any other relevant information source.
    Current implementation is based pretty much only on a short video describing
    how the Huffman tree works, that is all.
    Therefore, the implementation is naive, used nomenclature is completely
    arbitrary, there are no performance optimizations and it is probably bugged.
    It was written only as a product of curiosity and should be seen as such.

  Version 1.0 alpha (2023-05-09)

  Last change 2023-12-29

  ©2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleHuffman

  Dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    BasicUIM           - github.com/TheLazyTomcat/Lib.BasicUIM
    BitOps             - github.com/TheLazyTomcat/Lib.BitOps
  * SimpleCPUID        - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect

  SimpleCPUID might not be needed, see BitOps library for details.

===============================================================================}
unit SimpleHuffman;

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------
{
  UseSecureTreeTraversal

  When defined, decoding and decoded size obtaining uses more secure, but much
  slower implementation.

  Not defined by default.

  To enable/define this symbol in a project without changing this library,
  define project-wide symbol SimpleHuffman_UseSecureTreeTraversal_ON.
}
{$UNDEF UseSecureTreeTraversal}
{$IFDEF SimpleHuffman_UseSecureTreeTraversal_ON}
  {$DEFINE UseSecureTreeTraversal}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESHException = class(Exception);

  ESHInvalidValue     = class(ESHException);
  ESHInvalidState     = class(ESHException);
  ESHInvalidOperation = class(ESHException);
  ESHIndexOutOfBounds = class(ESHException);
  ESHBufferTooSmall   = class(ESHException);

{===============================================================================
--------------------------------------------------------------------------------
                                  THuffmanTree
--------------------------------------------------------------------------------
===============================================================================}
const
  SH_HUFFTREE_LIST_BYTENODES = 0;
  SH_HUFFTREE_LIST_TREENODES = 1;

type
  TSHNodeKind = (nkByteSaved,nkByteUnsaved,nkBranch,nkTerminator);

  // 256 bits, anyway what is the worst-case scenario?
  TSHBitSequenceData = packed array[0..31] of UInt8;

  TSHBitSequence = record
    Length: TMemSize;
    Data:   TSHBitSequenceData;
  end;

  PSHHuffmanTreeNode = ^TSHHuffmanTreeNode;
  TSHHuffmanTreeNode = record
    NodeKind:     TSHNodeKind;
    ByteIndex:    UInt8;
    TreeIndex:    Integer;
    Frequency:    Int64;
    BitSequence:  TSHBitSequence;
    ParentNode:   PSHHuffmanTreeNode;
    ParentPath:   Boolean;
    ChildNodes:   array[Boolean] of PSHHuffmanTreeNode;
    SiblingNode:  PSHHuffmanTreeNode;
  end;

type
  TSHTreeSavingScheme = (tssFullFreq,tssAvrgDiff);

  TSHTreeSavingFlag = (tsfNone);  // none implemented atm.

  TSHTreeSavingFlags = set of TSHTreeSavingFlag;

{===============================================================================
    THuffmanTree - class declaration
===============================================================================}
type
  THuffmanTree = class(TCustomMultiListObject)
  protected
    fByteNodes:       array[UInt8] of TSHHuffmanTreeNode;
    fTerminatorNode:  TSHHuffmanTreeNode;    
    fTreeNodes:       array of PSHHuffmanTreeNode;
    fTreeNodeCount:   Integer;
    fRootNode:        PSHHuffmanTreeNode;
    // getters, setters
    Function GetByteNode(Index: Integer): TSHHuffmanTreeNode; virtual;
    Function GetFrequency(Index: Integer): Int64; virtual;
    procedure SetFrequency(Index: Integer; Value: Int64); virtual;
    Function GetBitSequence(Index: Integer): TSHBitSequence; virtual;
    Function GetNodeSaved(Index: Integer): Boolean; virtual;
    procedure SetNodeSaved(Index: Integer; Value: Boolean); virtual;
    Function GetTreeNodePtr(Index: Integer): PSHHuffmanTreeNode; virtual;    
    Function GetTreeNode(Index: Integer): TSHHuffmanTreeNode; virtual;
    // inherited protected list methods
    Function GetCapacity(List: Integer): Integer; override;
    procedure SetCapacity(List,Value: Integer); override;
    Function GetCount(List: Integer): Integer; override;
    procedure SetCount(List,Value: Integer); override;
    // tree building
    Function AddTreeNode(Node: PSHHuffmanTreeNode): Integer; virtual;
  {
    streaming

      *_0 ... tssFullFreq
      *_1 ... tssAvrgDiff
  }
    Function CountSavedByteNodes: Integer; virtual;
    Function EncodeHeader(Scheme: TSHTreeSavingScheme; Flags: TSHTreeSavingFlags): UInt8; virtual;
    procedure DecodeHeader(Header: UInt8; out Scheme: TSHTreeSavingScheme; out Flags: TSHTreeSavingFlags); virtual;
    Function PreloadAllocationSize(Stream: TStream): TMemSize; virtual;
    Function StreamingSize_0(out FreqBits: Integer): TMemSize; virtual;
    Function StreamingSize_1(out AvrgFreq: Int64; out AvrgBits,DiffBits: Integer): TMemSize; virtual;
    procedure SaveToBuffer_0(out Buffer; Size: TMemSize); virtual;
    procedure SaveToBuffer_1(out Buffer; Size: TMemSize); virtual;
    procedure LoadFromBuffer_0(const Buffer; Size: TMemSize); virtual;
    procedure LoadFromBuffer_1(const Buffer; Size: TMemSize); virtual;  
    // initialization and finalization
    procedure ClearByteNodes; virtual;
    procedure ClearTreeNodes; virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // inherited public list methods
    Function LowIndex(List: Integer): Integer; override;
    Function HighIndex(List: Integer): Integer; override;
    // new list methods
    Function LowByteNodeIndex: Integer; virtual;
    Function HighByteNodeIndex: Integer; virtual;
    Function CheckByteNodeIndex(Index: UInt8): Boolean; virtual;
    Function LowTreeNodeIndex: Integer; virtual;
    Function HighTreeNodeIndex: Integer; virtual;
    Function CheckTreeNodeIndex(Index: Integer): Boolean; virtual;
    // frequency methods
    Function IncreaseFrequency(ByteIndex: UInt8): Int64; virtual;    
    // tree methods
    Function TreeIsReady: Boolean; virtual;
    procedure ConstructTree; virtual;
    procedure BuildTree(Frequencies: array of Int64); virtual;
    procedure CopyTree(Tree: THuffmanTree); virtual;
    Function SameTree(Tree: THuffmanTree): Boolean; virtual;
    procedure ClearTree; virtual;
  {
    TraverseTree

    Set TreeNodeIndex to an invalid (eg. negative) value before first call to
    TraverseTree.

    Returns true when returned TreeNodeIndex points to a branch node, false
    otherwise.
  }
    Function TraverseTree(BitPath: Boolean; var TreeNodeIndex: Integer): Boolean; virtual;
    // streaming (saving, loading)

    Function StreamingSize(Scheme: TSHTreeSavingScheme = tssFullFreq): TMemSize; virtual;
    Function BestStreamingSize(out Scheme: TSHTreeSavingScheme): TMemSize; virtual;

    procedure SaveToBuffer(out Buffer; Size: TMemSize; Scheme: TSHTreeSavingScheme = tssFullFreq); virtual;
    procedure SaveToStream(Stream: TStream; Scheme: TSHTreeSavingScheme = tssFullFreq); virtual;
    procedure SaveToFile(const FileName: String; Scheme: TSHTreeSavingScheme = tssFullFreq); virtual;

    procedure LoadFromBuffer(const Buffer; Size: TMemSize; out Scheme: TSHTreeSavingScheme); overload; virtual;
    procedure LoadFromBuffer(const Buffer; Size: TMemSize); overload; virtual;
    procedure LoadFromStream(Stream: TStream; out Scheme: TSHTreeSavingScheme); overload; virtual;
    procedure LoadFromStream(Stream: TStream); overload; virtual;
    procedure LoadFromFile(const FileName: String; out Scheme: TSHTreeSavingScheme); overload; virtual;
    procedure LoadFromFile(const FileName: String); overload; virtual;
  {
    properties

    Use tree node indices (eg. LowTreeNodeIndex) for properties TreeNodePtrs
    and TreeNodes, for all other array properties use byte indices
    (LowByteNodeIndex, ...).
  }
    property ByteNodes[Index: Integer]: TSHHuffmanTreeNode read GetByteNode; default;
    property Frequencies[Index: Integer]: Int64 read GetFrequency write SetFrequency;
    property BitSequences[Index: Integer]: TSHBitSequence read GetBitSequence;
  {
    NodeSaved[]

    When working with a data that can only contain a limited set of bytes, it
    might be desirable to not save full frequency list, only what is needed.
    You can do so by setting NodeSaved property of bytes that are not present
    to false (they all are true by default).

      WARNING - remember to do the same before loading a saved tree.
  }
    property NodeSaved[Index: Integer]: Boolean read GetNodeSaved write SetNodeSaved;
    property TreeNodePtrs[Index: Integer]: PSHHuffmanTreeNode read GetTreeNodePtr;    
    property TreeNodes[Index: Integer]: TSHHuffmanTreeNode read GetTreeNode;
    property TerminatorNode: TSHHuffmanTreeNode read fTerminatorNode;
    property RootNode: PSHHuffmanTreeNode read fRootNode;
    property ByteNodeCount: Integer index SH_HUFFTREE_LIST_BYTENODES read GetCount;
    property TreeNodeCount: Integer index SH_HUFFTREE_LIST_TREENODES read GetCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                  THuffmanBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    THuffmanBase - class declaration
===============================================================================}
type
  THuffmanBase = class(TCustomObject)
  protected
    fHuffmanTree:           THuffmanTree;
    fStreamBufferSize:      TMemSize;
    fUncompressedSize:      Int64;
    fCompressedSize:        Int64;
    fCompressionRatio:      Double;
    fBreakProcessing:       Boolean;
    fScanInitialized:       Boolean;
    fScanFinalized:         Boolean;
    fScanProgressCallback:  TProgressCallback;
    fScanProgressEvent:     TProgressEvent;
    procedure DoScanProgress(Progress: Double); virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    class Function StreamBufferSizeDefault: TMemSize; virtual;
    constructor Create;
    destructor Destroy; override;
  {
    BreakProcessing

    You can call this method from within progress event or callback to break
    out from a long processing.

    This affects scanning (Scan*), encoded size obtaining (EncodedSize*),
    encoding (Encode*), decoded size obtaining (DecodedSize*) and decoding
    (Decode*).
  }
    Function BreakProcessing: Boolean; virtual;
  {
    scanning

    Before encoding or decoding, it is necessary to provide a complete huffman
    tree. For decoding, this is often done by loading a tree saved in encoding.
    For encoding, the tree usually is not available in pre-computed form, but
    must be computed for the specific data being encoded. Scanning is here for
    this purpose.
    Pass all data that are to be encoded to the scanning, and the tree will be
    constructed from them. Also, the scanning returns number of bytes the data
    will occupy when encoded.
  }
    procedure ScanInit; virtual;
    procedure ScanUpdate(const Buffer; Size: TMemSize); virtual;
    Function ScanFinal: Int64; virtual;
    // scanning macros
    Function ScanMemory(Memory: Pointer; Size: TMemSize): Int64; virtual;
    Function ScanBuffer(const Buffer; Size: TMemSize): Int64; virtual;
    Function ScanAnsiString(const Str: AnsiString): Int64; virtual;
    Function ScanWideString(const Str: WideString): Int64; virtual;
    Function ScanString(const Str: String): Int64; virtual;
    Function ScanStream(Stream: TStream; Count: Int64 = -1): Int64; virtual;
    Function ScanFile(const FileName: String): Int64; virtual;
    // properties
    property HuffmanTree: THuffmanTree read fHuffmanTree;
    property StreamBufferSize: TMemSize read fStreamBufferSize write fStreamBufferSize;
    property UncompressedSize: Int64 read fUncompressedSize;
    property CompressedSize: Int64 read fCompressedSize;
    property CompressionRatio: Double read fCompressionRatio;
    // events
    property OnScanProgressCallback: TProgressCallback read fScanProgressCallback write fScanProgressCallback;
    property OnScanProgressEvent: TProgressEvent read fScanProgressEvent write fScanProgressEvent;
    property OnScanProgress: TProgressEvent read fScanProgressEvent write fScanProgressEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 THuffmanEncoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    THuffmanEncoder - class declaration
===============================================================================}
type
  THuffmanEncoder = class(THuffmanBase)
  protected
    fEncodedSizeInitialized:      Boolean;
    fEncodedSizeFinalized:        Boolean;
    fEncodedSizeCounters:         array[UInt8] of Int64;
    fEncodedSizeProgressCallback: TProgressCallback;
    fEncodedSizeProgressEvent:    TProgressEvent;
    fEncodeInitialized:           Boolean;
    fEncodeFinalized:             Boolean;
    fEncodeBuffer:                Pointer;
    fEncodeBufferBitCount:        TMemSize;
    fEncodeProgressCallback:      TProgressCallback;
    fEncodeProgressEvent:         TProgressEvent;
    procedure DoEncodedSizeProgress(Progress: Double); virtual;
    procedure DoEncodeProgress(Progress: Double); virtual;
    procedure Initialize; override;
    procedure Finalize; override;
  public
  {
    encoded size

    Use encoded size obtaining to get number of bytes the provided data will
    occupy when encoded using current huffman tree (the tree must be prepared).

    Data are usually scanned before encoding, and the scanning provides the
    encoded size too, therefore it is normally not necessaty to use encoded
    size obtaining. It is here for a case where pre-computed huffman tree is
    used for encoding, so scanning is not called, and one still needs to obtain
    the encoded size.

    Note that obtaining the encoded size is very fast, as there is little
    processing done. The performance depends pretty much only on how fast can
    the data be served.
  }
    procedure EncodedSizeInit; virtual;
    procedure EncodedSizeUpdate(const Buffer; Size: TMemSize); virtual;
    Function EncodedSizeFinal: Int64; virtual;
    // encoded size macros
    Function EncodedSizeMemory(Memory: Pointer; Size: TMemSize): Int64; virtual;
    Function EncodedSizeBuffer(const Buffer; Size: TMemSize): Int64; virtual;
    Function EncodedSizeAnsiString(const Str: AnsiString): Int64; virtual;
    Function EncodedSizeWideString(const Str: WideString): Int64; virtual;
    Function EncodedSizeString(const Str: String): Int64; virtual;
    Function EncodedSizeStream(Stream: TStream; Count: Int64 = -1): Int64; virtual;
    Function EncodedSizeFile(const FileName: String): Int64; virtual;
    // encoding
  {
    EncodeInit

    Initializes encoding.

    Huffman tree must be prepared before calling EncodeInit.
  }
    procedure EncodeInit; virtual;
  {
    EncodeUpdate

    Tries to encode as many input bytes as possible. The function returns when
    either all input bytes were consumed or when both the output buffer and
    internal processing buffer are full.

    Set SizeIn to size of the BufferIn and SizeOut to size of BufferOut (this
    buffer must be allocated by the caller).

    Upon return, the SizeIn will contain number of bytes that were consumed
    from BufferIn within the call, which might be less than was passed. SizeOut
    will contain number of bytes that were written into BufferOut (also might
    be less than specified).
  }
    procedure EncodeUpdate(const BufferIn; var SizeIn: TMemSize; out BufferOut; var SizeOut: TMemSize); virtual;
  {
    EncodeFinal

    Stores all data held in the internal processing buffer into provided output
    buffer (if they fit) and finalizes processing.

    Set SizeOut to a size of BufferOut.

    When the funtion succeeds (returns true), then the SizeOut will contain
    number of bytes written into the BufferOut.

    When the function fails (returns false), it can only be due to output
    buffer being too small. The SizeOut will then be set to a minimum size the
    output buffer needs to be (reallocate it as such and call this function
    again).
  }
    Function EncodeFinal(out BufferOut; var SizeOut: TMemSize): Boolean; virtual;
  {
    encoding macros

    WARNING - MemoryOut/BufferOut output buffers must be allocated by the
              caller and SizeOut must contain their allocated size. StrOut
              string variable parameters must also be allocated (string length
              set).
              You can get the required size, in bytes, when scanning the data,
              or, for pre-computed huffman tree, using method for encoded size
              obtaining (EncodedSize*)
  }
    procedure EncodeMemory(MemoryIn: Pointer; SizeIn: TMemSize; MemoryOut: Pointer; SizeOut: TMemSize); virtual;
    procedure EncodeBuffer(const BufferIn; SizeIn: TMemSize; out BufferOut; SizeOut: TMemSize); virtual;
    procedure EncodeAnsiString(const StrIn: AnsiString; var StrOut: AnsiString); virtual;
    procedure EncodeWideString(const StrIn: WideString; var StrOut: WideString); virtual;
    procedure EncodeString(const StrIn: String; var StrOut: String); virtual;
    procedure EncodeStream(StreamIn: TStream; CountIn: Int64; StreamOut: TStream); overload; virtual;
    procedure EncodeStream(StreamIn: TStream; StreamOut: TStream); overload; virtual;
    procedure EncodeFile(const FileNameIn,FileNameOut: String); virtual;
    // properties
    property OnEncodedSizeProgressCallback: TProgressCallback read fEncodedSizeProgressCallback write fEncodedSizeProgressCallback;
    property OnEncodedSizeProgressEvent: TProgressEvent read fEncodedSizeProgressEvent write fEncodedSizeProgressEvent;
    property OnEncodedSizeProgress: TProgressEvent read fEncodedSizeProgressEvent write fEncodedSizeProgressEvent;
    property OnEncodeProgressCallback: TProgressCallback read fEncodeProgressCallback write fEncodeProgressCallback;
    property OnEncodeProgressEvent: TProgressEvent read fEncodeProgressEvent write fEncodeProgressEvent;
    property OnEncodeProgress: TProgressEvent read fEncodeProgressEvent write fEncodeProgressEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 THuffmanDecoder
--------------------------------------------------------------------------------
===============================================================================}
type
  // for internal use only
  TSHDecodedSizeContext = record
    TreeNodeIndex:  Integer;
    Counter:        Int64;
    Terminated:     Boolean;
  end;

  // for internal use only
  TSHDecodeContext = record
    TreeNodeIndex:      Integer;
    TransferInBits:     NativeUInt;
    TransferInBitCount: TMemSize;
    TransferOutByte:    UInt8;
    TransferOutByteSet: Boolean;
    Terminated:         Boolean;
  end;

{===============================================================================
    THuffmanDecoder - class declaration
===============================================================================}
type
  THuffmanDecoder = class(THuffmanBase)
  protected
    fDecodedSizeInitialized:      Boolean;
    fDecodedSizeFinalized:        Boolean;
    fDecodedSizeContext:          TSHDecodedSizeContext;
    fDecodedSizeProgressCallback: TProgressCallback;
    fDecodedSizeProgressEvent:    TProgressEvent;
    fDecodeInitialized:           Boolean;
    fDecodeFinalized:             Boolean;
    fDecodeContext:               TSHDecodeContext;
    fDecodeProgressCallback:      TProgressCallback;
    fDecodeProgressEvent:         TProgressEvent;
    procedure DoDecodedSizeProgress(Progress: Double); virtual;
    procedure DoDecodeProgress(Progress: Double); virtual;
    procedure Initialize; override;
    procedure DecodeUpdateInternal(const BufferIn; var SizeIn: TMemSize; out BufferOut; var SizeOut: TMemSize); virtual;
  public
  {
    decoded size

    Scans the provided encoded data and calculates number of bytes necessary to
    store the same data in decoded state (ie. decoded size).

      WARNING - this process is more-or-less the same as full decoding, only
                the decoded data are not saved anywhere, which means obtaining
                decoded size can be quite time consuming.
                It is therefore recommended, if the size is required prior to
                decoding, to store unencoded size with the encoded data and
                only load it.
  }
    procedure DecodedSizeInit; virtual;
    Function DecodedSizeUpdate(const Buffer; Size: TMemSize): Boolean; virtual;
    Function DecodedSizeFinal: Int64; virtual;
    // decoded size macros
    Function DecodedSizeMemory(Memory: Pointer; Size: TMemSize): Int64; virtual;
    Function DecodedSizeBuffer(const Buffer; Size: TMemSize): Int64; virtual;
    Function DecodedSizeAnsiString(const Str: AnsiString): Int64; virtual;
    Function DecodedSizeWideString(const Str: WideString): Int64; virtual;
    Function DecodedSizeString(const Str: String): Int64; virtual;
    Function DecodedSizeStream(Stream: TStream; Count: Int64 = -1): Int64; virtual;
    Function DecodedSizeFile(const FileName: String): Int64; virtual;
    // decoding
  {
    Initializes decoding.

    The tree must be prepared before making the call.
  }
    procedure DecodeInit; virtual;
  {
    Decodes as many bits from input buffer as possible. The decoding stops
    either when all input bytes are consumed, when the output buffer becomes
    full or when termination sequence is encountered.

    The parameters work the same as in THuffmanEncoder.EncodeUpdate, see there
    for details.

    Returns true when termination seqence was not yet encountered, false when
    it was. If termination sequence was encountered, you should stop decoding
    right there, as no more data will be decoded even if you will provide more
    input data.
  }
    Function DecodeUpdate(const BufferIn; var SizeIn: TMemSize; out BufferOut; var SizeOut: TMemSize): Boolean; virtual;
  {
    DecodeFinal

    Only finalizes the decoding, no further processing is required.

    If the encoded data were not complete then this method raises an exception
    of type ESHInvalidState.
  }
    procedure DecodeFinal; virtual;
    // decoding macros
    procedure DecodeMemory(MemoryIn: Pointer; SizeIn: TMemSize; MemoryOut: Pointer; SizeOut: TMemSize); virtual;
    procedure DecodeBuffer(const BufferIn; SizeIn: TMemSize; out BufferOut; SizeOut: TMemSize); virtual;
    procedure DecodeAnsiString(const StrIn: AnsiString; var StrOut: AnsiString); virtual;
    procedure DecodeWideString(const StrIn: WideString; var StrOut: WideString); virtual;
    procedure DecodeString(const StrIn: String; var StrOut: String); virtual;
    procedure DecodeStream(StreamIn: TStream; CountIn: Int64; StreamOut: TStream); overload; virtual;
    procedure DecodeStream(StreamIn: TStream; StreamOut: TStream); overload; virtual;
    procedure DecodeFile(const FileNameIn,FileNameOut: String); virtual;
    // properties
    property OnDecodedSizeProgressCallback: TProgressCallback read fDecodedSizeProgressCallback write fDecodedSizeProgressCallback;
    property OnDecodedSizeProgressEvent: TProgressEvent read fDecodedSizeProgressEvent write fDecodedSizeProgressEvent;
    property OnDecodedSizeProgress: TProgressEvent read fDecodedSizeProgressEvent write fDecodedSizeProgressEvent;
    property OnDecodeProgressCallback: TProgressCallback read fDecodeProgressCallback write fDecodeProgressCallback;
    property OnDecodeProgressEvent: TProgressEvent read fDecodeProgressEvent write fDecodeProgressEvent;
    property OnDecodeProgress: TProgressEvent read fDecodeProgressEvent write fDecodeProgressEvent;
  end;

implementation

uses
  Math,
  BitOps, StaticMemoryStream, StrRect;

{===============================================================================
    Implementation constants
===============================================================================}
const
{$IF SizeOf(NativeUInt) = 8}
  NativeUIntSizeShift = 3;
{$ELSEIF SizeOf(NativeUInt) = 4}
  NativeUIntSizeShift = 2;
{$ELSE}
  {$MESSAGE FATAL 'Unsupported NativeUInt size.'}
{$IFEND}

{===============================================================================
    Auxiliary functions
===============================================================================}

procedure PutBitsAndMoveDest(var Destination: PUInt8; DstBitOffset: TMemSize; Source: PUInt8; BitCount: TMemSize);
var
  Mask:   UInt8;
  Buffer: UInt8;
begin
{
  This function assumes that destination and source are at completely different
  memory locations (they do not overlap) and DstBitOffset is in interval [0,7].
}
If (DstBitOffset <> 0) or ((BitCount and 7) <> 0) then
  begin
    // arbitrary bitstring or bit position
    // copy full octets
    If DstBitOffset <> 0 then
      begin
        Mask := UInt8(UInt8($FF) shl DstBitOffset);
        while BitCount >= 8 do
          begin
            Buffer := Source^;
            Destination^ := (Destination^ and not Mask) or UInt8(Buffer shl DstBitOffset);
            Inc(Destination);
            Destination^ := (Destination^ and Mask) or (Buffer shr (8 - DstBitOffset));
            // do NOT increase destination again
            Inc(Source);
            Dec(BitCount,8);
          end;
      end
    else
      begin
        while BitCount >= 8 do
          begin
            Destination^ := Source^;
            Inc(Destination);
            Inc(Source);
            Dec(BitCount,8);
          end;
      end;
    // copy remaining bits, if any
    If BitCount > 0 then
      begin
        Buffer := Source^;
        If BitCount > (8 - DstBitOffset) then
          begin
            // writing into two bytes (note that if here, DstBitOffset cannot be 0)
            Destination^ := (Destination^ and {Mask}(UInt8($FF) shr (8 - DstBitOffset))) or UInt8(Buffer shl DstBitOffset);
            Inc(Destination);
            Mask := UInt8(UInt8($FF) shl ((BitCount + DstBitOffset) and 7));
            Destination^ := (Destination^ and Mask) or ((Buffer shr (8 - DstBitOffset)) and not Mask);
          end
        else
          begin
            // writing into only one byte
            Mask := UInt8(UInt8($FF) shl DstBitOffset) and (UInt8($FF) shr (8 - DstBitOffset - BitCount));
            Destination^ := (Destination^ and not Mask) or (UInt8(Buffer shl DstBitOffset) and Mask);
            If (DstBitOffset + BitCount) >= 8 then
              Inc(Destination);
          end;
      end;
  end
else
  begin
    // integral bytes on byte boundary
    Move(Source^,Destination^,BitCount shr 3);
    Inc(Destination,BitCount shr 3);
  end;
end;

//------------------------------------------------------------------------------

Function LoadIntegerBits(var Source: PUInt8; SrcBitOffset,BitCount: TMemSize): Int64;
var
  DstBitOffset: TMemSize;
begin
// This function assumes that SrcBitOffset is in interval [0,7].
If BitCount < 64 then
  begin
    If BitCount > 0 then
      begin
        If SrcBitOffset <> 0 then
          begin
            If BitCount >= (8 - SrcBitOffset) then
              begin
                Result := Source^ shr SrcBitOffset;
                Inc(Source);
                Dec(BitCount,8 - SrcBitOffset);
                DstBitOffset := 8 - SrcBitOffset;
                while BitCount >= 8 do
                  begin
                    Result := Result or (Int64(Source^) shl DstBitOffset);
                    Dec(BitCount,8);
                    Inc(Source);
                    Inc(DstBitOffset,8);
                  end;
                If BitCount > 0 then
                  Result := Result or (Int64(Source^ and (UInt8($FF) shr (8 - BitCount))) shl DstBitOffset);
              end
            else Result := (Source^ shr SrcBitOffset) and (UInt8($FF) shr (8 - BitCount));
          end
        else
          begin
            Move(Source^,Addr(Result)^,(BitCount + 7) shr 3);
            // mask bits that are not supposed to be in the result
          {$IFDEF ENDIAN_BIG}
            Result := Int64(SwapEndian(UInt64(Result))) and (Int64($FFFFFFFFFFFFFFFF) shr (64 - BitCount));
          {$ELSE}
            Result := Result and (Int64($FFFFFFFFFFFFFFFF) shr (64 - BitCount));
          {$ENDIF}
            Inc(Source,BitCount shr 3);          
          end;
      end
    else Result := 0;
  end
else raise ESHInvalidValue.CreateFmt('LoadFrequencyBits: Invalid value of frequency bits (%d).',[BitCount]);
end;

//------------------------------------------------------------------------------

Function TreeSavingSchemeToNum(Scheme: TSHTreeSavingScheme): Integer;
begin
case Scheme of
  tssFullFreq:  Result := 0;
  tssAvrgDiff:  Result := 1;
else
  raise ESHInvalidValue.CreateFmt('TreeSavingSchemeToNum: Invalid tree saving scheme (%d).',[Ord(Scheme)]);
end;
end;

//------------------------------------------------------------------------------

Function NumToTreeSavingScheme(Num: Integer): TSHTreeSavingScheme;
begin
case Num of
  0:  Result := tssFullFreq;
  1:  Result := tssAvrgDiff;
else
  raise ESHInvalidValue.CreateFmt('NumToTreeSavingScheme: Invalid tree saving scheme (%d).',[Num]);
end;
end;

//------------------------------------------------------------------------------

Function TreeSavingFlagsToNum(Flags: TSHTreeSavingFlags): Integer;
begin
Result := 0;
If tsfNone in Flags then; // do nothing, no flag implemented
end;

//------------------------------------------------------------------------------

Function NumToTreeSavingFlags(Num: Integer): TSHTreeSavingFlags;
begin
Result := [];
If Num and 1 <> 0 then; // do nothing, no flag implemented
end;


{===============================================================================
--------------------------------------------------------------------------------
                                  THuffmanTree                                  
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    THuffmanTree - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    THuffmanTree - protected methods
-------------------------------------------------------------------------------}

Function THuffmanTree.GetByteNode(Index: Integer): TSHHuffmanTreeNode;
begin
If CheckByteNodeIndex(Index) then
  Result := fByteNodes[UInt8(Index)]
else
  raise ESHIndexOutOfBounds.CreateFmt('THuffmanTree.GetByteNode: Index (%d) ouf of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.GetFrequency(Index: Integer): Int64;
begin
If CheckByteNodeIndex(Index) then
  Result := fByteNodes[UInt8(Index)].Frequency
else
  raise ESHIndexOutOfBounds.CreateFmt('THuffmanTree.GetFrequency: Index (%d) ouf of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.SetFrequency(Index: Integer; Value: Int64);
begin
If CheckByteNodeIndex(Index) then
  fByteNodes[UInt8(Index)].Frequency := Value
else
  raise ESHIndexOutOfBounds.CreateFmt('THuffmanTree.SetFrequency: Index (%d) ouf of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.GetBitSequence(Index: Integer): TSHBitSequence;
begin
If CheckByteNodeIndex(Index) then
  Result := fByteNodes[UInt8(Index)].BitSequence
else
  raise ESHIndexOutOfBounds.CreateFmt('THuffmanTree.GetBitSequence: Index (%d) ouf of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.GetNodeSaved(Index: Integer): Boolean;
begin
If CheckByteNodeIndex(Index) then
  Result := (fByteNodes[UInt8(Index)].NodeKind = nkByteSaved)
else
  raise ESHIndexOutOfBounds.CreateFmt('THuffmanTree.GetNodeSaved: Index (%d) ouf of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.SetNodeSaved(Index: Integer; Value: Boolean);
begin
If CheckByteNodeIndex(Index) then
  begin
    // do not clear frequency
    If Value then
      fByteNodes[UInt8(Index)].NodeKind := nkByteSaved
    else
      fByteNodes[UInt8(Index)].NodeKind := nkByteUnsaved
  end
else raise ESHIndexOutOfBounds.CreateFmt('THuffmanTree.SetNodeSaved: Index (%d) ouf of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.GetTreeNodePtr(Index: Integer): PSHHuffmanTreeNode;
begin
If CheckTreeNodeIndex(Index) then
  Result := fTreeNodes[Index]
else
  raise ESHIndexOutOfBounds.CreateFmt('THuffmanTree.GetTreeNodePtr: Index (%d) ouf of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.GetTreeNode(Index: Integer): TSHHuffmanTreeNode;
begin
If CheckTreeNodeIndex(Index) then
  Result := fTreeNodes[Index]^
else
  raise ESHIndexOutOfBounds.CreateFmt('THuffmanTree.GetTreeNode: Index (%d) ouf of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.GetCapacity(List: Integer): Integer;
begin
case List of
  SH_HUFFTREE_LIST_BYTENODES: Result := Length(fByteNodes);
  SH_HUFFTREE_LIST_TREENODES: Result := Length(fTreeNodes);
else
  raise ESHInvalidValue.CreateFmt('THuffmanTree.GetCapacity: Invalid list index (%d).',[List]);
end;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.SetCapacity(List,Value: Integer);
begin
case List of
  SH_HUFFTREE_LIST_BYTENODES: raise ESHInvalidOperation.Create('THuffmanTree.SetCapacity: Cannot change capacity of byte nodes.');
{
  Since capacity of tree nodes is only changed internally, and then only
  increased, there is no need for complex checks and data protection.
}
  SH_HUFFTREE_LIST_TREENODES: SetLength(fTreeNodes,Value);
else
  raise ESHInvalidValue.CreateFmt('THuffmanTree.SetCapacity: Invalid list index (%d).',[List]);
end;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.GetCount(List: Integer): Integer;
begin
case List of
  SH_HUFFTREE_LIST_BYTENODES: Result := Length(fByteNodes);
  SH_HUFFTREE_LIST_TREENODES: Result := fTreeNodeCount;
else
  raise ESHInvalidValue.CreateFmt('THuffmanTree.GetCount: Invalid list index (%d).',[List]);
end;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.SetCount(List,Value: Integer);
begin
Value := List;  // only to prevent warnings
case Value of
  SH_HUFFTREE_LIST_BYTENODES,
  SH_HUFFTREE_LIST_TREENODES:;
else
  raise ESHInvalidValue.CreateFmt('THuffmanTree.SetCount: Invalid list index (%d).',[List]);
end;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.AddTreeNode(Node: PSHHuffmanTreeNode): Integer;
var
  i:  Integer;
begin
{
  NOTE - nodes are ordered from lowest frequency to highest.
}
Grow(SH_HUFFTREE_LIST_TREENODES);
// find index where to put the new node
Result := fTreeNodeCount;
For i := LowTreeNodeIndex to HighTreeNodeIndex do
  If Node^.Frequency < fTreeNodes[i]^.Frequency then
    begin
      Result := i;
      Break{For i};
    end;
// inser the node at selected position
For i := HighTreeNodeIndex downto Result do
  fTreeNodes[i + 1] := fTreeNodes[i];
fTreeNodes[Result] := Node;
Inc(fTreeNodeCount);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.CountSavedByteNodes: Integer;
var
  i:  Integer;
begin
Result := 0;
For i := LowByteNodeIndex to HighByteNodeIndex do
  If fByteNodes[UInt8(i)].NodeKind = nkByteSaved then
    Inc(Result);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.EncodeHeader(Scheme: TSHTreeSavingScheme; Flags: TSHTreeSavingFlags): UInt8;
begin
// scheme number
Result := TreeSavingSchemeToNum(Scheme) and $F;
// flag bits
Result := Result or ((TreeSavingFlagsToNum(Flags) and $F) shl 4);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.DecodeHeader(Header: UInt8; out Scheme: TSHTreeSavingScheme; out Flags: TSHTreeSavingFlags);
begin
Scheme := NumToTreeSavingScheme(Header and $F);
Flags := NumToTreeSavingFlags((Header shr 4) and $F);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.PreloadAllocationSize(Stream: TStream): TMemSize;
var
  OrigPos:  Int64;
  Buffer:   array[0..15] of UInt8;
  Scheme:   TSHTreeSavingScheme;
  Flags:    TSHTreeSavingFlags;
  AvrgBits: Integer;
  DiffBits: Integer;
begin
Result := 0;
OrigPos := Stream.Position;
try
  If (Stream.Size - Stream.Position) >= 1 then
    begin
      // load and decode header to get saving scheme
      Stream.ReadBuffer(Addr(Buffer)^,1);
      DecodeHeader(Buffer[Low(Buffer)],Scheme,Flags);
      case Scheme of
        tssFullFreq:
          If (Stream.Size - Stream.Position) >= 1{6 bits} then
            begin
              Stream.ReadBuffer(Buffer,1);
              Result := (((Buffer[Low(Buffer)] and $3F {FreqBits}) * CountSavedByteNodes) + 21) shr 3;
            end
          else raise ESHBufferTooSmall.Create('THuffmanTree.PrealoadAllocationSize: Stream too small to contain frequency bits.');
        tssAvrgDiff:
          If (Stream.Size - Stream.Position) >= 2{12 bits} then
            begin
              // load bit counts
              Stream.ReadBuffer(Buffer,2);
              AvrgBits := Buffer[Low(Buffer)] and $3F;
              DiffBits := ((Buffer[Low(Buffer)] shr 6) and $3) or UInt8((Buffer[Succ(Low(Buffer))] and $F) shl 2);
              // calculate the size from bit counts
              If DiffBits <> 0 then
                Result := TMemSize((AvrgBits + ((DiffBits + 1) * CountSavedByteNodes) + 27) shr 3)
              else
                Result := TMemSize((AvrgBits + 27) shr 3)
            end
          else raise ESHBufferTooSmall.Create('THuffmanTree.PrealoadAllocationSize: Stream too small to contain bit counts.');
      else
        raise ESHInvalidValue.CreateFmt('THuffmanTree.PrealoadAllocationSize: Invalid tree saving scheme (%d).',[Ord(Scheme)]);
      end;
    end
  else raise ESHBufferTooSmall.Create('THuffmanTree.PrealoadAllocationSize: Stream too small to contain a valid header.')
finally
  Stream.Seek(OrigPos,soBeginning);
end;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.StreamingSize_0(out FreqBits: Integer): TMemSize;
var
  SavedNodesCnt:  Integer;
  FreqHigh:       Int64;
  i:              Integer;
begin
FreqHigh := 0;
SavedNodesCnt := 0;
// get highest frequency and cound number of saved nodes
For i := LowByteNodeIndex to HighByteNodeIndex do
  If fByteNodes[UInt8(i)].NodeKind = nkByteSaved then
    begin
      Inc(SavedNodesCnt);
      If fByteNodes[UInt8(i)].Frequency > FreqHigh then
        FreqHigh := fByteNodes[UInt8(i)].Frequency;
    end
  else If fByteNodes[UInt8(i)].Frequency <> 0 then
    raise ESHInvalidValue.CreateFmt('THuffmanTree.StreamingSize: Unsaved node (%d) with frequency.',[i]);
// how many bits is needed to store this number (reversed bit scan)
If FreqHigh <> 0 then
  begin
    FreqBits := BSR(UInt64(FreqHigh)) + 1;
    // FreqBits of 64 (or more) would imply a negative value
    If FreqBits > 63 then
      raise ESHInvalidValue.CreateFmt('THuffmanTree.StreamingSize_0: Invalid frequency bits (%d).',[FreqBits]);
  end
else FreqBits := 0;
{
  Calculate resulting size in bytes...

  First four bits contain saving scheme number, next four bits are flags,
  following six bits contain how much bits are used to store each frequency
  (frequency bits).

  If frequency bits is above zero, then the next bits contain bit-packed
  frequencies (at most 256 values, for each only frequency bits are stored),
  when frequency bits is zero then nothing is stored after the first 10 bits.
}
Result := TMemSize((FreqBits * SavedNodesCnt) + 21 {8 header bits, 6 frequency bits}) shr 3 {Ceil(x / 8)};
end;

//------------------------------------------------------------------------------

Function THuffmanTree.StreamingSize_1(out AvrgFreq: Int64; out AvrgBits,DiffBits: Integer): TMemSize;
var
  SavedNodesCnt:  Integer;
  i:              Integer;
  HighDiff:       Int64;
begin
// get average frequency and cound number of saved nodes
AvrgFreq := 0;
SavedNodesCnt := 0;
For i := LowByteNodeIndex to HighByteNodeIndex do
  If fByteNodes[UInt8(i)].NodeKind = nkByteSaved then
    begin
      AvrgFreq := AvrgFreq + fByteNodes[UInt8(i)].Frequency;
      Inc(SavedNodesCnt);
    end
  else If fByteNodes[UInt8(i)].Frequency <> 0 then
    raise ESHInvalidValue.CreateFmt('THuffmanTree.StreamingSize: Unsaved node (%d) with frequency.',[i]);
AvrgFreq := Trunc(AvrgFreq / SavedNodesCnt);
// how many bits is needed to store average frequency
If AvrgFreq <> 0 then
  begin
    AvrgBits := BSR(UInt64(AvrgFreq)) + 1;
    // FreqBits of 64 (or more) would imply a negative value
    If AvrgBits > 63 then
      raise ESHInvalidValue.CreateFmt('THuffmanTree.StreamingSize_1: Invalid average frequency bits (%d).',[AvrgBits]);
  end
else AvrgBits := 0;
// get highest difference from average (positive or negative)
HighDiff := 0;
For i := LowByteNodeIndex to HighByteNodeIndex do
  If fByteNodes[UInt8(i)].NodeKind = nkByteSaved then
    If Abs(fByteNodes[UInt8(i)].Frequency - AvrgFreq) > HighDiff then
      HighDiff := Abs(fByteNodes[UInt8(i)].Frequency - AvrgFreq);
{
  How many bits is needed to store that difference.

  This number has very specific "encoding"...
  If it is zero, it is stored as zero and no difference is stored.
  If it is any other value, then the real number of bits stored per difference
  is DiffBits + 1. The one is here for a sign bit. This also means that no
  difference can be stored with only one bit, because that would be only the
  sign bit. And it also allows for store of full 64 bits of Int64.
}
If HighDiff <> 0 then
  DiffBits := BSR(UInt64(HighDiff)) + 1
else
  DiffBits := 0;
{
  Get final size in bytes.

  First four bits contain saving scheme number, next four bits are flags.
  Following six bits contain how many bits are used to store average frequency,
  and six bits after that contain how many bits are used to store each frequency
  difference (this number is encoded, see higher).

  After these, an average frequency is stored (only lower AvrgBits bits). Note
  that the average frequency might not be present if AvrgBits is zero. If this
  happens, then the average frequency is assumed to be equal to 0.

  Then, if DiffBits is not zero, a bitpacked array of frequency differences is
  stored (only lowest DiffBits + 1 bits are stored for each). Note that all
  differences are signed.
  If DiffBits is zero, then nothing is stored after the average frequency.
}
If DiffBits <> 0 then
  Result := TMemSize((AvrgBits + ((DiffBits + 1) * SavedNodesCnt) + 27) shr 3)
else
  Result := TMemSize((AvrgBits + 27) shr 3)
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.SaveToBuffer_0(out Buffer; Size: TMemSize);
var
  FreqBits:   Integer;
  BuffPtr:    PUInt8;
  BitOffset:  Integer;
  i:          Integer;
  Temp:       Int64;
begin
If Size >= StreamingSize_0(FreqBits) then
  begin
    BuffPtr := @Buffer;
    // save header
    BuffPtr^ := EncodeHeader(tssFullFreq,[]);
    Inc(BuffPtr);
    // save frequency bits (6 bits)
    Temp := {$IFDEF ENDIAN_BIG}Int64(SwapEndian(UInt64{$ELSE}(({$ENDIF}(FreqBits)));
    PutBitsAndMoveDest(BuffPtr,0,PUInt8(@Temp),6);
    BitOffset := 6;
    If FreqBits > 0 then
      For i := LowByteNodeIndex to HighByteNodeIndex do
        If fByteNodes[UInt8(i)].NodeKind = nkByteSaved then
          begin
            // make sure the number has little endianness
          {$IFDEF ENDIAN_BIG}
            Temp := Int64(SwapEndian(UInt64(fByteNodes[UInt8(i)].Frequency)));
          {$ELSE}
            Temp := fByteNodes[UInt8(i)].Frequency;
          {$ENDIF}
            PutBitsAndMoveDest(BuffPtr,BitOffset,PUInt8(@Temp),TMemSize(FreqBits));
            BitOffset := (BitOffset + FreqBits) and 7;
          end;
  end
else raise ESHBufferTooSmall.CreateFmt('THuffmanTree.SaveToBuffer_0: Insufficient buffer size (%u).',[Size]);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.SaveToBuffer_1(out Buffer; Size: TMemSize);
var
  AvrgFreq:   Int64;
  AvrgBits:   Integer;
  DiffBits:   Integer;
  BuffPtr:    PUInt8;
  BitOffset:  Integer;
  i:          Integer;
  Temp:       Int64;
begin
If Size >= StreamingSize_1(AvrgFreq,AvrgBits,DiffBits) then
  begin
    BuffPtr := @Buffer;
    // save header
    BuffPtr^ := EncodeHeader(tssAvrgDiff,[]);
    Inc(BuffPtr);
    // save average frequency bits (6 bits)
    Temp := {$IFDEF ENDIAN_BIG}Int64(SwapEndian(UInt64{$ELSE}(({$ENDIF}(AvrgBits)));
    PutBitsAndMoveDest(BuffPtr,0,PUInt8(@Temp),6);
    // save frequency difference bits (6 bits)
    Temp := {$IFDEF ENDIAN_BIG}Int64(SwapEndian(UInt64{$ELSE}(({$ENDIF}(DiffBits)));
    PutBitsAndMoveDest(BuffPtr,6,PUInt8(@Temp),6);  // BuffPtr is incremented here
    // save average frequency
    BitOffset := 4;
    If AvrgBits > 0 then
      begin
        Temp := {$IFDEF ENDIAN_BIG}Int64(SwapEndian(UInt64{$ELSE}(({$ENDIF}(AvrgFreq)));
        PutBitsAndMoveDest(BuffPtr,BitOffset,PUInt8(@Temp),TMemSize(AvrgBits));
        BitOffset := (BitOffset + AvrgBits) and 7;
      end;
    // save frequency diferences
    If DiffBits > 0 then
      For i := LowByteNodeIndex to HighByteNodeIndex do
        If fByteNodes[UInt8(i)].NodeKind = nkByteSaved then
          begin
          {$IFDEF ENDIAN_BIG}
            Temp := Int64(SwapEndian(UInt64(fByteNodes[UInt8(i)].Frequency - AvrgFreq)));
          {$ELSE}
            Temp := fByteNodes[UInt8(i)].Frequency - AvrgFreq;
          {$ENDIF}
            PutBitsAndMoveDest(BuffPtr,BitOffset,PUInt8(@Temp),TMemSize(DiffBits + 1{sign bit}));
            BitOffset := (BitOffset + DiffBits + 1) and 7;
          end;
  end
else raise ESHBufferTooSmall.CreateFmt('THuffmanTree.SaveToBuffer_1: Insufficient buffer size (%u).',[Size]);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.LoadFromBuffer_0(const Buffer; Size: TMemSize);
var
  BuffPtr:    PUInt8;
  FreqBits:   Integer;
  BitOffset:  Integer;
  i:          Integer;
begin
If Size >= 2 then
  begin
    BuffPtr := @Buffer;
    // skip header, we do not need it
    Inc(BuffPtr);
    // load frequency bits
    FreqBits := LoadIntegerBits(BuffPtr,0,6);
    If Size >= TMemSize(((FreqBits * CountSavedByteNodes) + 21) shr 3) then
      begin
        ClearTree;
        BitOffset := 6;
        If FreqBits > 0 then
          For i := LowByteNodeIndex to HighByteNodeIndex do
            If fByteNodes[UInt8(i)].NodeKind = nkByteSaved then
              begin
                fByteNodes[UInt8(i)].Frequency := LoadIntegerBits(BuffPtr,BitOffset,FreqBits);
                BitOffset := (BitOffset + FreqBits) and 7;
              end;
        ConstructTree;
      end
    else raise ESHBufferTooSmall.CreateFmt('THuffmanTree.LoadFromBuffer_0: Buffer too small to contain frequency table (%u).',[Size]);
  end
else raise ESHBufferTooSmall.CreateFmt('THuffmanTree.LoadFromBuffer_0: Buffer too small to contain frequency bits (%u).',[Size]);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.LoadFromBuffer_1(const Buffer; Size: TMemSize);
var
  AvrgFreq:   Int64;
  AvrgBits:   Integer;
  DiffBits:   Integer;
  BuffPtr:    PUInt8;
  BitOffset:  Integer;
  i:          Integer;
  Temp:       Int64;
begin
If Size >= 3 then
  begin
    BuffPtr := @Buffer;
    // skip header
    Inc(BuffPtr);
    // load average frequency bits
    AvrgBits := LoadIntegerBits(BuffPtr,0,6);
    // load frequency difference bits
    DiffBits := LoadIntegerBits(BuffPtr,6,6);
    If Size >= TMemSize((AvrgBits + (IfThen(DiffBits <> 0,DiffBits + 1,0) * CountSavedByteNodes) + 27) shr 3) then
      begin
        ClearTree;
        BitOffset := 4;
        // load average frequency
        If AvrgBits > 0 then
          begin
            AvrgFreq := LoadIntegerBits(BuffPtr,BitOffset,AvrgBits);
            BitOffset := (BitOffset + AvrgBits) and 7;
          end
        else AvrgFreq := 0;
        // load differences
        If DiffBits > 0 then
          begin
            For i := LowByteNodeIndex to HighByteNodeIndex do
              If fByteNodes[UInt8(i)].NodeKind = nkByteSaved then
                begin
                  Temp := LoadIntegerBits(BuffPtr,BitOffset,DiffBits + 1);
                  // expand sign bit and add to average
                  fByteNodes[UInt8(i)].Frequency := Int64(SAR(UInt64(Temp shl (63 - DiffBits)),63 - DiffBits)) + AvrgFreq;
                  BitOffset := (BitOffset + DiffBits + 1) and 7;
                end;
          end
        else
          begin
            For i := LowByteNodeIndex to HighByteNodeIndex do
              If fByteNodes[UInt8(i)].NodeKind = nkByteSaved then
                fByteNodes[UInt8(i)].Frequency := AvrgFreq;
          end;
        ConstructTree;
      end
    else raise ESHBufferTooSmall.CreateFmt('THuffmanTree.LoadFromBuffer_1: Buffer too small to contain all data (%u).',[Size]);
  end
else raise ESHBufferTooSmall.CreateFmt('THuffmanTree.LoadFromBuffer_1: Buffer too small to contain bit counts (%u).',[Size]);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.ClearByteNodes;
var
  i:        Integer;
  NodeKind: TSHNodeKind;
begin
For i := LowByteNodeIndex to HighByteNodeIndex do
  begin
    NodeKind := fByteNodes[UInt8(i)].NodeKind;  // preserve node kind
    FillChar(fByteNodes[UInt8(i)],SizeOf(TSHHuffmanTreeNode),0);
    fByteNodes[UInt8(i)].NodeKind := NodeKind;
    fByteNodes[UInt8(i)].ByteIndex := UInt8(i);
    fByteNodes[UInt8(i)].TreeIndex := -1;
  end;
// also clear terminator node  
FillChar(fTerminatorNode,SizeOf(TSHHuffmanTreeNode),0);
fTerminatorNode.NodeKind := nkTerminator;
// terminator is always once in the stream
fTerminatorNode.Frequency := 1;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.ClearTreeNodes;
var
  i:  Integer;
begin
For i := LowTreeNodeIndex to HighTreeNodeIndex do
  If fTreeNodes[i]^.NodeKind = nkBranch then
    Dispose(fTreeNodes[i]);
SetLength(fTreeNodes,0);
fTreeNodeCount := 0;
fRootNode := nil;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.Initialize;
begin
FillChar(fByteNodes,SizeOf(fByteNodes),0);
ClearTree;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.Finalize;
begin
ClearTreeNodes;
end;

{-------------------------------------------------------------------------------
    THuffmanTree - public methods
-------------------------------------------------------------------------------}

constructor THuffmanTree.Create;
begin
inherited Create(2);
Initialize;
end;

//------------------------------------------------------------------------------

destructor THuffmanTree.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.LowIndex(List: Integer): Integer;
begin
case List of
  SH_HUFFTREE_LIST_BYTENODES: Result := Low(fByteNodes);
  SH_HUFFTREE_LIST_TREENODES: Result := Low(fTreeNodes);
else
  raise ESHInvalidValue.CreateFmt('THuffmanTree.LowIndex: Invalid list index (%d).',[List]);
end;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.HighIndex(List: Integer): Integer;
begin
case List of
  SH_HUFFTREE_LIST_BYTENODES: Result := High(fByteNodes);
  SH_HUFFTREE_LIST_TREENODES: Result := Pred(fTreeNodeCount);
else
  raise ESHInvalidValue.CreateFmt('THuffmanTree.HighIndex: Invalid list index (%d).',[List]);
end;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.LowByteNodeIndex: Integer;
begin
Result := Low(fByteNodes);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.HighByteNodeIndex: Integer;
begin
Result := High(fByteNodes);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.CheckByteNodeIndex(Index: UInt8): Boolean;
begin
Result := (Index >= LowByteNodeIndex) and (Index <= HighByteNodeIndex);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.LowTreeNodeIndex: Integer;
begin
Result := Low(fTreeNodes);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.HighTreeNodeIndex: Integer;
begin
Result := Pred(fTreeNodeCount);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.CheckTreeNodeIndex(Index: Integer): Boolean;
begin
Result := (Index >= LowTreeNodeIndex) and (Index <= HighTreeNodeIndex);
end;

//------------------------------------------------------------------------------

Function THuffmanTree.IncreaseFrequency(ByteIndex: UInt8): Int64;
begin
Inc(fByteNodes[ByteIndex].Frequency);
Result := fByteNodes[ByteIndex].Frequency;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.TreeIsReady: Boolean;
begin
Result := Assigned(fRootNode);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.ConstructTree;

  procedure AddToBitSequence(var BitSequence: TSHBitSequence; Node: PSHHuffmanTreeNode);

    procedure AddToBitSequenceData(var BitSequenceData: TSHBitSequenceData; NewBit: Boolean);
    var
      i,Carry:  Integer;
    begin
      // shift existing sequence
      Carry := 0;
      For i := Low(BitSequenceData) to (BitSequence.Length shr 3) do
        begin
          Carry := Carry or (BitSequenceData[i] and $80);
          BitSequenceData[i] := UInt8(BitSequenceData[i] shl 1) or UInt8(Carry and 1);
          Carry := Carry shr 7;
        end;
      // add new bit  
      If NewBit then
        BitSequenceData[Low(BitSequenceData)] := BitSequenceData[Low(BitSequenceData)] or 1;
    end;

  begin
    If Assigned(Node^.ParentNode) then
      begin
        AddToBitSequenceData(BitSequence.Data,Node^.ParentPath);
        Inc(BitSequence.Length);  // must be second
        AddToBitSequence(BitSequence,Node^.ParentNode);
      end;
  end;

var
  i:        Integer;
  TempNode: PSHHuffmanTreeNode;
begin
If Assigned(fRootNode) then
  ClearTreeNodes;
{
  Preallocate tree nodes (513 should be enough for all cases, meaning there
  will be no need for reallocation) - there is always 256 byte nodes, 1
  terminator node and 256 branch nodes (don't ask me why, it just is). 
}
SetCapacity(SH_HUFFTREE_LIST_TREENODES,513);
// first add all the byte nodes (they are being ordered as added)
For i := LowByteNodeIndex to HighByteNodeIndex do
  begin
    If (fByteNodes[UInt8(i)].NodeKind = nkByteSaved) or (fByteNodes[UInt8(i)].Frequency = 0) then
      AddTreeNode(@fByteNodes[UInt8(i)])
    else
      raise ESHInvalidValue.CreateFmt('THuffmanTree.ConstructTree: Unsaved node (%d) with frequency.',[i]);
  end;
// add terminator node
AddTreeNode(@fTerminatorNode);
// now create branch nodes
i := LowTreeNodeIndex;
while i < HighTreeNodeIndex do
  begin
    New(TempNode);
    FillChar(TempNode^,SizeOf(TSHHuffmanTreeNode),0);
    TempNode^.NodeKind := nkBranch;
    // true TreeIndex is assined later, when the entire tree is constructed
    TempNode^.TreeIndex := -1;
    TempNode^.Frequency := fTreeNodes[i]^.Frequency + fTreeNodes[Succ(i)]^.Frequency;
    // connect nodes
    TempNode^.ChildNodes[False] := fTreeNodes[i];
    fTreeNodes[i]^.ParentNode := TempNode;
    fTreeNodes[i]^.ParentPath := False;
    fTreeNodes[i]^.SiblingNode := fTreeNodes[Succ(i)];
    TempNode^.ChildNodes[True] := fTreeNodes[Succ(i)];
    fTreeNodes[Succ(i)]^.ParentNode := TempNode;
    fTreeNodes[Succ(i)]^.ParentPath := True;
    fTreeNodes[Succ(i)]^.SiblingNode := fTreeNodes[i];
    AddTreeNode(TempNode);
    Inc(i,2);
  end;
fRootNode := fTreeNodes[HighTreeNodeIndex];
// assign correct tree indices
For i := LowTreeNodeIndex to HighTreeNodeIndex do
  fTreeNodes[i]^.TreeIndex := i;
// prepare bit sequences
For i := LowByteNodeIndex to HighByteNodeIndex do
  AddToBitSequence(fByteNodes[UInt8(i)].BitSequence,@fByteNodes[UInt8(i)]);
AddToBitSequence(fTerminatorNode.BitSequence,@fTerminatorNode);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.BuildTree(Frequencies: array of Int64);
var
  i:  Integer;
begin
ClearTree;
For i := Low(Frequencies) to Min(High(Frequencies),High(fByteNodes)) do
  fByteNodes[UInt8(i)].Frequency := Frequencies[i];
ConstructTree;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.CopyTree(Tree: THuffmanTree);
var
  i:  Integer;
begin
ClearTree;
For i := LowByteNodeIndex to HighByteNodeIndex do
  begin
    fByteNodes[UInt8(i)].Frequency := Tree.Frequencies[i];
    If Tree.NodeSaved[i] then
      fByteNodes[UInt8(i)].NodeKind := nkByteSaved
    else
      fByteNodes[UInt8(i)].NodeKind := nkByteUnsaved;
  end;
ConstructTree;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.SameTree(Tree: THuffmanTree): Boolean;

  Function SameBitSequence(A,B: TSHBitSequence): Boolean;
  var
    i:    Integer;
    Mask: UInt8;
  begin
    If A.Length = B.Length then
      begin
        Result := True;
        For i := 1 to (A.Length shr 3) do
          If A.Data[Pred(i)] <> B.Data[Pred(i)] then
            begin
              Result := False;
              Exit;
            end;
        If (A.Length and 7) <> 0 then
          begin
            i := (A.Length + 7) shr 3;
            Mask := UInt8($FF) shr (8 - (A.Length and 7));
            Result := (A.Data[i] and Mask) = (B.Data[i] and Mask);
          end;
      end
    else Result := False;
  end;

var
  i:  Integer;
begin
If SameBitSequence(fTerminatorNode.BitSequence,Tree.TerminatorNode.BitSequence) then
  begin
    Result := True;
    For i := LowByteNodeIndex to HighByteNodeIndex do
      If not SameBitSequence(fByteNodes[UInt8(i)].BitSequence,Tree.BitSequences[i]) then
        begin
          Result := False;
          Break{For i};
        end;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.ClearTree;
begin
ClearByteNodes;
ClearTreeNodes;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.TraverseTree(BitPath: Boolean; var TreeNodeIndex: Integer): Boolean;
var
  SubNode:  PSHHuffmanTreeNode;
begin
// following is much faster than calling CheckTreeNodeIndex
If (TreeNodeIndex < Low(fTreeNodes)) or (TreeNodeIndex >= fTreeNodeCount) then
  begin
    If Assigned(fRootNode) then
      SubNode := fRootNode^.ChildNodes[BitPath]
    else
      raise ESHInvalidOperation.Create('THuffmanTree.TraverseTree: Root node not assigned.');
  end
else SubNode := fTreeNodes[TreeNodeIndex]^.ChildNodes[BitPath];
If Assigned(SubNode) then
  TreeNodeIndex := SubNode^.TreeIndex
else
  raise ESHInvalidOperation.Create('THuffmanTree.TraverseTree: No subnode assigned.');
Result := SubNode^.NodeKind = nkBranch;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.StreamingSize(Scheme: TSHTreeSavingScheme): TMemSize;
var
  Dummy1,Dummy2:  Integer;
  Dummy64:        Int64;
begin
case Scheme of
  tssFullFreq:  Result := StreamingSize_0(Dummy1);
  tssAvrgDiff:  Result := StreamingSize_1(Dummy64,Dummy1,Dummy2);
else
  raise ESHInvalidValue.CreateFmt('THuffmanTree.StreamingSize: Invalid tree saving scheme (%d).',[Ord(Scheme)]);
end;
end;

//------------------------------------------------------------------------------

Function THuffmanTree.BestStreamingSize(out Scheme: TSHTreeSavingScheme): TMemSize;
var
  i:    TSHTreeSavingScheme;
  Temp: TMemSize;
begin
Result := StreamingSize(Low(TSHTreeSavingScheme));
Scheme := Low(TSHTreeSavingScheme);
For i := Succ(Low(TSHTreeSavingScheme)) to High(TSHTreeSavingScheme) do
  begin
    Temp := StreamingSize(i);
    If Temp < Result then
      begin
        Result := Temp;
        Scheme := i;
        Break{For i};
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.SaveToBuffer(out Buffer; Size: TMemSize; Scheme: TSHTreeSavingScheme = tssFullFreq);
begin
case Scheme of
  tssFullFreq:  SaveToBuffer_0(Buffer,Size);
  tssAvrgDiff:  SaveToBuffer_1(Buffer,Size);
else
  raise ESHInvalidValue.CreateFmt('THuffmanTree.SaveToBuffer: Invalid tree saving scheme (%d).',[Ord(Scheme)]);
end;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.SaveToStream(Stream: TStream; Scheme: TSHTreeSavingScheme = tssFullFreq);
var
  Buffer:   Pointer;
  BuffSize: TMemSize;
begin
BuffSize := StreamingSize(Scheme);
GetMem(Buffer,BuffSize);
try
  SaveToBuffer(Buffer^,BuffSize,Scheme);
  Stream.WriteBuffer(Buffer^,BuffSize);
finally
  FreeMem(Buffer,BuffSize);
end;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.SaveToFile(const FileName: String; Scheme: TSHTreeSavingScheme = tssFullFreq);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmCreate or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  SaveToStream(FileStream,Scheme);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.LoadFromBuffer(const Buffer; Size: TMemSize; out Scheme: TSHTreeSavingScheme);
var
  Flags:  TSHTreeSavingFlags;
begin
If Size >= 1 then
  begin
    // load and decode header to get saving scheme
    DecodeHeader(UInt8(Buffer),Scheme,Flags);
    case Scheme of
      tssFullFreq:  LoadFromBuffer_0(Buffer,Size);
      tssAvrgDiff:  LoadFromBuffer_1(Buffer,Size);
    else
      raise ESHInvalidValue.CreateFmt('THuffmanTree.LoadFromBuffer: Invalid tree saving scheme (%d).',[Ord(Scheme)]);
    end;
  end
else raise ESHBufferTooSmall.CreateFmt('THuffmanTree.LoadFromBuffer: Buffer too small to contain a valid header (%u).',[Size]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure THuffmanTree.LoadFromBuffer(const Buffer; Size: TMemSize);
var
  Scheme: TSHTreeSavingScheme;
begin
LoadFromBuffer(Buffer,Size,Scheme);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.LoadFromStream(Stream: TStream; out Scheme: TSHTreeSavingScheme);
var
  BuffSize: TMemSize;
  Buffer:   Pointer;
begin
BuffSize := PreloadAllocationSize(Stream);
If BuffSize > 0 then
  begin
    GetMem(Buffer,BuffSize);
    try
      Stream.ReadBuffer(Buffer^,BuffSize);
      LoadFromBuffer(Buffer^,BuffSize,Scheme);
    finally
      FreeMem(Buffer,BuffSize);
    end;  
  end
else raise ESHInvalidValue.CreateFmt('THuffmanTree.LoadFromStream: Invalid allocation size (%u).',[BuffSize]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure THuffmanTree.LoadFromStream(Stream: TStream);
var
  Scheme: TSHTreeSavingScheme;
begin
LoadFromStream(Stream,Scheme);
end;

//------------------------------------------------------------------------------

procedure THuffmanTree.LoadFromFile(const FileName: String; out Scheme: TSHTreeSavingScheme);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  LoadFromStream(FileStream,Scheme);
finally
  FileStream.Free;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure THuffmanTree.LoadFromFile(const FileName: String);
var
  Scheme: TSHTreeSavingScheme;
begin
LoadFromFile(FileName,Scheme);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 THuffmanBase
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    THuffmanBase - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    THuffmanBase - protected methods
-------------------------------------------------------------------------------}

procedure THuffmanBase.DoScanProgress(Progress: Double);
begin
If Assigned(fScanProgressEvent) then
  fScanProgressEvent(Self,Progress)
else If Assigned(fScanProgressCallback) then
  fScanProgressCallback(Self,Progress);
end;

//------------------------------------------------------------------------------

procedure THuffmanBase.Initialize;
begin
fHuffmanTree := THuffmanTree.Create;
fStreamBufferSize := StreamBufferSizeDefault;
fUncompressedSize := 0;
fCompressedSize := 0;
fCompressionRatio := 0.0;
fBreakProcessing := False;
fScanInitialized := False;
fScanFinalized := False;
fScanProgressCallback := nil;
fScanProgressEvent := nil;
end;

//------------------------------------------------------------------------------

procedure THuffmanBase.Finalize;
begin
fHuffmanTree.Free;
end;

{-------------------------------------------------------------------------------
    THuffmanBase - public methods
-------------------------------------------------------------------------------}

class Function THuffmanBase.StreamBufferSizeDefault: TMemSize;
begin
Result := 64 * 1024;  // 64KiB
end;

//------------------------------------------------------------------------------

constructor THuffmanBase.Create;
begin
inherited Create;
Initialize;
end;

//------------------------------------------------------------------------------

destructor THuffmanBase.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function THuffmanBase.BreakProcessing: Boolean;
begin
Result := fBreakProcessing;
fBreakProcessing := True;
end;

//------------------------------------------------------------------------------

procedure THuffmanBase.ScanInit;
begin
fHuffmanTree.ClearTree;
fScanInitialized := True;
fScanFinalized := False;
end;

//------------------------------------------------------------------------------

procedure THuffmanBase.ScanUpdate(const Buffer; Size: TMemSize);
var
  BuffPtr:  PUInt8;
  i:        TMemSize;
begin
If fScanInitialized then
  begin
    If not fScanFinalized then
      begin
        BuffPtr := @Buffer;
        For i := 1 to Size do
          begin
            fHuffmanTree.IncreaseFrequency(BuffPtr^);
            Inc(BuffPtr);
          end;
      end
    else raise ESHInvalidState.Create('THuffmanBase.ScanUpdate: Scanning already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanBase.ScanUpdate: Scanning not initialized.');
end;

//------------------------------------------------------------------------------

Function THuffmanBase.ScanFinal: Int64;
var
  i:  Integer;
begin
If fScanInitialized then
  begin
    If not fScanFinalized then
      begin
        fHuffmanTree.ConstructTree;
        // get encoded size in bits...
        Result := 7;
        For i := fHuffmanTree.LowByteNodeIndex to fHuffmanTree.HighByteNodeIndex do
          Inc(Result,(fHuffmanTree[i].Frequency * fHuffmanTree[i].BitSequence.Length));
        Inc(Result,fHuffmanTree.TerminatorNode.BitSequence.Length);
      {
        ...and convert to bytes - let's hope there is no overflow, 1 EiB (one
        exbibyte, 2^60) should be enough for everyone ;)
      }
        Result := Result shr 3;
        fScanFinalized := True;
      end
    else raise ESHInvalidState.Create('THuffmanBase.ScanFinal: Scanning already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanBase.ScanFinal: Scanning not initialized.');
end;

//------------------------------------------------------------------------------

Function THuffmanBase.ScanMemory(Memory: Pointer; Size: TMemSize): Int64;
var
  MemoryStream: TStaticMemoryStream;
begin
If Size > fStreamBufferSize then
  begin
    MemoryStream := TStaticMemoryStream.Create(Memory,Size);
    try
      MemoryStream.Seek(0,soBeginning);
      Result := ScanStream(MemoryStream);
    finally
      MemoryStream.Free;
    end;
  end
else
  begin
    fBreakProcessing := False;
    DoScanProgress(0.0);
    If not fBreakProcessing then
      begin
        ScanInit;
        ScanUpdate(Memory^,Size);
        Result := ScanFinal;
        DoScanProgress(1.0);
      end
    else Result := 0;
  end;
end;

//------------------------------------------------------------------------------

Function THuffmanBase.ScanBuffer(const Buffer; Size: TMemSize): Int64;
begin
Result := ScanMemory(@Buffer,Size);
end;

//------------------------------------------------------------------------------

Function THuffmanBase.ScanAnsiString(const Str: AnsiString): Int64;
begin
Result := ScanMemory(PAnsiChar(Str),Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function THuffmanBase.ScanWideString(const Str: WideString): Int64;
begin
Result := ScanMemory(PWideChar(Str),Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function THuffmanBase.ScanString(const Str: String): Int64;
begin
Result := ScanMemory(PChar(Str),Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function THuffmanBase.ScanStream(Stream: TStream; Count: Int64 = -1): Int64;
var
  InitialCount: Int64;
  Buffer:       Pointer;
  BytesRead:    Integer;
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
    InitialCount := Count;
    fBreakProcessing := False;
    DoScanProgress(0.0);
    If not fBreakProcessing then
      begin
        ScanInit;
        If InitialCount > 0 then
          begin
            GetMem(Buffer,fStreamBufferSize);
            try
              repeat
                BytesRead := Stream.Read(Buffer^,Min(Int64(fStreamBufferSize),Count));
                ScanUpdate(Buffer^,TMemSize(BytesRead));
                Dec(Count,BytesRead);
                DoScanProgress((InitialCount - Count) / InitialCount);
              until (TMemSize(BytesRead) < fStreamBufferSize) or fBreakProcessing;
            finally
              FreeMem(Buffer,fStreamBufferSize);
            end;
          end;
        If not fBreakProcessing then
          begin
            Result := ScanFinal;
            DoScanProgress(1.0);
          end
        else Result := 0;
      end
    else Result := 0;      
  end
else raise ESHInvalidValue.Create('THuffmanBase.ScanStream: Stream not assigned.');
end;

//------------------------------------------------------------------------------

Function THuffmanBase.ScanFile(const FileName: String): Int64;
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  Result := ScanStream(FileStream);
finally
  FileStream.Free;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 THuffmanEncoder
--------------------------------------------------------------------------------
===============================================================================}
const
  SH_ENCODE_BUFFER_SIZE = 4 * 1024; // 4KiB
  SH_ENCODE_BUFFER_BITS = SH_ENCODE_BUFFER_SIZE * 8;

{===============================================================================
    THuffmanEncoder - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    THuffmanEncoder - protected methods
-------------------------------------------------------------------------------}

procedure THuffmanEncoder.DoEncodedSizeProgress(Progress: Double);
begin
If Assigned(fEncodedSizeProgressEvent) then
  fEncodedSizeProgressEvent(Self,Progress)
else If Assigned(fEncodedSizeProgressCallback) then
  fEncodedSizeProgressCallback(Self,Progress);
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.DoEncodeProgress(Progress: Double);
begin
If Assigned(fEncodeProgressEvent) then
  fEncodeProgressEvent(Self,Progress)
else If Assigned(fEncodeProgressCallback) then
  fEncodeProgressCallback(Self,Progress);
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.Initialize;
begin
inherited;
fEncodedSizeInitialized := False;
fEncodedSizeFinalized := False;
FillChar(fEncodedSizeCounters,SizeOf(fEncodedSizeCounters),0);
fEncodedSizeProgressCallback := nil;
fEncodedSizeProgressEvent := nil;
fEncodeInitialized := False;
fEncodeFinalized := False;
GetMem(fEncodeBuffer,SH_ENCODE_BUFFER_SIZE);
fEncodeBufferBitCount := 0;
fEncodeProgressCallback := nil;
fEncodeProgressEvent := nil;
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.Finalize;
begin
FreeMem(fEncodeBuffer,SH_ENCODE_BUFFER_SIZE);
inherited;
end;

{-------------------------------------------------------------------------------
    THuffmanEncoder - public methods
-------------------------------------------------------------------------------}

procedure THuffmanEncoder.EncodedSizeInit;
begin
If fHuffmanTree.TreeIsReady then
  begin
    fEncodedSizeInitialized := True;
    fEncodedSizeFinalized := False;
    FillChar(fEncodedSizeCounters,SizeOf(fEncodedSizeCounters),0);
  end
else raise ESHInvalidState.Create('THuffmanEncoder.EncodedSizeInit: Tree not ready.');
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodedSizeUpdate(const Buffer; Size: TMemSize);
var
  BuffPtr:  PUInt8;
  i:        TMemSize;
begin
If fEncodedSizeInitialized then
  begin
    If not fEncodedSizeFinalized then
      begin
        BuffPtr := @Buffer;
        For i := 1 to Size do
          begin
            Inc(fEncodedSizeCounters[BuffPtr^]);
            Inc(BuffPtr);
          end;
      end
    else raise ESHInvalidState.Create('THuffmanEncoder.EncodedSizeUpdate: Encoded size obtaining already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanEncoder.EncodedSizeUpdate: Encoded size obtaining not initialized.');
end;

//------------------------------------------------------------------------------

Function THuffmanEncoder.EncodedSizeFinal: Int64;
var
  i:  Integer;
begin
If fEncodedSizeInitialized then
  begin
    If not fEncodedSizeFinalized then
      begin
        Result := 7;
        For i := Low(fEncodedSizeCounters) to High(fEncodedSizeCounters) do
          Inc(Result,fEncodedSizeCounters[i] * fHuffmanTree[i].BitSequence.Length);
        Inc(Result,fHuffmanTree.TerminatorNode.BitSequence.Length);
        // convert bits to bytes
        Result := Result shr 3;
        fEncodedSizeFinalized := True;
      end
    else raise ESHInvalidState.Create('THuffmanEncoder.EncodedSizeFinal: Encoded size obtaining already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanEncoder.EncodedSizeFinal: Encoded size obtaining not initialized.');
end;

//------------------------------------------------------------------------------

Function THuffmanEncoder.EncodedSizeMemory(Memory: Pointer; Size: TMemSize): Int64;
var
  MemoryStream: TStaticMemoryStream;
begin
If Size > fStreamBufferSize then
  begin
    MemoryStream := TStaticMemoryStream.Create(Memory,Size);
    try
      MemoryStream.Seek(0,soBeginning);
      Result := EncodedSizeStream(MemoryStream);
    finally
      MemoryStream.Free;
    end;
  end
else
  begin
    fBreakProcessing := False;
    DoEncodedSizeProgress(0.0);
    If not fBreakProcessing then
      begin
        EncodedSizeInit;
        EncodedSizeUpdate(Memory^,Size);
        Result := EncodedSizeFinal;
        DoEncodedSizeProgress(1.0);
      end
    else Result := 0;
  end;
end;

//------------------------------------------------------------------------------

Function THuffmanEncoder.EncodedSizeBuffer(const Buffer; Size: TMemSize): Int64;
begin
Result := EncodedSizeMemory(@Buffer,Size);
end;

//------------------------------------------------------------------------------

Function THuffmanEncoder.EncodedSizeAnsiString(const Str: AnsiString): Int64;
begin
Result := EncodedSizeMemory(PAnsiChar(Str),Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function THuffmanEncoder.EncodedSizeWideString(const Str: WideString): Int64;
begin
Result := EncodedSizeMemory(PWideChar(Str),Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function THuffmanEncoder.EncodedSizeString(const Str: String): Int64;
begin
Result := EncodedSizeMemory(PChar(Str),Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function THuffmanEncoder.EncodedSizeStream(Stream: TStream; Count: Int64 = -1): Int64;
var
  InitialCount: Int64;
  Buffer:       Pointer;
  BytesRead:    Integer;
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
    InitialCount := Count;
    fBreakProcessing := False;
    DoEncodedSizeProgress(0.0);
    If not fBreakProcessing  then
      begin
        EncodedSizeInit;
        If InitialCount > 0 then
          begin
            GetMem(Buffer,fStreamBufferSize);
            try
              repeat
                BytesRead := Stream.Read(Buffer^,Min(Int64(fStreamBufferSize),Count));
                EncodedSizeUpdate(Buffer^,TMemSize(BytesRead));
                Dec(Count,BytesRead);
                DoEncodedSizeProgress((InitialCount - Count) / InitialCount);
              until (TMemSize(BytesRead) < fStreamBufferSize) or fBreakProcessing;
            finally
              FreeMem(Buffer,fStreamBufferSize);
            end;
          end;
        If not fBreakProcessing then
          begin
            Result := EncodedSizeFinal;
            DoEncodedSizeProgress(1.0);
          end
        else Result := 0;
      end
    else Result := 0;      
  end
else raise ESHInvalidValue.Create('THuffmanEncoder.EncodedSizeStream: Stream not assigned.');
end;

//------------------------------------------------------------------------------

Function THuffmanEncoder.EncodedSizeFile(const FileName: String): Int64;
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  Result := EncodedSizeStream(FileStream);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodeInit;
begin
If fHuffmanTree.TreeIsReady then
  begin
    fUncompressedSize := 0;
    fCompressedSize := 0;
    fCompressionRatio := 0.0;
    fEncodeInitialized := True;
    fEncodeFinalized := False;
    FillChar(fEncodeBuffer^,SH_ENCODE_BUFFER_SIZE,0);
    fEncodeBufferBitCount := 0;
  end
else raise ESHInvalidState.Create('THuffmanEncoder.EncodeInit: Tree not ready.');
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodeUpdate(const BufferIn; var SizeIn: TMemSize; out BufferOut; var SizeOut: TMemSize);
var
  BuffInPtr:  PUInt8;   // moving pointer for input buffer
  BuffOutPtr: PUInt8;   // moving pointer for output buffer
  BytesIn:    TMemSize; // how many bytes is left in input buffer
  BytesOut:   TMemSize; // how many bytes can still fit into output buffer

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function ConsumeBytes: Boolean;
  var
    EncodeBufferPtr:  Pointer;
    BitSequence:      TSHBitSequence;
  begin
    Result := False;
    EncodeBufferPtr := PtrAdvance(fEncodeBuffer,fEncodeBufferBitCount shr 3);
    while BytesIn > 0 do
      begin
        BitSequence := fHuffmanTree.BitSequences[BuffInPtr^];
        If (fEncodeBufferBitCount + BitSequence.Length) <= SH_ENCODE_BUFFER_BITS then
          begin
            PutBitsAndMoveDest(PUInt8(EncodeBufferPtr),fEncodeBufferBitCount and 7,
                               PUInt8(@BitSequence.Data),BitSequence.Length);
            Inc(BuffInPtr);
            Dec(BytesIn);
            Inc(fEncodeBufferBitCount,BitSequence.Length);
            Inc(fUncompressedSize);
            Result := True;
          end
        else Break{while...};
      end;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

  Function ProduceBytes: Boolean;
  var
    BytesToMove:  TMemSize;
  begin
    If (BytesOut > 0) and (fEncodeBufferBitCount >= 8) then
      begin
        BytesToMove := TMemSize(Min(Int64(BytesOut),Int64(fEncodeBufferBitCount shr 3)));
        Move(fEncodeBuffer^,BuffOutPtr^,BytesToMove);
        Inc(BuffOutPtr,BytesToMove);
        Dec(BytesOut,BytesToMove);
        BufferShiftDown(fEncodeBuffer^,(fEncodeBufferBitCount + 7) shr 3,BytesToMove);
        Dec(fEncodeBufferBitCount,TMemSize(BytesToMove shl 3));
        Inc(fCompressedSize,BytesToMove);
        Result := True;
      end
    else Result := False;
  end;

//--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

var
  Consumed: Boolean;
  Produced: Boolean;
begin
If fEncodeInitialized then
  begin
    If not fEncodeFinalized then
      begin
        BuffInPtr := @BufferIn;
        BuffOutPtr := @BufferOut;
        BytesIn := SizeIn;
        BytesOut := SizeOut;
        Consumed := True;
        Produced := True;
        while Consumed or Produced do
          begin
            Consumed := ConsumeBytes;
            Produced := ProduceBytes;
          end;
        SizeIn := SizeIn - BytesIn;
        SizeOut := SizeOut - BytesOut;
      end
    else raise ESHInvalidState.Create('THuffmanEncoder.EncodeUpdate: Encoding already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanEncoder.EncodeUpdate: Encoding not initialized.');
end;

//------------------------------------------------------------------------------

Function THuffmanEncoder.EncodeFinal(out BufferOut; var SizeOut: TMemSize): Boolean;
var
  BuffOutPtr:       PUInt8;
  BitSequence:      TSHBitSequence;
  EncodeBufferPtr:  PUInt8;

  Function StoreWholeBytes: TMemSize;
  begin
    Result := fEncodeBufferBitCount shr 3;
    If Result > 0 then
      begin
        Move(fEncodeBuffer^,BuffOutPtr^,Result);
        Inc(BuffOutPtr,Result);
        BufferShiftDown(fEncodeBuffer^,(fEncodeBufferBitCount + 7) shr 3,Result);
        Dec(fEncodeBufferBitCount,Result shl 3{Result * 8});
      end;
  end;

begin
If fEncodeInitialized then
  begin
    If not fEncodeFinalized then
      begin
        If SizeOut >= ((fEncodeBufferBitCount + fHuffmanTree.TerminatorNode.BitSequence.Length + 7) shr 3) then
          begin
            // buffered data can fit into output
            BuffOutPtr := @BufferOut;
            // move whole bytes
            SizeOut := StoreWholeBytes;
            // store terminator
            BitSequence := fHuffmanTree.TerminatorNode.BitSequence;
            If (fEncodeBufferBitCount + BitSequence.Length) <= SH_ENCODE_BUFFER_BITS then
              begin
                EncodeBufferPtr := fEncodeBuffer;
                PutBitsAndMoveDest(EncodeBufferPtr,fEncodeBufferBitCount and 7,PUInt8(@BitSequence.Data),BitSequence.Length);
                Inc(fEncodeBufferBitCount,BitSequence.Length);
              end
            else raise ESHBufferTooSmall.CreateFmt('THuffmanEncoder.EncodeFinal: Internal buffer too small (%u).',[SH_ENCODE_BUFFER_SIZE]);
            Inc(SizeOut,StoreWholeBytes);
            // now the remaining partial byte (mask only valid bits)
            If fEncodeBufferBitCount and 7 <> 0 then
              begin
                BuffOutPtr^ := PUInt8(PtrAdvance(fEncodeBuffer,PtrInt(SizeOut)))^ and
                               (UInt8($FF) shr (8 - (fEncodeBufferBitCount and 7)));
                Inc(SizeOut);
              end;
            // final touches
            Inc(fCompressedSize,SizeOut);
            If fUncompressedSize <> 0 then
              fCompressionRatio := fCompressedSize / fUncompressedSize
            else
              fCompressionRatio := 0.0;
            fEncodeFinalized := True;
            Result := True;
          end
        else
          begin
            // buffered data cannot fit into output
            SizeOut := (fEncodeBufferBitCount + fHuffmanTree.TerminatorNode.BitSequence.Length + 7) shr 3;
            Result := False;
          end;
      end
    else raise ESHInvalidState.Create('THuffmanEncoder.EncodeFinal: Encoding already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanEncoder.EncodeFinal: Encoding not initialized.');
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodeMemory(MemoryIn: Pointer; SizeIn: TMemSize; MemoryOut: Pointer; SizeOut: TMemSize);
var
  OrigSizeOut:      TMemSize;
  MemoryStreamIn:   TStaticMemoryStream;
  MemoryStreamOut:  TWritableStaticMemoryStream;
  TempSizeIn:       TMemSize;
  TempSizeOut:      TMemSize;
begin
OrigSizeOut := SizeOut; // only for potential exceptions
If SizeIn > fStreamBufferSize then
  begin
    MemoryStreamIn := TStaticMemoryStream.Create(MemoryIn,SizeIn);
    try
      MemoryStreamIn.Seek(0,soBeginning);    
      MemoryStreamOut := TWritableStaticMemoryStream.Create(MemoryOut,SizeOut);
      try
        MemoryStreamOut.Seek(0,soBeginning);
        EncodeStream(MemoryStreamIn,MemoryStreamOut);
      finally
        MemoryStreamOut.Free;
      end;
    finally
      MemoryStreamIn.Free;
    end;
  end
else
  begin
    fBreakProcessing := False;
    DoEncodeProgress(0.0);
    If not fBreakProcessing then
      begin
        EncodeInit;
        TempSizeIn := SizeIn;
        TempSizeOut := SizeOut;
        EncodeUpdate(MemoryIn^,TempSizeIn,MemoryOut^,TempSizeOut);
      {
        EncodeUpdate encodes everything it can, so if not all data were
        consumed it can only mean the output buffer is too small for them.
      }
        If TempSizeIn >= SizeIn then
          begin
            SizeOut := SizeOut - TempSizeOut;
            If not EncodeFinal(PtrAdvance(MemoryOut,TempSizeOut)^,SizeOut) then
              raise ESHBufferTooSmall.CreateFmt('THuffmanEncoder.EncodeMemory: Output buffer too small (%u).',[OrigSizeOut]);
            DoEncodeProgress(1.0);
          end
        else raise ESHBufferTooSmall.CreateFmt('THuffmanEncoder.EncodeMemory: Output buffer too small (%u).',[OrigSizeOut]);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodeBuffer(const BufferIn; SizeIn: TMemSize; out BufferOut; SizeOut: TMemSize);
begin
EncodeMemory(@BufferIn,SizeIn,@BufferOut,Sizeout);
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodeAnsiString(const StrIn: AnsiString; var StrOut: AnsiString);
begin
EncodeMemory(PAnsiChar(StrIn),Length(StrIn) * SizeOf(AnsiChar),PAnsiChar(StrOut),Length(StrOut) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodeWideString(const StrIn: WideString; var StrOut: WideString);
begin
EncodeMemory(PWideChar(StrIn),Length(StrIn) * SizeOf(WideChar),PWideChar(StrOut),Length(StrOut) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodeString(const StrIn: String; var StrOut: String);
begin
EncodeMemory(PChar(StrIn),Length(StrIn) * SizeOf(Char),PChar(StrOut),Length(StrOut) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodeStream(StreamIn: TStream; CountIn: Int64; StreamOut: TStream);
var
  InitialCountIn:   Int64;
  BufferIn:         Pointer;
  BufferInPtr:      Pointer;  // moving pointer to input buffer
  BufferOut:        Pointer;
  BytesRead:        TMemSize;
  BytesIn:          TMemSize;
  BytesOut:         TMemSize;
  UnprocessedBytes: TMemSize;
begin
If Assigned(StreamIn) then
  begin
    If Assigned(StreamOut) then
      begin
        If CountIn = 0 then
          CountIn := StreamIn.Size - StreamIn.Position;
        If CountIn < 0 then
          begin
            StreamIn.Seek(0,soBeginning);
            CountIn := StreamIn.Size;
          end;
        InitialCountIn := CountIn;
        fBreakProcessing := False;
        DoEncodeProgress(0.0);
        If not fBreakProcessing then
          begin
            GetMem(BufferOut,fStreamBufferSize);
            try
              EncodeInit;
              If InitialCountIn > 0 then
                begin
                  GetMem(BufferIn,fStreamBufferSize);
                  try
                    repeat
                      BytesRead := TMemSize(StreamIn.Read(BufferIn^,Min(Int64(fStreamBufferSize),CountIn)));
                      BufferInPtr := BufferIn;
                      UnprocessedBytes := BytesRead;
                      // process everything that was read
                      repeat
                        BytesIn := UnprocessedBytes;
                        BytesOut := fStreamBufferSize;
                        EncodeUpdate(BufferInPtr^,BytesIn,BufferOut^,BytesOut);
                        If TMemSize(StreamOut.Write(BufferOut^,BytesOut)) >= BytesOut then
                          begin
                            PtrAdvanceVar(BufferInPtr,BytesIn);
                            UnprocessedBytes := UnprocessedBytes - BytesIn;
                          end
                        else raise ESHBufferTooSmall.Create('THuffmanEncoder.EncodeStream: Output stream too small.');
                      until (UnprocessedBytes <= 0) and (BytesOut < fStreamBufferSize);
                      Dec(CountIn,BytesRead);
                      DoEncodeProgress((InitialCountIn - CountIn) / InitialCountIn);
                    until (BytesRead < fStreamBufferSize) or fBreakProcessing;
                  finally
                    FreeMem(BufferIn,fStreamBufferSize);
                  end;
                end;
              If not fBreakProcessing then
                begin
                  // there should be at max. only few bytes buffered at this point
                  BytesOut := fStreamBufferSize;
                  If EncodeFinal(BufferOut^,BytesOut) then
                    begin
                      If TMemSize(StreamOut.Write(BufferOut^,BytesOut)) < BytesOut then
                        raise ESHBufferTooSmall.Create('THuffmanEncoder.EncodeStream: Output stream too small.');
                    end
                  else raise ESHBufferTooSmall.Create('THuffmanEncoder.EncodeStream: Output buffer too small.');
                end;
            finally
              FreeMem(BufferOut,fStreamBufferSize);
            end;
            DoEncodeProgress(1.0);
          end;
      end
    else raise ESHInvalidValue.Create('THuffmanEncoder.EncodeStream: Output stream not assigned.');
  end
else raise ESHInvalidValue.Create('THuffmanEncoder.EncodeStream: Input stream not assigned.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure THuffmanEncoder.EncodeStream(StreamIn: TStream; StreamOut: TStream);
begin
EncodeStream(StreamIn,-1,StreamOut);
end;

//------------------------------------------------------------------------------

procedure THuffmanEncoder.EncodeFile(const FileNameIn,FileNameOut: String);
var
  FileStreamIn:   TFileStream;
  FileStreamOut:  TFileStream;
begin
FileStreamIn := TFileStream.Create(StrToRTL(FileNameIn),fmOpenRead or fmShareDenyWrite);
try
  FileStreamIn.Seek(0,soBeginning);
  FileStreamOut := TFileStream.Create(StrToRTL(FileNameOut),fmCreate or fmShareDenyWrite);
  try
    FileStreamOut.Seek(0,soBeginning);
    EncodeStream(FileStreamIn,FileStreamOut);
  finally
    FileStreamOut.Free;
  end;
finally
  FileStreamIn.Free;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 THuffmanDecoder
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    THuffmanDecoder - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    THuffmanDecoder - protected methods
-------------------------------------------------------------------------------}

procedure THuffmanDecoder.DoDecodedSizeProgress(Progress: Double);
begin
If Assigned(fDecodedSizeProgressEvent) then
  fDecodedSizeProgressEvent(Self,Progress)
else If Assigned(fDecodedSizeProgressCallback) then
  fDecodedSizeProgressCallback(Self,Progress);
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DoDecodeProgress(Progress: Double);
begin
If Assigned(fDecodeProgressEvent) then
  fDecodeProgressEvent(Self,Progress)
else If Assigned(fDecodeProgressCallback) then
  fDecodeProgressCallback(Self,Progress);
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.Initialize;
begin
inherited;
fDecodedSizeInitialized := False;
fDecodedSizeFinalized := False;
fDecodedSizeContext.TreeNodeIndex := -1;
fDecodedSizeContext.Counter := 0;
fDecodedSizeContext.Terminated := False;
fDecodedSizeProgressCallback := nil;
fDecodedSizeProgressEvent := nil;
fDecodeInitialized := False;
fDecodeFinalized := False;
fDecodeContext.TreeNodeIndex := -1;
fDecodeContext.TransferInBits := 0;
fDecodeContext.TransferInBitCount := 0;
fDecodeContext.TransferOutByte := 0;
fDecodeContext.TransferOutByteSet := False;
fDecodeContext.Terminated := False;
fDecodeProgressCallback := nil;
fDecodeProgressEvent := nil;
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeUpdateInternal(const BufferIn; var SizeIn: TMemSize; out BufferOut; var SizeOut: TMemSize);
var
  BuffInPtr:    PUInt8;
  BuffOutPtr:   PUInt8;
  BytesIn:      TMemSize; // number of consumed input bytes
  BytesOut:     TMemSize; // number of produced output bytes
  LocalContext: TSHDecodeContext;  
  CanContinue:  Boolean;
{$IFNDEF UseSecureTreeTraversal}
  TempNode:     PSHHuffmanTreeNode;
{$ENDIF}

  Function TransferBufferedOutByte: Boolean;
  begin
    Result := True;
    If LocalContext.TransferOutByteSet then
      begin
        If SizeOut > 0 then
          begin
            BuffOutPtr^ := LocalContext.TransferOutByte;
            LocalContext.TransferOutByteSet := False;
            Dec(SizeOut);
            Inc(BytesOut);
            Inc(BuffOutPtr);
            Inc(fUncompressedSize);
          end
        else Result := False;
      end;
  end;

  Function DecodeAndTransferBits: Integer;
  begin
    Result := 0;
    while (LocalContext.TransferInBitCount > 0) and CanContinue do
      begin
     {$IFDEF UseSecureTreeTraversal}

        If not fHuffmanTree.TraverseTree((LocalContext.TransferInBits and 1) <> 0,LocalContext.TreeNodeIndex) then
          with fHuffmanTree.TreeNodes[LocalContext.TreeNodeIndex] do
            begin
              If NodeKind <> nkTerminator then
                begin
                  LocalContext.TreeNodeIndex := -1;
      {$ELSE}
        TempNode := TempNode^.ChildNodes[(LocalContext.TransferInBits and 1) <> 0];
          If TempNode^.NodeKind <> nkBranch then  // I know this should not be indented...
            begin
              If TempNode^.NodeKind <> nkTerminator then
                begin
      {$ENDIF}
                  If SizeOut > 0 then
                    begin
                    {$IFDEF UseSecureTreeTraversal}
                      BuffOutPtr^ := ByteIndex;
                    {$ELSE}
                      BuffOutPtr^ := TempNode^.ByteIndex;
                    {$ENDIF}
                      Dec(SizeOut);
                      Inc(BytesOut);
                      Inc(BuffOutPtr);
                      Inc(fUncompressedSize);
                    end
                  else
                    begin
                    {$IFDEF UseSecureTreeTraversal}
                      LocalContext.TransferOutByte := ByteIndex;
                    {$ELSE}
                      LocalContext.TransferOutByte := TempNode^.ByteIndex;
                    {$ENDIF}
                      LocalContext.TransferOutByteSet := True;
                      CanContinue := False;
                    end;
                {$IFNDEF UseSecureTreeTraversal}
                  TempNode := fHuffmanTree.RootNode;
                {$ENDIF}
                end
              else
                begin
                  LocalContext.Terminated := True;
                  CanContinue := False;
                end;
            end;
        LocalContext.TransferInBits := LocalContext.TransferInBits shr 1;
        Dec(LocalContext.TransferInBitCount);
        Inc(Result);
      end;
  end;

begin
BuffInPtr := @BufferIn;
BuffOutPtr := @BufferOut;
BytesIn := 0;
BytesOut := 0;
// get global data into local copies
LocalContext := fDecodeContext;
{$IFNDEF UseSecureTreeTraversal}
If LocalContext.TreeNodeIndex < 0 then
  TempNode := fHuffmanTree.RootNode
else
  TempNode := fHuffmanTree.TreeNodePtrs[LocalContext.TreeNodeIndex];
{$ENDIF}
try
  If TransferBufferedOutByte then
    begin
      CanContinue := True;
      // consume transfered bits
      If (LocalContext.TransferInBitCount > 0) then
        Inc(fCompressedSize,DecodeAndTransferBits);
      // consume input data, first words...
      while (SizeIn > SizeOf(NativeUInt)) and CanContinue and not LocalContext.Terminated do
        begin
          LocalContext.TransferInBits := PNativeUInt(BuffInPtr)^;
          LocalContext.TransferInBitCount := SizeOf(NativeUInt) * 8;
          Dec(SizeIn,SizeOf(NativeUInt));
          Inc(BytesIn,SizeOf(NativeUInt));
          Inc(BuffInPtr,SizeOf(NativeUInt));
          Inc(fCompressedSize,DecodeAndTransferBits);
        end;
      // ...and then remaining odd bytes
      while (SizeIn > 0) and CanContinue and not LocalContext.Terminated do
        begin
          LocalContext.TransferInBits := BuffInPtr^;
          LocalContext.TransferInBitCount := 8;
          Dec(SizeIn);
          Inc(BytesIn);
          Inc(BuffInPtr);
          Inc(fCompressedSize,DecodeAndTransferBits);
        end;
    end;
finally
{$IFNDEF UseSecureTreeTraversal}
  LocalContext.TreeNodeIndex := TempNode^.TreeIndex;
{$ENDIF}
  fDecodeContext := LocalContext;
end;
// return number of consumed and produced bytes
SizeIn := BytesIn;
SizeOut := BytesOut;
end;

{-------------------------------------------------------------------------------
    THuffmanDecoder - public methods
-------------------------------------------------------------------------------}

procedure THuffmanDecoder.DecodedSizeInit;
begin
If fHuffmanTree.TreeIsReady then
  begin
    fDecodedSizeInitialized := True;
    fDecodedSizeFinalized := False;
    fDecodedSizeContext.TreeNodeIndex := -1;
    fDecodedSizeContext.Counter := 0;
    fDecodedSizeContext.Terminated := False;
  end
else raise ESHInvalidState.Create('THuffmanDecoder.DecodedSizeInit: Tree not ready.');
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodedSizeUpdate(const Buffer; Size: TMemSize): Boolean;
var
  LocalContext: TSHDecodedSizeContext;
{$IFNDEF UseSecureTreeTraversal}
  TempNode:     PSHHuffmanTreeNode;
{$ENDIF}

  procedure DecodeBits(IntBits: NativeUInt; HighBit: Integer);
  var
    i:  Integer;
  begin
    For i := 0 to HighBit do
      begin
      {$IFDEF UseSecureTreeTraversal}
        If not fHuffmanTree.TraverseTree(((IntBits shr i) and 1) <> 0,LocalContext.TreeNodeIndex) then
          begin
            If fHuffmanTree.TreeNodes[LocalContext.TreeNodeIndex].NodeKind <> nkTerminator then
      {$ELSE}
        // because THuffmanTree.TraverseTree is slow...
        TempNode := TempNode^.ChildNodes[((IntBits shr i) and 1) <> 0];
        If TempNode^.NodeKind <> nkBranch then
          begin
            If TempNode^.NodeKind <> nkTerminator then
      {$ENDIF}
              begin
                // normal byte node
                Inc(LocalContext.Counter);
              {$IFDEF UseSecureTreeTraversal}
                LocalContext.TreeNodeIndex := -1;
              {$ELSE}
                TempNode := fHuffmanTree.RootNode;
              {$ENDIF}
              end
            else
              begin
                // terminator node
                LocalContext.Terminated := True;
                Break{For i};
              end;
          end;
      end;
  end;

var
  BuffPtr:  PUInt8;
  i:        TMemSize;
begin
If fDecodedSizeInitialized then
  begin
    If not fDecodedSizeFinalized then
      begin
        LocalContext := fDecodedSizeContext;
      {$IFNDEF UseSecureTreeTraversal}
        If LocalContext.TreeNodeIndex < 0 then
          TempNode := fHuffmanTree.RootNode
        else
          TempNode := fHuffmanTree.TreeNodePtrs[LocalContext.TreeNodeIndex];
      {$ENDIF}
        try
          BuffPtr := @Buffer;
          For i := 1 to (Size shr NativeUIntSizeShift) do
            If not LocalContext.Terminated then
              begin
                DecodeBits(PNativeUInt(BuffPtr)^,Pred(SizeOf(NativeUInt) * 8));
                Inc(BuffPtr,SizeOf(NativeUInt));
              end
            else Break{For i};
          For i := 1 to (Size and Pred(SizeOf(NativeUInt))) do
            If not LocalContext.Terminated then
              begin
                DecodeBits(BuffPtr^,7);
                Inc(BuffPtr);
              end
            else Break{For i};
        finally
        {$IFNDEF UseSecureTreeTraversal}
          LocalContext.TreeNodeIndex := TempNode^.TreeIndex;
        {$ENDIF}
          fDecodedSizeContext := LocalContext;
        end;
      end
    else raise ESHInvalidState.Create('THuffmanDecoder.DecodedSizeUpdate: Decoded size obtaining already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanDecoder.DecodedSizeUpdate: Decoded size obtaining not initialized.');
Result := not fDecodedSizeContext.Terminated;
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodedSizeFinal: Int64;
begin
If fDecodedSizeInitialized then
  begin
    If not fDecodedSizeFinalized then
      begin
        If fDecodedSizeContext.Terminated then
          begin
            Result := fDecodedSizeContext.Counter;
            fDecodedSizeFinalized := True;
          end
        else raise ESHInvalidState.Create('THuffmanDecoder.DecodedSizeFinal: Encoded data are not properly terminated.');
      end
    else raise ESHInvalidState.Create('THuffmanDecoder.DecodedSizeFinal: Decoded size obtaining already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanDecoder.DecodedSizeFinal: Decoded size obtaining not initialized.');
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodedSizeMemory(Memory: Pointer; Size: TMemSize): Int64;
var
  MemoryStream: TStaticMemoryStream;
begin
If Size > fStreamBufferSize then
  begin
    MemoryStream := TStaticMemoryStream.Create(Memory,Size);
    try
      MemoryStream.Seek(0,soBeginning);
      Result := DecodedSizeStream(MemoryStream);
    finally
      MemoryStream.Free;
    end;
  end
else
  begin
    fBreakProcessing := False;
    DoDecodedSizeProgress(0.0);
    If not fBreakProcessing then
      begin
        DecodedSizeInit;
        DecodedSizeUpdate(Memory^,Size);
        Result := DecodedSizeFinal;
        DoDecodedSizeProgress(1.0);
      end
    else Result := 0;
  end;
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodedSizeBuffer(const Buffer; Size: TMemSize): Int64;
begin
Result := DecodedSizeMemory(@Buffer,Size);
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodedSizeAnsiString(const Str: AnsiString): Int64;
begin
Result := DecodedSizeMemory(PAnsiChar(Str),Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodedSizeWideString(const Str: WideString): Int64;
begin
Result := DecodedSizeMemory(PWideChar(Str),Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodedSizeString(const Str: String): Int64;
begin
Result := DecodedSizeMemory(PChar(Str),Length(Str) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodedSizeStream(Stream: TStream; Count: Int64 = -1): Int64;
var
  InitialCount: Int64;
  Buffer:       Pointer;
  BytesRead:    Integer;
  CanContinue:  Boolean;
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
    InitialCount := Count;
    fBreakProcessing := False;
    DoDecodedSizeProgress(0.0);
    If not fBreakProcessing  then
      begin
        DecodedSizeInit;
        If InitialCount > 0 then
          begin
            GetMem(Buffer,fStreamBufferSize);
            try
              repeat
                BytesRead := Stream.Read(Buffer^,Min(Int64(fStreamBufferSize),Count));
                CanContinue := DecodedSizeUpdate(Buffer^,TMemSize(BytesRead));
                Dec(Count,BytesRead);
                DoDecodedSizeProgress((InitialCount - Count) / InitialCount);
              until (TMemSize(BytesRead) < fStreamBufferSize) or fBreakProcessing or not CanContinue;
            finally
              FreeMem(Buffer,fStreamBufferSize);
            end;
          end;
        If not fBreakProcessing then
          begin
            Result := DecodedSizeFinal;
            DoDecodedSizeProgress(1.0);
          end
        else Result := 0;
      end
    else Result := 0;      
  end
else raise ESHInvalidValue.Create('THuffmanDecoder.DecodedSizeStream: Stream not assigned.');
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodedSizeFile(const FileName: String): Int64;
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  Result := DecodedSizeStream(FileStream);
finally
  FileStream.Free;
end;
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeInit;
begin
If fHuffmanTree.TreeIsReady then
  begin
    fUncompressedSize := 0;
    fCompressedSize := 0;
    fCompressionRatio := 0.0;
    fDecodeInitialized := True;
    fDecodeFinalized := False;
    fDecodeContext.TreeNodeIndex := -1;
    fDecodeContext.TransferInBits := 0;
    fDecodeContext.TransferInBitCount := 0;
    fDecodeContext.TransferOutByte := 0;
    fDecodeContext.TransferOutByteSet := False;    
    fDecodeContext.Terminated := False;
  end
else raise ESHInvalidState.Create('THuffmanDecoder.DecodeInit: Tree not ready.');
end;

//------------------------------------------------------------------------------

Function THuffmanDecoder.DecodeUpdate(const BufferIn; var SizeIn: TMemSize; out BufferOut; var SizeOut: TMemSize): Boolean;
begin
If fDecodeInitialized then
  begin
    If not fDecodeFinalized then
      begin
        If not fDecodeContext.Terminated then
          DecodeUpdateInternal(BufferIn,SizeIn,BufferOut,SizeOut);
      end
    else raise ESHInvalidState.Create('THuffmanDecoder.DecodeUpdate: Decoding already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanDecoder.DecodeUpdate: Decoding not initialized.');
Result := not fDecodeContext.Terminated;
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeFinal;
begin
If fDecodeInitialized then
  begin
    If not fDecodeFinalized then
      begin
        If fDecodeContext.Terminated then
          begin
            // note that compressed size is in bits here, convert it to bytes
            fCompressedSize := (fCompressedSize + 7) shr 3;
            If fUncompressedSize <> 0 then
              fCompressionRatio := fCompressedSize / fUncompressedSize
            else
              fCompressionRatio := 0.0;
            fDecodeFinalized := True;
          end
        else raise ESHInvalidState.Create('THuffmanDecoder.DecodeFinal: Encoded data are not properly terminated.');
      end
    else raise ESHInvalidState.Create('THuffmanDecoder.DecodeFinal: Decoding already finalized.');
  end
else raise ESHInvalidState.Create('THuffmanDecoder.DecodeFinal: Decoding not initialized.');
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeMemory(MemoryIn: Pointer; SizeIn: TMemSize; MemoryOut: Pointer; SizeOut: TMemSize);
var
  MemoryStreamIn:   TStaticMemoryStream;
  MemoryStreamOut:  TWritableStaticMemoryStream;  
begin
If SizeIn > fStreamBufferSize then
  begin
    MemoryStreamIn := TStaticMemoryStream.Create(MemoryIn,SizeIn);
    try
      MemoryStreamIn.Seek(0,soBeginning);
      MemoryStreamOut := TWritableStaticMemoryStream.Create(MemoryOut,SizeOut);
      try
        MemoryStreamOut.Seek(0,soBeginning);
        DecodeStream(MemoryStreamIn,MemoryStreamOut);
      finally
        MemoryStreamOut.Free;
      end;
    finally
      MemoryStreamIn.Free;
    end;
  end
else
  begin
    fBreakProcessing := False;
    DoDecodeProgress(0.0);
    If not fBreakProcessing then
      begin
        DecodeInit;
        DecodeUpdate(MemoryIn^,SizeIn,MemoryOut^,SizeOut);
        DecodeFinal;
        DoDecodeProgress(1.0);  
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeBuffer(const BufferIn; SizeIn: TMemSize; out BufferOut; SizeOut: TMemSize);
begin
DecodeMemory(@BufferIn,SizeIn,@BufferOut,Sizeout);
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeAnsiString(const StrIn: AnsiString; var StrOut: AnsiString);
begin
DecodeMemory(PAnsiChar(StrIn),Length(StrIn) * SizeOf(AnsiChar),PAnsiChar(StrOut),Length(StrOut) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeWideString(const StrIn: WideString; var StrOut: WideString);
begin
DecodeMemory(PWideChar(StrIn),Length(StrIn) * SizeOf(WideChar),PWideChar(StrOut),Length(StrOut) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeString(const StrIn: String; var StrOut: String);
begin
DecodeMemory(PChar(StrIn),Length(StrIn) * SizeOf(Char),PChar(StrOut),Length(StrOut) * SizeOf(Char));
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeStream(StreamIn: TStream; CountIn: Int64; StreamOut: TStream);
var
  InitialCountIn:   Int64;
  BufferIn:         Pointer;
  BufferInPtr:      Pointer;  // moving pointer to input buffer
  BufferOut:        Pointer;
  BytesRead:        TMemSize;
  BytesIn:          TMemSize;
  BytesOut:         TMemSize;
  UnprocessedBytes: TMemSize;
  CanContinue:      Boolean;
begin
If Assigned(StreamIn) then
  begin
    If Assigned(StreamOut) then
      begin  
        // resolve input count
        If CountIn = 0 then
          CountIn := StreamIn.Size - StreamIn.Position;
        If CountIn < 0 then
          begin
            StreamIn.Seek(0,soBeginning);
            CountIn := StreamIn.Size;
          end;
        InitialCountIn := CountIn;
        fBreakProcessing := False;
        DoDecodeProgress(0.0);
        // main processing (allocate buffers)
        If not fBreakProcessing then
          begin
            DecodeInit;
            If InitialCountIn > 0 then
              begin
                GetMem(BufferIn,fStreamBufferSize);
                try
                  GetMem(BufferOut,fStreamBufferSize);
                  try
                    repeat
                      BytesRead := TMemSize(StreamIn.Read(BufferIn^,Min(Int64(fStreamBufferSize),CountIn)));
                      BufferInPtr := BufferIn;
                      UnprocessedBytes := BytesRead;
                      repeat
                        BytesIn := UnprocessedBytes;
                        BytesOut := fStreamBufferSize;
                        CanContinue := DecodeUpdate(BufferInPtr^,BytesIn,BufferOut^,BytesOut);
                        If TMemSize(StreamOut.Write(BufferOut^,BytesOut)) >= BytesOut then
                          begin
                            PtrAdvanceVar(BufferInPtr,BytesIn);
                            UnprocessedBytes := UnprocessedBytes - BytesIn;
                          end
                        else raise ESHBufferTooSmall.Create('THuffmanDecoder.DecodeStream: Output stream too small.');
                      until (UnprocessedBytes <= 0) or (BytesOut < fStreamBufferSize) or not CanContinue;
                      Dec(CountIn,BytesRead);
                      DoDecodeProgress((InitialCountIn - CountIn) / InitialCountIn);
                    until (BytesRead < fStreamBufferSize) or fBreakProcessing or not CanContinue;
                  finally
                    FreeMem(BufferOut,fStreamBufferSize);
                  end;
                finally
                  FreeMem(BufferIn,fStreamBufferSize);
                end;
              end;
            If not fBreakProcessing then
              begin
                DecodeFinal;
                DoDecodeProgress(1.0);
              end;
          end;
      end
    else raise ESHInvalidValue.Create('THuffmanDecoder.DecodeStream: Output stream not assigned.');
  end
else raise ESHInvalidValue.Create('THuffmanDecoder.DecodeStream: Input stream not assigned.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure THuffmanDecoder.DecodeStream(StreamIn: TStream; StreamOut: TStream);
begin
DecodeStream(StreamIn,-1,StreamOut);
end;

//------------------------------------------------------------------------------

procedure THuffmanDecoder.DecodeFile(const FileNameIn,FileNameOut: String);
var
  FileStreamIn:   TFileStream;
  FileStreamOut:  TFileStream;
begin
FileStreamIn := TFileStream.Create(StrToRTL(FileNameIn),fmOpenRead or fmShareDenyWrite);
try
  FileStreamIn.Seek(0,soBeginning);
  FileStreamOut := TFileStream.Create(StrToRTL(FileNameOut),fmCreate or fmShareDenyWrite);
  try
    FileStreamOut.Seek(0,soBeginning);
    DecodeStream(FileStreamIn,FileStreamOut);
  finally
    FileStreamOut.Free;
  end;
finally
  FileStreamIn.Free;
end;
end;

end.
