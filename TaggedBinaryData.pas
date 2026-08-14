{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  TaggedBinaryData

    Set of very simple classes (descendants of TStream) intended for
    serialization and deserialization of binary data into/from streams.

    Each data point is stored with its tag and the tagged values are split
    into groups called contexts.

    Tag is an 8bit unsigned integer (byte) that can have value between 0 and
    254. Value of 255 is reserved for a context tag - this tag signals that
    next byte(s) are not data, but a new context.
    Context is a 16bit unsigned integer that can be set to any value (none
    is reserved). Since the tag has very limited range, contexts are here to
    allow for more unique identification of data points.

    Unlike tags, which are stored before each data point, context is stored
    only when it changes. You should refrain from changing it too often, as
    each context change stores 4 more bytes into the resulting stream.

    General structure of taged binary data can be described as this:

          TaggedBinaryData
            Signature             - four byte signature ($54, $42, $44, $53)
            ContextGroup[]        - array of context groups
            ClosingSequence       - bytes $FF, $80 (see further for details)

      There can be no context group written, in which case absolutely nothing
      is written in the stream (not even signature or closing sequence - but
      see property WriteEmptyStream of the writer class).

      The closing sequence consinsts of context tag ($FF), which marks a
      context change, followed by context flags without an actual new context
      ID. In the context flags, a close flag is set - this actually marks the
      end of tagged data (usually no other flag is set, so the value $80).

      Context group is a sequence of context tag, context flags, context ID
      and an array of tagged data points:

          ContextGroup
            ContextTag            - tag of value $FF
            ContextFlags          - flags for this context group
            ContextID             - ID of this context group
            TaggedDataPoint[]     - array of tagged data points

      ... where TaggedDataPoint can be seen as:

          TaggedDataPoint
            Tag                   - tag for this data point
            Data                  - actual data (of variable size)

      Note that the first context group might not start with a ContextGroup
      pseudostructure. Instead, it can only contain and array of tagged data
      points. In such case, this group has an implicit ID of 0 and flags are
      empty.

      All metadata (signature, context ID) are written with little endianess.

  Version 1.0.4 (2026-08-10)

  Last change 2026-08-10

  ©2022-2026 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.TaggedBinaryData

  Dependencies:
    AuxClasses          - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions       - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes            - github.com/TheLazyTomcat/Lib.AuxTypes
  * BinaryStreamingLite - github.com/TheLazyTomcat/Lib.BinaryStreamingLite

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol TaggedBinaryData_UseAuxExceptions for details).

  BinaryStreamingLite can be replaced by full BinaryStreaming.

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    ListUtils   - github.com/TheLazyTomcat/Lib.ListUtils
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit TaggedBinaryData;
{
  TaggedBinaryData_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  TaggedBinaryData_UseAuxExceptions to achieve this.
}
{$IF Defined(TaggedBinaryData_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ETBDException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ETBDInvalidValue = class(ETBDException);

  ETBDReadError  = class(ETBDException);
  ETBDWriteError = class(ETBDException);

{===============================================================================
    Common types and constants
===============================================================================}
type
  TTBDContextFlags = UInt8;
  TTBDContextID    = UInt16;
  TTBDTag          = UInt8;

const
  TBD_SIGNATURE = UInt32($53444254);  // TBDS when read as a string

  TBD_CTXFLAGS_FLAG_CLOSE = TTBDContextFlags($80);

  TBD_TAG_CONTEXT = TTBDTag(-1);

{===============================================================================
--------------------------------------------------------------------------------
                             TTaggedBinaryDataWriter                                                                                               
--------------------------------------------------------------------------------
===============================================================================}
{
  Reading is not allowed in writer, a call to method Read (be it explicit or
  implicit) will raise an ETBDReadError exception.

  Seeking is directly passed to destination stream.

  All writes of metadata (context changes, tags and a signature) are deferred
  to a first call to Write method. Meaning no matter how many times you call
  SetContext or SetTag, nothing will be written into destination stream until
  you write some actual data, at which point only the last set context and tag
  will be written (and possibly the signature if at the start of stream).

  At the start, no context is written into destination, unless you explicitly
  change it (first, implicit, context has ID of 0) - first written thing after
  signature will be tag of the first data. If you do not set the tag before
  writing data, it will be 0.

  If current context matches the one last stored, then no context will be
  written, even if you explicitly set it.

  When property WriteEmptyStream is set to false (default value), then, if you
  do not write any data, nothing is stored in the destination, not even the
  signature or closing sequence will be written.
  WriteEmptyStream of true ensures that at least signature and closing sequence
  is written into destination stream, even if you do not write any actual data.

  It is possible to store compound data via multiple calls to write, only the
  first write after SetTag will actually write the tag (and other metadata).

  If context change is written, it also means that a tag is written, even if
  you do not explicitly set it before. This ensures that there are no bare
  (tag-less) data just after the context.

  An example on how to use the writer could be something like this (uses
  BinaryStreaming library):

      Writer := TTaggedBinaryDataWriter.Create(DestinationStream);
      try
        (* implicit context ID (0) *)
        Stream_WriteInt16(Writer.SetTag(0),<value_C0_T0>);
        Stream_WriteFloat32(Writer.SetTag(1),<value_C0_T1>);
        Stream_WriteString(Writer.SetTag(2),<value_C0_T2>);

        Writer.SetContext(1);
        Stream_WriteAnsiChar(Writer.SetTag(0),<value_C1_T0>);

        Writer.SetContext(22);
        Stream_WriteInt64(Writer.SetTag(100),<value_C22_T100>);
        Stream_WriteInt64(Writer.SetTag(200),<value_C22_T200>);
      finally
        Writer.Free;
      end;

    This will produce a following byte sequence in the destination stream:

      5442445300<v0>01<v1>02<v3>FF00010000<v4>FF00160064<v5>C8<v6>FF80

    ...where the bytes have following meanings:

        54424453  - signature
        00        - tag 0
        <v0>      - value_C0_T0
        01        - tag 1
        <v1>      - value_C0_T1
        02        - tag 2
        <v3>      - value_C0_T2
        FF        - context tag
        00        - context flags
        0100      - context ID 1
        00        - tag 0
        <v4>      - value_C1_T0
        FF        - context tag
        00        - context flags
        1600      - context ID 22
        64        - tag 100
        <v5>      - value_C22_T100
        C8        - tag 200
        <v6>      - value_C22_T200
        FF        - context tag
        80        - context flags with close flag set
}
{===============================================================================
    TTaggedBinaryDataWriter - class declaration
===============================================================================}
type
  TTaggedBinaryDataWriter = class(TStream)
  protected
    fDestination:       TStream;
    fDeferredActions:   set of (daWriteSignature,daWriteContext,daWriteTag);
    fPreviousContext:   TTBDContextID;
    fCurrentContext:    TTBDContextID;
    fCurrentTag:        TTBDTag;
    fWriteEmptyStream:  Boolean;
    fMetaBytes:         Int64;
    fDataBytes:         Int64;
    Function GetUtilization: Double; virtual;
    procedure Initialize(Destination: TStream); virtual;
    procedure Finalize; virtual;
    procedure WriteSignature; virtual;
    procedure WriteContext; virtual;
    procedure WriteTag; virtual;
    procedure WriteClose; virtual;
  public
    constructor Create(Destination: TStream);
    destructor Destroy; override;
    Function Read(var Buffer; Count: LongInt): LongInt; override;
    Function Write(const Buffer; Count: LongInt): LongInt; override;
    Function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetContext(Context: TTBDContextID); virtual;
  {
    SetTag

    Returns reference to self, so it can be used for inline tag set and write,
    for example:

      Writer.SetTag(15).WriteBuffer(Buff,SizeOf(Buff));

    Of course, if you hate such constructs, do not use it ;)
  }
    Function SetTag(Tag: TTBDTag): TStream; virtual;
    property Destination: TStream read fDestination;
    property CurrentContext: TTBDContextID read fCurrentContext;
    property CurrentTag: TTBDTag read fCurrentTag;
    property WriteEmptyStream: Boolean read fWriteEmptyStream write fWriteEmptyStream;
  {
    MetaBytes

    Number of bytes written that are not actual data - this includes signature,
    contexts and tags.

    Overwrites (eg. after back-seeking) are not catched - this number counts
    all meta data written, not what eventually ends-up in the stream.   
  }
    property MetaBytes: Int64 read fMetaBytes;
  {
    DataBytes

    Number of bytes written that belongs to the actual user-written data, not
    meta data (tags, contexts, ...).

    Overwrites are not catched - this number counts all actual data written,
    not what eventually ends-up in the stream.
  }
    property DataBytes: Int64 read fDataBytes;
  {
    Utilization

    Indicates how many of written bytes were actual data, not metadata (tags,
    contexts, ...). It is given as normalized value in range [0,1], where 0
    means that all written bytes were metadata, 1 means all written bytes were
    actual usefull data. It is calculated like:

      Utilization := DataBytes / (DataBytes + MetaBytes)

    If no data were writen, then 0 is returned.
  }
    property Utilization: Double read GetUtilization;
  end;

  // shorter alias
  TTBDWriter = TTaggedBinaryDataWriter;

{===============================================================================
--------------------------------------------------------------------------------
                             TTaggedBinaryDataReader
--------------------------------------------------------------------------------
===============================================================================}
{
  Writing is not allowed in reader, a call to method Write (be it explicit or
  implicit) will raise an ETBDWriteError exception.

  Both seeking and reading are directly passed to source stream.

  To properly use the reader, call method GetTag once (and only once) before
  every data point. This method will try to load next stored tag and, if
  necessary, a new context information.

  When GetTag returns true, it indicates that a tag was read and properties
  CurrentContext and CurrentTag now contains proper values. Use those values
  to discern which data point to read next. Size of the data point is not
  managed by this library so you are responsible to read proper number of
  bytes (if you fail to do so, you will damage the reading process and further
  behavior of the reader is completely undefined).

  When it returns false, it indicates that either the end of source stream or
  end of tagged binary data stream pseudostructure (also indicated by property
  EndOfDataReached) was reached. In any case, you should stop reading any
  further data points. Also, in this situation, values stored in properties
  CurrentContext and CurrentTag are undefined.

  An example how to use the reader could be (note that it is reading the same
  data that would be stored in the example for writer - see above):

      Reader := TTaggedBinaryDataReader.Create(SourceStream);
      try
        while Reader.GetTag do
          case Reader.CurrentContext of
             0: case Reader.CurrentTag of
                  0:  <value_C0_T0> := Stream_GetInt16(Reader);
                  1:  <value_C0_T1> := Stream_GetFloat32(Reader);
                  2:  <value_C0_T2> := Stream_GetString(Reader);
                end;
             1: If Reader.CurrentTag = 0 then
                  <value_C1_T0> := Stream_GetAnsiChar(Reader);
            22: case Reader.CurrentTag of
                  100:  <value_C22_T100> := Stream_GetInt64(Reader);
                  200:  <value_C22_T200> := Stream_GetInt64(Reader);
                end;
          end;
      finally
        Reader.Free;
      end;

  This is just one possible approach, you can create your own implementation,
  for example using provided events fired on context and tag change...

      NOTE - OnTagChange* is called even if the actual tag does not change
             from last occurence. This is to account for situation where
             two or more datapoints of the same tag are stored together.
}
{===============================================================================
    TTaggedBinaryDataReader - class declaration
===============================================================================}
type
  TTaggedBinaryDataReader = class(TStream)
  protected
    fSource:                TStream;
    fEndOfDataReached:      Boolean;
    fCurrentContext:        TTBDContextID;
    fCurrentTag:            TTBDTag;
    fInitialContext:        Boolean;
    fMetaBytes:             Int64;
    fDataBytes:             Int64;
    fContextChangeEvent:    TNotifyEvent;
    fContextChangeCallback: TNotifyCallback;
    fTagChangeEvent:        TNotifyEvent;
    fTagChangeCallback:     TNotifyCallback;
    Function GetUtilization: Double; virtual;
    procedure Initialize(Source: TStream); virtual;
    procedure Finalize; virtual;
    procedure DoContextChange; virtual;
    procedure DoTagChange; virtual;
  public
    constructor Create(Source: TStream);
    destructor Destroy; override;
    Function Read(var Buffer; Count: LongInt): LongInt; override;
    Function Write(const Buffer; Count: LongInt): LongInt; override;
    Function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    Function GetTag: Boolean; virtual;
    property Source: TStream read fSource;
    property EndOfDataReached: Boolean read fEndOfDataReached;
    property CurrentContext: TTBDContextID read fCurrentContext;
    property CurrentTag: TTBDTag read fCurrentTag;
    property MetaBytes: Int64 read fMetaBytes;
    property DataBytes: Int64 read fDataBytes;
    property Utilization: Double read GetUtilization;
    property OnContextChangeEvent: TNotifyEvent read fContextChangeEvent write fContextChangeEvent;
    property OnContextChangeCallback: TNotifyCallback read fContextChangeCallback write fContextChangeCallback;
    property OnContextChange: TNotifyEvent read fContextChangeEvent write fContextChangeEvent;
    property OnTagChangeEvent: TNotifyEvent read fTagChangeEvent write fTagChangeEvent;
    property OnTagChangeCallback: TNotifyCallback read fTagChangeCallback write fTagChangeCallback;
    property OnTagChange: TNotifyEvent read fTagChangeEvent write fTagChangeEvent;
  end;

  TTBDReader = TTaggedBinaryDataReader;

implementation

uses
  BinaryStreamingLite;

{===============================================================================
    Auxiliary routines
===============================================================================}

Function ConsumeArgs(const Args: array of const): Integer;
begin
Result := Length(Args) * 0;
end;

{===============================================================================
--------------------------------------------------------------------------------
                             TTaggedBinaryDataWriter                                                                                               
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TTaggedBinaryDataWriter - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TTaggedBinaryDataWriter - protected methods
-------------------------------------------------------------------------------}

Function TTaggedBinaryDataWriter.GetUtilization: Double;
begin
If (fMetaBytes + fDataBytes) <> 0 then
  Result := fDataBytes / (fDataBytes + fMetaBytes)
else
  Result := 0.0;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.Initialize(Destination: TStream);
begin
If Assigned(Destination) then
  fDestination := Destination
else
  raise ETBDInvalidValue.Create('TTaggedBinaryDataWriter.Initialize: Destination stream not assigned.');
fDeferredActions := [daWriteSignature,daWriteContext,daWriteTag];
fPreviousContext := 0;
fCurrentContext := 0;
fCurrentTag := 0;
fWriteEmptyStream := False;
fMetaBytes := 0;
fDataBytes := 0;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.Finalize;
begin
If Assigned(fDestination) then
  WriteClose;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.WriteSignature;
begin
Stream_WriteUInt32(fDestination,TBD_SIGNATURE);
Exclude(fDeferredActions,daWriteSignature);
Inc(fMetaBytes,SizeOf(UInt32));
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.WriteContext;
begin
// write context only if it really changed
If fCurrentContext <> fPreviousContext then
  begin
    Stream_WriteUInt8(fDestination,TBD_TAG_CONTEXT);
    Stream_WriteUInt8(fDestination,0{flags, nothing implemented atm});
    Stream_WriteUInt16(fDestination,fCurrentContext);
    // make sure bare data are not written directly after the context
    Include(fDeferredActions,daWriteTag);
    Inc(fMetaBytes,SizeOf(TTBDTag) + SizeOf(TTBDContextFlags) + SizeOf(TTBDContextID));
  end;
Exclude(fDeferredActions,daWriteContext);
fPreviousContext := fCurrentContext;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.WriteTag;
begin
Stream_WriteUInt8(fDestination,fCurrentTag);
Exclude(fDeferredActions,daWriteTag);
Inc(fMetaBytes,SizeOf(TTBDTag));
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.WriteClose;
begin
If not(daWriteSignature in fDeferredActions){something was written} or fWriteEmptyStream then
  begin
    If daWriteSignature in fDeferredActions then
      WriteSignature;
    // closing tag
    Stream_WriteUInt8(fDestination,TBD_TAG_CONTEXT);
    // terminating context flags without context id
    Stream_WriteUInt8(fDestination,TBD_CTXFLAGS_FLAG_CLOSE);
    Inc(fMetaBytes,SizeOf(TTBDTag) + SizeOf(TTBDContextFlags));
  end;
end;

{-------------------------------------------------------------------------------
    TTaggedBinaryDataWriter - public methods
-------------------------------------------------------------------------------}

constructor TTaggedBinaryDataWriter.Create(Destination: TStream);
begin
inherited Create;
Initialize(Destination);
end;

//------------------------------------------------------------------------------

destructor TTaggedBinaryDataWriter.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataWriter.Read(var Buffer; Count: LongInt): LongInt;
begin
{$IFDEF FPC}Result := {$ENDIF}ConsumeArgs([@Buffer,Count]);
raise ETBDReadError.Create('TTaggedBinaryDataWriter.Read: Reading not allowed.');
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataWriter.Write(const Buffer; Count: LongInt): LongInt;
begin
If daWritesignature in fDeferredActions then
  WriteSignature;
If daWriteContext in fDeferredActions then
  WriteContext;
If daWriteTag in fDeferredActions then
  WriteTag;
Result := fDestination.Write(Buffer,Count);
Inc(fDataBytes,Int64(Result));
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataWriter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
Result := fDestination.Seek(Offset,Origin);
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.SetContext(Context: TTBDContextID);
begin
Include(fDeferredActions,daWriteContext);
fCurrentContext := Context;
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataWriter.SetTag(Tag: TTBDTag): TStream;
begin
If Tag <> TBD_TAG_CONTEXT then
  begin
    Include(fDeferredActions,daWriteTag);
    fCurrentTag := Tag;
    Result := Self;
  end
else raise ETBDInvalidValue.CreateFmt('TTaggedBinaryDataWriter.SetTag: Invalid tag (0x%.2x).',[Tag]);
end;


{===============================================================================
--------------------------------------------------------------------------------
                             TTaggedBinaryDataReader
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TTaggedBinaryDataReader - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TTaggedBinaryDataReader - protected methods
-------------------------------------------------------------------------------}

Function TTaggedBinaryDataReader.GetUtilization: Double;
begin
If (fMetaBytes + fDataBytes) <> 0 then
  Result := fDataBytes / (fDataBytes + fMetaBytes)
else
  Result := 0.0;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataReader.Initialize(Source: TStream);
var
  Signature:  UInt32;
begin
If Assigned(Source) then
  fSource := Source
else
  raise ETBDInvalidValue.Create('TTaggedBinaryDataReader.Initialize: Source stream not assigned.');
{
  Check if the source can contain a valid TBD stream, and if so whether it
  starts with a proper signature.
}
fMetaBytes := 0;  // must be here since it is used
If (fSource.Size - fSource.Position) >= 6 {4B signature, 2B closing sequence} then
  begin
    Signature := Stream_GetUInt32(fSource);
    fEndOfDataReached := Signature <> TBD_SIGNATURE;
    If fEndOfDataReached then
      fSource.Seek(-SizeOf(UInt32),soCurrent)
    else
      Inc(fMetaBytes,SizeOf(UInt32));
  end
else fEndOfDataReached := True;
fInitialContext := True;
// init other fields
fCurrentContext := 0;
fCurrentTag := 0;
fDataBytes := 0;
fContextChangeEvent := nil;
fContextChangeCallback := nil;
fTagChangeEvent := nil;
fTagChangeCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataReader.Finalize;
begin
fContextChangeEvent := nil;
fContextChangeCallback := nil;
fTagChangeEvent := nil;
fTagChangeCallback := nil;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataReader.DoContextChange;
begin
If Assigned(fContextChangeEvent) then
  fContextChangeEvent(Self)
else If Assigned(fContextChangeCallback) then
  fContextChangeCallback(Self);
fInitialContext := False;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataReader.DoTagChange;
begin
If Assigned(fTagChangeEvent) then
  fTagChangeEvent(Self)
else If Assigned(fTagChangeCallback) then
  fTagChangeCallback(Self);
end;

{-------------------------------------------------------------------------------
    TTaggedBinaryDataReader - public methods
-------------------------------------------------------------------------------}

constructor TTaggedBinaryDataReader.Create(Source: TStream);
begin
inherited Create;
Initialize(Source);
end;

//------------------------------------------------------------------------------

destructor TTaggedBinaryDataReader.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataReader.Read(var Buffer; Count: LongInt): LongInt;
begin
Result := fSource.Read(Buffer,Count);
Inc(fDataBytes,Int64(Result));
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataReader.Write(const Buffer; Count: LongInt): LongInt;
begin
{$IFDEF FPC}Result := {$ENDIF}ConsumeArgs([@Buffer,Count]);
raise ETBDWriteError.Create('TTaggedBinaryDataReader.Write: Writing not allowed.');
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataReader.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
Result := fSource.Seek(Offset,Origin);
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataReader.GetTag: Boolean;
begin
Result := False;
If not fEndOfDataReached then
  begin
    If (fSource.Size - fSource.Position) >= SizeOf(TTBDTag) then
      begin
        // tag can fit in the rest of the stream after current position
        fCurrentTag := Stream_GetUInt8(fSource);
        Inc(fMetaBytes,SizeOf(TTBDTag));
        If fCurrentTag = TBD_TAG_CONTEXT then
          begin
            If (fSource.Size - fSource.Position) >= SizeOf(TTBDContextFlags) then
              begin
                Inc(fMetaBytes,SizeOf(TTBDContextFlags));
                If (Stream_GetUInt8(fSource) and TBD_CTXFLAGS_FLAG_CLOSE) = 0 then
                  If (fSource.Size - fSource.Position) >= SizeOf(TTBDContextID) then
                    begin
                      fCurrentContext := Stream_GetUInt16(fSource);
                      Inc(fMetaBytes,SizeOf(TTBDContextID));
                      DoContextChange;
                    {
                      Recursively call GetTag again to read next thing after
                      the context change (whatever it will be).

                      Note the brackets must be there for FPC - otherwise
                      GetTag is parsed as a result of this function (Boolean),
                      not as a call to it.
                    }
                      Result := GetTag();
                      Exit;
                    end;
              end;
            fEndOfDataReached := True;
          end
        else
          begin
            // non-context tag read, signal context change if at the start of stream
            If fInitialContext then
              DoContextChange;
            DoTagChange;
            Result := True;
          end;
      end
    // tag cannot fit
    else fEndOfDataReached := True;
  end;
end;

end.
