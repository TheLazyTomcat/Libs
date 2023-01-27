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
    Context is a 16bit unsigned integer that can be set to any value (none is
    reserved). Since the tag has very limited range, contexts are here to allow
    more unique identification of data points.

    Unlike tags, which are stored before each data point, context is stored
    only when it changes. You should refrain from changing it too often, as
    each context change stores 4 more bytes into the resulting stream.

    General structure of taged binary data can be described as this:

          TaggedBinaryData
            Signature             - four byte signature ($54, $42, $44, $53)
            ContextGroup[]        - array of context groups
            ClosingSequence       - bytes $FF, $80 (see further for details)

      There can be no context group written, in which case the entire structure
      consists only of signature and closing sequence.

      The closing sequence consinsts of context tag ($FF), which marks a context
      change, followed by context flags without an actual new context ID. In the
      context flags, a close flag is set - this actually marks the end of tagged
      data (usually no other flag is set, so the value $80).

      Context group is a sequence of context tag, context flags, context ID and
      an array of tagged data points:

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

  Version 1.0.1 (2022-10-26)

  Last change 2023-01-26

  ©2022-2023 František Milt

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
    AuxTypes        - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses      - github.com/TheLazyTomcat/Lib.AuxClasses
    BinaryStreaming - github.com/TheLazyTomcat/Lib.BinaryStreaming
    StrRect         - github.com/TheLazyTomcat/Lib.StrRect
    BinaryStreaming - github.com/TheLazyTomcat/Lib.BinaryStreaming

===============================================================================}
unit TaggedBinaryData;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ETBDException = class(Exception);

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

  At the start, no context is written into destination, unless you explicitly
  set it (first, implicit, context has ID of 0) - first written thing after
  signature will be tag of the first data. If you do not set the tag before
  writing data, it will be 0.

  Writing of context and tag is deferred to next call of method Write. So no
  matter how many times you call SetContext and SetTag, nothing will be written
  into destination stream until you write some actual data, at which point only
  the last set context and tag will be written.

  It is possible to store compound data via multiple calls to write, only the
  first write after SetTag will actually write the tag.

  Even if you do not write any data, the signature and closing sequence will be
  written.

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

type
  TTBDWriterAction = (waWriteContext,waWriteTag);

  TTBDWriterActions = set of TTBDWriterAction;

{===============================================================================
    TTaggedBinaryDataWriter - class declaration
===============================================================================}
type
  TTaggedBinaryDataWriter = class(TStream)
  protected
    fDestination:     TStream;
    fActions:         TTBDWriterActions;
    fCurrentContext:  TTBDContextID;
    fCurrentTag:      TTBDTag;
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
    SetTag returns reference to self, so it can be used for inline tag set and
    write, for example:

      Writer.SetTag(15).WriteBuffer(Buff,SizeOf(Buff));

    Of course, if you hate such constructs, do not use it ;)
  }
    Function SetTag(Tag: TTBDTag): TStream; virtual;
    property Destination: TStream read fDestination;
    property CurrentContext: TTBDContextID read fCurrentContext;
    property CurrentTag: TTBDTag read fCurrentTag;
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
  managed by this library so you are responsible to read proper number of bytes
  (if you fail to do so, you will damage the reading process and further
  behavior of the reader is completely undefined).

  When it returns false, it indicates either end of source stream or end of
  tagged binary data stream pseudostructure (this is also indicated by property
  EndOfDataReached). In any case, you should stop reading any further data
  points. Also, in this situation, values stored in properties CurrentContext
  and CurrentTag are undefined.

  An example how to use the reader could be (note that it is reading the same
  data that would be stored in the example for writer - see above):

      Reader := TTaggedBinaryDataReader.Create(SourceStream);
      try
        while Reader.GetTag do
          case Reader.CurrentContext of
             0: case Reader.CurrentTag of
                  0:  <value_C0_T0> := Stream_ReadInt16(Reader);
                  1:  <value_C0_T1> := Stream_ReadFloat32(Reader);
                  2:  <value_C0_T2> := Stream_ReadString(Reader);
                end;
             1: If Reader.CurrentTag = 0 then
                  <value_C1_T0> := Stream_ReadAnsiChar(Reader);
            22: case Reader.CurrentTag of
                  100:  <value_C22_T100> := Stream_ReadInt64(Reader);
                  200:  <value_C22_T200> := Stream_ReadInt64(Reader);
                end;
          end;
      finally
        Reader.Free;
      end;

    This is just one possible approach, you can create your own implementation
    (for example using provided events fired on context and tag change).
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
    fIsDefaultContext:      Boolean;
    fContextChangeEvent:    TNotifyEvent;
    fContextChangeCallback: TNotifyCallback;
    fTagChangeEvent:        TNotifyEvent;
    fTagChangeCallback:     TNotifyCallback;
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
  BinaryStreaming;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

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

procedure TTaggedBinaryDataWriter.Initialize(Destination: TStream);
begin
If Assigned(Destination) then
  fDestination := Destination
else
  raise ETBDInvalidValue.Create('TTaggedBinaryDataWriter.Initialize: Destination stream not assigned.');
fActions := [waWriteTag];
fCurrentContext := 0;
fCurrentTag := 0;
WriteSignature;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.Finalize;
begin
If Assigned(fDestination) then
  WriteClose;
fActions := [];
fDestination := nil;
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.WriteSignature;
begin
Stream_WriteUInt32(fDestination,TBD_SIGNATURE);
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.WriteContext;

  Function GetContextFlags: TTBDContextFlags;
  begin
    Result := 0;
  end;

begin
Stream_WriteUInt8(fDestination,TBD_TAG_CONTEXT);
Stream_WriteUInt8(fDestination,GetContextFlags);
Stream_WriteUInt16(fDestination,fCurrentContext);
Exclude(fActions,waWriteContext);
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.WriteTag;
begin
Stream_WriteUInt8(fDestination,fCurrentTag);
Exclude(fActions,waWriteTag);
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.WriteClose;
begin
// write closing tag
Stream_WriteUInt8(fDestination,TBD_TAG_CONTEXT);
// write terminating context flags without context id
Stream_WriteUInt8(fDestination,TBD_CTXFLAGS_FLAG_CLOSE);
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

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TTaggedBinaryDataWriter.Read(var Buffer; Count: LongInt): LongInt;
begin
{$IFDEF FPC}
Result := 0;
{$ENDIF}
raise ETBDReadError.Create('TTaggedBinaryDataWriter.Read: Reading not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TTaggedBinaryDataWriter.Write(const Buffer; Count: LongInt): LongInt;
begin
If waWriteContext in fActions then
  WriteContext;
If waWriteTag in fActions then
  WriteTag;
Result := fDestination.Write(Buffer,Count);
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataWriter.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
Result := fDestination.Seek(Offset,Origin);
end;

//------------------------------------------------------------------------------

procedure TTaggedBinaryDataWriter.SetContext(Context: TTBDContextID);
begin
Include(fActions,waWriteContext);
fCurrentContext := Context;
end;

//------------------------------------------------------------------------------

Function TTaggedBinaryDataWriter.SetTag(Tag: TTBDTag): TStream;
begin
If Tag <> TBD_TAG_CONTEXT then
  begin
    Include(fActions,waWriteTag);
    fCurrentTag := Tag;
    Result := Self;
  end
else raise ETBDInvalidValue.CreateFmt('TTaggedBinaryDataWriter.SetNewTag: Invalid tag (0x%.2x).',[Tag]);
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

procedure TTaggedBinaryDataReader.Initialize(Source: TStream);
var
  temp: uint32;
begin
If Assigned(Source) then
  fSource := Source
else
  raise ETBDInvalidValue.Create('TTaggedBinaryDataReader.Initialize: Source stream not assigned.');
{
  Check if the source can contain a valid TBD stream, and if so whether it
  starts with a proper signature.
}
If (fSource.Size - fSource.Position) >= 6 {4B signature, 2B closing sequence} then
  begin
    Temp := Stream_ReadUInt32(fSource);
    fEndOfDataReached := Temp <> TBD_SIGNATURE;
    If fEndOfDataReached then
      fSource.Seek(-SizeOf(UInt32),soCurrent);
  end
else fEndOfDataReached := False;
fIsDefaultContext := True;
// init other fields
fCurrentContext := 0;
fCurrentTag := 0;
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
fIsDefaultContext := False;
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
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TTaggedBinaryDataReader.Write(const Buffer; Count: LongInt): LongInt;
begin
{$IFDEF FPC}
Result := 0;
{$ENDIF}
raise ETBDWriteError.Create('TTaggedBinaryDataReader.Write: Writing not allowed.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

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
        fCurrentTag := Stream_ReadUInt8(fSource);
        If fCurrentTag = TBD_TAG_CONTEXT then
          begin
            If (fSource.Size - fSource.Position) >= SizeOf(TTBDContextFlags) then
              If (Stream_ReadUInt8(fSource) and TBD_CTXFLAGS_FLAG_CLOSE) = 0 then
                If (fSource.Size - fSource.Position) >= SizeOf(TTBDContextID) then
                  begin
                    fCurrentContext := Stream_ReadUInt16(fSource);
                    DoContextChange;
                  {
                    Recursively call GetTag again to read next thing after the
                    context change (whatever it will be).

                    Note the brackets must be there for FPC - otherwise GetTag
                    is parsed as a result of this function (Boolean), not as a
                    call to it.
                  }
                    Result := GetTag();
                    Exit;
                  end;
            fEndOfDataReached := True;
          end
        else
          begin
            // non-context tag read
            If fIsDefaultContext then
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
