{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  ZLibUtils

    Utility classes for data (de)compression build on zlib library.

  Version 1.0.8 (2021-03-22)

  Last change 2022-09-24

  ©2018-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.ZLibUtils

  Dependencies:
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
    MemoryBuffer   - github.com/TheLazyTomcat/Lib.MemoryBuffer
    ZLib           - github.com/TheLazyTomcat/Bnd.ZLib
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
    DynLibUtils    - github.com/TheLazyTomcat/Lib.DynLibUtils
    SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID
  * WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion

  Library WindowsVersion is only needed when compiling for Windows OS.

===============================================================================}
unit ZLibUtils;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH ClassicProcVars+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

{
  ZLib_Static

  When defined, a statically linked zlib is used (unit ZLibStatic). When not,
  a dynamically linked zlib (a DLL) is used (unit ZLibDynamic) - note that zlib
  library initialization and finalization is done automatically.
  
  Defined by default.

  To disable/undefine this symbol in a project without changing this library,
  define project-wide symbol ZLibUtils_ZLib_Static_Off.
}
{$DEFINE ZLib_Static}
{$IFDEF ZLibUtils_ZLib_Static_Off}
  {$UNDEF ZLib_Static}
{$ENDIF}        

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses, MemoryBuffer,
  ZLibCommon;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W3031:={$WARN 3031 OFF}} // Values in enumeration types have to be ascending
{$ENDIF}

{===============================================================================
    Types, constants and auxiliary classes
===============================================================================}
type
  TZCompressionLevel = (
    zclNoCompression   = Z_NO_COMPRESSION,
    zclBestSpeed       = Z_BEST_SPEED,
    zclBestCompression = Z_BEST_COMPRESSION,
  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    zclDefault         = Z_DEFAULT_COMPRESSION,
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    zclLevel0          = 0,
    zclLevel1          = 1,
    zclLevel2          = 2,
    zclLevel3          = 3,
    zclLevel4          = 4,
    zclLevel5          = 5,
    zclLevel6          = 6,
    zclLevel7          = 7,
    zclLevel8          = 8,
    zclLevel9          = 9);

  TZMemLevel = (
    zmlDefault = DEF_MEM_LEVEL,
  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    zmlLevel1  = 1,
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    zmlLevel2  = 2,
    zmlLevel3  = 3,
    zmlLevel4  = 4,
    zmlLevel5  = 5,
    zmlLevel6  = 6,
    zmlLevel7  = 7,
    zmlLevel8  = 8,
    zmlLevel9  = 9);

  TZStrategy = (
    zsFiltered = Z_FILTERED,
    zsHuffman  = Z_HUFFMAN_ONLY,
    zsRLE      = Z_RLE,
    zsFixed    = Z_FIXED,
  {$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
    zsDefault  = Z_DEFAULT_STRATEGY);
  {$IFDEF FPCDWM}{$POP}{$ENDIF}

{$IFDEF FPCDWM}{$PUSH}W3031{$ENDIF}
  TZStreamType = (zstZLib,zstGZip,zstRaw,zstDefault = zstZLib);
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

type
  EZError = class(Exception)
  protected
    fErrorCode: int;
  public
    constructor ZCreate(ErrCode: int; ZStream: z_stream);
    property ErrorCode: int read fErrorCode;
  end;

  EZCompressionError   = class(EZError);
  EZDecompressionError = class(EZError);

{-------------------------------------------------------------------------------
================================================================================
                                  TZProcessor
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZProcessor - class declaration
===============================================================================}
type
  TZProcessor = class(TCustomObject)
  protected
    fZLibState:         z_stream;
    fOutBuffer:         TMemoryBuffer;
    fTotalCompressed:   UInt64;
    fTotalUncompressed: UInt64;
    fOnOutputEvent:     TBufferEvent;
    fOnOutputCallback:  TBufferCallback;
    Function GetCompressionRatio: Double; // compressed / uncompressed
    procedure DoOutput(OutSize: TMemSize); virtual;
  public
    procedure Init; virtual;
    Function Update(const Data; Size: uInt): uInt; virtual; abstract;
    procedure Final; virtual;
    property TotalCompressed: UInt64 read fTotalCompressed;
    property TotalUncompressed: UInt64 read fTotalUncompressed;
    property CompressionRatio: Double read GetCompressionRatio;
    property OnOutput: TBufferEvent read fOnOutputEvent write fOnOutputEvent;
    property OnOutputEvent: TBufferEvent read fOnOutputEvent write fOnOutputEvent;
    property OnOutputCallback: TBufferCallback read fOnOutputCallback write fOnOutputCallback;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                  TZCompressor
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCompressor - class declaration
===============================================================================}
type
  TZCompressor = class(TZProcessor)
  protected
    fCompressionLevel:  TZCompressionLevel;
    fMemLevel:          TZMemLevel;
    fStrategy:          TZStrategy;
    fWindowBits:        int;
  public
    constructor Create(CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int); overload;
    constructor Create(CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType); overload;
    constructor Create(CompressionLevel: TZCompressionLevel; WindowBits: int); overload;
    constructor Create(CompressionLevel: TZCompressionLevel; StreamType: TZStreamType); overload;
    constructor Create(CompressionLevel: TZCompressionLevel = zclDefault); overload;
    procedure Init; override;
    Function Update(const Data; Size: uInt): uInt; override;
    procedure Final; override;
    property CompressionLevel: TZCompressionLevel read fCompressionLevel;
    property MemLevel: TZMemLevel read fMemLevel;
    property Strategy: TZStrategy read fStrategy;
    property WindowBits: int read fWindowBits;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TZDecompressor
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZDecompressor - class declaration
===============================================================================}
type
  TZDecompressor = class(TZProcessor)
  protected
    fWindowBits:  int;
  public
    constructor Create(WindowBits: int); overload;
    constructor Create(StreamType: TZStreamType = zstDefault); overload;
    procedure Init; override;
    Function Update(const Data; Size: uInt): uInt; override;
    procedure Final; override;
    property WindowBits: int read fWindowBits write fWindowBits;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TZCustomStream
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCustomStream - class declaration
===============================================================================}
type
  TZCustomStream = class(TStream)
  protected
    fZLibState:           z_stream;
    fBuffer:              TMemoryBuffer;
    fTotalCompressed:     UInt64;
    fTotalUncompressed:   UInt64;
    fTotalCounter:        UInt64;
    fUserIntData:         PtrInt;
    fUserPtrData:         Pointer;
    fOnUpdateEvent:       TNotifyEvent;
    fOnUpdateCallback:    TNotifyCallback;
    fOnProgressEvent:     TFloatEvent;
    fOnProgressCallback:  TFloatCallback;
    Function GetCompressionRatio: Double;
    procedure DoUpdate; virtual;
    procedure DoProgress(Progress: Double); overload; virtual;
    procedure DoProgress; overload; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Final; virtual; abstract;
    property TotalCompressed: UInt64 read fTotalCompressed;
    property TotalUncompressed: UInt64 read fTotalUncompressed;
    property CompressionRatio: Double read GetCompressionRatio;
    property UserIntData: PtrInt read fUserIntData write fUserIntData;
    property UserPtrData: Pointer read fUserPtrData write fUserPtrData;
    property UserData: PtrInt read fUserIntData write fUserIntData;    
  {
    OnUpdate* is called everytime the TotalCompressed (decompression stream) or
    TotalUncompressed (compression stream) is changed.
  }
    property OnUpdate: TNotifyEvent read fOnUpdateEvent write fOnUpdateEvent;
    property OnUpdateEvent: TNotifyEvent read fOnUpdateEvent write fOnUpdateEvent;
    property OnUpdateCallback: TNotifyCallback read fOnUpdateCallback write fOnUpdateCallback;
  {
    OnProgress* is called only when processing streams en-block, that is in
    methods TZCompressionStream.CompressFrom and TZDecompressionStream.ExtractTo.

    Note that TZDecompressionStream.ExtractTo can report wrong progress if the
    source stream contains data beyond the end of compressed data.
  }
    property OnProgress: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
    property OnProgressEvent: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
    property OnProgressCallback: TFloatCallback read fOnProgressCallback write fOnProgressCallback;
  end;

{-------------------------------------------------------------------------------
================================================================================
                              TZCompressionStream
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCompressionStream - class declaration
===============================================================================}
type
  TZCompressionStream = class(TZCustomStream)
  protected
    fCompressionLevel:  TZCompressionLevel;
    fMemLevel:          TZMemLevel;
    fStrategy:          TZStrategy;
    fWindowBits:        int;
    fDestination:       TStream;
    procedure DoProgress; overload; override;
  public
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int); overload;
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType); overload;
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel; WindowBits: int); overload;
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel; StreamType: TZStreamType); overload;
    constructor Create(Dest: TStream; CompressionLevel: TZCompressionLevel = zclDefault); overload;
    destructor Destroy; override;
    Function Read(var Buffer; Count: LongInt): LongInt; override;
    Function Write(const Buffer; Count: LongInt): LongInt; override;
    Function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    Function CompressFrom(Source: TStream): Int64; virtual;
    procedure Final; override;
    property CompressionLevel: TZCompressionLevel read fCompressionLevel;
    property MemLevel: TZMemLevel read fMemLevel;
    property Strategy: TZStrategy read fStrategy;
    property WindowBits: int read fWindowBits;
    property Destination: TStream read fDestination;
  end;

{-------------------------------------------------------------------------------
================================================================================
                             TZDecompressionStream
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZDecompressionStream - class declaration
===============================================================================}
type
  TZDecompressionStream = class(TZCustomStream)
  protected
    fWindowBits:  int;
    fSource:      TStream;
    procedure DoProgress; overload; override;
  public
    constructor Create(Src: TStream; WindowBits: int); overload;
    constructor Create(Src: TStream; StreamType: TZStreamType = zstDefault); overload;
    destructor Destroy; override;
    Function Read(var Buffer; Count: LongInt): LongInt; override;
    Function Write(const Buffer; Count: LongInt): LongInt; override;
    Function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    Function DecompressTo(Destination: TStream): Int64; virtual;
    Function ExtractTo(Destination: TStream): Int64; virtual;
    procedure Final; override;
    property WindowBits: int read fWindowBits;
  end;

{-------------------------------------------------------------------------------
================================================================================
                                 TZCustomBuffer
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCustomBuffer - class declaration
===============================================================================}
type
  TZCustomBuffer = class(TCustomObject)
  protected
    fSource:              TMemoryBuffer;
    fBuffer:              TMemoryBuffer;
    fResult:              TMemoryBuffer;
    fFreeResult:          Boolean;
    fTotalCompressed:     UInt64;
    fTotalUncompressed:   UInt64;
    fExpectedResultSize:  TMemSize;
    fOnProgressEvent:     TFloatEvent;
    fOnProgressCallback:  TFloatCallback;
    Function GetCompressionRatio: Double;
    procedure ProcessorHandler(Sender: TObject; const Data; Size: TMemSize); virtual; abstract;
    procedure DoProgress; virtual; abstract;
    procedure Init; virtual; abstract;
    Function Update: Boolean; virtual; abstract;
    procedure Final; virtual; abstract;
  public
    constructor Create(Src: TMemoryBuffer);
    destructor Destroy; override;
    procedure Process; virtual;
    property Source: TMemoryBuffer read fSource;
    property Result: TMemoryBuffer read fResult;
    property FreeResult: Boolean read fFreeResult write fFreeResult;
    property TotalCompressed: UInt64 read fTotalCompressed;
    property TotalUncompressed: UInt64 read fTotalUncompressed;
    property CompressionRatio: Double read GetCompressionRatio;
    property ExpectedResultSize: TMemSize read fExpectedResultSize write fExpectedResultSize;
    property OnProgress: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
    property OnProgressEvent: TFloatEvent read fOnProgressEvent write fOnProgressEvent;
    property OnProgressCallback: TFloatCallback read fOnProgressCallback write fOnProgressCallback;
  end;

{-------------------------------------------------------------------------------
================================================================================
                              TZCompressionBuffer
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCompressionBuffer - class declaration
===============================================================================}
type
  TZCompressionBuffer = class(TZCustomBuffer)
  protected
    fCompressor:        TZCompressor;
    fCompressionLevel:  TZCompressionLevel;
    fMemLevel:          TZMemLevel;
    fStrategy:          TZStrategy;
    fWindowBits:        int;
    procedure ProcessorHandler(Sender: TObject; const Data; Size: TMemSize); override;
    procedure DoProgress; override;
    procedure Init; override;
    Function Update: Boolean; override;
    procedure Final; override;
  public
    constructor Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int); overload;
    constructor Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType); overload;
    constructor Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel; WindowBits: int); overload;
    constructor Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel; StreamType: TZStreamType); overload;
    constructor Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel = zclDefault); overload;
    constructor Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int); overload;
    constructor Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType); overload;
    constructor Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel; WindowBits: int); overload;
    constructor Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel; StreamType: TZStreamType); overload;
    constructor Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel = zclDefault); overload;
    property CompressionLevel: TZCompressionLevel read fCompressionLevel;
    property MemLevel: TZMemLevel read fMemLevel;
    property Strategy: TZStrategy read fStrategy;
    property WindowBits: int read fWindowBits;
  end;

{-------------------------------------------------------------------------------
================================================================================
                             TZDecompressionBuffer
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZDecompressionBuffer - class declaration
===============================================================================}
type
  TZDecompressionBuffer = class(TZCustomBuffer)
  protected
    fDecompressor:  TZDecompressor;
    fWindowBits:    int;
    procedure ProcessorHandler(Sender: TObject; const Data; Size: TMemSize); override;
    procedure DoProgress; override;
    procedure Init; override;
    Function Update: Boolean; override;
    procedure Final; override;
  public
    constructor Create(Src: TMemoryBuffer; WindowBits: int); overload;
    constructor Create(Src: TMemoryBuffer; StreamType: TZStreamType = zstDefault); overload;
    constructor Create(Src: Pointer; SrcSize: TMemSize; WindowBits: int); overload;
    constructor Create(Src: Pointer; SrcSize: TMemSize; StreamType: TZStreamType = zstDefault); overload;
    property WindowBits: int read fWindowBits;    
  end;

implementation

uses
  Math,
{$IFDEF ZLib_Static}
  ZLibStatic;
{$ELSE}
  ZLibDynamic;
{$ENDIF}

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

const
  ZInvalidOp = 'Invalid operation.';

  ZU_PROC_BUFFSIZE = 1024 * 1024;  {1MiB}
  ZU_STRM_BUFFSIZE = 1024 * 1024;  {1MiB}
  ZU_BUFF_BUFFSIZE = 1024 * 1024;  {1MiB}
  ZU_INTR_BUFFSIZE = 1024 * 1024;  {1MiB}

{===============================================================================
    Auxiliary functions
===============================================================================}

Function CompressionErrCheck(ErrCode: int; State: z_stream; RaiseDictionaryError: Boolean = True): int;
begin
Result := ErrCode;
If (ErrCode < 0) or ((ErrCode = Z_NEED_DICT) and RaiseDictionaryError) then
  raise EZCompressionError.ZCreate(ErrCode,State)
end;

//------------------------------------------------------------------------------

Function DecompressionErrCheck(ErrCode: int; State: z_stream; RaiseDictionaryError: Boolean = True): int;
begin
Result := ErrCode;
If (ErrCode < 0) or ((ErrCode = Z_NEED_DICT) and RaiseDictionaryError) then
  raise EZDecompressionError.ZCreate(ErrCode,State);
end;

//------------------------------------------------------------------------------

Function StreamTypeToWBits(StreamType: TZStreamType): int;
begin
case StreamType of
  zstZLib:  Result := WBITS_ZLIB;
  zstGZip:  Result := WBITS_GZIP;
  zstRaw:   Result := WBITS_RAW;
else
  raise EZError.CreateFmt('StreamTypeToWBits: Unknown stream type (%d).',[Ord(StreamType)]);
end;
end;

{===============================================================================
    Auxiliary classes - implementation
===============================================================================}

constructor EZError.ZCreate(ErrCode: int; ZStream: z_stream);
begin
If Assigned(ZStream.msg) then
  CreateFmt('%s (%s)',[zError(ErrCode),ZStream.msg])
else
  CreateFmt('%s',[zError(ErrCode)]);
fErrorCode := ErrCode;
end;

{-------------------------------------------------------------------------------
================================================================================
                                  TZProcessor
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZProcessor - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TZProcessor - protected methods
-------------------------------------------------------------------------------}

Function TZProcessor.GetCompressionRatio: Double;
begin
If fTotalUncompressed <> 0 then
  Result := fTotalCompressed / fTotalUncompressed
else
  Result := 0.0;
end;

//------------------------------------------------------------------------------

procedure TZProcessor.DoOutput(OutSize: TMemSize);
begin
If OutSize > 0 then
  begin
    If Assigned(fOnOutputEvent) then
      fOnOutputEvent(Self,BufferMemory(fOutBuffer)^,OutSize)
    else If Assigned(fOnOutputCallback) then
      fOnOutputCallback(Self,BufferMemory(fOutBuffer)^,OutSize);
  end;
end;

{-------------------------------------------------------------------------------
    TZProcessor - public methods
-------------------------------------------------------------------------------}

procedure TZProcessor.Init;
begin
FillChar(fZLibState,SizeOf(fZLibState),0);
BufferGet(fOutBuffer,ZU_PROC_BUFFSIZE);
fTotalCompressed := 0;
fTotalUncompressed := 0;
end;

//------------------------------------------------------------------------------

procedure TZProcessor.Final;
begin
BufferFree(fOutBuffer);
end;


{-------------------------------------------------------------------------------
================================================================================
                                  TZCompressor
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCompressor - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TZCompressor - public methods
-------------------------------------------------------------------------------}

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int);
begin
inherited Create;
fCompressionLevel := CompressionLevel;
fMemLevel := MemLevel;
fStrategy := Strategy;
fWindowBits := WindowBits;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType);
begin
Create(CompressionLevel,MemLevel,Strategy,StreamTypeToWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel; WindowBits: int);
begin
Create(CompressionLevel,zmlDefault,zsDefault,WindowBits);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel; StreamType: TZStreamType);
begin
Create(CompressionLevel,zmlDefault,zsDefault,StreamTypeToWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressor.Create(CompressionLevel: TZCompressionLevel = zclDefault);
begin
Create(CompressionLevel,zmlDefault,zsDefault,zstDefault);
end;

//------------------------------------------------------------------------------

procedure TZCompressor.Init;
begin
inherited;
CompressionErrCheck(deflateInit2(@fZLibState,Ord(fCompressionLevel),Z_DEFLATED,fWindowBits,Ord(fMemLevel),Ord(fStrategy)),fZLibState);
end;

//------------------------------------------------------------------------------

Function TZCompressor.Update(const Data; Size: uInt): uInt;
var
  OutSize:  TMemSize;
begin
If Size > 0 then
  begin
    fZLibState.next_in := @Data;
    fZLibState.avail_in := Size;
    repeat
      fZLibState.next_out := BufferMemory(fOutBuffer);
      fZLibState.avail_out := uInt(BufferSize(fOutBuffer));
      CompressionErrCheck(deflate(@fZLibState,Z_NO_FLUSH),fZLibState);
      OutSize := BufferSize(fOutBuffer) - TMemSize(fZLibState.avail_out);
      Inc(fTotalCompressed,UInt64(OutSize));
      DoOutput(OutSize);
    until fZLibState.avail_in <= 0;
    Inc(fTotalUncompressed,UInt64(Size));
    Result := Size;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

procedure TZCompressor.Final;
var
  ResultCode: int;
  OutSize:    TMemSize;
begin
try
  // flush what is left in zlib internal state
  fZLibState.next_in := nil;
  fZLibState.avail_in := 0;
  repeat
    fZLibState.next_out := BufferMemory(fOutBuffer);
    fZLibState.avail_out := uInt(BufferSize(fOutBuffer));
    ResultCode := CompressionErrCheck(deflate(@fZLibState,Z_FINISH),fZLibState);
    OutSize := BufferSize(fOutBuffer) - TMemSize(fZLibState.avail_out);
    Inc(fTotalCompressed,UInt64(OutSize));
    DoOutput(OutSize);
  until ResultCode = Z_STREAM_END;
finally
  CompressionErrCheck(deflateEnd(@fZLibState),fZLibState);
end;
inherited;
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TZDecompressor
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZDecompressor - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TZDecompressor - public methods
-------------------------------------------------------------------------------}

constructor TZDecompressor.Create(WindowBits: int);
begin
inherited Create;
fWindowBits := WindowBits;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZDecompressor.Create(StreamType: TZStreamType = zstDefault);
begin
Create(StreamTypeToWBits(StreamType));
end;

//------------------------------------------------------------------------------

procedure TZDecompressor.Init;
begin
inherited;
DecompressionErrCheck(inflateInit2(@fZLibState,fWindowbits),fZLibState,True);
end;

//------------------------------------------------------------------------------

Function TZDecompressor.Update(const Data; Size: uInt): uInt;
var
  ResultCode: int;
  OutSize:    TMemSize;
begin
If Size > 0 then
  begin
    fZLibState.next_in := @Data;
    fZLibState.avail_in := Size;
    repeat
      fZLibState.next_out := BufferMemory(fOutBuffer);
      fZLibState.avail_out := uInt(BufferSize(fOutBuffer));
      ResultCode := DecompressionErrCheck(inflate(@fZLibState,Z_NO_FLUSH),fZLibState,True);
      OutSize := BufferSize(fOutBuffer) - TMemSize(fZLibState.avail_out);
      Inc(fTotalUncompressed,UInt64(OutSize));
      DoOutput(OutSize);
    until (ResultCode = Z_STREAM_END) or (fZLibState.avail_in <= 0);
    Inc(fTotalCompressed,UInt64(Size - fZLibState.avail_in));
    Result := Size - fZLibState.avail_in;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

procedure TZDecompressor.Final;
begin
DecompressionErrCheck(inflateEnd(@fZLibState),fZLibState,True);
inherited;
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TZCustomStream
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCustomStream - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TZCustomStream - protected methods
-------------------------------------------------------------------------------}

Function TZCustomStream.GetCompressionRatio: Double;
begin
If fTotalUncompressed <> 0 then
  Result := fTotalCompressed / fTotalUncompressed
else
  Result := 0.0;
end;

//------------------------------------------------------------------------------

procedure TZCustomStream.DoUpdate;
begin
If Assigned(fOnUpdateEvent) then
  fOnUpdateEvent(Self)
else If Assigned(fOnUpdateCallback) then
  fOnUpdateCallback(Self);
end;

//------------------------------------------------------------------------------

procedure TZCustomStream.DoProgress(Progress: Double);
begin
If Assigned(fOnProgressEvent) then
  fOnProgressEvent(Self,Progress)
else If Assigned(fOnProgressCallback) then
  fOnProgressCallback(Self,Progress);
end;

{-------------------------------------------------------------------------------
    TZCustomStream - public methods
-------------------------------------------------------------------------------}

constructor TZCustomStream.Create;
begin
inherited;
FillChar(fZLibState,SizeOf(fZLibState),0);
BufferGet(fBuffer,ZU_STRM_BUFFSIZE);
fTotalCompressed := 0;
fTotalUncompressed := 0;
end;

//------------------------------------------------------------------------------

destructor TZCustomStream.Destroy;
begin
BufferFree(fBuffer);
inherited;
end;


{-------------------------------------------------------------------------------
================================================================================
                              TZCompressionStream
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCompressionStream - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TZCompressionStream - protected methods
-------------------------------------------------------------------------------}

procedure TZCompressionStream.DoProgress;
begin
If fTotalCounter <> 0 then
  DoProgress(fTotalUncompressed / fTotalCounter)
else
  DoProgress(0.0);
end;

{-------------------------------------------------------------------------------
    TZCompressionStream - public methods
-------------------------------------------------------------------------------}

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int);
begin
inherited Create;
fCompressionLevel := CompressionLevel;
fMemLevel := MemLevel;
fStrategy := Strategy;
fWindowBits := WindowBits;
fDestination := Dest;
CompressionErrCheck(deflateInit2(@fZLibState,Ord(fCompressionLevel),Z_DEFLATED,fWindowBits,Ord(fMemLevel),Ord(fStrategy)),fZLibState);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType);
begin
Create(Dest,CompressionLevel,MemLevel,Strategy,StreamTypeToWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel; WindowBits: int);
begin
Create(Dest,CompressionLevel,zmlDefault,zsDefault,WindowBits);
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel; StreamType: TZStreamType);
begin
Create(Dest,CompressionLevel,zmlDefault,zsDefault,StreamTypeToWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionStream.Create(Dest: TStream; CompressionLevel: TZCompressionLevel = zclDefault);
begin
Create(Dest,CompressionLevel,zmlDefault,zsDefault,zstDefault);
end;

//------------------------------------------------------------------------------

destructor TZCompressionStream.Destroy;
begin
try
  Final;
finally
  CompressionErrCheck(deflateEnd(@fZLibState),fZLibState);
end;
inherited;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TZCompressionStream.Read(var Buffer; Count: LongInt): LongInt;
begin
{$IFDEF FPC}
Result := 0;
{$ENDIf}
raise EZCompressionError.Create('TZCompressionStream.Read: ' + ZInvalidOp);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TZCompressionStream.Write(const Buffer; Count: LongInt): LongInt;
var
  OutSize:  TMemSize;
begin
If Count > 0 then
  begin
    fZLibState.next_in := @Buffer;
    fZLibState.avail_in := Count;
    repeat
      fZLibState.next_out := BufferMemory(fBuffer);
      fZLibState.avail_out := uInt(BufferSize(fBuffer));
      CompressionErrCheck(deflate(@fZLibState,Z_NO_FLUSH),fZLibState);
      OutSize := BufferSize(fBuffer) - TMemSize(fZLibState.avail_out);
      Inc(fTotalCompressed,UInt64(OutSize));
      fDestination.WriteBuffer(BufferMemory(fBuffer)^,LongInt(OutSize));
      DoUpdate;
    until fZLibState.avail_in <= 0;
    Inc(fTotalUncompressed,UInt64(Count));
    Result := Count;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

Function TZCompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
{
  Since the source of uncompressed data is not controlled by this object, and
  seeking is done on uncompressed data, an arbitrary seek is pretty much not
  possible.
  Only one combination is possible - we know how much uncompressed data was
  written, and that is the current position, so seek into it is possible and
  implemented.
}
If (Origin = soCurrent) and (Offset = 0) then
  Result := Int64(fTotalUncompressed)
else
  raise EZCompressionError.Create('TZCompressionStream.Seek: ' + ZInvalidOp);
end;

//------------------------------------------------------------------------------

Function TZCompressionStream.CompressFrom(Source: TStream): Int64;
var
  Buffer:     TMemoryBuffer;
  BytesRead:  LongInt;
begin
Result := 0;
fTotalCounter := UInt64(Source.Size);
DoProgress(0.0);
BufferInit(Buffer);
BufferGet(Buffer,ZU_INTR_BUFFSIZE);
try
  repeat
    BytesRead := Source.Read(BufferMemory(Buffer)^,ZU_INTR_BUFFSIZE);
    WriteBuffer(BufferMemory(Buffer)^,BytesRead);
    Inc(Result,Int64(BytesRead));
    DoProgress;
  until BytesRead < ZU_INTR_BUFFSIZE;
finally
  Final;
  BufferFree(Buffer);
  DoProgress(1.0);
end;
end;

//------------------------------------------------------------------------------

procedure TZCompressionStream.Final;
var
  ResultCode: int;
  OutSize:    TMemSize;
begin
fZLibState.next_in := nil;
fZLibState.avail_in := 0;
repeat
  fZLibState.next_out := BufferMemory(fBuffer);
  fZLibState.avail_out := uInt(BufferSize(fBuffer));
  ResultCode := CompressionErrCheck(deflate(@fZLibState,Z_FINISH),fZLibState);
  OutSize := BufferSize(fBuffer) - TMemSize(fZLibState.avail_out);
  Inc(fTotalCompressed,UInt64(OutSize));
  fDestination.WriteBuffer(BufferMemory(fBuffer)^,LongInt(OutSize));
until ResultCode = Z_STREAM_END;
end;


{-------------------------------------------------------------------------------
================================================================================
                             TZDecompressionStream
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZDecompressionStream - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TZDecompressionStream - protected methods
-------------------------------------------------------------------------------}

procedure TZDecompressionStream.DoProgress;
begin
If fTotalCounter <> 0 then
  DoProgress(fTotalCompressed / fTotalCounter)
else
  DoProgress(0.0);
end;

{-------------------------------------------------------------------------------
    TZDecompressionStream - public methods
-------------------------------------------------------------------------------}

constructor TZDecompressionStream.Create(Src: TStream; WindowBits: int);
begin
inherited Create;
fWindowBits := WindowBits;
fSource := Src;
DecompressionErrCheck(inflateInit2(@fZLibState,fWindowBits),fZLibState);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZDecompressionStream.Create(Src: TStream; StreamType: TZStreamType = zstDefault);
begin
Create(Src,StreamTypeToWBits(StreamType));
end;

//------------------------------------------------------------------------------

destructor TZDecompressionStream.Destroy;
begin
try
  Final;
finally
  DecompressionErrCheck(inflateEnd(@fZLibState),fZLibState);
end;
inherited;
end;

//------------------------------------------------------------------------------

Function TZDecompressionStream.Read(var Buffer; Count: LongInt): LongInt;
var
  ReadBytes:    LongInt;
  TransferOff:  PtrUInt;
  ResultCode:   int;
begin
{
  Entire in-buffer might not be consumed in one call. TransferOff pretty much
  stores how many bytes were consumed and the buffer is not fillet again, only
  in-pointer is advanced by TransferOff bytes.
}
If Count > 0 then
  begin
    fZLibState.next_out := @Buffer;
    fZLibState.avail_out := uInt(Count);
    ReadBytes := 0;
    TransferOff := 0;
    repeat
      If (ReadBytes > 0) and (TransferOff > 0) then
        begin
        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
          fZLibState.next_in := Pointer(PtrUInt(BufferMemory(fBuffer)) + TransferOff);
        {$IFDEF FPCDWM}{$POP}{$ENDIF}
          fZLibState.avail_in := uInt(PtrUInt(ReadBytes) - TransferOff);
        end
      else
        begin
          ReadBytes := fSource.Read(BufferMemory(fBuffer)^,BufferSize(fBuffer));
          fZLibState.next_in := BufferMemory(fBuffer);
          fZLibState.avail_in := uInt(ReadBytes);
          TransferOff := 0;
        end;
      ResultCode := DecompressionErrCheck(inflate(@fZLibState,Z_NO_FLUSH),fZLibState,True);
      Inc(fTotalCompressed,UInt64(PtrUInt(ReadBytes) - TransferOff - PtrUInt(fZLibState.avail_in)));
      If fZLibState.avail_in > 0 then
        TransferOff := PtrUInt(ReadBytes) - PtrUInt(fZLibState.avail_in)
      else
        TransferOff := 0;
      DoUpdate;
    until (ResultCode = Z_STREAM_END) or (fZLibState.avail_out <= 0);
    If fZLibState.avail_in > 0 then
      fSource.Seek(-Int64(fZLibState.avail_in),soCurrent);    
    Inc(fTotalUncompressed,UInt64(Count - LongInt(fZLibState.avail_out)));
    Result := Count - LongInt(fZLibState.avail_out);
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TZDecompressionStream.Write(const Buffer; Count: LongInt): LongInt;
begin
{$IFDEF FPC}
Result := 0;
{$ENDIf}
raise EZDecompressionError.Create('TZDecompressionStream.Write: ' + ZInvalidOp);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TZDecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  BuffSize = 1024 * 1024; // 1MiB
var
  Buffer:     Pointer;
  OffTemp:    Int64;
  ReadBytes:  LongInt;
begin
{
  Note that seeking is done on uncompressed data, not directly on source, which
  is compressed, so there must be some processing done.
}
case Origin of
  soBeginning:
    If Offset <= 0 then
      begin
      {
        Seek to the beginning - reset zlib state and counters, seek to the
        beginning of source stream.
      }
        DecompressionErrCheck(inflateReset(@fZLibState),fZLibState);
        fTotalCompressed := 0;
        fTotalUncompressed := 0;
        Result := fSource.Seek(0,soBeginning);
      end
    else
      begin
        // Seek to an arbitrary offset from the beginning.
        If Offset = Int64(fTotalUncompressed) then
          Result := Int64(fTotalUncompressed) // current position
        else If Offset > Int64(fTotalUncompressed) then
          Result := Seek(Offset - Int64(fTotalUncompressed),soCurrent)
        else
          begin
          {
            Offset is smaller than uncompressed counter - reset to the start
            and then read until we are at the requested position.
          }
            Seek(0,soBeginning);
            Result := Seek(Offset,soCurrent);
          end;
      end;

  soCurrent:
    If Offset = 0 then
      begin
        // Seek to current position - just return uncompresed data counter.
        Result := Int64(fTotalUncompressed);
      end
    else
      begin
        // Seek to an arbitrary offset from current position.
        If (Int64(fTotalUncompressed) + Offset) <= 0 then
          Result := Seek(0,soBeginning)
        else If Offset < 0 then
          Result := Seek(Int64(fTotalUncompressed) + Offset,soBeginning)
        else
          begin
          {
            Offset is larger than zero, read (decompress) data until the
            decompressed counter reaches the requested offset or we are at the
            end.
          }
            GetMem(Buffer,BuffSize);
            try
              OffTemp := Offset;
              while OffTemp > 0 do begin
                ReadBytes := Read(Buffer^,Min(BuffSize,OffTemp));
                If ReadBytes >= Min(BuffSize,OffTemp) then
                  Dec(OffTemp,ReadBytes)
                else
                  OffTemp := 0;
              end;
              Result := Int64(fTotalUncompressed);
            finally
              FreeMem(Buffer,BuffSize);
            end;
          end;
      end;

  soEnd:
    If Offset = 0 then
      begin
      {
        Seek to the end - do read until there are no more compressed data to be
        read while discarding all decompressed data.
      }
        GetMem(Buffer,BuffSize);
        try
          while Read(Buffer^,BuffSize) > 0 do;
          Result := Int64(fTotalUncompressed);
        finally
          FreeMem(Buffer,BuffSize);
        end;
      end
    else raise EZDecompressionError.Create('TZDecompressionStream.Seek: ' + ZInvalidOp);
    
else
  raise EZDecompressionError.Create('TZDecompressionStream.Seek: ' + ZInvalidOp);
end;
end;

//------------------------------------------------------------------------------

Function TZDecompressionStream.DecompressTo(Destination: TStream): Int64;
begin
Result := ExtractTo(Destination);
end;

//------------------------------------------------------------------------------

Function TZDecompressionStream.ExtractTo(Destination: TStream): Int64;
var
  Buffer:     TMemoryBuffer;
  BytesRead:  LongInt;
begin
Result := 0;
fTotalCounter := UInt64(fSource.Size);
DoProgress(0.0);
BufferInit(Buffer);
BufferGet(Buffer,ZU_INTR_BUFFSIZE);
try
  repeat
    BytesRead := Read(BufferMemory(Buffer)^,ZU_INTR_BUFFSIZE);
    Destination.WriteBuffer(BufferMemory(Buffer)^,BytesRead);
    Inc(Result,Int64(BytesRead));
    DoProgress;
  until BytesRead < ZU_INTR_BUFFSIZE;
finally
  Final;
  BufferFree(Buffer);
  DoProgress(1.0);
end;
end;

//------------------------------------------------------------------------------

procedure TZDecompressionStream.Final;
begin
// nothing to do here
end;


{-------------------------------------------------------------------------------
================================================================================
                                 TZCustomBuffer
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCustomBuffer - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TZCustomBuffer - protected methods
-------------------------------------------------------------------------------}

Function TZCustomBuffer.GetCompressionRatio: Double;
begin
If fTotalUncompressed <> 0 then
  Result := fTotalCompressed / fTotalUncompressed
else
  Result := 0.0;
end;

{-------------------------------------------------------------------------------
    TZCustomBuffer - public methods
-------------------------------------------------------------------------------}

constructor TZCustomBuffer.Create(Src: TMemoryBuffer);
begin
inherited Create;
fSource := Src;
BufferGet(fBuffer,ZU_BUFF_BUFFSIZE);
fFreeResult := True;
fTotalCompressed := 0;
fTotalUncompressed := 0;
fExpectedResultSize := BufferSize(Src);
end;

//------------------------------------------------------------------------------

destructor TZCustomBuffer.Destroy;
begin
BufferFree(fBuffer);
If fFreeResult then
  BufferFree(fResult);
inherited;
end;

//------------------------------------------------------------------------------

procedure TZCustomBuffer.Process;
begin
If BufferSize(fSource) > 0 then
  begin
    DoProgress;
    Init;
    try
      while Update do
        DoProgress;
    finally
      Final;
    end;
    DoProgress;
  end;
end;


{-------------------------------------------------------------------------------
================================================================================
                              TZCompressionBuffer
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZCompressionBuffer - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TZCompressionBuffer - protected methods
-------------------------------------------------------------------------------}

{$IFDEF FPCDWM}{$PUSH}W4055 W5024{$ENDIF}
procedure TZCompressionBuffer.ProcessorHandler(Sender: TObject; const Data; Size: TMemSize);
begin
while (fTotalCompressed + Size) > BufferSize(fResult) do
  BufferRealloc(fResult,((BufferSize(fResult) + (BufferSize(fResult) shr 1)) + 255) and not TMemSize(255));
Move(Data,Pointer(PtrUInt(BufferMemory(fResult)) + PtrUInt(fTotalCompressed))^,Size);
Inc(fTotalCompressed,Size);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TZCompressionBuffer.DoProgress;
var
  Progress: Double;
begin
If BufferSize(fSource) <> 0 then
  Progress := fTotalUncompressed / BufferSize(fSource)
else
  Progress := 0.0;
If Assigned(fOnProgressEvent) then
  fOnProgressEvent(Self,Progress)
else If Assigned(fOnProgressCallback) then
  fOnProgressCallback(Self,Progress);
end;

//------------------------------------------------------------------------------

procedure TZCompressionBuffer.Init;
begin
BufferGet(fResult,fExpectedResultSize);
fCompressor := TZCompressor.Create(fCompressionLevel,fMemLevel,fStrategy,fWindowBits);
fCompressor.OnOutputEvent := ProcessorHandler;
fCompressor.Init;
end;

//------------------------------------------------------------------------------

Function TZCompressionBuffer.Update: Boolean;
var
  Size:       TMemSize;
  Processed:  TMemSize;
begin
If (fTotalUncompressed + ZU_BUFF_BUFFSIZE) > BufferSize(fSource) then
  Size := BufferSize(fSource) - TMemSize(fTotalUncompressed)
else
  Size := ZU_BUFF_BUFFSIZE;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Processed := TMemSize(fCompressor.Update(Pointer(PtrUInt(BufferMemory(fSource)) + PtrUInt(fTotalUncompressed))^,uInt(Size)));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
Inc(fTotalUncompressed,Processed);
Result := (Processed >= Size) and (TMemSize(fTotalUncompressed) < BufferSize(fSource));
end;

//------------------------------------------------------------------------------

procedure TZCompressionBuffer.Final;
begin
fCompressor.Final;
fCompressor.Free;
BufferRealloc(fResult,fTotalCompressed);
end;

{-------------------------------------------------------------------------------
    TZCompressionBuffer - public methods
-------------------------------------------------------------------------------}

constructor TZCompressionBuffer.Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int);
begin
inherited Create(Src);
fCompressionLevel := CompressionLevel;
fMemLevel := MemLevel;
fStrategy := Strategy;
fWindowBits := WindowBits;
fExpectedResultSize := Trunc(BufferSize(fSource) * 1.1);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionBuffer.Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType);
begin
Create(Src,CompressionLevel,MemLevel,Strategy,StreamTypeToWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionBuffer.Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel; WindowBits: int);
begin
Create(Src,CompressionLevel,zmlDefault,zsDefault,WindowBits);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionBuffer.Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel; StreamType: TZStreamType);
begin
Create(Src,CompressionLevel,zmlDefault,zsDefault,StreamTypeToWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionBuffer.Create(Src: TMemoryBuffer; CompressionLevel: TZCompressionLevel = zclDefault);
begin
Create(Src,CompressionLevel,zmlDefault,zsDefault,zstDefault);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionBuffer.Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; WindowBits: int);
begin
Create(BufferBuild(Src,SrcSize),CompressionLevel,MemLevel,Strategy,WindowBits);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionBuffer.Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel; MemLevel: TZMemLevel; Strategy: TZStrategy; StreamType: TZStreamType);
begin
Create(BufferBuild(Src,SrcSize),CompressionLevel,MemLevel,Strategy,StreamTypeToWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionBuffer.Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel; WindowBits: int);
begin
Create(BufferBuild(Src,SrcSize),CompressionLevel,zmlDefault,zsDefault,WindowBits);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionBuffer.Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel; StreamType: TZStreamType);
begin
Create(BufferBuild(Src,SrcSize),CompressionLevel,zmlDefault,zsDefault,StreamTypeToWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZCompressionBuffer.Create(Src: Pointer; SrcSize: TMemSize; CompressionLevel: TZCompressionLevel = zclDefault);
begin
Create(BufferBuild(Src,SrcSize),CompressionLevel,zmlDefault,zsDefault,zstDefault);
end;


{-------------------------------------------------------------------------------
================================================================================
                             TZDecompressionBuffer
================================================================================
-------------------------------------------------------------------------------}
{===============================================================================
    TZDecompressionBuffer - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TZDecompressionBuffer - protected methods
-------------------------------------------------------------------------------}

{$IFDEF FPCDWM}{$PUSH}W4055 W5024{$ENDIF}
procedure TZDecompressionBuffer.ProcessorHandler(Sender: TObject; const Data; Size: TMemSize);
begin
while (fTotalUncompressed + Size) > BufferSize(fResult) do
  BufferRealloc(fResult,((BufferSize(fResult) * 2) + 255) and not TMemSize(255));
Move(Data,Pointer(PtrUInt(BufferMemory(fResult)) + PtrUInt(fTotalUncompressed))^,Size);
Inc(fTotalUncompressed,Size);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TZDecompressionBuffer.DoProgress;
var
  Progress: Double;
begin
If BufferSize(fSource) <> 0 then
  Progress := fTotalCompressed / BufferSize(fSource)
else
  Progress := 0.0;
If Assigned(fOnProgressEvent) then
  fOnProgressEvent(Self,Progress)
else If Assigned(fOnProgressCallback) then
  fOnProgressCallback(Self,Progress);
end;

//------------------------------------------------------------------------------

procedure TZDecompressionBuffer.Init;
begin
BufferGet(fResult,fExpectedResultSize);
fDecompressor := TZDecompressor.Create(fWindowBits);
fDecompressor.OnOutputEvent := ProcessorHandler;
fDecompressor.Init;
end;

//------------------------------------------------------------------------------

Function TZDecompressionBuffer.Update: Boolean;
var
  Size:       TMemSize;
  Processed:  TMemSize;
begin
If (fTotalCompressed + ZU_BUFF_BUFFSIZE) > BufferSize(fSource) then
  Size := BufferSize(fSource) - TMemSize(fTotalCompressed)
else
  Size := ZU_BUFF_BUFFSIZE;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Processed := fDecompressor.Update(Pointer(PtrUInt(BufferMemory(fSource)) + PtrUInt(fTotalCompressed))^,uInt(Size));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
Inc(fTotalCompressed,Processed);
Result := (Processed >= Size) and (TMemSize(fTotalCompressed) < BufferSize(fSource));
end;

//------------------------------------------------------------------------------

procedure TZDecompressionBuffer.Final;
begin
fDecompressor.Final;
fDecompressor.Free;
BufferRealloc(fResult,fTotalUncompressed);
end;

{-------------------------------------------------------------------------------
    TZDecompressionBuffer - public methods
-------------------------------------------------------------------------------}

constructor TZDecompressionBuffer.Create(Src: TMemoryBuffer; WindowBits: int);
begin
inherited Create(Src);
fWindowBits := WindowBits;
fExpectedResultSize := BufferSize(fSource) * 2;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZDecompressionBuffer.Create(Src: TMemoryBuffer; StreamType: TZStreamType = zstDefault);
begin
Create(Src,StreamTypeToWBits(StreamType));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZDecompressionBuffer.Create(Src: Pointer; SrcSize: TMemSize; WindowBits: int);
begin
Create(BufferBuild(Src,SrcSize),WindowBits);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TZDecompressionBuffer.Create(Src: Pointer; SrcSize: TMemSize; StreamType: TZStreamType = zstDefault);
begin
Create(BufferBuild(Src,SrcSize),StreamTypeToWBits(StreamType));
end;


{===============================================================================
    Unit initialization/finalization
===============================================================================}

{$IFNDEF ZLib_Static}
initialization
  If not ZLibDynamic.ZLib_Initialize then
    raise Exception.Create('Failed to initialize dynamically loaded zlib library.');

finalization
  ZLibDynamic.ZLib_Finalize;
{$ENDIF}

end.
