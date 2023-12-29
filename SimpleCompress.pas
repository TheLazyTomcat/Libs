{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple Compress

    Set of functions designed to ease compression and decompression of memory
    buffers, streams and files. Actual compression is done using zlib library.

  Version 1.4.1 (2021-12-12)

  Last change 2023-12-29

  ©2015-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleCompress

  Dependencies:
    AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses     - github.com/TheLazyTomcat/Lib.AuxClasses
    MemoryBuffer   - github.com/TheLazyTomcat/Lib.MemoryBuffer
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
    WinFileInfo    - github.com/TheLazyTomcat/Lib.WinFileInfo
  * DynLibUtils    - github.com/TheLazyTomcat/Lib.DynLibUtils
  * SimpleCPUID    - github.com/TheLazyTomcat/Lib.SimpleCPUID
    ZLibUtils      - github.com/TheLazyTomcat/Lib.ZLibUtils
  * WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion
    ZLib           - github.com/TheLazyTomcat/Bnd.ZLib

  Libraries DynLibUtils, SimpleCPUID and WindowsVersion are needed only when
  compiling for Windows OS.

===============================================================================}
unit SimpleCompress;

{$IFDEF FPC}
  {$MODE ObjFPC}
{$ENDIF}
{$H+}

interface

uses
  Classes,
  AuxTypes, AuxClasses, ZLibUtils;

{===============================================================================
--------------------------------------------------------------------------------
                                  Compression
--------------------------------------------------------------------------------
===============================================================================}
{
  ZCompressBuffer

  Compresses content of given memory into a new memory buffer and returns
  pointer to this buffer along with its size.

  The buffer is allocated using standard memory management functions, so the
  memory can be freed by those functions as well (eg. FreeMemory).

  Sender in callbacks is of type TZCompressionBuffer and user data are stored in
  its UserPtrData property.
}
Function ZCompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

{
  ZCompressStream

  Compresses data provided in input stream and stores result in output stream.

  Position in both input and output streams are set to beginning before the
  compression, meaning entire input stream is compressed, irrespective of
  current position.
  Size of output stream is altered after compression so it matches amount of
  compressed data.

  Position of both streams after return from this function is undefined.

  Sender in callbacks is of type TZCompressionStream and user data are stored in
  its UserPtrData property.
}
Function ZCompressStream(InStream, OutStream: TStream; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressStream(InStream, OutStream: TStream; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressStream(InStream, OutStream: TStream; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

{
  ZCompressStream

  Compresses data from the provided stream and stores result back into the same
  stream.

  Position of the input stream is set to beginning before processing, so the
  entire stream is compressed.
  After the compression, the result is stored back at the start of the stream
  and size of the stream is altered to match an amount of compressed data.
  During the processing, the compressed data are temporarily stored in their
  entirity in memory buffer - this might pose a problem for large streams, so
  be carefull with this function.

  Sender in callbacks is of type TZCompressionStream and user data are stored in
  its UserPtrData property.
}
Function ZCompressStream(Stream: TStream; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressStream(Stream: TStream; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressStream(Stream: TStream; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

{
  ZCompressFile

  Compresses data from input file and stores the result in output file.

  If the output file already exists, it will be rewritten.

  Sender in callbacks is of type TZCompressionStream and user data are stored in
  its UserPtrData property.
}
Function ZCompressFile(const InFileName, OutFileName: String; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressFile(const InFileName, OutFileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressFile(const InFileName, OutFileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

{
  ZCompressFile

  Compresses data from provided file and stores the result back to the same
  file, rewriting it.

  Size of the file is altered to match the amount of compressed data.
  All compressed data are temporarily stored in memory before they are stored
  back to the file - this might be problematic for massive files, as the data
  might not fit into memory. If this problem occurs, use function
  ZCompressFileTemp instead.

  Sender in callbacks is of type TZCompressionStream and user data are stored in
  its UserPtrData property.
}
Function ZCompressFile(const FileName: String; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressFile(const FileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressFile(const FileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

{
  Compresses selected file to a temporary file (in the same folder), then
  deletes original file and renames the temporary file to the original input
  file name.

  Sender in callbacks is of type TZCompressionStream and user data are stored in
  its UserPtrData property.
}
Function ZCompressFileTemp(const FileName: String; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressFileTemp(const FileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZCompressFileTemp(const FileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                 Decompression
--------------------------------------------------------------------------------
===============================================================================}
{
  Decompression functions work exactly the same as their respective compression
  counterparts - so refer to them for details.
}
Function ZDecompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

Function ZDecompressStream(InStream, OutStream: TStream; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressStream(InStream, OutStream: TStream; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressStream(InStream, OutStream: TStream; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

Function ZDecompressStream(Stream: TStream; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressStream(Stream: TStream; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressStream(Stream: TStream; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

Function ZDecompressFile(const InFileName, OutFileName: String; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressFile(const InFileName, OutFileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressFile(const InFileName, OutFileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

Function ZDecompressFile(const FileName: String; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressFile(const FileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressFile(const FileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

Function ZDecompressFileTemp(const FileName: String; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressFileTemp(const FileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;
Function ZDecompressFileTemp(const FileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; overload;

implementation

uses
  {$IFNDEF FPC}Windows,{$ENDIF} SysUtils,
  MemoryBuffer, StrRect, WinFileInfo;

{===============================================================================
    Auxiliary functions
===============================================================================}

procedure CopyStream(Src,Dest: TStream);
const
  COPY_BUFFER_SIZE = 1024 * 1024 {1MiB};
var
  Buffer:     Pointer;
  BytesRead:  Integer;
begin
GetMem(Buffer,COPY_BUFFER_SIZE);
try
  repeat
    BytesRead := Src.Read(Buffer^,COPY_BUFFER_SIZE);
    Dest.WriteBuffer(Buffer^,BytesRead);
  until TMemSize(BytesRead) < COPY_BUFFER_SIZE;
finally
  FreeMem(Buffer);
end;
end;

{===============================================================================
--------------------------------------------------------------------------------
                                  Compression
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Internal compression functions
===============================================================================}

Function ZCompressBuffer2(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  Compressor: TZCompressionBuffer;
begin
try
  Compressor := TZCompressionBuffer.Create(InBuff,InSize,zclDefault,StreamType);
  try
    Compressor.FreeResult := False;
    Compressor.OnProgressEvent := ProgressEvent;
    Compressor.OnProgressCallback := ProgressCallback;
    Compressor.UserPtrData := UserData;
    Compressor.Process;
    OutBuff := BufferMemory(Compressor.Result);
    OutSize := BufferSize(Compressor.Result);
    Result := True;
  finally
    Compressor.Free;
  end;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function ZCompressStream1(Stream: TStream; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean; forward;
{
  forward declaration, implemented later
}

//------------------------------------------------------------------------------

Function ZCompressStream2(InStream, OutStream: TStream; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  Compressor: TZCompressionStream;
begin
try
  If InStream <> OutStream then
    begin
      Compressor := TZCompressionStream.Create(OutStream,zclDefault,StreamType);
      try
        Compressor.OnProgressEvent := ProgressEvent;
        Compressor.OnProgressCallback := ProgressCallback;
        Compressor.UserPtrData := UserData;
        InStream.Seek(0,soBeginning);
        OutStream.Seek(0,soBeginning);
        Compressor.CompressFrom(InStream);
        OutStream.Size := OutStream.Position;
        Result := True;
      finally
        Compressor.Free;
      end;
    end
  else Result := ZCompressStream1(InStream,ProgressEvent,ProgressCallback,UserData,StreamType);
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function ZCompressStream1(Stream: TStream; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  TempStream: TMemoryStream;
begin
TempStream := TMemoryStream.Create;
try
  TempStream.Size := Stream.Size; // preallocate
  TempStream.Seek(0,soBeginning);
  Stream.Seek(0,soBeginning);
  Result := ZCompressStream2(Stream,TempStream,ProgressEvent,ProgressCallback,UserData,StreamType);
  If Result then
    begin
      TempStream.Seek(0,soBeginning);
      Stream.Seek(0,soBeginning);
      CopyStream(TempStream,Stream);
      Stream.Size := Stream.Position;
    end;
finally
  TempStream.Free;
end;
end;

//------------------------------------------------------------------------------

Function ZCompressFile1(const FileName: String; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean; forward;
{
  forward declaration, implemented later
}

//------------------------------------------------------------------------------

Function ZCompressFile2(const InFileName, OutFileName: String; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  IsSameFile:     Boolean;
  InFileStream:   TFileStream;
  OutFileStream:  TFileStream;
begin
If FileExists(OutFileName) then
  IsSameFile := SameFile(InFileName,OutFileName)
else
  IsSameFile := False;
If not IsSameFile then
  begin
    InFileStream := TFileStream.Create(StrToRTL(InFileName),fmOpenRead or fmShareDenyWrite);
    try
      OutFileStream := TFileStream.Create(StrToRTL(OutFileName),fmCreate or fmShareExclusive);
      try
        Result := ZCompressStream2(InFileStream,OutFileStream,ProgressEvent,ProgressCallback,UserData,StreamType);
      finally
        OutFileStream.Free;
      end;
    finally
      InFileStream.Free;
    end;
  end
else Result := ZCompressFile1(InFileName,ProgressEvent,ProgressCallback,UserData,StreamType);
end;

//------------------------------------------------------------------------------

Function ZCompressFile1(const FileName: String; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  FileStream: TFileStream;
begin
try
  FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenReadWrite or fmShareExclusive);
  try
    Result := ZCompressStream1(FileStream,ProgressEvent,ProgressCallback,UserData,StreamType);
  finally
    FileStream.Free;
  end;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function ZCompressFileTemp1(const FileName: String; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  TempFileName: String;
begin
TempFileName := FileName;
repeat
  TempFileName := TempFileName + '.tmp';
until not FileExists(StrToRTL(TempFileName));
If ZCompressFile2(FileName,TempFileName,ProgressEvent,ProgressCallback,UserData,StreamType) then
  begin
    If DeleteFile(FileName) then
      Result := RenameFile(TempFileName,FileName)
    else
      Result := False;
  end
else Result := False;
end;

{===============================================================================
    Public compression functions
===============================================================================}

Function ZCompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressBuffer2(InBuff,InSize,OutBuff,OutSize,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressBuffer2(InBuff,InSize,OutBuff,OutSize,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressBuffer2(InBuff,InSize,OutBuff,OutSize,nil,ProgressCallback,UserData,StreamType);
end;

//==============================================================================

Function ZCompressStream(InStream, OutStream: TStream; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressStream2(InStream,OutStream,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressStream(InStream, OutStream: TStream; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressStream2(InStream,OutStream,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressStream(InStream, OutStream: TStream; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressStream2(InStream,OutStream,nil,ProgressCallback,UserData,StreamType);
end;

//------------------------------------------------------------------------------

Function ZCompressStream(Stream: TStream; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressStream1(Stream,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressStream(Stream: TStream; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressStream1(Stream,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressStream(Stream: TStream; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressStream1(Stream,nil,ProgressCallback,UserData,StreamType);
end;

//==============================================================================

Function ZCompressFile(const InFileName, OutFileName: String; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressFile2(InFileName,OutFileName,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressFile(const InFileName, OutFileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressFile2(InFileName,OutFileName,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressFile(const InFileName, OutFileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressFile2(InFileName,OutFileName,nil,ProgressCallback,UserData,StreamType);
end;

//------------------------------------------------------------------------------

Function ZCompressFile(const FileName: String; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressFile1(FileName,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressFile(const FileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressFile1(FileName,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressFile(const FileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressFile1(FileName,nil,ProgressCallback,UserData,StreamType);
end;

//------------------------------------------------------------------------------

Function ZCompressFileTemp(const FileName: String; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressFileTemp1(FileName,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressFileTemp(const FileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressFileTemp1(FileName,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZCompressFileTemp(const FileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZCompressFileTemp1(FileName,nil,ProgressCallback,UserData,StreamType);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Decompression
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Internal decompression functions
===============================================================================}

Function ZDecompressBuffer2(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  Decompressor: TZDecompressionBuffer;
begin
try
  Decompressor := TZDecompressionBuffer.Create(InBuff,InSize,StreamType);
  try
    Decompressor.FreeResult := False;
    Decompressor.OnProgressEvent := ProgressEvent;
    Decompressor.OnProgressCallback := ProgressCallback;
    Decompressor.UserPtrData := UserData;
    Decompressor.Process;
    OutBuff := BufferMemory(Decompressor.Result);
    OutSize := BufferSize(Decompressor.Result);
    Result := True;
  finally
    Decompressor.Free;
  end;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function ZDecompressStream1(Stream: TStream; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean; forward;
{
  forward declaration, implemented later
}

//------------------------------------------------------------------------------

Function ZDecompressStream2(InStream, OutStream: TStream; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  Decompressor: TZDecompressionStream;
begin
try
  If InStream <> OutStream then
    begin
      Decompressor := TZDecompressionStream.Create(InStream,StreamType);
      try
        Decompressor.OnProgressEvent := ProgressEvent;
        Decompressor.OnProgressCallback := ProgressCallback;
        Decompressor.UserPtrData := UserData;
        InStream.Seek(0,soBeginning);
        OutStream.Seek(0,soBeginning);
        Decompressor.DecompressTo(OutStream);
        OutStream.Size := OutStream.Position;
        Result := True;
      finally
        Decompressor.Free;
      end;
    end
  else Result := ZDecompressStream1(InStream,ProgressEvent,ProgressCallback,UserData,StreamType);
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function ZDecompressStream1(Stream: TStream; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  TempStream: TMemoryStream;
begin
TempStream := TMemoryStream.Create;
try
  TempStream.Size := Stream.Size; // preallocate
  TempStream.Seek(0,soBeginning);
  Stream.Seek(0,soBeginning);
  Result := ZDecompressStream2(Stream,TempStream,ProgressEvent,ProgressCallback,UserData,StreamType);
  If Result then
    begin
      TempStream.Seek(0,soBeginning);
      Stream.Seek(0,soBeginning);
      CopyStream(TempStream,Stream);
      Stream.Size := Stream.Position;
    end;
finally
  TempStream.Free;
end;
end;

//------------------------------------------------------------------------------

Function ZDecompressFile1(const FileName: String; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean; forward;
{
  forward declaration, implemented later
}

//------------------------------------------------------------------------------

Function ZDecompressFile2(const InFileName, OutFileName: String; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  IsSameFile:     Boolean;
  InFileStream:   TFileStream;
  OutFileStream:  TFileStream;
begin
If FileExists(OutFileName) then
  IsSameFile := SameFile(InFileName,OutFileName)
else
  IsSameFile := False;
If not IsSameFile then
  begin
    InFileStream := TFileStream.Create(StrToRTL(InFileName),fmOpenRead or fmShareDenyWrite);
    try
      OutFileStream := TFileStream.Create(StrToRTL(OutFileName),fmCreate or fmShareExclusive);
      try
        Result := ZDecompressStream2(InFileStream,OutFileStream,ProgressEvent,ProgressCallback,UserData,StreamType);
      finally
        OutFileStream.Free;
      end;
    finally
      InFileStream.Free;
    end;
  end
else Result := ZCompressFile1(InFileName,ProgressEvent,ProgressCallback,UserData,StreamType);
end;

//------------------------------------------------------------------------------

Function ZDecompressFile1(const FileName: String; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  FileStream: TFileStream;
begin
try
  FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenReadWrite or fmShareExclusive);
  try
    Result := ZDecompressStream1(FileStream,ProgressEvent,ProgressCallback,UserData,StreamType);
  finally
    FileStream.Free;
  end;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function ZDecompressFileTemp1(const FileName: String; ProgressEvent: TFloatEvent; ProgressCallback: TFloatCallback; UserData: Pointer; StreamType: TZStreamType): Boolean;
var
  TempFileName: String;
begin
TempFileName := FileName;
repeat
  TempFileName := TempFileName + '.tmp';
until not FileExists(StrToRTL(TempFileName));
If ZDecompressFile2(FileName,TempFileName,ProgressEvent,ProgressCallback,UserData,StreamType) then
  begin
    If DeleteFile(FileName) then
      Result := RenameFile(TempFileName,FileName)
    else
      Result := False;
  end
else Result := False;
end;


{===============================================================================
    Public decompression functions
===============================================================================}

Function ZDecompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressBuffer2(InBuff,InSize,OutBuff,OutSize,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressBuffer2(InBuff,InSize,OutBuff,OutSize,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressBuffer(InBuff: Pointer; InSize: TMemSize; out OutBuff: Pointer; out OutSize: TMemSize; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressBuffer2(InBuff,InSize,OutBuff,OutSize,nil,ProgressCallback,UserData,StreamType);
end;

//==============================================================================

Function ZDecompressStream(InStream, OutStream: TStream; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressStream2(InStream,OutStream,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressStream(InStream, OutStream: TStream; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; 
begin
Result := ZDecompressStream2(InStream,OutStream,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressStream(InStream, OutStream: TStream; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; 
begin
Result := ZDecompressStream2(InStream,OutStream,nil,ProgressCallback,UserData,StreamType);
end;

//------------------------------------------------------------------------------

Function ZDecompressStream(Stream: TStream; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressStream1(Stream,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressStream(Stream: TStream; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean; 
begin
Result := ZDecompressStream1(Stream,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressStream(Stream: TStream; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressStream1(Stream,nil,ProgressCallback,UserData,StreamType);
end;

//==============================================================================

Function ZDecompressFile(const InFileName, OutFileName: String; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressFile2(InFileName,OutFileName,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressFile(const InFileName, OutFileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressFile2(InFileName,OutFileName,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressFile(const InFileName, OutFileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressFile2(InFileName,OutFileName,nil,ProgressCallback,UserData,StreamType);
end;

//------------------------------------------------------------------------------

Function ZDecompressFile(const FileName: String; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressFile1(FileName,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressFile(const FileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressFile1(FileName,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressFile(const FileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressFile1(FileName,nil,ProgressCallback,UserData,StreamType);
end;

//------------------------------------------------------------------------------

Function ZDecompressFileTemp(const FileName: String; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressFileTemp1(FileName,nil,nil,nil,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressFileTemp(const FileName: String; ProgressEvent: TFloatEvent; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressFileTemp1(FileName,ProgressEvent,nil,UserData,StreamType);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ZDecompressFileTemp(const FileName: String; ProgressCallback: TFloatCallback; UserData: Pointer = nil; StreamType: TZStreamType = zstDefault): Boolean;
begin
Result := ZDecompressFileTemp1(FileName,nil,ProgressCallback,UserData,StreamType);
end;

end.
