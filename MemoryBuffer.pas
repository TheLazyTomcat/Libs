{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Memory buffer

    This library provides simple memory buffer that is intended to simplify
    work with dynamically allocated memory - mainly by storing pointer and
    size in one place.

    The buffer is implemented as a record, but you should treat it as an opaque
    object and never access individual fields directly. Instead, use provided
    functions for any desired access.

    Note that the object does not directly contain any buffered data, merely a
    pointer to them. So if you want to access this data, use function that
    provides access to memory pointer - BufferMemory.

    Also remember that this buffer does not automatically free or allocate
    the memory, you are responsible to call proper functions for that.

    In current implementation, the buffer has two sizes - indicated and
    allocated. Indicated size is number of bytes of data stored, allocated is
    size of the memory allocated for the buffer. This two numbers might differ
    (indicated size might be smaller) if the buffer was reallocated with
    AllowShrink set to false.
    To access the data, always use indicated size.

  Version 1.1.4 (2021-10-15)

  Last change 2021-10-15

  ©2015-2021 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.MemoryBuffer

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes

===============================================================================}
unit MemoryBuffer;

{$IF defined(CPU64) or defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF defined(CPU16)}
  {$MESSAGE FATAL '16bit CPU not supported'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes,
  AuxTypes;

{===============================================================================
    Memory Buffer - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Memory Buffer - main structure
-------------------------------------------------------------------------------}
type
  TMemoryBuffer = record
    Memory:     Pointer;
    Signature:  PtrUInt;
    Size:       TMemSize;
    AllocSize:  TMemSize;
    UserData:   PtrInt;
    Checksum:   PtrUInt;
    CheckStr:   String;
  end;
  PMemoryBuffer = ^TMemoryBuffer;

{-------------------------------------------------------------------------------
    Memory Buffer - exception classes
-------------------------------------------------------------------------------}

type
  EMBException = class(Exception);

  EMBInvalidBuffer = class(EMBException);
  EMBInvalidValue  = class(EMBException);

{-------------------------------------------------------------------------------
    Memory Buffer - validity routines
-------------------------------------------------------------------------------}
{
  BufferIsValid

  Returns true when buffer is considered to be valid (ie. was initialized),
  false otherwise.

  Invalid buffers cannot be worked with (eg. copied from).

  To make buffer valid, it must be passed to function BufferInit or any other
  function that initializes it implicitly.
}
Function BufferIsValid(const Buff: TMemoryBuffer): Boolean;

//------------------------------------------------------------------------------
{
  BufferInit

  Initializes the buffer object (makes it valid) so it can be used in other
  functions.

  Note that if the buffer was already initialized, it will be re-initialized
  without freeing potentially allocated memory. You are responsible for
  checking buffer (in)validity before passing it to this function.

  You are not required to explicitly call BufferInit to work with the buffer,
  as most functions call it when uninitialized buffer is passed to them.
  Nevertheless, it is a recommended practice to do so.
}
procedure BufferInit(out Buff: TMemoryBuffer);

//------------------------------------------------------------------------------
{
  BufferFinal

  Makes the buffer invalid.

  If buffer with allocated memory is passed to this function, it is first freed
  and then invalidated.

  Note that it is not required that the buffer is invalidated at the end of its
  lifespan, it must only be freed.
}
procedure BufferFinal(var Buff: TMemoryBuffer);


{-------------------------------------------------------------------------------
    Memory Buffer - allocation, freeing
-------------------------------------------------------------------------------}
{
  BufferGet

  Allocates memory of requested size for the buffer.

  If an uninitialized buffer is passed into this function, it is automatically
  initialized.

    WARNING - the second override always initializes the result, irrespective
              of where it will be assigned.

  If buffer with already allocated memory is passed, the memory is freed before
  a new memory is allocated.
}
procedure BufferGet(var Buff: TMemoryBuffer; Size: TMemSize); overload;
Function BufferGet(Size: TMemSize): TMemoryBuffer; overload;

//------------------------------------------------------------------------------
{
  BufferAlloc

  Allocates memory of requested size for the buffer, and initializes this
  memory (fills it with zeroes).

  If an uninitialized buffer is passed into this function, it is automatically
  initialized.

    WARNING - the second override always initializes the result, irrespective
              of where it will be assigned.  

  If buffer with already allocated memory is passed, the memory is freed before
  a new memory is allocated.  
}
procedure BufferAlloc(var Buff: TMemoryBuffer; Size: TMemSize); overload;
Function BufferAlloc(Size: TMemSize): TMemoryBuffer; overload;

//------------------------------------------------------------------------------
{
  BufferFree

  Frees memory allocated for the buffer.

  If an uninitialized buffer is passed into this function, it is initialized
  and no freeing is performed.

  This function does not invalidate the buffer, so it can be used for further
  allocation.
}
procedure BufferFree(var Buff: TMemoryBuffer);

//------------------------------------------------------------------------------
{
  BufferRealloc

  Reallocates memory of the buffer (changes size while preserving stored data).

  When the buffer is enlarged, the content of newly allocated memory is
  undefined. When it is shrinked, data beyond the new size are removed.

  For AllowShrink set to false, the memory is reallocated only if new size is
  larger than currently allocated memory, if the new size is equal or smaller
  then the memory is preserved as is and only indicated size is changed.

  For AllowShrink set to true, the reallocation always takes place (for new
  size equal to allocated size nothing happens, for new size of zero the bufer
  is freed).

  If an uninitialized buffer is passed into this function, it is initialized
  and normally allocated - equivalent to calling BufferAlloc function.
}
procedure BufferRealloc(var Buff: TMemoryBuffer; NewSize: TMemSize; AllowShrink: Boolean = True);


{-------------------------------------------------------------------------------
    Memory Buffer - whole buffer manipulation
-------------------------------------------------------------------------------}
{
  BufferCopy

  Copies indicated data from source buffer into preexisting destination buffer,
  reallocating it if required. Also copies user data.

  If the destination buffer is not initialized, it will be initialized and
  allocated to required size.

  If the passed source buffer is not valid, this function will raise an
  EMBInvalidBuffer exception.
}
procedure BufferCopy(const Src: TMemoryBuffer; var Dest: TMemoryBuffer); overload;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{
  BufferCopy

  Creates (allocates) new buffer and copies all indicated data from the source
  buffer into it. Also copies user data.

  Do not use this function to assign buffer variable that might have been
  already allocated, this function will rewrite it without freeing the memory,
  causing a memory leak.
  In that situation, use the second overload that can better manage such
  situation.

  If the passed source buffer is not valid, this function will raise an
  EMBInvalidBuffer exception.
}
Function BufferCopy(const Src: TMemoryBuffer): TMemoryBuffer; overload;


{-------------------------------------------------------------------------------
    Memory Buffer - data access
-------------------------------------------------------------------------------}
{
  BufferMemory

  Returns offset memory location within the buffer (address of the allocated
  buffer, plus given offset).

  When the passed buffer is invalid, it returns nil.

  If the offset is larger than indicated size, the function will raise an
  EMBInvalidValue exception.
}
Function BufferMemory(const Buff: TMemoryBuffer; Offset: TMemSize = 0): Pointer;

//------------------------------------------------------------------------------
{
  BufferSize

  Returns indicated size of the buffer - that is, number of bytes of stored
  data.

  When the passed buffer is invalid, it returns 0.
}
Function BufferSize(const Buff: TMemoryBuffer): TMemSize;

//------------------------------------------------------------------------------
{
  BufferAllocSize

  Returns allocated size of the buffer - that is, number of bytes allocated
  in the memory for the passed buffer.

  When the passed buffer is invalid, it returns 0.
}
Function BufferAllocSize(const Buff: TMemoryBuffer): TMemSize;

//------------------------------------------------------------------------------
{
  BufferGetUserData

  Returns value of stored user data.

  When the passed buffer is invalid, it returns 0.
}
Function BufferGetUserData(const Buff: TMemoryBuffer): PtrInt;

//------------------------------------------------------------------------------
{
  BufferSetUserData

  Stores new value of user data and returns previous value of stored user data.

  When the passed buffer is invalid, it returns 0 and does not store anything.
}
Function BufferSetUserData(var Buff: TMemoryBuffer; UserData: PtrInt): PtrInt;


{-------------------------------------------------------------------------------
    Memory Buffer - data read/write
-------------------------------------------------------------------------------}
{
  BufferStore

  Stores data from provided untyped variable of given size into the buffer,
  reallocating the buffer if necessary.

  If the passed buffer is invalid, it is first initialized.  
}
procedure BufferStore(var Buff: TMemoryBuffer; const Src; Size: TMemSize); overload;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{
  BufferStore

  Stores data from provided memory location of given size into the buffer,
  reallocating the buffer if necessary.

  If the passed buffer is invalid, it is first initialized.
}
procedure BufferStore(var Buff: TMemoryBuffer; Src: Pointer; Size: TMemSize); overload;

//------------------------------------------------------------------------------
{
  BufferWrite

  Writes Count number of bytes from untyped variable Src into the buffer memory
  at a given offset from the beginning.

  If the passed buffer is invalid, it is first initialized and allocated.

  The buffer is enlarged when written data cannot fit. If thre is gap between
  currently buffered data and written data, then content of this gap is
  undefined.
}
procedure BufferWrite(var Buff: TMemoryBuffer; const Src; Count: TMemSize; Offset: TMemSize = 0);

//------------------------------------------------------------------------------
{
  BufferRead

  Reads Count number of bytes from buffered data at given offset into untyped
  variable Dest.

  If the passed buffer is invalid, then an EMBInvalidBuffer exception is raised.

  If the required data are outside of the buffer, either due to offset or count
  being too large, then an EMBInvalidValue exception is raised.
}
procedure BufferRead(const Buff: TMemoryBuffer; out Dest; Count: TMemSize; Offset: TMemSize = 0);


{-------------------------------------------------------------------------------
    Memory Buffer - buffered data streaming
-------------------------------------------------------------------------------}
{
  Stream_WriteMemoryBuffer

  Writes content (if any) of provided buffer into the stream.

  If buffer is invalid, this function raises an EMBInvalidBuffer exception.

  When Advance is true, the stream position is changed accordingly to number of
  bytes written, otherwise it is preserved.

  Note that this function merely writes the buffered data, not the size or user
  data.
}
procedure Stream_WriteMemoryBuffer(Stream: TStream; const Buff: TMemoryBuffer; Advance: Boolean = True);

//------------------------------------------------------------------------------
{
  Stream_SaveMemoryBuffer

  Writes user data and indicated size into the stream (both are stored as
  unsigned 64bit integer with little endianity) and then writes content
  (if any) of provided buffer.

  If buffer is invalid, this function raises an EMBInvalidBuffer exception.

  When Advance is true, the stream position is changed accordingly to number of
  bytes written, otherwise it is preserved.
}
procedure Stream_SaveMemoryBuffer(Stream: TStream; const Buff: TMemoryBuffer; Advance: Boolean = True);

//------------------------------------------------------------------------------
{
  Stream_ReadMemoryBuffer

  Reads data from a stream into provided buffer.

  If buffer is invalid, this function raises an EMBInvalidBuffer exception.

  When Advance is true, the stream position is changed accordingly to number of
  bytes read, otherwise it is preserved.

  Number of bytes read depends on indicated size of the buffer, meaning the
  buffer must be properly allocated before a call to this function.
}
procedure Stream_ReadMemoryBuffer(Stream: TStream; const Buff: TMemoryBuffer; Advance: Boolean = True);

//------------------------------------------------------------------------------
{
  Stream_LoadMemoryBuffer

  Reads user data and indicated size from the stream, (re)allocates the buffer
  as needed and then reads data into it.

  If buffer is invalid, it will be properly initialized.

  When Advance is true, the stream position is changed accordingly to number of
  bytes written, otherwise it is preserved.
}
procedure Stream_LoadMemoryBuffer(Stream: TStream; var Buff: TMemoryBuffer; Advance: Boolean = True);


{-------------------------------------------------------------------------------
    Memory Buffer - utility functions
-------------------------------------------------------------------------------}
{
  BufferBuild

  Builds a new buffer from provided memory location.

  Note that the provided pointer is directly incorporated into the buffer
  without any checking and without any copying, so make sure it is valid and
  do not free it after calling this function. Instead, leave the freeing on
  the buffer itself.
  This also means that the provided pointer is required to be previously
  allocated using standard memory management functions (GetMem, AllocMem,
  ReallocMem).

  If you want to create a buffer that contains a copy of provided data, not
  direct reference, use function BufferStore.
}
Function BufferBuild(Memory: Pointer; Size: TMemSize; UserData: PtrInt = 0): TMemoryBuffer;

//------------------------------------------------------------------------------
{
  BufferCompress

  Sets allocated size to indicated size when they differ, reallocating memory
  and moving data when necessary.

  Does nothing for invalid buffers.

  Returns true when a reallocation was performed, false when nothing was done
  (invalid buffer or there is no need for reallocation).
}
Function BufferCompress(var Buff: TMemoryBuffer): Boolean;

//------------------------------------------------------------------------------
{
  BufferClear

  Fills the buffer with zeroes.

    NOTE - fills only indicated size.

  Does nothing for invalid buffers.
}
procedure BufferClear(var Buff: TMemoryBuffer);


implementation

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

{===============================================================================
    Memory Buffer - implementation
===============================================================================}
const
  MB_CHECK_STRING = 'VALID';

{-------------------------------------------------------------------------------
    Memory Buffer - internal routines
-------------------------------------------------------------------------------}

Function BufferChecksum(const Buff: TMemoryBuffer): PtrUInt;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := PtrUInt(Buff.Signature) xor PtrUInt(Buff.Memory) xor not PtrUInt(Buff.Size) xor not PtrUInt(Buff.AllocSize);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function AdjustEndianity(Val: UInt64): UInt64;
{$IFDEF ENDIAN_BIG}
  Function SwapEndian(A: UInt32): UInt32;
  begin
    Result := UInt32(
      ((A and $000000FF) shl 24) or
      ((A and $0000FF00) shl 8) or
      ((A and $00FF0000) shr 8) or
      ((A and $FF000000) shr 24));
  end;

begin
Int64Rec(Val).Lo := SwapEndian(Int64Rec(Val).Hi);
Int64Rec(Val).Hi := SwapEndian(Int64Rec(Val).Lo);
{$ELSE}
begin
Result := Val;
{$ENDIF}
end;


{-------------------------------------------------------------------------------
    Memory Buffer - validity routines
-------------------------------------------------------------------------------}

Function BufferIsValid(const Buff: TMemoryBuffer): Boolean;
begin
Result := (Buff.Checksum = BufferChecksum(Buff)) and (Buff.CheckStr = MB_CHECK_STRING);
end;

//------------------------------------------------------------------------------

procedure BufferInit(out Buff: TMemoryBuffer);

  Function GetSignature: PtrUInt;
  begin
  {$IFDEF CPU64bit}
    Result := (PtrUInt(Random($10000)) shl 48) or (PtrUInt(Random($10000)) shl 32) or
              (PtrUInt(Random($10000)) shl 16) or PtrUInt(Random($10000)) ;
  {$ELSE}
    Result := (PtrUInt(Random($10000)) shl 16) or PtrUInt(Random($10000));
  {$ENDIF}
  end;

begin
Buff.Memory := nil;
Buff.Signature := GetSignature;
Buff.Size := 0;
Buff.AllocSize := 0;
Buff.UserData := 0;
Buff.Checksum := BufferChecksum(Buff);
Buff.CheckStr := MB_CHECK_STRING;
end;

//------------------------------------------------------------------------------

procedure BufferFinal(var Buff: TMemoryBuffer);
begin
If BufferIsValid(Buff) then
  BufferFree(Buff);
Buff.Signature := 0;
Buff.Checksum := PtrUInt(-1);
Buff.CheckStr := '';
end;


{-------------------------------------------------------------------------------
    Memory Buffer - allocation, freeing
-------------------------------------------------------------------------------}

procedure BufferGet(var Buff: TMemoryBuffer; Size: TMemSize);
begin
// if buffer was already used, free it, otherwise initialize it
If BufferIsValid(Buff) then
  BufferFree(Buff)
else
  BufferInit(Buff);
// allocate memory
If Size > 0 then
  GetMem(Buff.Memory,Size)
else
  Buff.Memory := nil;
// store size
Buff.Size := Size;
Buff.AllocSize := Size;
Buff.Checksum := BufferChecksum(Buff);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferGet(Size: TMemSize): TMemoryBuffer;
begin
BufferInit(Result);
BufferGet(Result,Size);
end;

//------------------------------------------------------------------------------

procedure BufferAlloc(var Buff: TMemoryBuffer; Size: TMemSize);
begin
BufferGet(Buff,Size);
FillChar(Buff.Memory^,Buff.AllocSize,0);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferAlloc(Size: TMemSize): TMemoryBuffer;
begin
BufferInit(Result);
BufferAlloc(Result,Size);
end;

//------------------------------------------------------------------------------

procedure BufferFree(var Buff: TMemoryBuffer);
begin
// if buffer is already initialized, free it, otherwise initialize it
If BufferIsValid(Buff) then
  begin
    If Buff.AllocSize > 0 then
      FreeMem(Buff.Memory,Buff.AllocSize);
    Buff.Memory := nil;
    Buff.Size := 0;
    Buff.AllocSize := 0;
    Buff.Checksum := BufferChecksum(Buff);
  end
else BufferInit(Buff);
end;

//------------------------------------------------------------------------------

procedure BufferRealloc(var Buff: TMemoryBuffer; NewSize: TMemSize; AllowShrink: Boolean = True);
begin
If BufferIsValid(Buff) then
  begin
    {
      reallocate only when new size is larger than current allocated size or
      shrinking is alloved, otherwise just change indicated size
    }
    If (NewSize > Buff.AllocSize) or AllowShrink then
      begin
        If NewSize <> 0 then
          begin
            If NewSize <> Buff.Size then
              begin
                ReallocMem(Buff.Memory,NewSize);
                Buff.AllocSize := NewSize;
                Buff.Size := NewSize;
              end;
          end
        else BufferFree(Buff);
      end
    else Buff.Size := NewSize;
    Buff.Checksum := BufferChecksum(Buff);
  end
else BufferAlloc(Buff,NewSize);
end;


{-------------------------------------------------------------------------------
    Memory Buffer - whole buffer manipulation
-------------------------------------------------------------------------------}

procedure BufferCopy(const Src: TMemoryBuffer; var Dest: TMemoryBuffer);
begin
If BufferIsValid(Src) then
  begin
    BufferRealloc(Dest,Src.Size); // allocates an uninitialized buffer
    Move(Src.Memory^,Dest.Memory^,Src.Size);
    Dest.UserData := Src.UserData;
    Dest.Checksum := BufferChecksum(Dest);
  end
else raise EMBInvalidBuffer.Create('BufferCopy: Invalid source buffer, cannot make a copy.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function BufferCopy(const Src: TMemoryBuffer): TMemoryBuffer;
begin
If BufferIsValid(Src) then
  begin
    Result := BufferGet(Src.Size);  // this will also init the result
    Move(Src.Memory^,Result.Memory^,Src.Size);
    Result.UserData := Src.UserData;
    Result.Checksum := BufferChecksum(Result);
  end
else raise EMBInvalidBuffer.Create('BufferCopy: Invalid source buffer, cannot make a copy.');
end;


{-------------------------------------------------------------------------------
    Memory Buffer - data access
-------------------------------------------------------------------------------}

Function BufferMemory(const Buff: TMemoryBuffer; Offset: TMemSize = 0): Pointer;
begin
If BufferIsValid(Buff) then
  begin
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    If Offset <= Buff.Size then 
      Result := Pointer(PtrUInt(Buff.Memory) + PtrUInt(Offset))
    else
      raise EMBInvalidValue.CreateFmt('BufferMemory: Offset (%p) out of allowed range.',[Pointer(Offset)]);
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  end
else Result := nil;
end;
 
//------------------------------------------------------------------------------

Function BufferSize(const Buff: TMemoryBuffer): TMemSize;
begin
If BufferIsValid(Buff) then
  Result := Buff.Size
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function BufferAllocSize(const Buff: TMemoryBuffer): TMemSize;
begin
If BufferIsValid(Buff) then
  Result := Buff.AllocSize
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function BufferGetUserData(const Buff: TMemoryBuffer): PtrInt;
begin
If BufferIsValid(Buff) then
  Result := Buff.UserData
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function BufferSetUserData(var Buff: TMemoryBuffer; UserData: PtrInt): PtrInt;
begin
If BufferIsValid(Buff) then
  begin
    Result := Buff.UserData;
    Buff.UserData := UserData;
  end
else Result := 0;
end;


{-------------------------------------------------------------------------------
    Memory Buffer - data read/write
-------------------------------------------------------------------------------}

procedure BufferStore(var Buff: TMemoryBuffer; const Src; Size: TMemSize);
begin
BufferRealloc(Buff,Size); // creates the buffer if it is invalid
Move(Src,Buff.Memory^,Size);
Buff.Checksum := BufferChecksum(Buff);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure BufferStore(var Buff: TMemoryBuffer; Src: Pointer; Size: TMemSize);
begin
BufferStore(Buff,Src^,Size);
end;

//------------------------------------------------------------------------------

procedure BufferWrite(var Buff: TMemoryBuffer; const Src; Count: TMemSize; Offset: TMemSize = 0);
begin
If BufferIsValid(Buff) then
  begin
    If (Offset + Count) > Buff.Size then
      BufferRealloc(Buff,Offset + Count);
  end
else BufferAlloc(Buff,Offset + Count);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Move(Src,Pointer(PtrUInt(Buff.Memory) + PtrUInt(Offset))^,Count);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure BufferRead(const Buff: TMemoryBuffer; out Dest; Count: TMemSize; Offset: TMemSize = 0);
begin
If BufferIsValid(Buff) then
  begin
    If Offset < Buff.Size then
      begin
        If (Offset + Count) <= Buff.Size then
        {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
          Move(Pointer(PtrUInt(Buff.Memory) + PtrUInt(Offset))^,Addr(Dest)^,Count)
        {$IFDEF FPCDWM}{$POP}{$ENDIF}
        else
          raise EMBInvalidValue.CreateFmt('BufferRead: Count (%u) too large.',[Count]);
      end
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    else raise EMBInvalidValue.CreateFmt('BufferRead: Offset (%p) out of allowed range.',[Pointer(Offset)]);
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  end
else raise EMBInvalidBuffer.Create('BufferRead: Invalid buffer, cannot read data.');
end;


{-------------------------------------------------------------------------------
    Memory Buffer - buffered data streaming
-------------------------------------------------------------------------------}

procedure Stream_WriteMemoryBuffer(Stream: TStream; const Buff: TMemoryBuffer; Advance: Boolean = True);
begin
If BufferIsValid(Buff) then
  begin
    If Buff.Size <> 0 then
      begin
        Stream.WriteBuffer(Buff.Memory^,Int64(Buff.Size));
        If not Advance then
          Stream.Seek(-Int64(Buff.Size),soCurrent);
      end;
  end
else raise EMBInvalidBuffer.Create('Stream_WriteMemoryBuffer: Invalid buffer.');
end;

//------------------------------------------------------------------------------

procedure Stream_SaveMemoryBuffer(Stream: TStream; const Buff: TMemoryBuffer; Advance: Boolean = True);
var
  InitPos:  Int64;
  Temp:     UInt64;
begin
If BufferIsValid(Buff) then
  begin
    InitPos := Stream.Position;
    // save metadata (do not use BinaryStreaming library)
    Temp := AdjustEndianity(UInt64(Buff.UserData));
    Stream.WriteBuffer(Temp,SizeOf(Temp));
    Temp := AdjustEndianity(UInt64(Buff.Size));
    Stream.WriteBuffer(Temp,SizeOf(Temp));
    // save content
    If Buff.Size <> 0 then
      Stream.WriteBuffer(Buff.Memory^,Int64(Buff.Size));
    If not Advance then
      Stream.Seek(InitPos,soBeginning);
  end
else raise EMBInvalidBuffer.Create('Stream_SaveMemoryBuffer: Invalid buffer.');
end;

//------------------------------------------------------------------------------

procedure Stream_ReadMemoryBuffer(Stream: TStream; const Buff: TMemoryBuffer; Advance: Boolean = True);
begin
If BufferIsValid(Buff) then
  begin
    If Buff.Size <> 0 then
      begin
        Stream.ReadBuffer(Buff.Memory^,Int64(Buff.Size));
        If not Advance then
          Stream.Seek(-Int64(Buff.Size),soCurrent);
      end;
  end
else raise EMBInvalidBuffer.Create('Stream_ReadMemoryBuffer: Invalid buffer.');
end;

//------------------------------------------------------------------------------

procedure Stream_LoadMemoryBuffer(Stream: TStream; var Buff: TMemoryBuffer; Advance: Boolean = True);
var
  InitPos:  Int64;
  UserData: UInt64;
  Size:     UInt64;
begin
InitPos := Stream.Position;
// read metadata
Stream.ReadBuffer(Addr(UserData)^,SizeOf(UserData));
Stream.ReadBuffer(Addr(Size)^,SizeOf(Size));
{$IFDEF ENDIAN_BIG}
UserData := AdjustEndianity(UserData);
Size := AdjustEndianity(Size);
{$ENDIF}
// prepare buffer
BufferRealloc(Buff,TMemSize(Size)); // this also initializes the buffer when needed
Buff.UserData := PtrInt(UserData);
// load content
If Buff.Size <> 0 then
  Stream.ReadBuffer(Buff.Memory^,Int64(Buff.Size));
If not Advance then
  Stream.Seek(InitPos,soBeginning);
end;


{-------------------------------------------------------------------------------
    Memory Buffer - utility functions
-------------------------------------------------------------------------------}

Function BufferBuild(Memory: Pointer; Size: TMemSize; UserData: PtrInt = 0): TMemoryBuffer;
begin
BufferInit(Result);
Result.Memory := Memory;
Result.Size := Size;
Result.AllocSize := Size;
Result.UserData := UserData;
Result.Checksum := BufferChecksum(Result);
end;

//------------------------------------------------------------------------------

Function BufferCompress(var Buff: TMemoryBuffer): Boolean;
begin
Result := False;
If BufferIsValid(Buff) then
  If (Buff.AllocSize <> 0) and (Buff.Size <> Buff.AllocSize) then
    begin
      ReallocMem(Buff.Memory,Buff.Size);
      Buff.AllocSize := Buff.Size;
      Buff.Checksum := BufferChecksum(Buff);
      Result := True;
    end;
end;

//------------------------------------------------------------------------------

procedure BufferClear(var Buff: TMemoryBuffer);
begin
If BufferIsValid(Buff) then
  If Buff.Size <> 0 then
    FillChar(Buff.Memory^,Buff.Size,0);
end;


{-------------------------------------------------------------------------------
    Memory Buffer - unit management
-------------------------------------------------------------------------------}

initialization
  Randomize;  // required for signatures generation 

end.
