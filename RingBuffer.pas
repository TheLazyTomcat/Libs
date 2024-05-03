{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Ring buffer

    Simple and naive implementation of general ring buffer, also known as
    circular buffer.

    General buffer (TRingBuffer) can be used for any data as it operates on
    bytes and pointers. But if you want to create a typed ring buffer, there
    is a class TTypedRingBuffer created for that purpose. It should not be used
    directly, it is provided only as a base for other typed buffers. Create
    its descendant and implement it for type you want.

    An integer ring buffer is implemented as a guideline for how to inherit
    from TTypedRingBuffer and create specialized ring buffers.

  Version 1.2.1 (2024-05-03)

  Last change 2024-05-03

  ©2018-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.RingBuffer

  Dependencies:
    AuxClasses    - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol RingBuffer_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit RingBuffer;
{
  RingBuffer_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  RingBuffer_UseAuxExceptions to achieve this.
}
{$IF Defined(RingBuffer_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND} 

//------------------------------------------------------------------------------

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH DuplicateLocals+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes, AuxClasses{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ERBException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ERBOverwriteError   = class(ERBException);
  ERBInvalidValue     = class(ERBException);
  ERBIndexOutOfBounds = class(ERBException);
  ERBWriteError       = class(ERBException);
  ERBReadError        = class(ERBException);
  ERBPeekError        = class(ERBException);

{===============================================================================
--------------------------------------------------------------------------------
                                   TRingBuffer
--------------------------------------------------------------------------------
===============================================================================}

type
  TOverwriteBehavior = (obOverwrite,obDrop,obError);

  TOverwriteEvent = procedure(Sender: TObject; Count: TMemSize) of object;
  TOverwriteCallback = procedure(Sender: TObject; Count: TMemSize);

{===============================================================================
    TRingBuffer - class declaration
===============================================================================}
type
  TRingBuffer = class(TCustomObject)
  protected
    fMemory:              Pointer;
    fSize:                TMemSize;
    fWritePtr:            Pointer;
    fReadPtr:             Pointer;
    fOverwriteBehavior:   TOverwriteBehavior;
    fIsEmpty:             Boolean;
    fOnOverwriteEvent:    TOverwriteEvent;
    fOnOverwriteCallback: TOverwriteCallback;
    procedure SetSize(Value: TMemSize); virtual;
    Function DoOverwrite(Count: TMemSize): Boolean; virtual;
  public
    constructor Create(Size: TMemSize);
    destructor Destroy; override;
    Function WriteBuff(const Buff; Count: TMemSize): TMemSize; virtual;
    Function WriteMem(Ptr: Pointer; Count: TMemSize): TMemSize; virtual;
    Function ReadBuff(out Buff; Count: TMemSize): TMemSize; virtual;
    Function ReadMem(Ptr: Pointer; Count: TMemSize): TMemSize; virtual;
    Function PeekBuff(out Buff; Count: TMemSize): TMemSize; virtual;
    Function PeekMem(Ptr: Pointer; Count: TMemSize): TMemSize; virtual;
    Function UsedSpace: TMemSize; virtual;
    Function FreeSpace: TMemSize; virtual;
    Function IsEmpty: Boolean; virtual;
    Function IsFull: Boolean; virtual;
    property Memory: Pointer read fMemory;
    property Size: TMemSize read fSize write SetSize;
    property WritePtr: Pointer read fWritePtr;
    property ReadPtr: Pointer read fReadPtr;
    property OverwriteBehavior: TOverwriteBehavior read fOverwriteBehavior write fOverwriteBehavior;
    property OnOverwriteEvent: TOverwriteEvent read fOnOverwriteEvent write fOnOverwriteEvent;
    property OnOverwriteCallback: TOverwriteCallback read fOnOverwriteCallback write fOnOverwriteCallback;
    property OnOverwrite: TOverwriteEvent read fOnOverwriteEvent write fOnOverwriteEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TTypedRingBuffer
--------------------------------------------------------------------------------
===============================================================================}
type
  TValueOverwriteEvent = procedure(Sender: TObject; Count: Integer) of object;
  TValueOverwriteCallback = procedure(Sender: TObject; Count: Integer);

{===============================================================================
    TTypedRingBuffer - class declaration
===============================================================================}
type
  TTypedRingBuffer = class(TRingBuffer)
  protected
    fBaseTypeSize:              TMemSize;
    fOnValueOverwriteEvent:     TValueOverwriteEvent;
    fOnValueOverwriteCallback:  TValueOverwriteCallback;
    procedure SetSize(Value: TMemSize); override;
    Function GetCount: Integer; virtual;
    procedure SetCount(Value: Integer); virtual;
    Function GetWriteIndex: Integer; virtual;
    Function GetReadIndex: Integer; virtual;
    Function DoOverwrite(Count: TMemSize): Boolean; override;
  public
    constructor Create(BaseTypeSize: TMemSize; Count: Integer);
    Function CheckIndex(Index: Integer): Boolean; virtual;
    Function WriteBuff(const Buff; Count: TMemSize): TMemSize; override;
    Function WriteMem(Ptr: Pointer; Count: TMemSize): TMemSize; override;
    Function ReadBuff(out Buff; Count: TMemSize): TMemSize; override;
    Function ReadMem(Ptr: Pointer; Count: TMemSize): TMemSize; override;
    Function PeekBuff(out Buff; Count: TMemSize): TMemSize; override;
    Function PeekMem(Ptr: Pointer; Count: TMemSize): TMemSize; override;
    Function UsedCount: Integer; virtual;
    Function FreeCount: Integer; virtual;
    property BaseTypeSize: TMemSize read fBaseTypeSize;
    property Count: Integer read GetCount write SetCount;
    property WriteIndex: Integer read GetWriteIndex;
    property ReadIndex: Integer read GetReadIndex;
    property OnValueOverwriteEvent: TValueOverwriteEvent read fOnValueOverwriteEvent write fOnValueOverwriteEvent;
    property OnValueOverwriteCallback: TValueOverwriteCallback read fOnValueOverwriteCallback write fOnValueOverwriteCallback;
    property OnValueOverwrite: TValueOverwriteEvent read fOnValueOverwriteEvent write fOnValueOverwriteEvent;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TIntegerRingBuffer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerRingBuffer - class declaration
===============================================================================}
type
  TIntegerRingBuffer = class(TTypedRingBuffer)
  protected
    Function GetValue(Index: Integer): Integer; virtual;
    procedure SetValue(Index: Integer; Value: Integer); virtual;
    Function GetUsedValue(Index: Integer): Integer; virtual;
    procedure SetUsedValue(Index: Integer; Value: Integer); virtual;
  public
    constructor Create(Count: Integer);
    Function CheckUsedIndex(Index: Integer): Boolean; virtual;
    procedure Write(Value: Integer); overload; virtual;
    Function Write(Values: PInteger; Count: Integer): Integer; overload; virtual;
    Function Write(Values: array of Integer): Integer; overload; virtual;
    Function Read: Integer; overload; virtual;
    Function Read(out Value: Integer): Boolean; overload; virtual;
    Function Read(Value: PInteger; Count: Integer): Integer; overload; virtual;
    Function Peek: Integer; overload; virtual;
    Function Peek(out Value: Integer): Boolean; overload; virtual;
    Function Peek(Value: PInteger; Count: Integer): Integer; overload; virtual;
    property Values[Index: Integer]: Integer read GetValue write SetValue; default;
  {
    Note that items in UsedValues array property are indexed from current read
    position (index 0) and there is UsedCount of them.

    To check validity of index, use method CheckUsedIndex, NOT CheckIndex.
  }
    property UsedValues[Index: Integer]: Integer read GetUsedValue write SetUsedValue;
  end;

implementation

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W4056:={$WARN 4056 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                   TRingBuffer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TRingBuffer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TRingBuffer - protected methods
-------------------------------------------------------------------------------}

procedure TRingBuffer.SetSize(Value: TMemSize);
var
  UsedSpaceBytes: TMemSize;
  NewMem:         Pointer;
begin
If Value > 0 then
  begin
    If Value <> fSize then
      begin
      {
        New value actually differs from current size, if they are equal then we
        don't have to do anything.

        If no data are buffered, then just free current allocation and create a
        new one. But...
      }
        If UsedSpace > 0 then
          begin
            // there are buffered data
            If Value > fSize then
              begin
              {
                The buffer will be enlarged.

                Allocate new memory and copy all the date there. Then free
                the current buffer. No need for any complexities - all data
                will fit into the new buffer since it is larger.
              }
                UsedSpaceBytes := UsedSpace;  // leave this here, do not move it up!
                NewMem := AllocMem(Value);
                ReadMem(NewMem,UsedSpaceBytes);
                FreeMem(fMemory,fSize);
                fMemory := NewMem;
                fSize := Value;
              {
                Because new size is larger than the old one, the UsedSpaceBytes
                cannot be larger or equal to new size, it must be smaller.
                Therefore the write pointer cannot be be put at the end, so
                no need to check for it.
              }
              {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                fWritePtr := Pointer(PtrUInt(fMemory) + PtrUInt(UsedSpaceBytes));
              {$IFDEF FPCDWM}{$POP}{$ENDIF}
                fReadPtr := fMemory;
              end
            else
              begin
              {
                The buffer will be shrinked.

                Allocate new memory, copy whatever data will fit into the new
                memory and free current buffer.

                Also signal overwrite if we will lose any data.
              }
                If UsedSpace > Value then
                  If not DoOverwrite(UsedSpace - Value) then Exit;
                UsedSpaceBytes := UsedSpace;  // used space could have changed, so it must be here
                NewMem := AllocMem(Value);
                If UsedSpaceBytes > Value then
                  begin
                  {
                    Stored data will not fit into the new memory.

                    Advance read pointer by the amount of bytes we will drop,
                    then read all remaining bytes into new memory, free current
                    memory and setup the buffer.
                  }
                  {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
                    fReadPtr := Pointer(PtrUInt(fReadPtr) + PtrUInt(UsedSpaceBytes - Value));
                    If PtrUInt(fReadPtr) >= (PtrUInt(fMemory) + PtrUInt(fSize)) then
                      fReadPtr := Pointer(PtrUInt(fReadPtr) - PtrUInt(fSize));
                  {$IFDEF FPCDWM}{$POP}{$ENDIF}
                    ReadMem(NewMem,Value);
                    FreeMem(fMemory,fSize);
                    fMemory := NewMem;
                    fSize := Value;
                    fWritePtr := fMemory;
                    fReadPtr := fMemory;
                  end
                else
                  begin
                    // stored data will fit into the new memory
                    ReadMem(NewMem,UsedSpaceBytes);
                    FreeMem(fMemory,fSize);
                    fMemory := NewMem;
                    fSize := Value;
                    If UsedSpaceBytes < fSize then
                    {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
                      fWritePtr := Pointer(PtrUInt(fMemory) + PtrUInt(UsedSpaceBytes))
                    else
                      fWritePtr := fMemory;
                    {$IFDEF FPCDWM}{$POP}{$ENDIF}
                    fReadPtr := fMemory;
                  end;
              end;
            fIsEmpty := False;
          end
        else
          begin
            // simple case, no data are buffered
            FreeMem(fMemory,fSize);
            fMemory := AllocMem(Value);
            fSize := Value;
            fWritePtr := fMemory;
            fReadPtr := fMemory;
            fIsEmpty := True;
          end;
      end;
  end
else raise ERBInvalidValue.CreateFmt('TRingBuffer.SetSize: Invalid size (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TRingBuffer.DoOverwrite(Count: TMemSize): Boolean;
begin
If Assigned(fOnOverwriteEvent) then
  fOnOverwriteEvent(Self,Count)
else If Assigned(fOnOverwriteCallback) then
  fOnOverwriteCallback(Self,Count);
case fOverwriteBehavior of
  obOverwrite:  Result := True;
  obDrop:       Result := False;
  obError:      raise ERBOverwriteError.CreateFmt('TRingBuffer: Overwriting %d bytes',[Count]);
else
  raise ERBInvalidValue.CreateFmt('TRingBuffer.DoOverwrite: Invalid overwrite behavior (%d).',[Ord(fOverwriteBehavior)]);
end;
end;

{-------------------------------------------------------------------------------
    TRingBuffer - public methods
-------------------------------------------------------------------------------}

constructor TRingBuffer.Create(Size: TMemSize);
begin
inherited Create;
If Size > 0 then
  begin
    fMemory := AllocMem(Size);
    fSize := Size;
  end
else raise ERBInvalidValue.CreateFmt('TRingBuffer.Create: Invalid size (%d).',[Size]);
fWritePtr := fMemory;
fReadPtr := fMemory;
fOverwriteBehavior := obOverwrite;
fIsEmpty := True;
end;

//------------------------------------------------------------------------------

destructor TRingBuffer.Destroy;
begin
FreeMem(fMemory,fSize);
inherited;
end;

//------------------------------------------------------------------------------

Function TRingBuffer.WriteBuff(const Buff; Count: TMemSize): TMemSize;
begin
Result := WriteMem(@Buff,Count);
end;

//------------------------------------------------------------------------------

Function TRingBuffer.WriteMem(Ptr: Pointer; Count: TMemSize): TMemSize;
var
  Overwrite:      Boolean;
  HighWriteSpace: TMemSize;
begin
Result := 0;
If Count > 0 then
  begin
    If Count < fSize then
      begin
        // all data can fit into the buffer
        Overwrite := Count > FreeSpace;
        If Overwrite then
          If not DoOverwrite(Count - FreeSpace) then Exit;
      {
        Get how much bytes can be written from write pointer up to the end of
        the buffer.
      }
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        HighWriteSpace := TMemSize(PtrUInt(fSize) - (PtrUInt(fWritePtr) - PtrUInt(fMemory)));
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        // will the data fit without splitting?
        If Count <= HighWriteSpace then
          begin
            // data will fit without splitting
            Move(Ptr^,fWritePtr^,Count);
            If Count < HighWriteSpace then
            {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
              fWritePtr := Pointer(PtrUInt(fWritePtr) + PtrUInt(Count))
            else
              fWritePtr := fMemory;
            {$IFDEF FPCDWM}{$POP}{$ENDIF}
            Result := Count;
          end
        else
          begin
          {
            Splitting is required.
            First write what can fit to the high write space, then write the
            rest to the beginning of the buffer.
          }
            Move(Ptr^,fWritePtr^,HighWriteSpace);
          {$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
            Move(Pointer(PtrUInt(Ptr) + PtrUInt(HighWriteSpace))^,fMemory^,Count - HighWriteSpace);
            fWritePtr := Pointer(PtrUInt(fMemory) + PtrUInt(Count - HighWriteSpace));
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
            Result := Count;
          end;
      end
    else
      begin
      {
        Data cannot fit into the buffer, or they exactly fill it.
        Drop everything that is currently written in the buffer and store only
        number of bytes from the end of passed data that can fit.
      }
        Overwrite := UsedSpace > 0;
        If Overwrite then
          If not DoOverwrite(UsedSpace) then Exit;
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        Move(Pointer(PtrUInt(Ptr) + PtrUInt(Count - fSize))^,fMemory^,fSize);
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
        fWritePtr := fMemory;
        fReadPtr := fMemory;
        Result := fSize;
      end;
    fIsEmpty := False;
    If Overwrite then
      fReadPtr := fWritePtr;
  end;
end;

//------------------------------------------------------------------------------

Function TRingBuffer.ReadBuff(out Buff; Count: TMemSize): TMemSize;
begin
Result := ReadMem(@Buff,Count);
end;

//------------------------------------------------------------------------------

Function TRingBuffer.ReadMem(Ptr: Pointer; Count: TMemSize): TMemSize;
var
  HighReadCount:  TMemSize;
  UsedSpaceBytes: TMemSize;
begin
Result := 0;
If (Count > 0) and not IsEmpty then
  begin
    // something is to be read and the buffer is not empty
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    HighReadCount := TMemSize(PtrUInt(fSize) - (PtrUInt(fReadPtr) - PtrUInt(fMemory)));
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
    UsedSpaceBytes := UsedSpace;
    If Count < UsedSpaceBytes then
      begin
        // only part of the buffered data will be consumed
        If Count > HighReadCount then
          begin
          {
            Data are broken into two parts - one from the read pointer up to
            the end of the buffer, and second from the start of the buffer.
          }
            Move(fReadPtr^,Ptr^,HighReadCount);
          {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
            Move(fMemory^,Pointer(PtrUInt(Ptr) + PtrUInt(HighReadCount))^,Count - HighReadCount);
            fReadPtr := Pointer(PtrUInt(fMemory) + PtrUInt(Count - HighReadCount));
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
          end
        else
          begin
            // All data being read are stored before the end of the buffer.
            Move(fReadPtr^,Ptr^,Count);
            If Count < HighReadCount then
            {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
              fReadPtr := Pointer(PtrUInt(fReadPtr) + PtrUInt(Count))
            else
              fReadPtr := fMemory;
            {$IFDEF FPCDWM}{$POP}{$ENDIF}
          end;
        Result := Count;
      end
    else
      begin
        // all stored bytes will be consumed
        If HighReadCount < UsedSpaceBytes then
          begin
            // Again, data are broken into two parts - see above for details.
            Move(fReadPtr^,Ptr^,HighReadCount);
          {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
            Move(fMemory^,Pointer(PtrUInt(Ptr) + PtrUInt(HighReadCount))^,UsedSpaceBytes - HighReadCount);
          {$IFDEF FPCDWM}{$POP}{$ENDIF}
          end
        else Move(fReadPtr^,Ptr^,UsedSpaceBytes);
        fWritePtr := fMemory;
        fReadPtr := fMemory;
        fIsEmpty := True;
        Result := UsedSpaceBytes;
      end;
  end;
end;

//------------------------------------------------------------------------------

Function TRingBuffer.PeekBuff(out Buff; Count: TMemSize): TMemSize;
begin
Result := PeekMem(@Buff,Count);
end;

//------------------------------------------------------------------------------

Function TRingBuffer.PeekMem(Ptr: Pointer; Count: TMemSize): TMemSize;
var
  WritePtrTemp: Pointer;
  ReadPtrTemp:  Pointer;
  IsEmptyTemp:  Boolean;
begin
{
  Let's not write almost the same code as in read...

  Normally read the data, which in itself is not affecting them, and then just
  restore changed properties to a state before reading.
}
WritePtrTemp := fWritePtr;
ReadPtrTemp := fReadPtr;
IsEmptyTemp := fIsEmpty;
try
  Result := ReadMem(Ptr,Count);
finally
  fWritePtr := WritePtrTemp;
  fReadPtr := ReadPtrTemp;
  fIsEmpty := IsEmptyTemp;
end;
end;

//------------------------------------------------------------------------------

Function TRingBuffer.UsedSpace: TMemSize;
begin
If fWritePtr <> fReadPtr then
  begin
  {
    Raad and write pointers differ.

    If write pointer is above read pointer, it means the data are stored between
    read pointer (lowed bound) and write pointer (upped bound).

    If write pointer is below read pointer, it indicates that data are stored
    from read pointer to the end of the buffer, and they then continue from
    the start of the buffer up to the write pointer.
  }
  {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
    If PtrUInt(fWritePtr) > PtrUInt(fReadPtr) then
      Result := TMemSize(PtrUInt(fWritePtr) - PtrUInt(fReadPtr))
    else
      Result := TMemSize(PtrUInt(fSize) - (PtrUInt(fReadPtr) - PtrUInt(fWritePtr)));
  {$IFDEF FPCDWM}{$POP}{$ENDIF}
  end
else
  begin
  {
    Read and write pointers are at the same position, it means the buffer is
    either full, or completely empty.
  }
    If fIsEmpty then
      Result := 0
    else
      Result := fSize;
  end;
end;

//------------------------------------------------------------------------------

Function TRingBuffer.FreeSpace: TMemSize;
begin
Result := fSize - UsedSpace;
end;

//------------------------------------------------------------------------------

Function TRingBuffer.IsEmpty: Boolean;
begin
Result := UsedSpace <= 0;
end;

//------------------------------------------------------------------------------

Function TRingBuffer.IsFull: Boolean;
begin
Result := UsedSpace >= fSize;
end;

{===============================================================================
--------------------------------------------------------------------------------
                                TTypedRingBuffer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TTypedRingBuffer - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TTypedRingBuffer - protected methods
-------------------------------------------------------------------------------}

procedure TTypedRingBuffer.SetSize(Value: TMemSize);
begin
If Value mod fBaseTypeSize = 0 then
  inherited SetSize(Value)
else
  raise ERBInvalidValue.CreateFmt('TTypedRingBuffer.SetSize: Invalid size (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.GetCount: Integer;
begin
Result := Integer(fSize div fBaseTypeSize);
end;

//------------------------------------------------------------------------------

procedure TTypedRingBuffer.SetCount(Value: Integer);
begin
If Value > 0 then
  SetSize(TMemSize(Value) * fBaseTypeSize)
else
  raise ERBInvalidValue.CreateFmt('TTypedRingBuffer.SetCount: Invalid count (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.GetWriteIndex: Integer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Integer((PtrUInt(fWritePtr) - PtrUInt(fMemory)) div fBaseTypeSize);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.GetReadIndex: Integer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Integer((PtrUInt(fReadPtr) - PtrUInt(fMemory)) div fBaseTypeSize);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.DoOverwrite(Count: TMemSize): Boolean;
begin
If Assigned(fOnValueOverwriteEvent) then
  fOnValueOverwriteEvent(Self,Count div fBaseTypeSize)
else If Assigned(fOnValueOverwriteCallback) then
  fOnValueOverwriteCallback(Self,Count div fBaseTypeSize);
Result := inherited DoOverwrite(Count);
end;

{-------------------------------------------------------------------------------
    TTypedRingBuffer - public methods
-------------------------------------------------------------------------------}

constructor TTypedRingBuffer.Create(BaseTypeSize: TMemSize; Count: Integer);
begin
If Count > 0 then
  begin
    If BaseTypeSize > 0 then
      begin
        inherited Create(TMemSize(Count) * BaseTypeSize);
        fBaseTypeSize := BaseTypeSize;
      end
    else raise ERBInvalidValue.CreateFmt('TTypedRingBuffer.Create: Invalid base type size (%d)',[BaseTypeSize]);
  end
else raise ERBInvalidValue.CreateFmt('TTypedRingBuffer.Create: Invalid count (%d)',[Count]);
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.WriteBuff(const Buff; Count: TMemSize): TMemSize;
begin
Result := inherited WriteBuff(Buff,Count);
If Result mod fBaseTypeSize <> 0 then
  raise ERBWriteError.Create('TTypedRingBuffer.WriteBuff: Write error.');
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.WriteMem(Ptr: Pointer; Count: TMemSize): TMemSize;
begin
Result := inherited WriteMem(Ptr,Count);
If Result mod fBaseTypeSize <> 0 then
  raise ERBWriteError.Create('TTypedRingBuffer.WriteMem: Write error.');
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.ReadBuff(out Buff; Count: TMemSize): TMemSize;
begin
Result := inherited ReadBuff(Buff,Count);
If Result mod fBaseTypeSize <> 0 then
  raise ERBReadError.Create('TTypedRingBuffer.ReadBuff: Read error.');
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.ReadMem(Ptr: Pointer; Count: TMemSize): TMemSize;
begin
Result := inherited ReadMem(Ptr,Count);
If Result mod fBaseTypeSize <> 0 then
  raise ERBReadError.Create('TTypedRingBuffer.ReadMem: Read error.');
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.PeekBuff(out Buff; Count: TMemSize): TMemSize;
begin
Result := inherited PeekBuff(Buff,Count);
If Result mod fBaseTypeSize <> 0 then
  raise ERBPeekError.Create('TTypedRingBuffer.PeekBuff: Peek error.');
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.PeekMem(Ptr: Pointer; Count: TMemSize): TMemSize;
begin
Result := inherited PeekMem(Ptr,Count);
If Result mod fBaseTypeSize <> 0 then
  raise ERBPeekError.Create('TTypedRingBuffer.PeekMem: Peek error.');
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= 0) and (Index < Count);
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.UsedCount: Integer;
begin
Result := UsedSpace div fBaseTypeSize;
end;

//------------------------------------------------------------------------------

Function TTypedRingBuffer.FreeCount: Integer;
begin
Result := FreeSpace div fBaseTypeSize;
end;

{===============================================================================
--------------------------------------------------------------------------------
                               TIntegerRingBuffer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TIntegerRingBuffer - class declaration
===============================================================================}
{-------------------------------------------------------------------------------
    TIntegerRingBuffer - protected methods
-------------------------------------------------------------------------------}

Function TIntegerRingBuffer.GetValue(Index: Integer): Integer;
begin
If CheckIndex(Index) then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  Result := PInteger(PtrUInt(fMemory) + (PtrUInt(Index) * PtrUInt(fBaseTypeSize)))^
{$IFDEF FPCDWM}{$POP}{$ENDIF}
else
  raise ERBIndexOutOfBounds.CreateFmt('TIntegerRingBuffer.GetValue: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TIntegerRingBuffer.SetValue(Index: Integer; Value: Integer);
begin
If CheckIndex(Index) then
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  PInteger(PtrUInt(fMemory) + (PtrUInt(Index) * PtrUInt(fBaseTypeSize)))^ := Value
{$IFDEF FPCDWM}{$POP}{$ENDIF}
else
  raise ERBIndexOutOfBounds.CreateFmt('TIntegerRingBuffer.SetValue: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TIntegerRingBuffer.GetUsedValue(Index: Integer): Integer;
begin
If CheckUsedIndex(Index) then
  Result := GetValue((ReadIndex + Index) mod Count)
else
  raise ERBIndexOutOfBounds.CreateFmt('TIntegerRingBuffer.GetUsedValue: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TIntegerRingBuffer.SetUsedValue(Index: Integer; Value: Integer);
begin
If CheckUsedIndex(Index) then
  SetValue((ReadIndex + Index) mod Count,Value)
else
  raise ERBIndexOutOfBounds.CreateFmt('TIntegerRingBuffer.SetUsedValue: Index (%d) out of bounds.',[Index]);
end;

{-------------------------------------------------------------------------------
    TIntegerRingBuffer - public methods
-------------------------------------------------------------------------------}

constructor TIntegerRingBuffer.Create(Count: Integer);
begin
inherited Create(SizeOf(Integer),Count);
end;

//------------------------------------------------------------------------------

Function TIntegerRingBuffer.CheckUsedIndex(Index: Integer): Boolean;
begin
Result := (Index >= 0) and (Index < UsedCount);
end;

//------------------------------------------------------------------------------

procedure TIntegerRingBuffer.Write(Value: Integer);
begin
WriteBuff(Value,fBaseTypeSize); // checks are done in the ancestor class
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TIntegerRingBuffer.Write(Values: PInteger; Count: Integer): Integer;
begin
Result := WriteMem(Values,TMemSize(Count) * fBaseTypeSize) div fBaseTypeSize;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TIntegerRingBuffer.Write(Values: array of Integer): Integer;
begin
If Length(Values) > 0 then
  Result := Write(Addr(Values[Low(Values)]),Length(Values))
else
  Result := 0;
end;

//------------------------------------------------------------------------------

Function TIntegerRingBuffer.Read: Integer;
begin
If ReadBuff(Result,fBaseTypeSize) <> fBaseTypeSize then
  raise ERBReadError.Create('TTypedRingBuffer.Read: Read error.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TIntegerRingBuffer.Read(out Value: Integer): Boolean;
begin
Result := ReadBuff(Value,fBaseTypeSize) = fBaseTypeSize;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TIntegerRingBuffer.Read(Value: PInteger; Count: Integer): Integer;
begin
Result := ReadMem(Value,TMemSize(Count) * fBaseTypeSize) div fBaseTypeSize;
end;

//------------------------------------------------------------------------------

Function TIntegerRingBuffer.Peek: Integer;
begin
If PeekBuff(Result,fBaseTypeSize) <> fBaseTypeSize then
  raise ERBPeekError.Create('TTypedRingBuffer.Peek: Read error.');
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TIntegerRingBuffer.Peek(out Value: Integer): Boolean;
begin
Result := PeekBuff(Value,fBaseTypeSize) = fBaseTypeSize;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TIntegerRingBuffer.Peek(Value: PInteger; Count: Integer): Integer;
begin
Result := PeekMem(Value,TMemSize(Count) * fBaseTypeSize) div fBaseTypeSize;
end;

end.
