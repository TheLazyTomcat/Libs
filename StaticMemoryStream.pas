{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Static memory stream

    Very simple classes intended to provide a way of accessing general memory
    location using usual streams.
    They are designed to be safe for use on pointers from external sources
    (libraries, OS, ...).

  Version 1.1.2 (2024-04-14)

  Last change 2024-04-14

  ©2017-2024 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.StaticMemoryStream

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect       - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol StaticMemoryStream_UseAuxExceptions for details).

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit StaticMemoryStream;
{
  StaticMemoryStream_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  StaticMemoryStream_UseAuxExceptions to achieve this.
}
{$IF Defined(StaticMemoryStream_UseAuxExceptions)}
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
  SysUtils, Classes,
  AuxTypes{$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

type
  ESMSException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  ESMSCannotWrite  = class(ESMSException);

{===============================================================================
--------------------------------------------------------------------------------
                              TStaticMemoryStream
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TStaticMemoryStream - declaration
===============================================================================}

type
  TStaticMemoryStream = class(TCustomMemoryStream)
  protected
    Function GetAddress: Pointer; virtual;
    procedure SetAddress(Value: Pointer); virtual;
  public
    constructor Create(Memory: Pointer; Size: TMemSize); overload;
    Function Write(const Buffer; Count: LongInt): LongInt; override;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: String); virtual;
    property Address: Pointer read GetAddress write SetAddress;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                          TWritableStaticMemoryStream
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TWritableStaticMemoryStream - declaration
===============================================================================}

  TWritableStaticMemoryStream = class(TStaticMemoryStream)
  public
    Function Write(const Buffer; Count: LongInt): LongInt; override;
  {
    LoadFromStream loads as many bytes as can fit into the internal static
    memory.
    If less bytes is loaded than is the size of internal memory, then any bytes
    in the internal memory above this count are left unchanged.
  }
    procedure LoadFromStream(Stream: TStream); override;
  end;

implementation

uses
  StrRect;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                              TStaticMemoryStream
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TStaticMemoryStream - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TStaticMemoryStream - protected methods
-------------------------------------------------------------------------------}

Function TStaticMemoryStream.GetAddress: Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(PtrUInt(Memory) + PtrUInt(Position));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TStaticMemoryStream.SetAddress(Value: Pointer);
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If PtrUInt(Value) <= PtrUInt(Memory) then
  Seek(0,soBeginning)
else If PtrUInt(Value) >= (PtrUInt(Memory) + PtrUInt(Size)) then
  Seek(0,soEnd)
else
  Seek(Int64(PtrUInt(Value) - PtrUInt(Memory)),soBeginning);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{-------------------------------------------------------------------------------
    TStaticMemoryStream - public methods
-------------------------------------------------------------------------------}

constructor TStaticMemoryStream.Create(Memory: Pointer; Size: TMemSize);
begin
inherited Create;
SetPointer(Memory,Size);
Position := 0;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TStaticMemoryStream.Write(const Buffer; Count: LongInt): LongInt;
begin
{$IFDEF FPC}
Result := 0;
{$ENDIF}
raise ESMSCannotWrite.Create('TStaticMemoryStream.Write: Write operation not allowed on static memory.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TStaticMemoryStream.LoadFromStream(Stream: TStream);
begin
raise ESMSCannotWrite.Create('TStaticMemoryStream.LoadFromStream: Cannot load into static memory.');
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TStaticMemoryStream.LoadFromFile(const FileName: String);
var
  FileStream: TFileStream;
begin
FileStream := TFileStream.Create(StrToRTL(FileName),fmOpenRead or fmShareDenyWrite);
try
  FileStream.Seek(0,soBeginning);
  LoadFromStream(FileStream);
finally
  FileStream.Free;
end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                          TWritableStaticMemoryStream
--------------------------------------------------------------------------------
===============================================================================}

{===============================================================================
    TWritableStaticMemoryStream - implementation
===============================================================================}

{-------------------------------------------------------------------------------
    TWritableStaticMemoryStream - public methods
-------------------------------------------------------------------------------}

Function TWritableStaticMemoryStream.Write(const Buffer; Count: LongInt): LongInt;
begin
If (Count > 0) and (Position >= 0) then
  begin
    Result := Size - Position;
    If Result > 0 then
      begin
        If Result > Count then
          Result := Count;
      {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
        Move(Buffer,Pointer(PtrUInt(Memory) + PtrUInt(Position))^,Result);
       Position := Position + Result;
      {$IFDEF FPCDWM}{$POP}{$ENDIF}
      end
    else Result := 0;
  end
else Result := 0;
end;

//------------------------------------------------------------------------------

procedure TWritableStaticMemoryStream.LoadFromStream(Stream: TStream);
begin
Stream.Seek(0,soBeginning);
If (Stream.Size - Stream.Position) > Size then
  Stream.ReadBuffer(Memory^,Size)
else
  Stream.ReadBuffer(Memory^,LongInt(Stream.Size - Stream.Position));
end;

end.
