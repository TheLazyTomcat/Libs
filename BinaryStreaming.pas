{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Binary streaming

    Set of functions and classes designed to help writing and reading of binary
    data into/from streams and memory locations.

    All primitives larger than one byte (integers, floats, wide characters -
    including in UTF-16 strings, ...) are always written with little endiannes.

    Boolean data are written as one byte, where zero represents false and any
    non-zero value represents true.

    All strings are written with explicit length, and without terminating zero
    character. First a length is stored, followed immediately by the character
    stream.

    Short strings are stored with 8bit unsigned integer length, all other
    strings are stored with 32bit signed integer length (note that the length
    of 0 or lower marks an empty string, such length is not followed by any
    character belonging to that particular string). Length is in characters,
    not bytes or code points.

    Default type Char is always stored as 16bit unsigned integer, irrespective
    of how it is declared.

    Default type String is always stored as UTF-8 encoded string, again
    irrespective of how it is declared.

    Buffers and array of bytes are both stored as plain byte streams, without
    explicit size.

    Parameter Advance in writing and reading functions indicates whether the
    position in stream being written to or read from (or the passed memory
    pointer) can be advanced by number of bytes written or read. When set to
    true, the position (pointer) is advanced, when false, the position is the
    same after the call as was before it.

    Return value of almost all reading and writing functions is number of bytes
    written or read. The exception to this are read functions that are directly
    returning the value being read.

  Version 1.8 (2021-03-04)

  Last change 2022-09-13

  ©2015-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.BinaryStreaming

  Dependencies:
    AuxTypes   - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses - github.com/TheLazyTomcat/Lib.AuxClasses
    StrRect    - github.com/TheLazyTomcat/Lib.StrRect

===============================================================================}
unit BinaryStreaming;

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes,
  AuxTypes, AuxClasses;

type
  EBSException = class(Exception);

  EBSIndexOutOfBounds = class(EBSException);

{===============================================================================
--------------------------------------------------------------------------------
                               Allocation helpers
--------------------------------------------------------------------------------
===============================================================================}
{
  Following functions are returning number of bytes that are required to store
  a given object. They are here mainly to ease allocation when streaming into
  memory.
  Basic types have static size (nevertheless for completeness sake they are
  included), but some strings might be a little tricky (because of implicit
  conversion and explicitly stored size).
}
Function StreamedSize_Bool: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_Boolean: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function StreamedSize_Int8: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UInt8: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_Int16: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UInt16: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_Int32: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UInt32: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_Int64: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UInt64: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function StreamedSize_Float32: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_Float64: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_Float80: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_DateTime: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_Currency: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function StreamedSize_AnsiChar: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UTF8Char: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_WideChar: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UnicodeChar: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UCS4Char: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_Char: TMemSize;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function StreamedSize_ShortString(const Str: ShortString): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_AnsiString(const Str: AnsiString): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UTF8String(const Str: UTF8String): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_WideString(const Str: WideString): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UnicodeString(const Str: UnicodeString): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_UCS4String(const Str: UCS4String): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_String(const Str: String): TMemSize;

//------------------------------------------------------------------------------

Function StreamedSize_Buffer(Size: TMemSize): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}
Function StreamedSize_Bytes(Count: TMemSize): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                 Memory writing
--------------------------------------------------------------------------------
===============================================================================}

Function Ptr_WriteBool(var Dest: Pointer; Value: ByteBool; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteBool(Dest: Pointer; Value: ByteBool): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteBoolean(var Dest: Pointer; Value: Boolean; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_WriteBoolean(Dest: Pointer; Value: Boolean): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteInt8(var Dest: Pointer; Value: Int8; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteInt8(Dest: Pointer; Value: Int8): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUInt8(var Dest: Pointer; Value: UInt8; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUInt8(Dest: Pointer; Value: UInt8): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteInt16(var Dest: Pointer; Value: Int16; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteInt16(Dest: Pointer; Value: Int16): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUInt16(var Dest: Pointer; Value: UInt16; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUInt16(Dest: Pointer; Value: UInt16): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteInt32(var Dest: Pointer; Value: Int32; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteInt32(Dest: Pointer; Value: Int32): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUInt32(var Dest: Pointer; Value: UInt32; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUInt32(Dest: Pointer; Value: UInt32): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteInt64(var Dest: Pointer; Value: Int64; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteInt64(Dest: Pointer; Value: Int64): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUInt64(var Dest: Pointer; Value: UInt64; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUInt64(Dest: Pointer; Value: UInt64): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteFloat32(var Dest: Pointer; Value: Float32; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteFloat32(Dest: Pointer; Value: Float32): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteFloat64(var Dest: Pointer; Value: Float64; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteFloat64(Dest: Pointer; Value: Float64): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteFloat80(var Dest: Pointer; Value: Float80; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteFloat80(Dest: Pointer; Value: Float80): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteDateTime(var Dest: Pointer; Value: TDateTime; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_WriteDateTime(Dest: Pointer; Value: TDateTime): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteCurrency(var Dest: Pointer; Value: Currency; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteCurrency(Dest: Pointer; Value: Currency): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteAnsiChar(var Dest: Pointer; Value: AnsiChar; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteAnsiChar(Dest: Pointer; Value: AnsiChar): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUTF8Char(var Dest: Pointer; Value: UTF8Char; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUTF8Char(Dest: Pointer; Value: UTF8Char): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteWideChar(var Dest: Pointer; Value: WideChar; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteWideChar(Dest: Pointer; Value: WideChar): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUnicodeChar(var Dest: Pointer; Value: UnicodeChar; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUnicodeChar(Dest: Pointer; Value: UnicodeChar): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUCS4Char(var Dest: Pointer; Value: UCS4Char; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUCS4Char(Dest: Pointer; Value: UCS4Char): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteChar(var Dest: Pointer; Value: Char; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_WriteChar(Dest: Pointer; Value: Char): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteShortString(var Dest: Pointer; const Str: ShortString; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteShortString(Dest: Pointer; const Str: ShortString): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteAnsiString(var Dest: Pointer; const Str: AnsiString; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteAnsiString(Dest: Pointer; const Str: AnsiString): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUTF8String(var Dest: Pointer; const Str: UTF8String; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUTF8String(Dest: Pointer; const Str: UTF8String): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteWideString(var Dest: Pointer; const Str: WideString; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteWideString(Dest: Pointer; const Str: WideString): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUnicodeString(var Dest: Pointer; const Str: UnicodeString; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUnicodeString(Dest: Pointer; const Str: UnicodeString): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteUCS4String(var Dest: Pointer; const Str: UCS4String; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUCS4String(Dest: Pointer; const Str: UCS4String): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_WriteString(var Dest: Pointer; const Str: String; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_WriteString(Dest: Pointer; const Str: String): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteBuffer(var Dest: Pointer; const Buffer; Size: TMemSize; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteBuffer(Dest: Pointer; const Buffer; Size: TMemSize): TMemSize; overload;

//------------------------------------------------------------------------------

Function Ptr_WriteBytes(var Dest: Pointer; const Value: array of UInt8; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteBytes(Dest: Pointer; const Value: array of UInt8): TMemSize; overload;

//------------------------------------------------------------------------------

Function Ptr_FillBytes(var Dest: Pointer; Count: TMemSize; Value: UInt8; Advance: Boolean): TMemSize; overload;
Function Ptr_FillBytes(Dest: Pointer; Count: TMemSize; Value: UInt8): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                 Memory reading
--------------------------------------------------------------------------------
===============================================================================}

Function Ptr_ReadBool(var Src: Pointer; out Value: ByteBool; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadBool(Src: Pointer; out Value: ByteBool): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadBool(var Src: Pointer; Advance: Boolean): ByteBool; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadBool(Src: Pointer): ByteBool; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadBoolean(var Src: Pointer; out Value: Boolean; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadBoolean(Src: Pointer; out Value: Boolean): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadInt8(var Src: Pointer; out Value: Int8; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadInt8(Src: Pointer; out Value: Int8): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadInt8(var Src: Pointer; Advance: Boolean): Int8; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadInt8(Src: Pointer): Int8; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUInt8(var Src: Pointer; out Value: UInt8; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUInt8(Src: Pointer; out Value: UInt8): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUInt8(var Src: Pointer; Advance: Boolean): UInt8; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUInt8(Src: Pointer): UInt8; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadInt16(var Src: Pointer; out Value: Int16; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadInt16(Src: Pointer; out Value: Int16): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadInt16(var Src: Pointer; Advance: Boolean): Int16; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadInt16(Src: Pointer): Int16; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUInt16(var Src: Pointer; out Value: UInt16; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUInt16(Src: Pointer; out Value: UInt16): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUInt16(var Src: Pointer; Advance: Boolean): UInt16; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUInt16(Src: Pointer): UInt16; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadInt32(var Src: Pointer; out Value: Int32; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadInt32(Src: Pointer; out Value: Int32): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadInt32(var Src: Pointer; Advance: Boolean): Int32; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadInt32(Src: Pointer): Int32; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUInt32(var Src: Pointer; out Value: UInt32; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUInt32(Src: Pointer; out Value: UInt32): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUInt32(var Src: Pointer; Advance: Boolean): UInt32; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUInt32(Src: Pointer): UInt32; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadInt64(var Src: Pointer; out Value: Int64; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadInt64(Src: Pointer; out Value: Int64): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadInt64(var Src: Pointer; Advance: Boolean): Int64; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadInt64(Src: Pointer): Int64; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUInt64(var Src: Pointer; out Value: UInt64; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUInt64(Src: Pointer; out Value: UInt64): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUInt64(var Src: Pointer; Advance: Boolean): UInt64; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUInt64(Src: Pointer): UInt64; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadFloat32(var Src: Pointer; out Value: Float32; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadFloat32(Src: Pointer; out Value: Float32): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadFloat32(var Src: Pointer; Advance: Boolean): Float32; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadFloat32(Src: Pointer): Float32; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadFloat64(var Src: Pointer; out Value: Float64; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadFloat64(Src: Pointer; out Value: Float64): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadFloat64(var Src: Pointer; Advance: Boolean): Float64; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadFloat64(Src: Pointer): Float64; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadFloat80(var Src: Pointer; out Value: Float80; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadFloat80(Src: Pointer; out Value: Float80): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadFloat80(var Src: Pointer; Advance: Boolean): Float80; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadFloat80(Src: Pointer): Float80; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadDateTime(var Src: Pointer; out Value: TDateTime; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadDateTime(Src: Pointer; out Value: TDateTime): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadDateTime(var Src: Pointer; Advance: Boolean): TDateTime; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadDateTime(Src: Pointer): TDateTime; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadCurrency(var Src: Pointer; out Value: Currency; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadCurrency(Src: Pointer; out Value: Currency): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadCurrency(var Src: Pointer; Advance: Boolean): Currency; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadCurrency(Src: Pointer): Currency; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadAnsiChar(var Src: Pointer; out Value: AnsiChar; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadAnsiChar(Src: Pointer; out Value: AnsiChar): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadAnsiChar(var Src: Pointer; Advance: Boolean): AnsiChar; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadAnsiChar(Src: Pointer): AnsiChar; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUTF8Char(var Src: Pointer; out Value: UTF8Char; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUTF8Char(Src: Pointer; out Value: UTF8Char): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUTF8Char(var Src: Pointer; Advance: Boolean): UTF8Char; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUTF8Char(Src: Pointer): UTF8Char; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadWideChar(var Src: Pointer; out Value: WideChar; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadWideChar(Src: Pointer; out Value: WideChar): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadWideChar(var Src: Pointer; Advance: Boolean): WideChar; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadWideChar(Src: Pointer): WideChar; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUnicodeChar(var Src: Pointer; out Value: UnicodeChar; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUnicodeChar(Src: Pointer; out Value: UnicodeChar): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUnicodeChar(var Src: Pointer; Advance: Boolean): UnicodeChar; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUnicodeChar(Src: Pointer): UnicodeChar; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUCS4Char(var Src: Pointer; out Value: UCS4Char; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUCS4Char(Src: Pointer; out Value: UCS4Char): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUCS4Char(var Src: Pointer; Advance: Boolean): UCS4Char; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUCS4Char(Src: Pointer): UCS4Char; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadChar(var Src: Pointer; out Value: Char; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadChar(Src: Pointer; out Value: Char): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadChar(var Src: Pointer; Advance: Boolean): Char; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadChar(Src: Pointer): Char; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadShortString(var Src: Pointer; out Str: ShortString; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadShortString(Src: Pointer; out Str: ShortString): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadShortString(var Src: Pointer; Advance: Boolean): ShortString; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadShortString(Src: Pointer): ShortString; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadAnsiString(var Src: Pointer; out Str: AnsiString; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadAnsiString(Src: Pointer; out Str: AnsiString): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadAnsiString(var Src: Pointer; Advance: Boolean): AnsiString; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadAnsiString(Src: Pointer): AnsiString; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUTF8String(var Src: Pointer; out Str: UTF8String; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUTF8String(Src: Pointer; out Str: UTF8String): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUTF8String(var Src: Pointer; Advance: Boolean): UTF8String; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUTF8String(Src: Pointer): UTF8String; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadWideString(var Src: Pointer; out Str: WideString; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadWideString(Src: Pointer; out Str: WideString): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadWideString(var Src: Pointer; Advance: Boolean): WideString; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadWideString(Src: Pointer): WideString; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUnicodeString(var Src: Pointer; out Str: UnicodeString; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUnicodeString(Src: Pointer; out Str: UnicodeString): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUnicodeString(var Src: Pointer; Advance: Boolean): UnicodeString; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUnicodeString(Src: Pointer): UnicodeString; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadUCS4String(var Src: Pointer; out Str: UCS4String; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUCS4String(Src: Pointer; out Str: UCS4String): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUCS4String(var Src: Pointer; Advance: Boolean): UCS4String; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadUCS4String(Src: Pointer): UCS4String; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Ptr_ReadString(var Src: Pointer; out Str: String; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadString(Src: Pointer; out Str: String): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadString(var Src: Pointer; Advance: Boolean): String; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadString(Src: Pointer): String; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadBuffer(var Src: Pointer; var Buffer; Size: TMemSize; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadBuffer(Src: Pointer; var Buffer; Size: TMemSize): TMemSize; overload;


{===============================================================================
--------------------------------------------------------------------------------
                                 Stream writing
--------------------------------------------------------------------------------
===============================================================================}

Function Stream_WriteBool(Stream: TStream; Value: ByteBool; Advance: Boolean = True): TMemSize;

Function Stream_WriteBoolean(Stream: TStream; Value: Boolean; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Stream_WriteInt8(Stream: TStream; Value: Int8; Advance: Boolean = True): TMemSize;

Function Stream_WriteUInt8(Stream: TStream; Value: UInt8; Advance: Boolean = True): TMemSize;

Function Stream_WriteInt16(Stream: TStream; Value: Int16; Advance: Boolean = True): TMemSize;

Function Stream_WriteUInt16(Stream: TStream; Value: UInt16; Advance: Boolean = True): TMemSize;

Function Stream_WriteInt32(Stream: TStream; Value: Int32; Advance: Boolean = True): TMemSize;

Function Stream_WriteUInt32(Stream: TStream; Value: UInt32; Advance: Boolean = True): TMemSize;

Function Stream_WriteInt64(Stream: TStream; Value: Int64; Advance: Boolean = True): TMemSize;

Function Stream_WriteUInt64(Stream: TStream; Value: UInt64; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteFloat32(Stream: TStream; Value: Float32; Advance: Boolean = True): TMemSize;

Function Stream_WriteFloat64(Stream: TStream; Value: Float64; Advance: Boolean = True): TMemSize;

Function Stream_WriteFloat80(Stream: TStream; Value: Float80; Advance: Boolean = True): TMemSize;

Function Stream_WriteDateTime(Stream: TStream; Value: TDateTime; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_WriteCurrency(Stream: TStream; Value: Currency; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteAnsiChar(Stream: TStream; Value: AnsiChar; Advance: Boolean = True): TMemSize;

Function Stream_WriteUTF8Char(Stream: TStream; Value: UTF8Char; Advance: Boolean = True): TMemSize;

Function Stream_WriteWideChar(Stream: TStream; Value: WideChar; Advance: Boolean = True): TMemSize;

Function Stream_WriteUnicodeChar(Stream: TStream; Value: UnicodeChar; Advance: Boolean = True): TMemSize;

Function Stream_WriteUCS4Char(Stream: TStream; Value: UCS4Char; Advance: Boolean = True): TMemSize;

Function Stream_WriteChar(Stream: TStream; Value: Char; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Stream_WriteShortString(Stream: TStream; const Str: ShortString; Advance: Boolean = True): TMemSize;

Function Stream_WriteAnsiString(Stream: TStream; const Str: AnsiString; Advance: Boolean = True): TMemSize;

Function Stream_WriteUTF8String(Stream: TStream; const Str: UTF8String; Advance: Boolean = True): TMemSize;

Function Stream_WriteWideString(Stream: TStream; const Str: WideString; Advance: Boolean = True): TMemSize;

Function Stream_WriteUnicodeString(Stream: TStream; const Str: UnicodeString; Advance: Boolean = True): TMemSize;

Function Stream_WriteUCS4String(Stream: TStream; const Str: UCS4String; Advance: Boolean = True): TMemSize;

Function Stream_WriteString(Stream: TStream; const Str: String; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Stream_WriteBuffer(Stream: TStream; const Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteBytes(Stream: TStream; const Value: array of UInt8; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_FillBytes(Stream: TStream; Count: TMemSize; Value: UInt8; Advance: Boolean = True): TMemSize;

{===============================================================================
--------------------------------------------------------------------------------
                                 Stream reading
--------------------------------------------------------------------------------
===============================================================================}

Function Stream_ReadBool(Stream: TStream; out Value: ByteBool; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadBool(Stream: TStream; Advance: Boolean = True): ByteBool; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadBoolean(Stream: TStream; out Value: Boolean; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_ReadInt8(Stream: TStream; out Value: Int8; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadInt8(Stream: TStream; Advance: Boolean = True): Int8; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUInt8(Stream: TStream; out Value: UInt8; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUInt8(Stream: TStream; Advance: Boolean = True): UInt8; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadInt16(Stream: TStream; out Value: Int16; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadInt16(Stream: TStream; Advance: Boolean = True): Int16; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUInt16(Stream: TStream; out Value: UInt16; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUInt16(Stream: TStream; Advance: Boolean = True): UInt16; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadInt32(Stream: TStream; out Value: Int32; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadInt32(Stream: TStream; Advance: Boolean = True): Int32; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUInt32(Stream: TStream; out Value: UInt32; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUInt32(Stream: TStream; Advance: Boolean = True): UInt32; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadInt64(Stream: TStream; out Value: Int64; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadInt64(Stream: TStream; Advance: Boolean = True): Int64; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUInt64(Stream: TStream; out Value: UInt64; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUInt64(Stream: TStream; Advance: Boolean = True): UInt64; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadFloat32(Stream: TStream; out Value: Float32; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadFloat32(Stream: TStream; Advance: Boolean = True): Float32; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadFloat64(Stream: TStream; out Value: Float64; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadFloat64(Stream: TStream; Advance: Boolean = True): Float64; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadFloat80(Stream: TStream; out Value: Float80; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadFloat80(Stream: TStream; Advance: Boolean = True): Float80; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadDateTime(Stream: TStream; out Value: TDateTime; Advance: Boolean = True): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Stream_ReadDateTime(Stream: TStream; Advance: Boolean = True): TDateTime; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadCurrency(Stream: TStream; out Value: Currency; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadCurrency(Stream: TStream; Advance: Boolean = True): Currency; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadAnsiChar(Stream: TStream; out Value: AnsiChar; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadAnsiChar(Stream: TStream; Advance: Boolean = True): AnsiChar; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUTF8Char(Stream: TStream; out Value: UTF8Char; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUTF8Char(Stream: TStream; Advance: Boolean = True): UTF8Char; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadWideChar(Stream: TStream; out Value: WideChar; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadWideChar(Stream: TStream; Advance: Boolean = True): WideChar; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUnicodeChar(Stream: TStream; out Value: UnicodeChar; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUnicodeChar(Stream: TStream; Advance: Boolean = True): UnicodeChar; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUCS4Char(Stream: TStream; out Value: UCS4Char; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUCS4Char(Stream: TStream; Advance: Boolean = True): UCS4Char; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadChar(Stream: TStream; out Value: Char; Advance: Boolean = True): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Stream_ReadChar(Stream: TStream; Advance: Boolean = True): Char; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadShortString(Stream: TStream; out Str: ShortString; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadShortString(Stream: TStream; Advance: Boolean = True): ShortString; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadAnsiString(Stream: TStream; out Str: AnsiString; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadAnsiString(Stream: TStream; Advance: Boolean = True): AnsiString; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUTF8String(Stream: TStream; out Str: UTF8String; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUTF8String(Stream: TStream; Advance: Boolean = True): UTF8String; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadWideString(Stream: TStream; out Str: WideString; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadWideString(Stream: TStream; Advance: Boolean = True): WideString; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUnicodeString(Stream: TStream; out Str: UnicodeString; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUnicodeString(Stream: TStream; Advance: Boolean = True): UnicodeString; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadUCS4String(Stream: TStream; out Str: UCS4String; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadUCS4String(Stream: TStream; Advance: Boolean = True): UCS4String; overload;{$IFDEF CanInline} inline; {$ENDIF}

Function Stream_ReadString(Stream: TStream; out Str: String; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadString(Stream: TStream; Advance: Boolean = True): String; overload;{$IFDEF CanInline} inline; {$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadBuffer(Stream: TStream; var Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize; overload;


{===============================================================================
--------------------------------------------------------------------------------
                                 TCustomStreamer
--------------------------------------------------------------------------------
===============================================================================}
type
  TValueType = (vtShortString,vtAnsiString,vtUTF8String,vtWideString,
                vtUnicodeString,vtUCS4String,vtString,vtFillBytes,vtBytes,
                vtPrimitive1B,vtPrimitive2B,vtPrimitive4B,vtPrimitive8B,
                vtPrimitive10B);

{===============================================================================
    TCustomStreamer - class declaration
===============================================================================}
type
  TCustomStreamer = class(TCustomListObject)
  protected
    fBookmarks:       array of Int64;
    fCount:           Integer;
    fStartPosition:   Int64;
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    Function GetBookmark(Index: Integer): Int64; virtual;
    procedure SetBookmark(Index: Integer; Value: Int64); virtual;
    Function GetCurrentPosition: Int64; virtual; abstract;
    procedure SetCurrentPosition(NewPosition: Int64); virtual; abstract;
    Function GetDistance: Int64; virtual;
    Function WriteValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize; virtual; abstract;
    Function ReadValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize; virtual; abstract;
  public
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    procedure Initialize; virtual;
    procedure MoveToStart; virtual;
    procedure MoveToBookmark(Index: Integer); virtual;
    procedure MoveBy(Offset: Int64); virtual;
    // bookmarks methods
    Function IndexOfBookmark(Position: Int64): Integer; virtual;
    Function AddBookmark: Integer; overload; virtual;
    Function AddBookmark(Position: Int64): Integer; overload; virtual;
    Function RemoveBookmark(Position: Int64; RemoveAll: Boolean = True): Integer; virtual;
    procedure DeleteBookmark(Index: Integer); virtual;
    procedure ClearBookmark; virtual;
    // write methods
    Function WriteBool(Value: ByteBool; Advance: Boolean = True): TMemSize; virtual;
    Function WriteBoolean(Value: Boolean; Advance: Boolean = True): TMemSize; virtual;
    Function WriteInt8(Value: Int8; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUInt8(Value: UInt8; Advance: Boolean = True): TMemSize; virtual;
    Function WriteInt16(Value: Int16; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUInt16(Value: UInt16; Advance: Boolean = True): TMemSize; virtual;
    Function WriteInt32(Value: Int32; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUInt32(Value: UInt32; Advance: Boolean = True): TMemSize; virtual;
    Function WriteInt64(Value: Int64; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUInt64(Value: UInt64; Advance: Boolean = True): TMemSize; virtual;
    Function WriteFloat32(Value: Float32; Advance: Boolean = True): TMemSize; virtual;
    Function WriteFloat64(Value: Float64; Advance: Boolean = True): TMemSize; virtual;
    Function WriteFloat80(Value: Float80; Advance: Boolean = True): TMemSize; virtual;
    Function WriteDateTime(Value: TDateTime; Advance: Boolean = True): TMemSize; virtual;
    Function WriteCurrency(Value: Currency; Advance: Boolean = True): TMemSize; virtual;
    Function WriteAnsiChar(Value: AnsiChar; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUTF8Char(Value: UTF8Char; Advance: Boolean = True): TMemSize; virtual;
    Function WriteWideChar(Value: WideChar; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUnicodeChar(Value: UnicodeChar; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUCS4Char(Value: UCS4Char; Advance: Boolean = True): TMemSize; virtual;
    Function WriteChar(Value: Char; Advance: Boolean = True): TMemSize; virtual;
    Function WriteShortString(const Value: ShortString; Advance: Boolean = True): TMemSize; virtual;
    Function WriteAnsiString(const Value: AnsiString; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUTF8String(const Value: UTF8String; Advance: Boolean = True): TMemSize; virtual;
    Function WriteWideString(const Value: WideString; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUnicodeString(const Value: UnicodeString; Advance: Boolean = True): TMemSize; virtual;
    Function WriteUCS4String(const Value: UCS4String; Advance: Boolean = True): TMemSize; virtual;
    Function WriteString(const Value: String; Advance: Boolean = True): TMemSize; virtual;
    Function WriteBuffer(const Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize; virtual;
    Function WriteBytes(const Value: array of UInt8; Advance: Boolean = True): TMemSize; virtual;
    Function FillBytes(ByteCount: TMemSize; Value: UInt8; Advance: Boolean = True): TMemSize; virtual;
    // read methods
    Function ReadBool(out Value: ByteBool; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadBool(Advance: Boolean = True): ByteBool; overload; virtual;
    Function ReadBoolean(out Value: Boolean; Advance: Boolean = True): TMemSize; virtual;
    Function ReadInt8(out Value: Int8; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadInt8(Advance: Boolean = True): Int8; overload; virtual;
    Function ReadUInt8(out Value: UInt8; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUInt8(Advance: Boolean = True): UInt8; overload; virtual;
    Function ReadInt16(out Value: Int16; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadInt16(Advance: Boolean = True): Int16; overload; virtual;
    Function ReadUInt16(out Value: UInt16; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUInt16(Advance: Boolean = True): UInt16; overload; virtual;
    Function ReadInt32(out Value: Int32; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadInt32(Advance: Boolean = True): Int32; overload; virtual;
    Function ReadUInt32(out Value: UInt32; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUInt32(Advance: Boolean = True): UInt32; overload; virtual;
    Function ReadInt64(out Value: Int64; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadInt64(Advance: Boolean = True): Int64; overload; virtual;
    Function ReadUInt64(out Value: UInt64; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUInt64(Advance: Boolean = True): UInt64; overload; virtual;
    Function ReadFloat32(out Value: Float32; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadFloat32(Advance: Boolean = True): Float32; overload; virtual;
    Function ReadFloat64(out Value: Float64; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadFloat64(Advance: Boolean = True): Float64; overload; virtual;
    Function ReadFloat80(out Value: Float80; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadFloat80(Advance: Boolean = True): Float80; overload; virtual;
    Function ReadDateTime(out Value: TDateTime; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadDateTime(Advance: Boolean = True): TDateTime; overload; virtual;
    Function ReadCurrency(out Value: Currency; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadCurrency(Advance: Boolean = True): Currency; overload; virtual;
    Function ReadAnsiChar(out Value: AnsiChar; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadAnsiChar(Advance: Boolean = True): AnsiChar; overload; virtual;
    Function ReadUTF8Char(out Value: UTF8Char; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUTF8Char(Advance: Boolean = True): UTF8Char; overload; virtual;
    Function ReadWideChar(out Value: WideChar; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadWideChar(Advance: Boolean = True): WideChar; overload; virtual;
    Function ReadUnicodeChar(out Value: UnicodeChar; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUnicodeChar(Advance: Boolean = True): UnicodeChar; overload; virtual;
    Function ReadUCS4Char(out Value: UCS4Char; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUCS4Char(Advance: Boolean = True): UCS4Char; overload; virtual;
    Function ReadChar(out Value: Char; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadChar(Advance: Boolean = True): Char; overload; virtual;
    Function ReadShortString(out Value: ShortString; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadShortString(Advance: Boolean = True): ShortString; overload; virtual;
    Function ReadAnsiString(out Value: AnsiString; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadAnsiString(Advance: Boolean = True): AnsiString; overload; virtual;
    Function ReadUTF8String(out Value: UTF8String; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUTF8String(Advance: Boolean = True): UTF8String; overload; virtual;
    Function ReadWideString(out Value: WideString; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadWideString(Advance: Boolean = True): WideString; overload; virtual;
    Function ReadUnicodeString(out Value: UnicodeString; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUnicodeString(Advance: Boolean = True): UnicodeString; overload; virtual;
    Function ReadUCS4String(out Value: UCS4String; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadUCS4String(Advance: Boolean = True): UCS4String; overload; virtual;
    Function ReadString(out Value: String; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadString(Advance: Boolean = True): String; overload; virtual;
    Function ReadBuffer(var Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize; overload; virtual;
    // properties
    property Bookmarks[Index: Integer]: Int64 read GetBookmark write SetBookmark;
    property CurrentPosition: Int64 read GetCurrentPosition write SetCurrentPosition;
    property StartPosition: Int64 read fStartPosition;
    property Distance: Int64 read GetDistance;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TMemoryStreamer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMemoryStreamer - class declaration
===============================================================================}
type
  TMemoryStreamer = class(TCustomStreamer)
  protected
    fCurrentPtr:  Pointer;
    fOwnsPointer: Boolean;
    fMemorySize:  TMemSize;
    Function GetStartPtr: Pointer; virtual;
    procedure SetBookmark(Index: Integer; Value: Int64); override;
    Function GetCurrentPosition: Int64; override;
    procedure SetCurrentPosition(NewPosition: Int64); override;
    Function WriteValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize; override;
    Function ReadValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize; override;
  public
    constructor Create(Memory: Pointer); overload;
    constructor Create(MemorySize: TMemSize); overload;
    destructor Destroy; override;
    procedure Initialize(Memory: Pointer); reintroduce; overload; virtual;
    procedure Initialize(MemorySize: TMemSize); reintroduce; overload; virtual;
    Function IndexOfBookmark(Position: Int64): Integer; override;
    Function AddBookmark(Position: Int64): Integer; override;
    Function RemoveBookmark(Position: Int64; RemoveAll: Boolean = True): Integer; override;
    property OwnsPointer: Boolean read fOwnsPointer;
    property MemorySize: TMemSize read fMemorySize;
    property CurrentPtr: Pointer read fCurrentPtr write fCurrentPtr;
    property StartPtr: Pointer read GetStartPtr;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TStreamStreamer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TStreamStreamer - class declaration
===============================================================================}
type
  TStreamStreamer = class(TCustomStreamer)
  protected
    fTarget:  TStream;
    Function GetCurrentPosition: Int64; override;
    procedure SetCurrentPosition(NewPosition: Int64); override;
    Function WriteValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize; override;
    Function ReadValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize; override;
  public
    constructor Create(Target: TStream);
    procedure Initialize(Target: TStream); reintroduce; virtual;
    property Target: TStream read fTarget;
  end;

implementation

uses
  StrRect;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W4056:={$WARN 4056 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
  {$DEFINE W5057:={$WARN 5057 OFF}} // Local variable "$1" does not seem to be initialized
  {$DEFINE W5058:={$WARN 5058 OFF}} // Variable "$1" does not seem to be initialized
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                               Auxiliary routines
--------------------------------------------------------------------------------
===============================================================================}

Function BoolToNum(Val: Boolean): UInt8;
begin
If Val then Result := $FF
  else Result := 0;
end;

//------------------------------------------------------------------------------

Function NumToBool(Val: UInt8): Boolean;
begin
Result := Val <> 0;
end;

//------------------------------------------------------------------------------

{$IFDEF ENDIAN_BIG}

type
  Int32Rec = packed record
    LoWord: UInt16;
    HiWord: UInt16;
  end;

//------------------------------------------------------------------------------

Function SwapEndian(Value: UInt16): UInt16; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := UInt16(Value shl 8) or UInt16(Value shr 8);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SwapEndian(Value: UInt32): UInt32; overload;
begin
Int32Rec(Result).HiWord := SwapEndian(Int32Rec(Value).LoWord);
Int32Rec(Result).LoWord := SwapEndian(Int32Rec(Value).HiWord);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SwapEndian(Value: UInt64): UInt64; overload;
begin
Int64Rec(Result).Hi := SwapEndian(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := SwapEndian(Int64Rec(Value).Hi);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SwapEndian(Value: Float32): Float32; overload;
begin
Int32Rec(Result).HiWord := SwapEndian(Int32Rec(Value).LoWord);
Int32Rec(Result).LoWord := SwapEndian(Int32Rec(Value).HiWord);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SwapEndian(Value: Float64): Float64; overload; 
begin
Int64Rec(Result).Hi := SwapEndian(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := SwapEndian(Int64Rec(Value).Hi);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function SwapEndian(Value: Float80): Float80; overload;
type
  TOverlay = packed array[0..9] of Byte;
begin
TOverlay(Result)[0] := TOverlay(Value)[9];
TOverlay(Result)[1] := TOverlay(Value)[8];
TOverlay(Result)[2] := TOverlay(Value)[7];
TOverlay(Result)[3] := TOverlay(Value)[6];
TOverlay(Result)[4] := TOverlay(Value)[5];
TOverlay(Result)[5] := TOverlay(Value)[4];
TOverlay(Result)[6] := TOverlay(Value)[3];
TOverlay(Result)[7] := TOverlay(Value)[2];
TOverlay(Result)[8] := TOverlay(Value)[1];
TOverlay(Result)[9] := TOverlay(Value)[0];
end;

//------------------------------------------------------------------------------

Function Ptr_WriteUTF16LE(var Dest: PUInt16; Data: PUInt16; Length: TStrSize): TMemSize;
var
  i:  TStrSize;
begin
Result := 0;
For i := 1 to Length do
  begin
    Dest^ := SwapEndian(Data^);
    Inc(Dest);
    Inc(Data);
    Inc(Result,SizeOf(UInt16));
  end;
end;

//------------------------------------------------------------------------------

Function Ptr_WriteUCS4LE(var Dest: PUInt32; Data: PUInt32; Length: TStrSize): TMemSize;
var
  i:  TStrSize;
begin
Result := 0;
For i := 1 to Length do
  begin
    Dest^ := SwapEndian(Data^);
    Inc(Dest);
    Inc(Data);
    Inc(Result,SizeOf(UInt32));
  end;
end;

//------------------------------------------------------------------------------

Function Ptr_ReadUTF16LE(var Src: PUInt16; Data: PUInt16; Length: TStrSize): TMemSize;
var
  i:  TStrSize;
begin
Result := 0;
For i := 1 to Length do
  begin
    Data^ := SwapEndian(Src^);
    Inc(Src);
    Inc(Data);
    Inc(Result,SizeOf(UInt16));
  end;
end;

//------------------------------------------------------------------------------

Function Ptr_ReadUCS4LE(var Src: PUInt32; Data: PUInt32; Length: TStrSize): TMemSize;
var
  i:  TStrSize;
begin
Result := 0;
For i := 1 to Length do
  begin
    Data^ := SwapEndian(Src^);
    Inc(Src);
    Inc(Data);
    Inc(Result,SizeOf(UInt32));
  end;
end;

//------------------------------------------------------------------------------

Function Stream_WriteUTF16LE(Stream: TStream; Data: PUInt16; Length: TStrSize): TMemSize;
var
  i:    TStrSize;
  Buff: UInt16;
begin
Result := 0;
For i := 1 to Length do
  begin
    Buff := SwapEndian(Data^);
    Stream.WriteBuffer(Buff,SizeOf(UInt16));
    Inc(Result,TMemSize(SizeOf(UInt16)));
    Inc(Data);
  end;
end;

//------------------------------------------------------------------------------

Function Stream_WriteUCS4LE(Stream: TStream; Data: PUInt32; Length: TStrSize): TMemSize;
var
  i:    TStrSize;
  Buff: UInt32;
begin
Result := 0;
For i := 1 to Length do
  begin
    Buff := SwapEndian(Data^);
    Stream.WriteBuffer(Buff,SizeOf(UInt32));
    Inc(Result,TMemSize(SizeOf(UInt32)));
    Inc(Data);
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
Function Stream_ReadUTF16LE(Stream: TStream; Data: PUInt16; Length: TStrSize): TMemSize;
var
  i:    TStrSize;
  Buff: UInt16;
begin
Result := 0;
For i := 1 to Length do
  begin
    Stream.ReadBuffer(Buff,SizeOf(UInt16));
    Inc(Result,TMemSize(SizeOf(UInt16)));
    Data^ := SwapEndian(Buff);
    Inc(Data);
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
Function Stream_ReadUCS4LE(Stream: TStream; Data: PUInt32; Length: TStrSize): TMemSize;
var
  i:    TStrSize;
  Buff: UInt32;
begin
Result := 0;
For i := 1 to Length do
  begin
    Stream.ReadBuffer(Buff,SizeOf(UInt32));
    Inc(Result,TMemSize(SizeOf(UInt32)));
    Data^ := SwapEndian(Buff);
    Inc(Data);
  end;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{$ENDIF ENDIAN_BIG}

{===============================================================================
--------------------------------------------------------------------------------
                               Allocation helpers
--------------------------------------------------------------------------------
===============================================================================}

Function StreamedSize_Bool: TMemSize;
begin
Result := SizeOf(UInt8);
end;

//------------------------------------------------------------------------------

Function StreamedSize_Boolean: TMemSize;
begin
Result := StreamedSize_Bool;
end;

//==============================================================================

Function StreamedSize_Int8: TMemSize;
begin
Result := SizeOf(Int8);
end;

//------------------------------------------------------------------------------

Function StreamedSize_UInt8: TMemSize;
begin
Result := SizeOf(UInt8);
end;

//------------------------------------------------------------------------------

Function StreamedSize_Int16: TMemSize;
begin
Result := SizeOf(Int16);
end;

//------------------------------------------------------------------------------

Function StreamedSize_UInt16: TMemSize;
begin
Result := SizeOf(UInt16);
end;

//------------------------------------------------------------------------------

Function StreamedSize_Int32: TMemSize;
begin
Result := SizeOf(Int32);
end;

//------------------------------------------------------------------------------

Function StreamedSize_UInt32: TMemSize;
begin
Result := SizeOf(UInt32);
end;

//------------------------------------------------------------------------------

Function StreamedSize_Int64: TMemSize;
begin
Result := SizeOf(Int64);
end;

//------------------------------------------------------------------------------

Function StreamedSize_UInt64: TMemSize;
begin
Result := SizeOf(UInt64);
end;

//==============================================================================

Function StreamedSize_Float32: TMemSize;
begin
Result := SizeOf(Float32);
end;

//------------------------------------------------------------------------------

Function StreamedSize_Float64: TMemSize;
begin
Result := SizeOf(Float64);
end;

//------------------------------------------------------------------------------

Function StreamedSize_Float80: TMemSize;
begin
Result := SizeOf(Float80);
end;

//------------------------------------------------------------------------------

Function StreamedSize_DateTime: TMemSize;
begin
Result := StreamedSize_Float64;
end;

//------------------------------------------------------------------------------

Function StreamedSize_Currency: TMemSize;
begin
Result := SizeOf(Currency);
end;

//==============================================================================

Function StreamedSize_AnsiChar: TMemSize;
begin
Result := SizeOf(AnsiChar);
end;

//------------------------------------------------------------------------------

Function StreamedSize_UTF8Char: TMemSize;
begin
Result := SizeOf(UTF8Char);
end;

//------------------------------------------------------------------------------

Function StreamedSize_WideChar: TMemSize;
begin
Result := SizeOf(WideChar);
end;

//------------------------------------------------------------------------------

Function StreamedSize_UnicodeChar: TMemSize;
begin
Result := SizeOf(UnicodeChar);
end;

//------------------------------------------------------------------------------

Function StreamedSize_UCS4Char: TMemSize;
begin
Result := SizeOf(UCS4Char);
end;

//------------------------------------------------------------------------------

Function StreamedSize_Char: TMemSize;
begin
Result := StreamedSize_UInt16;
end;

//==============================================================================

Function StreamedSize_ShortString(const Str: ShortString): TMemSize;
begin
Result := StreamedSize_UInt8 + TMemSize(Length(Str));
end;     

//------------------------------------------------------------------------------

Function StreamedSize_AnsiString(const Str: AnsiString): TMemSize;
begin
Result := StreamedSize_Int32 + TMemSize(Length(Str) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function StreamedSize_UTF8String(const Str: UTF8String): TMemSize;
begin
Result := StreamedSize_Int32 + TMemSize(Length(Str) * SizeOf(UTF8Char));
end;

//------------------------------------------------------------------------------

Function StreamedSize_WideString(const Str: WideString): TMemSize;
begin
Result := StreamedSize_Int32 + TMemSize(Length(Str) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StreamedSize_UnicodeString(const Str: UnicodeString): TMemSize;
begin
Result := StreamedSize_Int32 + TMemSize(Length(Str) * SizeOf(UnicodeChar));
end;

//------------------------------------------------------------------------------

Function StreamedSize_UCS4String(const Str: UCS4String): TMemSize;
begin
// note that UCS4 strings contain an explicit terminating zero
Result := StreamedSize_Int32 + TMemSize(Pred(Length(Str)) * SizeOf(UnicodeChar));
end;

//------------------------------------------------------------------------------

Function StreamedSize_String(const Str: String): TMemSize;
begin
Result := StreamedSize_UTF8String(StrToUTF8(Str));
end;

//==============================================================================

Function StreamedSize_Buffer(Size: TMemSize): TMemSize;
begin
Result := Size;
end;

//------------------------------------------------------------------------------

Function StreamedSize_Bytes(Count: TMemSize): TMemSize;
begin
Result := Count;
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 Memory writing
--------------------------------------------------------------------------------
===============================================================================}

Function Ptr_WriteBool(var Dest: Pointer; Value: ByteBool; Advance: Boolean): TMemSize;
begin
UInt8(Dest^) := BoolToNum(Value);
Result := SizeOf(UInt8);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteBool(Dest: Pointer; Value: ByteBool): TMemSize;
begin
Result := Ptr_WriteBool(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteBoolean(var Dest: Pointer; Value: Boolean; Advance: Boolean): TMemSize;
begin
Result := Ptr_WriteBool(Dest,Value,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_WriteBoolean(Dest: Pointer; Value: Boolean): TMemSize;
begin
Result := Ptr_WriteBool(Dest,Value);
end;

//==============================================================================

Function Ptr_WriteInt8(var Dest: Pointer; Value: Int8; Advance: Boolean): TMemSize;
begin
Int8(Dest^) := Value;
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteInt8(Dest: Pointer; Value: Int8): TMemSize;
begin
Result := Ptr_WriteInt8(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUInt8(var Dest: Pointer; Value: UInt8; Advance: Boolean): TMemSize;
begin
UInt8(Dest^) := Value;
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
 
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUInt8(Dest: Pointer; Value: UInt8): TMemSize;   
begin
Result := Ptr_WriteUInt8(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteInt16(var Dest: Pointer; Value: Int16; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Int16(Dest^) := Int16(SwapEndian(UInt16(Value)));
{$ELSE}
Int16(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
  
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteInt16(Dest: Pointer; Value: Int16): TMemSize;  
begin
Result := Ptr_WriteInt16(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
 
//------------------------------------------------------------------------------

Function Ptr_WriteUInt16(var Dest: Pointer; Value: UInt16; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt16(Dest^) := SwapEndian(Value);
{$ELSE}
UInt16(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
  
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUInt16(Dest: Pointer; Value: UInt16): TMemSize;       
begin
Result := Ptr_WriteUInt16(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
 
//------------------------------------------------------------------------------

Function Ptr_WriteInt32(var Dest: Pointer; Value: Int32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Int32(Dest^) := Int32(SwapEndian(UInt32(Value)));
{$ELSE}
Int32(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
 
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteInt32(Dest: Pointer; Value: Int32): TMemSize;    
begin
Result := Ptr_WriteInt32(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUInt32(var Dest: Pointer; Value: UInt32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt32(Dest^) := SwapEndian(Value);
{$ELSE}
UInt32(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
 
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUInt32(Dest: Pointer; Value: UInt32): TMemSize;
begin
Result := Ptr_WriteUInt32(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
  
//------------------------------------------------------------------------------

Function Ptr_WriteInt64(var Dest: Pointer; Value: Int64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Int64(Dest^) := Int64(SwapEndian(UInt64(Value)));
{$ELSE}
Int64(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
  
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteInt64(Dest: Pointer; Value: Int64): TMemSize; 
begin
Result := Ptr_WriteInt64(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUInt64(var Dest: Pointer; Value: UInt64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt64(Dest^) := SwapEndian(Value);
{$ELSE}
UInt64(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUInt64(Dest: Pointer; Value: UInt64): TMemSize;
begin
Result := Ptr_WriteUInt64(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_WriteFloat32(var Dest: Pointer; Value: Float32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Float32(Dest^) := SwapEndian(Value);
{$ELSE}
Float32(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteFloat32(Dest: Pointer; Value: Float32): TMemSize;
begin
Result := Ptr_WriteFloat32(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteFloat64(var Dest: Pointer; Value: Float64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Float64(Dest^) := SwapEndian(Value);
{$ELSE}
Float64(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteFloat64(Dest: Pointer; Value: Float64): TMemSize;
begin
Result := Ptr_WriteFloat64(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteFloat80(var Dest: Pointer; Value: Float80; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Float80(Dest^) := SwapEndian(Value);
{$ELSE}
Float80(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteFloat80(Dest: Pointer; Value: Float80): TMemSize;
begin
Result := Ptr_WriteFloat80(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteDateTime(var Dest: Pointer; Value: TDateTime; Advance: Boolean): TMemSize;
begin
Result := Ptr_WriteFloat64(Dest,Value,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteDateTime(Dest: Pointer; Value: TDateTime): TMemSize;
begin
Result := Ptr_WriteDateTime(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteCurrency(var Dest: Pointer; Value: Currency; Advance: Boolean): TMemSize;
begin
// prevent conversion of currency to other types
{$IFDEF ENDIAN_BIG}
Int64(Dest^) := SwapEndian(Int64(Addr(Value)^));
{$ELSE}
Int64(Dest^) := Int64(Addr(Value)^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteCurrency(Dest: Pointer; Value: Currency): TMemSize;
begin
Result := Ptr_WriteCurrency(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_WriteAnsiChar(var Dest: Pointer; Value: AnsiChar; Advance: Boolean): TMemSize;
begin
AnsiChar(Dest^) := Value;
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteAnsiChar(Dest: Pointer; Value: AnsiChar): TMemSize;
begin
Result := Ptr_WriteAnsiChar(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUTF8Char(var Dest: Pointer; Value: UTF8Char; Advance: Boolean): TMemSize;
begin
UTF8Char(Dest^) := Value;
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUTF8Char(Dest: Pointer; Value: UTF8Char): TMemSize;
begin
Result := Ptr_WriteUTF8Char(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteWideChar(var Dest: Pointer; Value: WideChar; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
WideChar(Dest^) := WideChar(SwapEndian(UInt16(Value)));
{$ELSE}
WideChar(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteWideChar(Dest: Pointer; Value: WideChar): TMemSize;
begin
Result := Ptr_WriteWideChar(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUnicodeChar(var Dest: Pointer; Value: UnicodeChar; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UnicodeChar(Dest^) := UnicodeChar(SwapEndian(UInt16(Value)));
{$ELSE}
UnicodeChar(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUnicodeChar(Dest: Pointer; Value: UnicodeChar): TMemSize;
begin
Result := Ptr_WriteUnicodeChar(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUCS4Char(var Dest: Pointer; Value: UCS4Char; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt32(Dest^) := SwapEndian(UInt32(Value));
{$ELSE}
UCS4Char(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUCS4Char(Dest: Pointer; Value: UCS4Char): TMemSize;
begin
Result := Ptr_WriteUCS4Char(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteChar(var Dest: Pointer; Value: Char; Advance: Boolean): TMemSize;
begin
Result := Ptr_WriteUInt16(Dest,UInt16(Ord(Value)),Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteChar(Dest: Pointer; Value: Char): TMemSize;
begin
Result := Ptr_WriteChar(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_WriteShortString(var Dest: Pointer; const Str: ShortString; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Assigned(Dest) then
  begin
    WorkPtr := Dest;
    Result := Ptr_WriteUInt8(WorkPtr,UInt8(Length(Str)),True);
    Inc(Result,Ptr_WriteBuffer(WorkPtr,(Addr(Str[1]))^,Length(Str),True));
    If Advance then
      Dest := WorkPtr;
  end
else
  Result := Length(Str) + 1;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteShortString(Dest: Pointer; const Str: ShortString): TMemSize;
begin
Result := Ptr_WriteShortString(Dest,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteAnsiString(var Dest: Pointer; const Str: AnsiString; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Assigned(Dest) then
  begin
    WorkPtr := Dest;
    Result := Ptr_WriteInt32(WorkPtr,Length(Str),True);
    Inc(Result,Ptr_WriteBuffer(WorkPtr,PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar),True));
    If Advance then
      Dest := WorkPtr;
  end
else Result := SizeOf(Int32) + (Length(Str) * SizeOf(AnsiChar));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteAnsiString(Dest: Pointer; const Str: AnsiString): TMemSize;
begin
Result := Ptr_WriteAnsiString(Dest,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUTF8String(var Dest: Pointer; const Str: UTF8String; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Assigned(Dest) then
  begin
    WorkPtr := Dest;
    Result := Ptr_WriteInt32(WorkPtr,Length(Str),True);
    Inc(Result,Ptr_WriteBuffer(WorkPtr,PUTF8Char(Str)^,Length(Str) * SizeOf(UTF8Char),True));
    If Advance then
      Dest := WorkPtr;
  end
else Result := SizeOf(Int32) + (Length(Str) * SizeOf(UTF8Char));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
  
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUTF8String(Dest: Pointer; const Str: UTF8String): TMemSize;
begin
Result := Ptr_WriteUTF8String(Dest,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteWideString(var Dest: Pointer; const Str: WideString; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Assigned(Dest) then
  begin
    WorkPtr := Dest;
    Result := Ptr_WriteInt32(WorkPtr,Length(Str),True);
  {$IFDEF ENDIAN_BIG}
    Inc(Result,Ptr_WriteUTF16LE(PUInt16(WorkPtr),PUInt16(PWideChar(Str)),Length(Str)));
  {$ELSE}
    Inc(Result,Ptr_WriteBuffer(WorkPtr,PWideChar(Str)^,Length(Str) * SizeOf(WideChar),True));
  {$ENDIF}
    If Advance then
      Dest := WorkPtr;
  end
else Result := SizeOf(Int32) + (Length(Str) * SizeOf(WideChar));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
 
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteWideString(Dest: Pointer; const Str: WideString): TMemSize;
begin
Result := Ptr_WriteWideString(Dest,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUnicodeString(var Dest: Pointer; const Str: UnicodeString; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Assigned(Dest) then
  begin
    WorkPtr := Dest;
    Result := Ptr_WriteInt32(WorkPtr,Length(Str),True);
  {$IFDEF ENDIAN_BIG}
    Inc(Result,Ptr_WriteUTF16LE(PUInt16(WorkPtr),PUInt16(PUnicodeChar(Str)),Length(Str)));
  {$ELSE}
    Inc(Result,Ptr_WriteBuffer(WorkPtr,PUnicodeChar(Str)^,Length(Str) * SizeOf(UnicodeChar),True));
  {$ENDIF}
    If Advance then
      Dest := WorkPtr;
  end
else Result := SizeOf(Int32) + (Length(Str) * SizeOf(UnicodeChar));
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUnicodeString(Dest: Pointer; const Str: UnicodeString): TMemSize;
begin
Result := Ptr_WriteUnicodeString(Dest,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUCS4String(var Dest: Pointer; const Str: UCS4String; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
If Assigned(Dest) then
  begin
    WorkPtr := Dest;
    If Length(Str) > 0 then
      begin
        Result := Ptr_WriteInt32(WorkPtr,Pred(Length(Str)),True);
      {$IFDEF ENDIAN_BIG}
        Inc(Result,Ptr_WriteUCS4LE(PUInt32(WorkPtr),PUInt32(Addr(Str[0])),Pred(Length(Str))));
      {$ELSE}
        Inc(Result,Ptr_WriteBuffer(WorkPtr,Addr(Str[0])^,Pred(Length(Str)) * SizeOf(UCS4Char),True));
      {$ENDIF}
      end
    else Result := Ptr_WriteInt32(WorkPtr,0,True);
    If Advance then
      Dest := WorkPtr;
  end
else
  begin
    If Length(Str) > 0 then
      Result := SizeOf(Int32) + (Pred(Length(Str)) * SizeOf(UCS4Char))
    else
      Result := SizeOf(Int32);
  end;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteUCS4String(Dest: Pointer; const Str: UCS4String): TMemSize;
begin
Result := Ptr_WriteUCS4String(Dest,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteString(var Dest: Pointer; const Str: String; Advance: Boolean): TMemSize;
begin
Result := Ptr_WriteUTF8String(Dest,StrToUTF8(Str),Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
    
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteString(Dest: Pointer; const Str: String): TMemSize;
begin
Result := Ptr_WriteString(Dest,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_WriteBuffer(var Dest: Pointer; const Buffer; Size: TMemSize; Advance: Boolean): TMemSize;
begin
Move(Buffer,Dest^,Size);
Result := Size;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
    
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteBuffer(Dest: Pointer; const Buffer; Size: TMemSize): TMemSize;
begin
Result := Ptr_WriteBuffer(Dest,Buffer,Size,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_WriteBytes(var Dest: Pointer; const Value: array of UInt8; Advance: Boolean): TMemSize;
var
  i:  Integer;
begin
Result := 0;
For i := Low(Value) to High(Value) do
  Inc(Result,Ptr_WriteUInt8(Dest,Value[i],True));
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
If not Advance then
  Dest := Pointer(PtrUInt(Dest) - Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
  
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteBytes(Dest: Pointer; const Value: array of UInt8): TMemSize;
begin
Result := Ptr_WriteBytes(Dest,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_FillBytes(var Dest: Pointer; Count: TMemSize; Value: UInt8; Advance: Boolean): TMemSize;
begin
FillChar(Dest^,Count,Value);
Result := Count;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Dest := Pointer(PtrUInt(Dest) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
   
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_FillBytes(Dest: Pointer; Count: TMemSize; Value: UInt8): TMemSize;
begin
Result := Ptr_FillBytes(Dest,Count,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                 Memory reading
--------------------------------------------------------------------------------
===============================================================================}

Function Ptr_ReadBool(var Src: Pointer; out Value: ByteBool; Advance: Boolean): TMemSize;
begin
Value := NumToBool(UInt8(Src^));
Result := SizeOf(UInt8);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
       
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadBool(Src: Pointer; out Value: ByteBool): TMemSize;
begin
Result := Ptr_ReadBool(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
 
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadBool(var Src: Pointer; Advance: Boolean): ByteBool;
begin
Ptr_ReadBool(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
      
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadBool(Src: Pointer): ByteBool;
begin
Ptr_ReadBool(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadBoolean(var Src: Pointer; out Value: Boolean; Advance: Boolean): TMemSize;
var
  TempBool: ByteBool;
begin
Result := Ptr_ReadBool(Src,TempBool,Advance);
Value := TempBool;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
       
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadBoolean(Src: Pointer; out Value: Boolean): TMemSize;
begin
Result := Ptr_ReadBoolean(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_ReadInt8(var Src: Pointer; out Value: Int8; Advance: Boolean): TMemSize;
begin
Value := Int8(Src^);
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
        
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadInt8(Src: Pointer; out Value: Int8): TMemSize;
begin
Result := Ptr_ReadInt8(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadInt8(var Src: Pointer; Advance: Boolean): Int8;
begin
Ptr_ReadInt8(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
           
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadInt8(Src: Pointer): Int8;
begin
Ptr_ReadInt8(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUInt8(var Src: Pointer; out Value: UInt8; Advance: Boolean): TMemSize;
begin
Value := UInt8(Src^);
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
         
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUInt8(Src: Pointer; out Value: UInt8): TMemSize;
begin
Result := Ptr_ReadUInt8(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUInt8(var Src: Pointer; Advance: Boolean): UInt8;
begin
Ptr_ReadUInt8(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
               
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUInt8(Src: Pointer): UInt8;
begin
Ptr_ReadUInt8(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadInt16(var Src: Pointer; out Value: Int16; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int16(SwapEndian(UInt16(Src^)));
{$ELSE}
Value := Int16(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
             
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadInt16(Src: Pointer; out Value: Int16): TMemSize;
begin
Result := Ptr_ReadInt16(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadInt16(var Src: Pointer; Advance: Boolean): Int16;
begin
Ptr_ReadInt16(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
             
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadInt16(Src: Pointer): Int16;
begin
Ptr_ReadInt16(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUInt16(var Src: Pointer; out Value: UInt16; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(UInt16(Src^));
{$ELSE}
Value := UInt16(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
  
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUInt16(Src: Pointer; out Value: UInt16): TMemSize;
begin
Result := Ptr_ReadUInt16(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUInt16(var Src: Pointer; Advance: Boolean): UInt16;
begin
Ptr_ReadUInt16(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUInt16(Src: Pointer): UInt16;
begin
Ptr_ReadUInt16(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadInt32(var Src: Pointer; out Value: Int32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int32(SwapEndian(UInt32(Src^)));
{$ELSE}
Value := Int32(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
          
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadInt32(Src: Pointer; out Value: Int32): TMemSize;
begin
Result := Ptr_ReadInt32(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadInt32(var Src: Pointer; Advance: Boolean): Int32;
begin
Ptr_ReadInt32(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
             
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadInt32(Src: Pointer): Int32;
begin
Ptr_ReadInt32(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUInt32(var Src: Pointer; out Value: UInt32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(UInt32(Src^));
{$ELSE}
Value := UInt32(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
          
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUInt32(Src: Pointer; out Value: UInt32): TMemSize;
begin
Result := Ptr_ReadUInt32(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUInt32(var Src: Pointer; Advance: Boolean): UInt32;
begin
Ptr_ReadUInt32(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
             
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUInt32(Src: Pointer): UInt32;
begin
Ptr_ReadUInt32(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}
 
//------------------------------------------------------------------------------

Function Ptr_ReadInt64(var Src: Pointer; out Value: Int64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int64(SwapEndian(UInt64(Src^)));
{$ELSE}
Value := Int64(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
             
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadInt64(Src: Pointer; out Value: Int64): TMemSize;
begin
Result := Ptr_ReadInt64(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadInt64(var Src: Pointer; Advance: Boolean): Int64;
begin
Ptr_ReadInt64(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadInt64(Src: Pointer): Int64;
begin
Ptr_ReadInt64(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUInt64(var Src: Pointer; out Value: UInt64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(UInt64(Src^));
{$ELSE}
Value := UInt64(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                 
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUInt64(Src: Pointer; out Value: UInt64): TMemSize;
begin
Result := Ptr_ReadUInt64(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUInt64(var Src: Pointer; Advance: Boolean): UInt64;
begin
Ptr_ReadUInt64(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                  
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUInt64(Src: Pointer): UInt64;
begin
Ptr_ReadUInt64(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_ReadFloat32(var Src: Pointer; out Value: Float32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Float32(Src^));
{$ELSE}
Value := Float32(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;
 
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadFloat32(Src: Pointer; out Value: Float32): TMemSize;
begin
Result := Ptr_ReadFloat32(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadFloat32(var Src: Pointer; Advance: Boolean): Float32;
begin
Ptr_ReadFloat32(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                 
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadFloat32(Src: Pointer): Float32;
begin
Ptr_ReadFloat32(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadFloat64(var Src: Pointer; out Value: Float64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Float64(Src^));
{$ELSE}
Value := Float64(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                 
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadFloat64(Src: Pointer; out Value: Float64): TMemSize;
begin
Result := Ptr_ReadFloat64(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadFloat64(var Src: Pointer; Advance: Boolean): Float64;
begin
Ptr_ReadFloat64(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                  
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadFloat64(Src: Pointer): Float64;
begin
Ptr_ReadFloat64(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadFloat80(var Src: Pointer; out Value: Float80; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Float80(Src^));
{$ELSE}
Value := Float80(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadFloat80(Src: Pointer; out Value: Float80): TMemSize;
begin
Result := Ptr_ReadFloat80(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadFloat80(var Src: Pointer; Advance: Boolean): Float80;
begin
Ptr_ReadFloat80(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadFloat80(Src: Pointer): Float80;
begin
Ptr_ReadFloat80(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadDateTime(var Src: Pointer; out Value: TDateTime; Advance: Boolean): TMemSize;
begin
Result := Ptr_ReadFloat64(Src,Float64(Value),Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadDateTime(Src: Pointer; out Value: TDateTime): TMemSize;
begin
Result := Ptr_ReadDateTime(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadDateTime(var Src: Pointer; Advance: Boolean): TDateTime;
begin
Ptr_ReadDateTime(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadDateTime(Src: Pointer): TDateTime;
begin
Ptr_ReadDateTime(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadCurrency(var Src: Pointer; out Value: Currency; Advance: Boolean): TMemSize;
{$IFDEF ENDIAN_BIG}
var
  Temp: UInt64 absolute Value;
{$ENDIF}
begin
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(UInt64(Src^));
{$ELSE}
Value := Currency(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadCurrency(Src: Pointer; out Value: Currency): TMemSize;
begin
Result := Ptr_ReadCurrency(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadCurrency(var Src: Pointer; Advance: Boolean): Currency;
begin
Ptr_ReadCurrency(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadCurrency(Src: Pointer): Currency;
begin
Ptr_ReadCurrency(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_ReadAnsiChar(var Src: Pointer; out Value: AnsiChar; Advance: Boolean): TMemSize;
begin
Value := AnsiChar(Src^);
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                 
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadAnsiChar(Src: Pointer; out Value: AnsiChar): TMemSize;
begin
Result := Ptr_ReadAnsiChar(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadAnsiChar(var Src: Pointer; Advance: Boolean): AnsiChar;
begin
Ptr_ReadAnsiChar(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
               
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadAnsiChar(Src: Pointer): AnsiChar;
begin
Ptr_ReadAnsiChar(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUTF8Char(var Src: Pointer; out Value: UTF8Char; Advance: Boolean): TMemSize;
begin
Value := UTF8Char(Src^);
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUTF8Char(Src: Pointer; out Value: UTF8Char): TMemSize;
begin
Result := Ptr_ReadUTF8Char(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUTF8Char(var Src: Pointer; Advance: Boolean): UTF8Char;
begin
Ptr_ReadUTF8Char(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUTF8Char(Src: Pointer): UTF8Char;
begin
Ptr_ReadUTF8Char(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadWideChar(var Src: Pointer; out Value: WideChar; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := WideChar(SwapEndian(UInt16(Src^)));
{$ELSE}
Value := WideChar(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadWideChar(Src: Pointer; out Value: WideChar): TMemSize;
begin
Result := Ptr_ReadWideChar(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadWideChar(var Src: Pointer; Advance: Boolean): WideChar;
begin
Ptr_ReadWideChar(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                 
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadWideChar(Src: Pointer): WideChar;
begin
Ptr_ReadWideChar(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUnicodeChar(var Src: Pointer; out Value: UnicodeChar; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := UnicodeChar(SwapEndian(UInt16(Src^)));
{$ELSE}
Value := UnicodeChar(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUnicodeChar(Src: Pointer; out Value: UnicodeChar): TMemSize;
begin
Result := Ptr_ReadUnicodeChar(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUnicodeChar(var Src: Pointer; Advance: Boolean): UnicodeChar;
begin
Ptr_ReadUnicodeChar(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUnicodeChar(Src: Pointer): UnicodeChar;
begin
Ptr_ReadUnicodeChar(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUCS4Char(var Src: Pointer; out Value: UCS4Char; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := UCS4Char(SwapEndian(UInt32(Src^)));
{$ELSE}
Value := UCS4Char(Src^);
{$ENDIF}
Result := SizeOf(Value);
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUCS4Char(Src: Pointer; out Value: UCS4Char): TMemSize;
begin
Result := Ptr_ReadUCS4Char(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUCS4Char(var Src: Pointer; Advance: Boolean): UCS4Char;
begin
Ptr_ReadUCS4Char(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUCS4Char(Src: Pointer): UCS4Char;
begin
Ptr_ReadUCS4Char(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadChar(var Src: Pointer; out Value: Char; Advance: Boolean): TMemSize;
var
  Temp: UInt16;
begin
Result := Ptr_ReadUInt16(Src,Temp,Advance);
Value := Char(Temp);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
               
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadChar(Src: Pointer; out Value: Char): TMemSize;
begin
Result := Ptr_ReadChar(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadChar(var Src: Pointer; Advance: Boolean): Char;
begin
Ptr_ReadChar(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadChar(Src: Pointer): Char;
begin
Ptr_ReadChar(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_ReadShortString(var Src: Pointer; out Str: ShortString; Advance: Boolean): TMemSize;
var
  StrLength:  UInt8;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadUInt8(WorkPtr,StrLength,True);
SetLength(Str,StrLength);
Inc(Result,Ptr_ReadBuffer(WorkPtr,Addr(Str[1])^,StrLength,True));
If Advance then
  Src := WorkPtr;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
            
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadShortString(Src: Pointer; out Str: ShortString): TMemSize;
begin
Result := Ptr_ReadShortString(Src,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadShortString(var Src: Pointer; Advance: Boolean): ShortString;
begin
Ptr_ReadShortString(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
            
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadShortString(Src: Pointer): ShortString;
begin
Ptr_ReadShortString(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadAnsiString(var Src: Pointer; out Str: AnsiString; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
SetLength(Str,StrLength);
Inc(Result,Ptr_ReadBuffer(WorkPtr,PAnsiChar(Str)^,StrLength * SizeOf(AnsiChar),True));
If Advance then
  Src := WorkPtr;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
           
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadAnsiString(Src: Pointer; out Str: AnsiString): TMemSize;
begin
Result := Ptr_ReadAnsiString(Src,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadAnsiString(var Src: Pointer; Advance: Boolean): AnsiString;
begin
Ptr_ReadAnsiString(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
           
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadAnsiString(Src: Pointer): AnsiString;
begin
Ptr_ReadAnsiString(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUTF8String(var Src: Pointer; out Str: UTF8String; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
SetLength(Str,StrLength);
Inc(Result,Ptr_ReadBuffer(WorkPtr,PUTF8Char(Str)^,StrLength * SizeOf(UTF8Char),True));
If Advance then
  Src := WorkPtr;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
       
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUTF8String(Src: Pointer; out Str: UTF8String): TMemSize;
begin
Result := Ptr_ReadUTF8String(Src,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUTF8String(var Src: Pointer; Advance: Boolean): UTF8String;
begin
Ptr_ReadUTF8String(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
          
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUTF8String(Src: Pointer): UTF8String;
begin
Ptr_ReadUTF8String(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadWideString(var Src: Pointer; out Str: WideString; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
SetLength(Str,StrLength);
{$IFDEF ENDIAN_BIG}
Inc(Result,Ptr_ReadUTF16LE(PUInt16(WorkPtr),PUInt16(PWideChar(Str)),StrLength));
{$ELSE}
Inc(Result,Ptr_ReadBuffer(WorkPtr,PWideChar(Str)^,StrLength * SizeOf(WideChar),True));
{$ENDIF}
If Advance then
  Src := WorkPtr;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
          
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadWideString(Src: Pointer; out Str: WideString): TMemSize;
begin
Result := Ptr_ReadWideString(Src,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadWideString(var Src: Pointer; Advance: Boolean): WideString;
begin
Ptr_ReadWideString(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
           
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadWideString(Src: Pointer): WideString;
begin
Ptr_ReadWideString(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUnicodeString(var Src: Pointer; out Str: UnicodeString; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
SetLength(Str,StrLength);
{$IFDEF ENDIAN_BIG}
Inc(Result,Ptr_ReadUTF16LE(PUInt16(WorkPtr),PUInt16(PUnicodeChar(Str)),StrLength));
{$ELSE}
Inc(Result,Ptr_ReadBuffer(WorkPtr,PUnicodeChar(Str)^,StrLength * SizeOf(UnicodeChar),True));
{$ENDIF}
If Advance then
  Src := WorkPtr;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUnicodeString(Src: Pointer; out Str: UnicodeString): TMemSize;
begin
Result := Ptr_ReadUnicodeString(Src,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUnicodeString(var Src: Pointer; Advance: Boolean): UnicodeString;
begin
Ptr_ReadUnicodeString(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
        
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUnicodeString(Src: Pointer): UnicodeString;
begin
Ptr_ReadUnicodeString(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUCS4String(var Src: Pointer; out Str: UCS4String; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
SetLength(Str,StrLength + 1);
Str[High(Str)] := 0;
{$IFDEF ENDIAN_BIG}
Inc(Result,Ptr_ReadUCS4LE(PUInt32(WorkPtr),PUInt32(Addr(Str[0])),Pred(StrLength)));
{$ELSE}
Inc(Result,Ptr_ReadBuffer(WorkPtr,Addr(Str[0])^,Pred(StrLength) * SizeOf(UCS4Char),True));
{$ENDIF}
If Advance then
  Src := WorkPtr;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUCS4String(Src: Pointer; out Str: UCS4String): TMemSize;
begin
Result := Ptr_ReadUCS4String(Src,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadUCS4String(var Src: Pointer; Advance: Boolean): UCS4String;
begin
Ptr_ReadUCS4String(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadUCS4String(Src: Pointer): UCS4String;
begin
Ptr_ReadUCS4String(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadString(var Src: Pointer; out Str: String; Advance: Boolean): TMemSize;
var
  TempStr:  UTF8String;
begin
Result := Ptr_ReadUTF8String(Src,TempStr,Advance);
Str := UTF8ToStr(TempStr);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
        
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadString(Src: Pointer; out Str: String): TMemSize;
begin
Result := Ptr_ReadString(Src,Str,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadString(var Src: Pointer; Advance: Boolean): String;
begin
Ptr_ReadString(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                   
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadString(Src: Pointer): String;
begin
Ptr_ReadString(Src,Result,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//==============================================================================

Function Ptr_ReadBuffer(var Src: Pointer; var Buffer; Size: TMemSize; Advance: Boolean): TMemSize; 
begin
Move(Src^,Buffer,Size);
Result := Size;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Src := Pointer(PtrUInt(Src) + Result);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---
                       
{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadBuffer(Src: Pointer; var Buffer; Size: TMemSize): TMemSize;
begin
Result := Ptr_ReadBuffer(Src,Buffer,Size,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}


{===============================================================================
--------------------------------------------------------------------------------
                                 Stream writing
--------------------------------------------------------------------------------
===============================================================================}

Function Stream_WriteBool(Stream: TStream; Value: ByteBool; Advance: Boolean = True): TMemSize;
var
  Temp: UInt8;
begin
Temp := BoolToNum(Value);
Stream.WriteBuffer(Temp,SizeOf(Temp));
Result := SizeOf(Temp);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteBoolean(Stream: TStream; Value: Boolean; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteBool(Stream,Value,Advance);
end;

//==============================================================================

Function Stream_WriteInt8(Stream: TStream; Value: Int8; Advance: Boolean = True): TMemSize;
begin
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteUInt8(Stream: TStream; Value: UInt8; Advance: Boolean = True): TMemSize;
begin
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteInt16(Stream: TStream; Value: Int16; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int16(SwapEndian(UInt16(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;
 
//------------------------------------------------------------------------------

Function Stream_WriteUInt16(Stream: TStream; Value: UInt16; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteInt32(Stream: TStream; Value: Int32; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int32(SwapEndian(UInt32(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteUInt32(Stream: TStream; Value: UInt32; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;
 
//------------------------------------------------------------------------------

Function Stream_WriteInt64(Stream: TStream; Value: Int64; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int64(SwapEndian(UInt64(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteUInt64(Stream: TStream; Value: UInt64; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//==============================================================================

Function Stream_WriteFloat32(Stream: TStream; Value: Float32; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteFloat64(Stream: TStream; Value: Float64; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteFloat80(Stream: TStream; Value: Float80; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteDateTime(Stream: TStream; Value: TDateTime; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteFloat64(Stream,Value,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_WriteCurrency(Stream: TStream; Value: Currency; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt64(Addr(Value)^) := SwapEndian(UInt64(Addr(Value)^));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//==============================================================================

Function Stream_WriteAnsiChar(Stream: TStream; Value: AnsiChar; Advance: Boolean = True): TMemSize;
begin
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteUTF8Char(Stream: TStream; Value: UTF8Char; Advance: Boolean = True): TMemSize;
begin
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteWideChar(Stream: TStream; Value: WideChar; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := WideChar(SwapEndian(UInt16(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteUnicodeChar(Stream: TStream; Value: UnicodeChar; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := UnicodeChar(SwapEndian(UInt16(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteUCS4Char(Stream: TStream; Value: UCS4Char; Advance: Boolean = True): TMemSize;
{$IFDEF ENDIAN_BIG}
{$IF SizeOf(UInt32) <> SizeOf(UCS4Char)}
  {$MESSAGE ERROR 'Type size mismatch (UInt32 - UCS4Char).'}
{$IFEND}
var
  Temp: UInt32;
begin
// to prevent potential problems with range checks
Temp := SwapEndian(UInt32(Value));
Stream.WriteBuffer(Temp,SizeOf(Temp));
{$ELSE}
begin
Stream.WriteBuffer(Value,SizeOf(Value));
{$ENDIF}
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteChar(Stream: TStream; Value: Char; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteUInt16(Stream,UInt16(Ord(Value)),Advance);
end;

//==============================================================================

Function Stream_WriteShortString(Stream: TStream; const Str: ShortString; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteUInt8(Stream,UInt8(Length(Str)),True);
Inc(Result,Stream_WriteBuffer(Stream,Addr(Str[1])^,Length(Str),True));
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteAnsiString(Stream: TStream; const Str: AnsiString; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteInt32(Stream,Length(Str),True);
Inc(Result,Stream_WriteBuffer(Stream,PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar),True));
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteUTF8String(Stream: TStream; const Str: UTF8String; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteInt32(Stream,Length(Str),True);
Inc(Result,Stream_WriteBuffer(Stream,PUTF8Char(Str)^,Length(Str) * SizeOf(UTF8Char),True));
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteWideString(Stream: TStream; const Str: WideString; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteInt32(Stream,Length(Str),True);
{$IFDEF ENDIAN_BIG}
Inc(Result,Stream_WriteUTF16LE(Stream,PUInt16(PWideChar(Str)),Length(Str)));
{$ELSE}
Inc(Result,Stream_WriteBuffer(Stream,PWideChar(Str)^,Length(Str) * SizeOf(WideChar),True));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteUnicodeString(Stream: TStream; const Str: UnicodeString; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteInt32(Stream,Length(Str),True);
{$IFDEF ENDIAN_BIG}
Inc(Result,Stream_WriteUTF16LE(Stream,PUInt16(PUnicodeChar(Str)),Length(Str)));
{$ELSE}
Inc(Result,Stream_WriteBuffer(Stream,PUnicodeChar(Str)^,Length(Str) * SizeOf(UnicodeChar),True));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteUCS4String(Stream: TStream; const Str: UCS4String; Advance: Boolean = True): TMemSize;
begin
If Length(Str) > 0 then
  begin
    Result := Stream_WriteInt32(Stream,Pred(Length(Str)),True);
  {$IFDEF ENDIAN_BIG}
    Inc(Result,Stream_WriteUCS4LE(Stream,PUInt32(Addr(Str[0])),Pred(Length(Str))));
  {$ELSE}
    Inc(Result,Stream_WriteBuffer(Stream,Addr(Str[0])^,Pred(Length(Str)) * SizeOf(UCS4Char),True));
  {$ENDIF}
  end
else Result := Stream_WriteInt32(Stream,0,True);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//------------------------------------------------------------------------------

Function Stream_WriteString(Stream: TStream; const Str: String; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteUTF8String(Stream,StrToUTF8(Str),Advance);
end;

//==============================================================================

Function Stream_WriteBuffer(Stream: TStream; const Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;
begin
Stream.WriteBuffer(Buffer,Size);
Result := Size;
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//==============================================================================

Function Stream_WriteBytes(Stream: TStream; const Value: array of UInt8; Advance: Boolean = True): TMemSize;
var
  i:  Integer;
begin
Result := 0;
For i := Low(Value) to High(Value) do
  Inc(Result,Stream_WriteUInt8(Stream,Value[i],True));
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//==============================================================================

Function Stream_FillBytes(Stream: TStream; Count: TMemSize; Value: UInt8; Advance: Boolean = True): TMemSize;
var
  i:  TMemSize;
begin
Result := 0;
For i := 1 to Count do
  begin
    Stream.WriteBuffer(Value,SizeOf(Value));
    Inc(Result,SizeOf(Value));
  end;
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 Stream reading
--------------------------------------------------------------------------------
===============================================================================}

{$IFDEF FPCDWM}{$PUSH}W5057{$ENDIF}
Function Stream_ReadBool(Stream: TStream; out Value: ByteBool; Advance: Boolean = True): TMemSize;
var
  Temp: UInt8;
begin
Stream.ReadBuffer(Temp,SizeOf(Temp));
Result := SizeOf(Temp);
Value := NumToBool(Temp);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadBool(Stream: TStream; Advance: Boolean = True): ByteBool;
begin
Stream_ReadBool(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadBoolean(Stream: TStream; out Value: Boolean; Advance: Boolean = True): TMemSize;
var
  TempBool: ByteBool;
begin
Result := Stream_ReadBool(Stream,TempBool,Advance);
Value := TempBool;
end;

//==============================================================================

Function Stream_ReadInt8(Stream: TStream; out Value: Int8; Advance: Boolean = True): TMemSize;
begin
Value := 0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadInt8(Stream: TStream; Advance: Boolean = True): Int8;
begin
Stream_ReadInt8(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUInt8(Stream: TStream; out Value: UInt8; Advance: Boolean = True): TMemSize;
begin
Value := 0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUInt8(Stream: TStream; Advance: Boolean = True): UInt8;
begin
Stream_ReadUInt8(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadInt16(Stream: TStream; out Value: Int16; Advance: Boolean = True): TMemSize;
begin
Value := 0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := Int16(SwapEndian(UInt16(Value)));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadInt16(Stream: TStream; Advance: Boolean = True): Int16;
begin
Stream_ReadInt16(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUInt16(Stream: TStream; out Value: UInt16; Advance: Boolean = True): TMemSize;
begin
Value := 0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUInt16(Stream: TStream; Advance: Boolean = True): UInt16;
begin
Stream_ReadUInt16(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadInt32(Stream: TStream; out Value: Int32; Advance: Boolean = True): TMemSize;
begin
Value := 0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := Int32(SwapEndian(UInt32(Value)));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadInt32(Stream: TStream; Advance: Boolean = True): Int32;
begin
Stream_ReadInt32(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUInt32(Stream: TStream; out Value: UInt32; Advance: Boolean = True): TMemSize;
begin
Value := 0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUInt32(Stream: TStream; Advance: Boolean = True): UInt32;
begin
Stream_ReadUInt32(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadInt64(Stream: TStream; out Value: Int64; Advance: Boolean = True): TMemSize;
begin
Value := 0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := Int64(SwapEndian(UInt64(Value)));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadInt64(Stream: TStream; Advance: Boolean = True): Int64;
begin
Stream_ReadInt64(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUInt64(Stream: TStream; out Value: UInt64; Advance: Boolean = True): TMemSize;
begin
Value := 0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUInt64(Stream: TStream; Advance: Boolean = True): UInt64;
begin
Stream_ReadUInt64(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadFloat32(Stream: TStream; out Value: Float32; Advance: Boolean = True): TMemSize;
begin
Value := 0.0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadFloat32(Stream: TStream; Advance: Boolean = True): Float32;
begin
Stream_ReadFloat32(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadFloat64(Stream: TStream; out Value: Float64; Advance: Boolean = True): TMemSize;
begin
Value := 0.0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadFloat64(Stream: TStream; Advance: Boolean = True): Float64;
begin
Stream_ReadFloat64(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadFloat80(Stream: TStream; out Value: Float80; Advance: Boolean = True): TMemSize;
begin
FillChar(Addr(Value)^,SizeOf(Value),0);
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadFloat80(Stream: TStream; Advance: Boolean = True): Float80;
begin
Stream_ReadFloat80(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadDateTime(Stream: TStream; out Value: TDateTime; Advance: Boolean = True): TMemSize;
begin
Result := Stream_ReadFloat64(Stream,Float64(Value),Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadDateTime(Stream: TStream; Advance: Boolean = True): TDateTime;
begin
Stream_ReadDateTime(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadCurrency(Stream: TStream; out Value: Currency; Advance: Boolean = True): TMemSize;
begin
Value := 0.0;
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
UInt64(Addr(Value)^) := SwapEndian(UInt64(Addr(Value)^));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadCurrency(Stream: TStream; Advance: Boolean = True): Currency;
begin
Stream_ReadCurrency(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadAnsiChar(Stream: TStream; out Value: AnsiChar; Advance: Boolean = True): TMemSize;
begin
Value := AnsiChar(0);
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadAnsiChar(Stream: TStream; Advance: Boolean = True): AnsiChar;
begin
Stream_ReadAnsiChar(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUTF8Char(Stream: TStream; out Value: UTF8Char; Advance: Boolean = True): TMemSize;
begin
Value := UTF8Char(0);
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;
 
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUTF8Char(Stream: TStream; Advance: Boolean = True): UTF8Char;
begin
Stream_ReadUTF8Char(Stream,Result,Advance);
end;
 
//------------------------------------------------------------------------------

Function Stream_ReadWideChar(Stream: TStream; out Value: WideChar; Advance: Boolean = True): TMemSize;
begin
Value := WideChar(0);
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := WideChar(SwapEndian(UInt16(Value)));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;
 
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadWideChar(Stream: TStream; Advance: Boolean = True): WideChar;
begin
Stream_ReadWideChar(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUnicodeChar(Stream: TStream; out Value: UnicodeChar; Advance: Boolean = True): TMemSize;
begin
Value := UnicodeChar(0);
Stream.ReadBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
{$IFDEF ENDIAN_BIG}
Value := UnicodeChar(SwapEndian(UInt16(Value)));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUnicodeChar(Stream: TStream; Advance: Boolean = True): UnicodeChar;
begin
Stream_ReadUnicodeChar(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUCS4Char(Stream: TStream; out Value: UCS4Char; Advance: Boolean = True): TMemSize;
{$IFDEF ENDIAN_BIG}
{$IF SizeOf(UInt32) <> SizeOf(UCS4Char)}
  {$MESSAGE ERROR 'Type size mismatch (UInt32 - UCS4Char).'}
{$IFEND}
var
  Temp: UInt32;
begin
Temp := 0;
Stream.ReadBuffer(Temp,SizeOf(Temp));
Value := UCS4Char(SwapEndian(Temp));
{$ELSE}
begin
Value := UCS4Char(0);
Stream.ReadBuffer(Value,SizeOf(Value));
{$ENDIF}
Result := SizeOf(Value);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUCS4Char(Stream: TStream; Advance: Boolean = True): UCS4Char;
begin
Stream_ReadUCS4Char(Stream,Result,Advance);
end;
 
//------------------------------------------------------------------------------

Function Stream_ReadChar(Stream: TStream; out Value: Char; Advance: Boolean = True): TMemSize;
var
  Temp: UInt16;
begin
Result := Stream_ReadUInt16(Stream,Temp,Advance);
Value := Char(Temp);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadChar(Stream: TStream; Advance: Boolean = True): Char;
begin
Stream_ReadChar(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadShortString(Stream: TStream; out Str: ShortString; Advance: Boolean = True): TMemSize;
var
  StrLength:  UInt8;
begin
Result := Stream_ReadUInt8(Stream,StrLength,True);
SetLength(Str,StrLength);
Inc(Result,Stream_ReadBuffer(Stream,Addr(Str[1])^,StrLength,True));
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadShortString(Stream: TStream; Advance: Boolean = True): ShortString;
begin
Stream_ReadShortString(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadAnsiString(Stream: TStream; out Str: AnsiString; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
SetLength(Str,StrLength);
Inc(Result,Stream_ReadBuffer(Stream,PAnsiChar(Str)^,StrLength * SizeOf(AnsiChar),True));
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadAnsiString(Stream: TStream; Advance: Boolean = True): AnsiString;
begin
Stream_ReadAnsiString(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUTF8String(Stream: TStream; out Str: UTF8String; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
SetLength(Str,StrLength);
Inc(Result,Stream_ReadBuffer(Stream,PUTF8Char(Str)^,StrLength * SizeOf(UTF8Char),True));
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUTF8String(Stream: TStream; Advance: Boolean = True): UTF8String;
begin
Stream_ReadUTF8String(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadWideString(Stream: TStream; out Str: WideString; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
SetLength(Str,StrLength);
{$IFDEF ENDIAN_BIG}
Inc(Result,Stream_ReadUTF16LE(Stream,PUInt16(PWideChar(Str)),StrLength));
{$ELSE}
Inc(Result,Stream_ReadBuffer(Stream,PWideChar(Str)^,StrLength * SizeOf(WideChar),True));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadWideString(Stream: TStream; Advance: Boolean = True): WideString;
begin
Stream_ReadWideString(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUnicodeString(Stream: TStream; out Str: UnicodeString; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
SetLength(Str,StrLength);
{$IFDEF ENDIAN_BIG}
Inc(Result,Stream_ReadUTF16LE(Stream,PUInt16(PUnicodeChar(Str)),StrLength));
{$ELSE}
Inc(Result,Stream_ReadBuffer(Stream,PUnicodeChar(Str)^,StrLength * SizeOf(UnicodeChar),True));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUnicodeString(Stream: TStream; Advance: Boolean = True): UnicodeString;
begin
Stream_ReadUnicodeString(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadUCS4String(Stream: TStream; out Str: UCS4String; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
SetLength(Str,StrLength + 1);
Str[High(Str)] := 0;
{$IFDEF ENDIAN_BIG}
Inc(Result,Stream_ReadUCS4LE(Stream,PUInt32(Addr(Str[0])),Pred(StrLength)));
{$ELSE}
Inc(Result,Stream_ReadBuffer(Stream,Addr(Str[0])^,Pred(StrLength) * SizeOf(UCS4Char),True));
{$ENDIF}
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadUCS4String(Stream: TStream; Advance: Boolean = True): UCS4String;
begin
Stream_ReadUCS4String(Stream,Result,Advance);
end;

//------------------------------------------------------------------------------

Function Stream_ReadString(Stream: TStream; out Str: String; Advance: Boolean = True): TMemSize;
var
  TempStr:  UTF8String;
begin
Result := Stream_ReadUTF8String(Stream,TempStr,Advance);
Str := UTF8ToStr(TempStr);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadString(Stream: TStream; Advance: Boolean = True): String;
begin
Stream_ReadString(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadBuffer(Stream: TStream; var Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Buffer,Size);
Result := Size;
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TCustomStreamer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustomStreamer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustomStreamer - protected methods
-------------------------------------------------------------------------------}

Function TCustomStreamer.GetCapacity: Integer;
begin
Result := Length(fBookmarks);
end;

//------------------------------------------------------------------------------

procedure TCustomStreamer.SetCapacity(Value: Integer);
begin
If Value <> Length(fBookmarks) then
  begin
    SetLength(fBookMarks,Value);
    If Value < fCount then
      fCount := Value;
  end;
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TCustomStreamer.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TCustomStreamer.GetBookmark(Index: Integer): Int64;
begin
If CheckIndex(Index) then
  Result := fBookmarks[Index]
else
  raise EBSIndexOutOfBounds.CreateFmt('TCustomStreamer.GetBookmark: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TCustomStreamer.SetBookmark(Index: Integer; Value: Int64);
begin
If CheckIndex(Index) then
  fBookmarks[Index] := Value
else
  raise EBSIndexOutOfBounds.CreateFmt('TCustomStreamer.SetBookmark: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.GetDistance: Int64;
begin
Result := CurrentPosition - StartPosition;
end;

{-------------------------------------------------------------------------------
    TCustomStreamer - public methods
-------------------------------------------------------------------------------}

Function TCustomStreamer.LowIndex: Integer;
begin
Result := Low(fBookmarks);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

procedure TCustomStreamer.Initialize;
begin
SetLength(fBookmarks,0);
fStartPosition := 0;
end;

//------------------------------------------------------------------------------

procedure TCustomStreamer.MoveToStart;
begin
CurrentPosition := StartPosition;
end;

//------------------------------------------------------------------------------

procedure TCustomStreamer.MoveToBookmark(Index: Integer);
begin
If CheckIndex(Index) then
  CurrentPosition := fBookmarks[Index]
else
  raise EBSIndexOutOfBounds.CreateFmt('TCustomStreamer.MoveToBookmark: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TCustomStreamer.MoveBy(Offset: Int64);
begin
CurrentPosition := CurrentPosition + Offset;
end;

//------------------------------------------------------------------------------
 
Function TCustomStreamer.IndexOfBookmark(Position: Int64): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fBookmarks[i] = Position then
    begin
      Result := i;
      Break;
    end;
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.AddBookmark: Integer;
begin
Result := AddBookmark(CurrentPosition);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.AddBookmark(Position: Int64): Integer;
begin
Grow;
Result := fCount;
fBookmarks[Result] := Position;
Inc(fCount);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.RemoveBookmark(Position: Int64; RemoveAll: Boolean = True): Integer;
begin
repeat
  Result := IndexOfBookmark(Position);
  If Result >= 0 then
    DeleteBookMark(Result);
until (Result < 0) or not RemoveAll;
end;

//------------------------------------------------------------------------------

procedure TCustomStreamer.DeleteBookmark(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    For i := Index to Pred(High(fBookmarks)) do
      fBookmarks[i] := fBookMarks[i + 1];
    Dec(fCount);
    Shrink;
  end
else raise EBSIndexOutOfBounds.CreateFmt('TCustomStreamer.DeleteBookmark: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TCustomStreamer.ClearBookmark;
begin
fCount := 0;
Shrink;
end;

//==============================================================================

Function TCustomStreamer.WriteBool(Value: ByteBool; Advance: Boolean = True): TMemSize;
var
  Temp: UInt8;
begin
Temp := BoolToNum(Value);
Result := WriteValue(@Temp,Advance,SizeOf(Temp),vtPrimitive1B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteBoolean(Value: Boolean; Advance: Boolean = True): TMemSize;
begin
Result := WriteBool(Value,Advance);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteInt8(Value: Int8; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive1B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUInt8(Value: UInt8; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive1B);
end;
 
//------------------------------------------------------------------------------

Function TCustomStreamer.WriteInt16(Value: Int16; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive2B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUInt16(Value: UInt16; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive2B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteInt32(Value: Int32; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive4B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUInt32(Value: UInt32; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive4B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteInt64(Value: Int64; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUInt64(Value: UInt64; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteFloat32(Value: Float32; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive4B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteFloat64(Value: Float64; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteFloat80(Value: Float80; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive10B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteDateTime(Value: TDateTime; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteCurrency(Value: Currency; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;   

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteAnsiChar(Value: AnsiChar; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive1B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUTF8Char(Value: UTF8Char; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive1B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteWideChar(Value: WideChar; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive2B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUnicodeChar(Value: UnicodeChar; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive2B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUCS4Char(Value: UCS4Char; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,SizeOf(Value),vtPrimitive4B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteChar(Value: Char; Advance: Boolean = True): TMemSize;
var
  Temp: UInt16;
begin
Temp := UInt16(Ord(Value));
Result := WriteValue(@Temp,Advance,SizeOf(Temp),vtPrimitive2B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteShortString(const Value: ShortString; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,0,vtShortString);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteAnsiString(const Value: AnsiString; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,0,vtAnsiString);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUTF8String(const Value: UTF8String; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,0,vtUTF8String);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteWideString(const Value: WideString; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,0,vtWideString);
end;
 
//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUnicodeString(const Value: UnicodeString; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,0,vtUnicodeString);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteUCS4String(const Value: UCS4String; Advance: Boolean = True): TMemSize; 
begin
Result := WriteValue(@Value,Advance,0,vtUCS4String);
end;
  
//------------------------------------------------------------------------------

Function TCustomStreamer.WriteString(const Value: String; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,0,vtString);
end;
  
//------------------------------------------------------------------------------

Function TCustomStreamer.WriteBuffer(const Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Buffer,Advance,Size,vtBytes);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteBytes(const Value: array of UInt8; Advance: Boolean = True): TMemSize;
var
  i:      Integer;
  OldPos: Int64;
begin
OldPos := CurrentPosition;
Result := 0;
For i := Low(Value) to High(Value) do
  WriteUInt8(Value[i],True);
If not Advance then
  CurrentPosition := OldPos;
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.FillBytes(ByteCount: TMemSize; Value: UInt8; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,ByteCount,vtFillBytes);
end;

//==============================================================================

Function TCustomStreamer.ReadBool(out Value: ByteBool; Advance: Boolean = True): TMemSize;
var
  Temp: UInt8;
begin
Result := ReadValue(@Temp,Advance,SizeOf(Temp),vtPrimitive1B);
Value := NumToBool(Temp);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadBool(Advance: Boolean = True): ByteBool;
var
  Temp: UInt8;
begin
ReadValue(@Temp,Advance,SizeOf(Temp),vtPrimitive1B);
Result := NumToBool(Temp);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadBoolean(out Value: Boolean; Advance: Boolean = True): TMemSize;
var
  TempBool: ByteBool;
begin
Result := ReadBool(TempBool,Advance);
Value := TempBool;
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadInt8(out Value: Int8; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive1B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadInt8(Advance: Boolean = True): Int8;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive1B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUInt8(out Value: UInt8; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive1B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUInt8(Advance: Boolean = True): UInt8;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive1B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadInt16(out Value: Int16; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive2B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadInt16(Advance: Boolean = True): Int16;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive2B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUInt16(out Value: UInt16; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive2B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUInt16(Advance: Boolean = True): UInt16;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive2B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadInt32(out Value: Int32; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive4B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadInt32(Advance: Boolean = True): Int32;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive4B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUInt32(out Value: UInt32; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive4B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUInt32(Advance: Boolean = True): UInt32;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive4B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadInt64(out Value: Int64; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadInt64(Advance: Boolean = True): Int64;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive8B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUInt64(out Value: UInt64; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUInt64(Advance: Boolean = True): UInt64;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive8B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadFloat32(out Value: Float32; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive4B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadFloat32(Advance: Boolean = True): Float32;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive4B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadFloat64(out Value: Float64; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadFloat64(Advance: Boolean = True): Float64;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive8B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadFloat80(out Value: Float80; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive10B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadFloat80(Advance: Boolean = True): Float80;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive10B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadDateTime(out Value: TDateTime; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadDateTime(Advance: Boolean = True): TDateTime;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive8B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadCurrency(out Value: Currency; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive8B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadCurrency(Advance: Boolean = True): Currency;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive8B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadAnsiChar(out Value: AnsiChar; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive1B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadAnsiChar(Advance: Boolean = True): AnsiChar;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive1B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUTF8Char(out Value: UTF8Char; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive1B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUTF8Char(Advance: Boolean = True): UTF8Char;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive1B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadWideChar(out Value: WideChar; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive2B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadWideChar(Advance: Boolean = True): WideChar;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive2B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUnicodeChar(out Value: UnicodeChar; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive2B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUnicodeChar(Advance: Boolean = True): UnicodeChar;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive2B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUCS4Char(out Value: UCS4Char; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,SizeOf(Value),vtPrimitive4B);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUCS4Char(Advance: Boolean = True): UCS4Char;
begin
ReadValue(@Result,Advance,SizeOf(Result),vtPrimitive4B);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadChar(out Value: Char; Advance: Boolean = True): TMemSize;
var
  Temp: UInt16;
begin
Result := ReadValue(@Temp,Advance,SizeOf(Temp),vtPrimitive2B);
Value := Char(Temp);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadChar(Advance: Boolean = True): Char;
var
  Temp: UInt16;
begin
ReadValue(@Temp,Advance,SizeOf(Temp),vtPrimitive2B);
Result := Char(Temp);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadShortString(out Value: ShortString; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,0,vtShortString);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadShortString(Advance: Boolean = True): ShortString;
begin
ReadValue(@Result,Advance,0,vtShortString);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadAnsiString(out Value: AnsiString; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,0,vtAnsiString);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadAnsiString(Advance: Boolean = True): AnsiString;
begin
ReadValue(@Result,Advance,0,vtAnsiString);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUTF8String(out Value: UTF8String; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,0,vtUTF8String);
end;
 
//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUTF8String(Advance: Boolean = True): UTF8String;
begin
ReadValue(@Result,Advance,0,vtUTF8String);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadWideString(out Value: WideString; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,0,vtWideString);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadWideString(Advance: Boolean = True): WideString;
begin
ReadValue(@Result,Advance,0,vtWideString);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUnicodeString(out Value: UnicodeString; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,0,vtUnicodeString);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUnicodeString(Advance: Boolean = True): UnicodeString;
begin
ReadValue(@Result,Advance,0,vtUnicodeString);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadUCS4String(out Value: UCS4String; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,0,vtUCS4String);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadUCS4String(Advance: Boolean = True): UCS4String;
begin
ReadValue(@Result,Advance,0,vtUCS4String);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadString(out Value: String; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,0,vtString);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadString(Advance: Boolean = True): String;
begin
ReadValue(@Result,Advance,0,vtString);
end;

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadBuffer(var Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Buffer,Advance,Size,vtBytes);
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TMemoryStreamer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TMemoryStreamer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TMemoryStreamer - protected methods
-------------------------------------------------------------------------------}

Function TMemoryStreamer.GetStartPtr: Pointer;
begin
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
Result := Pointer(fStartPosition);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TMemoryStreamer.SetBookmark(Index: Integer; Value: Int64);
begin
{
  casting to PtrInt is done to cut off higher 32bits on 32bit system
}
inherited SetBookmark(Index,Int64(PtrInt(Value)));
end;

//------------------------------------------------------------------------------

Function TMemoryStreamer.GetCurrentPosition: Int64;
begin
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
Result := Int64(fCurrentPtr);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TMemoryStreamer.SetCurrentPosition(NewPosition: Int64);
begin
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
fCurrentPtr := Pointer(NewPosition);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TMemoryStreamer.WriteValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize;
begin
case ValueType of
  vtShortString:    Result := Ptr_WriteShortString(fCurrentPtr,ShortString(Value^),Advance);
  vtAnsiString:     Result := Ptr_WriteAnsiString(fCurrentPtr,AnsiString(Value^),Advance);
  vtUTF8String:     Result := Ptr_WriteUTF8String(fCurrentPtr,UTF8String(Value^),Advance);
  vtWideString:     Result := Ptr_WriteWideString(fCurrentPtr,WideString(Value^),Advance);
  vtUnicodeString:  Result := Ptr_WriteUnicodeString(fCurrentPtr,UnicodeString(Value^),Advance);
  vtUCS4String:     Result := Ptr_WriteUCS4String(fCurrentPtr,UCS4String(Value^),Advance);
  vtString:         Result := Ptr_WriteString(fCurrentPtr,String(Value^),Advance);
  vtFillBytes:      Result := Ptr_FillBytes(fCurrentPtr,Size,UInt8(Value^),Advance);
  vtPrimitive1B:    Result := Ptr_WriteUInt8(fCurrentPtr,UInt8(Value^),Advance);
  vtPrimitive2B:    Result := Ptr_WriteUInt16(fCurrentPtr,UInt16(Value^),Advance);
  vtPrimitive4B:    Result := Ptr_WriteUInt32(fCurrentPtr,UInt32(Value^),Advance);
  vtPrimitive8B:    Result := Ptr_WriteUInt64(fCurrentPtr,UInt64(Value^),Advance);
  vtPrimitive10B:   Result := Ptr_WriteFloat80(fCurrentPtr,Float80(Value^),Advance);
else
 {vtBytes}
  Result := Ptr_WriteBuffer(fCurrentPtr,Value^,Size,Advance);
end;
end;

//------------------------------------------------------------------------------

Function TMemoryStreamer.ReadValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize;
begin
case ValueType of
  vtShortString:    Result := Ptr_ReadShortString(fCurrentPtr,ShortString(Value^),Advance);
  vtAnsiString:     Result := Ptr_ReadAnsiString(fCurrentPtr,AnsiString(Value^),Advance);
  vtUTF8String:     Result := Ptr_ReadUTF8String(fCurrentPtr,UTF8String(Value^),Advance);
  vtWideString:     Result := Ptr_ReadWideString(fCurrentPtr,WideString(Value^),Advance);
  vtUnicodeString:  Result := Ptr_ReadUnicodeString(fCurrentPtr,UnicodeString(Value^),Advance);
  vtUCS4String:     Result := Ptr_ReadUCS4String(fCurrentPtr,UCS4String(Value^),Advance);
  vtString:         Result := Ptr_ReadString(fCurrentPtr,String(Value^),Advance);
  vtPrimitive1B:    Result := Ptr_ReadUInt8(fCurrentPtr,UInt8(Value^),Advance);
  vtPrimitive2B:    Result := Ptr_ReadUInt16(fCurrentPtr,UInt16(Value^),Advance);
  vtPrimitive4B:    Result := Ptr_ReadUInt32(fCurrentPtr,UInt32(Value^),Advance);
  vtPrimitive8B:    Result := Ptr_ReadUInt64(fCurrentPtr,UInt64(Value^),Advance);
  vtPrimitive10B:   Result := Ptr_ReadFloat80(fCurrentPtr,Float80(Value^),Advance);
else
 {vtFillBytes, vtBytes}
  Result := Ptr_ReadBuffer(fCurrentPtr,Value^,Size,Advance);
end;
end;

{-------------------------------------------------------------------------------
    TMemoryStreamer - public methods
-------------------------------------------------------------------------------}

constructor TMemoryStreamer.Create(Memory: Pointer);
begin
inherited Create;
fOwnsPointer := False;
Initialize(Memory);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

constructor TMemoryStreamer.Create(MemorySize: TMemSize);
begin
inherited Create;
fOwnsPointer := False;
Initialize(MemorySize);
end;

//------------------------------------------------------------------------------

destructor TMemoryStreamer.Destroy;
begin
If fOwnsPointer then
  FreeMem(StartPtr,fMemorySize);
inherited;
end;

//------------------------------------------------------------------------------

procedure TMemoryStreamer.Initialize(Memory: Pointer);
begin
inherited Initialize;
fOwnsPointer := False;
fMemorySize := 0;
fCurrentPtr := Memory;
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
fStartPosition := Int64(Memory);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

procedure TMemoryStreamer.Initialize(MemorySize: TMemSize);
var
  TempPtr:  Pointer;
begin
inherited Initialize;
If fOwnsPointer then
  begin
    TempPtr := StartPtr;
    ReallocMem(TempPtr,MemorySize);
  end
else TempPtr := AllocMem(MemorySize);
fOwnsPointer := True;
fMemorySize := MemorySize;
fCurrentPtr := TempPtr;
{$IFDEF FPCDWM}{$PUSH}W4055 W4056{$ENDIF}
fStartPosition := Int64(TempPtr);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TMemoryStreamer.IndexOfBookmark(Position: Int64): Integer;
begin
Result := inherited IndexOfBookmark(PtrInt(Position));
end;

//------------------------------------------------------------------------------

Function TMemoryStreamer.AddBookmark(Position: Int64): Integer;
begin
Result := inherited AddBookmark(PtrInt(Position));
end;

//------------------------------------------------------------------------------

Function TMemoryStreamer.RemoveBookmark(Position: Int64; RemoveAll: Boolean = True): Integer;
begin
Result := inherited RemoveBookmark(PtrInt(Position),RemoveAll);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TStreamStreamer
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TStreamStreamer - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TStreamStreamer - protected methods
-------------------------------------------------------------------------------}

Function TStreamStreamer.GetCurrentPosition: Int64;
begin
Result := fTarget.Position;
end;

//------------------------------------------------------------------------------

procedure TStreamStreamer.SetCurrentPosition(NewPosition: Int64);
begin
fTarget.Position := NewPosition;
end;

//------------------------------------------------------------------------------

Function TStreamStreamer.WriteValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize;
begin
case ValueType of
  vtShortString:    Result := Stream_WriteShortString(fTarget,ShortString(Value^),Advance);
  vtAnsiString:     Result := Stream_WriteAnsiString(fTarget,AnsiString(Value^),Advance);
  vtUTF8String:     Result := Stream_WriteUTF8String(fTarget,UTF8String(Value^),Advance);
  vtWideString:     Result := Stream_WriteWideString(fTarget,WideString(Value^),Advance);
  vtUnicodeString:  Result := Stream_WriteUnicodeString(fTarget,UnicodeString(Value^),Advance);
  vtUCS4String:     Result := Stream_WriteUCS4String(fTarget,UCS4String(Value^),Advance);
  vtString:         Result := Stream_WriteString(fTarget,String(Value^),Advance);
  vtFillBytes:      Result := Stream_FillBytes(fTarget,Size,UInt8(Value^),Advance);
  vtPrimitive1B:    Result := Stream_WriteUInt8(fTarget,UInt8(Value^),Advance);
  vtPrimitive2B:    Result := Stream_WriteUInt16(fTarget,UInt16(Value^),Advance);
  vtPrimitive4B:    Result := Stream_WriteUInt32(fTarget,UInt32(Value^),Advance);
  vtPrimitive8B:    Result := Stream_WriteUInt64(fTarget,UInt64(Value^),Advance);
  vtPrimitive10B:   Result := Stream_WriteFloat80(fTarget,Float80(Value^),Advance);
else
 {vtBytes}
  Result := Stream_WriteBuffer(fTarget,Value^,Size,Advance);
end;
end;

//------------------------------------------------------------------------------

Function TStreamStreamer.ReadValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize;
begin
case ValueType of
  vtShortString:    Result := Stream_ReadShortString(fTarget,ShortString(Value^),Advance);
  vtAnsiString:     Result := Stream_ReadAnsiString(fTarget,AnsiString(Value^),Advance);
  vtUTF8String:     Result := Stream_ReadUTF8String(fTarget,UTF8String(Value^),Advance);
  vtWideString:     Result := Stream_ReadWideString(fTarget,WideString(Value^),Advance);
  vtUnicodeString:  Result := Stream_ReadUnicodeString(fTarget,UnicodeString(Value^),Advance);
  vtUCS4String:     Result := Stream_ReadUCS4String(fTarget,UCS4String(Value^),Advance);
  vtString:         Result := Stream_ReadString(fTarget,String(Value^),Advance);
  vtPrimitive1B:    Result := Stream_ReadUInt8(fTarget,UInt8(Value^),Advance);
  vtPrimitive2B:    Result := Stream_ReadUInt16(fTarget,UInt16(Value^),Advance);
  vtPrimitive4B:    Result := Stream_ReadUInt32(fTarget,UInt32(Value^),Advance);
  vtPrimitive8B:    Result := Stream_ReadUInt64(fTarget,UInt64(Value^),Advance);
  vtPrimitive10B:   Result := Stream_ReadFloat80(fTarget,Float80(Value^),Advance);
else
 {vtFillBytes,vtBytes}
  Result := Stream_ReadBuffer(fTarget,Value^,Size,Advance);
end;
end;

{-------------------------------------------------------------------------------
    TStreamStreamer - public methods
-------------------------------------------------------------------------------}

constructor TStreamStreamer.Create(Target: TStream);
begin
inherited Create;
Initialize(Target);
end;

//------------------------------------------------------------------------------

procedure TStreamStreamer.Initialize(Target: TStream);
begin
inherited Initialize;
fTarget := Target;
fStartPosition := Target.Position;
end;

end.

