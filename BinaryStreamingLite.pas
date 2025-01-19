{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Binary streaming lite

    This unit provides lightened version of BinaryStreaming library (which can
    be found here: github.com/TheLazyTomcat/Lib.BinaryStreaming).

    Over time, BinaryStreaming (further referred to only as BS) became bloated
    with features that were only seldomly needed. This library was created for
    situations, where core functionality was sufficient and/or using full BS
    was undesirable (eg. because of long(er) compile times).

    It provides functions for streaming of almost all basic types to memory
    (Ptr_* functions) and to TStream descendants (Stream_* functions), with
    exception being variants and small/tiny strings - they are not supported.
    In provided functions, selection of endianness was removed - everything is
    done in little endian.
    And lastly, streamer objects were also completely removed.

    For further details (eg. about how the primitives are streamed), please
    refer to descriptions in the original BS.

      NOTE - this entire unit was build from the original BS using a
             specialized program, it was not manually written. The code was
             copied, so if BS contains some error, it might appear here too.

  Build from BinaryStreaming of version 2.1 (2025-01-19)             

  Version 1.0.2 (2025-01-19)

  Last change 2025-01-19

  ©2025 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.BinaryStreamingLite

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect       - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol BinaryStreamingLite_UseAuxExceptions for details).

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit BinaryStreamingLite;
{
  BinaryStreamingLite_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  BinaryStreamingLite_UseAuxExceptions to achieve this.
}
{$IF Defined(BinaryStreamingLite_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND}

//------------------------------------------------------------------------------  

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17} // Delphi 2005+
    {$DEFINE CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

interface

uses
  SysUtils, Classes,
  AuxTypes, StrRect {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EBSLException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

{===============================================================================
--------------------------------------------------------------------------------
                               Allocation helpers
--------------------------------------------------------------------------------
===============================================================================}
{
  Following functions are returning number of bytes that are required to store
  a given object. They are here mainly to ease allocation when streaming into
  memory.
  Basic types have static size (for completeness sake they are nevertheless
  included), but strings and variants are a little trickier and must be scanned
  to obtain the true size.
}

Function StreamedSize_Bool: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_Boolean: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function StreamedSize_Int8: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UInt8: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_Int16: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UInt16: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_Int32: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UInt32: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_Int64: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UInt64: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function StreamedSize_Float32: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_Float64: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_Float80: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_DateTime: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_Currency: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function StreamedSize_AnsiChar: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UTF8Char: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_WideChar: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UnicodeChar: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UCS4Char: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_Char: TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function StreamedSize_ShortString(const Value: ShortString): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_AnsiString(const Value: AnsiString): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UTF8String(const Value: UTF8String): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_WideString(const Value: WideString): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UnicodeString(const Value: UnicodeString): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_UCS4String(const Value: UCS4String): TMemSize;
Function StreamedSize_String(const Value: String): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function StreamedSize_Buffer(Size: TMemSize): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}
Function StreamedSize_Bytes(Count: TMemSize): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                 Memory writing
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    Booleans
-------------------------------------------------------------------------------}

Function Ptr_WriteBool(var Dest: Pointer; Value: ByteBool; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteBool(Dest: Pointer; Value: ByteBool): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteBoolean(var Dest: Pointer; Value: Boolean; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_WriteBoolean(Dest: Pointer; Value: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Integers
-------------------------------------------------------------------------------}

Function Ptr_WriteInt8(var Dest: Pointer; Value: Int8; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteInt8(Dest: Pointer; Value: Int8): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUInt8(var Dest: Pointer; Value: UInt8; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUInt8(Dest: Pointer; Value: UInt8): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteInt16(var Dest: Pointer; Value: Int16; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteInt16(Dest: Pointer; Value: Int16): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUInt16(var Dest: Pointer; Value: UInt16; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUInt16(Dest: Pointer; Value: UInt16): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteInt32(var Dest: Pointer; Value: Int32; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteInt32(Dest: Pointer; Value: Int32): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUInt32(var Dest: Pointer; Value: UInt32; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUInt32(Dest: Pointer; Value: UInt32): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteInt64(var Dest: Pointer; Value: Int64; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteInt64(Dest: Pointer; Value: Int64): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUInt64(var Dest: Pointer; Value: UInt64; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUInt64(Dest: Pointer; Value: UInt64): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Floating point numbers
-------------------------------------------------------------------------------}

Function Ptr_WriteFloat32(var Dest: Pointer; Value: Float32; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteFloat32(Dest: Pointer; Value: Float32): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteFloat64(var Dest: Pointer; Value: Float64; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteFloat64(Dest: Pointer; Value: Float64): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteFloat80(var Dest: Pointer; Value: Float80; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteFloat80(Dest: Pointer; Value: Float80): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteDateTime(var Dest: Pointer; Value: TDateTime; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_WriteDateTime(Dest: Pointer; Value: TDateTime): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteCurrency(var Dest: Pointer; Value: Currency; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteCurrency(Dest: Pointer; Value: Currency): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Characters
-------------------------------------------------------------------------------}

Function Ptr_WriteAnsiChar(var Dest: Pointer; Value: AnsiChar; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteAnsiChar(Dest: Pointer; Value: AnsiChar): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUTF8Char(var Dest: Pointer; Value: UTF8Char; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUTF8Char(Dest: Pointer; Value: UTF8Char): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteWideChar(var Dest: Pointer; Value: WideChar; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteWideChar(Dest: Pointer; Value: WideChar): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUnicodeChar(var Dest: Pointer; Value: UnicodeChar; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUnicodeChar(Dest: Pointer; Value: UnicodeChar): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUCS4Char(var Dest: Pointer; Value: UCS4Char; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUCS4Char(Dest: Pointer; Value: UCS4Char): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteChar(var Dest: Pointer; Value: Char; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_WriteChar(Dest: Pointer; Value: Char): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Strings
-------------------------------------------------------------------------------}

Function Ptr_WriteShortString(var Dest: Pointer; const Value: ShortString; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteShortString(Dest: Pointer; const Value: ShortString): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteAnsiString(var Dest: Pointer; const Value: AnsiString; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteAnsiString(Dest: Pointer; const Value: AnsiString): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUTF8String(var Dest: Pointer; const Value: UTF8String; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUTF8String(Dest: Pointer; const Value: UTF8String): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteWideString(var Dest: Pointer; const Value: WideString; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteWideString(Dest: Pointer; const Value: WideString): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUnicodeString(var Dest: Pointer; const Value: UnicodeString; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUnicodeString(Dest: Pointer; const Value: UnicodeString): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteUCS4String(var Dest: Pointer; const Value: UCS4String; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteUCS4String(Dest: Pointer; const Value: UCS4String): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_WriteString(var Dest: Pointer; const Value: String; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_WriteString(Dest: Pointer; const Value: String): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    General data buffers
-------------------------------------------------------------------------------}

Function Ptr_WriteBuffer(var Dest: Pointer; const Buffer; Size: TMemSize; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteBuffer(Dest: Pointer; const Buffer; Size: TMemSize): TMemSize; overload;

//------------------------------------------------------------------------------
// note - calls with open array cannot be inlined both in Delphi and FPC

Function Ptr_WriteBytes(var Dest: Pointer; const Value: array of UInt8; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteBytes(Dest: Pointer; const Value: array of UInt8): TMemSize; overload;

//------------------------------------------------------------------------------

Function Ptr_FillBytes(var Dest: Pointer; Count: TMemSize; Value: UInt8; Advance: Boolean): TMemSize; overload;
Function Ptr_FillBytes(Dest: Pointer; Count: TMemSize; Value: UInt8): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                 Memory reading
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    Booleans
-------------------------------------------------------------------------------}

Function Ptr_ReadBool(var Src: Pointer; out Value: ByteBool; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadBool(Src: Pointer; out Value: ByteBool): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetBool(var Src: Pointer; Advance: Boolean): ByteBool; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetBool(Src: Pointer): ByteBool; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadBoolean(var Src: Pointer; out Value: Boolean; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_ReadBoolean(Src: Pointer; out Value: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetBoolean(var Src: Pointer; Advance: Boolean): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetBoolean(Src: Pointer): Boolean; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Integers
-------------------------------------------------------------------------------}

Function Ptr_ReadInt8(var Src: Pointer; out Value: Int8; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadInt8(Src: Pointer; out Value: Int8): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetInt8(var Src: Pointer; Advance: Boolean): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetInt8(Src: Pointer): Int8; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUInt8(var Src: Pointer; out Value: UInt8; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUInt8(Src: Pointer; out Value: UInt8): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUInt8(var Src: Pointer; Advance: Boolean): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUInt8(Src: Pointer): UInt8; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadInt16(var Src: Pointer; out Value: Int16; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadInt16(Src: Pointer; out Value: Int16): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetInt16(var Src: Pointer; Advance: Boolean): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetInt16(Src: Pointer): Int16; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUInt16(var Src: Pointer; out Value: UInt16; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUInt16(Src: Pointer; out Value: UInt16): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUInt16(var Src: Pointer; Advance: Boolean): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUInt16(Src: Pointer): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadInt32(var Src: Pointer; out Value: Int32; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadInt32(Src: Pointer; out Value: Int32): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetInt32(var Src: Pointer; Advance: Boolean): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetInt32(Src: Pointer): Int32; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUInt32(var Src: Pointer; out Value: UInt32; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUInt32(Src: Pointer; out Value: UInt32): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUInt32(var Src: Pointer; Advance: Boolean): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUInt32(Src: Pointer): UInt32; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadInt64(var Src: Pointer; out Value: Int64; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadInt64(Src: Pointer; out Value: Int64): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetInt64(var Src: Pointer; Advance: Boolean): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetInt64(Src: Pointer): Int64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUInt64(var Src: Pointer; out Value: UInt64; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUInt64(Src: Pointer; out Value: UInt64): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUInt64(var Src: Pointer; Advance: Boolean): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUInt64(Src: Pointer): UInt64; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Floating point numbers
-------------------------------------------------------------------------------}

Function Ptr_ReadFloat32(var Src: Pointer; out Value: Float32; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadFloat32(Src: Pointer; out Value: Float32): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetFloat32(var Src: Pointer; Advance: Boolean): Float32; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetFloat32(Src: Pointer): Float32; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadFloat64(var Src: Pointer; out Value: Float64; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadFloat64(Src: Pointer; out Value: Float64): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetFloat64(var Src: Pointer; Advance: Boolean): Float64; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetFloat64(Src: Pointer): Float64; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadFloat80(var Src: Pointer; out Value: Float80; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadFloat80(Src: Pointer; out Value: Float80): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetFloat80(var Src: Pointer; Advance: Boolean): Float80; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetFloat80(Src: Pointer): Float80; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadDateTime(var Src: Pointer; out Value: TDateTime; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_ReadDateTime(Src: Pointer; out Value: TDateTime): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetDateTime(var Src: Pointer; Advance: Boolean): TDateTime; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetDateTime(Src: Pointer): TDateTime; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadCurrency(var Src: Pointer; out Value: Currency; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadCurrency(Src: Pointer; out Value: Currency): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetCurrency(var Src: Pointer; Advance: Boolean): Currency; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetCurrency(Src: Pointer): Currency; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Characters
-------------------------------------------------------------------------------}

Function Ptr_ReadAnsiChar(var Src: Pointer; out Value: AnsiChar; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadAnsiChar(Src: Pointer; out Value: AnsiChar): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetAnsiChar(var Src: Pointer; Advance: Boolean): AnsiChar; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetAnsiChar(Src: Pointer): AnsiChar; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUTF8Char(var Src: Pointer; out Value: UTF8Char; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUTF8Char(Src: Pointer; out Value: UTF8Char): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUTF8Char(var Src: Pointer; Advance: Boolean): UTF8Char; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUTF8Char(Src: Pointer): UTF8Char; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadWideChar(var Src: Pointer; out Value: WideChar; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadWideChar(Src: Pointer; out Value: WideChar): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetWideChar(var Src: Pointer; Advance: Boolean): WideChar; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetWideChar(Src: Pointer): WideChar; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUnicodeChar(var Src: Pointer; out Value: UnicodeChar; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUnicodeChar(Src: Pointer; out Value: UnicodeChar): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUnicodeChar(var Src: Pointer; Advance: Boolean): UnicodeChar; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUnicodeChar(Src: Pointer): UnicodeChar; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUCS4Char(var Src: Pointer; out Value: UCS4Char; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUCS4Char(Src: Pointer; out Value: UCS4Char): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUCS4Char(var Src: Pointer; Advance: Boolean): UCS4Char; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUCS4Char(Src: Pointer): UCS4Char; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadChar(var Src: Pointer; out Value: Char; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_ReadChar(Src: Pointer; out Value: Char): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetChar(var Src: Pointer; Advance: Boolean): Char; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetChar(Src: Pointer): Char; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Strings
-------------------------------------------------------------------------------}

Function Ptr_ReadShortString(var Src: Pointer; out Value: ShortString; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadShortString(Src: Pointer; out Value: ShortString): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetShortString(var Src: Pointer; Advance: Boolean): ShortString; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetShortString(Src: Pointer): ShortString; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadAnsiString(var Src: Pointer; out Value: AnsiString; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadAnsiString(Src: Pointer; out Value: AnsiString): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetAnsiString(var Src: Pointer; Advance: Boolean): AnsiString; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetAnsiString(Src: Pointer): AnsiString; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUTF8String(var Src: Pointer; out Value: UTF8String; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUTF8String(Src: Pointer; out Value: UTF8String): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUTF8String(var Src: Pointer; Advance: Boolean): UTF8String; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUTF8String(Src: Pointer): UTF8String; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadWideString(var Src: Pointer; out Value: WideString; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadWideString(Src: Pointer; out Value: WideString): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetWideString(var Src: Pointer; Advance: Boolean): WideString; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetWideString(Src: Pointer): WideString; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUnicodeString(var Src: Pointer; out Value: UnicodeString; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUnicodeString(Src: Pointer; out Value: UnicodeString): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUnicodeString(var Src: Pointer; Advance: Boolean): UnicodeString; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUnicodeString(Src: Pointer): UnicodeString; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadUCS4String(var Src: Pointer; out Value: UCS4String; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadUCS4String(Src: Pointer; out Value: UCS4String): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetUCS4String(var Src: Pointer; Advance: Boolean): UCS4String; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetUCS4String(Src: Pointer): UCS4String; overload;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Ptr_ReadString(var Src: Pointer; out Value: String; Advance: Boolean): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_ReadString(Src: Pointer; out Value: String): TMemSize; overload;{$IFDEF CanInline} inline;{$ENDIF}

Function Ptr_GetString(var Src: Pointer; Advance: Boolean): String; overload;{$IFDEF CanInline} inline;{$ENDIF}
Function Ptr_GetString(Src: Pointer): String; overload;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    General data buffers
-------------------------------------------------------------------------------}

Function Ptr_ReadBuffer(var Src: Pointer; out Buffer; Size: TMemSize; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadBuffer(Src: Pointer; out Buffer; Size: TMemSize): TMemSize; overload;

{===============================================================================
--------------------------------------------------------------------------------
                                 Stream writing
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    Booleans
-------------------------------------------------------------------------------}

Function Stream_WriteBool(Stream: TStream; Value: ByteBool; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteBoolean(Stream: TStream; Value: Boolean; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Integers
-------------------------------------------------------------------------------}

Function Stream_WriteInt8(Stream: TStream; Value: Int8; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUInt8(Stream: TStream; Value: UInt8; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteInt16(Stream: TStream; Value: Int16; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUInt16(Stream: TStream; Value: UInt16; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteInt32(Stream: TStream; Value: Int32; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUInt32(Stream: TStream; Value: UInt32; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteInt64(Stream: TStream; Value: Int64; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUInt64(Stream: TStream; Value: UInt64; Advance: Boolean = True): TMemSize;

{-------------------------------------------------------------------------------
    Floating point numbers
-------------------------------------------------------------------------------}

Function Stream_WriteFloat32(Stream: TStream; Value: Float32; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteFloat64(Stream: TStream; Value: Float64; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteFloat80(Stream: TStream; Value: Float80; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteDateTime(Stream: TStream; Value: TDateTime; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_WriteCurrency(Stream: TStream; Value: Currency; Advance: Boolean = True): TMemSize;

{-------------------------------------------------------------------------------
    Characters
-------------------------------------------------------------------------------}

Function Stream_WriteAnsiChar(Stream: TStream; Value: AnsiChar; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUTF8Char(Stream: TStream; Value: UTF8Char; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteWideChar(Stream: TStream; Value: WideChar; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUnicodeChar(Stream: TStream; Value: UnicodeChar; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUCS4Char(Stream: TStream; Value: UCS4Char; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteChar(Stream: TStream; Value: Char; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Strings
-------------------------------------------------------------------------------}

Function Stream_WriteShortString(Stream: TStream; const Value: ShortString; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteAnsiString(Stream: TStream; const Value: AnsiString; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUTF8String(Stream: TStream; const Value: UTF8String; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteWideString(Stream: TStream; const Value: WideString; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUnicodeString(Stream: TStream; const Value: UnicodeString; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteUCS4String(Stream: TStream; const Value: UCS4String; Advance: Boolean = True): TMemSize;

//------------------------------------------------------------------------------

Function Stream_WriteString(Stream: TStream; const Value: String; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    General data buffers
-------------------------------------------------------------------------------}

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
{-------------------------------------------------------------------------------
    Booleans
-------------------------------------------------------------------------------}

Function Stream_ReadBool(Stream: TStream; out Value: ByteBool; Advance: Boolean = True): TMemSize;

Function Stream_GetBool(Stream: TStream; Advance: Boolean = True): ByteBool;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadBoolean(Stream: TStream; out Value: Boolean; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function Stream_GetBoolean(Stream: TStream; Advance: Boolean = True): Boolean;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Integers
-------------------------------------------------------------------------------}

Function Stream_ReadInt8(Stream: TStream; out Value: Int8; Advance: Boolean = True): TMemSize;

Function Stream_GetInt8(Stream: TStream; Advance: Boolean = True): Int8;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUInt8(Stream: TStream; out Value: UInt8; Advance: Boolean = True): TMemSize;

Function Stream_GetUInt8(Stream: TStream; Advance: Boolean = True): UInt8;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadInt16(Stream: TStream; out Value: Int16; Advance: Boolean = True): TMemSize;

Function Stream_GetInt16(Stream: TStream; Advance: Boolean = True): Int16;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUInt16(Stream: TStream; out Value: UInt16; Advance: Boolean = True): TMemSize;

Function Stream_GetUInt16(Stream: TStream; Advance: Boolean = True): UInt16;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadInt32(Stream: TStream; out Value: Int32; Advance: Boolean = True): TMemSize;

Function Stream_GetInt32(Stream: TStream; Advance: Boolean = True): Int32;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUInt32(Stream: TStream; out Value: UInt32; Advance: Boolean = True): TMemSize;

Function Stream_GetUInt32(Stream: TStream; Advance: Boolean = True): UInt32;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadInt64(Stream: TStream; out Value: Int64; Advance: Boolean = True): TMemSize;

Function Stream_GetInt64(Stream: TStream; Advance: Boolean = True): Int64;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUInt64(Stream: TStream; out Value: UInt64; Advance: Boolean = True): TMemSize;

Function Stream_GetUInt64(Stream: TStream; Advance: Boolean = True): UInt64;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Floating point numbers
-------------------------------------------------------------------------------}

Function Stream_ReadFloat32(Stream: TStream; out Value: Float32; Advance: Boolean = True): TMemSize;

Function Stream_GetFloat32(Stream: TStream; Advance: Boolean = True): Float32;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadFloat64(Stream: TStream; out Value: Float64; Advance: Boolean = True): TMemSize;

Function Stream_GetFloat64(Stream: TStream; Advance: Boolean = True): Float64;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadFloat80(Stream: TStream; out Value: Float80; Advance: Boolean = True): TMemSize;

Function Stream_GetFloat80(Stream: TStream; Advance: Boolean = True): Float80;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadDateTime(Stream: TStream; out Value: TDateTime; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function Stream_GetDateTime(Stream: TStream; Advance: Boolean = True): TDateTime;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadCurrency(Stream: TStream; out Value: Currency; Advance: Boolean = True): TMemSize;

Function Stream_GetCurrency(Stream: TStream; Advance: Boolean = True): Currency;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Characters
-------------------------------------------------------------------------------}

Function Stream_ReadAnsiChar(Stream: TStream; out Value: AnsiChar; Advance: Boolean = True): TMemSize;

Function Stream_GetAnsiChar(Stream: TStream; Advance: Boolean = True): AnsiChar;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUTF8Char(Stream: TStream; out Value: UTF8Char; Advance: Boolean = True): TMemSize;

Function Stream_GetUTF8Char(Stream: TStream; Advance: Boolean = True): UTF8Char;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadWideChar(Stream: TStream; out Value: WideChar; Advance: Boolean = True): TMemSize;

Function Stream_GetWideChar(Stream: TStream; Advance: Boolean = True): WideChar;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUnicodeChar(Stream: TStream; out Value: UnicodeChar; Advance: Boolean = True): TMemSize;

Function Stream_GetUnicodeChar(Stream: TStream; Advance: Boolean = True): UnicodeChar;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUCS4Char(Stream: TStream; out Value: UCS4Char; Advance: Boolean = True): TMemSize;

Function Stream_GetUCS4Char(Stream: TStream; Advance: Boolean = True): UCS4Char;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadChar(Stream: TStream; out Value: Char; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function Stream_GetChar(Stream: TStream; Advance: Boolean = True): Char;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Strings
-------------------------------------------------------------------------------}

Function Stream_ReadShortString(Stream: TStream; out Value: ShortString; Advance: Boolean = True): TMemSize;

Function Stream_GetShortString(Stream: TStream; Advance: Boolean = True): ShortString;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadAnsiString(Stream: TStream; out Value: AnsiString; Advance: Boolean = True): TMemSize;

Function Stream_GetAnsiString(Stream: TStream; Advance: Boolean = True): AnsiString;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUTF8String(Stream: TStream; out Value: UTF8String; Advance: Boolean = True): TMemSize;

Function Stream_GetUTF8String(Stream: TStream; Advance: Boolean = True): UTF8String;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadWideString(Stream: TStream; out Value: WideString; Advance: Boolean = True): TMemSize;

Function Stream_GetWideString(Stream: TStream; Advance: Boolean = True): WideString;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUnicodeString(Stream: TStream; out Value: UnicodeString; Advance: Boolean = True): TMemSize;

Function Stream_GetUnicodeString(Stream: TStream; Advance: Boolean = True): UnicodeString;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadUCS4String(Stream: TStream; out Value: UCS4String; Advance: Boolean = True): TMemSize;

Function Stream_GetUCS4String(Stream: TStream; Advance: Boolean = True): UCS4String;{$IFDEF CanInline} inline;{$ENDIF}

//------------------------------------------------------------------------------

Function Stream_ReadString(Stream: TStream; out Value: String; Advance: Boolean = True): TMemSize;{$IFDEF CanInline} inline;{$ENDIF}

Function Stream_GetString(Stream: TStream; Advance: Boolean = True): String;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    General data buffers
-------------------------------------------------------------------------------}

Function Stream_ReadBuffer(Stream: TStream; out Buffer; Size: TMemSize; Advance: Boolean = True): TMemSize;

implementation

uses
  Math;
  
{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

{$IF SizeOf(UInt32) <> SizeOf(UCS4Char)}  // just to be sure
  {$MESSAGE ERROR 'Type size mismatch (UInt32 - UCS4Char).'}
{$IFEND}

{===============================================================================
    Internal variables
===============================================================================}
var
  OpenByteArrayPacked:  Boolean = True;
  OpenByteArrayStride:  TMemSize = 1;

{===============================================================================
--------------------------------------------------------------------------------
                               Auxiliary routines
--------------------------------------------------------------------------------
===============================================================================}

procedure AdvancePointer(Advance: Boolean; var Ptr: Pointer; Offset: TMemSize);{$IFDEF CanInline} inline;{$ENDIF}
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
If Advance then
  Ptr := Pointer(TMemSize(PtrUInt(Ptr)) + Offset);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure AdvanceStream(Advance: Boolean; Stream: TStream; Offset: TMemSize);{$IFDEF CanInline} inline;{$ENDIF}
begin
If not Advance then
  Stream.Seek(-Int64(Offset),soCurrent);
end;

//==============================================================================

Function BoolToNum(Val: Boolean): UInt8;{$IFDEF CanInline} inline;{$ENDIF}
begin
If Val then Result := $FF
  else Result := 0;
end;

//------------------------------------------------------------------------------

Function NumToBool(Val: UInt8): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := Val <> 0;
end;

//------------------------------------------------------------------------------

procedure ClampStringLength(var StrLength: Int32);{$IFDEF CanInline} inline;{$ENDIF}
begin
If StrLength < 0 then
  StrLength := 0;
end;

//------------------------------------------------------------------------------
type
  TFloat80Overlay = packed array[0..9] of Byte;

{$IFDEF ENDIAN_BIG}
//==============================================================================

Function SwapEndian(Value: UInt16): UInt16; overload;{$IFDEF CanInline} inline;{$ENDIF}
begin
Result := UInt16(Value shl 8) or UInt16(Value shr 8);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: UInt32): UInt32; overload;
type
  Int32Rec = packed record
    LoWord: UInt16;
    HiWord: UInt16;
  end;
begin
Int32Rec(Result).HiWord := SwapEndian(Int32Rec(Value).LoWord);
Int32Rec(Result).LoWord := SwapEndian(Int32Rec(Value).HiWord);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: UInt64): UInt64; overload;
begin
Int64Rec(Result).Hi := SwapEndian(Int64Rec(Value).Lo);
Int64Rec(Result).Lo := SwapEndian(Int64Rec(Value).Hi);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function SwapEndian(Value: TFloat80Overlay): TFloat80Overlay; overload;
begin
Result[0] := Value[9];
Result[1] := Value[8];
Result[2] := Value[7];
Result[3] := Value[6];
Result[4] := Value[5];
Result[5] := Value[4];
Result[6] := Value[3];
Result[7] := Value[2];
Result[8] := Value[1];
Result[9] := Value[0];
end;

//==============================================================================

Function Ptr_WriteUInt16Arr_SwapEndian(var Dest: PUInt16; Data: PUInt16; Length: TStrSize): TMemSize;
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

Function Ptr_WriteUInt32Arr_SwapEndian(var Dest: PUInt32; Data: PUInt32; Length: TStrSize): TMemSize;
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

Function Ptr_ReadUInt16Arr_SwapEndian(var Src: PUInt16; Data: PUInt16; Length: TStrSize): TMemSize;
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

Function Ptr_ReadUInt32Arr_SwapEndian(var Src: PUInt32; Data: PUInt32; Length: TStrSize): TMemSize;
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

Function Stream_WriteUInt16Arr_SwapEndian(Stream: TStream; Data: PUInt16; Length: TStrSize): TMemSize;
var
  Buffer:   array[0..511] of UInt16;
  CopyCnt:  TStrSize;
  i:        TStrSize;
begin
{
  Buffering might not be needed when operating on TCustomMemoryStream and its
  descendants, but it should not hurt.
}
Result := 0;
while Length > 0 do
  begin
    CopyCnt := Min(Length,System.Length(Buffer));
    For i := 0 to Pred(CopyCnt) do
      begin
        Buffer[i] := SwapEndian(Data^);
        Inc(Data);
        Inc(Result,SizeOf(UInt16));
      end;
    Stream.WriteBuffer(Addr(Buffer)^,CopyCnt * SizeOf(UInt16));
    Dec(Length,CopyCnt);
  end;
end;

//------------------------------------------------------------------------------

Function Stream_WriteUInt32Arr_SwapEndian(Stream: TStream; Data: PUInt32; Length: TStrSize): TMemSize;
var
  Buffer:   array[0..255] of UInt32;
  CopyCnt:  TStrSize;
  i:        TStrSize;
begin
Result := 0;
while Length > 0 do
  begin
    CopyCnt := Min(Length,System.Length(Buffer));
    For i := 0 to Pred(CopyCnt) do
      begin
        Buffer[i] := SwapEndian(Data^);
        Inc(Data);
        Inc(Result,SizeOf(UInt32));
      end;
    Stream.WriteBuffer(Addr(Buffer)^,CopyCnt * SizeOf(UInt32));
    Dec(Length,CopyCnt);
  end;
end;

//------------------------------------------------------------------------------

Function Stream_ReadUInt16Arr_SwapEndian(Stream: TStream; Data: PUInt16; Length: TStrSize): TMemSize;
var
  Buffer:   array[0..511] of UInt16;
  CopyCnt:  TStrSize;
  i:        TStrSize;
begin
Result := 0;
while Length > 0 do
  begin
    CopyCnt := Min(Length,System.Length(Buffer));
    Stream.ReadBuffer(Addr(Buffer)^,CopyCnt * SizeOf(UInt16));
    For i := 0 to Pred(CopyCnt) do
      begin
        Data^ := SwapEndian(Buffer[i]);
        Inc(Data);
        Inc(Result,SizeOf(UInt16));
      end;
    Dec(Length,CopyCnt);
  end;
end;

//------------------------------------------------------------------------------

Function Stream_ReadUInt32Arr_SwapEndian(Stream: TStream; Data: PUInt32; Length: TStrSize): TMemSize;
var
  Buffer:   array[0..255] of UInt32;
  CopyCnt:  TStrSize;
  i:        TStrSize;
begin
Result := 0;
while Length > 0 do
  begin
    CopyCnt := Min(Length,System.Length(Buffer));
    Stream.ReadBuffer(Addr(Buffer)^,CopyCnt * SizeOf(UInt32));
    For i := 0 to Pred(CopyCnt) do
      begin
        Data^ := SwapEndian(Buffer[i]);
        Inc(Data);
        Inc(Result,SizeOf(UInt32));
      end;
    Dec(Length,CopyCnt);
  end;
end;

{$ENDIF}

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

Function StreamedSize_ShortString(const Value: ShortString): TMemSize;
begin
Result := StreamedSize_UInt8 + TMemSize(Length(Value));
end;

//------------------------------------------------------------------------------

Function StreamedSize_AnsiString(const Value: AnsiString): TMemSize;
begin
Result := StreamedSize_Int32 + TMemSize(Length(Value) * SizeOf(AnsiChar));
end;

//------------------------------------------------------------------------------

Function StreamedSize_UTF8String(const Value: UTF8String): TMemSize;
begin
Result := StreamedSize_Int32 + TMemSize(Length(Value) * SizeOf(UTF8Char));
end;

//------------------------------------------------------------------------------

Function StreamedSize_WideString(const Value: WideString): TMemSize;
begin
Result := StreamedSize_Int32 + TMemSize(Length(Value) * SizeOf(WideChar));
end;

//------------------------------------------------------------------------------

Function StreamedSize_UnicodeString(const Value: UnicodeString): TMemSize;
begin
Result := StreamedSize_Int32 + TMemSize(Length(Value) * SizeOf(UnicodeChar));
end;

//------------------------------------------------------------------------------

Function StreamedSize_UCS4String(const Value: UCS4String): TMemSize;
begin
{
  Note that UCS4 string CAN (we must cope with situation it does not) contain
  an explicit terminating zero, which is not being streamed.
}
If Length(Value) > 0 then
  Result := StreamedSize_Int32 + TMemSize(IfThen(Value[High(Value)] = 0,Pred(Length(Value)),Length(Value)) * SizeOf(UCS4Char))
else
  Result := StreamedSize_Int32;
end;

//------------------------------------------------------------------------------

Function StreamedSize_String(const Value: String): TMemSize;
begin
Result := StreamedSize_UTF8String(StrToUTF8(Value));
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
{-------------------------------------------------------------------------------
    Booleans
-------------------------------------------------------------------------------}

Function Ptr_WriteBool(var Dest: Pointer; Value: ByteBool; Advance: Boolean): TMemSize;
begin
UInt8(Dest^) := BoolToNum(Value);
Result := SizeOf(UInt8);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteBool(Dest: Pointer; Value: ByteBool): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteBool(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteBoolean(var Dest: Pointer; Value: Boolean; Advance: Boolean): TMemSize;
begin
Result := Ptr_WriteBool(Dest,Value,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteBoolean(Dest: Pointer; Value: Boolean): TMemSize;
begin
Result := Ptr_WriteBool(Dest,Value);
end;

{-------------------------------------------------------------------------------
    Integers
-------------------------------------------------------------------------------}

Function Ptr_WriteInt8(var Dest: Pointer; Value: Int8; Advance: Boolean): TMemSize;
begin
Int8(Dest^) := Value;
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteInt8(Dest: Pointer; Value: Int8): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteInt8(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUInt8(var Dest: Pointer; Value: UInt8; Advance: Boolean): TMemSize;
begin
UInt8(Dest^) := Value;
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUInt8(Dest: Pointer; Value: UInt8): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUInt8(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteInt16(var Dest: Pointer; Value: Int16; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Int16(Dest^) := Int16(SwapEndian(UInt16(Value)));
{$ELSE}
Int16(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteInt16(Dest: Pointer; Value: Int16): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteInt16(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUInt16(var Dest: Pointer; Value: UInt16; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt16(Dest^) := SwapEndian(Value);
{$ELSE}
UInt16(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUInt16(Dest: Pointer; Value: UInt16): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUInt16(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteInt32(var Dest: Pointer; Value: Int32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Int32(Dest^) := Int32(SwapEndian(UInt32(Value)));
{$ELSE}
Int32(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteInt32(Dest: Pointer; Value: Int32): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteInt32(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUInt32(var Dest: Pointer; Value: UInt32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt32(Dest^) := SwapEndian(Value);
{$ELSE}
UInt32(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUInt32(Dest: Pointer; Value: UInt32): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUInt32(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteInt64(var Dest: Pointer; Value: Int64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Int64(Dest^) := Int64(SwapEndian(UInt64(Value)));
{$ELSE}
Int64(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteInt64(Dest: Pointer; Value: Int64): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteInt64(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUInt64(var Dest: Pointer; Value: UInt64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt64(Dest^) := SwapEndian(Value);
{$ELSE}
UInt64(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUInt64(Dest: Pointer; Value: UInt64): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUInt64(Ptr,Value,False);
end;

{-------------------------------------------------------------------------------
    Floating point numbers
-------------------------------------------------------------------------------}

Function Ptr_WriteFloat32(var Dest: Pointer; Value: Float32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
{
  Casting to UInt32 is done to prevent invalid floating point operation when
  assigning byte-swapped value, which can be SNaN or denormal.
}
UInt32(Dest^) := SwapEndian(UInt32(Addr(Value)^));
{$ELSE}
Float32(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteFloat32(Dest: Pointer; Value: Float32): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteFloat32(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteFloat64(var Dest: Pointer; Value: Float64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt64(Dest^) := SwapEndian(UInt64(Addr(Value)^));
{$ELSE}
Float64(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteFloat64(Dest: Pointer; Value: Float64): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteFloat64(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteFloat80(var Dest: Pointer; Value: Float80; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
TFloat80Overlay(Dest^) := SwapEndian(TFloat80Overlay(Value));
{$ELSE}
Float80(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteFloat80(Dest: Pointer; Value: Float80): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteFloat80(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteDateTime(var Dest: Pointer; Value: TDateTime; Advance: Boolean): TMemSize;
begin
Result := Ptr_WriteFloat64(Dest,Value,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteDateTime(Dest: Pointer; Value: TDateTime): TMemSize;
begin
Result := Ptr_WriteFloat64(Dest,Value);
end;

//==============================================================================

Function Ptr_WriteCurrency(var Dest: Pointer; Value: Currency; Advance: Boolean): TMemSize;
begin
// prevent implicit conversion of currency to other types
{$IFDEF ENDIAN_BIG}
UInt64(Dest^) := SwapEndian(UInt64(Addr(Value)^));
{$ELSE}
UInt64(Dest^) := UInt64(Addr(Value)^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteCurrency(Dest: Pointer; Value: Currency): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteCurrency(Ptr,Value,False);
end;

{-------------------------------------------------------------------------------
    Characters
-------------------------------------------------------------------------------}

Function Ptr_WriteAnsiChar(var Dest: Pointer; Value: AnsiChar; Advance: Boolean): TMemSize;
begin
AnsiChar(Dest^) := Value;
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteAnsiChar(Dest: Pointer; Value: AnsiChar): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteAnsiChar(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUTF8Char(var Dest: Pointer; Value: UTF8Char; Advance: Boolean): TMemSize;
begin
UTF8Char(Dest^) := Value;
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUTF8Char(Dest: Pointer; Value: UTF8Char): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUTF8Char(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteWideChar(var Dest: Pointer; Value: WideChar; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
WideChar(Dest^) := WideChar(SwapEndian(UInt16(Value)));
{$ELSE}
WideChar(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteWideChar(Dest: Pointer; Value: WideChar): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteWideChar(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUnicodeChar(var Dest: Pointer; Value: UnicodeChar; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UnicodeChar(Dest^) := UnicodeChar(SwapEndian(UInt16(Value)));
{$ELSE}
UnicodeChar(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUnicodeChar(Dest: Pointer; Value: UnicodeChar): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUnicodeChar(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUCS4Char(var Dest: Pointer; Value: UCS4Char; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt32(Dest^) := SwapEndian(UInt32(Value));
{$ELSE}
UCS4Char(Dest^) := Value;
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUCS4Char(Dest: Pointer; Value: UCS4Char): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUCS4Char(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteChar(var Dest: Pointer; Value: Char; Advance: Boolean): TMemSize;
begin
Result := Ptr_WriteUInt16(Dest,UInt16(Ord(Value)),Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteChar(Dest: Pointer; Value: Char): TMemSize;
begin
Result := Ptr_WriteUInt16(Dest,UInt16(Ord(Value)));
end;

{-------------------------------------------------------------------------------
    Strings
-------------------------------------------------------------------------------}

Function Ptr_WriteShortString(var Dest: Pointer; const Value: ShortString; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Dest;
Result := Ptr_WriteUInt8(WorkPtr,UInt8(Length(Value)),True);
If Length(Value) > 0 then
  Inc(Result,Ptr_WriteBuffer(WorkPtr,(Addr(Value[1]))^,Length(Value),True));
If Advance then
  Dest := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteShortString(Dest: Pointer; const Value: ShortString): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteShortString(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteAnsiString(var Dest: Pointer; const Value: AnsiString; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Dest;
Result := Ptr_WriteInt32(WorkPtr,Length(Value),True);
If Length(Value) > 0 then
  Inc(Result,Ptr_WriteBuffer(WorkPtr,PAnsiChar(Value)^,Length(Value) * SizeOf(AnsiChar),True));
If Advance then
  Dest := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteAnsiString(Dest: Pointer; const Value: AnsiString): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteAnsiString(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUTF8String(var Dest: Pointer; const Value: UTF8String; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Dest;
Result := Ptr_WriteInt32(WorkPtr,Length(Value),True);
If Length(Value) > 0 then
  Inc(Result,Ptr_WriteBuffer(WorkPtr,PUTF8Char(Value)^,Length(Value) * SizeOf(UTF8Char),True));
If Advance then
  Dest := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUTF8String(Dest: Pointer; const Value: UTF8String): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUTF8String(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteWideString(var Dest: Pointer; const Value: WideString; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Dest;
Result := Ptr_WriteInt32(WorkPtr,Length(Value),True);
If Length(Value) > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Ptr_WriteUInt16Arr_SwapEndian(PUInt16(WorkPtr),PUInt16(PWideChar(Value)),Length(Value)));
{$ELSE}
  Inc(Result,Ptr_WriteBuffer(WorkPtr,PWideChar(Value)^,Length(Value) * SizeOf(WideChar),True));
{$ENDIF}
If Advance then
  Dest := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteWideString(Dest: Pointer; const Value: WideString): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteWideString(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUnicodeString(var Dest: Pointer; const Value: UnicodeString; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
begin
WorkPtr := Dest;
Result := Ptr_WriteInt32(WorkPtr,Length(Value),True);
If Length(Value) > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Ptr_WriteUInt16Arr_SwapEndian(PUInt16(WorkPtr),PUInt16(PUnicodeChar(Value)),Length(Value)));
{$ELSE}
  Inc(Result,Ptr_WriteBuffer(WorkPtr,PUnicodeChar(Value)^,Length(Value) * SizeOf(UnicodeChar),True));
{$ENDIF}
If Advance then
  Dest := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUnicodeString(Dest: Pointer; const Value: UnicodeString): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUnicodeString(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteUCS4String(var Dest: Pointer; const Value: UCS4String; Advance: Boolean): TMemSize;
var
  WorkPtr:  Pointer;
  TrueLen:  TStrSize;
begin
WorkPtr := Dest;
If Length(Value) > 0 then
  begin
    If Value[High(Value)] = 0 then
      TrueLen := Pred(Length(Value))
    else
      TrueLen := Length(Value);
    If TrueLen > 0 then
      begin
        Result := Ptr_WriteInt32(WorkPtr,Int32(TrueLen),True);
      {$IFDEF ENDIAN_BIG}
        Inc(Result,Ptr_WriteUInt32Arr_SwapEndian(PUInt32(WorkPtr),PUInt32(Addr(Value[Low(Value)])),TrueLen));
      {$ELSE}
        Inc(Result,Ptr_WriteBuffer(WorkPtr,Addr(Value[Low(Value)])^,TrueLen * SizeOf(UCS4Char),True));
      {$ENDIF}
      end
    else Result := Ptr_WriteInt32(WorkPtr,0,True);
  end
else Result := Ptr_WriteInt32(WorkPtr,0,True);
If Advance then
  Dest := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteUCS4String(Dest: Pointer; const Value: UCS4String): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteUCS4String(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_WriteString(var Dest: Pointer; const Value: String; Advance: Boolean): TMemSize;
begin
Result := Ptr_WriteUTF8String(Dest,StrToUTF8(Value),Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteString(Dest: Pointer; const Value: String): TMemSize;
begin
Result := Ptr_WriteUTF8String(Dest,StrToUTF8(Value));
end;

{-------------------------------------------------------------------------------
    General data buffers
-------------------------------------------------------------------------------}

Function Ptr_WriteBuffer(var Dest: Pointer; const Buffer; Size: TMemSize; Advance: Boolean): TMemSize;
begin
Move(Buffer,Dest^,Size);
Result := Size;
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteBuffer(Dest: Pointer; const Buffer; Size: TMemSize): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteBuffer(Ptr,Buffer,Size,False);
end;

//==============================================================================

Function Ptr_WriteBytes(var Dest: Pointer; const Value: array of UInt8; Advance: Boolean): TMemSize;
var
  WorkPtr:  PUInt8;
  i:        Integer;
begin
If OpenByteArrayPacked then
  begin
    // write whole array
    If Length(Value) > 0 then
      Move(Value[Low(Value)],Dest^,Length(Value));
    Result := TMemSize(Length(Value));
    AdvancePointer(Advance,Dest,Result);
  end
else
  begin
  {
    write byte-by-byte

    As we are writing into memory, there is no point in buffering the
    individual bytes.
  }
    WorkPtr := Dest;
    For i := Low(Value) to High(Value) do
      begin
        WorkPtr^ := Value[i];
        Inc(WorkPtr);
      end;
    Result := TMemSize(Length(Value));
    If Advance then
      Dest := WorkPtr;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_WriteBytes(Dest: Pointer; const Value: array of UInt8): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_WriteBytes(Ptr,Value,False);
end;

//==============================================================================

Function Ptr_FillBytes(var Dest: Pointer; Count: TMemSize; Value: UInt8; Advance: Boolean): TMemSize;
begin
FillChar(Dest^,Count,Value);
Result := Count;
AdvancePointer(Advance,Dest,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_FillBytes(Dest: Pointer; Count: TMemSize; Value: UInt8): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Dest;
Result := Ptr_FillBytes(Ptr,Count,Value,False);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Memory reading
--------------------------------------------------------------------------------
===============================================================================}

Function Ptr_ReadBool(var Src: Pointer; out Value: ByteBool; Advance: Boolean): TMemSize;
begin
Value := NumToBool(UInt8(Src^));
Result := SizeOf(UInt8);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadBool(Src: Pointer; out Value: ByteBool): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadBool(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetBool(var Src: Pointer; Advance: Boolean): ByteBool;
begin
Ptr_ReadBool(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetBool(Src: Pointer): ByteBool;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadBool(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadBoolean(var Src: Pointer; out Value: Boolean; Advance: Boolean): TMemSize;
var
  TempBool: ByteBool;
begin
Result := Ptr_ReadBool(Src,TempBool,Advance);
Value := TempBool;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadBoolean(Src: Pointer; out Value: Boolean): TMemSize;
var
  TempBool: ByteBool;
begin
Result := Ptr_ReadBool(Src,TempBool);
Value := TempBool;
end;

//------------------------------------------------------------------------------

Function Ptr_GetBoolean(var Src: Pointer; Advance: Boolean): Boolean;
begin
Result := Ptr_GetBool(Src,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetBoolean(Src: Pointer): Boolean;
begin
Result := Ptr_GetBool(Src);
end;

{-------------------------------------------------------------------------------
    Integers
-------------------------------------------------------------------------------}

Function Ptr_ReadInt8(var Src: Pointer; out Value: Int8; Advance: Boolean): TMemSize;
begin
Value := Int8(Src^);
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadInt8(Src: Pointer; out Value: Int8): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadInt8(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetInt8(var Src: Pointer; Advance: Boolean): Int8;
begin
Ptr_ReadInt8(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetInt8(Src: Pointer): Int8;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadInt8(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUInt8(var Src: Pointer; out Value: UInt8; Advance: Boolean): TMemSize;
begin
Value := UInt8(Src^);
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUInt8(Src: Pointer; out Value: UInt8): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUInt8(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUInt8(var Src: Pointer; Advance: Boolean): UInt8;
begin
Ptr_ReadUInt8(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUInt8(Src: Pointer): UInt8;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUInt8(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadInt16(var Src: Pointer; out Value: Int16; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int16(SwapEndian(UInt16(Src^)));
{$ELSE}
Value := Int16(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadInt16(Src: Pointer; out Value: Int16): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadInt16(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetInt16(var Src: Pointer; Advance: Boolean): Int16;
begin
Ptr_ReadInt16(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetInt16(Src: Pointer): Int16;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadInt16(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUInt16(var Src: Pointer; out Value: UInt16; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(UInt16(Src^));
{$ELSE}
Value := UInt16(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUInt16(Src: Pointer; out Value: UInt16): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUInt16(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUInt16(var Src: Pointer; Advance: Boolean): UInt16;
begin
Ptr_ReadUInt16(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUInt16(Src: Pointer): UInt16;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUInt16(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadInt32(var Src: Pointer; out Value: Int32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int32(SwapEndian(UInt32(Src^)));
{$ELSE}
Value := Int32(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadInt32(Src: Pointer; out Value: Int32): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadInt32(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetInt32(var Src: Pointer; Advance: Boolean): Int32;
begin
Ptr_ReadInt32(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetInt32(Src: Pointer): Int32;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadInt32(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUInt32(var Src: Pointer; out Value: UInt32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(UInt32(Src^));
{$ELSE}
Value := UInt32(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUInt32(Src: Pointer; out Value: UInt32): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUInt32(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUInt32(var Src: Pointer; Advance: Boolean): UInt32;
begin
Ptr_ReadUInt32(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUInt32(Src: Pointer): UInt32;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUInt32(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadInt64(var Src: Pointer; out Value: Int64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int64(SwapEndian(UInt64(Src^)));
{$ELSE}
Value := Int64(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadInt64(Src: Pointer; out Value: Int64): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadInt64(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetInt64(var Src: Pointer; Advance: Boolean): Int64;
begin
Ptr_ReadInt64(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetInt64(Src: Pointer): Int64;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadInt64(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUInt64(var Src: Pointer; out Value: UInt64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(UInt64(Src^));
{$ELSE}
Value := UInt64(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUInt64(Src: Pointer; out Value: UInt64): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUInt64(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUInt64(var Src: Pointer; Advance: Boolean): UInt64;
begin
Ptr_ReadUInt64(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUInt64(Src: Pointer): UInt64;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUInt64(Ptr,Result,False);
end;

{-------------------------------------------------------------------------------
    Floating point numbers
-------------------------------------------------------------------------------}

Function Ptr_ReadFloat32(var Src: Pointer; out Value: Float32; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt32(Addr(Value)^) := SwapEndian(UInt32(Src^));
{$ELSE}
Value := Float32(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadFloat32(Src: Pointer; out Value: Float32): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadFloat32(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetFloat32(var Src: Pointer; Advance: Boolean): Float32;
begin
Ptr_ReadFloat32(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetFloat32(Src: Pointer): Float32;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadFloat32(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadFloat64(var Src: Pointer; out Value: Float64; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
UInt64(Addr(Value)^) := SwapEndian(UInt64(Src^));
{$ELSE}
Value := Float64(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadFloat64(Src: Pointer; out Value: Float64): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadFloat64(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetFloat64(var Src: Pointer; Advance: Boolean): Float64;
begin
Ptr_ReadFloat64(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetFloat64(Src: Pointer): Float64;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadFloat64(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadFloat80(var Src: Pointer; out Value: Float80; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
TFloat80Overlay(Addr(Value)^) := SwapEndian(TFloat80Overlay(Src^));
{$ELSE}
Value := Float80(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadFloat80(Src: Pointer; out Value: Float80): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadFloat80(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetFloat80(var Src: Pointer; Advance: Boolean): Float80;
begin
Ptr_ReadFloat80(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetFloat80(Src: Pointer): Float80;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadFloat80(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadDateTime(var Src: Pointer; out Value: TDateTime; Advance: Boolean): TMemSize;
begin
Result := Ptr_ReadFloat64(Src,Float64(Value),Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadDateTime(Src: Pointer; out Value: TDateTime): TMemSize;
begin
Result := Ptr_ReadFloat64(Src,Float64(Value));
end;

//------------------------------------------------------------------------------

Function Ptr_GetDateTime(var Src: Pointer; Advance: Boolean): TDateTime;
begin
Result := Ptr_GetFloat64(Src,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetDateTime(Src: Pointer): TDateTime;
begin
Result := Ptr_GetFloat64(Src);
end;

//==============================================================================

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
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadCurrency(Src: Pointer; out Value: Currency): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadCurrency(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetCurrency(var Src: Pointer; Advance: Boolean): Currency;
begin
Ptr_ReadCurrency(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetCurrency(Src: Pointer): Currency;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadCurrency(Ptr,Result,False);
end;

{-------------------------------------------------------------------------------
    Characters
-------------------------------------------------------------------------------}

Function Ptr_ReadAnsiChar(var Src: Pointer; out Value: AnsiChar; Advance: Boolean): TMemSize;
begin
Value := AnsiChar(Src^);
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadAnsiChar(Src: Pointer; out Value: AnsiChar): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadAnsiChar(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetAnsiChar(var Src: Pointer; Advance: Boolean): AnsiChar;
begin
Ptr_ReadAnsiChar(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetAnsiChar(Src: Pointer): AnsiChar;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadAnsiChar(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUTF8Char(var Src: Pointer; out Value: UTF8Char; Advance: Boolean): TMemSize;
begin
Value := UTF8Char(Src^);
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUTF8Char(Src: Pointer; out Value: UTF8Char): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUTF8Char(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUTF8Char(var Src: Pointer; Advance: Boolean): UTF8Char;
begin
Ptr_ReadUTF8Char(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUTF8Char(Src: Pointer): UTF8Char;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUTF8Char(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadWideChar(var Src: Pointer; out Value: WideChar; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := WideChar(SwapEndian(UInt16(Src^)));
{$ELSE}
Value := WideChar(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadWideChar(Src: Pointer; out Value: WideChar): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadWideChar(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetWideChar(var Src: Pointer; Advance: Boolean): WideChar;
begin
Ptr_ReadWideChar(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetWideChar(Src: Pointer): WideChar;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadWideChar(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUnicodeChar(var Src: Pointer; out Value: UnicodeChar; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := UnicodeChar(SwapEndian(UInt16(Src^)));
{$ELSE}
Value := UnicodeChar(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUnicodeChar(Src: Pointer; out Value: UnicodeChar): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUnicodeChar(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUnicodeChar(var Src: Pointer; Advance: Boolean): UnicodeChar;
begin
Ptr_ReadUnicodeChar(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUnicodeChar(Src: Pointer): UnicodeChar;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUnicodeChar(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUCS4Char(var Src: Pointer; out Value: UCS4Char; Advance: Boolean): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := UCS4Char(SwapEndian(UInt32(Src^)));
{$ELSE}
Value := UCS4Char(Src^);
{$ENDIF}
Result := SizeOf(Value);
AdvancePointer(Advance,Src,Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUCS4Char(Src: Pointer; out Value: UCS4Char): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUCS4Char(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUCS4Char(var Src: Pointer; Advance: Boolean): UCS4Char;
begin
Ptr_ReadUCS4Char(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUCS4Char(Src: Pointer): UCS4Char;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUCS4Char(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadChar(var Src: Pointer; out Value: Char; Advance: Boolean): TMemSize;
var
  Temp: UInt16;
begin
Result := Ptr_ReadUInt16(Src,Temp,Advance);
Value := Char(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadChar(Src: Pointer; out Value: Char): TMemSize;
var
  Temp: UInt16;
begin
Result := Ptr_ReadUInt16(Src,Temp);
Value := Char(Temp);
end;

//------------------------------------------------------------------------------

Function Ptr_GetChar(var Src: Pointer; Advance: Boolean): Char;
begin
Result := Char(Ptr_GetUInt16(Src,Advance));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetChar(Src: Pointer): Char;
begin
Result := Char(Ptr_GetUInt16(Src));
end;

{-------------------------------------------------------------------------------
    Strings
-------------------------------------------------------------------------------}

Function Ptr_ReadShortString(var Src: Pointer; out Value: ShortString; Advance: Boolean): TMemSize;
var
  StrLength:  UInt8;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadUInt8(WorkPtr,StrLength,True);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
  Inc(Result,Ptr_ReadBuffer(WorkPtr,Addr(Value[1])^,StrLength,True));
If Advance then
  Src := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadShortString(Src: Pointer; out Value: ShortString): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadShortString(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetShortString(var Src: Pointer; Advance: Boolean): ShortString;
begin
Ptr_ReadShortString(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetShortString(Src: Pointer): ShortString;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadShortString(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadAnsiString(var Src: Pointer; out Value: AnsiString; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
ClampStringLength(StrLength);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
  Inc(Result,Ptr_ReadBuffer(WorkPtr,PAnsiChar(Value)^,StrLength * SizeOf(AnsiChar),True));
If Advance then
  Src := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadAnsiString(Src: Pointer; out Value: AnsiString): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadAnsiString(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetAnsiString(var Src: Pointer; Advance: Boolean): AnsiString;
begin
Ptr_ReadAnsiString(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetAnsiString(Src: Pointer): AnsiString;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadAnsiString(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUTF8String(var Src: Pointer; out Value: UTF8String; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
ClampStringLength(StrLength);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
  Inc(Result,Ptr_ReadBuffer(WorkPtr,PUTF8Char(Value)^,StrLength * SizeOf(UTF8Char),True));
If Advance then
  Src := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUTF8String(Src: Pointer; out Value: UTF8String): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUTF8String(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUTF8String(var Src: Pointer; Advance: Boolean): UTF8String;
begin
Ptr_ReadUTF8String(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUTF8String(Src: Pointer): UTF8String;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUTF8String(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadWideString(var Src: Pointer; out Value: WideString; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
ClampStringLength(StrLength);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Ptr_ReadUInt16Arr_SwapEndian(PUInt16(WorkPtr),PUInt16(PWideChar(Value)),StrLength));
{$ELSE}
  Inc(Result,Ptr_ReadBuffer(WorkPtr,PWideChar(Value)^,StrLength * SizeOf(WideChar),True));
{$ENDIF}
If Advance then
  Src := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadWideString(Src: Pointer; out Value: WideString): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadWideString(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetWideString(var Src: Pointer; Advance: Boolean): WideString;
begin
Ptr_ReadWideString(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetWideString(Src: Pointer): WideString;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadWideString(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUnicodeString(var Src: Pointer; out Value: UnicodeString; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
ClampStringLength(StrLength);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Ptr_ReadUInt16Arr_SwapEndian(PUInt16(WorkPtr),PUInt16(PUnicodeChar(Value)),StrLength));
{$ELSE}
  Inc(Result,Ptr_ReadBuffer(WorkPtr,PUnicodeChar(Value)^,StrLength * SizeOf(UnicodeChar),True));
{$ENDIF}
If Advance then
  Src := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUnicodeString(Src: Pointer; out Value: UnicodeString): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUnicodeString(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUnicodeString(var Src: Pointer; Advance: Boolean): UnicodeString;
begin
Ptr_ReadUnicodeString(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUnicodeString(Src: Pointer): UnicodeString;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUnicodeString(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadUCS4String(var Src: Pointer; out Value: UCS4String; Advance: Boolean): TMemSize;
var
  StrLength:  Int32;
  WorkPtr:    Pointer;
begin
WorkPtr := Src;
Result := Ptr_ReadInt32(WorkPtr,StrLength,True);
ClampStringLength(StrLength);
Value := nil;
SetLength(Value,StrLength + 1);
Value[High(Value)] := 0;
If StrLength > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Ptr_ReadUInt32Arr_SwapEndian(PUInt32(WorkPtr),PUInt32(Addr(Value[Low(Value)])),StrLength));
{$ELSE}
  Inc(Result,Ptr_ReadBuffer(WorkPtr,Addr(Value[Low(Value)])^,StrLength * SizeOf(UCS4Char),True));
{$ENDIF}
If Advance then
  Src := WorkPtr;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadUCS4String(Src: Pointer; out Value: UCS4String): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadUCS4String(Ptr,Value,False);
end;

//------------------------------------------------------------------------------

Function Ptr_GetUCS4String(var Src: Pointer; Advance: Boolean): UCS4String;
begin
Ptr_ReadUCS4String(Src,Result,Advance);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetUCS4String(Src: Pointer): UCS4String;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Ptr_ReadUCS4String(Ptr,Result,False);
end;

//==============================================================================

Function Ptr_ReadString(var Src: Pointer; out Value: String; Advance: Boolean): TMemSize;
var
  TempStr:  UTF8String;
begin
Result := Ptr_ReadUTF8String(Src,TempStr,Advance);
Value := UTF8ToStr(TempStr);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadString(Src: Pointer; out Value: String): TMemSize;
var
  TempStr:  UTF8String;
begin
Result := Ptr_ReadUTF8String(Src,TempStr);
Value := UTF8ToStr(TempStr);
end;

//------------------------------------------------------------------------------

Function Ptr_GetString(var Src: Pointer; Advance: Boolean): String;
begin
Result := UTF8ToStr(Ptr_GetUTF8String(Src,Advance));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_GetString(Src: Pointer): String;
begin
Result := UTF8ToStr(Ptr_GetUTF8String(Src));
end;

{-------------------------------------------------------------------------------
    General data buffers
-------------------------------------------------------------------------------}

Function Ptr_ReadBuffer(var Src: Pointer; out Buffer; Size: TMemSize; Advance: Boolean): TMemSize;
begin
If Size > 0 then
  begin
    Move(Src^,Addr(Buffer)^,Size);
    Result := Size;
    AdvancePointer(Advance,Src,Result);
  end
else Result := 0;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Ptr_ReadBuffer(Src: Pointer; out Buffer; Size: TMemSize): TMemSize;
var
  Ptr:  Pointer;
begin
Ptr := Src;
Result := Ptr_ReadBuffer(Ptr,Buffer,Size,False);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Stream writing
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    Booleans
-------------------------------------------------------------------------------}

Function Stream_WriteBool(Stream: TStream; Value: ByteBool; Advance: Boolean): TMemSize;
var
  Temp: UInt8;
begin
Temp := BoolToNum(Value);
Stream.WriteBuffer(Temp,SizeOf(Temp));
Result := SizeOf(Temp);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteBoolean(Stream: TStream; Value: Boolean; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteBool(Stream,Value,Advance);
end;

{-------------------------------------------------------------------------------
    Integers
-------------------------------------------------------------------------------}

Function Stream_WriteInt8(Stream: TStream; Value: Int8; Advance: Boolean): TMemSize;
begin
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUInt8(Stream: TStream; Value: UInt8; Advance: Boolean): TMemSize;
begin
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteInt16(Stream: TStream; Value: Int16; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int16(SwapEndian(UInt16(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUInt16(Stream: TStream; Value: UInt16; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteInt32(Stream: TStream; Value: Int32; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int32(SwapEndian(UInt32(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUInt32(Stream: TStream; Value: UInt32; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteInt64(Stream: TStream; Value: Int64; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := Int64(SwapEndian(UInt64(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUInt64(Stream: TStream; Value: UInt64; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

{-------------------------------------------------------------------------------
    Floating point numbers
-------------------------------------------------------------------------------}

Function Stream_WriteFloat32(Stream: TStream; Value: Float32; Advance: Boolean = True): TMemSize;
var
  Temp: UInt32 absolute Value;
begin
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(Temp);
{$ENDIF}
Stream.WriteBuffer(Temp,SizeOf(Temp));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteFloat64(Stream: TStream; Value: Float64; Advance: Boolean = True): TMemSize;
var
  Temp: UInt64 absolute Value;
begin
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(Temp);
{$ENDIF}
Stream.WriteBuffer(Temp,SizeOf(Temp));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteFloat80(Stream: TStream; Value: Float80; Advance: Boolean = True): TMemSize;
var
  Temp: TFloat80Overlay absolute Value;
begin
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(Temp);
{$ENDIF}
Stream.WriteBuffer(Temp,SizeOf(Temp));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteDateTime(Stream: TStream; Value: TDateTime; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteFloat64(Stream,Value,Advance);
end;

//==============================================================================

Function Stream_WriteCurrency(Stream: TStream; Value: Currency; Advance: Boolean = True): TMemSize;
var
  Temp: UInt64 absolute Value;
begin
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(Temp);
{$ENDIF}
Stream.WriteBuffer(Temp,SizeOf(Temp));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

{-------------------------------------------------------------------------------
    Characters
-------------------------------------------------------------------------------}

Function Stream_WriteAnsiChar(Stream: TStream; Value: AnsiChar; Advance: Boolean): TMemSize;
begin
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUTF8Char(Stream: TStream; Value: UTF8Char; Advance: Boolean): TMemSize;
begin
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteWideChar(Stream: TStream; Value: WideChar; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := WideChar(SwapEndian(UInt16(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUnicodeChar(Stream: TStream; Value: UnicodeChar; Advance: Boolean = True): TMemSize;
begin
{$IFDEF ENDIAN_BIG}
Value := UnicodeChar(SwapEndian(UInt16(Value)));
{$ENDIF}
Stream.WriteBuffer(Value,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUCS4Char(Stream: TStream; Value: UCS4Char; Advance: Boolean = True): TMemSize;
var
  Temp: UInt32 absolute Value;
begin
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(Temp);
{$ENDIF}
Stream.WriteBuffer(Temp,SizeOf(Temp));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteChar(Stream: TStream; Value: Char; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteUInt16(Stream,UInt16(Ord(Value)),Advance);
end;

{-------------------------------------------------------------------------------
    Strings
-------------------------------------------------------------------------------}

Function Stream_WriteShortString(Stream: TStream; const Value: ShortString; Advance: Boolean): TMemSize;
begin
Result := Stream_WriteUInt8(Stream,UInt8(Length(Value)),True);
If Length(Value) > 0 then
  Inc(Result,Stream_WriteBuffer(Stream,Addr(Value[1])^,Length(Value),True));
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteAnsiString(Stream: TStream; const Value: AnsiString; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteInt32(Stream,Length(Value),True);
If Length(Value) > 0 then
  Inc(Result,Stream_WriteBuffer(Stream,PAnsiChar(Value)^,Length(Value) * SizeOf(AnsiChar),True));
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUTF8String(Stream: TStream; const Value: UTF8String; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteInt32(Stream,Length(Value),True);
If Length(Value) > 0 then
  Inc(Result,Stream_WriteBuffer(Stream,PUTF8Char(Value)^,Length(Value) * SizeOf(UTF8Char),True));
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteWideString(Stream: TStream; const Value: WideString; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteInt32(Stream,Length(Value),True);
If Length(Value) > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Stream_WriteUInt16Arr_SwapEndian(Stream,PUInt16(PWideChar(Value)),Length(Value)));
{$ELSE}
  Inc(Result,Stream_WriteBuffer(Stream,PWideChar(Value)^,Length(Value) * SizeOf(WideChar),True));
{$ENDIF}
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUnicodeString(Stream: TStream; const Value: UnicodeString; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteInt32(Stream,Length(Value),True);
If Length(Value) > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Stream_WriteUInt16Arr_SwapEndian(Stream,PUInt16(PUnicodeChar(Value)),Length(Value)));
{$ELSE}
  Inc(Result,Stream_WriteBuffer(Stream,PUnicodeChar(Value)^,Length(Value) * SizeOf(UnicodeChar),True));
{$ENDIF}
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteUCS4String(Stream: TStream; const Value: UCS4String; Advance: Boolean = True): TMemSize;
var
  TrueLen:  TStrSize;
begin
If Length(Value) > 0 then
  begin
    If Value[High(Value)] = 0 then
      TrueLen := Pred(Length(Value))
    else
      TrueLen := Length(Value);
    If TrueLen > 0 then
      begin
        Result := Stream_WriteInt32(Stream,Int32(TrueLen),True);
      {$IFDEF ENDIAN_BIG}
        Inc(Result,Stream_WriteUInt32Arr_SwapEndian(Stream,PUInt32(Addr(Value[Low(Value)])),TrueLen));
      {$ELSE}
        Inc(Result,Stream_WriteBuffer(Stream,Addr(Value[Low(Value)])^,TrueLen * SizeOf(UCS4Char),True));
      {$ENDIF}
      end
    else Result := Stream_WriteInt32(Stream,0,True);
  end
else Result := Stream_WriteInt32(Stream,0,True);
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteString(Stream: TStream; const Value: String; Advance: Boolean = True): TMemSize;
begin
Result := Stream_WriteUTF8String(Stream,StrToUTF8(Value),Advance);
end;

{-------------------------------------------------------------------------------
    General data buffers
-------------------------------------------------------------------------------}

Function Stream_WriteBuffer(Stream: TStream; const Buffer; Size: TMemSize; Advance: Boolean): TMemSize;
begin
Stream.WriteBuffer(Buffer,Size);
Result := Size;
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_WriteBytes(Stream: TStream; const Value: array of UInt8; Advance: Boolean): TMemSize;
var
  Buffer:   packed array[0..1023] of UInt8; // buffer is on stack, keep it small
  Remain:   Integer;
  Offset:   Integer;
  CopyCnt:  Integer;
  i:        Integer;
begin
If not OpenByteArrayPacked then
  begin
    Remain := Length(Value);
    Offset := 0;
    while Remain > 0 do
      begin
        CopyCnt := Min(Remain,SizeOf(Buffer));
        For i := 0 to Pred(CopyCnt) do
          Buffer[i] := Value[Offset + i];
        Stream.WriteBuffer(Addr(Buffer)^,CopyCnt);
        Dec(Remain,CopyCnt);
        Inc(Offset,CopyCnt);
      end;
  end
else If Length(Value) > 0 then
  Stream.WriteBuffer(Value[Low(Value)],Length(Value));
Result := TMemSize(Length(Value));
AdvanceStream(Advance,Stream,Result);
end;

//==============================================================================

Function Stream_FillBytes(Stream: TStream; Count: TMemSize; Value: UInt8; Advance: Boolean): TMemSize;

  Function Min(A,B: TMemSize): TMemSize;
  begin
    If A < B then
      Result := A
    else
      Result := B;
  end;

var
  Buffer:   packed array[0..1023] of UInt8;
  CopyCnt:  Integer;
begin
If Count >= SizeOf(Buffer) then
  FillChar(Addr(Buffer)^,SizeOf(Buffer),Value)
else
  FillChar(Addr(Buffer)^,Count,Value);
Result := 0;
while Count > 0 do
  begin
    CopyCnt := Min(Count,TMemSize(SizeOf(Buffer)));
    Stream.WriteBuffer(Buffer,CopyCnt);
    Dec(Count,TMemSize(CopyCnt));
    Inc(Result,TMemSize(CopyCnt));
  end;
AdvanceStream(Advance,Stream,Result);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Stream reading
--------------------------------------------------------------------------------
===============================================================================}
{-------------------------------------------------------------------------------
    Booleans
-------------------------------------------------------------------------------}

Function Stream_ReadBool(Stream: TStream; out Value: ByteBool; Advance: Boolean): TMemSize;
var
  Temp: UInt8;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(Temp));
Result := SizeOf(Temp);
Value := NumToBool(Temp);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetBool(Stream: TStream; Advance: Boolean = True): ByteBool;
begin
Stream_ReadBool(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadBoolean(Stream: TStream; out Value: Boolean; Advance: Boolean = True): TMemSize;
var
  TempBool: ByteBool;
begin
Result := Stream_ReadBool(Stream,TempBool,Advance);
Value := TempBool;
end;

//------------------------------------------------------------------------------

Function Stream_GetBoolean(Stream: TStream; Advance: Boolean = True): Boolean;
begin
Result := Stream_GetBool(Stream,Advance);
end;

{-------------------------------------------------------------------------------
    Integers
-------------------------------------------------------------------------------}

Function Stream_ReadInt8(Stream: TStream; out Value: Int8; Advance: Boolean): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetInt8(Stream: TStream; Advance: Boolean = True): Int8;
begin
Stream_ReadInt8(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUInt8(Stream: TStream; out Value: UInt8; Advance: Boolean): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUInt8(Stream: TStream; Advance: Boolean = True): UInt8;
begin
Stream_ReadUInt8(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadInt16(Stream: TStream; out Value: Int16; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
{$IFDEF ENDIAN_BIG}
Value := Int16(SwapEndian(UInt16(Value)));
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetInt16(Stream: TStream; Advance: Boolean = True): Int16;
begin
Stream_ReadInt16(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUInt16(Stream: TStream; out Value: UInt16; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUInt16(Stream: TStream; Advance: Boolean = True): UInt16;
begin
Stream_ReadUInt16(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadInt32(Stream: TStream; out Value: Int32; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
{$IFDEF ENDIAN_BIG}
Value := Int32(SwapEndian(UInt32(Value)));
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetInt32(Stream: TStream; Advance: Boolean = True): Int32;
begin
Stream_ReadInt32(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUInt32(Stream: TStream; out Value: UInt32; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUInt32(Stream: TStream; Advance: Boolean = True): UInt32;
begin
Stream_ReadUInt32(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadInt64(Stream: TStream; out Value: Int64; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
{$IFDEF ENDIAN_BIG}
Value := Int64(SwapEndian(UInt64(Value)));
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetInt64(Stream: TStream; Advance: Boolean = True): Int64;
begin
Stream_ReadInt64(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUInt64(Stream: TStream; out Value: UInt64; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
{$IFDEF ENDIAN_BIG}
Value := SwapEndian(Value);
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUInt64(Stream: TStream; Advance: Boolean = True): UInt64;
begin
Stream_ReadUInt64(Stream,Result,Advance);
end;

{-------------------------------------------------------------------------------
    Floating point numbers
-------------------------------------------------------------------------------}

Function Stream_ReadFloat32(Stream: TStream; out Value: Float32; Advance: Boolean = True): TMemSize;
var
  Temp: UInt32 absolute Value;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(Temp));
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(Temp);
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetFloat32(Stream: TStream; Advance: Boolean = True): Float32;
begin
Stream_ReadFloat32(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadFloat64(Stream: TStream; out Value: Float64; Advance: Boolean = True): TMemSize;
var
  Temp: UInt64 absolute Value;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(Temp));
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(Temp);
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetFloat64(Stream: TStream; Advance: Boolean = True): Float64;
begin
Stream_ReadFloat64(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadFloat80(Stream: TStream; out Value: Float80; Advance: Boolean = True): TMemSize;
var
  Temp: TFloat80Overlay absolute Value;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(Temp));
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(Temp);
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetFloat80(Stream: TStream; Advance: Boolean = True): Float80;
begin
Stream_ReadFloat80(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadDateTime(Stream: TStream; out Value: TDateTime; Advance: Boolean = True): TMemSize;
begin
Result := Stream_ReadFloat64(Stream,Float64(Value),Advance);
end;

//------------------------------------------------------------------------------

Function Stream_GetDateTime(Stream: TStream; Advance: Boolean = True): TDateTime;
begin
Result := Stream_GetFloat64(Stream,Advance);
end;

//==============================================================================

Function Stream_ReadCurrency(Stream: TStream; out Value: Currency; Advance: Boolean = True): TMemSize;
var
  Temp: UInt64 absolute Value;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(Temp));
{$IFDEF ENDIAN_BIG}
Temp := SwapEndian(Temp);
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetCurrency(Stream: TStream; Advance: Boolean = True): Currency;
begin
Stream_ReadCurrency(Stream,Result,Advance);
end;

{-------------------------------------------------------------------------------
    Characters
-------------------------------------------------------------------------------}

Function Stream_ReadAnsiChar(Stream: TStream; out Value: AnsiChar; Advance: Boolean): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetAnsiChar(Stream: TStream; Advance: Boolean = True): AnsiChar;
begin
Stream_ReadAnsiChar(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUTF8Char(Stream: TStream; out Value: UTF8Char; Advance: Boolean): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUTF8Char(Stream: TStream; Advance: Boolean = True): UTF8Char;
begin
Stream_ReadUTF8Char(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadWideChar(Stream: TStream; out Value: WideChar; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
{$IFDEF ENDIAN_BIG}
Value := WideChar(SwapEndian(UInt16(Value)));
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetWideChar(Stream: TStream; Advance: Boolean = True): WideChar;
begin
Stream_ReadWideChar(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUnicodeChar(Stream: TStream; out Value: UnicodeChar; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
{$IFDEF ENDIAN_BIG}
Value := UnicodeChar(SwapEndian(UInt16(Value)));
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUnicodeChar(Stream: TStream; Advance: Boolean = True): UnicodeChar;
begin
Stream_ReadUnicodeChar(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUCS4Char(Stream: TStream; out Value: UCS4Char; Advance: Boolean = True): TMemSize;
begin
Stream.ReadBuffer(Addr(Value)^,SizeOf(Value));
{$IFDEF ENDIAN_BIG}
Value := UCS4Char(SwapEndian(UInt32(Value)));
{$ENDIF}
Result := SizeOf(Value);
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUCS4Char(Stream: TStream; Advance: Boolean = True): UCS4Char;
begin
Stream_ReadUCS4Char(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadChar(Stream: TStream; out Value: Char; Advance: Boolean = True): TMemSize;
var
  Temp: UInt16;
begin
Result := Stream_ReadUInt16(Stream,Temp,Advance);
Value := Char(Temp);
end;

//------------------------------------------------------------------------------

Function Stream_GetChar(Stream: TStream; Advance: Boolean = True): Char;
begin
Result := Char(Stream_GetUInt16(Stream,Advance));
end;

{-------------------------------------------------------------------------------
    Strings
-------------------------------------------------------------------------------}

Function Stream_ReadShortString(Stream: TStream; out Value: ShortString; Advance: Boolean): TMemSize;
var
  StrLength:  UInt8;
begin
Result := Stream_ReadUInt8(Stream,StrLength,True);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
  Inc(Result,Stream_ReadBuffer(Stream,Addr(Value[1])^,StrLength,True));
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetShortString(Stream: TStream; Advance: Boolean = True): ShortString;
begin
Stream_ReadShortString(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadAnsiString(Stream: TStream; out Value: AnsiString; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
ClampStringLength(StrLength);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
  Inc(Result,Stream_ReadBuffer(Stream,PAnsiChar(Value)^,StrLength * SizeOf(AnsiChar),True));
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetAnsiString(Stream: TStream; Advance: Boolean = True): AnsiString;
begin
Stream_ReadAnsiString(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUTF8String(Stream: TStream; out Value: UTF8String; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
ClampStringLength(StrLength);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
  Inc(Result,Stream_ReadBuffer(Stream,PUTF8Char(Value)^,StrLength * SizeOf(UTF8Char),True));
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUTF8String(Stream: TStream; Advance: Boolean = True): UTF8String;
begin
Stream_ReadUTF8String(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadWideString(Stream: TStream; out Value: WideString; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
ClampStringLength(StrLength);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Stream_ReadUInt16Arr_SwapEndian(Stream,PUInt16(PWideChar(Value)),StrLength));
{$ELSE}
  Inc(Result,Stream_ReadBuffer(Stream,PWideChar(Value)^,StrLength * SizeOf(WideChar),True));
{$ENDIF}
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetWideString(Stream: TStream; Advance: Boolean = True): WideString;
begin
Stream_ReadWideString(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUnicodeString(Stream: TStream; out Value: UnicodeString; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
ClampStringLength(StrLength);
Value := '';
SetLength(Value,StrLength);
If StrLength > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Stream_ReadUInt16Arr_SwapEndian(Stream,PUInt16(PUnicodeChar(Value)),StrLength));
{$ELSE}
  Inc(Result,Stream_ReadBuffer(Stream,PUnicodeChar(Value)^,StrLength * SizeOf(UnicodeChar),True));
{$ENDIF}
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUnicodeString(Stream: TStream; Advance: Boolean = True): UnicodeString;
begin
Stream_ReadUnicodeString(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadUCS4String(Stream: TStream; out Value: UCS4String; Advance: Boolean = True): TMemSize;
var
  StrLength:  Int32;
begin
Result := Stream_ReadInt32(Stream,StrLength,True);
ClampStringLength(StrLength);
Value := nil;
SetLength(Value,StrLength + 1);
Value[High(Value)] := 0;
If StrLength > 0 then
{$IFDEF ENDIAN_BIG}
  Inc(Result,Stream_ReadUInt32Arr_SwapEndian(Stream,PUInt32(Addr(Value[Low(Value)])),StrLength));
{$ELSE}
  Inc(Result,Stream_ReadBuffer(Stream,Addr(Value[Low(Value)])^,StrLength * SizeOf(UCS4Char),True));
{$ENDIF}
AdvanceStream(Advance,Stream,Result);
end;

//------------------------------------------------------------------------------

Function Stream_GetUCS4String(Stream: TStream; Advance: Boolean = True): UCS4String;
begin
Stream_ReadUCS4String(Stream,Result,Advance);
end;

//==============================================================================

Function Stream_ReadString(Stream: TStream; out Value: String; Advance: Boolean = True): TMemSize;
var
  TempStr:  UTF8String;
begin
Result := Stream_ReadUTF8String(Stream,TempStr,Advance);
Value := UTF8ToStr(TempStr);
end;

//------------------------------------------------------------------------------

Function Stream_GetString(Stream: TStream; Advance: Boolean = True): String;
begin
Result := UTF8ToStr(Stream_GetUTF8String(Stream,Advance));
end;

{-------------------------------------------------------------------------------
    General data buffers
-------------------------------------------------------------------------------}

Function Stream_ReadBuffer(Stream: TStream; out Buffer; Size: TMemSize; Advance: Boolean): TMemSize;
begin
Stream.ReadBuffer(Addr(Buffer)^,Size);
Result := Size;
AdvanceStream(Advance,Stream,Result);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                Unit preparation
--------------------------------------------------------------------------------
===============================================================================}

procedure UnitInitialize;

  Function GetOBAStride(const Arr: array of UInt8): Integer;
  begin
    If Length(Arr) >= 2 then
    {$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
      Result := Integer(PtrUInt(Addr(Arr[1])) - PtrUInt(Addr(Arr[0])))
    {$IFDEF FPCDWM}{$POP}{$ENDIF}
    else
      Result := 0;
  end;

begin
OpenByteArrayStride := GetOBAStride([0,0]);
// following should be always true, but the paranoia needs feeding...
OpenByteArrayPacked := OpenByteArrayStride = 1;
end;

//==============================================================================

initialization
  UnitInitialize;

end.
