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

    Variants are stored in a litle more compley way - first a byte denoting
    the type of the variant is stored (note that value of this byte do NOT
    correspond to TVarType value), directly followed by the value itself.
    The value is stored as if it was a normal variable (eg. for varWord variant
    the function [Ptr/Stream]_WriteUInt16 is called).

      NOTE - given the implementation, variant being written into memory
             (function Ptr_WriteVariant) must not be larger than 2GiB-1 bytes.

    For variant arrays, immediately after the type byte a dimension count is
    stored (int32), followed by an array of indices bounds (two int32 values
    for each dimension, first low bound followed by a high bound, ordered from
    lowest dimension to highest). After this an array of items is stored, each
    item stored as a separate variant. When saving the items, the right-most
    index is incremented first.

      NOTE - only standard variant types are supported, and from them only the
             "streamable" types (boolean, integers, floats, strings) and their
             arrays are allowed - so no empty/null vars, interfaces, objects,
             errors, and so on.

    Also, since older compilers do not support all in-here used variant types
    (namely varUInt64 and varUString), there are some specific rules in effect
    when saving and loading such types in programs compiled by those
    compilers...

      Unsupported types are not saved at all, simply because a variant of that
      type cannot be even created (note that the type UInt64 is sometimes
      declared only an alias for Int64, then it will be saved as Int64).

      When loading unsupported varUInt64, it will be loaded as Int64 (with the
      same BINARY data, ie. not necessarily the same numerical value).

      When loading unsupported varUString (unicode string), it is normally
      loaded, but the returned variant will be of type varOleStr (wide string).

    Parameter Advance in writing and reading functions indicates whether the
    position in stream being written to or read from (or the passed memory
    pointer) can be advanced by number of bytes written or read. When set to
    true, the position (pointer) is advanced, when false, the position is the
    same after the call as was before it.

    Return value of almost all reading and writing functions is number of bytes
    written or read. The exception to this are read functions that are directly
    returning the value being read.

  Version 1.9 (2023-01-22)

  Last change 2023-01-22

  �2015-2023 Franti�ek Milt

  Contacts:
    Franti�ek Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.BinaryStreaming

  Dependencies:
    AuxClasses         - github.com/TheLazyTomcat/Lib.AuxClasses  
    AuxTypes           - github.com/TheLazyTomcat/Lib.AuxTypes
    StaticMemoryStream - github.com/TheLazyTomcat/Lib.StaticMemoryStream
    StrRect            - github.com/TheLazyTomcat/Lib.StrRect

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

  EBSUnsupportedVarType = class(EBSException);

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

//------------------------------------------------------------------------------

Function StreamedSize_Variant(const Value: Variant): TMemSize;

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

//------------------------------------------------------------------------------

Function Ptr_WriteVariant(var Dest: Pointer; const Value: Variant; Advance: Boolean): TMemSize; overload;
Function Ptr_WriteVariant(Dest: Pointer; const Value: Variant): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}

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

//------------------------------------------------------------------------------

Function Ptr_ReadVariant(var Src: Pointer; out Value: Variant; Advance: Boolean): TMemSize; overload;
Function Ptr_ReadVariant(Src: Pointer; out Value: Variant): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadVariant(var Src: Pointer; Advance: Boolean): Variant; overload;{$IFDEF CanInline} inline; {$ENDIF}
Function Ptr_ReadVariant(Src: Pointer): Variant; overload;{$IFDEF CanInline} inline; {$ENDIF}

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

//------------------------------------------------------------------------------

Function Stream_WriteVariant(Stream: TStream; const Value: Variant; Advance: Boolean = True): TMemSize;

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

//------------------------------------------------------------------------------

Function Stream_ReadVariant(Stream: TStream; out Value: Variant; Advance: Boolean = True): TMemSize; overload;
Function Stream_ReadVariant(Stream: TStream; Advance: Boolean = True): Variant; overload;{$IFDEF CanInline} inline; {$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                                 TCustomStreamer
--------------------------------------------------------------------------------
===============================================================================}
type
  // TValueType is only used internally
  TValueType = (vtShortString,vtAnsiString,vtUTF8String,vtWideString,
                vtUnicodeString,vtUCS4String,vtString,vtFillBytes,vtBytes,
                vtPrimitive1B,vtPrimitive2B,vtPrimitive4B,vtPrimitive8B,
                vtPrimitive10B,vtVariant);

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
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  public
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
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
    Function WriteVariant(const Value: Variant; Advance: Boolean = True): TMemSize; virtual;
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
    Function ReadVariant(out Value: Variant; Advance: Boolean = True): TMemSize; overload; virtual;
    Function ReadVariant(Advance: Boolean = True): Variant; overload; virtual;
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
    Function GetStartPtr: Pointer; virtual;
    procedure SetBookmark(Index: Integer; Value: Int64); override;
    Function GetCurrentPosition: Int64; override;
    procedure SetCurrentPosition(NewPosition: Int64); override;
    Function WriteValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize; override;
    Function ReadValue(Value: Pointer; Advance: Boolean; Size: TMemSize; ValueType: TValueType): TMemSize; override;
    procedure Initialize(Memory: Pointer); reintroduce; virtual;
  public
    constructor Create(Memory: Pointer); overload;
    Function IndexOfBookmark(Position: Int64): Integer; override;
    Function AddBookmark(Position: Int64): Integer; override;
    Function RemoveBookmark(Position: Int64; RemoveAll: Boolean = True): Integer; override;
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
    procedure Initialize(Target: TStream); reintroduce; virtual;
  public
    constructor Create(Target: TStream);
    property Target: TStream read fTarget;
  end;

implementation

uses
  Variants,
  StrRect, StaticMemoryStream;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
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
                                Variant utilities
--------------------------------------------------------------------------------
===============================================================================}
{
  Following constants and functions are here to strictly separate implementation
  details from the actual streaming.
}
const
  BS_VARTYPENUM_BOOLEN   = 0;
  BS_VARTYPENUM_SHORTINT = 1;
  BS_VARTYPENUM_SMALLINT = 2;
  BS_VARTYPENUM_INTEGER  = 3;
  BS_VARTYPENUM_INT64    = 4;
  BS_VARTYPENUM_BYTE     = 5;
  BS_VARTYPENUM_WORD     = 6;
  BS_VARTYPENUM_LONGWORD = 7;
  BS_VARTYPENUM_UINT64   = 8;
  BS_VARTYPENUM_SINGLE   = 9;
  BS_VARTYPENUM_DOUBLE   = 10;
  BS_VARTYPENUM_CURRENCY = 11;
  BS_VARTYPENUM_DATE     = 12;
  BS_VARTYPENUM_OLESTR   = 13;
  BS_VARTYPENUM_STRING   = 14;
  BS_VARTYPENUM_USTRING  = 15;
  BS_VARTYPENUM_VARIANT  = 16;

  BS_VARTYPENUM_TYPEMASK = $3F;
  BS_VARTYPENUM_ARRAY    = $80;

//------------------------------------------------------------------------------

Function VarTypeToInt(VarType: TVarType): UInt8;
begin
// varByRef is ignored
case VarType and varTypeMask of
  varBoolean:   Result := BS_VARTYPENUM_BOOLEN;
  varShortInt:  Result := BS_VARTYPENUM_SHORTINT;
  varSmallint:  Result := BS_VARTYPENUM_SMALLINT;
  varInteger:   Result := BS_VARTYPENUM_INTEGER;
  varInt64:     Result := BS_VARTYPENUM_INT64;
  varByte:      Result := BS_VARTYPENUM_BYTE;
  varWord:      Result := BS_VARTYPENUM_WORD;
  varLongWord:  Result := BS_VARTYPENUM_LONGWORD;
{$IF Declared(varUInt64)}
  varUInt64:    Result := BS_VARTYPENUM_UINT64;
{$IFEND}
  varSingle:    Result := BS_VARTYPENUM_SINGLE;
  varDouble:    Result := BS_VARTYPENUM_DOUBLE;
  varCurrency:  Result := BS_VARTYPENUM_CURRENCY;
  varDate:      Result := BS_VARTYPENUM_DATE;
  varOleStr:    REsult := BS_VARTYPENUM_OLESTR;
  varString:    Result := BS_VARTYPENUM_STRING;
{$IF Declared(varUString)}
  varUString:   Result := BS_VARTYPENUM_USTRING;
{$IFEND}
  varVariant:   Result := BS_VARTYPENUM_VARIANT;
else
  raise EBSUnsupportedVarType.CreateFmt('VarTypeToInt: Unsupported variant type (%d).',[VarType]);
end;
If VarType and varArray <> 0 then
  Result := Result or BS_VARTYPENUM_ARRAY;
end;

//------------------------------------------------------------------------------  

Function IntToVarType(Value: UInt8): TVarType;
begin
case Value and BS_VARTYPENUM_TYPEMASK of
  BS_VARTYPENUM_BOOLEN:   Result := varBoolean;
  BS_VARTYPENUM_SHORTINT: Result := varShortInt;
  BS_VARTYPENUM_SMALLINT: Result := varSmallint;
  BS_VARTYPENUM_INTEGER:  Result := varInteger;
  BS_VARTYPENUM_INT64:    Result := varInt64;
  BS_VARTYPENUM_BYTE:     Result := varByte;
  BS_VARTYPENUM_WORD:     Result := varWord;
  BS_VARTYPENUM_LONGWORD: Result := varLongWord;
  BS_VARTYPENUM_UINT64:   Result := {$IF Declared(varUInt64)}varUInt64{$ELSE}varInt64{$IFEND};
  BS_VARTYPENUM_SINGLE:   Result := varSingle;
  BS_VARTYPENUM_DOUBLE:   Result := varDouble;
  BS_VARTYPENUM_CURRENCY: Result := varCurrency;
  BS_VARTYPENUM_DATE:     Result := varDate;
  BS_VARTYPENUM_OLESTR:   Result := varOleStr;
  BS_VARTYPENUM_STRING:   Result := varString;
  BS_VARTYPENUM_USTRING:  Result := {$IF Declared(varUString)}varUString{$ELSE}varOleStr{$IFEND};
  BS_VARTYPENUM_VARIANT:  Result := varVariant;
else
  raise EBSUnsupportedVarType.CreateFmt('IntToVarType: Unsupported variant type number (%d).',[Value]);
end;
If Value and BS_VARTYPENUM_ARRAY <> 0 then
  Result := Result or varArray;
end;


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
If Length(Str) > 0 then
  Result := StreamedSize_Int32 + TMemSize(Pred(Length(Str)) * SizeOf(UCS4Char))
else
  Result := StreamedSize_Int32;
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

//==============================================================================

Function StreamedSize_Variant(const Value: Variant): TMemSize;
var
  Dimensions: Integer;
  Indices:    array of Integer;

  Function StreamedSize_VarArrayDimension(Dimension: Integer): TMemSize;
  var
    Index:  Integer;
  begin
    Result := 0;
    For Index := VarArrayLowBound(Value,Dimension) to VarArrayHighBound(Value,Dimension) do
      begin
        Indices[Pred(Dimension)] := Index;
        If Dimension >= Dimensions then
          Inc(Result,StreamedSize_Variant(VarArrayGet(Value,Indices)))
        else
          Inc(Result,StreamedSize_VarArrayDimension(Succ(Dimension)));
      end;
  end;

begin
If VarIsArray(Value) then
  begin
    // array, need to do complete scan to get full size
    Dimensions := VarArrayDimCount(Value);
    Result := 5{type (byte) + number of dimensions (int32)} +
              (Dimensions * 8{2 * int32 (indices bounds)});
    If Dimensions > 0 then
      begin
        SetLength(Indices,Dimensions);
        Inc(Result,StreamedSize_VarArrayDimension(1));
      end;
  end
else
  begin
    // simple type
    If VarType(Value) <> (varVariant or varByRef) then
      begin
        case VarType(Value) and varTypeMask of
          // +1 is for the variant type byte
          varBoolean:   Result := StreamedSize_Boolean + 1;
          varShortInt:  Result := StreamedSize_Int8 + 1;
          varSmallint:  Result := StreamedSize_Int16 + 1;
          varInteger:   Result := StreamedSize_Int32 + 1;
          varInt64:     Result := StreamedSize_Int64 + 1;
          varByte:      Result := StreamedSize_UInt8 + 1;
          varWord:      Result := StreamedSize_UInt16 + 1;
          varLongWord:  Result := StreamedSize_UInt32 + 1;
        {$IF Declared(varUInt64)}
          varUInt64:    Result := StreamedSize_UInt64 + 1;
        {$IFEND}
          varSingle:    Result := StreamedSize_Float32 + 1;
          varDouble:    Result := StreamedSize_Float64 + 1;
          varCurrency:  Result := StreamedSize_Currency + 1;
          varDate:      Result := StreamedSize_DateTime + 1;
          varOleStr:    Result := StreamedSize_WideString(Value) + 1;
          varString:    Result := StreamedSize_AnsiString(AnsiString(Value)) + 1;
        {$IF Declared(varUInt64)}
          varUString:   Result := StreamedSize_UnicodeString(Value) + 1;
        {$IFEND}
        else
          raise EBSUnsupportedVarType.CreateFmt('StreamedSize_Variant: Cannot scan variant of this type (%d).',[VarType(Value) and varTypeMask]);
        end;
      end
    else Result := StreamedSize_Variant(Variant(FindVarData(Value)^));
  end;
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
WorkPtr := Dest;
Result := Ptr_WriteUInt8(WorkPtr,UInt8(Length(Str)),True);
Inc(Result,Ptr_WriteBuffer(WorkPtr,(Addr(Str[1]))^,Length(Str),True));
If Advance then
  Dest := WorkPtr;
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
WorkPtr := Dest;
Result := Ptr_WriteInt32(WorkPtr,Length(Str),True);
Inc(Result,Ptr_WriteBuffer(WorkPtr,PAnsiChar(Str)^,Length(Str) * SizeOf(AnsiChar),True));
If Advance then
  Dest := WorkPtr;
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
WorkPtr := Dest;
Result := Ptr_WriteInt32(WorkPtr,Length(Str),True);
Inc(Result,Ptr_WriteBuffer(WorkPtr,PUTF8Char(Str)^,Length(Str) * SizeOf(UTF8Char),True));
If Advance then
  Dest := WorkPtr;
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
WorkPtr := Dest;
Result := Ptr_WriteInt32(WorkPtr,Length(Str),True);
{$IFDEF ENDIAN_BIG}
Inc(Result,Ptr_WriteUTF16LE(PUInt16(WorkPtr),PUInt16(PWideChar(Str)),Length(Str)));
{$ELSE}
Inc(Result,Ptr_WriteBuffer(WorkPtr,PWideChar(Str)^,Length(Str) * SizeOf(WideChar),True));
{$ENDIF}
If Advance then
  Dest := WorkPtr;
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
WorkPtr := Dest;
Result := Ptr_WriteInt32(WorkPtr,Length(Str),True);
{$IFDEF ENDIAN_BIG}
Inc(Result,Ptr_WriteUTF16LE(PUInt16(WorkPtr),PUInt16(PUnicodeChar(Str)),Length(Str)));
{$ELSE}
Inc(Result,Ptr_WriteBuffer(WorkPtr,PUnicodeChar(Str)^,Length(Str) * SizeOf(UnicodeChar),True));
{$ENDIF}
If Advance then
  Dest := WorkPtr;
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
  WorkPtr:  Pointer;
  i:        Integer;
begin
Result := 0;
WorkPtr := Dest;
For i := Low(Value) to High(Value) do
  Inc(Result,Ptr_WriteUInt8(WorkPtr,Value[i],True));
If Advance then
  Dest := WorkPtr;
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

//==============================================================================

Function Ptr_WriteVariant(var Dest: Pointer; const Value: Variant; Advance: Boolean): TMemSize;
var
  Stream: TWritableStaticMemoryStream;
begin
Stream := TWritableStaticMemoryStream.Create(Dest,TMemSize($7FFFFFFF){2GiB - 1});
try
  Stream.Seek(0,SoBeginning);
  Result := Stream_WriteVariant(Stream,Value,True);
  If Advance then
    Dest := Stream.Address;
finally
  Stream.Free;
end;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_WriteVariant(Dest: Pointer; const Value: Variant): TMemSize;
begin
Result := Ptr_WriteVariant(Dest,Value,False);
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

//==============================================================================

Function Ptr_ReadVariant(var Src: Pointer; out Value: Variant; Advance: Boolean): TMemSize;
var
  Stream: TStaticMemoryStream;
begin
Stream := TStaticMemoryStream.Create(Src,TMemSize($7FFFFFFF){2GiB - 1});
try
  Stream.Seek(0,SoBeginning);
  Result := Stream_ReadVariant(Stream,Value,True);
  If Advance then
    Src := Stream.Address;
finally
  Stream.Free;
end;
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadVariant(Src: Pointer; out Value: Variant): TMemSize; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Result := Ptr_ReadVariant(Src,Value,False);
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Ptr_ReadVariant(var Src: Pointer; Advance: Boolean): Variant; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Ptr_ReadVariant(Src,Result,Advance);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

{$IFDEF FPCDWM}{$PUSH}W5058{$ENDIF}
Function Ptr_ReadVariant(Src: Pointer): Variant; overload;{$IFDEF CanInline} inline; {$ENDIF}
begin
Ptr_ReadVariant(Src,Result,False);
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

//==============================================================================

Function Stream_WriteVariant(Stream: TStream; const Value: Variant; Advance: Boolean = True): TMemSize;

  Function VarToBool: Boolean;
  begin
    Result := Value;
  end;

  Function VarToInt64: Int64;
  begin
    Result := Value;
  end;

  Function VarToUInt64: UInt64;
  begin
    Result := Value;
  end;

var
  Dimensions: Integer;
  i:          Integer;       
  Indices:    array of Integer;

  Function WriteVarArrayDimension(Dimension: Integer): TMemSize;
  var
    Index:  Integer;
  begin
    Result := 0;
    For Index := VarArrayLowBound(Value,Dimension) to VarArrayHighBound(Value,Dimension) do
      begin
        Indices[Pred(Dimension)] := Index;
        If Dimension >= Dimensions then
          Inc(Result,Stream_WriteVariant(Stream,VarArrayGet(Value,Indices),True))
        else
          Inc(Result,WriteVarArrayDimension(Succ(Dimension)));
      end;
  end;

begin
If VarIsArray(Value) then
  begin
    // array type
    Result := Stream_WriteUInt8(Stream,VarTypeToInt(VarType(Value)),True);
    Dimensions := VarArrayDimCount(Value);
    // write number of dimensions
    Inc(Result,Stream_WriteInt32(Stream,Dimensions,True));
    // write indices bounds (pairs of integers - low and high bound) for each dimension
    For i := 1 to Dimensions do
      begin
        Inc(Result,Stream_WriteInt32(Stream,VarArrayLowBound(Value,i),True));
        Inc(Result,Stream_WriteInt32(Stream,VarArrayHighBound(Value,i),True));
      end;
  {
    Now (recursively :P) write data af any exist.

    Btw. I know about VarArrayLock/VarArrayUnlock and pointer directly to the
    data, but I want it to be fool proof, therefore using VarArrayGet instead
    of direct access,
  }
    If Dimensions > 0 then
      begin
        SetLength(Indices,Dimensions);
        Inc(Result,WriteVarArrayDimension(1));
      end;
  end
else
  begin
    // simple type
    If VarType(Value) <> (varVariant or varByRef) then
      begin
        Result := Stream_WriteUInt8(Stream,VarTypeToInt(VarType(Value)),True);
        case VarType(Value) and varTypeMask of
          varBoolean:   Inc(Result,Stream_WriteBool(Stream,VarToBool,True));
          varShortInt:  Inc(Result,Stream_WriteInt8(Stream,Value,True));
          varSmallint:  Inc(Result,Stream_WriteInt16(Stream,Value,True));
          varInteger:   Inc(Result,Stream_WriteInt32(Stream,Value,True));
          varInt64:     Inc(Result,Stream_WriteInt64(Stream,VarToInt64,True));
          varByte:      Inc(Result,Stream_WriteUInt8(Stream,Value,True));
          varWord:      Inc(Result,Stream_WriteUInt16(Stream,Value,True));
          varLongWord:  Inc(Result,Stream_WriteUInt32(Stream,Value,True));
        {$IF Declared(varUInt64)}
          varUInt64:    Inc(Result,Stream_WriteUInt64(Stream,VarToUInt64,True));
        {$IFEND}
          varSingle:    Inc(Result,Stream_WriteFloat32(Stream,Value,True));
          varDouble:    Inc(Result,Stream_WriteFloat64(Stream,Value,True));
          varCurrency:  Inc(Result,Stream_WriteCurrency(Stream,Value,True));
          varDate:      Inc(Result,Stream_WriteDateTime(Stream,Value,True));
          varOleStr:    Inc(Result,Stream_WriteWideString(Stream,Value,True));
          varString:    Inc(Result,Stream_WriteAnsiString(Stream,AnsiString(Value),True));
        {$IF Declared(varUString)}
          varUString:   Inc(Result,Stream_WriteUnicodeString(Stream,Value,True));
        {$IFEND}
        else
          raise EBSUnsupportedVarType.CreateFmt('Stream_WriteVariant: Cannot write variant of this type (%d).',[VarType(Value) and varTypeMask]);
        end;
      end
    else Result := Stream_WriteVariant(Stream,Variant(FindVarData(Value)^),True);
  end;
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 Stream reading
--------------------------------------------------------------------------------
===============================================================================}

Function Stream_ReadBool(Stream: TStream; out Value: ByteBool; Advance: Boolean = True): TMemSize;
var
  Temp: UInt8;
begin
Stream.ReadBuffer(Addr(Temp)^,SizeOf(Temp));
Result := SizeOf(Temp);
Value := NumToBool(Temp);
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

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

//==============================================================================

Function Stream_ReadVariant(Stream: TStream; out Value: Variant; Advance: Boolean = True): TMemSize;
var
  VariantTypeInt: UInt8;
  Dimensions:     Integer;
  i:              Integer;
  IndicesBounds:  array of PtrInt;
  Indices:        array of Integer;
  WideStrTemp:    WideString;
  AnsiStrTemp:    AnsiString;
  UnicodeStrTemp: UnicodeString;

  Function ReadVarArrayDimension(Dimension: Integer): TMemSize;
  var
    Index:    Integer;
    TempVar:  Variant;
  begin
    Result := 0;
    For Index := VarArrayLowBound(Value,Dimension) to VarArrayHighBound(Value,Dimension) do
      begin
        Indices[Pred(Dimension)] := Index;
        If Dimension >= Dimensions then
          begin
            Inc(Result,Stream_ReadVariant(Stream,TempVar,True));
            VarArrayPut(Value,TempVar,Indices);
          end
        else Inc(Result,ReadVarArrayDimension(Succ(Dimension)));
      end;
  end;

begin
VariantTypeInt := Stream_ReadUInt8(Stream,True);
If VariantTypeInt and BS_VARTYPENUM_ARRAY <> 0 then
  begin
    // array
    Dimensions := Stream_ReadInt32(Stream,True);
    Result := 5 + (Dimensions * 8);
    If Dimensions > 0 then
      begin
        // read indices bounds
        SetLength(IndicesBounds,Dimensions * 2);
        For i := Low(IndicesBounds) to High(IndicesBounds) do
          IndicesBounds[i] := PtrInt(Stream_ReadInt32(Stream,True));
        // create the array
        Value := VarArrayCreate(IndicesBounds,IntToVarType(VariantTypeInt and BS_VARTYPENUM_TYPEMASK));
        // read individual dimensions/items
        SetLength(Indices,Dimensions);
        Inc(Result,ReadVarArrayDimension(1));
      end;
  end
else
  begin
    // simple type
    TVarData(Value).vType := IntToVarType(VariantTypeInt);
    case VariantTypeInt and BS_VARTYPENUM_TYPEMASK of
      BS_VARTYPENUM_BOOLEN:   begin
                                TVarData(Value).vBoolean := Stream_ReadBool(Stream,True);
                                // +1 is for the already read variable type byte
                                Result := StreamedSize_Bool + 1;
                              end;
      BS_VARTYPENUM_SHORTINT: Result := Stream_ReadInt8(Stream,TVarData(Value).vShortInt,True) + 1;
      BS_VARTYPENUM_SMALLINT: Result := Stream_ReadInt16(Stream,TVarData(Value).vSmallInt,True) + 1;
      BS_VARTYPENUM_INTEGER:  Result := Stream_ReadInt32(Stream,TVarData(Value).vInteger,True) + 1;
      BS_VARTYPENUM_INT64:    Result := Stream_ReadInt64(Stream,TVarData(Value).vInt64,True) + 1;
      BS_VARTYPENUM_BYTE:     Result := Stream_ReadUInt8(Stream,TVarData(Value).vByte,True) + 1;
      BS_VARTYPENUM_WORD:     Result := Stream_ReadUInt16(Stream,TVarData(Value).vWord,True) + 1;
      BS_VARTYPENUM_LONGWORD: Result := Stream_ReadUInt32(Stream,TVarData(Value).vLongWord,True) + 1;
      BS_VARTYPENUM_UINT64: {$IF Declared(varUInt64)}
                              Result := Stream_ReadUInt64(Stream,TVarData(Value).{$IFDEF FPC}vQWord{$ELSE}vUInt64{$ENDIF},True) + 1;
                            {$ELSE}
                              Result := Stream_ReadUInt64(Stream,UInt64(TVarData(Value).vInt64),True) + 1;
                            {$IFEND}
      BS_VARTYPENUM_SINGLE:   Result := Stream_ReadFloat32(Stream,TVarData(Value).vSingle,True) + 1;
      BS_VARTYPENUM_DOUBLE:   Result := Stream_ReadFloat64(Stream,TVarData(Value).vDouble,True) + 1;
      BS_VARTYPENUM_CURRENCY: Result := Stream_ReadCurrency(Stream,TVarData(Value).vCurrency,True) + 1;
      BS_VARTYPENUM_DATE:     Result := Stream_ReadDateTime(Stream,TVarData(Value).vDate,True) + 1;
      BS_VARTYPENUM_OLESTR:   begin
                                Result := Stream_ReadWideString(Stream,WideStrTemp,True) + 1;
                                Value := VarAsType(WideStrTemp,varOleStr);
                              end;
      BS_VARTYPENUM_STRING:   begin
                                Result := Stream_ReadAnsiString(Stream,AnsiStrTemp,True) + 1;
                                Value := VarAsType(AnsiStrTemp,varString);
                              end;
      BS_VARTYPENUM_USTRING:  begin
                                Result := Stream_ReadUnicodeString(Stream,UnicodeStrTemp,True) + 1;
                              {$IF Declared(varUString)}
                                Value := VarAsType(UnicodeStrTemp,varUString);
                              {$ELSE}
                                Value := VarAsType(UnicodeStrTemp,varOleStr);
                              {$IFEND}
                              end;
    else
      raise EBSUnsupportedVarType.CreateFmt('Stream_ReadVariant: Cannot read variant of this type number (%d).',[VariantTypeInt and BS_VARTYPENUM_TYPEMASK]);
    end;
  end;
If not Advance then
  Stream.Seek(-Int64(Result),soCurrent);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function Stream_ReadVariant(Stream: TStream; Advance: Boolean = True): Variant;
begin
Stream_ReadVariant(Stream,Result,Advance);
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

//------------------------------------------------------------------------------

procedure TCustomStreamer.Initialize;
begin
SetLength(fBookmarks,0);
fCount := 0;
fStartPosition := 0;
end;

//------------------------------------------------------------------------------

procedure TCustomStreamer.Finalize;
begin
// nothing to do
end;

{-------------------------------------------------------------------------------
    TCustomStreamer - public methods
-------------------------------------------------------------------------------}

destructor TCustomStreamer.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

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
  If CheckIndex(Result) then
    DeleteBookMark(Result);
until not CheckIndex(Result) or not RemoveAll;
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

//------------------------------------------------------------------------------

Function TCustomStreamer.WriteVariant(const Value: Variant; Advance: Boolean = True): TMemSize;
begin
Result := WriteValue(@Value,Advance,0,vtVariant);
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

//------------------------------------------------------------------------------

Function TCustomStreamer.ReadVariant(out Value: Variant; Advance: Boolean = True): TMemSize;
begin
Result := ReadValue(@Value,Advance,0,vtVariant);
end;

//   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---   ---

Function TCustomStreamer.ReadVariant(Advance: Boolean = True): Variant;
begin
ReadValue(@Result,Advance,0,vtVariant)
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
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Pointer(PtrUInt(fStartPosition));
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
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
Result := Int64(PtrUInt(fCurrentPtr));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TMemoryStreamer.SetCurrentPosition(NewPosition: Int64);
begin
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
fCurrentPtr := Pointer(PtrUInt(NewPosition));
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
  vtVariant:        Result := Ptr_WriteVariant(fCurrentPtr,Variant(Value^),Advance);
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
  vtVariant:        Result := Ptr_ReadVariant(fCurrentPtr,Variant(Value^),Advance);
else
 {vtFillBytes, vtBytes}
  Result := Ptr_ReadBuffer(fCurrentPtr,Value^,Size,Advance);
end;
end;

//------------------------------------------------------------------------------

procedure TMemoryStreamer.Initialize(Memory: Pointer);
begin
inherited Initialize;
fCurrentPtr := Memory;
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
fStartPosition := Int64(PtrUInt(Memory));
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{-------------------------------------------------------------------------------
    TMemoryStreamer - public methods
-------------------------------------------------------------------------------}

constructor TMemoryStreamer.Create(Memory: Pointer);
begin
inherited Create;
Initialize(Memory);
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
  vtVariant:        Result := Stream_WriteVariant(fTarget,Variant(Value^),Advance);
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
  vtVariant:        Result := Stream_ReadVariant(fTarget,Variant(Value^),Advance);
else
 {vtFillBytes,vtBytes}
  Result := Stream_ReadBuffer(fTarget,Value^,Size,Advance);
end;
end;

//------------------------------------------------------------------------------

procedure TStreamStreamer.Initialize(Target: TStream);
begin
inherited Initialize;
fTarget := Target;
fStartPosition := Target.Position;
end;

{-------------------------------------------------------------------------------
    TStreamStreamer - public methods
-------------------------------------------------------------------------------}

constructor TStreamStreamer.Create(Target: TStream);
begin
inherited Create;
Initialize(Target);
end;

end.

