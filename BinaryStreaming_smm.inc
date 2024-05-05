{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Binary streaming - streamer macro-methods template

  Version 2.0.3 (2024-04-14)

  Last change 2024-04-14

  ©2015-2024 František Milt

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
    AuxClasses    - github.com/TheLazyTomcat/Lib.AuxClasses
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect       - github.com/TheLazyTomcat/Lib.StrRect

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol BinaryStreaming_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
{!tun_end!} // ignore this line
{$IFDEF BS_INC_MM}
var
  OldPosition:  Int64;
begin
OldPosition := Self.Position;
// macro type selection
{$IF Defined(BS_INC_MM_AT)}
MoveAt(Position);
{$ELSEIF Defined(BS_INC_MM_AT_OFF)}
MoveAtOffset(Offset);
{$ELSEIF Defined(BS_INC_MM_TO)}
MoveTo(ID);    
{$ELSEIF Defined(BS_INC_MM_TO_IDX)}
MoveToIndex(Index);
{$ELSEIF Defined(BS_INC_MM_AT_ADDR)}
MoveAt(Address);
{$ELSE}
  {$MESSAGE FATAL 'unknown macro type'}
{$IFEND}
{$IF not(Defined(BS_INC_MM_WRITE) or Defined(BS_INC_MM_READ) or Defined(BS_INC_MM_GET))}
  {$MESSAGE FATAL 'unknown macro operation'}
{$IFEND}
 // value type selection
{$IF Defined(BS_INC_MM_BOOL)}
// macro operation selection
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteBool(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadBool(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetBool(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_BOOLEAN)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteBoolean(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadBoolean(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetBoolean(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_INT8)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteInt8(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadInt8(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetInt8(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UINT8)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUInt8(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUInt8(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUInt8(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_INT16)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteInt16(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadInt16(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetInt16(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UINT16)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUInt16(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUInt16(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUInt16(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_INT32)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteInt32(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadInt32(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetInt32(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UINT32)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUInt32(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUInt32(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUInt32(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_INT64)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteInt64(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadInt64(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetInt64(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UINT64)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUInt64(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUInt64(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUInt64(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_FLOAT32)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteFloat32(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadFloat32(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetFloat32(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_FLOAT64)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteFloat64(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadFloat64(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetFloat64(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_FLOAT80)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteFloat80(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadFloat80(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetFloat80(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_DATETIME)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteDateTime(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadDateTime(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetDateTime(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_CURRENCY)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteCurrency(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadCurrency(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetCurrency(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_ANSICHAR)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteAnsiChar(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadAnsiChar(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetAnsiChar(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UTF8CHAR)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUTF8Char(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUTF8Char(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUTF8Char(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_WIDECHAR)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteWideChar(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadWideChar(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetWideChar(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UNICODECHAR)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUnicodeChar(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUnicodeChar(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUnicodeChar(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UCS4CHAR)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUCS4Char(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUCS4Char(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUCS4Char(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_CHAR)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteChar(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadChar(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetChar(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_SHORTSTRING)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteShortString(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadShortString(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetShortString(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_ANSISTRING)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteAnsiString(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadAnsiString(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetAnsiString(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UTF8STRING)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUTF8String(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUTF8String(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUTF8String(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_WIDESTRING)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteWideString(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadWideString(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetWideString(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UNICODESTRING)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUnicodeString(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUnicodeString(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUnicodeString(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_UCS4STRING)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteUCS4String(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadUCS4String(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetUCS4String(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_STRING)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteString(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadString(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetString(True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_BUFFER)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteBuffer(Buffer,Size,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadBuffer(Buffer,Size,True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_BYTES)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteBytes(Value,True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_FILL)}
Result := {$IF Defined(BS_INC_MM_WRITE)}FillBytes(Count,Value,True){$IFEND};
{$ELSEIF Defined(BS_INC_MM_VARIANT)}
Result := {$IF Defined(BS_INC_MM_WRITE)}WriteVariant(Value,True){$ELSEIF Defined(BS_INC_MM_READ)}ReadVariant(Value,True){$ELSEIF Defined(BS_INC_MM_GET)}GetVariant(True){$IFEND};
{$ELSE}
  {$MESSAGE FATAL 'unknown value type'}
{$IFEND}   
If not Advance then
  Self.Position := OldPosition;
end;
{$ENDIF}
