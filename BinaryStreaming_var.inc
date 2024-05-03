{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Binary streaming - variants streaming template

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
{$IFDEF BS_INC_VW}
var
{$IFDEF BS_INC_M}
  Ptr:        Pointer;
{$ENDIF}
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
          Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteVariant_LE{$ELSE}Ptr_WriteVariant_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteVariant_LE{$ELSE}Stream_WriteVariant_BE{$ENDIF}(Stream{$ENDIF},VarArrayGet(Value,Indices),True))
        else
          Inc(Result,WriteVarArrayDimension(Succ(Dimension)));
      end;
  end;

begin
{$IFDEF BS_INC_M}
Ptr := Dest;
{$ENDIF}
If VarIsArray(Value) then
  begin
    // array type
    Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteUInt8_LE{$ELSE}Ptr_WriteUInt8_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteUInt8_LE{$ELSE}Stream_WriteUInt8_BE{$ENDIF}(Stream{$ENDIF},VarTypeToInt(VarType(Value)),True);
    Dimensions := VarArrayDimCount(Value);
    // write number of dimensions
    Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteInt32_LE{$ELSE}Ptr_WriteInt32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteInt32_LE{$ELSE}Stream_WriteInt32_BE{$ENDIF}(Stream{$ENDIF},Dimensions,True));
    // write indices bounds (pairs of integers - low and high bound) for each dimension
    For i := 1 to Dimensions do
      begin
        Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteInt32_LE{$ELSE}Ptr_WriteInt32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteInt32_LE{$ELSE}Stream_WriteInt32_BE{$ENDIF}(Stream{$ENDIF},VarArrayLowBound(Value,i),True));
        Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteInt32_LE{$ELSE}Ptr_WriteInt32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteInt32_LE{$ELSE}Stream_WriteInt32_BE{$ENDIF}(Stream{$ENDIF},VarArrayHighBound(Value,i),True));
      end;
  {
    Now (recursively :P) write data if any exist.

    Btw. I know about VarArrayLock/VarArrayUnlock and pointer directly to the
    data, but I want it to be fool proof, therefore using VarArrayGet instead
    of direct access,
  }
    If Dimensions > 0 then
      begin
        Indices := nil;
        SetLength(Indices,Dimensions);
        Inc(Result,WriteVarArrayDimension(1));
      end;
  end
else
  begin
    // simple type
    If VarType(Value) <> (varVariant or varByRef) then
      begin
        Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteUInt8_LE{$ELSE}Ptr_WriteUInt8_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteUInt8_LE{$ELSE}Stream_WriteUInt8_BE{$ENDIF}(Stream{$ENDIF},VarTypeToInt(VarType(Value)),True);
        case VarType(Value) and varTypeMask of
          varBoolean:   Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteBool_LE{$ELSE}Ptr_WriteBool_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteBool_LE{$ELSE}Stream_WriteBool_BE{$ENDIF}(Stream{$ENDIF},VarToBool(Value),True));
          varShortInt:  Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteInt8_LE{$ELSE}Ptr_WriteInt8_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteInt8_LE{$ELSE}Stream_WriteInt8_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varSmallint:  Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteInt16_LE{$ELSE}Ptr_WriteInt16_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteInt16_LE{$ELSE}Stream_WriteInt16_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varInteger:   Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteInt32_LE{$ELSE}Ptr_WriteInt32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteInt32_LE{$ELSE}Stream_WriteInt32_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varInt64:     Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteInt64_LE{$ELSE}Ptr_WriteInt64_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteInt64_LE{$ELSE}Stream_WriteInt64_BE{$ENDIF}(Stream{$ENDIF},VarToInt64(Value),True));
          varByte:      Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteUInt8_LE{$ELSE}Ptr_WriteUInt8_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteUInt8_LE{$ELSE}Stream_WriteUInt8_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varWord:      Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteUInt16_LE{$ELSE}Ptr_WriteUInt16_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteUInt16_LE{$ELSE}Stream_WriteUInt16_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varLongWord:  Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteUInt32_LE{$ELSE}Ptr_WriteUInt32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteUInt32_LE{$ELSE}Stream_WriteUInt32_BE{$ENDIF}(Stream{$ENDIF},Value,True));
        {$IF Declared(varUInt64)}
          varUInt64:    Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteUInt64_LE{$ELSE}Ptr_WriteUInt64_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteUInt64_LE{$ELSE}Stream_WriteUInt64_BE{$ENDIF}(Stream{$ENDIF},VarToUInt64(Value),True));
        {$IFEND}
          varSingle:    Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteFloat32_LE{$ELSE}Ptr_WriteFloat32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteFloat32_LE{$ELSE}Stream_WriteFloat32_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varDouble:    Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteFloat64_LE{$ELSE}Ptr_WriteFloat64_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteFloat64_LE{$ELSE}Stream_WriteFloat64_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varCurrency:  Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteCurrency_LE{$ELSE}Ptr_WriteCurrency_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteCurrency_LE{$ELSE}Stream_WriteCurrency_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varDate:      Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteDateTime_LE{$ELSE}Ptr_WriteDateTime_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteDateTime_LE{$ELSE}Stream_WriteDateTime_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varOleStr:    Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteWideString_LE{$ELSE}Ptr_WriteWideString_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteWideString_LE{$ELSE}Stream_WriteWideString_BE{$ENDIF}(Stream{$ENDIF},Value,True));
          varString:    Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteAnsiString_LE{$ELSE}Ptr_WriteAnsiString_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteAnsiString_LE{$ELSE}Stream_WriteAnsiString_BE{$ENDIF}(Stream{$ENDIF},AnsiString(Value),True));
        {$IF Declared(varUString)}
          varUString:   Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteUnicodeString_LE{$ELSE}Ptr_WriteUnicodeString_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteUnicodeString_LE{$ELSE}Stream_WriteUnicodeString_BE{$ENDIF}(Stream{$ENDIF},Value,True));
        {$IFEND}
        else
          raise EBSUnsupportedVarType.CreateFmt({$IFDEF BS_INC_M}{$IFDEF BS_INC_L}'Ptr_WriteVariant_LE'{$ELSE}'Ptr_WriteVariant_BE'{$ENDIF}{$ELSE}{$IFDEF BS_INC_L}'Stream_WriteVariant_LE'{$ELSE}'Stream_WriteVariant_BE'{$ENDIF}{$ENDIF}+
            ': Cannot write variant of this type (%d).',[VarType(Value) and varTypeMask]);
        end;
      end
    else Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_WriteVariant_LE{$ELSE}Ptr_WriteVariant_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_WriteVariant_LE{$ELSE}Stream_WriteVariant_BE{$ENDIF}(Stream{$ENDIF},Variant(FindVarData(Value)^),True);
  end;
{$IFDEF BS_INC_M}
If Advance then
  Dest := Ptr;
{$ELSE}
AdvanceStream(Advance,Stream,Result);
{$ENDIF}
end;
{$ENDIF}

//==============================================================================

{$IFDEF BS_INC_VR}
var
{$IFDEF BS_INC_M}
  Ptr:            Pointer;
{$ENDIF}
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
            TempVar := Null;
            Inc(Result,{$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadVariant_LE{$ELSE}Ptr_ReadVariant_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadVariant_LE{$ELSE}Stream_ReadVariant_BE{$ENDIF}(Stream{$ENDIF},TempVar,True));
            VarArrayPut(Value,TempVar,Indices);
          end
        else Inc(Result,ReadVarArrayDimension(Succ(Dimension)));
      end;
  end;

begin
{$IFDEF BS_INC_M}
Ptr := Src;
{$ENDIF}
VariantTypeInt := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_GetUInt8_LE{$ELSE}Ptr_GetUInt8_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_GetUInt8_LE{$ELSE}Stream_GetUInt8_BE{$ENDIF}(Stream{$ENDIF},True);
If VariantTypeInt and BS_VARTYPENUM_ARRAY <> 0 then
  begin
    // array
    Dimensions := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_GetInt32_LE{$ELSE}Ptr_GetInt32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_GetInt32_LE{$ELSE}Stream_GetInt32_BE{$ENDIF}(Stream{$ENDIF},True);
    Result := 5 + (Dimensions * 8);
    If Dimensions > 0 then
      begin
        // read indices bounds
        IndicesBounds := nil;
        SetLength(IndicesBounds,Dimensions * 2);
        For i := Low(IndicesBounds) to High(IndicesBounds) do
          IndicesBounds[i] := PtrInt({$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_GetInt32_LE{$ELSE}Ptr_GetInt32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_GetInt32_LE{$ELSE}Stream_GetInt32_BE{$ENDIF}(Stream{$ENDIF},True));
        // create the array
        Value := VarArrayCreate(IndicesBounds,IntToVarType(VariantTypeInt and BS_VARTYPENUM_TYPEMASK));
        // read individual dimensions/items
        Indices := nil;
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
                                TVarData(Value).vBoolean := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_GetBool_LE{$ELSE}Ptr_GetBool_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_GetBool_LE{$ELSE}Stream_GetBool_BE{$ENDIF}(Stream{$ENDIF},True);
                                // +1 is for the already read variable type byte
                                Result := StreamedSize_Bool + 1;
                              end;
      BS_VARTYPENUM_SHORTINT: Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadInt8_LE{$ELSE}Ptr_ReadInt8_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadInt8_LE{$ELSE}Stream_ReadInt8_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vShortInt,True) + 1;
      BS_VARTYPENUM_SMALLINT: Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadInt16_LE{$ELSE}Ptr_ReadInt16_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadInt16_LE{$ELSE}Stream_ReadInt16_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vSmallInt,True) + 1;
      BS_VARTYPENUM_INTEGER:  Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadInt32_LE{$ELSE}Ptr_ReadInt32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadInt32_LE{$ELSE}Stream_ReadInt32_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vInteger,True) + 1;
      BS_VARTYPENUM_INT64:    Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadInt64_LE{$ELSE}Ptr_ReadInt64_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadInt64_LE{$ELSE}Stream_ReadInt64_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vInt64,True) + 1;
      BS_VARTYPENUM_BYTE:     Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadUInt8_LE{$ELSE}Ptr_ReadUInt8_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadUInt8_LE{$ELSE}Stream_ReadUInt8_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vByte,True) + 1;
      BS_VARTYPENUM_WORD:     Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadUInt16_LE{$ELSE}Ptr_ReadUInt16_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadUInt16_LE{$ELSE}Stream_ReadUInt16_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vWord,True) + 1;
      BS_VARTYPENUM_LONGWORD: Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadUInt32_LE{$ELSE}Ptr_ReadUInt32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadUInt32_LE{$ELSE}Stream_ReadUInt32_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vLongWord,True) + 1;
      BS_VARTYPENUM_UINT64: {$IF Declared(varUInt64)}
                              Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadUInt64_LE{$ELSE}Ptr_ReadUInt64_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadUInt64_LE{$ELSE}Stream_ReadUInt64_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).{$IFDEF FPC}vQWord{$ELSE}vUInt64{$ENDIF},True) + 1;
                            {$ELSE}
                              Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadUInt64_LE{$ELSE}Ptr_ReadUInt64_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadUInt64_LE{$ELSE}Stream_ReadUInt64_BE{$ENDIF}(Stream{$ENDIF},UInt64(TVarData(Value).vInt64),True) + 1;
                            {$IFEND}
      BS_VARTYPENUM_SINGLE:   Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadFloat32_LE{$ELSE}Ptr_ReadFloat32_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadFloat32_LE{$ELSE}Stream_ReadFloat32_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vSingle,True) + 1;
      BS_VARTYPENUM_DOUBLE:   Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadFloat64_LE{$ELSE}Ptr_ReadFloat64_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadFloat64_LE{$ELSE}Stream_ReadFloat64_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vDouble,True) + 1;
      BS_VARTYPENUM_CURRENCY: Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadCurrency_LE{$ELSE}Ptr_ReadCurrency_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadCurrency_LE{$ELSE}Stream_ReadCurrency_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vCurrency,True) + 1;
      BS_VARTYPENUM_DATE:     Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadDateTime_LE{$ELSE}Ptr_ReadDateTime_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadDateTime_LE{$ELSE}Stream_ReadDateTime_BE{$ENDIF}(Stream{$ENDIF},TVarData(Value).vDate,True) + 1;
      BS_VARTYPENUM_OLESTR:   begin
                                Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadWideString_LE{$ELSE}Ptr_ReadWideString_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadWideString_LE{$ELSE}Stream_ReadWideString_BE{$ENDIF}(Stream{$ENDIF},WideStrTemp,True) + 1;
                                Value := VarAsType(WideStrTemp,varOleStr);
                              end;
      BS_VARTYPENUM_STRING:   begin
                                Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadAnsiString_LE{$ELSE}Ptr_ReadAnsiString_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadAnsiString_LE{$ELSE}Stream_ReadAnsiString_BE{$ENDIF}(Stream{$ENDIF},AnsiStrTemp,True) + 1;
                                Value := VarAsType(AnsiStrTemp,varString);
                              end;
      BS_VARTYPENUM_USTRING:  begin
                                Result := {$IFDEF BS_INC_M}{$IFDEF BS_INC_L}Ptr_ReadUnicodeString_LE{$ELSE}Ptr_ReadUnicodeString_BE{$ENDIF}(Ptr{$ELSE}{$IFDEF BS_INC_L}Stream_ReadUnicodeString_LE{$ELSE}Stream_ReadUnicodeString_BE{$ENDIF}(Stream{$ENDIF},UnicodeStrTemp,True) + 1;
                              {$IF Declared(varUString)}
                                Value := VarAsType(UnicodeStrTemp,varUString);
                              {$ELSE}
                                Value := VarAsType(UnicodeStrTemp,varOleStr);
                              {$IFEND}
                              end;
    else
      raise EBSUnsupportedVarType.CreateFmt({$IFDEF BS_INC_M}{$IFDEF BS_INC_L}'Ptr_ReadVariant_LE'{$ELSE}'Ptr_ReadVariant_BE'{$ENDIF}{$ELSE}{$IFDEF BS_INC_L}'Stream_ReadVariant_LE'{$ELSE}'Stream_ReadVariant_BE'{$ENDIF}{$ENDIF}+
        ': Cannot read variant of this type number (%d).',[VariantTypeInt and BS_VARTYPENUM_TYPEMASK]);
    end;
  end;
{$IFDEF BS_INC_M}
If Advance then
  Src := Ptr;
{$ELSE}
AdvanceStream(Advance,Stream,Result);
{$ENDIF}
end;
{$ENDIF}
