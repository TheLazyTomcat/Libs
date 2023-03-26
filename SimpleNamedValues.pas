{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Simple Named Values

    This library provides a class that implements a list of name-value pairs.

    It is intended for passing data to objects or functions when the type and
    amount of data cannot be known at compile-time, or there is too many
    arguments to be passed as function parameters. It is similar to passing
    an array of variants, but is more structured and reliable, as the data do
    not need to be ordered.
    It should NOT be used for long-term data storage or when the data are
    frequently accessed (access is not optimized for speed).

    Only some basic types are implemented, but by using typecasting and dynamic
    memory allocation, practically any data can be stored.

    Note on type nvtBuffer (general memory buffers):

      Values of this type are stored and accessed using structure TSNVBuffer
      and are copy-on-assign.
      When you assign buffer to a named value, it is not directly stored,
      instead a copy is made and this copy is stored. This means you can then
      free or otherwise invalidate the source.
      When retrieving this value type, again a copy is made and this copy is
      then returned. You have to free the returned buffer using function
      FreeNamedValueBuffer after you are done using it.
      Internal storage of the buffer is managed automatically but can be
      directly accessed using properties BufferValueMemory and BufferValueSize
      (both read-only).

    The named values list can be used in two ways - as a normal object instance,
    where you are responsible for creation and destruction of the object, or
    in mode in-here called transient instancing.

    Normal mode is clear enough, typical use should be:

      - create an instance of TSimpleNamedValues
      - fill it with required data (values)
      - pass it to function/class that requires this data
      - the function/class should copy the passed data to a local storage
        and/or use them immediately
      - after return from the function/method, free the instance and with it
        the data

    In transient instancing, the object is created internally by function
    TransientNamedValues, containing values passed to this function, and
    only special interface (ITransientSimpleNamedValues) is returned.
    This interface is passed to a function/method requiring the data. This
    function will use method Implementor, defined by the interface, to obtain
    the actual named value list (object of type TSimpleNamedValues) and
    continue to use this list the same way as in normal mode.
    Since the implementor is reference counted, it will be, thanks to automatic
    release of interfaces, freed after the function accepting the interface
    exits (note that the destruction might not happed right after the exit, but
    after all variables, including implicit/hidden ones, will go out of scope).
    Therefore, in this mode, you are not responsible for managing instances of
    the named value list.

  Version 1.3.4 (2023-03-24)

  Last change 2023-03-24

  ©2020-2023 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.SimpleNamedValues

  Dependencies:
    AuxTypes   - github.com/TheLazyTomcat/Lib.AuxTypes
    AuxClasses - github.com/TheLazyTomcat/Lib.AuxClasses

===============================================================================}
unit SimpleNamedValues;

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes, AuxClasses;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESNVException = class(Exception);

  ESNVIndexOutOfBounds  = class(ESNVException);
  ESNVInvalidValue      = class(ESNVException);
  ESNVUnknownNamedValue = class(ESNVException);
  ESNVValueTypeMismatch = class(ESNVException);
  ESNVDuplicateValue    = class(ESNVException);
  ESNVInvalidValueType  = class(ESNVException);

{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleNamedValues
--------------------------------------------------------------------------------
===============================================================================}
type
  TSNVBuffer = record
    Memory: Pointer;
    Size:   TMemSize;
  end;

Function NamedValueBuffer(Memory: Pointer; Size: TMemSize): TSNVBuffer;
procedure FreeNamedValueBuffer(var Buffer: TSNVBuffer);

//------------------------------------------------------------------------------

type
  TSNVNamedValueType = (nvtBool,nvtInteger,nvtInt64,nvtFloat,nvtDateTime,
                        nvtCurrency,nvtString,nvtPointer,nvtGUID,nvtBuffer);

  TSNVNamedValue = record
    Name:     String;
    Changed:  Boolean;  // internal field
    case ValueType: TSNVNamedValueType of
      nvtBool:     (BoolValue:      Boolean);
      nvtInteger:  (IntegerValue:   Integer);
      nvtInt64:    (Int64Value:     Int64);
      nvtFloat:    (FloatValue:     Extended);
      nvtDateTime: (DateTimeValue:  TDateTime);
      nvtCurrency: (CurrencyValue:  Currency);
      nvtString:   (StringValue:    PChar);
      nvtPointer:  (PointerValue:   Pointer);
      nvtGUID:     (GUIDValue:      TGUID);
      nvtBuffer:   (BufferValue:    TSNVBuffer)
  end;

{===============================================================================
    TSimpleNamedValues - class declaration
===============================================================================}
type
  TSimpleNamedValues = class(TCustomListObject)
  protected
    fValues:                array of TSNVNamedValue;
    fCount:                 Integer;
    fUpdateCounter:         Integer;
    fChanged:               Boolean;
    fOnChangeEvent:         TNotifyEvent;
    fOnChangeCallback:      TNotifyCallback;
    fOnValueChangeEvent:    TIntegerEvent;
    fOnValueChangeCallback: TIntegerCallback;
    // getters/setters
    Function GetValue(Index: Integer): TSNVNamedValue; virtual;
    // value getters/setters
    Function GetBoolValue(const Name: String): Boolean; virtual;
    procedure SetBoolValue(const Name: String; Value: Boolean); virtual;
    Function GetIntegerValue(const Name: String): Integer; virtual;
    procedure SetIntegerValue(const Name: String; Value: Integer); virtual;
    Function GetInt64Value(const Name: String): Int64; virtual;
    procedure SetInt64Value(const Name: String; Value: Int64); virtual;
    Function GetFloatValue(const Name: String): Extended; virtual;
    procedure SetFloatValue(const Name: String; Value: Extended); virtual;
    Function GetDateTimeValue(const Name: String): TDateTime; virtual;
    procedure SetDateTimeValue(const Name: String; Value: TDateTime); virtual;
    Function GetCurrencyValue(const Name: String): Currency; virtual;
    procedure SetCurrencyValue(const Name: String; Value: Currency); virtual;
    Function GetStringValue(const Name: String): String; virtual;
    procedure SetStringValue(const Name: String; const Value: String); virtual;
    Function GetPointerValue(const Name: String): Pointer; virtual;
    procedure SetPointerValue(const Name: String; Value: Pointer); virtual;
    Function GetGUIDValue(const Name: String): TGUID; virtual;
    procedure SetGUIDValue(const Name: String; Value: TGUID); virtual;
    Function GetBufferValue(const Name: String): TSNVBuffer; virtual;
    procedure SetBufferValue(const Name: String; Value: TSNVBuffer); virtual;
    Function GetBufferValueMemory(const Name: String): Pointer; virtual;
    Function GetBufferValueSize(const Name: String): TMemSize; virtual;
    // list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    // change reporting
    procedure DoChange; virtual;
    procedure DoValueChange(Index: Integer); virtual;
    // utility
    Function PrepareValue(const Name: String; ValueType: TSNVNamedValueType): Integer; virtual;
    class procedure InitializeNamedValue(var NamedValue: TSNVNamedValue); virtual;
    class procedure FinalizeNamedValue(var NamedValue: TSNVNamedValue); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function IndexOf(const Name: String): Integer; overload; virtual;
    Function IndexOf(const Name: String; ValueType: TSNVNamedValueType): Integer; overload; virtual;
    Function Find(const Name: String; out Index: Integer): Boolean; overload; virtual;
    Function Find(const Name: String; ValueType: TSNVNamedValueType; out Index: Integer): Boolean; overload; virtual;
    Function Exists(const Name: String): Boolean; overload; virtual;
    Function Exists(const Name: String; ValueType: TSNVNamedValueType): Boolean; overload; virtual;
    Function Add(const Name: String; ValueType: TSNVNamedValueType): Integer; virtual;
    procedure Append(Values: TSimpleNamedValues); virtual;
    procedure Insert(Index: Integer; const Name: String; ValueType: TSNVNamedValueType); virtual;
    procedure Move(SrcIdx,DstIdx: Integer); virtual;
    procedure Exchange(Idx1,Idx2: Integer); virtual;
    Function Remove(const Name: String): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    property Values[Index: Integer]: TSNVNamedValue read GetValue; default;
    // values access
    property BoolValue[const Name: String]: Boolean read GetBoolValue write SetBoolValue;
    property IntegerValue[const Name: String]: Integer read GetIntegerValue write SetIntegerValue;
    property Int64Value[const Name: String]: Int64 read GetInt64Value write SetInt64Value;
    property FloatValue[const Name: String]: Extended read GetFloatValue write SetFloatValue;
    property DateTimeValue[const Name: String]: TDateTime read GetDateTimeValue write SetDateTimeValue;
    property CurrencyValue[const Name: String]: Currency read GetCurrencyValue write SetCurrencyValue;
    property StringValue[const Name: String]: String read GetStringValue write SetStringValue;
    property PointerValue[const Name: String]: Pointer read GetPointerValue write SetPointerValue;
    property GUIDValue[const Name: String]: TGUID read GetGUIDValue write SetGUIDValue;
    property BufferValue[const Name: String]: TSNVBuffer read GetBufferValue write SetBufferValue;
    property BufferValueMemory[const Name: String]: Pointer read GetBufferValueMemory;
    property BufferValueSize[const Name: String]: TMemSize read GetBufferValueSize;
    // events, callbacks
    property OnChange: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeEvent: TNotifyEvent read fOnChangeEvent write fOnChangeEvent;
    property OnChangeCallback: TNotifyCallback read fOnChangeCallback write fOnChangeCallback;
    property OnValueChange: TIntegerEvent read fOnValueChangeEvent write fOnValueChangeEvent;
    property OnValueChangeEvent: TIntegerEvent read fOnValueChangeEvent write fOnValueChangeEvent;
    property OnValueChangeCallback: TIntegerCallback read fOnValueChangeCallback write fOnValueChangeCallback;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                              Transient instancing
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Transient instancing - declaration
===============================================================================}
const
  IID_TransientSimpleNamedValues: TGUID = '{E0AD5430-0B30-49C6-8EBC-E28310DFF011}';

type
  ITransientSimpleNamedValues = interface(IInterface)
  ['{E0AD5430-0B30-49C6-8EBC-E28310DFF011}']
    Function Implementor: TSimpleNamedValues;
  end;

//------------------------------------------------------------------------------
{
  TSNVNamedValueContainer and following functions should only be used to pass
  values to function TransientNamedValues, nowhere else.
}
type
  TSNVNamedValueContainer = record
    Name:         String;
    StringValue:  String;
    case ValueType: TSNVNamedValueType of
      nvtBool:     (BoolValue:      Boolean);
      nvtInteger:  (IntegerValue:   Integer);
      nvtInt64:    (Int64Value:     Int64);
      nvtFloat:    (FloatValue:     Extended);
      nvtDateTime: (DateTimeValue:  TDateTime);
      nvtCurrency: (CurrencyValue:  Currency);
      nvtPointer:  (PointerValue:   Pointer);
      nvtGUID:     (GUIDValue:      TGUID);
      nvtBuffer:   (BufferValue:    TSNVBuffer)
  end;

Function BoolNamedValue(const Name: String; const Value: Boolean): TSNVNamedValueContainer;
Function IntegerNamedValue(const Name: String; const Value: Integer): TSNVNamedValueContainer;
Function Int64NamedValue(const Name: String; const Value: Int64): TSNVNamedValueContainer;
Function FloatNamedValue(const Name: String; const Value: Extended): TSNVNamedValueContainer;
Function DateTimeNamedValue(const Name: String; const Value: TDateTime): TSNVNamedValueContainer;
Function CurrencyNamedValue(const Name: String; const Value: Currency): TSNVNamedValueContainer;
Function StringNamedValue(const Name: String; const Value: String): TSNVNamedValueContainer;
Function PointerNamedValue(const Name: String; const Value: Pointer): TSNVNamedValueContainer;
Function GUIDNamedValue(const Name: String; const Value: TGUID): TSNVNamedValueContainer;
Function BufferNamedValue(const Name: String; const Value: TSNVBuffer): TSNVNamedValueContainer;

//------------------------------------------------------------------------------

Function TransientNamedValues(const NamedValues: array of TSNVNamedValueContainer): ITransientSimpleNamedValues; overload;
Function TransientNamedValues(const NamedValue: TSNVNamedValueContainer): ITransientSimpleNamedValues; overload;

{===============================================================================
--------------------------------------------------------------------------------
                              Named values access
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Named values access - declaration
===============================================================================}
{
  Following functions are here to simplify access to named values.
  Each function checks whether the NamedValues list is assigned. When it is,
  it then checks existence of requested named value and, when found, assigns
  its value to a passed variable.
  If the value is assigned, the function returns true.
  When the list is not assigned or the requested value is not present, the
  function will return false and the passed variable is left unchanged (its
  value is preserved).
}

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Boolean): Boolean; overload;
Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Integer): Boolean; overload;
Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int64): Boolean; overload;
Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Extended): Boolean; overload;
Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: TDateTime): Boolean; overload;
Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Currency): Boolean; overload;
Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: String): Boolean; overload;
Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Pointer): Boolean; overload;
Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: TGUID): Boolean; overload;
Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: TSNVBuffer): Boolean; overload;

Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int8): Boolean; overload;
Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: UInt8): Boolean; overload;
Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int16): Boolean; overload;
Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: UInt16): Boolean; overload;
Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int32): Boolean; overload;
Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: UInt32): Boolean; overload;
Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int64): Boolean; overload;
Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: UInt64): Boolean; overload;

Function GetFloatNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Single): Boolean; overload;
Function GetFloatNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Double): Boolean; overload;

implementation

uses
  {$IF not Defined(FPC) and Defined(Windows)}Windows,{$IFEND} Variants;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{===============================================================================
--------------------------------------------------------------------------------
                               TSimpleNamedValues
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TSimpleNamedValues - auxiliary functions
===============================================================================}

Function NamedValueBuffer(Memory: Pointer; Size: TMemSize): TSNVBuffer;
begin
Result.Memory := Memory;
Result.Size := Size;
end;

//------------------------------------------------------------------------------

procedure FreeNamedValueBuffer(var Buffer: TSNVBuffer);
begin
FreeMem(Buffer.Memory,Buffer.Size);
Buffer.Memory := nil;
Buffer.Size := 0;
end;

{===============================================================================
    TSimpleNamedValues - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TSimpleNamedValues - protected methods
-------------------------------------------------------------------------------}

Function TSimpleNamedValues.GetValue(Index: Integer): TSNVNamedValue;
begin
If CheckIndex(Index) then
  Result := fValues[Index]
else
  raise ESNVIndexOutOfBounds.CreateFmt('TSimpleNamedValues.GetValue: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetBoolValue(const Name: String): Boolean;
var
  Index:  Integer;
begin
If Find(Name,nvtBool,Index) then
  Result := fValues[Index].BoolValue
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetBool: Unknown bool value "%s".',[Name]);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetBoolValue(const Name: String; Value: Boolean);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtBool);
fValues[Index].BoolValue := Value;
DoValueChange(Index);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetIntegerValue(const Name: String): Integer;
var
  Index:  Integer;
begin
If Find(Name,nvtInteger,Index) then
  Result := fValues[Index].IntegerValue
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetIntegerValue: Unknown integer value "%s".',[Name]);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetIntegerValue(const Name: String; Value: Integer);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtInteger);
fValues[Index].IntegerValue := Value;
DoValueChange(Index);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetInt64Value(const Name: String): Int64;
var
  Index:  Integer;
begin
If Find(Name,nvtInt64,Index) then
  Result := fValues[Index].Int64Value
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetInt64Value: Unknown int64 value "%s".',[Name]);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetInt64Value(const Name: String; Value: Int64);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtInt64);
fValues[Index].Int64Value := Value;
DoValueChange(Index);
end;
 
//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetFloatValue(const Name: String): Extended;
var
  Index:  Integer;
begin
If Find(Name,nvtFloat,Index) then
  Result := fValues[Index].FloatValue
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetFloatValue: Unknown float value "%s".',[Name]);
end;
 
//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetFloatValue(const Name: String; Value: Extended);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtFloat);
fValues[Index].FloatValue := Value;
DoValueChange(Index);
end;
  
//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetDateTimeValue(const Name: String): TDateTime;
var
  Index:  Integer;
begin
If Find(Name,nvtDateTime,Index) then
  Result := fValues[Index].DateTimeValue
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetDateTimeValue: Unknown datetime value "%s".',[Name]);
end;
  
//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetDateTimeValue(const Name: String; Value: TDateTime);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtDateTime);
fValues[Index].DateTimeValue := Value;
DoValueChange(Index);
end;
   
//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetCurrencyValue(const Name: String): Currency;
var
  Index:  Integer;
begin
If Find(Name,nvtCurrency,Index) then
  Result := fValues[Index].CurrencyValue
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetCurrencyValue: Unknown currency value "%s".',[Name]);
end;
  
//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetCurrencyValue(const Name: String; Value: Currency);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtCurrency);
fValues[Index].CurrencyValue := Value;
DoValueChange(Index);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetStringValue(const Name: String): String;
var
  Index:  Integer;
begin
If Find(Name,nvtString,Index) then
  Result := fValues[Index].StringValue
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetStringValue: Unknown textual value "%s".',[Name]);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetStringValue(const Name: String; const Value: String);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtString);
with fValues[Index] do
  begin
    If Assigned(StringValue) then
      StrDispose(StringValue);
    StringValue := StrNew(PChar(Value));
  end;
DoValueChange(Index);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetPointerValue(const Name: String): Pointer;
var
  Index:  Integer;
begin
If Find(Name,nvtPointer,Index) then
  Result := fValues[Index].PointerValue
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetPointerValue: Unknown pointer value "%s".',[Name]);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetPointerValue(const Name: String; Value: Pointer);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtPointer);
fValues[Index].PointerValue := Value;
DoValueChange(Index);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetGUIDValue(const Name: String): TGUID;
var
  Index:  Integer;
begin
If Find(Name,nvtGUID,Index) then
  Result := fValues[Index].GUIDValue
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetGUIDValue: Unknown guid value "%s".',[Name]);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetGUIDValue(const Name: String; Value: TGUID);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtGUID);
fValues[Index].GUIDValue := Value;
DoValueChange(Index);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetBufferValue(const Name: String): TSNVBuffer;
var
  Index:  Integer;
begin
If Find(Name,nvtBuffer,Index) then
  begin
    Result.Size := fValues[Index].BufferValue.Size;
    GetMem(Result.Memory,Result.Size);
    System.Move(fValues[Index].BufferValue.Memory^,Result.Memory^,Result.Size);
  end
else raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetBufferValue: Unknown buffer value "%s".',[Name]);
end;


//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetBufferValue(const Name: String; Value: TSNVBuffer);
var
  Index:  Integer;
begin
Index := PrepareValue(Name,nvtBuffer);
with fValues[Index] do
  begin
    If Assigned(BufferValue.Memory) then
      FreeNamedValueBuffer(BufferValue);
    BufferValue.Size := Value.Size;
    GetMem(BufferValue.Memory,BufferValue.Size);
    System.Move(Value.Memory^,BufferValue.Memory^,BufferValue.Size);
  end;
DoValueChange(Index);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetBufferValueMemory(const Name: String): Pointer;
var
  Index:  Integer;
begin
If Find(Name,nvtBuffer,Index) then
  Result := fValues[Index].BufferValue.Memory
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetBufferValueMemory: Unknown buffer value "%s".',[Name]);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetBufferValueSize(const Name: String): TMemSize;
var
  Index:  Integer;
begin
If Find(Name,nvtBuffer,Index) then
  Result := fValues[Index].BufferValue.Size
else
  raise ESNVUnknownNamedValue.CreateFmt('TSimpleNamedValues.GetBufferValueSize: Unknown buffer value "%s".',[Name]);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetCapacity: Integer;
begin
Result := Length(fValues);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.SetCapacity(Value: Integer);
var
  i:  Integer;
begin
If Value >= 0 then
  begin
    If Value <> Length(fValues) then
      begin
        If Value < fCount then
          For i := Value to HighIndex do
            FinalizeNamedValue(fValues[i]);
        SetLength(fValues,Value);
        If Value < fCount then
          begin
            fCount := Value;
            DoChange;
          end;
      end;
  end
else raise ESNVInvalidValue.CreateFmt('TSimpleNamedValues.SetCapacity: Invalid capacity value (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.GetCount: Integer;
begin
Result := fCount;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
procedure TSimpleNamedValues.SetCount(Value: Integer);
begin
// do nothing
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.DoChange;
begin
fChanged := True;
If fUpdateCounter <= 0 then
  begin
    If Assigned(fOnChangeEvent) then
      fOnChangeEvent(Self)
    else If Assigned(fOnChangeCallback) then
      fOnChangeCallback(Self);
  end
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.DoValueChange(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    If fUpdateCounter <= 0 then
      begin
        If Assigned(fOnValueChangeEvent) then
          fOnValueChangeEvent(Self,Index)
        else If Assigned(fOnValueChangeCallback) then
          fOnValueChangeCallback(Self,Index);
      end
    else fValues[Index].Changed := True;
  end
else
  begin
    // report all changed values
    If (fUpdateCounter <= 0) and (Assigned(fOnValueChangeEvent) or Assigned(fOnValueChangeCallback)) then
      For i := LowIndex to HighIndex do
        If fValues[i].Changed then
          begin
            If Assigned(fOnValueChangeEvent) then
              fOnValueChangeEvent(Self,i);
            If Assigned(fOnValueChangeCallback) then
              fOnValueChangeCallback(Self,i);
            fValues[i].Changed := False;
          end;
  end;
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.PrepareValue(const Name: String; ValueType: TSNVNamedValueType): Integer;
begin
// do create-on-write
If Find(Name,Result) then
  begin
    // value with a proper name was found, check type
    If fValues[Result].ValueType <> ValueType then
      raise ESNVValueTypeMismatch.CreateFmt('TSimpleNamedValues.PrepareValue: Wrong value type (%d) for "%s".',[Ord(ValueType),Name]);
  end
// value not found, add it
else Result := Add(Name,ValueType);
end;

//------------------------------------------------------------------------------

class procedure TSimpleNamedValues.InitializeNamedValue(var NamedValue: TSNVNamedValue);
begin
NamedValue.Name := '';
FillChar(NamedValue,SizeOf(TSNVNamedValue),0);
end;

//------------------------------------------------------------------------------

class procedure TSimpleNamedValues.FinalizeNamedValue(var NamedValue: TSNVNamedValue);
begin
NamedValue.Name := '';
If NamedValue.ValueType = nvtString then
  begin
    StrDispose(NamedValue.StringValue);
    NamedValue.StringValue := nil;
  end;
If NamedValue.ValueType = nvtBuffer then
  FreeNamedValueBuffer(NamedValue.BufferValue);
end;

{-------------------------------------------------------------------------------
    TSimpleNamedValues - public methods
-------------------------------------------------------------------------------}

constructor TSimpleNamedValues.Create;
begin
inherited Create;
SetLength(fValues,0);
fCount := 0;
fUpdateCounter := 0;
fChanged := False;
fOnChangeEvent := nil;
fOnChangeCallback := nil;
fOnValueChangeEvent := nil;
fOnValueChangeCallback := nil;
end;

//------------------------------------------------------------------------------

destructor TSimpleNamedValues.Destroy;
begin
// prevent change reporting
fOnChangeEvent := nil;
fOnChangeCallback := nil;
fOnValueChangeEvent := nil;
fOnValueChangeCallback := nil;
Clear;
inherited;
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.BeginUpdate;
begin
If fUpdateCounter <= 0 then
  fChanged := False;
Inc(fUpdateCounter);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.EndUpdate;
begin
Dec(fUpdateCounter);
If fUpdateCounter <= 0 then
  begin
    fUpdateCounter := 0;
    If fChanged then
      DoChange;
    DoValueChange(-1);  
    fChanged := False;
  end;
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.LowIndex: Integer;
begin
Result := Low(fValues);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.HighIndex: Integer;
begin
Result := Pred(fCount);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.IndexOf(const Name: String): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If AnsiSameText(Name,fValues[i].Name) then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TSimpleNamedValues.IndexOf(const Name: String; ValueType: TSNVNamedValueType): Integer;
begin
Result := IndexOf(Name);
If CheckIndex(Result) then
  If fValues[Result].ValueType <> ValueType then
    Result := -1;
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.Find(const Name: String; out Index: Integer): Boolean;
begin
Index := IndexOf(Name);
Result := CheckIndex(Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TSimpleNamedValues.Find(const Name: String; ValueType: TSNVNamedValueType; out Index: Integer): Boolean;
begin
Index := IndexOf(Name,ValueType);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.Exists(const Name: String): Boolean;
begin
Result := CheckIndex(IndexOf(Name));
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TSimpleNamedValues.Exists(const Name: String; ValueType: TSNVNamedValueType): Boolean;
begin
Result := CheckIndex(IndexOf(Name,ValueType));
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.Add(const Name: String; ValueType: TSNVNamedValueType): Integer;
begin
If not Find(Name,Result) then
  begin
    Grow;
    InitializeNamedValue(fValues[fCount]);
    fValues[fCount].Name := Name;
    fValues[fCount].ValueType := ValueType;
    Result := fCount;
    Inc(fCount);
    DoChange;
  end
else raise ESNVDuplicateValue.CreateFmt('TSimpleNamedValues.Add: Value "%s" already exists.',[Name]);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.Append(Values: TSimpleNamedValues);
var
  i:      Integer;
  Index:  Integer;
begin
// first check for duplicitites
For i := Values.LowIndex to Values.HighIndex do
  If Find(Values[i].Name,Index) then
    raise ESNVDuplicateValue.CreateFmt('TSimpleNamedValues.Add: Value "%s" already exists.',[Values[i].Name]);
// add values
For i := Values.LowIndex to Values.HighIndex do
  begin
    Index := Add(Values[i].Name,Values[i].ValueType);
    If CheckIndex(Index) then
      case fValues[Index].ValueType of
        nvtBool:      fValues[Index].BoolValue := Values[i].BoolValue;
        nvtInteger:   fValues[Index].IntegerValue := Values[i].IntegerValue;
        nvtInt64:     fValues[Index].Int64Value := Values[i].Int64Value;
        nvtFloat:     fValues[Index].FloatValue := Values[i].FloatValue;
        nvtDateTime:  fValues[Index].DateTimeValue := Values[i].DateTimeValue;
        nvtCurrency:  fValues[Index].CurrencyValue := Values[i].CurrencyValue;
        nvtString:    fValues[Index].StringValue := StrNew(PChar(Values[i].StringValue));
        nvtPointer:   fValues[Index].PointerValue := Values[i].PointerValue;
        nvtGUID:      fValues[Index].GUIDValue := Values[i].GUIDValue;
        nvtBuffer:    begin
                        fValues[Index].BufferValue.Size := Values[i].BufferValue.Size;
                        GetMem(fValues[Index].BufferValue.Memory,fValues[Index].BufferValue.Size);
                        System.Move(Values[i].BufferValue.Memory^,
                                    fValues[Index].BufferValue.Memory^,
                                    fValues[Index].BufferValue.Size);
                      end;
      else
        raise ESNVInvalidValueType.CreateFmt('TSimpleNamedValues.Add: Invalid value type (%d).',[Ord(fValues[Index].ValueType)]);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.Insert(Index: Integer; const Name: String; ValueType: TSNVNamedValueType);
var
  i:  Integer;
begin
If not CheckIndex(IndexOf(Name)) then
  begin
    If CheckIndex(Index) then
      begin
        Grow;
        For i := HighIndex downto Index do
          fValues[i + 1] := fValues[i];
        InitializeNamedValue(fValues[Index]);
        fValues[Index].Name := Name;
        fValues[Index].ValueType := ValueType;
        Inc(fCount);
        DoChange;
      end
    else If Index = fCount then
      Add(Name,ValueType)
    else
      raise ESNVIndexOutOfBounds.CreateFmt('TSimpleNamedValues.Insert: Insertion index (%d) out of bounds.',[Index]);
  end
else raise ESNVDuplicateValue.CreateFmt('TSimpleNamedValues.Insert: Value "%s" already exists.',[Name]);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.Move(SrcIdx,DstIdx: Integer);
var
  Temp: TSNVNamedValue;
  i:    Integer;
begin
If SrcIdx <> DstIdx then
  begin
    If not CheckIndex(SrcIdx) then
      raise ESNVIndexOutOfBounds.CreateFmt('TSimpleNamedValues.Move: Source index (%d) out of bounds.',[SrcIdx]);
    If not CheckIndex(DstIdx) then
      raise ESNVIndexOutOfBounds.CreateFmt('TSimpleNamedValues.Move: Destination index (%d) out of bounds.',[DstIdx]);
    Temp := fValues[SrcIdx];
    If SrcIdx < DstIdx then
      For i := SrcIdx to Pred(DstIdx) do
        fValues[i] := fValues[i + 1]
    else
      For i := SrcIdx downto Succ(DstIdx) do
        fValues[i] := fValues[i - 1];
    fValues[DstIdx] := Temp;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.Exchange(Idx1,Idx2: Integer);
var
  Temp: TSNVNamedValue;
begin
If Idx1 <> Idx2 then
  begin
    If not CheckIndex(Idx1) then
      raise ESNVIndexOutOfBounds.CreateFmt('TSimpleNamedValues.Exchange: Index 1 (%d) out of bounds.',[Idx1]);
    If not CheckIndex(Idx2) then
      raise ESNVIndexOutOfBounds.CreateFmt('TSimpleNamedValues.Exchange: Index 2 (%d) out of bounds.',[Idx2]);
    Temp := fValues[Idx1];
    fValues[Idx1] := fValues[Idx2];
    fValues[Idx2] := Temp;
    DoChange;
  end;
end;

//------------------------------------------------------------------------------

Function TSimpleNamedValues.Remove(const Name: String): Integer;
begin
Result := IndexOf(Name);
If CheckIndex(Result) then
  Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.Delete(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    FinalizeNamedValue(fValues[Index]);
    For i := Index to Pred(HighIndex) do
      fValues[i] := fValues[i + 1];
    fValues[HighIndex].Name := '';  // to be sure
    Dec(fCount);
    Shrink;
    DoChange;
  end
else raise ESNVIndexOutOfBounds.CreateFmt('TSimpleNamedValues.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TSimpleNamedValues.Clear;
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  FinalizeNamedValue(fValues[i]);
SetLength(fValues,0);
fCount := 0;
DoChange;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Transient instancing
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Transient instancing - class declaration
===============================================================================}
type
  TTransientSimpleNamedValues = class(TInterfacedObject,ITransientSimpleNamedValues)
  protected
    fImplementor: TSimpleNamedValues;
  public
    constructor Create(Implementor: TSimpleNamedValues);
    destructor Destroy; override;
    Function Implementor: TSimpleNamedValues; virtual;
  end;

{===============================================================================
    Transient instancing - implementation
===============================================================================}

Function BoolNamedValue(const Name: String; const Value: Boolean): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtBool;
Result.BoolValue := Value;
end;

//------------------------------------------------------------------------------

Function IntegerNamedValue(const Name: String; const Value: Integer): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtInteger;
Result.IntegerValue := Value;
end;

//------------------------------------------------------------------------------

Function Int64NamedValue(const Name: String; const Value: Int64): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtInt64;
Result.Int64Value := Value;
end;

//------------------------------------------------------------------------------

Function FloatNamedValue(const Name: String; const Value: Extended): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtFloat;
Result.FloatValue := Value;
end;

//------------------------------------------------------------------------------

Function DateTimeNamedValue(const Name: String; const Value: TDateTime): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtDateTime;
Result.DateTimeValue := Value;
end;

//------------------------------------------------------------------------------

Function CurrencyNamedValue(const Name: String; const Value: Currency): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtCurrency;
Result.CurrencyValue := Value;
end;

//------------------------------------------------------------------------------

Function StringNamedValue(const Name: String; const Value: String): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtString;
Result.StringValue := Value;
end;

//------------------------------------------------------------------------------

Function PointerNamedValue(const Name: String; const Value: Pointer): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtPointer;
Result.PointerValue := Value;
end;

//------------------------------------------------------------------------------

Function GUIDNamedValue(const Name: String; const Value: TGUID): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtGUID;
Result.GUIDValue := Value;
end;

//------------------------------------------------------------------------------

Function BufferNamedValue(const Name: String; const Value: TSNVBuffer): TSNVNamedValueContainer;
begin
Result.Name := Name;
Result.ValueType := nvtBuffer;
Result.BufferValue := Value;
end;

//------------------------------------------------------------------------------

Function TransientNamedValues(const NamedValues: array of TSNVNamedValueContainer): ITransientSimpleNamedValues;
var
  List: TSimpleNamedValues;
  i:    Integer;
begin
List := TSimpleNamedValues.Create;
For i := Low(NamedValues) to High(NamedValues) do
  case NamedValues[i].ValueType of
    nvtBool:      List.BoolValue[NamedValues[i].Name] := NamedValues[i].BoolValue;
    nvtInteger:   List.IntegerValue[NamedValues[i].Name] := NamedValues[i].IntegerValue;
    nvtInt64:     List.Int64Value[NamedValues[i].Name] := NamedValues[i].Int64Value;
    nvtFloat:     List.FloatValue[NamedValues[i].Name] := NamedValues[i].FloatValue;
    nvtDateTime:  List.DateTimeValue[NamedValues[i].Name] := NamedValues[i].DateTimeValue;
    nvtCurrency:  List.CurrencyValue[NamedValues[i].Name] := NamedValues[i].CurrencyValue;
    nvtString:    List.StringValue[NamedValues[i].Name] := NamedValues[i].StringValue;
    nvtPointer:   List.PointerValue[NamedValues[i].Name] := NamedValues[i].PointerValue;
    nvtGUID:      List.GUIDValue[NamedValues[i].Name] := NamedValues[i].GUIDValue;
    nvtBuffer:    List.BufferValue[NamedValues[i].Name] := NamedValues[i].BufferValue;
  else
    raise ESNVInvalidValue.CreateFmt('TransientNamedValues: Invalid value type (%d).',[Ord(NamedValues[i].ValueType)]);
  end;
Result := TTransientSimpleNamedValues.Create(List);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TransientNamedValues(const NamedValue: TSNVNamedValueContainer): ITransientSimpleNamedValues;
begin
Result := TransientNamedValues([NamedValue]);
end;

{===============================================================================
    Transient instancing - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Transient instancing - public methods
-------------------------------------------------------------------------------}

constructor TTransientSimpleNamedValues.Create(Implementor: TSimpleNamedValues);
begin
inherited Create;
fImplementor := Implementor;
end;

//------------------------------------------------------------------------------

destructor TTransientSimpleNamedValues.Destroy;
begin
FreeAndNil(fImplementor);
inherited;
end;

//------------------------------------------------------------------------------

Function TTransientSimpleNamedValues.Implementor: TSimpleNamedValues;
begin
Result := fImplementor;
end;


{===============================================================================
--------------------------------------------------------------------------------
                              Named values access
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Named values access - implementation
===============================================================================}

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Boolean): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtBool) then
      begin
        Value := NamedValues.BoolValue[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Integer): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtInteger) then
      begin
        Value := NamedValues.IntegerValue[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int64): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtInt64) then
      begin
        Value := NamedValues.Int64Value[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Extended): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtFloat) then
      begin
        Value := NamedValues.FloatValue[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: TDateTime): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtDateTime) then
      begin
        Value := NamedValues.DateTimeValue[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Currency): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtCurrency) then
      begin
        Value := NamedValues.CurrencyValue[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: String): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtString) then
      begin
        Value := NamedValues.StringValue[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Pointer): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtPointer) then
      begin
        Value := NamedValues.PointerValue[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: TGUID): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtGUID) then
      begin
        Value := NamedValues.GUIDValue[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: TSNVBuffer): Boolean;
begin
If Assigned(NamedValues) then
  begin
    If NamedValues.Exists(Name,nvtBuffer) then
      begin
        Value := NamedValues.BufferValue[Name];
        Result := True;
      end
    else Result := False;
  end
else Result := False;
end;

//------------------------------------------------------------------------------

Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int8): Boolean;
var
  Temp: Integer;
begin
Temp := 0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := Int8(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: UInt8): Boolean;
var
  Temp: Integer;
begin
Temp := 0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := UInt8(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int16): Boolean;
var
  Temp: Integer;
begin
Temp := 0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := Int16(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: UInt16): Boolean;
var
  Temp: Integer;
begin
Temp := 0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := UInt16(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int32): Boolean;
var
  Temp: Integer;
begin
Temp := 0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := Int32(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: UInt32): Boolean;
var
  Temp: Integer;
begin
Temp := 0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := UInt32(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Int64): Boolean;
var
  Temp: Integer;
begin
Temp := 0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := Int64(Temp);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetIntegerNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: UInt64): Boolean;
var
  Temp: Integer;
begin
Temp := 0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := UInt64(Temp);
end;

//------------------------------------------------------------------------------

Function GetFloatNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Single): Boolean;
var
  Temp: Extended;
begin
Temp := 0.0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := Temp;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetFloatNamedValue(NamedValues: TSimpleNamedValues; const Name: String; var Value: Double): Boolean;
var
  Temp: Extended;
begin
Temp := 0.0;
Result := GetNamedValue(NamedValues,Name,Temp);
If Result then
  Value := Temp;
end;

end.