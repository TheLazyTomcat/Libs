{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Auxiliary classes and other class-related things

  Version 1.1.3 (2020-09-25)

  Last change 2022-09-24

  ©2018-2022 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.AuxClasses

  Dependencies:
    AuxTypes - github.com/TheLazyTomcat/Lib.AuxTypes

===============================================================================}
unit AuxClasses;
{
  AuxClasses_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.
}
{$IFDEF AuxClasses_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$ASMMODE Intel}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes;

{===============================================================================
    Event and callback types
===============================================================================}

type
  TPlainEvent    = procedure of object;
  TPlainCallback = procedure;
{
  TNotifyEvent is declared in classes, but if including entire classes unit
  into the project is not desirable, this declaration can be used instead.
}
  TNotifyEvent    = procedure(Sender: TObject) of object;
  TNotifyCallback = procedure(Sender: TObject);

  TIntegerEvent    = procedure(Sender: TObject; Value: Integer) of object;
  TIntegerCallback = procedure(Sender: TObject; Value: Integer);

  TIndexEvent    = procedure(Sender: TObject; Index: Integer) of object;
  TIndexCallback = procedure(Sender: TObject; Index: Integer);

  TFloatEvent    = procedure(Sender: TObject; Value: Double) of object;
  TFloatCallback = procedure(Sender: TObject; Value: Double);

  TStringEvent    = procedure(Sender: TObject; const Value: String) of object;
  TStringCallback = procedure(Sender: TObject; const Value: String);

  TMemoryEvent    = procedure(Sender: TObject; Addr: Pointer) of object;
  TMemoryCallback = procedure(Sender: TObject; Addr: Pointer);

  TBufferEvent    = procedure(Sender: TObject; const Buffer; Size: TMemSize) of object;
  TBufferCallback = procedure(Sender: TObject; const Buffer; Size: TMemSize);

  TObjectEvent    = procedure(Sender: TObject; Obj: TObject) of object;
  TObjectCallback = procedure(Sender: TObject; Obj: TObject);

  TOpenEvent    = procedure(Sender: TObject; Values: array of const) of object;
  TOpenCallback = procedure(Sender: TObject; Values: array of const);

{===============================================================================
    Functions
===============================================================================}

Function GetInstanceString(Instance: TObject): String;

{===============================================================================
    Some unit-specific exceptions
===============================================================================}

type
  EACException = class(Exception);

  EACIndexOutOfBounds  = class(EACException);
  EACIncompatibleClass = class(EACException);

{===============================================================================
--------------------------------------------------------------------------------
                                 TCustomObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustomObject - class declaration
===============================================================================}
{
  Normal object only with added fields/properties that can be used by user for
  any purpose, and also some methods.
}
type
  TCustomObject = class(TObject)
  protected
    fUserIntData: PtrInt;
    fUserPtrData: Pointer;
    procedure RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String; const Args: array of const); overload; virtual;
    procedure RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String); overload; virtual;
    procedure RaiseError(const Method,Msg: String; const Args: array of const); overload; virtual;
    procedure RaiseError(const Method,Msg: String); overload; virtual;
    procedure RaiseError(ExceptionClass: ExceptClass; const Msg: String; const Args: array of const); overload; virtual;
    procedure RaiseError(ExceptionClass: ExceptClass; const Msg: String); overload; virtual;
    procedure RaiseError(const Msg: String; const Args: array of const); overload; virtual;
    procedure RaiseError(const Msg: String); overload; virtual;
  public
    constructor Create;
    Function InstanceString: String; virtual;
    property UserIntData: PtrInt read fUserIntData write fUserIntData;
    property UserPtrData: Pointer read fUserPtrData write fUserPtrData;
    property UserData: PtrInt read fUserIntData write fUserIntData;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                            TCustomRefCountedObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustomRefCountedObject - class declaration
===============================================================================}
{
  Reference counted object.
  Note that reference counting is not automatic, you have to call methods
  Acquire and Release for it to work.
  When FreeOnRelease is set to true (by default set to false), then the object
  is automatically freed inside of function Release when reference counter upon
  entry to this function is 1 (ie. it reaches 0 in this call).
}
type
  TCustomRefCountedObject = class(TCustomObject)
  protected
    fRefCount:      Int32;
    fFreeOnRelease: Boolean;
    Function GetRefCount: Int32; virtual;
  public
    constructor Create;
    Function Acquire: Int32; virtual;
    Function Release: Int32; virtual;
    property RefCount: Int32 read GetRefCount;
    property FreeOnRelease: Boolean read fFreeOnRelease write fFreeOnRelease;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                               TCustomListObject
--------------------------------------------------------------------------------
===============================================================================}

type
{
  gmSlow            - grow by 1
  gmLinear          - grow by GrowFactor (integer part of the float)
  gmFast            - grow by capacity * GrowFactor
  gmFastAttenuated  - if capacity is below GrowLimit, grow by capacity * GrowFactor
                      if capacity is above or equal to GrowLimit, grow by 1/16 * GrowLimit
}
  TGrowMode = (gmSlow, gmLinear, gmFast, gmFastAttenuated);
{
  smKeepCap - list is not shrinked, capacity is preserved
  smNormal  - if capacity is above ShrinkLimit AND count is below (capacity * ShrinkFactor) / 2
              then capacity is set to capacity * ShrinkFactor, otherwise capacity is preserved
  smToCount - capacity is set to count
}
  TShrinkMode = (smKeepCap, smNormal, smToCount);

{
  Structure used to store grow settings in one place.
}
  TListGrowSettings = record
    GrowMode:      TGrowMode;
    GrowFactor:    Double;
    GrowLimit:     Integer;
    ShrinkMode:    TShrinkMode;
    ShrinkFactor:  Double;
    ShrinkLimit:   Integer;
  end;
  PListGrowSettings = ^TListGrowSettings;

const
{
  Default list grow/shrink settings.
}
  AC_LIST_GROW_SETTINGS_DEF: TListGrowSettings = (
    GrowMode:      gmFast;
    GrowFactor:    1.0;
    GrowLimit:     128 * 1024 * 1024;
    ShrinkMode:    smNormal;
    ShrinkFactor:  0.5;
    ShrinkLimit:   256);

{===============================================================================
    TCustomListObject - class declaration
===============================================================================}
{
  Implements methods for advanced parametrized growing and shrinking of any
  list and a few more.
  Expects derived class to properly implement capacity and count properties
  (both getters and setters) and LowIndex and HighIndex functions.
}
type
  TCustomListObject = class(TCustomObject)
  protected
    fListGrowSettings:  TListGrowSettings;
    Function GetCapacity: Integer; virtual; abstract;
    procedure SetCapacity(Value: Integer); virtual; abstract;
    Function GetCount: Integer; virtual; abstract;
    procedure SetCount(Value: Integer); virtual; abstract;
    procedure Grow(MinDelta: Integer = 1); virtual;
    procedure Shrink; virtual;
  public
    constructor Create;
    Function LowIndex: Integer; virtual; abstract;
    Function HighIndex: Integer; virtual; abstract;
    Function CheckIndex(Index: Integer): Boolean; virtual;
    procedure CopyGrowSettings(Source: TCustomListObject); virtual;
    property ListGrowSettings: TListGrowSettings read fListGrowSettings write fListGrowSettings;
    property GrowMode: TGrowMode read fListGrowSettings.GrowMode write fListGrowSettings.GrowMode;
    property GrowFactor: Double read fListGrowSettings.GrowFactor write fListGrowSettings.GrowFactor;
    property GrowLimit: Integer read fListGrowSettings.GrowLimit write fListGrowSettings.GrowLimit;
    property ShrinkMode: TShrinkMode read fListGrowSettings.ShrinkMode write fListGrowSettings.ShrinkMode;
    property ShrinkFactor: Double read fListGrowSettings.ShrinkFactor write fListGrowSettings.ShrinkFactor;
    property ShrinkLimit: Integer read fListGrowSettings.ShrinkLimit write fListGrowSettings.ShrinkLimit;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                             TCustomMultiListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustomMultiListObject - class declaration
===============================================================================}
{
  Very similar to TCustomListObject, but this class can support multiple
  separate lists that are distinguished by index (integer).
}
type
  TCustomMultiListObject = class(TCustomObject)
  protected
    fListGrowSettings:  array of TListGrowSettings;
    Function GetListCount: Integer; virtual;
    procedure SetListCount(Value: Integer); virtual;
    Function GetListGrowSettings(List: Integer): TListGrowSettings; virtual;
    procedure SetListGrowSettings(List: Integer; Value: TListGrowSettings); virtual;
    Function GetListGrowSettingsPtr(List: Integer): PListGrowSettings; virtual;
    Function GetCapacity(List: Integer): Integer; virtual; abstract;
    procedure SetCapacity(List,Value: Integer); virtual; abstract;
    Function GetCount(List: Integer): Integer; virtual; abstract;
    procedure SetCount(List,Value: Integer); virtual; abstract;
    procedure Grow(List: Integer; MinDelta: Integer = 1); virtual;
    procedure Shrink(List: Integer); virtual;
  public
    constructor Create(ListCount: Integer);
    destructor Destroy; override;
    Function LowList: Integer; virtual;
    Function HighList: Integer; virtual;
    Function LowIndex(List: Integer): Integer; virtual; abstract;
    Function HighIndex(List: Integer): Integer; virtual; abstract;
    Function CheckList(List: Integer): Boolean; virtual;
    Function CheckIndex(List,Index: Integer): Boolean; virtual;
    procedure CopyGrowSettings(Source: TCustomMultiListObject); virtual;
    property ListCount: Integer read GetListCount write SetListCount;
    property ListGrowSettings[List: Integer]: TListGrowSettings read GetListGrowSettings write SetListGrowSettings;
    property ListGrowSettingsPtrs[List: Integer]: PListGrowSettings read GetListGrowSettingsPtr;
    property Capacity[List: Integer]: Integer read GetCapacity write SetCapacity;
    property Count[List: Integer]: Integer read GetCount write SetCount;
  end;

implementation

{$IF not Defined(FPC) and Defined(Windows) and Defined(PurePascal)}
uses
  Windows;
{$IFEND}

{===============================================================================
    Functions
===============================================================================}

Function GetInstanceString(Instance: TObject): String;
begin
If Assigned(Instance) then
  Result := Format('%s(%p)',[Instance.ClassName,Pointer(Instance)])
else
  Result := 'TObject(nil)'; // return some sensible string, not just nothing
end;

{===============================================================================
--------------------------------------------------------------------------------
                                 TCustomObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustomObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustomObject - protected methods
-------------------------------------------------------------------------------}

procedure TCustomObject.RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String; const Args: array of const);
begin
raise ExceptionClass.CreateFmt(Format('%s.%s: %s',[InstanceString,Method,Msg]),Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCustomObject.RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String);
begin
RaiseError(ExceptionClass,Method,Msg,[]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


procedure TCustomObject.RaiseError(const Method,Msg: String; const Args: array of const);
begin
RaiseError(Exception,Method,Msg,Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCustomObject.RaiseError(const Method,Msg: String);
begin
RaiseError(Exception,Method,Msg,[]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCustomObject.RaiseError(ExceptionClass: ExceptClass; const Msg: String; const Args: array of const);
begin
raise ExceptionClass.CreateFmt(Format('%s: %s',[InstanceString,Msg]),Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCustomObject.RaiseError(ExceptionClass: ExceptClass; const Msg: String);
begin
RaiseError(ExceptionClass,Msg,[]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCustomObject.RaiseError(const Msg: String; const Args: array of const);
begin
RaiseError(Exception,Msg,Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure TCustomObject.RaiseError(const Msg: String);
begin
RaiseError(Exception,Msg,[]);
end;

{-------------------------------------------------------------------------------
    TCustomObject - public methods
-------------------------------------------------------------------------------}

constructor TCustomObject.Create;
begin
inherited;
fUserIntData := 0;
fUserPtrData := nil;
end;

//------------------------------------------------------------------------------

Function TCustomObject.InstanceString: String;
begin
Result := GetInstanceString(Self);
end;

{===============================================================================
--------------------------------------------------------------------------------
                            TCustomRefCountedObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustomRefCountedObject - auxiliary functions
===============================================================================}

{$IFNDEF PurePascal}
Function InterlockedExchangeAdd(var A: Int32; B: Int32): Int32; register; assembler;
asm
{$IFDEF x64}
  {$IFDEF Windows}
        XCHG  RCX,  RDX
  LOCK  XADD  dword ptr [RDX], ECX
        MOV   EAX,  ECX
  {$ELSE}
        XCHG  RDI,  RSI
  LOCK  XADD  dword ptr [RSI], EDI
        MOV   EAX,  EDI
  {$ENDIF}
{$ELSE}
        XCHG  EAX,  EDX
  LOCK  XADD  dword ptr [EDX], EAX
{$ENDIF}
end;
{$ENDIF}

{===============================================================================
    TCustomRefCountedObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustomRefCountedObject - protected methods
-------------------------------------------------------------------------------}

Function TCustomRefCountedObject.GetRefCount: Int32;
begin
Result := InterlockedExchangeAdd(fRefCount,0);
end;

//-----------------------------------------------------------------------------

constructor TCustomRefCountedObject.Create;
begin
inherited Create;
fRefCount := 0;
fFreeOnRelease := False;
end;

//------------------------------------------------------------------------------

Function TCustomRefCountedObject.Acquire: Int32;
begin
Result := InterlockedExchangeAdd(fRefCount,1) + 1;
end;

//------------------------------------------------------------------------------

Function TCustomRefCountedObject.Release: Int32;
begin
Result := InterlockedExchangeAdd(fRefCount,-1) - 1;
If fFreeOnRelease and (Result <= 0) then
  Self.Free;
end;

{===============================================================================
--------------------------------------------------------------------------------
                               TCustomListObject
--------------------------------------------------------------------------------
===============================================================================}

const
  CAPACITY_GROW_INIT = 32;

{===============================================================================
    TCustomListObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustomListObject - protected methods
-------------------------------------------------------------------------------}

procedure TCustomListObject.Grow(MinDelta: Integer = 1);
var
  Delta:  Integer;
begin
If Count >= Capacity then
  begin
    If Capacity = 0 then
      Delta := CAPACITY_GROW_INIT
    else
      case fListGrowSettings.GrowMode of
        gmLinear:
          Delta := Trunc(fListGrowSettings.GrowFactor);
        gmFast:
          Delta := Trunc(Capacity * fListGrowSettings.GrowFactor);
        gmFastAttenuated:
          If Capacity >= fListGrowSettings.GrowLimit then
            Delta := fListGrowSettings.GrowLimit shr 4
          else
            Delta := Trunc(Capacity * fListGrowSettings.GrowFactor);
      else
       {gmSlow}
       Delta := 1;
      end;
    If Delta < MinDelta then
      Delta := MinDelta
    else If Delta <= 0 then
      Delta := 1;
    Capacity := Capacity + Delta;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomListObject.Shrink;
begin
If Capacity > 0 then
  case fListGrowSettings.ShrinkMode of
    smNormal:
      If (Capacity > fListGrowSettings.ShrinkLimit) and
        (Count < Integer(Trunc((Capacity * fListGrowSettings.ShrinkFactor) / 2))) then
        Capacity := Trunc(Capacity * fListGrowSettings.ShrinkFactor);
    smToCount:
      Capacity := Count;
  else
    {smKeepCap}
    //do nothing
  end;
end;

{-------------------------------------------------------------------------------
    TCustomListObject - public methods
-------------------------------------------------------------------------------}

constructor TCustomListObject.Create;
begin
inherited;
fListGrowSettings := AC_LIST_GROW_SETTINGS_DEF;
end;

//------------------------------------------------------------------------------

Function TCustomListObject.CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

procedure TCustomListObject.CopyGrowSettings(Source: TCustomListObject);
begin
fListGrowSettings := Source.ListGrowSettings;
end;

{===============================================================================
--------------------------------------------------------------------------------
                             TCustomMultiListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustomMultiListObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustomMultiListObject - protected methods
-------------------------------------------------------------------------------}

Function TCustomMultiListObject.GetListCount: Integer;
begin
Result := Length(fListGrowSettings);
end;

//------------------------------------------------------------------------------

procedure TCustomMultiListObject.SetListCount(Value: Integer);
var
  OldCount: Integer;
  i:        Integer;
begin
If (Value <> Length(fListGrowSettings)) and (Value >= 0) then
  begin
    OldCount := Length(fListGrowSettings);
    SetLength(fListGrowSettings,Value);
    If Value > OldCount then
      For i := OldCount to High(fListGrowSettings) do
       fListGrowSettings[i] := AC_LIST_GROW_SETTINGS_DEF;
  end;
end;

//------------------------------------------------------------------------------

Function TCustomMultiListObject.GetListGrowSettings(List: Integer): TListGrowSettings;
begin
If CheckList(List) then
  Result := fListGrowSettings[List]
else
  raise EACIndexOutOfBounds.CreateFmt('TCustomMultiListObject.GetListGrowSettings: List index %d out of bounds.',[List]);
end;

//------------------------------------------------------------------------------

procedure TCustomMultiListObject.SetListGrowSettings(List: Integer; Value: TListGrowSettings);
begin
If CheckList(List) then
  fListGrowSettings[List] := Value
else
  raise EACIndexOutOfBounds.CreateFmt('TCustomMultiListObject.SetListGrowSettings: List index %d out of bounds.',[List]);
end;

//------------------------------------------------------------------------------

Function TCustomMultiListObject.GetListGrowSettingsPtr(List: Integer): PListGrowSettings;
begin
If CheckList(List) then
  Result := Addr(fListGrowSettings[List])
else
  raise EACIndexOutOfBounds.CreateFmt('TCustomMultiListObject.GetListGrowSettingsPtr: List index %d out of bounds.',[List]);
end;

//------------------------------------------------------------------------------

procedure TCustomMultiListObject.Grow(List: Integer; MinDelta: Integer = 1);
var
  Delta:  Integer;
begin
If CheckList(List) then
  begin
    If Count[List] >= Capacity[List] then
      begin
        If Capacity[List] = 0 then
          Delta := CAPACITY_GROW_INIT
        else
          case fListGrowSettings[List].GrowMode of
            gmLinear:
              Delta := Trunc(fListGrowSettings[List].GrowFactor);
            gmFast:
              Delta := Trunc(Capacity[List] * fListGrowSettings[List].GrowFactor);
            gmFastAttenuated:
              If Capacity[List] >= fListGrowSettings[List].GrowLimit then
                Delta := fListGrowSettings[List].GrowLimit shr 4
              else
                Delta := Trunc(Capacity[List] * fListGrowSettings[List].GrowFactor);
          else
           {gmSlow}
           Delta := 1;
          end;
        If Delta < MinDelta then
          Delta := MinDelta
        else If Delta <= 0 then
          Delta := 1;
        Capacity[List] := Capacity[List] + Delta;
      end;
end
else EACIndexOutOfBounds.CreateFmt('TCustomMultiListObject.Grow: List index %d out of bounds.',[List]);
end;

//------------------------------------------------------------------------------

procedure TCustomMultiListObject.Shrink(List: Integer);
begin
If CheckList(List) then
  begin
    If Capacity[List] > 0 then
      case fListGrowSettings[List].ShrinkMode of
        smNormal:
          If (Capacity[List] > fListGrowSettings[List].ShrinkLimit) and
            (Count[List] < Integer(Trunc((Capacity[List] * fListGrowSettings[List].ShrinkFactor) / 2))) then
            Capacity[List] := Trunc(Capacity[List] * fListGrowSettings[List].ShrinkFactor);
        smToCount:
          Capacity[List] := Count[List];
      else
        {smKeepCap}
        //do nothing
      end;
end
else EACIndexOutOfBounds.CreateFmt('TCustomMultiListObject.Shrink: List index %d out of bounds.',[List]);
end;

{-------------------------------------------------------------------------------
    TCustomMultiListObject - public methods
-------------------------------------------------------------------------------}

constructor TCustomMultiListObject.Create(ListCount: Integer);
begin
inherited Create;
SetListCount(ListCount)
end;

//------------------------------------------------------------------------------

destructor TCustomMultiListObject.Destroy;
begin
SetLength(fListGrowSettings,0);
inherited;
end;

//------------------------------------------------------------------------------

Function TCustomMultiListObject.LowList: Integer;
begin
Result := Low(fListGrowSettings);
end;

//------------------------------------------------------------------------------

Function TCustomMultiListObject.HighList: Integer;
begin
Result := High(fListGrowSettings);
end;

//------------------------------------------------------------------------------

Function TCustomMultiListObject.CheckList(List: Integer): Boolean;
begin
Result := (List >= LowList) and (List <= HighList);
end;

//------------------------------------------------------------------------------

Function TCustomMultiListObject.CheckIndex(List,Index: Integer): Boolean;
begin
Result := (Index >= LowIndex(List)) and (Index <= HighIndex(List));
end;

//------------------------------------------------------------------------------

procedure TCustomMultiListObject.CopyGrowSettings(Source: TCustomMultiListObject);
var
  i:  Integer;
begin
If Self.ListCount = Source.ListCount then
  begin
    For i := LowList to HighList do
      fListGrowSettings[i] := AC_LIST_GROW_SETTINGS_DEF;
  end
else raise EACIncompatibleClass.CreateFmt('TCustomMultiListObject.CopyGrowSettings: List count mismatch (%d,%d).',[Self.ListCount,Source.ListCount]);
end;

end.
