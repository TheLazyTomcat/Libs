{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Auxiliary classes and other class-related things

    This file contains main classes template used by AuxClasses library.

  Version 1.3 (2025-10-01)

  Last change 2025-10-01

  ©2018-2025 František Milt

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
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    AuxTypes      - github.com/TheLazyTomcat/Lib.AuxTypes

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol AuxClasses_UseAuxExceptions for details).

  Indirect dependencies:
    SimpleCPUID - github.com/TheLazyTomcat/Lib.SimpleCPUID
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
{!tun_end!} // ignore this line

{===============================================================================
********************************************************************************

                                  Declarations

********************************************************************************
===============================================================================}

{$IFDEF AC_Include_Declaration}
{===============================================================================
--------------------------------------------------------------------------------
                        TCustom((Simple)Interfaced)Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)Object  - class declaration
===============================================================================}
{
  Normal object only with added fields/properties that can be used by user for
  any purpose, and also some methods.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedObject = class(TInterfacedObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedObject = class(TSimpleInterfacedObject)
{$ELSE}
  TCustomObject = class(TObject)
{$IFEND}
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
                   TCustom((Simple)Interfaced)RefCountedObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)RefCountedObject - class declaration
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
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedRefCountedObject = class(TCustomInterfacedObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedRefCountedObject = class(TCustomSimpleInterfacedObject)
{$ELSE}
  TCustomRefCountedObject = class(TCustomObject)
{$IFEND}
  protected
    fReferenceCount:  Int32;
    fFreeOnRelease:   Boolean;
    Function GetReferenceCount: Int32; virtual;
  public
    constructor Create;
    Function Acquire: Int32; virtual;
    Function Release: Int32; virtual;
    property ReferenceCount: Int32 read GetReferenceCount;
    property FreeOnRelease: Boolean read fFreeOnRelease write fFreeOnRelease;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                      TCustom((Simple)Interfaced)ListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ListObject - class declaration
===============================================================================}
{
  Implements methods for advanced parametrized growing and shrinking of any
  list and a few more.
  Expects derived class to properly implement capacity and count properties
  (both getters and setters) and LowIndex and HighIndex functions.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedListObject = class(TCustomInterfacedObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedListObject = class(TCustomSimpleInterfacedObject)
{$ELSE}
  TCustomListObject = class(TCustomObject)
{$IFEND}
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
    procedure CopyGrowSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedListObject
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedListObject{$ELSE}TCustomListObject{$IFEND}); virtual;
    property ListGrowSettings: TListGrowSettings read fListGrowSettings write fListGrowSettings;
    property GrowInit: Integer read fListGrowSettings.GrowInit write fListGrowSettings.GrowInit;
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
                   TCustom((Simple)Interfaced)MultiListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiListObject - class declaration
===============================================================================}
{
  Very similar to TCustom((Simple)Interfaced)ListObject, but this class can
  support multiple separate lists that are selected by integer index.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedMultiListObject = class(TCustomInterfacedObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedMultiListObject = class(TCustomSimpleInterfacedObject)
{$ELSE}
  TCustomMultiListObject = class(TCustomObject)
{$IFEND}
  private
    Function StaticClassName: String;
  protected
    fListGrowSettings:  array of TListGrowSettings;
    Function GetListCount: Integer; virtual;
    procedure SetListCount(Value: Integer); virtual;
    Function GetListGrowSettings(List: Integer): TListGrowSettings; virtual;
    procedure SetListGrowSettings(List: Integer; Value: TListGrowSettings); virtual;
    Function GetListGrowSettingsPtr(List: Integer): PListGrowSettings; virtual;
    Function GetListGrowSettings_GrowInit(List: Integer): Integer; virtual;
    procedure SetListGrowSettings_GrowInit(List: Integer; Value: Integer); virtual;
    Function GetListGrowSettings_GrowMode(List: Integer): TGrowMode; virtual;
    procedure SetListGrowSettings_GrowMode(List: Integer; Value: TGrowMode); virtual;
    Function GetListGrowSettings_GrowFactor(List: Integer): Double; virtual;
    procedure SetListGrowSettings_GrowFactor(List: Integer; Value: Double); virtual;
    Function GetListGrowSettings_GrowLimit(List: Integer): Integer; virtual;
    procedure SetListGrowSettings_GrowLimit(List: Integer; Value: Integer); virtual;
    Function GetListGrowSettings_ShrinkMode(List: Integer): TShrinkMode; virtual;
    procedure SetListGrowSettings_ShrinkMode(List: Integer; Value: TShrinkMode); virtual;
    Function GetListGrowSettings_ShrinkFactor(List: Integer): Double; virtual;
    procedure SetListGrowSettings_ShrinkFactor(List: Integer; Value: Double); virtual;
    Function GetListGrowSettings_ShrinkLimit(List: Integer): Integer; virtual;
    procedure SetListGrowSettings_ShrinkLimit(List: Integer; Value: Integer); virtual;
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
    Function CheckList(List: Integer): Boolean; virtual;
    Function LowIndex(List: Integer): Integer; virtual; abstract;
    Function HighIndex(List: Integer): Integer; virtual; abstract;
    Function CheckIndex(List,Index: Integer): Boolean; virtual;
    procedure CopyGrowSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedMultiListObject
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}); virtual;
    property ListCount: Integer read GetListCount write SetListCount;
    property ListGrowSettings[List: Integer]: TListGrowSettings read GetListGrowSettings write SetListGrowSettings;
    property ListGrowSettingsPtrs[List: Integer]: PListGrowSettings read GetListGrowSettingsPtr;
    property GrowInit[List: Integer]: Integer read GetListGrowSettings_GrowInit write SetListGrowSettings_GrowInit;
    property GrowMode[List: Integer]: TGrowMode read GetListGrowSettings_GrowMode write SetListGrowSettings_GrowMode;
    property GrowFactor[List: Integer]: Double read GetListGrowSettings_GrowFactor write SetListGrowSettings_GrowFactor;
    property GrowLimit[List: Integer]: Integer read GetListGrowSettings_GrowLimit write SetListGrowSettings_GrowLimit;
    property ShrinkMode[List: Integer]: TShrinkMode read GetListGrowSettings_ShrinkMode write SetListGrowSettings_ShrinkMode;
    property ShrinkFactor[List: Integer]: Double read GetListGrowSettings_ShrinkFactor write SetListGrowSettings_ShrinkFactor;
    property ShrinkLimit[List: Integer]: Integer read GetListGrowSettings_ShrinkLimit write SetListGrowSettings_ShrinkLimit;
    property Capacity[List: Integer]: Integer read GetCapacity write SetCapacity;
    property Count[List: Integer]: Integer read GetCount write SetCount;
  end;

{$IF Defined(AC_Include_Interfaced)}
type
  // shorter aliases
  TCustomIObject = TCustomInterfacedObject;
  TCustomIRefCountedObject = TCustomInterfacedRefCountedObject;
  TCustomIListObject = TCustomInterfacedListObject;
  TCustomIMultiListObject = TCustomInterfacedMultiListObject;

{$ELSEIF Defined(AC_Include_SInterfaced)}
type
  // shorter aliases
  TCustomSIObject = TCustomSimpleInterfacedObject;
  TCustomSIRefCountedObject = TCustomSimpleInterfacedRefCountedObject;
  TCustomSIListObject = TCustomSimpleInterfacedListObject;
  TCustomSIMultiListObject = TCustomSimpleInterfacedMultiListObject;
{$IFEND}

{$ENDIF}

{===============================================================================
********************************************************************************

                                 Implementations

********************************************************************************
===============================================================================}

{$IFDEF AC_Include_Implementation}

{$UNDEF AC_I_I}
{$UNDEF AC_I_SI}
{$IF Defined(AC_Include_Interfaced)}
  {$DEFINE AC_I_I}
{$ELSEIF Defined(AC_Include_SInterfaced)}
  {$DEFINE AC_I_SI}
{$IFEND}

{===============================================================================
--------------------------------------------------------------------------------
                        TCustom((Simple)Interfaced)Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)Object - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)Object - protected methods
-------------------------------------------------------------------------------}

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String; const Args: array of const);
begin
raise ExceptionClass.CreateFmt(Format('%s.%s: %s',[InstanceString,Method,Msg]),Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(ExceptionClass: ExceptClass; const Method,Msg: String);
begin
RaiseError(ExceptionClass,Method,Msg,[]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(const Method,Msg: String; const Args: array of const);
begin
RaiseError(Exception,Method,Msg,Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(const Method,Msg: String);
begin
RaiseError(Exception,Method,Msg,[]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(ExceptionClass: ExceptClass; const Msg: String; const Args: array of const);
begin
raise ExceptionClass.CreateFmt(Format('%s: %s',[InstanceString,Msg]),Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(ExceptionClass: ExceptClass; const Msg: String);
begin
RaiseError(ExceptionClass,Msg,[]);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(const Msg: String; const Args: array of const);
begin
RaiseError(Exception,Msg,Args);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  RaiseError(const Msg: String);
begin
RaiseError(Exception,Msg,[]);
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)Object - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  Create;
begin
inherited;
fUserIntData := 0;
fUserPtrData := nil;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIObject{$ELSEIF Defined(AC_I_SI)}TCustomSIObject{$ELSE}TCustomObject{$IFEND}.
  InstanceString: String;
begin
Result := GetInstanceString(Self);
end;


{===============================================================================
--------------------------------------------------------------------------------
                   TCustom((Simple)Interfaced)RefCountedObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)RefCountedObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)RefCountedObject - protected methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIRefCountedObject{$ELSEIF Defined(AC_I_SI)}TCustomSIRefCountedObject{$ELSE}TCustomRefCountedObject{$IFEND}.
  GetReferenceCount: Int32;
begin
Result := InterlockedExchangeAdd(fReferenceCount,0);
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)RefCountedObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIRefCountedObject{$ELSEIF Defined(AC_I_SI)}TCustomSIRefCountedObject{$ELSE}TCustomRefCountedObject{$IFEND}.
  Create;
begin
inherited Create;
fReferenceCount := 0;
fFreeOnRelease := False;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIRefCountedObject{$ELSEIF Defined(AC_I_SI)}TCustomSIRefCountedObject{$ELSE}TCustomRefCountedObject{$IFEND}.
  Acquire: Int32;
begin
Result := InterlockedExchangeAdd(fReferenceCount,1) + 1;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIRefCountedObject{$ELSEIF Defined(AC_I_SI)}TCustomSIRefCountedObject{$ELSE}TCustomRefCountedObject{$IFEND}.
  Release: Int32;
begin
Result := InterlockedExchangeAdd(fReferenceCount,-1) - 1;
If fFreeOnRelease and (Result <= 0) then
  Self.Free;
end;


{===============================================================================
--------------------------------------------------------------------------------
                      TCustom((Simple)Interfaced)ListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)ListObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListObject - protected methods
-------------------------------------------------------------------------------}

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Grow(MinDelta: Integer = 1);
var
  Delta:  Integer;
begin
If ResolveGrowDelta(Capacity,Count,MinDelta,fListGrowSettings,Delta) then
  Capacity := Capacity + Delta;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Shrink;
var
  NewCap: Integer;
begin
If ResolveShrinkCapacity(Capacity,Count,fListGrowSettings,NewCap) then
  Capacity := NewCap;
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Create;
begin
inherited;
fListGrowSettings := AC_LIST_GROW_SETTINGS_DEF;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  CopyGrowSettings(Source: {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND});
begin
fListGrowSettings := Source.ListGrowSettings;
end;



{===============================================================================
--------------------------------------------------------------------------------
                   TCustom((Simple)Interfaced)MultiListObject
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiListObject - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiListObject - private methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  StaticClassName: String;
begin
Result :=
{$IF Defined(AC_I_I)}
  'TCustomInterfacedMultiListObject'
{$ELSEIF Defined(AC_I_SI)}
  'TCustomSimpleInterfacedMultiListObject'
{$ELSE}
  'TCustomMultiListObject'
{$IFEND};
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiListObject - protected methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListCount: Integer;
begin
Result := Length(fListGrowSettings);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListCount(Value: Integer);
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

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListGrowSettings(List: Integer): TListGrowSettings;
begin
If CheckList(List) then
  Result := fListGrowSettings[List]
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListGrowSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListGrowSettings(List: Integer; Value: TListGrowSettings);
begin
If CheckList(List) then
  fListGrowSettings[List] := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListGrowSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListGrowSettingsPtr(List: Integer): PListGrowSettings;
begin
If CheckList(List) then
  Result := Addr(fListGrowSettings[List])
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListGrowSettingsPtr: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListGrowSettings_GrowInit(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListGrowSettings[List].GrowInit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListGrowSettings_GrowInit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListGrowSettings_GrowInit(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListGrowSettings[List].GrowInit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListGrowSettings_GrowInit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListGrowSettings_GrowMode(List: Integer): TGrowMode;
begin
If CheckList(List) then
  Result := fListGrowSettings[List].GrowMode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListGrowSettings_GrowMode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListGrowSettings_GrowMode(List: Integer; Value: TGrowMode);
begin
If CheckList(List) then
  fListGrowSettings[List].GrowMode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListGrowSettings_GrowMode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListGrowSettings_GrowFactor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListGrowSettings[List].GrowFactor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListGrowSettings_GrowFactor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListGrowSettings_GrowFactor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListGrowSettings[List].GrowFactor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListGrowSettings_GrowFactor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListGrowSettings_GrowLimit(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListGrowSettings[List].GrowLimit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListGrowSettings_GrowLimit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListGrowSettings_GrowLimit(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListGrowSettings[List].GrowLimit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListGrowSettings_GrowLimit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListGrowSettings_ShrinkMode(List: Integer): TShrinkMode;
begin
If CheckList(List) then
  Result := fListGrowSettings[List].ShrinkMode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListGrowSettings_ShrinkMode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListGrowSettings_ShrinkMode(List: Integer; Value: TShrinkMode);
begin
If CheckList(List) then
  fListGrowSettings[List].ShrinkMode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListGrowSettings_ShrinkMode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListGrowSettings_ShrinkFactor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListGrowSettings[List].ShrinkFactor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListGrowSettings_ShrinkFactor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListGrowSettings_ShrinkFactor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListGrowSettings[List].ShrinkFactor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListGrowSettings_ShrinkFactor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListGrowSettings_ShrinkLimit(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListGrowSettings[List].ShrinkLimit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListGrowSettings_ShrinkLimit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListGrowSettings_ShrinkLimit(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListGrowSettings[List].ShrinkLimit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListGrowSettings_ShrinkLimit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Grow(List: Integer; MinDelta: Integer = 1);
var
  Delta:  Integer;
begin
If CheckList(List) then
  begin
    If ResolveGrowDelta(Capacity[List],Count[List],MinDelta,fListGrowSettings[List],Delta) then
      Capacity[List] := Capacity[List] + Delta;
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.Grow: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Shrink(List: Integer);
var
  NewCap: Integer;
begin
If CheckList(List) then
  begin
    If ResolveShrinkCapacity(Capacity[List],Count[List],fListGrowSettings[List],NewCap) then
      Capacity[List] := NewCap;
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.Shrink: List index (%d) out of bounds.',[StaticClassName,List]);
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiListObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Create(ListCount: Integer);
begin
inherited Create;
SetListCount(ListCount)
end;

//------------------------------------------------------------------------------

destructor {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Destroy;
begin
SetLength(fListGrowSettings,0);
inherited;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  LowList: Integer;
begin
Result := Low(fListGrowSettings);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  HighList: Integer;
begin
Result := High(fListGrowSettings);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  CheckList(List: Integer): Boolean;
begin
Result := (List >= LowList) and (List <= HighList);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  CheckIndex(List,Index: Integer): Boolean;
begin
Result := (Index >= LowIndex(List)) and (Index <= HighIndex(List));
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  CopyGrowSettings(Source: {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND});
var
  i:  Integer;
begin
If Self.ListCount = Source.ListCount then
  begin
    For i := LowList to HighList do
      fListGrowSettings[i] := Source.ListGrowSettings[i];
  end
else raise EACIncompatibleClass.CreateFmt('%s.CopyGrowSettings: List count mismatch (%d,%d).',[StaticClassName,Self.ListCount,Source.ListCount]);
end;

{$ENDIF}
