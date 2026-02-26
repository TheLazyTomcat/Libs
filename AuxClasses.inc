{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Auxiliary classes and other class-related things

    This file contains main classes template used by AuxClasses library.

  Version 1.4 (2026-02-25)

  Last change 2026-02-25

  ©2018-2026 František Milt

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
    ListUtils     - github.com/TheLazyTomcat/Lib.ListUtils

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol AuxClasses_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

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
    fListResizeSettings:  TListResizeSettings;
    Function GetCapacity: Integer; virtual; abstract;
    procedure SetCapacity(Value: Integer); virtual; abstract;
    Function GetCount: Integer; virtual; abstract;
    procedure SetCount(Value: Integer); virtual; abstract;
    Function GetListResizeSettingsPtr: PListResizeSettings; virtual;
    Function GetGrowSettingsPtr: PGrowSettings; virtual;
    Function GetShrinkSettingsPtr: PShrinkSettings; virtual;
    procedure Grow(MinSpace: Integer = 1); virtual;
    procedure Shrink; virtual;
  public
    constructor Create;
    Function LowIndex: Integer; virtual; abstract;
    Function HighIndex: Integer; virtual; abstract;
    Function CheckIndex(Index: Integer): Boolean; virtual;
    // parametrized list resizing (growing/shrinking)
    procedure CopyListResizeSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedListObject
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedListObject{$ELSE}TCustomListObject{$IFEND}); virtual;
    property ListResizeSettings: TListResizeSettings read fListResizeSettings write fListResizeSettings;
    property ListResizeSettingsPtr: PListResizeSettings read GetListResizeSettingsPtr;
    // grow settings
    property GrowSettings: TGrowSettings read fListResizeSettings.Grow write fListResizeSettings.Grow;
    property GrowSettingsPtr: PGrowSettings read GetGrowSettingsPtr;
    property GrowMode: TGrowMode read fListResizeSettings.Grow.Mode write fListResizeSettings.Grow.Mode;
    property GrowInit: Integer read fListResizeSettings.Grow.Init write fListResizeSettings.Grow.Init;
    property GrowDelta: Integer read fListResizeSettings.Grow.Delta write fListResizeSettings.Grow.Delta;
    property GrowFactor: Double read fListResizeSettings.Grow.Factor write fListResizeSettings.Grow.Factor;
    property GrowLimit: Integer read fListResizeSettings.Grow.Limit write fListResizeSettings.Grow.Limit;
    // shrink settings
    property ShrinkSettings: TShrinkSettings read fListResizeSettings.Shrink write fListResizeSettings.Shrink;
    property ShrinkSettingsPtr: PShrinkSettings read GetShrinkSettingsPtr;
    property ShrinkMode: TShrinkMode read fListResizeSettings.Shrink.Mode write fListResizeSettings.Shrink.Mode;
    property ShrinkDelta: Integer read fListResizeSettings.Shrink.Delta write fListResizeSettings.Shrink.Delta;
    property ShrinkFactor: Double read fListResizeSettings.Shrink.Factor write fListResizeSettings.Shrink.Factor;
    property ShrinkLimit: Integer read fListResizeSettings.Shrink.Limit write fListResizeSettings.Shrink.Limit;
    // capacity, count properties
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                     TCustom((Simple)Interfaced)List64Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)List64Object - class declaration
===============================================================================}
{
  This class is intended as a base for massive lists that need to have more
  than 2^32 (about two billion) items - their indices, count and capacity are
  accessed using 64bit integer instead of usual 32bit type.
  I know such lists will be extremely rare and implementable pretty much only
  on 64bit systems, but if anyone needs it, here it is.
}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedList64Object = class(TCustomInterfacedObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedList64Object = class(TCustomSimpleInterfacedObject)
{$ELSE}
  TCustomList64Object = class(TCustomObject)
{$IFEND}
  protected
    fListResizeSettings:  TListResizeSettings64;
    Function GetCapacity: Int64; virtual; abstract;
    procedure SetCapacity(Value: Int64); virtual; abstract;
    Function GetCount: Int64; virtual; abstract;
    procedure SetCount(Value: Int64); virtual; abstract;
    Function GetListResizeSettingsPtr: PListResizeSettings64; virtual;
    Function GetGrowSettingsPtr: PGrowSettings64; virtual;
    Function GetShrinkSettingsPtr: PShrinkSettings64; virtual;
    procedure Grow(MinSpace: Int64 = 1); virtual;
    procedure Shrink; virtual;
  public
    constructor Create;
    Function LowIndex: Int64; virtual; abstract;
    Function HighIndex: Int64; virtual; abstract;
    Function CheckIndex(Index: Int64): Boolean; virtual;
    // parametrized list resizing (growing/shrinking)
    procedure CopyListResizeSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedList64Object
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedList64Object{$ELSE}TCustomList64Object{$IFEND}); virtual;
    property ListResizeSettings: TListResizeSettings64 read fListResizeSettings write fListResizeSettings;
    property ListResizeSettingsPtr: PListResizeSettings64 read GetListResizeSettingsPtr;
    // grow settings
    property GrowSettings: TGrowSettings64 read fListResizeSettings.Grow write fListResizeSettings.Grow;
    property GrowSettingsPtr: PGrowSettings64 read GetGrowSettingsPtr;
    property GrowMode: TGrowMode read fListResizeSettings.Grow.Mode write fListResizeSettings.Grow.Mode;
    property GrowInit: Int64 read fListResizeSettings.Grow.Init write fListResizeSettings.Grow.Init;
    property GrowDelta: Int64 read fListResizeSettings.Grow.Delta write fListResizeSettings.Grow.Delta;
    property GrowFactor: Double read fListResizeSettings.Grow.Factor write fListResizeSettings.Grow.Factor;
    property GrowLimit: Int64 read fListResizeSettings.Grow.Limit write fListResizeSettings.Grow.Limit;
    // shrink settings
    property ShrinkSettings: TShrinkSettings64 read fListResizeSettings.Shrink write fListResizeSettings.Shrink;
    property ShrinkSettingsPtr: PShrinkSettings64 read GetShrinkSettingsPtr;
    property ShrinkMode: TShrinkMode read fListResizeSettings.Shrink.Mode write fListResizeSettings.Shrink.Mode;
    property ShrinkDelta: Int64 read fListResizeSettings.Shrink.Delta write fListResizeSettings.Shrink.Delta;
    property ShrinkFactor: Double read fListResizeSettings.Shrink.Factor write fListResizeSettings.Shrink.Factor;
    property ShrinkLimit: Int64 read fListResizeSettings.Shrink.Limit write fListResizeSettings.Shrink.Limit;
    // capacity, count properties
    property Capacity: Int64 read GetCapacity write SetCapacity;
    property Count: Int64 read GetCount write SetCount;
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
    fListResizeSettings:  array of TListResizeSettings;
    Function GetListCount: Integer; virtual;
    procedure SetListCount(Value: Integer); virtual;
    Function GetCapacity(List: Integer): Integer; virtual; abstract;
    procedure SetCapacity(List,Value: Integer); virtual; abstract;
    Function GetCount(List: Integer): Integer; virtual; abstract;
    procedure SetCount(List,Value: Integer); virtual; abstract;
    // getters/setters for grow and shrink settings
    Function GetListResizeSettings(List: Integer): TListResizeSettings; virtual;
    procedure SetListResizeSettings(List: Integer; Value: TListResizeSettings); virtual;
    Function GetListResizeSettingsPtr(List: Integer): PListResizeSettings; virtual;
    Function GetGrowSettings(List: Integer): TGrowSettings; virtual;
    procedure SetGrowSettings(List: Integer; Value: TGrowSettings); virtual;
    Function GetGrowSettingsPtr(List: Integer): PGrowSettings; virtual;
    Function GetGrowSettings_Mode(List: Integer): TGrowMode; virtual;
    procedure SetGrowSettings_Mode(List: Integer; Value: TGrowMode); virtual;
    Function GetGrowSettings_Init(List: Integer): Integer; virtual;
    procedure SetGrowSettings_Init(List: Integer; Value: Integer); virtual;
    Function GetGrowSettings_Delta(List: Integer): Integer; virtual;
    procedure SetGrowSettings_Delta(List: Integer; Value: Integer); virtual;
    Function GetGrowSettings_Factor(List: Integer): Double; virtual;
    procedure SetGrowSettings_Factor(List: Integer; Value: Double); virtual;
    Function GetGrowSettings_Limit(List: Integer): Integer; virtual;
    procedure SetGrowSettings_Limit(List: Integer; Value: Integer); virtual; 
    Function GetShrinkSettings(List: Integer): TShrinkSettings; virtual;
    procedure SetShrinkSettings(List: Integer; Value: TShrinkSettings); virtual;
    Function GetShrinkSettingsPtr(List: Integer): PShrinkSettings; virtual;
    Function GetShrinkSettings_Mode(List: Integer): TShrinkMode; virtual;
    procedure SetShrinkSettings_Mode(List: Integer; Value: TShrinkMode); virtual;
    Function GetShrinkSettings_Delta(List: Integer): Integer; virtual;
    procedure SetShrinkSettings_Delta(List: Integer; Value: Integer); virtual;
    Function GetShrinkSettings_Factor(List: Integer): Double; virtual;
    procedure SetShrinkSettings_Factor(List: Integer; Value: Double); virtual;
    Function GetShrinkSettings_Limit(List: Integer): Integer; virtual;
    procedure SetShrinkSettings_Limit(List: Integer; Value: Integer); virtual;
    procedure Grow(List: Integer; MinSpace: Integer = 1); virtual;
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
    // parametrized list resizing (growing/shrinking)
    procedure CopyListResizeSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedMultiListObject
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}); virtual;
    property ListResizeSettings[List: Integer]: TListResizeSettings read GetListResizeSettings write SetListResizeSettings;
    property ListResizeSettingsPtr[List: Integer]: PListResizeSettings read GetListResizeSettingsPtr;
    // grow settings
    property GrowSettings[List: Integer]: TGrowSettings read GetGrowSettings write SetGrowSettings;
    property GrowSettingsPtr[List: Integer]: PGrowSettings read GetGrowSettingsPtr;
    property GrowMode[List: Integer]: TGrowMode read GetGrowSettings_Mode write SetGrowSettings_Mode;
    property GrowInit[List: Integer]: Integer read GetGrowSettings_Init write SetGrowSettings_Init;
    property GrowDelta[List: Integer]: Integer read GetGrowSettings_Delta write SetGrowSettings_Delta;
    property GrowFactor[List: Integer]: Double read GetGrowSettings_Factor write SetGrowSettings_Factor;
    property GrowLimit[List: Integer]: Integer read GetGrowSettings_Limit write SetGrowSettings_Limit;
    // shrink settings
    property ShrinkSettings[List: Integer]: TShrinkSettings read GetShrinkSettings write SetShrinkSettings;
    property ShrinkSettingsPtr[List: Integer]: PShrinkSettings read GetShrinkSettingsPtr;
    property ShrinkMode[List: Integer]: TShrinkMode read GetShrinkSettings_Mode write SetShrinkSettings_Mode;
    property ShrinkDelta[List: Integer]: Integer read GetShrinkSettings_Delta write SetShrinkSettings_Delta;
    property ShrinkFactor[List: Integer]: Double read GetShrinkSettings_Factor write SetShrinkSettings_Factor;
    property ShrinkLimit[List: Integer]: Integer read GetShrinkSettings_Limit write SetShrinkSettings_Limit;
    // capacity, count properties
    property ListCount: Integer read GetListCount write SetListCount;
    property Capacity[List: Integer]: Integer read GetCapacity write SetCapacity;
    property Count[List: Integer]: Integer read GetCount write SetCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                 TCustom((Simple)Interfaced)MultiList64Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiList64Object - class declaration
===============================================================================}
type
{$IF Defined(AC_Include_Interfaced)}
  TCustomInterfacedMultiList64Object = class(TCustomInterfacedObject)
{$ELSEIF Defined(AC_Include_SInterfaced)}
  TCustomSimpleInterfacedMultiList64Object = class(TCustomSimpleInterfacedObject)
{$ELSE}
  TCustomMultiList64Object = class(TCustomObject)
{$IFEND}
  private
    Function StaticClassName: String;
  protected
    fListResizeSettings:  array of TListResizeSettings64;
    Function GetListCount: Integer; virtual;
    procedure SetListCount(Value: Integer); virtual;
    Function GetCapacity(List: Integer): Int64; virtual; abstract;
    procedure SetCapacity(List: Integer; Value: Int64); virtual; abstract;
    Function GetCount(List: Integer): Int64; virtual; abstract;
    procedure SetCount(List: Integer; Value: Int64); virtual; abstract;
    // getters/setters for grow and shrink settings
    Function GetListResizeSettings(List: Integer): TListResizeSettings64; virtual;
    procedure SetListResizeSettings(List: Integer; Value: TListResizeSettings64); virtual;
    Function GetListResizeSettingsPtr(List: Integer): PListResizeSettings64; virtual;
    Function GetGrowSettings(List: Integer): TGrowSettings64; virtual;
    procedure SetGrowSettings(List: Integer; Value: TGrowSettings64); virtual;
    Function GetGrowSettingsPtr(List: Integer): PGrowSettings64; virtual;
    Function GetGrowSettings_Mode(List: Integer): TGrowMode; virtual;
    procedure SetGrowSettings_Mode(List: Integer; Value: TGrowMode); virtual;
    Function GetGrowSettings_Init(List: Integer): Int64; virtual;
    procedure SetGrowSettings_Init(List: Integer; Value: Int64); virtual;
    Function GetGrowSettings_Delta(List: Integer): Int64; virtual;
    procedure SetGrowSettings_Delta(List: Integer; Value: Int64); virtual;
    Function GetGrowSettings_Factor(List: Integer): Double; virtual;
    procedure SetGrowSettings_Factor(List: Integer; Value: Double); virtual;
    Function GetGrowSettings_Limit(List: Integer): Int64; virtual;
    procedure SetGrowSettings_Limit(List: Integer; Value: Int64); virtual;
    Function GetShrinkSettings(List: Integer): TShrinkSettings64; virtual;
    procedure SetShrinkSettings(List: Integer; Value: TShrinkSettings64); virtual;
    Function GetShrinkSettingsPtr(List: Integer): PShrinkSettings64; virtual;
    Function GetShrinkSettings_Mode(List: Integer): TShrinkMode; virtual;
    procedure SetShrinkSettings_Mode(List: Integer; Value: TShrinkMode); virtual;
    Function GetShrinkSettings_Delta(List: Integer): Int64; virtual;
    procedure SetShrinkSettings_Delta(List: Integer; Value: Int64); virtual;
    Function GetShrinkSettings_Factor(List: Integer): Double; virtual;
    procedure SetShrinkSettings_Factor(List: Integer; Value: Double); virtual;
    Function GetShrinkSettings_Limit(List: Integer): Int64; virtual;
    procedure SetShrinkSettings_Limit(List: Integer; Value: Int64); virtual;
    procedure Grow(List: Integer; MinSpace: Int64 = 1); virtual;
    procedure Shrink(List: Integer); virtual;
  public
    constructor Create(ListCount: Integer);
    destructor Destroy; override;
    Function LowList: Integer; virtual;
    Function HighList: Integer; virtual;
    Function CheckList(List: Integer): Boolean; virtual;
    Function LowIndex(List: Integer): Int64; virtual; abstract;
    Function HighIndex(List: Integer): Int64; virtual; abstract;
    Function CheckIndex(List: Integer; Index: Int64): Boolean; virtual;
    // parametrized list resizing (growing/shrinking)
    procedure CopyListResizeSettings(Source: {$IF Defined(AC_Include_Interfaced)}TCustomInterfacedMultiList64Object
      {$ELSEIF Defined(AC_Include_SInterfaced)}TCustomSimpleInterfacedMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}); virtual;
    property ListResizeSettings[List: Integer]: TListResizeSettings64 read GetListResizeSettings write SetListResizeSettings;
    property ListResizeSettingsPtr[List: Integer]: PListResizeSettings64 read GetListResizeSettingsPtr;
    // grow settings
    property GrowSettings[List: Integer]: TGrowSettings64 read GetGrowSettings write SetGrowSettings;
    property GrowSettingsPtr[List: Integer]: PGrowSettings64 read GetGrowSettingsPtr;
    property GrowMode[List: Integer]: TGrowMode read GetGrowSettings_Mode write SetGrowSettings_Mode;
    property GrowInit[List: Integer]: Int64 read GetGrowSettings_Init write SetGrowSettings_Init;
    property GrowDelta[List: Integer]: Int64 read GetGrowSettings_Delta write SetGrowSettings_Delta;
    property GrowFactor[List: Integer]: Double read GetGrowSettings_Factor write SetGrowSettings_Factor;
    property GrowLimit[List: Integer]: Int64 read GetGrowSettings_Limit write SetGrowSettings_Limit;
    // shrink settings
    property ShrinkSettings[List: Integer]: TShrinkSettings64 read GetShrinkSettings write SetShrinkSettings;
    property ShrinkSettingsPtr[List: Integer]: PShrinkSettings64 read GetShrinkSettingsPtr;
    property ShrinkMode[List: Integer]: TShrinkMode read GetShrinkSettings_Mode write SetShrinkSettings_Mode;
    property ShrinkDelta[List: Integer]: Int64 read GetShrinkSettings_Delta write SetShrinkSettings_Delta;
    property ShrinkFactor[List: Integer]: Double read GetShrinkSettings_Factor write SetShrinkSettings_Factor;
    property ShrinkLimit[List: Integer]: Int64 read GetShrinkSettings_Limit write SetShrinkSettings_Limit;
    // capacity, count properties
    property ListCount: Integer read GetListCount write SetListCount;
    property Capacity[List: Integer]: Int64 read GetCapacity write SetCapacity;
    property Count[List: Integer]: Int64 read GetCount write SetCount;
  end;

{$IF Defined(AC_Include_Interfaced)}
type
  // shorter aliases
  TCustomIObject = TCustomInterfacedObject;
  TCustomIRefCountedObject = TCustomInterfacedRefCountedObject;
  TCustomIListObject = TCustomInterfacedListObject;
  TCustomIList64Object = TCustomInterfacedList64Object;
  TCustomIMultiListObject = TCustomInterfacedMultiListObject;
  TCustomIMultiList64Object = TCustomInterfacedMultiList64Object;

{$ELSEIF Defined(AC_Include_SInterfaced)}
type
  // shorter aliases
  TCustomSIObject = TCustomSimpleInterfacedObject;
  TCustomSIRefCountedObject = TCustomSimpleInterfacedRefCountedObject;
  TCustomSIListObject = TCustomSimpleInterfacedListObject;
  TCustomSIList64Object = TCustomSimpleInterfacedList64Object;  
  TCustomSIMultiListObject = TCustomSimpleInterfacedMultiListObject;
  TCustomSIMultiList64Object = TCustomSimpleInterfacedMultiList64Object;
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

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  GetListResizeSettingsPtr: PListResizeSettings;
begin
Result := Addr(fListResizeSettings);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  GetGrowSettingsPtr: PGrowSettings;
begin
Result := Addr(fListResizeSettings.Grow);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  GetShrinkSettingsPtr: PShrinkSettings;
begin
Result := Addr(fListResizeSettings.Shrink);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Grow(MinSpace: Integer = 1);
var
  NewCapacity:  Integer;
begin
If GrowResolve(Capacity,Count,MinSpace,fListResizeSettings.Grow,NewCapacity) then
  Capacity := NewCapacity;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Shrink;
var
  NewCapacity:  Integer;
begin
If ShrinkResolve(Capacity,Count,fListResizeSettings.Shrink,NewCapacity) then
  Capacity := NewCapacity;
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)ListObject - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  Create;
begin
inherited;
fListResizeSettings := DefaultListResizeSettings;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND}.
  CopyListResizeSettings(Source: {$IF Defined(AC_I_I)}TCustomIListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIListObject{$ELSE}TCustomListObject{$IFEND});
begin
fListResizeSettings := Source.ListResizeSettings;
// no need for further processing
end;


{===============================================================================
--------------------------------------------------------------------------------
                     TCustom((Simple)Interfaced)List64Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)List64Object - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)List64Object - protected methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  GetListResizeSettingsPtr: PListResizeSettings64;
begin
Result := Addr(fListResizeSettings);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  GetGrowSettingsPtr: PGrowSettings64;
begin
Result := Addr(fListResizeSettings.Grow);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  GetShrinkSettingsPtr: PShrinkSettings64;
begin
Result := Addr(fListResizeSettings.Shrink);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  Grow(MinSpace: Int64 = 1);
var
  NewCapacity:  Int64;
begin
If GrowResolve(Capacity,Count,MinSpace,fListResizeSettings.Grow,NewCapacity) then
  Capacity := NewCapacity;
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  Shrink;
var
  NewCapacity:  Int64;
begin
If ShrinkResolve(Capacity,Count,fListResizeSettings.Shrink,NewCapacity) then
  Capacity := NewCapacity;
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)List64Object - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  Create;
begin
inherited;
fListResizeSettings := DefaultListResizeSettings64;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  CheckIndex(Index: Int64): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND}.
  CopyListResizeSettings(Source: {$IF Defined(AC_I_I)}TCustomIList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIList64Object{$ELSE}TCustomList64Object{$IFEND});
begin
fListResizeSettings := Source.ListResizeSettings;
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
Result := Length(fListResizeSettings);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListCount(Value: Integer);
var
  OldCount: Integer;
  i:        Integer;
begin
If Value < 0 then
  raise EACInvalidValue.CreateFmt('%s.SetListCount: Invalid list count (%d).',[StaticClassName,Value]);
If Value <> Length(fListResizeSettings) then
  begin
    OldCount := Length(fListResizeSettings);
    SetLength(fListResizeSettings,Value);
    If Value > OldCount then
      For i := OldCount to High(fListResizeSettings) do
       fListResizeSettings[i] := DefaultListResizeSettings;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListResizeSettings(List: Integer): TListResizeSettings;
begin
If CheckList(List) then
  Result := fListResizeSettings[List]
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListResizeSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetListResizeSettings(List: Integer; Value: TListResizeSettings);
begin
If CheckList(List) then
  fListResizeSettings[List] := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListResizeSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetListResizeSettingsPtr(List: Integer): PListResizeSettings;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List])
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListResizeSettingsPtr: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings(List: Integer): TGrowSettings;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings(List: Integer; Value: TGrowSettings);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettingsPtr(List: Integer): PGrowSettings;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List].Grow)
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettingsPtr: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Mode(List: Integer): TGrowMode;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Mode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Mode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Mode(List: Integer; Value: TGrowMode);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Mode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Mode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Init(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Init
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Init: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Init(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Init := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Init: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Delta(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Delta
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Delta: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Delta(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Delta := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Delta: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Factor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Factor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Factor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Factor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Factor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Factor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetGrowSettings_Limit(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Limit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Limit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetGrowSettings_Limit(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Limit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Limit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings(List: Integer): TShrinkSettings;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings(List: Integer; Value: TShrinkSettings);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettingsPtr(List: Integer): PShrinkSettings;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List].Shrink)
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettingsPtr: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings_Mode(List: Integer): TShrinkMode;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Mode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Mode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings_Mode(List: Integer; Value: TShrinkMode);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Mode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Mode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings_Delta(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Delta
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Delta: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings_Delta(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Delta := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Delta: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings_Factor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Factor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Factor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings_Factor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Factor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Factor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  GetShrinkSettings_Limit(List: Integer): Integer;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Limit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Limit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  SetShrinkSettings_Limit(List: Integer; Value: Integer);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Limit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Limit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Grow(List: Integer; MinSpace: Integer = 1);
var
  NewCapacity:  Integer;
begin
If CheckList(List) then
  begin
    If GrowResolve(Capacity[List],Count[List],MinSpace,fListResizeSettings[List].Grow,NewCapacity) then
      Capacity[List] := NewCapacity;
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.Grow: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  Shrink(List: Integer);
var
  NewCapacity:  Integer;
begin
If CheckList(List) then
  begin
    If ShrinkResolve(Capacity[List],Count[List],fListResizeSettings[List].Shrink,NewCapacity) then
      Capacity[List] := NewCapacity;
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
SetLength(fListResizeSettings,0);
inherited;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  LowList: Integer;
begin
Result := Low(fListResizeSettings);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND}.
  HighList: Integer;
begin
Result := High(fListResizeSettings);
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
  CopyListResizeSettings(Source: {$IF Defined(AC_I_I)}TCustomIMultiListObject{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiListObject{$ELSE}TCustomMultiListObject{$IFEND});
var
  i:  Integer;
begin
If Self.ListCount = Source.ListCount then
  begin
    For i := LowList to HighList do
      fListResizeSettings[i] := Source.ListResizeSettings[i];
  end
else raise EACIncompatibleClass.CreateFmt('%s.CopyListResizeSettings: List count mismatch (%d,%d).',[StaticClassName,Self.ListCount,Source.ListCount]);
end;


{===============================================================================
--------------------------------------------------------------------------------
                 TCustom((Simple)Interfaced)MultiList64Object
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TCustom((Simple)Interfaced)MultiList64Object - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiList64Object - private methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  StaticClassName: String;
begin
Result :=
{$IF Defined(AC_I_I)}
  'TCustomInterfacedMultiList64Object'
{$ELSEIF Defined(AC_I_SI)}
  'TCustomSimpleInterfacedMultiList64Object'
{$ELSE}
  'TCustomMultiList64Object'
{$IFEND};
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiList64Object - protected methods
-------------------------------------------------------------------------------}

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetListCount: Integer;
begin
Result := Length(fListResizeSettings);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetListCount(Value: Integer);
var
  OldCount: Integer;
  i:        Integer;
begin
If Value < 0 then
  raise EACInvalidValue.CreateFmt('%s.SetListCount: Invalid list count (%d).',[StaticClassName,Value]);
If Value <> Length(fListResizeSettings) then
  begin
    OldCount := Length(fListResizeSettings);
    SetLength(fListResizeSettings,Value);
    If Value > OldCount then
      For i := OldCount to High(fListResizeSettings) do
       fListResizeSettings[i] := DefaultListResizeSettings64;
  end;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetListResizeSettings(List: Integer): TListResizeSettings64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List]
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListResizeSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetListResizeSettings(List: Integer; Value: TListResizeSettings64);
begin
If CheckList(List) then
  fListResizeSettings[List] := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetListResizeSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetListResizeSettingsPtr(List: Integer): PListResizeSettings64;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List])
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetListResizeSettingsPtr: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings(List: Integer): TGrowSettings64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings(List: Integer; Value: TGrowSettings64);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettingsPtr(List: Integer): PGrowSettings64;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List].Grow)
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettingsPtr: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Mode(List: Integer): TGrowMode;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Mode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Mode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Mode(List: Integer; Value: TGrowMode);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Mode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Mode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Init(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Init
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Init: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Init(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Init := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Init: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Delta(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Delta
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Delta: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Delta(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Delta := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Delta: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Factor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Factor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Factor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Factor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Factor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Factor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetGrowSettings_Limit(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Grow.Limit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetGrowSettings_Limit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetGrowSettings_Limit(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Grow.Limit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetGrowSettings_Limit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings(List: Integer): TShrinkSettings64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings(List: Integer; Value: TShrinkSettings64);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettingsPtr(List: Integer): PShrinkSettings64;
begin
If CheckList(List) then
  Result := Addr(fListResizeSettings[List].Shrink)
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettingsPtr: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings_Mode(List: Integer): TShrinkMode;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Mode
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Mode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings_Mode(List: Integer; Value: TShrinkMode);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Mode := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Mode: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings_Delta(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Delta
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Delta: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings_Delta(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Delta := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Delta: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings_Factor(List: Integer): Double;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Factor
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Factor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings_Factor(List: Integer; Value: Double);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Factor := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Factor: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  GetShrinkSettings_Limit(List: Integer): Int64;
begin
If CheckList(List) then
  Result := fListResizeSettings[List].Shrink.Limit
else
  raise EACIndexOutOfBounds.CreateFmt('%s.GetShrinkSettings_Limit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  SetShrinkSettings_Limit(List: Integer; Value: Int64);
begin
If CheckList(List) then
  fListResizeSettings[List].Shrink.Limit := Value
else
  raise EACIndexOutOfBounds.CreateFmt('%s.SetShrinkSettings_Limit: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  Grow(List: Integer; MinSpace: Int64 = 1);
var
  NewCapacity:  Int64;
begin
If CheckList(List) then
  begin
    If GrowResolve(Capacity[List],Count[List],MinSpace,fListResizeSettings[List].Grow,NewCapacity) then
      Capacity[List] := NewCapacity;
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.Grow: List index (%d) out of bounds.',[StaticClassName,List]);
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  Shrink(List: Integer);
var
  NewCapacity:  Int64;
begin
If CheckList(List) then
  begin
    If ShrinkResolve(Capacity[List],Count[List],fListResizeSettings[List].Shrink,NewCapacity) then
      Capacity[List] := NewCapacity;
  end
else raise EACIndexOutOfBounds.CreateFmt('%s.Shrink: List index (%d) out of bounds.',[StaticClassName,List]);
end;

{-------------------------------------------------------------------------------
    TCustom((Simple)Interfaced)MultiList64Object - public methods
-------------------------------------------------------------------------------}

constructor {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  Create(ListCount: Integer);
begin
inherited Create;
SetListCount(ListCount)
end;

//------------------------------------------------------------------------------

destructor {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  Destroy;
begin
SetLength(fListResizeSettings,0);
inherited;
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  LowList: Integer;
begin
Result := Low(fListResizeSettings);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  HighList: Integer;
begin
Result := High(fListResizeSettings);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  CheckList(List: Integer): Boolean;
begin
Result := (List >= LowList) and (List <= HighList);
end;

//------------------------------------------------------------------------------

Function {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  CheckIndex(List: Integer; Index: Int64): Boolean;
begin
Result := (Index >= LowIndex(List)) and (Index <= HighIndex(List));
end;

//------------------------------------------------------------------------------

procedure {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND}.
  CopyListResizeSettings(Source: {$IF Defined(AC_I_I)}TCustomIMultiList64Object{$ELSEIF Defined(AC_I_SI)}TCustomSIMultiList64Object{$ELSE}TCustomMultiList64Object{$IFEND});
var
  i:  Integer;
begin
If Self.ListCount = Source.ListCount then
  begin
    For i := LowList to HighList do
      fListResizeSettings[i] := Source.ListResizeSettings[i];
  end
else raise EACIncompatibleClass.CreateFmt('%s.CopyListResizeSettings: List count mismatch (%d,%d).',[StaticClassName,Self.ListCount,Source.ListCount]);
end;

{$ENDIF}
