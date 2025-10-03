{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  BasicUIM

    Provides some very basic implementation management, namely routing of
    function calls, method calls, objects and classes via variables.

    Note that it was written for a specific purpose, and was never meant to be
    some universal library. Threfore there is not much functionality and also
    absolutely no documentation. But, I am open to suggestions, if anyone will
    be interested.

  Version 1.2 (2025-10-03)

  Last change 2025-10-03

  ©2023-2025 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.BasicUIM

  Dependencies:
  * AuxExceptions - github.com/TheLazyTomcat/Lib.AuxExceptions
    SimpleCPUID   - github.com/TheLazyTomcat/Lib.SimpleCPUID

  Library AuxExceptions is required only when rebasing local exception classes
  (see symbol BasicUIM_UseAuxExceptions for details).

  Library AuxExceptions might also be required as an indirect dependency.

  Indirect dependencies:
    AuxTypes    - github.com/TheLazyTomcat/Lib.AuxTypes
    StrRect     - github.com/TheLazyTomcat/Lib.StrRect
    UInt64Utils - github.com/TheLazyTomcat/Lib.UInt64Utils
    WinFileInfo - github.com/TheLazyTomcat/Lib.WinFileInfo

===============================================================================}
unit BasicUIM;
{
  BasicUIM_PurePascal

  If you want to compile this unit without ASM, don't want to or cannot define
  PurePascal for the entire project and at the same time you don't want to or
  cannot make changes to this unit, define this symbol for the entire project
  and this unit will be compiled in PurePascal mode.

    NOTE - this unit cannot be compiled without asm, but this symbol is still
           provided for the sake of completeness.
}
{$IFDEF BasicUIM_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{
  BasicUIM_UseAuxExceptions

  If you want library-specific exceptions to be based on more advanced classes
  provided by AuxExceptions library instead of basic Exception class, and don't
  want to or cannot change code in this unit, you can define global symbol
  BasicUIM_UseAuxExceptions to achieve this.
}
{$IF Defined(BasicUIM_UseAuxExceptions)}
  {$DEFINE UseAuxExceptions}
{$IFEND} 

//------------------------------------------------------------------------------

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported CPU architecture.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFpc}
  {$MODESWITCH ClassicProcVars+}
  {$ASMMODE Intel}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------

{$IF Defined(PurePascal) and not Defined(CompTest)}
  {$MESSAGE WARN 'This unit cannot be compiled in PurePascal mode.'}
{$IFEND} 

interface

uses
  SysUtils
  {$IFDEF UseAuxExceptions}, AuxExceptions{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EUIMException = class({$IFDEF UseAuxExceptions}EAEGeneralException{$ELSE}Exception{$ENDIF});

  EUIMIndexOutOfBounds  = class(EUIMException);
  EUIMInvalidValue      = class(EUIMException);
  EUIMInvalidState      = class(EUIMException);
  EUIMDuplicateItem     = class(EUIMException);
  EUIMUnknownIdentifier = class(EUIMException);

{===============================================================================
    Auxiliary functions - declaration
===============================================================================}

Function Method(Code,Data: Pointer): TMethod;

{===============================================================================
--------------------------------------------------------------------------------
                                 TUIMCommonClass
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUIMCommonClass - class declaration
===============================================================================}
type
  TUIMCommonClass = class(TObject)
  protected
    Function GetCapacity(List: Integer): Integer; virtual; abstract;
    procedure SetCapacity(List,Value: Integer); virtual; abstract;
    Function GetCount(List: Integer): Integer; virtual; abstract;
    class Function GrowDelta(List: Integer): Integer; virtual; abstract;
    procedure Grow(List: Integer); virtual;  // only linear growth by GrowFactor
  public
    property Capacity[List: Integer]: Integer read GetCapacity write SetCapacity;
    property Count[List: Integer]: Integer read GetCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                   TUIMRouting
--------------------------------------------------------------------------------
===============================================================================}
type
  TUIMIdentifier = type Integer;

  TUIMRoutingType = (rtFunction,rtMethod,rtObject,rtClass);

  TUIMImplementationFlag  = (ifSelect,ifAvailable,ifSupported);
  TUIMImplementationFlags = set of TUIMImplementationFlag;

  TUIMImplementationType = (itFunction,itMethod,itObject,itClass,itAlias);

type
  TUIMImplementation = record
    ImplementationID:     TUIMIdentifier;
    ImplementationFlags:  TUIMImplementationFlags;
    case ImplementorType: TUIMImplementationType of
      itFunction: (ImplementorFunction: Pointer);
      itMethod:   (ImplementorMethod:   TMethod);
      itObject:   (ImplementorObject:   TObject);
      itClass:    (ImplementorClass:    TClass);
      itAlias:    (ImplementorOriginal: TUIMIdentifier);
  end;

{===============================================================================
    TUIMRouting - class declaration
===============================================================================}
type
  TUIMRouting = class(TUIMCommonClass)
  protected
    fRoutingID:           TUIMIdentifier;
    fRoutingType:         TUIMRoutingType;
    fRoutingVarAddr:      Pointer;
    fImplementations:     array of TUIMImplementation;
    fImplementationCount: Integer;
    fSelectedIndex:       Integer;
    Function GetImplementation(Index: Integer): TUIMImplementation; virtual;
    Function GetImplementationFlags(Index: Integer): TUIMImplementationFlags; virtual;
    procedure SetImplementationFlags(Index: Integer; Value: TUIMImplementationFlags); virtual;
    Function GetSelectedIndex: Integer; virtual;
    Function GetCapacity(List: Integer): Integer; override;
    procedure SetCapacity(List,Value: Integer); override;
    Function GetCount(List: Integer): Integer; override;
    class Function GrowDelta(List: Integer): Integer; override;
    procedure Initialize(RoutingID: TUIMIdentifier; RoutingType: TUIMRoutingType; RoutingVarAddr: Pointer); virtual;
    procedure Finalize; virtual;
    Function Add(ImplementationID: TUIMIdentifier; ImplementorType: TUIMImplementationType; ImplementorPtr: Pointer; ImplementationFlags: TUIMImplementationFlags): Integer; overload; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; ImplementorType: TUIMImplementationType; ImplementorPtr: Pointer; ImplementationFlags: TUIMImplementationFlags): Integer; overload; virtual;
  public
    constructor Create(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer); overload;
    constructor Create(RoutingID: TUIMIdentifier; var MethodVariable: TMethod); overload;
    constructor Create(RoutingID: TUIMIdentifier; var ObjectVariable: TObject); overload;
    constructor Create(RoutingID: TUIMIdentifier; var ClassVariable: TClass); overload;
    destructor Destroy; override;
    Function LowIndex: Integer; virtual;
    Function HighIndex: Integer; virtual;
    Function CheckIndex(Index: Integer): Boolean; virtual;
    Function IndexOf(ImplementationID: TUIMIdentifier): Integer; overload; virtual;
    Function IndexOf(ImplementorFunction: Pointer): Integer; overload; virtual;
    Function IndexOf(ImplementorMethod: TMethod): Integer; overload; virtual;
    Function IndexOf(ImplementorMethodCode,ImplementorMethodData: Pointer): Integer; overload; virtual;
    Function IndexOf(ImplementorObject: TObject): Integer; overload; virtual;
    Function IndexOf(ImplementorClass: TClass): Integer; overload; virtual;
    Function Find(ImplementationID: TUIMIdentifier; out Index: Integer): Boolean; overload; virtual;
    Function Find(ImplementorFunction: Pointer; out Index: Integer): Boolean; overload; virtual;
    Function Find(ImplementorMethod: TMethod; out Index: Integer): Boolean; overload; virtual;
    Function Find(ImplementorMethodCode,ImplementorMethodData: Pointer; out Index: Integer): Boolean; overload; virtual;
    Function Find(ImplementorObject: TObject; out Index: Integer): Boolean; overload; virtual;
    Function Find(ImplementorClass: TClass; out Index: Integer): Boolean; overload; virtual;
    Function Add(ImplementationID: TUIMIdentifier; ImplementorFunction: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Add(ImplementationID: TUIMIdentifier; ImplementorMethod: TMethod; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Add(ImplementationID: TUIMIdentifier; ImplementorMethodCode,ImplementorMethodData: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Add(ImplementationID: TUIMIdentifier; ImplementorObject: TObject; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Add(ImplementationID: TUIMIdentifier; ImplementorClass: TClass; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function AddAlias(ReferencedImplementationID, AliasImplementationID: TUIMIdentifier; ImplementationFlags: TUIMImplementationFlags = []): Integer; virtual;
    Function Copy(SourceImplementationID, NewImplementationID: TUIMIdentifier): Integer; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; ImplementorFunction: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; ImplementorMethod: TMethod; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; ImplementorMethodCode,ImplementorMethodData: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; ImplementorObject: TObject; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Replace(ImplementationID: TUIMIdentifier; ImplementorClass: TClass; ImplementationFlags: TUIMImplementationFlags = []): Integer; overload; virtual;
    Function Remove(ImplementationID: TUIMIdentifier): Integer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    Function FlagsGet(ImplementationID: TUIMIdentifier): TUIMImplementationFlags; virtual;
    Function FlagsSet(ImplementationID: TUIMIdentifier; ImplementationFlags: TUIMImplementationFlags): TUIMImplementationFlags; virtual;
    Function FlagAdd(ImplementationID: TUIMIdentifier; ImplementationFlag: TUIMImplementationFlag): Boolean; virtual;
    Function FlagRemove(ImplementationID: TUIMIdentifier; ImplementationFlag: TUIMImplementationFlag): Boolean; virtual;
    Function Selected(out SelectedImplementationID: TUIMIdentifier): Boolean; overload; virtual;
    Function Selected: TUIMIdentifier; overload; virtual;
    Function IsSelected(ImplementationID: TUIMIdentifier): Boolean; overload; virtual;
    Function SelectIndex(Index: Integer): TUIMIdentifier; virtual;
    procedure Select(ImplementationID: TUIMIdentifier); virtual;
    procedure InvalidateSelection; virtual;
    procedure Deselect; virtual;
    Function CheckRouting: Boolean; virtual;
    property RoutingID: TUIMIdentifier read fRoutingID;
    property RoutingType: TUIMRoutingType read fRoutingType;
    property RoutingVariableAddress: Pointer read fRoutingVarAddr;
    property Implementations[Index: Integer]: TUIMImplementation read GetImplementation; default;
    property ImplementationsFlags[Index: Integer]: TUIMImplementationFlags read GetImplementationFlags write SetImplementationFlags;
    property ImplementationCapacity: Integer index 0 read GetCapacity write SetCapacity;
    property ImplementationCount: Integer index 0 read GetCount;
    property SelectedIndex: Integer read GetSelectedIndex;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                                TUIMRoutingGroup
--------------------------------------------------------------------------------
===============================================================================}
type
  TUIMRepreSelect = (rsFirst,rsLast,rsMiddle,rsRandom);

{===============================================================================
    TUIMRoutingGroup - class declaration
===============================================================================}
type
  TUIMRoutingGroup = class(TUIMCommonClass)
  protected
    fRoutingGroupID:  TUIMIdentifier;  
    fRoutings:        array of TUIMRouting;
    fRoutingCount:    Integer;
    Function GetRouting(Index: Integer): TUIMRouting; virtual;
    Function GetCapacity(List: Integer): Integer; override;
    procedure SetCapacity(List,Value: Integer); override;
    Function GetCount(List: Integer): Integer; override;
    class Function GrowDelta(List: Integer): Integer; override;
    procedure Initialize(RoutingGroupID: TUIMIdentifier); virtual;
    procedure Finalize; virtual;
  public
    constructor Create(RoutingGroupID: TUIMIdentifier);
    destructor Destroy; override;
    Function LowIndex: Integer; virtual;
    Function HighIndex: Integer; virtual;
    Function CheckIndex(Index: Integer): Boolean; overload; virtual;
    Function IndexOf(Routing: TUIMRouting): Integer; overload; virtual;
    Function IndexOf(RoutingID: TUIMIdentifier): Integer; overload; virtual;
    Function Find(Routing: TUIMRouting; out Index: Integer): Boolean; overload; virtual;
    Function Find(RoutingID: TUIMIdentifier; out Index: Integer): Boolean; overload; virtual;
    Function Add(Routing: TUIMRouting): Integer; virtual;
    Function Remove(Routing: TUIMRouting): Integer; overload; virtual;
    Function Remove(RoutingID: TUIMIdentifier): Integer; overload; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    Function Consistent(CheckSelectedImplementation: Boolean = True): Boolean; virtual;
    procedure Select(ImplementationID: TUIMIdentifier); virtual;
    Function Representative(Selection: TUIMRepreSelect = rsFirst; CheckSelectedImplementation: Boolean = True): TUIMRouting; virtual;
    property RoutingGroupID: TUIMIdentifier read fRoutingGroupID;    
    property Routings[Index: Integer]: TUIMRouting read GetRouting; default;
    property RoutingCapacity: Integer index 0 read GetCapacity write SetCapacity;
    property RoutingCount: Integer index 0 read GetCount;
  end;

{===============================================================================
--------------------------------------------------------------------------------
                             TImplementationManager
--------------------------------------------------------------------------------
===============================================================================}
const
  UIM_IMLIST_ROUTINGS = 0;
  UIM_IMLIST_GROUPS   = 1;

{===============================================================================
    TImplementationManager - class declaration
===============================================================================}
type
  TImplementationManager = class(TUIMCommonClass)
  protected
    fRoutings:            array of TUIMRouting;
    fRoutingCount:        Integer;
    fRoutingGroups:       array of TUIMRoutingGroup;
    fRoutingGroupCount:   Integer;
    fRoutingGroupActIdx:  Integer;
    Function GetRouting(Index: Integer): TUIMRouting; virtual;
    Function GetRoutingGroup(Index: Integer): TUIMRoutingGroup; virtual;
    Function GetCapacity(List: Integer): Integer; override;
    procedure SetCapacity(List,Value: Integer); override;
    Function GetCount(List: Integer): Integer; override;
    class Function GrowDelta(List: Integer): Integer; override;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    Function RoutingAdd(RoutingID: TUIMIdentifier; RoutingType: TUIMRoutingType; RoutingVarAddr: Pointer): Integer; overload; virtual;
    procedure RoutingFinal(var Routing: TUIMRouting); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    // routing list
    Function RoutingLowIndex: Integer; overload; virtual;
    Function RoutingHighIndex: Integer; overload; virtual;
    Function RoutingCheckIndex(Index: Integer): Boolean; overload; virtual;
    Function RoutingIndexOf(RoutingID: TUIMIdentifier): Integer; virtual;
    Function RoutingFind(RoutingID: TUIMIdentifier; out Index: Integer): Boolean; virtual;
    Function RoutingFindObj(RoutingID: TUIMIdentifier): TUIMRouting; virtual;
    Function RoutingAdd(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer): Integer; overload; virtual;
    Function RoutingAdd(RoutingID: TUIMIdentifier; var MethodVariable: TMethod): Integer; overload; virtual;
    Function RoutingAdd(RoutingID: TUIMIdentifier; var ObjectVariable: TObject): Integer; overload; virtual;
    Function RoutingAdd(RoutingID: TUIMIdentifier; var ClassVariable: TClass): Integer; overload; virtual;
    Function RoutingAddObj(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer): TUIMRouting; overload; virtual;
    Function RoutingAddObj(RoutingID: TUIMIdentifier; var MethodVariable: TMethod): TUIMRouting; overload; virtual;
    Function RoutingAddObj(RoutingID: TUIMIdentifier; var ObjectVariable: TObject): TUIMRouting; overload; virtual;
    Function RoutingAddObj(RoutingID: TUIMIdentifier; var ClassVariable: TClass): TUIMRouting; overload; virtual;
    Function RoutingRemove(RoutingID: TUIMIdentifier): Integer; virtual;
    procedure RoutingDelete(Index: Integer); virtual;
    procedure RoutingClear; virtual;
    // routing group list
    Function RoutingGroupLowIndex: Integer; virtual;
    Function RoutingGroupHighIndex: Integer; virtual;
    Function RoutingGroupCheckIndex(Index: Integer): Boolean; virtual;
    Function RoutingGroupIndexOf(RoutingGroupID: TUIMIdentifier): Integer; virtual;
    Function RoutingGroupFind(RoutingGroupID: TUIMIdentifier; out Index: Integer): Boolean; virtual;
    Function RoutingGroupFindObj(RoutingGroupID: TUIMIdentifier): TUIMRoutingGroup; virtual;
    Function RoutingGroupAdd(RoutingGroupID: TUIMIdentifier): Integer; virtual;
    Function RoutingGroupAddObj(RoutingGroupID: TUIMIdentifier): TUIMRoutingGroup; virtual;
    Function RoutingGroupRemove(RoutingGroupID: TUIMIdentifier): Integer; virtual;
    procedure RoutingGroupDelete(Index: Integer); virtual;
    procedure RoutingGroupClear; virtual;
    Function RoutingGroupBegin(RoutingGroupID: TUIMIdentifier): Integer; virtual;
    Function RoutingGroupEnd: Integer; virtual;
    // properties
    property Routings[Index: Integer]: TUIMRouting read GetRouting; default;
    property RoutingCapacity: Integer index UIM_IMLIST_ROUTINGS read GetCapacity write SetCapacity;
    property RoutingCount: Integer index UIM_IMLIST_ROUTINGS read GetCount;
    property RoutingGroups[Index: Integer]: TUIMRoutingGroup read GetRoutingGroup;
    property RoutingGroupCapacity: Integer index UIM_IMLIST_GROUPS read GetCapacity write SetCapacity;
    property RoutingGroupCount: Integer index UIM_IMLIST_GROUPS read GetCount;
    property RoutingGroupActiveIndex: Integer read fRoutingGroupActIdx;
  end;

type
  // some aliases
  TUnitImplementationManager = TImplementationManager;
  TUnitImplManager = TImplementationManager;

{===============================================================================
--------------------------------------------------------------------------------
                                     Helpers                                                                   
--------------------------------------------------------------------------------
===============================================================================}
type
  TUIMImplementationInfo = record
    Identifier:     TUIMIdentifier;
    Implementator:  record
      case ImplementorType: TUIMImplementationType of
        itFunction: (ImplementorFunction: Pointer);
        itMethod:   (ImplementorMethod:   TMethod);
        itObject:   (ImplementorObject:   TObject);
        itClass:    (ImplementorClass:    TClass);
    end;
    Supported:      Boolean;
    Available:      Boolean;
  end;

Function ImplInfo(Identifier: TUIMIdentifier; ImplementorFunction: Pointer;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo; overload;
Function ImplInfo(Identifier: TUIMIdentifier; ImplementorMethod: TMethod;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo; overload;
Function ImplInfo(Identifier: TUIMIdentifier; ImplementorMethodCode,ImplementorMEthodData: Pointer;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo; overload;
Function ImplInfo(Identifier: TUIMIdentifier; ImplementorObject: TObject;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo; overload;
Function ImplInfo(Identifier: TUIMIdentifier; ImplementorClass: TClass;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo; overload;

procedure AddRouting(ImplementationManager: TImplementationManager; RoutingID: TUIMIdentifier; var FunctionVariable: Pointer;
  const Implementations: array of TUIMImplementationInfo; DefaultSelect: Integer = -1); overload;
procedure AddRouting(ImplementationManager: TImplementationManager; RoutingID: TUIMIdentifier; var MethodVariable: TMethod;
  const Implementations: array of TUIMImplementationInfo; DefaultSelect: Integer = -1); overload;
procedure AddRouting(ImplementationManager: TImplementationManager; RoutingID: TUIMIdentifier; var ObjectVariable: TObject;
  const Implementations: array of TUIMImplementationInfo; DefaultSelect: Integer = -1); overload;
procedure AddRouting(ImplementationManager: TImplementationManager; RoutingID: TUIMIdentifier; var ClassVariable: TClass;
  const Implementations: array of TUIMImplementationInfo; DefaultSelect: Integer = -1); overload;

implementation

uses
  SimpleCPUID;

{===============================================================================
    Auxiliary functions - implementation
===============================================================================}

Function Method(Code,Data: Pointer): TMethod;
begin
Result.Code := Code;
Result.Data := Data;
end;

//------------------------------------------------------------------------------

Function RoutToImplType(RoutingType: TUIMRoutingType): TUIMImplementationType;
begin
case RoutingType of
  rtFunction: Result := itFunction;
  rtMethod:   Result := itMethod;
  rtObject:   Result := itObject;
  rtClass:    Result := itClass;
else
  raise EUIMInvalidValue.CreateFmt('RoutToImplType: Invalid routing type (%d).',[Ord(RoutingType)]);
end;
end;

{===============================================================================
    Memory barrier functions - implementation
===============================================================================}

procedure MemoryBarrier_LOCK; assembler;
asm
{$IFDEF x64}
    LOCK ADD  RSP, 0
{$ELSE}
    LOCK ADD  ESP, 0
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure MemoryBarrier_CPUID;
var
  Buffer: array[0..15] of Byte;
begin
SimpleCPUID.CPUID(0,@Buffer);
end;

//------------------------------------------------------------------------------

procedure MemoryBarrier_MFENCE; assembler;
asm
    MFENCE
end;

//==============================================================================
var
  VAR_MemoryBarrier: procedure = MemoryBarrier_LOCK;

procedure MemoryBarrier;
begin
VAR_MemoryBarrier;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                 TUIMCommonClass
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUIMCommonClass - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUIMCommonClass - protected methods
-------------------------------------------------------------------------------}

procedure TUIMCommonClass.Grow(List: Integer);
begin
If Count[List] >= Capacity[List] then
  Capacity[List] := Capacity[List] + GrowDelta(List); 
end;


{===============================================================================
--------------------------------------------------------------------------------
                                   TUIMRouting
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUIMRouting - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUIMRouting - protected methods
-------------------------------------------------------------------------------}

Function TUIMRouting.GetImplementation(Index: Integer): TUIMImplementation;
begin
If CheckIndex(Index) then
  Result := fImplementations[Index]
else
  raise EUIMIndexOutOfBounds.CreateFmt('TUIMRouting.GetImplementation: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.GetImplementationFlags(Index: Integer): TUIMImplementationFlags;
begin
If CheckIndex(Index) then
  Result := fImplementations[Index].ImplementationFlags
else
  raise EUIMIndexOutOfBounds.CreateFmt('TUIMRouting.GetImplementationFlags: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.SetImplementationFlags(Index: Integer; Value: TUIMImplementationFlags);
begin
If CheckIndex(Index) then
  begin
    fImplementations[Index].ImplementationFlags := Value - [ifSelect];  
    If ifSelect in Value then
      SelectIndex(Index);
  end
else raise EUIMIndexOutOfBounds.CreateFmt('TUIMRouting.SetImplementationFlags: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.GetSelectedIndex: Integer;
begin
If not CheckRouting then
  fSelectedIndex := -1;
Result := fSelectedIndex;
end;

//------------------------------------------------------------------------------

Function TUIMRouting.GetCapacity(List: Integer): Integer;
begin
If List <> 0 then
  raise EUIMInvalidValue.CreateFmt('TUIMRouting.GetCapacity: Unknown list (%d).',[List]);
Result := Length(fImplementations);
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.SetCapacity(List,Value: Integer);
begin
If List <> 0 then
  raise EUIMInvalidValue.CreateFmt('TUIMRouting.SetCapacity: Unknown list (%d).',[List]);
If Value >= 0 then
  begin
    // there is no need for per-item initialization or finalization
    SetLength(fImplementations,Value);
    If Value < fImplementationCount then
      fImplementationCount := Value;
  end
else raise EUIMInvalidValue.CreateFmt('TUIMRouting.SetCapacity: Invalid capacity value (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.GetCount(List: Integer): Integer;
begin
If List <> 0 then
  raise EUIMInvalidValue.CreateFmt('TUIMRouting.GetCount: Unknown list (%d).',[List]);
Result := fImplementationCount;
end;

//------------------------------------------------------------------------------

class Function TUIMRouting.GrowDelta(List: Integer): Integer;
begin
If List <> 0 then
  raise EUIMInvalidValue.CreateFmt('TUIMRouting.GrowDelta: Unknown list (%d).',[List]);
Result := 4;
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.Initialize(RoutingID: TUIMIdentifier; RoutingType: TUIMRoutingType; RoutingVarAddr: Pointer);
begin
fRoutingID := RoutingID;
fRoutingType := RoutingType;
fRoutingVarAddr := RoutingVarAddr;
SetLength(fImplementations,0);
fImplementationCount := 0;
fSelectedIndex := -1;
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.Finalize;
begin
// nothing to do atm. (do NOT deselect implementations)
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; ImplementorType: TUIMImplementationType; ImplementorPtr: Pointer; ImplementationFlags: TUIMImplementationFlags): Integer;
begin
If (RoutToImplType(fRoutingType) = ImplementorType) or (ImplementorType = itAlias) then
  begin
    If not Find(ImplementationID,Result) then
      begin
        Grow(0);
        Result := fImplementationCount;
        fImplementations[Result].ImplementationID := ImplementationID;
        fImplementations[Result].ImplementationFlags := ImplementationFlags - [ifSelect];
        fImplementations[Result].ImplementorType := ImplementorType;
        case ImplementorType of
          itFunction: fImplementations[Result].ImplementorFunction := Pointer(ImplementorPtr^);
          itMethod:   fImplementations[Result].ImplementorMethod := TMethod(ImplementorPtr^);
          itObject:   fImplementations[Result].ImplementorObject := TObject(ImplementorPtr^);
          itClass:    fImplementations[Result].ImplementorClass := TClass(ImplementorPtr^);
          itAlias:    fImplementations[Result].ImplementorOriginal := TUIMIdentifier(ImplementorPtr^);
        else
          raise EUIMInvalidValue.CreateFmt('TUIMRouting.Add: Invalid implementor type (%d).',[Ord(ImplementorType)]);
        end;
        Inc(fImplementationCount);
        If ifSelect in ImplementationFlags then
          SelectIndex(Result);
      end
    else raise EUIMDuplicateItem.CreateFmt('TUIMRouting.Add: Implementation with selected id (%d) already exists.',[ImplementationID]);
  end
else raise EUIMInvalidValue.CreateFmt('TUIMRouting.Add: Wrong implementor type (%d, required %d).',[Ord(ImplementorType),Ord(fRoutingType)]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; ImplementorType: TUIMImplementationType; ImplementorPtr: Pointer; ImplementationFlags: TUIMImplementationFlags): Integer;
begin
If (RoutToImplType(fRoutingType) = ImplementorType) or (ImplementorType = itAlias) then
  begin
    If Find(ImplementationID,Result) then
      begin
        fImplementations[Result].ImplementationFlags := ImplementationFlags - [ifSelect];
        fImplementations[Result].ImplementorType := ImplementorType;
        case ImplementorType of
          itFunction: fImplementations[Result].ImplementorFunction := Pointer(ImplementorPtr^);
          itMethod:   fImplementations[Result].ImplementorMethod := TMethod(ImplementorPtr^);
          itObject:   fImplementations[Result].ImplementorObject := TObject(ImplementorPtr^);
          itClass:    fImplementations[Result].ImplementorClass := TClass(ImplementorPtr^);
          // itAlias should not happen here, but meh...
          itAlias:    fImplementations[Result].ImplementorOriginal := TUIMIdentifier(ImplementorPtr^);
        else
          raise EUIMInvalidValue.CreateFmt('TUIMRouting.Replace: Invalid implementor type (%d).',[Ord(ImplementorType)]);
        end;
        If (ifSelect in ImplementationFlags) or (fSelectedIndex = Result){reselct/reassign} then
          SelectIndex(Result);
      end
    else raise EUIMDuplicateItem.CreateFmt('TUIMRouting.Replace: Implementation to be replaced (%d) not found.',[ImplementationID]);
  end
else raise EUIMInvalidValue.CreateFmt('TUIMRouting.Replace: Wrong implementor type (%d, required %d).',[Ord(ImplementorType),Ord(fRoutingType)]);
end;

{-------------------------------------------------------------------------------
    TUIMRouting - public methods
-------------------------------------------------------------------------------}

constructor TUIMRouting.Create(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer);
begin
inherited Create;
Initialize(RoutingID,rtFunction,@FunctionVariable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TUIMRouting.Create(RoutingID: TUIMIdentifier; var MethodVariable: TMethod);
begin
inherited Create;
Initialize(RoutingID,rtMethod,@MethodVariable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TUIMRouting.Create(RoutingID: TUIMIdentifier; var ObjectVariable: TObject);
begin
inherited Create;
Initialize(RoutingID,rtObject,@ObjectVariable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TUIMRouting.Create(RoutingID: TUIMIdentifier; var ClassVariable: TClass);
begin
inherited Create;
Initialize(RoutingID,rtClass,@ClassVariable);
end;

//------------------------------------------------------------------------------

destructor TUIMRouting.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TUIMRouting.LowIndex: Integer;
begin
Result := Low(fImplementations);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.HighIndex: Integer;
begin
Result := Pred(fImplementationCount);
end;
//------------------------------------------------------------------------------

Function TUIMRouting.CheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.IndexOf(ImplementationID: TUIMIdentifier): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fImplementations[i].ImplementationID = ImplementationID then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.IndexOf(ImplementorFunction: Pointer): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If (fImplementations[i].ImplementorType = itFunction) and
     (fImplementations[i].ImplementorFunction = ImplementorFunction) then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.IndexOf(ImplementorMethod: TMethod): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If (fImplementations[i].ImplementorType = itMethod) and
     (fImplementations[i].ImplementorMethod.Code = ImplementorMethod.Code) and
     (fImplementations[i].ImplementorMethod.Data = ImplementorMethod.Data) then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.IndexOf(ImplementorMethodCode,ImplementorMethodData: Pointer): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If (fImplementations[i].ImplementorType = itMethod) and
     (fImplementations[i].ImplementorMethod.Code = ImplementorMethodCode) and
     (fImplementations[i].ImplementorMethod.Data = ImplementorMethodData) then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.IndexOf(ImplementorObject: TObject): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If (fImplementations[i].ImplementorType = itObject) and
     (fImplementations[i].ImplementorObject = ImplementorObject) then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.IndexOf(ImplementorClass: TClass): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If (fImplementations[i].ImplementorType = itClass) and
     (fImplementations[i].ImplementorClass = ImplementorClass) then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Find(ImplementationID: TUIMIdentifier; out Index: Integer): Boolean;
begin
Index := IndexOf(ImplementationID);
Result := CheckIndex(Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Find(ImplementorFunction: Pointer; out Index: Integer): Boolean;
begin
Index := IndexOf(ImplementorFunction);
Result := CheckIndex(Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Find(ImplementorMethod: TMethod; out Index: Integer): Boolean;
begin
Index := IndexOf(ImplementorMethod);
Result := CheckIndex(Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Find(ImplementorMethodCode,ImplementorMethodData: Pointer; out Index: Integer): Boolean;
begin
Index := IndexOf(ImplementorMethodCode,ImplementorMethodData);
Result := CheckIndex(Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Find(ImplementorObject: TObject; out Index: Integer): Boolean;
begin
Index := IndexOf(ImplementorObject);
Result := CheckIndex(Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Find(ImplementorClass: TClass; out Index: Integer): Boolean;
begin
Index := IndexOf(ImplementorClass);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; ImplementorFunction: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Add(ImplementationID,itFunction,@ImplementorFunction,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; ImplementorMethod: TMethod; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Add(ImplementationID,itMethod,@ImplementorMethod,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; ImplementorMethodCode,ImplementorMethodData: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer;
var
  MethodTemp: TMethod;
begin
MethodTemp.Code := ImplementorMethodCode;
MethodTemp.Data := ImplementorMethodData;
Result := Add(ImplementationID,itMethod,@MethodTemp,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; ImplementorObject: TObject; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Add(ImplementationID,itObject,@ImplementorObject,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Add(ImplementationID: TUIMIdentifier; ImplementorClass: TClass; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Add(ImplementationID,itClass,@ImplementorClass,ImplementationFlags);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.AddAlias(ReferencedImplementationID, AliasImplementationID: TUIMIdentifier; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Add(AliasImplementationID,itAlias,@ReferencedImplementationID,ImplementationFlags);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Copy(SourceImplementationID, NewImplementationID: TUIMIdentifier): Integer;
var
  SrcIndex: Integer;
begin
If Find(SourceImplementationID,SrcIndex) then
  begin
    with fImplementations[SrcIndex] do
      case ImplementorType of
        itFunction: Result := Add(NewImplementationID,ImplementorFunction,ImplementationFlags);
        itMethod:   Result := Add(NewImplementationID,ImplementorMethod,ImplementationFlags);
        itObject:   Result := Add(NewImplementationID,ImplementorObject,ImplementationFlags);
        itClass:    Result := Add(NewImplementationID,ImplementorClass,ImplementationFlags);
        itAlias:    Result := AddAlias(ImplementorOriginal,NewImplementationID,ImplementationFlags);
      else
        raise EUIMInvalidValue.CreateFmt('TUIMRouting.Copy: Invalid implementor type (%d).',[Ord(ImplementorType)])
      end;
  end
else raise EUIMUnknownIdentifier.CreateFmt('TUIMRouting.Copy: Implementation with selected ID (%d) not found.',[SourceImplementationID]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; ImplementorFunction: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Replace(ImplementationID,itFunction,@ImplementorFunction,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; ImplementorMethod: TMethod; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Replace(ImplementationID,itMethod,@ImplementorMethod,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; ImplementorMethodCode,ImplementorMethodData: Pointer; ImplementationFlags: TUIMImplementationFlags = []): Integer;
var
  MethodTemp: TMethod;
begin
MethodTemp.Code := ImplementorMethodCode;
MethodTemp.Data := ImplementorMethodData;
Result := Replace(ImplementationID,itMethod,@MethodTemp,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; ImplementorObject: TObject; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Replace(ImplementationID,itObject,@ImplementorObject,ImplementationFlags);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Replace(ImplementationID: TUIMIdentifier; ImplementorClass: TClass; ImplementationFlags: TUIMImplementationFlags = []): Integer;
begin
Result := Replace(ImplementationID,itClass,@ImplementorClass,ImplementationFlags);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Remove(ImplementationID: TUIMIdentifier): Integer;
begin
If Find(ImplementationID,Result) then
  Delete(Result)
else
  Result := -1;
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.Delete(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    If fSelectedIndex = Index then
      fSelectedIndex := -1;
    For i := Index to Pred(HighIndex) do
      begin
        fImplementations[i] := fImplementations[i + 1];
        If fSelectedIndex = (i + 1) then
          Dec(fSelectedIndex);
      end;
    Dec(fImplementationCount);
  end
else raise EUIMIndexOutOfBounds.CreateFmt('TUIMRouting.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.Clear;
begin
SetCapacity(0,0);
fSelectedIndex := -1;
end;

//------------------------------------------------------------------------------

Function TUIMRouting.FlagsGet(ImplementationID: TUIMIdentifier): TUIMImplementationFlags;
var
  Index:  Integer;
begin
If Find(ImplementationID,Index) then
  Result := fImplementations[Index].ImplementationFlags
else
  raise EUIMUnknownIdentifier.CreateFmt('TUIMRouting.FlagsGet: Implementation with selected ID (%d) not found.',[ImplementationID]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.FlagsSet(ImplementationID: TUIMIdentifier; ImplementationFlags: TUIMImplementationFlags): TUIMImplementationFlags;
var
  Index:  Integer;
begin
If Find(ImplementationID,Index) then
  begin
    Result := fImplementations[Index].ImplementationFlags;
    If ifSelect in ImplementationFlags then
      SelectIndex(Index);
    fImplementations[Index].ImplementationFlags := ImplementationFlags - [ifSelect];
  end
else raise EUIMUnknownIdentifier.CreateFmt('TUIMRouting.FlagsSet: Implementation with selected ID (%d) not found.',[ImplementationID]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.FlagAdd(ImplementationID: TUIMIdentifier; ImplementationFlag: TUIMImplementationFlag): Boolean;
var
  Index:  Integer;
begin
If Find(ImplementationID,Index) then
  begin
    Result := ImplementationFlag in fImplementations[Index].ImplementationFlags;
    If ImplementationFlag = ifSelect then
      SelectIndex(Index)
    else
      Include(fImplementations[Index].ImplementationFlags,ImplementationFlag);
  end
else raise EUIMUnknownIdentifier.CreateFmt('TUIMRouting.FlagAdd: Implementation with selected ID (%d) not found.',[ImplementationID]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.FlagRemove(ImplementationID: TUIMIdentifier; ImplementationFlag: TUIMImplementationFlag): Boolean;
var
  Index:  Integer;
begin
If Find(ImplementationID,Index) then
  begin
    Result := ImplementationFlag in fImplementations[Index].ImplementationFlags;
    Exclude(fImplementations[Index].ImplementationFlags,ImplementationFlag);
  end
else raise EUIMUnknownIdentifier.CreateFmt('TUIMRouting.FlagRemove: Implementation with selected ID (%d) not found.',[ImplementationID]);
end;

//------------------------------------------------------------------------------

Function TUIMRouting.Selected(out SelectedImplementationID: TUIMIdentifier): Boolean;
begin
If not CheckRouting then
  fSelectedIndex := -1;
If CheckIndex(fSelectedIndex) then
  begin
    SelectedImplementationID := fImplementations[fSelectedIndex].ImplementationID;
    Result := True;
  end
else Result := False;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRouting.Selected: TUIMIdentifier;
begin
If not Selected(Result) then
  raise EUIMInvalidState.Create('TUIMRouting.Selected: No implementation selected.');
end;

//------------------------------------------------------------------------------

Function TUIMRouting.IsSelected(ImplementationID: TUIMIdentifier): Boolean;
begin
If not CheckRouting then
  fSelectedIndex := -1;
If CheckIndex(fSelectedIndex) then
  Result := ImplementationID = fImplementations[fSelectedIndex].ImplementationID
else
  Result := False;
end;

//------------------------------------------------------------------------------

Function TUIMRouting.SelectIndex(Index: Integer): TUIMIdentifier;
begin
If CheckIndex(Index) then
  begin
    If fImplementations[Index].ImplementorType <> itAlias then
      begin
        fSelectedIndex := Index;
        case fRoutingType of
          rtFunction: Pointer(fRoutingVarAddr^) := fImplementations[fSelectedIndex].ImplementorFunction;
          rtMethod:   TMethod(fRoutingVarAddr^) := fImplementations[fSelectedIndex].ImplementorMethod;
          rtObject:   TObject(fRoutingVarAddr^) := fImplementations[fSelectedIndex].ImplementorObject;
          rtClass:    TClass(fRoutingVarAddr^) := fImplementations[fSelectedIndex].ImplementorClass;
        else
          raise EUIMInvalidValue.CreateFmt('TUIMRouting.SelectIndex: Invalid routing type (%d).',[Ord(fRoutingType)]);
        end;
      end
    else Select(fImplementations[Index].ImplementorOriginal);
    Result := fImplementations[Index].ImplementationID;
    MemoryBarrier;
  end
else raise EUIMIndexOutOfBounds.CreateFmt('TUIMRouting.SelectIndex: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.Select(ImplementationID: TUIMIdentifier);
var
  Index:  Integer;
begin
If Find(ImplementationID,Index) then
  SelectIndex(Index)
else
  raise EUIMUnknownIdentifier.CreateFmt('TUIMRouting.Select: Implementation with selected ID (%d) not found.',[ImplementationID]);
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.InvalidateSelection;
begin
fSelectedIndex := -1;
end;

//------------------------------------------------------------------------------

procedure TUIMRouting.Deselect;
begin
InvalidateSelection;
case fRoutingType of
  rtFunction: Pointer(fRoutingVarAddr^) := nil;
  rtMethod:   begin
                TMethod(fRoutingVarAddr^).Code := nil;
                TMethod(fRoutingVarAddr^).Data := nil;
              end;
  rtObject:   TObject(fRoutingVarAddr^) := TObject(nil);
  rtClass:    TClass(fRoutingVarAddr^) := TClass(nil);
else
  raise EUIMInvalidValue.CreateFmt('TUIMRouting.Deselect: Invalid routing type (%d).',[Ord(fRoutingType)])
end;
MemoryBarrier;
end;

//------------------------------------------------------------------------------

Function TUIMRouting.CheckRouting: Boolean;
begin
If CheckIndex(fSelectedIndex) then
  case fRoutingType of
    rtFunction: Result := Pointer(fRoutingVarAddr^) = fImplementations[fSelectedIndex].ImplementorFunction;
    rtMethod:   Result := (TMethod(fRoutingVarAddr^).Code = fImplementations[fSelectedIndex].ImplementorMethod.Code) and
                          (TMethod(fRoutingVarAddr^).Data = fImplementations[fSelectedIndex].ImplementorMethod.Data);
    rtObject:   Result := TObject(fRoutingVarAddr^) = fImplementations[fSelectedIndex].ImplementorObject;
    rtClass:    Result := TClass(fRoutingVarAddr^) = fImplementations[fSelectedIndex].ImplementorClass;
  else
    raise EUIMInvalidValue.CreateFmt('TUIMRouting.CheckRouting: Invalid routing type (%d).',[Ord(fRoutingType)])
  end
else Result := True;
end;


{===============================================================================
--------------------------------------------------------------------------------
                                TUIMRoutingGroup
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TUIMRoutingGroup - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TUIMRoutingGroup - protected methods
-------------------------------------------------------------------------------}

Function TUIMRoutingGroup.GetRouting(Index: Integer): TUIMRouting;
begin
If CheckIndex(Index) then
  Result := fRoutings[Index]
else
  raise EUIMIndexOutOfBounds.CreateFmt('TUIMRoutingGroup.GetRouting: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.GetCapacity(List: Integer): Integer;
begin
If List <> 0 then
  raise EUIMInvalidValue.CreateFmt('TUIMRoutingGroup.GetCapacity: Unknown list (%d).',[List]);
Result := Length(fRoutings);
end;

//------------------------------------------------------------------------------

procedure TUIMRoutingGroup.SetCapacity(List,Value: Integer);
begin
If List <> 0 then
  raise EUIMInvalidValue.CreateFmt('TUIMRoutingGroup.SetCapacity: Unknown list (%d).',[List]);
// no need to free removed items, we do not own them
SetLength(fRoutings,Value);
If Value < fRoutingCount then
  fRoutingCount := Value;
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.GetCount(List: Integer): Integer;
begin
If List <> 0 then
  raise EUIMInvalidValue.CreateFmt('TUIMRoutingGroup.GetCount: Unknown list (%d).',[List]);
Result := fRoutingCount;
end;

//------------------------------------------------------------------------------

class Function TUIMRoutingGroup.GrowDelta(List: Integer): Integer;
begin
If List <> 0 then
  raise EUIMInvalidValue.CreateFmt('TUIMRoutingGroup.GrowDelta: Unknown list (%d).',[List]);
Result := 8;
end;

//------------------------------------------------------------------------------

procedure TUIMRoutingGroup.Initialize(RoutingGroupID: TUIMIdentifier);
begin
fRoutingGroupID := RoutingGroupID;
fRoutings := nil;
fRoutingCount := 0;
end;

//------------------------------------------------------------------------------

procedure TUIMRoutingGroup.Finalize;
begin
fRoutings := nil;
fRoutingCount := 0;
end;

{-------------------------------------------------------------------------------
    TUIMRoutingGroup - public methods
-------------------------------------------------------------------------------}

constructor TUIMRoutingGroup.Create(RoutingGroupID: TUIMIdentifier);
begin
inherited Create;
Initialize(RoutingGroupID);
end;

//------------------------------------------------------------------------------

destructor TUIMRoutingGroup.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.LowIndex: Integer;
begin
Result := Low(froutings);
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.HighIndex: Integer;
begin
Result := Pred(fRoutingCount);
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.CheckIndex(Index: Integer): Boolean;
begin
result := (Index >= LowIndex) and (Index <= HighIndex);
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.IndexOf(Routing: TUIMRouting): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If Routing = fRoutings[i] then
    begin
      Result := i;
      Break{For i};
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRoutingGroup.IndexOf(RoutingID: TUIMIdentifier): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If RoutingID = fRoutings[i].RoutingID then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.Find(Routing: TUIMRouting; out Index: Integer): Boolean;
begin
Index := IndexOf(Routing);
Result := CheckIndex(Index);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRoutingGroup.Find(RoutingID: TUIMIdentifier; out Index: Integer): Boolean;
begin
Index := IndexOf(RoutingID);
Result := CheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.Add(Routing: TUIMRouting): Integer;
begin
If not Find(Routing,Result) then
  begin
    Grow(0);
    Result := fRoutingCount;
    fRoutings[Result] := Routing;
    Inc(fRoutingCount);
  end;
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.Remove(Routing: TUIMRouting): Integer;
begin
If Find(Routing,Result) then
  Delete(Result);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TUIMRoutingGroup.Remove(RoutingID: TUIMIdentifier): Integer;
begin
If Find(RoutingID,Result) then
  Delete(Result);
end;

//------------------------------------------------------------------------------

procedure TUIMRoutingGroup.Delete(Index: Integer);
var
  i:  Integer;
begin
If CheckIndex(Index) then
  begin
    For i := Index to Pred(HighIndex) do
      fRoutings[i] := fRoutings[i + 1];
    fRoutings[HighIndex] := nil;
    Dec(fRoutingCount);
  end
else raise EUIMIndexOutOfBounds.CreateFmt('TUIMRoutingGroup.Delete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TUIMRoutingGroup.Clear;
begin
SetLength(fRoutings,0);
fRoutingCount := 0;
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.Consistent(CheckSelectedImplementation: Boolean = True): Boolean;
var
  i,j:    Integer;
  Index:  Integer;
  A,B:    TUIMIdentifier;
begin
{
  Check whether all listed routings offer the same set of implementations and
  also that they all have the same implementation currently selected.
}
Result := False;
For i := LowIndex to Pred(HighIndex) do
  begin
    If fRoutings[i].ImplementationCount <> fRoutings[i + 1].ImplementationCount then
      Exit;
    // implementations can be in different order in each routing, so...
    For j := fRoutings[i].LowIndex to fRoutings[i].HighIndex do
      If not fRoutings[i + 1].Find(fRoutings[i][j].ImplementationID,Index) then
        Exit;
    If CheckSelectedImplementation then
      begin
        If fRoutings[i].Selected(A) <> fRoutings[i + 1].Selected(B) then
          Exit;
        If A <> B then
          Exit;
      end;
  end;
// if here, all is well
Result := True;  
end;

//------------------------------------------------------------------------------

procedure TUIMRoutingGroup.Select(ImplementationID: TUIMIdentifier);
var
  i:  Integer;
begin
For i := LowIndex to HighIndex do
  fRoutings[i].Select(ImplementationID);
end;

//------------------------------------------------------------------------------

Function TUIMRoutingGroup.Representative(Selection: TUIMRepreSelect = rsFirst; CheckSelectedImplementation: Boolean = True): TUIMRouting;
begin
If not Consistent(CheckSelectedImplementation) then
  raise EUIMInvalidState.Create('TUIMRoutingGroup.Representative: Routing group inconsistent.');
If fRoutingCount > 0 then
  case Selection of 
    rsLast:   Result := fRoutings[HighIndex];
    rsMiddle: Result := fRoutings[RoutingCount shr 1];
    rsRandom: Result := fRoutings[Random(RoutingCount)];
  else
   {rsFirst}
    Result := fRoutings[LowIndex];
  end
else raise EUIMInvalidState.Create('TUIMRoutingGroup.Representative: Routing group is empty.');
end;


{===============================================================================
--------------------------------------------------------------------------------
                             TImplementationManager
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TImplementationManager - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TImplementationManager - protected methods
-------------------------------------------------------------------------------}

Function TImplementationManager.GetRouting(Index: Integer): TUIMRouting;
begin
If RoutingCheckIndex(Index) then
  Result := fRoutings[Index]
else
  raise EUIMIndexOutOfBounds.CreateFmt('TImplementationManager.GetRouting: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.GetRoutingGroup(Index: Integer): TUIMRoutingGroup;
begin
If RoutingGroupCheckIndex(Index) then
  Result := fRoutingGroups[Index]
else
  raise EUIMIndexOutOfBounds.CreateFmt('TImplementationManager.GetRoutingGroup: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.GetCapacity(List: Integer): Integer;
begin
case List of
  UIM_IMLIST_ROUTINGS:  Result := Length(fRoutings);
  UIM_IMLIST_GROUPS:    Result := Length(fRoutingGroups);
else
  raise EUIMInvalidValue.CreateFmt('TImplementationManager.GetCapacity: Unknown list (%d).',[List]);
end;
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.SetCapacity(List,Value: Integer);
var
  i:  Integer;
begin
If Value >= 0 then
  case List of
    UIM_IMLIST_ROUTINGS:
      begin
        If Value < fRoutingCount then
          begin
            For i := Value to RoutingHighIndex do
              RoutingFinal(fRoutings[i]);
            fRoutingCount := Value;
          end;
        SetLength(fRoutings,Value);
      end;
    UIM_IMLIST_GROUPS:
      begin
        If Value < fRoutingGroupCount then
          begin
            For i := Value to RoutingGroupHighIndex do
              FreeAndNil(fRoutingGroups[i]);
            fRoutingGroupCount := Value;
          end;
        SetLength(fRoutingGroups,Value);
      end;
  else
    raise EUIMInvalidValue.CreateFmt('TImplementationManager.SetCapacity: Unknown list (%d).',[List]);
  end
else raise EUIMInvalidValue.CreateFmt('TImplementationManager.SetCapacity: Invalid capacity value (%d).',[Value]);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.GetCount(List: Integer): Integer;
begin
case List of
  UIM_IMLIST_ROUTINGS:  Result := fRoutingCount;
  UIM_IMLIST_GROUPS:    Result := fRoutingGroupCount;
else
  raise EUIMInvalidValue.CreateFmt('TImplementationManager.GetCount: Unknown list (%d).',[List]);
end;
end;

//------------------------------------------------------------------------------

class Function TImplementationManager.GrowDelta(List: Integer): Integer;
begin
case List of
  UIM_IMLIST_ROUTINGS:  Result := 16;
  UIM_IMLIST_GROUPS:    Result := 8;
else
  raise EUIMInvalidValue.CreateFmt('TImplementationManager.GrowDelta: Unknown list (%d).',[List]);
end;
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.Initialize;
begin
fRoutings := nil;
fRoutingCount := 0;
fRoutingGroups := nil;
fRoutingGroupCount := 0;
fRoutingGroupActIdx := -1;
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.Finalize;
begin
{
  Call to RoutingClear would remove all routings from groups one by one, which
  is a long process and also pointless, since they will be freed anyway.
  So first remove all groups - any attempt of removing routing from groups then
  just encounters empty group list.
}
RoutingGroupClear;
RoutingClear;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingAdd(RoutingID: TUIMIdentifier; RoutingType: TUIMRoutingType; RoutingVarAddr: Pointer): Integer;
begin
If not RoutingFind(RoutingID,Result) then
  begin
    Grow(UIM_IMLIST_ROUTINGS);
    Result := fRoutingCount;
    case RoutingType of
      rtFunction: fRoutings[Result] := TUIMRouting.Create(RoutingID,Pointer(RoutingVarAddr^));
      rtMethod:   fRoutings[Result] := TUIMRouting.Create(RoutingID,TMethod(RoutingVarAddr^));
      rtObject:   fRoutings[Result] := TUIMRouting.Create(RoutingID,TObject(RoutingVarAddr^));
      rtClass:    fRoutings[Result] := TUIMRouting.Create(RoutingID,TClass(RoutingVarAddr^));
    else
      raise EUIMInvalidValue.CreateFmt('TImplementationManager.RoutingAdd: Invalid routing type (%d).',[Ord(RoutingType)])
    end;
    Inc(fRoutingCount);
    If RoutingGroupCheckIndex(fRoutingGroupActIdx) then
      fRoutingGroups[fRoutingGroupActIdx].Add(fRoutings[Result]);
  end
else raise EUIMDuplicateItem.CreateFmt('TImplementationManager.RoutingAdd: Routing with selected ID (%d) already exists.',[RoutingID]);
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.RoutingFinal(var Routing: TUIMRouting);
var
  i:  Integer;
begin
For i := RoutingGroupLowIndex to RoutingGroupHighIndex do
  fRoutingGroups[i].Remove(Routing);
FreeAndNil(Routing);
end;

{-------------------------------------------------------------------------------
    TImplementationManager - public methods
-------------------------------------------------------------------------------}

constructor TImplementationManager.Create;
begin
inherited;
Initialize;
end;

//------------------------------------------------------------------------------

destructor TImplementationManager.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingLowIndex: Integer;
begin
Result := Low(fRoutings);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingHighIndex: Integer;
begin
Result := Pred(fRoutingCount);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingCheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= RoutingLowIndex) and (Index <= RoutingHighIndex);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingIndexOf(RoutingID: TUIMIdentifier): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := RoutingLowIndex to RoutingHighIndex do
  If fRoutings[i].RoutingID = RoutingID then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingFind(RoutingID: TUIMIdentifier; out Index: Integer): Boolean;
begin
Index := RoutingIndexOf(RoutingID);
Result := RoutingCheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingFindObj(RoutingID: TUIMIdentifier): TUIMRouting;
var
  Index:  Integer;
begin
If RoutingFind(RoutingID,Index) then
  Result := fRoutings[Index]
else
  raise EUIMUnknownIdentifier.CreateFmt('TImplementationManager.RoutingFindObj: Routing with selected ID (%d) not found.',[RoutingID]);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingAdd(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer): Integer;
begin
Result := RoutingAdd(RoutingID,rtFunction,@FunctionVariable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.RoutingAdd(RoutingID: TUIMIdentifier; var MethodVariable: TMethod): Integer;
begin
Result := RoutingAdd(RoutingID,rtMethod,@MethodVariable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.RoutingAdd(RoutingID: TUIMIdentifier; var ObjectVariable: TObject): Integer;
begin
Result := RoutingAdd(RoutingID,rtObject,@ObjectVariable);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.RoutingAdd(RoutingID: TUIMIdentifier; var ClassVariable: TClass): Integer;
begin
Result := RoutingAdd(RoutingID,rtClass,@ClassVariable);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingAddObj(RoutingID: TUIMIdentifier; var FunctionVariable: Pointer): TUIMRouting;
var
  Index:  Integer;
begin
Index := RoutingAdd(RoutingID,FunctionVariable);
Result := fRoutings[Index];
end;
 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.RoutingAddObj(RoutingID: TUIMIdentifier; var MethodVariable: TMethod): TUIMRouting;
var
  Index:  Integer;
begin
Index := RoutingAdd(RoutingID,MethodVariable);
Result := Routings[Index];
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.RoutingAddObj(RoutingID: TUIMIdentifier; var ObjectVariable: TObject): TUIMRouting;
var
  Index:  Integer;
begin
Index := RoutingAdd(RoutingID,ObjectVariable);
Result := Routings[Index];
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function TImplementationManager.RoutingAddObj(RoutingID: TUIMIdentifier; var ClassVariable: TClass): TUIMRouting;
var
  Index:  Integer;
begin
Index := RoutingAdd(RoutingID,ClassVariable);
Result := Routings[Index];
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingRemove(RoutingID: TUIMIdentifier): Integer;
begin
If RoutingFind(RoutingID,Result) then
  RoutingDelete(Result);
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.RoutingDelete(Index: Integer);
var
  i:  Integer;
begin
If RoutingCheckIndex(Index) then
  begin
    RoutingFinal(fRoutings[Index]);
    For i := Index to Pred(RoutingHighIndex) do
      fRoutings[i] := fRoutings[i + 1];
    Dec(fRoutingCount);
  end
else raise EUIMIndexOutOfBounds.CreateFmt('TImplementationManager.RoutingDelete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.RoutingClear;
var
  i:  Integer;
begin
For i := RoutingLowIndex to RoutingHighIndex do
  RoutingFinal(fRoutings[i]);
SetLength(fRoutings,0);
fRoutingCount := 0;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupLowIndex: Integer;
begin
Result := Low(fRoutingGroups);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupHighIndex: Integer;
begin
Result := Pred(fRoutingGroupCount);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupCheckIndex(Index: Integer): Boolean;
begin
Result := (Index >= RoutingGroupLowIndex) and (Index <= RoutingGroupHighIndex);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupIndexOf(RoutingGroupID: TUIMIdentifier): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := RoutingGroupLowIndex to RoutingGroupHighIndex do
  If fRoutingGroups[i].RoutingGroupID = RoutingGroupID then
    begin
      Result := i;
      Break{For i};
    end;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupFind(RoutingGroupID: TUIMIdentifier; out Index: Integer): Boolean;
begin
Index := RoutingGroupIndexOf(RoutingGroupID);
Result := RoutingGroupCheckIndex(Index);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupFindObj(RoutingGroupID: TUIMIdentifier): TUIMRoutingGroup;
var
  Index:  Integer;
begin
If RoutingGroupFind(RoutingGroupID,Index) then
  Result := fRoutingGroups[Index]
else
  raise EUIMUnknownIdentifier.CreateFmt('TImplementationManager.RoutingGroupFindObj: Routing group with selected ID (%d) not found.',[RoutingGroupID]);
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupAdd(RoutingGroupID: TUIMIdentifier): Integer;
begin
If not RoutingGroupFind(RoutingGroupID,Result) then
  begin
    Grow(UIM_IMLIST_GROUPS);
    Result := fRoutingGroupCount;
    fRoutingGroups[Result] := TUIMRoutingGroup.Create(RoutingGroupID);
    Inc(fRoutingGroupCount);
    RoutingGroupEnd;
  end;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupAddObj(RoutingGroupID: TUIMIdentifier): TUIMRoutingGroup;
var
  Index:  Integer;
begin
Index := RoutingGroupAdd(RoutingGroupID);
Result := fRoutingGroups[Index];
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupRemove(RoutingGroupID: TUIMIdentifier): Integer;
begin
If RoutingGroupFind(RoutingGroupID,Result) then
  RoutingGroupDelete(Result);
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.RoutingGroupDelete(Index: Integer);
var
  i:  Integer;
begin
If RoutingGroupCheckIndex(Index) then
  begin
    FreeAndNil(fRoutingGroups[Index]);
    For i := Index to Pred(RoutingGroupHighIndex) do
      fRoutingGroups[i] := fRoutingGroups[i + 1];
    Dec(fRoutingGroupCount);
    RoutingGroupEnd;
  end
else raise EUIMIndexOutOfBounds.CreateFmt('TImplementationManager.RoutingGroupDelete: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

procedure TImplementationManager.RoutingGroupClear;
var
  i:  Integer;
begin
For i := RoutingGroupLowIndex to RoutingGroupHighIndex do
  FreeAndNil(fRoutingGroups[i]);
SetLength(fRoutingGroups,0);
fRoutingGroupCount := 0;
RoutingGroupEnd;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupBegin(RoutingGroupID: TUIMIdentifier): Integer;
begin
// if the group already exits then RoutingGroupAdd just returns its index
fRoutingGroupActIdx := RoutingGroupAdd(RoutingGroupID);
Result := fRoutingGroupActIdx;
end;

//------------------------------------------------------------------------------

Function TImplementationManager.RoutingGroupEnd: Integer;
begin
Result := fRoutingGroupActIdx;
fRoutingGroupActIdx := -1;
end;

{===============================================================================
--------------------------------------------------------------------------------
                                     Helpers                                                                   
--------------------------------------------------------------------------------
===============================================================================}

Function ImplInfo(Identifier: TUIMIdentifier; ImplementorFunction: Pointer;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo;
begin
Result.Identifier := Identifier;
Result.Implementator.ImplementorType := itFunction;
Result.Implementator.ImplementorFunction := ImplementorFunction;
Result.Supported := Supported;
Result.Available := Available;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ImplInfo(Identifier: TUIMIdentifier; ImplementorMethod: TMethod;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo;
begin
Result.Identifier := Identifier;
Result.Implementator.ImplementorType := itMethod;
Result.Implementator.ImplementorMethod := ImplementorMethod;
Result.Supported := Supported;
Result.Available := Available;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ImplInfo(Identifier: TUIMIdentifier; ImplementorMethodCode,ImplementorMEthodData: Pointer;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo;
begin
Result.Identifier := Identifier;
Result.Implementator.ImplementorType := itMethod;
Result.Implementator.ImplementorMethod.Code := ImplementorMethodCode;
Result.Implementator.ImplementorMethod.Data := ImplementorMethodData;
Result.Supported := Supported;
Result.Available := Available;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ImplInfo(Identifier: TUIMIdentifier; ImplementorObject: TObject;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo;
begin
Result.Identifier := Identifier;
Result.Implementator.ImplementorType := itObject;
Result.Implementator.ImplementorObject := ImplementorObject;
Result.Supported := Supported;
Result.Available := Available;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ImplInfo(Identifier: TUIMIdentifier; ImplementorClass: TClass;
  Supported: Boolean = True; Available: Boolean = True): TUIMImplementationInfo;
begin
Result.Identifier := Identifier;
Result.Implementator.ImplementorType := itClass;
Result.Implementator.ImplementorClass := ImplementorClass;
Result.Supported := Supported;
Result.Available := Available;
end;

//------------------------------------------------------------------------------

procedure AddRouting(ImplementationManager: TImplementationManager; RoutingID: TUIMIdentifier; var FunctionVariable: Pointer;
  const Implementations: array of TUIMImplementationInfo; DefaultSelect: Integer = -1);
var
  RoutingObject:              TUIMRouting;
  ExpectedImplementationType: TUIMImplementationType;
  ImplementationFlags:        TUIMImplementationFlags;
  i,Index:                    Integer;
begin
// RoutingAddObj always returns a valid object or crashes, nothing in between
RoutingObject := ImplementationManager.RoutingAddObj(RoutingID,FunctionVariable);
// check types of provided implementations
ExpectedImplementationType := RoutToImplType(rtFunction);
For i := Low(Implementations) to High(Implementations) do
  If Implementations[i].Implementator.ImplementorType <> ExpectedImplementationType then
    raise EUIMInvalidValue.CreateFmt('AddRouting: Implementor #%d type mismatch.',[i]);
// traverse provided implementations and add them for the new routing
For i := Low(Implementations) to High(Implementations) do
  begin
    // build flags used for current implementation
    ImplementationFlags := [];
    If Implementations[i].Available then
      begin
        Include(ImplementationFlags,ifAvailable);
        If Implementations[i].Supported then
          Include(ImplementationFlags,ifSupported);
      end;
    If i = DefaultSelect then
      Include(ImplementationFlags,ifSelect);
    // if the implementor is already present, add the new implementation as an alias
    If RoutingObject.Find(Implementations[i].Implementator.ImplementorFunction,Index) then
      RoutingObject.AddAlias(RoutingObject[Index].ImplementationID,Implementations[i].Identifier,ImplementationFlags)
    else
      RoutingObject.Add(Implementations[i].Identifier,Implementations[i].Implementator.ImplementorFunction,ImplementationFlags);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure AddRouting(ImplementationManager: TImplementationManager; RoutingID: TUIMIdentifier; var MethodVariable: TMethod;
  const Implementations: array of TUIMImplementationInfo; DefaultSelect: Integer = -1);
var
  RoutingObject:              TUIMRouting;
  ExpectedImplementationType: TUIMImplementationType;
  ImplementationFlags:        TUIMImplementationFlags;
  i,Index:                    Integer;
begin
RoutingObject := ImplementationManager.RoutingAddObj(RoutingID,MethodVariable);
ExpectedImplementationType := RoutToImplType(rtMethod);
For i := Low(Implementations) to High(Implementations) do
  If Implementations[i].Implementator.ImplementorType <> ExpectedImplementationType then
    raise EUIMInvalidValue.CreateFmt('AddRouting: Implementor #%d type mismatch.',[i]);
For i := Low(Implementations) to High(Implementations) do
  begin
    ImplementationFlags := [];
    If Implementations[i].Available then
      begin
        Include(ImplementationFlags,ifAvailable);
        If Implementations[i].Supported then
          Include(ImplementationFlags,ifSupported);
      end;
    If i = DefaultSelect then
      Include(ImplementationFlags,ifSelect);
    If RoutingObject.Find(Implementations[i].Implementator.ImplementorMethod,Index) then
      RoutingObject.AddAlias(RoutingObject[Index].ImplementationID,Implementations[i].Identifier,ImplementationFlags)
    else
      RoutingObject.Add(Implementations[i].Identifier,Implementations[i].Implementator.ImplementorMethod,ImplementationFlags);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure AddRouting(ImplementationManager: TImplementationManager; RoutingID: TUIMIdentifier; var ObjectVariable: TObject;
  const Implementations: array of TUIMImplementationInfo; DefaultSelect: Integer = -1);
var
  RoutingObject:              TUIMRouting;
  ExpectedImplementationType: TUIMImplementationType;
  ImplementationFlags:        TUIMImplementationFlags;
  i,Index:                    Integer;
begin
RoutingObject := ImplementationManager.RoutingAddObj(RoutingID,ObjectVariable);
ExpectedImplementationType := RoutToImplType(rtObject);
For i := Low(Implementations) to High(Implementations) do
  If Implementations[i].Implementator.ImplementorType <> ExpectedImplementationType then
    raise EUIMInvalidValue.CreateFmt('AddRouting: Implementor #%d type mismatch.',[i]);
For i := Low(Implementations) to High(Implementations) do
  begin
    ImplementationFlags := [];
    If Implementations[i].Available then
      begin
        Include(ImplementationFlags,ifAvailable);
        If Implementations[i].Supported then
          Include(ImplementationFlags,ifSupported);
      end;
    If i = DefaultSelect then
      Include(ImplementationFlags,ifSelect);
    If RoutingObject.Find(Implementations[i].Implementator.ImplementorObject,Index) then
      RoutingObject.AddAlias(RoutingObject[Index].ImplementationID,Implementations[i].Identifier,ImplementationFlags)
    else
      RoutingObject.Add(Implementations[i].Identifier,Implementations[i].Implementator.ImplementorObject,ImplementationFlags);
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure AddRouting(ImplementationManager: TImplementationManager; RoutingID: TUIMIdentifier; var ClassVariable: TClass;
  const Implementations: array of TUIMImplementationInfo; DefaultSelect: Integer = -1);
var
  RoutingObject:              TUIMRouting;
  ExpectedImplementationType: TUIMImplementationType;
  ImplementationFlags:        TUIMImplementationFlags;
  i,Index:                    Integer;
begin
RoutingObject := ImplementationManager.RoutingAddObj(RoutingID,ClassVariable);
ExpectedImplementationType := RoutToImplType(rtClass);
For i := Low(Implementations) to High(Implementations) do
  If Implementations[i].Implementator.ImplementorType <> ExpectedImplementationType then
    raise EUIMInvalidValue.CreateFmt('AddRouting: Implementor #%d type mismatch.',[i]);
For i := Low(Implementations) to High(Implementations) do
  begin
    ImplementationFlags := [];
    If Implementations[i].Available then
      begin
        Include(ImplementationFlags,ifAvailable);
        If Implementations[i].Supported then
          Include(ImplementationFlags,ifSupported);
      end;
    If i = DefaultSelect then
      Include(ImplementationFlags,ifSelect);
    If RoutingObject.Find(Implementations[i].Implementator.ImplementorClass,Index) then
      RoutingObject.AddAlias(RoutingObject[Index].ImplementationID,Implementations[i].Identifier,ImplementationFlags)
    else
      RoutingObject.Add(Implementations[i].Identifier,Implementations[i].Implementator.ImplementorClass,ImplementationFlags);
  end;
end;


{===============================================================================
--------------------------------------------------------------------------------
                      Unit initialization and finalization                     
--------------------------------------------------------------------------------
===============================================================================}

procedure UnitInitialize;
begin
Randomize;
If SimpleCPUID.CPUIDSupported then
  with TSimpleCPUID.Create do
  try
    If Info.SupportedExtensions.SSE2 then
      // MFENCE instruction should be available
      VAR_MemoryBarrier := MemoryBarrier_MFENCE
    else
      // MFENCE not available, use serialization instruction CPUID
      VAR_MemoryBarrier := MemoryBarrier_CPUID;
  finally
    Free;
  end
{
  If the CPU is so old or bare that it does not support CPUID, then it most
  probably does not need serialization in the first place.
}
else VAR_MemoryBarrier := MemoryBarrier_LOCK;
end;

//==============================================================================

initialization
  UnitInitialize;

end.
